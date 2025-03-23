#lang racket
(require "lex.rkt")
(require "simpleParser.rkt")
(provide interpret)

;main function of the program
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (exit)
       (S_statementList (parser filename) '() (lambda (v) v) exit #f #f #f)))))

;top level - runs statements in statement list in order
(define S_statementList
  (lambda (lst state return exit break-k continue-k throw-k)
    (cond
      ((null? lst) (return state))
      (else (S_statementEval (car lst) state
                           (lambda (s)
                             (S_statementList (cdr lst) s return exit break-k continue-k throw-k))
                           exit break-k continue-k throw-k)))))

;evaluates individual statement
(define S_statementEval
  (lambda (statement state return exit break-k continue-k throw-k)
    (cond
      ((eq? (car statement) 'return) (V_returnStatement (cdr statement) state exit))
      ((eq? (car statement) 'var) (declareStatement (cdr statement) state return))
      ((eq? (car statement) '=) (S_assignStatement (cdr statement) state return))
      ((eq? (car statement) 'if) (S_ifStatement (cdr statement) state return exit break-k continue-k throw-k))
      ((eq? (car statement) 'while) (S_whileStatement (cdr statement) state return exit throw-k))
      ((eq? (car statement) 'begin) (S_codeBlockStatement (cdr statement) state return exit break-k continue-k throw-k))
      ((eq? (car statement) 'break) (if break-k (break-k state) (error "'break' used outside of loop")))
      ((eq? (car statement) 'continue) (if continue-k (continue-k state) (error "'continue' used outside of loop")))
      ((eq? (car statement) 'throw) (if throw-k (SV_throw (cadr statement) state throw-k) (error "'throw' used outside of try")))
    ((eq? (car statement) 'try) (S_tryCatchFinallyStatement (cdr statement) state return exit break-k continue-k throw-k)))
    ))

;--------------------- Try Catch Finally Statements -------------------

;abstractions
(define try-block (lambda (stmts) (car stmts)))
(define catch-block (lambda (stmts) (cadr stmts)))
(define has-finally? (lambda (stmts) (not (null? (caddr stmts)))))
(define finally-block (lambda (stmts) (if (null? (caddr stmts)) '() (cdaddr stmts))))

;run try/catch/finally block
(define S_tryCatchFinallyStatement
  (lambda (stmts state return exit break-k continue-k throw-k)
    (S_statementList (try-block stmts) state
      ;; Normal completion
      (lambda (try-state)
        (if (has-finally? stmts)
            (S_statementList (car (finally-block stmts)) try-state return exit break-k continue-k throw-k)
            (return try-state)))
      exit break-k continue-k
      ;; Exception caught via throw
      (lambda (state-k e)
        (S_addBinding (caadr (catch-block stmts)) e state-k
          (lambda (state-with-exn)
            (S_statementList (caddr (catch-block stmts)) state-with-exn
              (lambda (catch-state)
                (if (has-finally? stmts)
                    (S_statementList (car (finally-block stmts)) catch-state return exit break-k continue-k throw-k)
                    (return catch-state)))
              exit break-k continue-k throw-k)))))))

;helper to call throw continuation
(define SV_throw
  (lambda (value state throw-k)
    (V_expressionEval value state (lambda (v) (throw-k state v)))))  ;pass state and exception value

;----------------------------- Code Block -----------------------------

;handle entering and exiting code blocks
(define S_codeBlockStatement
  (lambda (stmts state return exit break-k continue-k throw-k)
    (S_statementList stmts (cons '() state) (lambda (v) (return (cdr v))) exit break-k continue-k throw-k
     )))


;--------------------------- While Statement --------------------------

;handle while loops
(define S_whileStatement
  (lambda (stmts state return exit throw-k)
    (return (cdr (call/cc
     (lambda (break-k)
       (V_expressionEval (car stmts) state (lambda (v)
                                          (if v
                                              (S_whileStatement stmts
                                                             (cdr (call/cc
                                                              (lambda (continue-k)
                                                                (S_statementEval (cadr stmts) state (lambda (s) (S_whileStatement stmts s return exit throw-k)) exit break-k continue-k throw-k))))
                                                             return exit throw-k)
                               (return state))
                           ))
     ))))
    ))



;--------------------------- If Statement -----------------------------

;handle if statements
(define S_ifStatement
  (lambda (stmts state return exit break-k continue-k throw-k)
    (V_expressionEval (car stmts) state
                    (lambda (v)
                      (if v
                          (S_statementEval (cadr stmts) state return exit break-k continue-k throw-k)
                          (if (null? (cddr stmts))
                              (return state)
                              (S_statementEval (caddr stmts) state return exit break-k continue-k throw-k)))))))

;--------------------------- Return -----------------------------------

;handle returning
(define V_returnStatement
  (lambda (expression state exit)
    (V_expressionEval (car expression) state
                    (lambda (v)
                      (cond
                        ((eq? #t v) (exit 'true))
                        ((eq? #f v) (exit 'false))
                        (else (exit v))))))) ; <-- jump directly to top-level

;--------------------------- Declare ----------------------------------

;handle declaration
(define declareStatement
  (lambda (statement state return)
    ( cond
       ((null? (cdr statement)) (S_assign (car statement) (cdr statement) state return))
       (else (S_assign (car statement) (cadr statement) state return))
      )))


;--------------------------- Assigment --------------------------------

;handle assignment statement
(define S_assignStatement
  (lambda (stmt state return)
    (if (V_declaredVar? (car stmt) state (lambda (v) v))
        (S_assign (car stmt) (cadr stmt) state return)
        (error "variable not declared"))))

;helper to check if variable declared
(define V_declaredVar? 
  (lambda (name state cont)
    (cond
      ((null? state) (cont #f))
      ((or (null? (car state)) (list? (caar state)))
       (V_declaredVar? name (car state) 
         (lambda (inner-result)
           (if inner-result
               (cont #t)
               (V_declaredVar? name (cdr state) cont)))))
      ((eq? name (caar state)) (cont #t))
      (else (V_declaredVar? name (cdr state) cont)))))

;function to do an assignmnet
(define S_assign
  (lambda (name exp state return)
    (cond
      ((null? name) (return (error "No name given")))
      (else (V_expressionEval exp state
                    (lambda (val)
                      (if (V_declaredVar? name state (lambda (v) v))
                          (S_replaceBinding name val state return)
                          (S_addBinding name val state return))
                      )))
      )))

;--------------------------- State ------------------------------------
; Sample State ((x 10) (y 9) (z true))
; Sample State with Code Block : ( ( (x 10) (y 9) ) (z true) )

;update a binding without changing order of state
(define S_replaceBinding
  (lambda (name val state return)
    (cond
      ((null? state) (return '()))
      ((and (list? (car state)) (or (null? (car state)) (list? (caar state))))
            (S_replaceBinding name val (car state) (lambda (v1)
                                                     (S_replaceBinding name val (cdr state) (lambda (v2)
                                                                                            (return (cons v1 v2)))))))
      ((eq? name (caar state)) (return (cons (list name val) (cdr state))))

      (else (S_replaceBinding name val (cdr state) (lambda (rest)
                                                   (return (cons (car state) rest))))))
      ))

;remove a binding from state
(define S_removeBinding
  (lambda (name state return)
    (cond
     ((null? name) (error "No name given"))
     ((null? state) (return '()))
     ((eq? name (caar state)) (return (cdr state)))
     (else (S_removeBinding name (cdr state) (lambda (v) (return (cons (car state) v)))))
    )))

;add a binding to state
(define S_addBinding
  (lambda (name value state return)
    (cond
      ((null? name) (error "No name given"))
      ((null? state) (return (list (list name value))))
      ((and (list? (car state)) (or (null? (car state)) (list? (caar state))))
       (S_addBinding name value (car state)
         (lambda (v)
           (return (cons v (cdr state))))))
      (else
       (return (cons (list name value) state))))))

;helper to get the value of a variable from state
(define V_getVar* 
  (lambda (name state return)
    (cond
      ((null? state) (return '()))
      ((or (null? (car state)) (list? (caar state)))
       (V_getVar* name (car state) (lambda (v1)
                                   (V_getVar* name (cdr state) (lambda (v2)
                                                               (cond
                                                                 ((null? v1) (return v2))
                                                                 (else (return v1))))))))
      ((eq? name (caar state))
       (if (null? (cadar state))
           (error "Variable not assigned value")
           (return (cadar state)))
       )
      (else (V_getVar* name (cdr state) return))
      )))

;wrapper to check for errors when gettign variables from state 
(define V_getVar 
  (lambda (name state return)
    (cond
      ((null? name)(error "No name given"))
      (else (V_getVar* name state (lambda (v)
                                  (cond
                                    ((null? v) (error "Variable not declared"))
                                    (else (return v))))))
      )))



; ------------------- Expression Evalutation -------------------------

;evaluate value of an expression
(define V_expressionEval
  (lambda (exp state return)
    (cond
      ((null? exp) (return null))

     ;<int> -> [0-9]+
     ((number?  exp) (return (exact-truncate exp)))
     ;<boolean> -> true | false | <int>
     ((eq? exp 'true) (return #t))
     ((eq? exp 'false) (return #f))
     ; <var> -> x | y | z | <boolean>
     ((symbol? exp) (V_getVar exp state return))    ;don't need to wrap in return b/c getVar is cps itself
     
     ; <uniary> -> -<var> |  !<var> | <var>
     ((and (list? exp) (and (eq? (car exp) '-) (null? (cddr exp))))
      (V_expressionEval (cadr exp) state (lambda (v) (return (- v)))))
     ((and (list? exp) (eq? (car exp) '!))
      (V_expressionEval (cadr exp) state (lambda (v) (return (not v)))))
     
     ; <multi> -> <multi> * <unary> | <multi> * <unary> | <multi> % <unary> | <uniary>
     ((and (list? exp) (or (eq? (car exp) '%) (or (eq? (car exp) '*) (eq? (car exp) '/))))
      (V_multi exp state return))
     ; <arith> -> <arith> + <multi> | <arith> - <multi> | <multi>
     ((and (list? exp) (or (eq? (car exp) '+) (eq? (car exp) '-)))
      (V_arith exp state return)
      )
     ; <comp> -> <comp> < <arith>| <comp> > <arith>| <comp> ≤ <arith>| <comp> ≥ <arith>| <arith>
     ((or (eq? (car exp) '<) (or (eq? (car exp) '>) (or (eq? (car exp ) '<= ) (eq? (car exp) '>=))))
      (V_comp exp state return)
      )
     ;<eql> -> <eql> == <comp> | <eql> != <comp> | <comp>
     ((and (list? exp) (or (eq? (car exp) '==) (eq? (car exp) '!=)))
      (V_eql exp state return)
      )
     ;<and> -> <and> && <eql> | <eql>
     ((and (list? exp) (eq? (car exp) '&&))
      (V_expressionEval (cadr exp) state (lambda (v1)
                                         (V_expressionEval (caddr exp) state (lambda (v2)
                                                                             (return (and v1 v2))))))
      )
     ;<or> -> <or> || <and> | <and>
     ((and (list? exp) (eq? (car exp) '||))
      (V_expressionEval (cadr exp) state (lambda (v1)
                                         (V_expressionEval (caddr exp) state (lambda (v2)
                                                                             (return (or v1 v2))))))
      )
     )))

;evaluate equality 
(define V_eql
  (lambda (exp state return)
    (cond
      ((eq? (car exp) '==)
       (V_expressionEval (cadr exp) state (lambda (v1)
                                          (V_expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (eq? v1 v2)))))))
      (else
       (V_expressionEval (cadr exp) state (lambda (v1)
                                          (V_expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (not (eq? v1 v2))))))))
      )))

;evaluate comparison
(define V_comp
  (lambda (exp state return)
    (cond
      ((eq? (car exp) '<)
       (V_expressionEval (cadr exp) state (lambda (v1)
                                          (V_expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (< v1 v2)))))))
      ((eq? (car exp) '>)
       (V_expressionEval (cadr exp) state (lambda (v1)
                                          (V_expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (> v1 v2)))))))
      ((eq? (car exp) '<=)
       (V_expressionEval (cadr exp) state (lambda (v1)
                                           (V_expressionEval (caddr exp) state (lambda (v2)
                                                                               (return (<= v1 v2)))))))
      (else
       (V_expressionEval (cadr exp) state (lambda (v1)
                                           (V_expressionEval (caddr exp) state (lambda (v2)
                                                                               (return (>= v1 v2)))))))
     )))

;evaulate multiplication
(define V_multi
  (lambda (exp state return)
    (cond
      ((eq? (car exp) '*)
       (V_expressionEval (cadr exp) state (lambda (v1)
                                           (V_expressionEval (caddr exp) state (lambda (v2)
                                                                               (return (* v1 v2)))))))
      ((eq? (car exp) '%)
       (V_expressionEval (cadr exp) state (lambda (v1)
                                          (V_expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (remainder v1 v2)))))))
      (else (V_expressionEval (cadr exp) state (lambda (v1)
                                               (V_expressionEval (caddr exp) state (lambda (v2)
                                                                                   (return (exact-truncate (/ v1 v2))))))))
    )))

;evaluate arithmetic (+ -)
(define V_arith
  (lambda (exp state return)
    (cond
      ((eq? (car exp) '+)
       (V_expressionEval (cadr exp) state (lambda (v1)
                                          (V_expressionEval (caddr exp) state (lambda (v2)
                                                                        (return (+ v1 v2)))))))
      (else
       (V_expressionEval (cadr exp) state (lambda (v1)
                                          (V_expressionEval (caddr exp) state (lambda (v2)
                                                                        (return (- v1 v2)))))))
      )))
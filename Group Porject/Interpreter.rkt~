#lang racket
(require "lex.rkt")
(require "functionParser.rkt")
(provide interpret)


;main function of the program
(define interpret
  (lambda (filename)
    (S_globalPass filename (lambda (globalState)
                             (V_callFunction 'main '() globalState (lambda (v) v))))))  ;call main function on the state compiled by global pass

;--------------------- Global Declarations-------------------

;first pass of file execution - binds all functions and global variables to state
(define S_globalPass
  (lambda (filename return)
    (S_globalDeclarationList (parser filename) '() return)))

;top level of global declarations - runs global declarations in declaration list in order
(define S_globalDeclarationList
  (lambda (lst state return)
    (cond
      ((null? lst) (return state))
      (else (S_globalDeclarationEval (car lst) state (lambda (s)
                                                       (S_globalDeclarationList (cdr lst) s return)))))))

;abstraction for declare type
(define dclrType car)
(define dclrBody cdr)

;processes individual global declaration
(define S_globalDeclarationEval
  (lambda (declaration state return)
    (cond
      ((eq? (dclrType declaration) 'var)      (declareStatement (dclrBody declaration) state return))
      ((eq? (dclrType declaration) 'function) (S_bindFunction (dclrBody declaration) state return))
      )))

;abstraction for function name
(define name car)
;abstraction for formal param list
(define params cadr)
;abstraction for body of function
(define body caddr)

;process function binding
(define S_bindFunction
  (lambda (f state return)    
    (V_makeClosure (params f) (body f) (name f) state (lambda (closure) 
                                                        (S_addBinding (name f) closure state return)))
    ))

;generate function closure
(define V_makeClosure
  (lambda (params body fname localState return)
    (return
     (list params body (lambda (args selfClosure closureReturn)  ;pass a diff return for closure to maintain our CPS to the right place
                         (S_bindParamsToArgs params args localState (lambda (paramState)
                                                                      (S_addBinding fname selfClosure paramState closureReturn))))))))

;helper to bind list of formal params to function arguments
;ASSUMPTION - params and args are same length
(define S_bindParamsToArgs
  (lambda (params args localState return)
    (if (null? params)
        (return localState)
        (S_addBinding (car params) (car args) localState (lambda (updatedState)
                                                           (S_bindParamsToArgs (cdr params) (cdr args) updatedState return)))
        )))


;--------------------- Function Execution-------------------
;abstraction to get formal params from function closure - currently not needed
(define cParams car)
;abstraction to get body from function closure
(define cBody cadr)
;abstraction to get environment from function closure
(define cEnvFunction caddr)

;calls a function and returns its output value or nothing
(define V_callFunction
  (lambda (fname argExpressions state return)
    (V_get fname state (lambda (closure)
                         (V_evaluateArgs argExpressions state (lambda (args)
                                                                (V_evaluateFunction args closure state return)))))))

;helper to evaluate method arguments expressions and return list of their values
(define V_evaluateArgs
  (lambda (argExprs state return)
    (if (null? argExprs)
        (return '())
        (V_expressionEval (car argExprs) state (lambda (v1)
                                                 (V_evaluateArgs (cdr argExprs) state (lambda (v2)
                                                                                        (return (cons v1 v2)))))))))
                                                

;evaluates a function given its closure - helper
(define V_evaluateFunction
  (lambda (args closure state return)  ;inparams list should be bound to formal params when generating function environment
    (return 
     (call/cc
      (lambda (exit)
        (S_statementList (cBody closure) ((cEnvFunction closure) args closure (lambda (v) v)) (lambda (v) v) exit #f #f #f))))))  ;call statement list on body of function


;--------------------- Statement List-------------------    

;top level of line-by-line code execution- runs statements in statement list in order
(define S_statementList
  (lambda (lst state return exit break-k continue-k throw-k)
    (cond
      ((null? lst) (return state))
      (else (S_statementEval (car lst) state
                           (lambda (s)
                             (S_statementList (cdr lst) s return exit break-k continue-k throw-k))
                           exit break-k continue-k throw-k)))))

;abstraction for statement type
(define stmtType car)
;abstraction for statement body
(define stmtBody cdr)
;abstraction for function name from statement body
(define fName cadr)
;abstraction for the function input parameters from the statement body
(define fParams cddr)

;evaluates individual statement
(define S_statementEval
  (lambda (statement state return exit break-k continue-k throw-k)
    (cond
      ((eq? (stmtType statement) 'return)   (V_returnStatement (stmtBody statement) state exit))
      ((eq? (stmtType statement) 'funcall)  (V_callFunction (fName statement) (fParams statement) state (lambda (ignore) (return state)))) ;return current state - discard output from function call
      ((eq? (stmtType statement) 'var)      (declareStatement (stmtBody statement) state return))
      ((eq? (stmtType statement) '=)        (S_assignStatement (stmtBody statement) state return))
      ((eq? (stmtType statement) 'if)       (S_ifStatement (stmtBody statement) state return exit break-k continue-k throw-k))
      ((eq? (stmtType statement) 'while)    (S_whileStatement (stmtBody statement) state return exit throw-k))
      ((eq? (stmtType statement) 'begin)    (S_codeBlockStatement (stmtBody statement) state return exit break-k continue-k throw-k))
      ((eq? (stmtType statement) 'break)    (if break-k (break-k state) (error "'break' used outside of loop")))
      ((eq? (stmtType statement) 'continue) (if continue-k (continue-k state) (error "'continue' used outside of loop")))
      ((eq? (stmtType statement) 'throw)    (if throw-k (SV_throw (cadr statement) state throw-k) (error "'throw' used outside of try")))
      ((eq? (stmtType statement) 'try)      (S_tryCatchFinallyStatement (stmtBody statement) state return exit break-k continue-k throw-k)))
    ))

;----------------------------- Code Block -----------------------------

;handle entering and exiting code blocks
(define S_codeBlockStatement
  (lambda (stmts state return exit break-k continue-k throw-k)
    (S_statementList stmts (cons '() state) (lambda (v) (return (cdr v))) exit break-k continue-k throw-k
     )))


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
  (lambda (stmt state exit)
        (V_expressionEval (car stmt) state
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
      ((eq? name (caar state)) (return (begin (set-box! (cadar state) val) (cons (list name (cadar state)) (cdr state))))) ;(cons (list name val) (cdr state))))

      (else (S_replaceBinding name val (cdr state) (lambda (rest)
                                                   (return (cons (car state) rest))))))
      ))

;remove a binding from state ; NOTE - currently not used
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
      ((null? state) (return (list (list name (box value)))))
      ((and (list? (car state)) (or (null? (car state)) (list? (caar state))))
       (S_addBinding name value (car state)
         (lambda (v)
           (return (cons v (cdr state))))))
      (else
       (return (cons (list name (box value)) state))))))    ;used to be name value

;helper to get the value of a variable from state
(define V_get* 
  (lambda (name state return)
    (cond
      ((null? state) (return '()))
      ((or (null? (car state)) (list? (caar state)))
       (V_get* name (car state) (lambda (v1)
                                   (V_get* name (cdr state) (lambda (v2)
                                                               (cond
                                                                 ((null? v1) (return v2))
                                                                 (else (return v1))))))))
      ((eq? name (caar state))
       (if (null? (cadar state))
           (error "Variable not assigned value")
           (return (unbox (cadar state))))
       )
      (else (V_get* name (cdr state) return))
      )))

;wrapper to check for errors when gettign variables from state 
(define V_get 
  (lambda (name state return)
    (cond
      ((null? name)(error "No name given"))
      (else (V_get* name state (lambda (v)
                                  (cond
                                    ((null? v) (error "Variable not declared"))
                                    (else (return v))))))
      )))



; ------------------- Expression Evalutation -------------------------

;evaluate value of an expression - also evaluates functions since they return values
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
      ((symbol? exp) (V_get exp state return))    ;don't need to wrap in return b/c getVar is cps itself
     
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
      ;function
      ((eq? (car exp) 'funcall) (V_callFunction (fName exp) (fParams exp) state return))
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
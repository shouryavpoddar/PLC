#lang racket
(require "lex.rkt")
(require "simpleParser.rkt")
(provide interpret)

;NOTE: expressionEval-cpsWrap is being used EVERYWHERE ABOVE EXPRESSION SECTION b/c CPS not implemented there yet
;WHEN IMLPEMENTING CPS, if you see expressionEval-cpsWrap, replace it with CPS call to expressionEval


(define interpret
  (lambda (filename)
    (call/cc
     (lambda (exit)
       (statementList (parser filename) '() (lambda (v) v) exit #f #f #f)))))

(define statementList
  (lambda (lst state return exit break-k continue-k throw-k)
    (cond
      ((null? lst) (return state))
      (else (statementEval (car lst) state
                           (lambda (s)
                             (statementList (cdr lst) s return exit break-k continue-k throw-k))
                           exit break-k continue-k throw-k)))))

(define statementEval
  (lambda (statement state return exit break-k continue-k throw-k)
    (cond
      ((eq? (car statement) 'return) (returnStatement (cdr statement) state exit))
      ((eq? (car statement) 'var) (declareStatement (cdr statement) state return))
      ((eq? (car statement) '=) (assignStatement (cdr statement) state return))
      ((eq? (car statement) 'if) (ifStatement (cdr statement) state return exit break-k continue-k throw-k))
      ((eq? (car statement) 'while) (whileStatement (cdr statement) state return exit throw-k))
      ((eq? (car statement) 'begin) (codeBlockStatement (cdr statement) state return exit break-k continue-k throw-k))
      ((eq? (car statement) 'break) (if break-k (break-k state) (error "'break' used outside of loop")))
      ((eq? (car statement) 'continue) (if continue-k (continue-k state) (error "'continue' used outside of loop")))

      ((eq? (car statement) 'try) (tryCatchFinallyStatement (cdr statement) state return exit break-k continue-k throw-k))
      ((eq? (car statement) 'throw) (if throw-k (handleThrow state (cadr statement) throw-k return) (error "'throw' used outside of try block"))))
    ))

;----------------------------- Code Block -----------------------------

(define codeBlockStatement
  (lambda (stmts state return exit break-k continue-k throw-k)
    (statementList stmts (cons '() state) (lambda (v) (return (cdr v))) exit break-k continue-k throw-k)))

;----------------------------Try/Catch -----------------------------
(define tryStmt car)
(define catchStmt cadr)
(define finallyStmt caddr)
;(define exceptionName

(define tryCatchFinallyStatement
  (lambda (stmts state return exit break-k outer-throw continue-k)
    (return (call/cc
             (lambda (throw-k)
               (call/cc
                (lambda (continue-k)
                  (tryStatement (tryStmt stmts) state exit break-k continue-k throw-k
                                (lambda (try-state-out)
                                  (call/cc
                                   (lambda (catch-k)
                                     (catchStatement (catchStmt stmts) try-state-out exit break-k continue-k outer-throw
                                                     (lambda (catch-state-out)
                                                       (finallyStatement (finallyStmt stmts) catch-state-out exit break-k continue-k outer-throw return))))))
                                  ;(finallyStatement (finallyStmt stmts) state exit break-k continue-k outer-throw return)
                                  ))))))))  ;is the state here correct?? - depends on how call/cc works

                                                                             


(define tryStatement
  (lambda (stmts state exit break-k continue-k throw-k return)
    (statementList stmts state return exit break-k continue-k throw-k))) ;should this be using CodeBlock? 

;helper to update/add binding and call the call/cc for throw - this doesn't need return b/c no recursive calls...? CHECK
(define handleThrow
  (lambda (state thrown throw-k return)
    (addBinding 'e thrown state (lambda (v)
                                  (throw-k v)))))
    ;(throw-k (addBinding 'e (expressionEval thrown state return) state return))))  ;TODO - cannot add state binding without name of var to update - using 'e' now as standin but FIXXX

    

(define catchStatement
  (lambda (stmts state exit break-k continue-k throw-k return)
    (statementList (caddr stmts) state return exit break-k continue-k throw-k)))  ;use caddr to get past catch and (e) at front of list
    

(define finallyStatement
  (lambda (stmts state exit break-k continue-k throw-k return)
    (statementList (cadr stmts) state return exit break-k continue-k throw-k)))
    
    



;--------------------------- While Statement --------------------------
;CPS DONE: SVP

(define whileStatement
  (lambda (stmts state return exit throw-k)
    (return (cdr (call/cc
     (lambda (break-k)
       (expressionEval (car stmts) state (lambda (v)
                                          (if v
                                              (whileStatement stmts
                                                             (cdr (call/cc
                                                              (lambda (continue-k)
                                                                (statementEval (cadr stmts) state (lambda (s) (whileStatement stmts s return exit throw-k)) exit break-k continue-k throw-k))))
                                                             return exit throw-k)
                               (return state))
                           ))
     ))))))



;--------------------------- If Statement -----------------------------
;CPS DONE: SVP

(define ifStatement
  (lambda (stmts state return exit break-k continue-k throw-k)
    (expressionEval (car stmts) state
                    (lambda (v)
                      (if v
                          (statementEval (cadr stmts) state return exit break-k continue-k throw-k)
                          (if (null? (cddr stmts))
                              (return state)
                              (statementEval (caddr stmts) state return exit break-k continue-k throw-k)))))))

;--------------------------- Return -----------------------------------
;CPS DONE: SVP

(define returnStatement
  (lambda (expression state exit)
    (expressionEval (car expression) state
                    (lambda (v)
                      (cond
                        ((eq? #t v) (exit 'true))
                        ((eq? #f v) (exit 'false))
                        (else (exit v))))))) ; <-- jump directly to top-level

;--------------------------- Declare ----------------------------------
;CPS DONE: SVP

(define declareStatement
  (lambda (statement state return)
    ( cond
       ((null? (cdr statement)) (assign (car statement) (cdr statement) state return))
       (else (assign (car statement) (cadr statement) state return))
      )))


;--------------------------- Assigment --------------------------------
;CPS DONE: SVP

(define assignStatement
  (lambda (stmt state return)
    (if (declaredVar? (car stmt) state)
        (assign (car stmt) (cadr stmt) state return)
        (error "variable not declared"))))

(define declaredVar?
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((or (null? (car state)) (list? (caar state)))
       (or (declaredVar? name (car state))    ; check inner block
           (declaredVar? name (cdr state))))  ; check rest of state
      ((eq? name (caar state)) #t)
      (else (declaredVar? name (cdr state))))))

(define assign
  (lambda (name exp state return)
    (cond
      ((null? name) (return (error "No name given")))
      (else (expressionEval exp state
                    (lambda (val)
                      (if (declaredVar? name state)
                          (replaceBinding name val state return)
                          (addBinding name val state return))
                      )))
      )))

;--------------------------- State ------------------------------------
; Sample State ((x 10) (y 9) (z true))
; Sample State with Code Block : ( ( (x 10) (y 9) ) (z true) )

(define replaceBinding
  (lambda (name val state return)
    (cond
      ((null? state) (return '()))
      ((and (list? (car state)) (or (null? (car state)) (list? (caar state)))
            (replaceBinding name val (car state) (lambda (v1)
                                                     (replaceBinding name val (cdr state) (lambda (v2)
                                                                                            (return (cons v1 v2))))))))
      ((eq? name (caar state)) (return (cons (list name val) (cdr state))))

      (else (replaceBinding name val (cdr state) (lambda (rest)
                                                   (return (cons (car state) rest)))))
      )))

; CPS DONE: SVP
(define removeBinding
  (lambda (name state return)
    (cond
     ((null? name) (error "No name given"))
     ((null? state) (return '()))
     ((eq? name (caar state)) (return (cdr state)))
     (else (removeBinding name (cdr state) (lambda (v) (return (cons (car state) v)))))
    )))

; CPS DONE: SVP
(define addBinding
  (lambda (name value state return)
    (cond
      ((null? name) (error "No name given"))
      ((null? state) (return (list (list name value))))
      ((and (list? (car state)) (or (null? (car state)) (list? (caar state))))
       (addBinding name value (car state)
         (lambda (v)
           (return (cons v (cdr state))))))
      (else
       (return (cons (list name value) state))))))


(define getVar* 
  (lambda (name state return)
    (cond
      ((null? state) (return '()))
      ((or (null? (car state)) (list? (caar state)))
       (getVar* name (car state) (lambda (v1)
                                   (getVar* name (cdr state) (lambda (v2)
                                                               (cond
                                                                 ((null? v1) (return v2))
                                                                 (else (return v1))))))))
      ((eq? name (caar state))
       (if (null? (cadar state))
           (error "Variable not assigned value")
           (return (cadar state)))
       )
      (else (getVar* name (cdr state) return))
      )))

; CPS DONE: ET
(define getVar 
  (lambda (name state return)
    (cond
      ((null? name)(error "No name given"))
      (else (getVar* name state (lambda (v)
                                  (cond
                                    ((null? v) (error "Variable not declared"))
                                    (else (return v))))))
      )))



; ------------------- Expression Evalutation -------------------------
(define expressionEval-cpsWrap
  (lambda (exp state)
    (expressionEval exp state (lambda (v) v))))

;CPS DONE
(define expressionEval
  (lambda (exp state return)
    (cond
      ((null? exp) (return null))

     ;<int> -> [0-9]+
     ((number?  exp) (return (exact-truncate exp)))
     ;<boolean> -> true | false | <int>
     ((eq? exp 'true) (return #t))
     ((eq? exp 'false) (return #f))
     ; <var> -> x | y | z | <boolean>
     ((symbol? exp) (getVar exp state return))    ;don't need to wrap in return b/c getVar is cps itself
     
     ; <uniary> -> -<var> |  !<var> | <var>
     ((and (list? exp) (and (eq? (car exp) '-) (null? (cddr exp))))
      (expressionEval (cadr exp) state (lambda (v) (return (- v)))))
     ((and (list? exp) (eq? (car exp) '!))
      (expressionEval (cadr exp) state (lambda (v) (return (not v)))))
     
     ; <multi> -> <multi> * <unary> | <multi> * <unary> | <multi> % <unary> | <uniary>
     ((and (list? exp) (or (eq? (car exp) '%) (or (eq? (car exp) '*) (eq? (car exp) '/))))
      (multi exp state return))
     ; <arith> -> <arith> + <multi> | <arith> - <multi> | <multi>
     ((and (list? exp) (or (eq? (car exp) '+) (eq? (car exp) '-)))
      (arith exp state return)
      )
     ; <comp> -> <comp> < <arith>| <comp> > <arith>| <comp> ≤ <arith>| <comp> ≥ <arith>| <arith>
     ((or (eq? (car exp) '<) (or (eq? (car exp) '>) (or (eq? (car exp ) '<= ) (eq? (car exp) '>=))))
      (comp exp state return)
      )
     ;<eql> -> <eql> == <comp> | <eql> != <comp> | <comp>
     ((and (list? exp) (or (eq? (car exp) '==) (eq? (car exp) '!=)))
      (eql exp state return)
      )
     ;<and> -> <and> && <eql> | <eql>
     ((and (list? exp) (eq? (car exp) '&&))
      (expressionEval (cadr exp) state (lambda (v1)
                                         (expressionEval (caddr exp) state (lambda (v2)
                                                                             (return (and v1 v2))))))
      )
     ;<or> -> <or> || <and> | <and>
     ((and (list? exp) (eq? (car exp) '||))
      (expressionEval (cadr exp) state (lambda (v1)
                                         (expressionEval (caddr exp) state (lambda (v2)
                                                                             (return (or v1 v2))))))
      )
     )))

;CPS DONE
(define eql
  (lambda (exp state return)
    (cond
      ((eq? (car exp) '==)
       (expressionEval (cadr exp) state (lambda (v1)
                                          (expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (eq? v1 v2)))))))
      (else
       (expressionEval (cadr exp) state (lambda (v1)
                                          (expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (not (eq? v1 v2))))))))
      )))

;CPS DONE
(define comp
  (lambda (exp state return)
    (cond
      ((eq? (car exp) '<)
       (expressionEval (cadr exp) state (lambda (v1)
                                          (expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (< v1 v2)))))))
      ((eq? (car exp) '>)
       (expressionEval (cadr exp) state (lambda (v1)
                                          (expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (> v1 v2)))))))
      ((eq? (car exp) '<=)
       (expressionEval (cadr exp) state (lambda (v1)
                                           (expressionEval (caddr exp) state (lambda (v2)
                                                                               (return (<= v1 v2)))))))
      (else
       (expressionEval (cadr exp) state (lambda (v1)
                                           (expressionEval (caddr exp) state (lambda (v2)
                                                                               (return (>= v1 v2)))))))
     )))

;CPS DONE
(define multi
  (lambda (exp state return)
    (cond
      ((eq? (car exp) '*)
       (expressionEval (cadr exp) state (lambda (v1)
                                           (expressionEval (caddr exp) state (lambda (v2)
                                                                               (return (* v1 v2)))))))
      ((eq? (car exp) '%)
       (expressionEval (cadr exp) state (lambda (v1)
                                          (expressionEval (caddr exp) state (lambda (v2)
                                                                              (return (remainder v1 v2)))))))
      (else (expressionEval (cadr exp) state (lambda (v1)
                                               (expressionEval (caddr exp) state (lambda (v2)
                                                                                   (return (exact-truncate (/ v1 v2))))))))
    )))

;CPS DONE
(define arith
  (lambda (exp state return)
    (cond
      ((eq? (car exp) '+)
       (expressionEval (cadr exp) state (lambda (v1)
                                          (expressionEval (caddr exp) state (lambda (v2)
                                                                        (return (+ v1 v2)))))))
      (else
       (expressionEval (cadr exp) state (lambda (v1)
                                          (expressionEval (caddr exp) state (lambda (v2)
                                                                        (return (- v1 v2)))))))
      )))
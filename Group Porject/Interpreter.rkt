#lang racket
(require "lex.rkt")
(require "simpleParser.rkt")
(provide interpret)

;NOTE: expressionEval-cpsWrap is being used EVERYWHERE ABOVE EXPRESSION SECTION b/c CPS not implemented there yet
         ;WHEN IMLPEMENTING CPS, if you see expressionEval-cpsWrap, replace it with CPS call to expressionEval


(define interpret
  (lambda (filename)
    (statementList (parser filename) '() (lambda (v) v))))

(define statementList
  (lambda (lst state return)
    (cond
      ((null? lst) (return state))
      (else (statementEval (car lst) state (lambda (s)
                                             (statementList (cdr lst) s return)))))))

(define statementEval
  (lambda (statement state return)
    (cond
      ((eq? (car statement) 'return) (returnStatement (cdr statement) state return))
      ((eq? (car statement) 'var) (declareStatement (cdr statement) state return))
      ((eq? (car statement) '=) (assignStatement (cdr statement) state return))
      ((eq? (car statement) 'if) (ifStatement (cdr statement) state return))
      ((eq? (car statement) 'while) (whileStatement (cdr statement) state return)))))

;--------------------------- While Statement --------------------------
;CPS DONE: SVP

(define whileStatement
  (lambda (stmts state return)
    (expressionEval (car stmts) state
                    (lambda (v)
                      (if v
                          (statementEval (cadr stmts) state
                                         (lambda (s)
                                           (whileStatement stmts s return)))
                      (return state))))))

;--------------------------- If Statement -----------------------------
;CPS DONE: SVP

(define ifStatement
  (lambda (stmts state return)
    (expressionEval (car stmts) state
                    (lambda (v)
                      (if v
                          (statementEval (cadr stmts) state return)
                          (if (null? (cddr stmts))
                              (return state)
                              (statementEval (caddr stmts) state return)))))))

;--------------------------- Return -----------------------------------
;CPS DONE: SVP

(define returnStatement
  (lambda (expression state return)
    (expressionEval (car expression) state
                    (lambda (v)
                      (cond
                        ((eq? #t v) (return 'true))
                        ((eq? #f v) (return 'false))
                        (else (return v)))))))

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
    (if (doesExist? stmt state)
        (assign (car stmt) (cadr stmt) state return)
        (error "variable not declared"))))

(define doesExist?
  (lambda (stmt state)
    (cond
      ((null? state)  #f)
      ((eq? (car stmt) (caar state)) #t)
      (else (doesExist? stmt (cdr state))
     ))))

(define assign
  (lambda (name exp state return)
    (cond
      ((null? name) (return (error "No name given")))
      (else (expressionEval exp state
                    (lambda (val)
                      (removeBinding name state
                                     (lambda (state-removed)
                                       (addBinding name val state-removed return)))))))))

;--------------------------- State ------------------------------------
; Sample State ((x 10) (y 9) (z true))
; Sample State with Code Block : ( ( (x 10) (y 9) ) (z true) )

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
      ((null? name)(error "No name given"))
      (else (return (cons (list name value) state)))
       )))

; CPS DONE: ET
(define getVar 
  (lambda (name state return)
    (cond
      ((null? name)(error "No name given"))
      ((null? state) (error "Variable not declared"))
      ((eq? name (caar state))
       (if (null? (cadar state))
           (error "Variable not assigned value")
           (return (cadar state)))
       )
      (else (getVar name (cdr state) return))
      )))



; ------------------- Expression Evalutation --------------------------
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



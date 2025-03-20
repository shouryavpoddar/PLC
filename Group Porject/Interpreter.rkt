#lang racket
(require "lex.rkt")
(require "simpleParser.rkt")
(provide interpret)

;NOTE: expressionEval-cpsWrap is being used EVERYWHERE ABOVE EXPRESSION SECTION b/c CPS not implemented there yet
         ;WHEN IMLPEMENTING CPS, if you see expressionEval-cpsWrap, replace it with CPS call to expressionEval


(define interpret
  (lambda (filename)
    (statementList (parser filename) '())))

(define statementList
  (lambda (lst state)
    (cond
      ((null?  lst) state)
      (else (statementList(cdr lst) (statementEval (car lst) state))))))

(define statementEval
  (lambda (statement state)
    (
     cond
      ((eq? (car statement) 'return) (returnStatement (cdr statement) state))
      ((eq? (car statement) 'var)(declareStatement (cdr statement) state))
      ((eq? (car statement) '=)(assignStatement (cdr statement) state))
      ((eq? (car statement) 'if)(ifStatement (cdr statement) state))
      ((eq? (car statement) 'while)(whileStatement (cdr statement) state))
      )))

;--------------------------- While Statement --------------------------

(define whileStatement
  (lambda (stmts state)
    (if (expressionEval-cpsWrap (car stmts) state)
        
        (whileStatement stmts (statementEval (cadr stmts) state))
        
        state)))

;--------------------------- If Statement -----------------------------

(define ifStatement
  (lambda (stmts state)
    (if (expressionEval-cpsWrap (car stmts) state)
        (statementEval (cadr stmts) state)
        (if (null? (cddr stmts))
            state
            (statementEval (caddr stmts) state)))))

;--------------------------- Return -----------------------------------

(define returnStatement
  (lambda (expression state)
    (cond
      ((eq? #t (expressionEval-cpsWrap (car expression) state)) 'true)
      ((eq? #f (expressionEval-cpsWrap (car expression) state)) 'false)
      (else (expressionEval-cpsWrap (car expression) state))
     )))

;--------------------------- Declare ----------------------------------

(define declareStatement
  (lambda (statement state)
    ( cond
       ((null? (cdr statement)) (assign (car statement) (cdr statement) state))
       (else (assign (car statement) (cadr statement) state))
      )))


;--------------------------- Assigment --------------------------------
(define assignStatement
  (lambda (stmt state)
    (if (doesExist? stmt state)
        (assign (car stmt) (cadr stmt) state)
        (error "variable not declared"))))

(define doesExist?
  (lambda (stmt state)
    (cond
      ((null? state) #f)
      ((eq? (car stmt) (caar state)) #t)
      (else (doesExist? stmt (cdr state)))
     )))

(define assign
  (lambda (name exp state)
    (cond
      ((null? name) (error "No name given"))
      (else (addBinding name (expressionEval-cpsWrap exp state) (removeBinding name state)))
    )))


;--------------------------- State ------------------------------------
; Sample State ((x 10) (y 9) (z true))

(define removeBinding
  (lambda (name state)
    (cond
     ((null? name)(error "No name given"))
     ((null? state) '())
     ((eq? name (caar state)) (cdr state))
     (else (cons (car state) (removeBinding name (cdr state)))
    ))))

(define addBinding
  (lambda (name value state)
    (cond
      ((null? name)(error "No name given"))
      (else (cons (list name value) state))
       )))

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
      ((null? exp) null)

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



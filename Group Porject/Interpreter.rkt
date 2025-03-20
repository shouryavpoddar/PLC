#lang racket
(require "lex.rkt")
(require "simpleParser.rkt")
(provide interpret)

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
    (if (expressionEval (car stmts) state)
        
        (whileStatement stmts (statementEval (cadr stmts) state))
        
        state)))

;--------------------------- If Statement -----------------------------

(define ifStatement
  (lambda (stmts state)
    (if (expressionEval (car stmts) state)
        (statementEval (cadr stmts) state)
        (if (null? (cddr stmts))
            state
            (statementEval (caddr stmts) state)))))

;--------------------------- Return -----------------------------------

(define returnStatement
  (lambda (expression state)
    (cond
      ((eq? #t (expressionEval (car expression) state)) 'true)
      ((eq? #f (expressionEval (car expression) state)) 'false)
      (else (expressionEval (car expression) state))
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
      (else (addBinding name (expressionEval exp state) (removeBinding name state)))
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
  (lambda (name state)
    (cond
      ((null? name)(error "No name given"))
      ((null? state) (error "Variable not declared"))
      ((eq? name (caar state))
       (if (null? (cadar state))
           (error "Variable not assigned value")
           (cadar state))
       )
      (else (getVar name (cdr state)))
      )))



; ------------------- Expression Evalutation --------------------------

(define expressionEval
  (lambda (exp state)
    (cond
      ((null? exp) null)

     ;<int> -> [0-9]+
     ((number?  exp) (exact-truncate exp))
     ;<boolean> -> true | false | <int>
     ((eq? exp 'true) #t)
     ((eq? exp 'false) #f)
     ; <var> -> x | y | z | <boolean>
     ((symbol? exp) (getVar exp state))
     
     ; <uniary> -> -<var> |  !<var> | <var>
     ((and (list? exp) (and (eq? (car exp) '-) (null? (cddr exp)))) (- (expressionEval (cadr exp) state)))
     ((and (list? exp) (eq? (car exp) '!)) (not (expressionEval (cadr exp) state)))
     
     ; <multi> -> <multi> * <unary> | <multi> * <unary> | <multi> % <unary> | <uniary>
     ((and (list? exp) (or (eq? (car exp) '%) (or (eq? (car exp) '*) (eq? (car exp) '/))))
      (multi exp state)
      )
     ; <arith> -> <arith> + <multi> | <arith> - <multi> | <multi>
     ((and (list? exp) (or (eq? (car exp) '+) (eq? (car exp) '-)))
      (arith exp state)
      )
     ; <comp> -> <comp> < <arith>| <comp> > <arith>| <comp> ≤ <arith>| <comp> ≥ <arith>| <arith>
     ((or (eq? (car exp) '<) (or (eq? (car exp) '>) (or (eq? (car exp ) '<= ) (eq? (car exp) '>=))))
      (comp exp state)
      )
     ;<eql> -> <eql> == <comp> | <eql> != <comp> | <comp>
     ((and (list? exp) (or (eq? (car exp) '==) (eq? (car exp) '!=)))
      (eql exp state)
      )
     ;<and> -> <and> && <eql> | <eql>
     ((and (list? exp) (eq? (car exp) '&&))
      (and (expressionEval (cadr exp) state) (expressionEval (caddr exp) state))
      )
     ;<or> -> <or> || <and> | <and>
     ((and (list? exp) (eq? (car exp) '||))
      (or (expressionEval (cadr exp) state) (expressionEval (caddr exp) state))
      )
     )))


(define eql
  (lambda (exp state)
    (cond
      ((eq? (car exp) '==)(eq? (expressionEval(cadr exp) state) (expressionEval(caddr exp) state)))
      (else (not (eq? (expressionEval(cadr exp) state) (expressionEval(caddr exp) state))))
      )))

(define comp
  (lambda (exp state)
    (cond
      ((eq? (car exp) '<)(< (expressionEval(cadr exp) state) (expressionEval(caddr exp) state)))
      ((eq? (car exp) '>)(> (expressionEval(cadr exp) state) (expressionEval(caddr exp) state)))
      ((eq? (car exp) '<=)(<= (expressionEval(cadr exp) state) (expressionEval(caddr exp) state)))
      (else (>= (expressionEval(cadr exp) state) (expressionEval(caddr exp) state)))
     )))

(define multi
  (lambda (exp state)
    (cond
      ((eq? (car exp) '*) (* (expressionEval(cadr exp) state) (expressionEval(caddr exp) state)))
      ((eq? (car exp) '%) (remainder (expressionEval(cadr exp) state) (expressionEval(caddr exp) state)))
      (else (exact-truncate (/ (expressionEval(cadr exp) state) (expressionEval(caddr exp) state))))
    )))

(define arith
  (lambda (exp state)
    (cond
      ((eq? (car exp) '+) (+ (expressionEval(cadr exp) state) (expressionEval(caddr exp) state)))
      (else (- (expressionEval(cadr exp) state) (expressionEval(caddr exp) state)))
      )))



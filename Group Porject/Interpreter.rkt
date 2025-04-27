;Ethan Tobey, Shourya Poddar

#lang racket
(require "lex.rkt")
(require "classParser.rkt")
(provide interpret)


;main function of the program
;(define interpret
;  (lambda (filename)
;    (S_methodPass filename (lambda (globalState)   ;this should call classPass
;                             (V_callFunction 'main '() globalState (lambda (v) v) #f)))))  ;call main function on the state compiled by global pass

(define interpret
  (lambda (filename className)
    ;; 1) bind all the classes
    (S_classPass filename
      (lambda (classState)
        ;; 2) look up the requested class’s closure
        (V_get className classState
          (lambda (inputClassClosure)           
            ;; cc is a list: (parent instState)
            ;(let ((instState  (cadr inputClassClosure)))     ;we should not do this - instead, pass whole closure as compileTimeType
              ;; 3) invoke its `main` in that instance‐state
              (V_callFunction 'main
                              '()           ; no args
                              classState    ; the state holding that class’s vars & methods
                              '()           ;null reference to 'this' b/c main is static
                              inputClassClosure     ;the compile time type
                              (lambda (v) v)    ; identity continuation
                              #f))
            ))
           )))     ; no throw‐k
    
;----------------------Global Class Pass------------------

;1. define S_classPass
     ;this function should go through outer level of parse and run through list of class declarations
     ;probably have a classDeclarationList -> classDeclarationEval methods

;2. define classDeclarationEval
    ; this should call bindClass

;3. define bindClass
     ;this should create class binding, will need to call createClassClosure

(define S_classPass
  (lambda (filename return)
    (S_classDeclarationList (parser filename)
                            '(())      ; <- start empty
                            return)))

(define S_classDeclarationList
  (lambda (decls state return)
    (if (null? decls)
        (return state)
        (let ((d (car decls)))
          (if (and (list? d) (eq? (car d) 'class))
              (classDeclarationEval d state
                (lambda (st)
                  (S_classDeclarationList (cdr decls) st return)))
              (S_classDeclarationList (cdr decls) state return))))))

(define classDeclarationEval
  (lambda (classAST state return)
    (bindClass classAST state return)))

(define bindClass
  (lambda (classAST state return)
    (createClassClosure
      classAST
      '()                    ; ← fresh state for *this* class’s members
      (lambda (raw-cc)
        (S_addBinding
         (cadr classAST)     ; bind under the class’s own name
         raw-cc              ; the closure you just built
         state               ; the *global* class‐pass state
         return))))) 

;-----------------------Class Closures--------------------------


;4. define createClassClosure
      ; class closure should contain:
         ;a)parent/super class
         ;b) list of instance field names and expressions that compute their initial values (if any)
         ;c) list of method names and their closures
         ;      - doing this will likely require calling S_methodPass (adjusted) inside the class

;(define createClassClosure
;  (lambda (class-body state return)
    ;; a class‐closure is just a list:
    ;;   '(class parent (list inst-fields inst-methods))
;    (return (getParentName (caddr class-body) (lambda(v1) (list (S_classInternalDeclarationList (cadddr class-body)  state (lambda(v2) (list (list v1)  v2)))))))))

;updated version of createClassClosure with fixed CPS
(define createClassClosure
  (lambda (class-body state return)
    (getParentName (caddr class-body)
      (lambda (parent)
        (S_classInternalDeclarationList (cadddr class-body) state
          (lambda (methAndVars)
            (return (list parent methAndVars))))))))


;;make this parent closure for easy access
(define getParentName
  (lambda (class-info return)
    (if (null? class-info)         
        (return '())          ;if no parent class
        (return (cadr (caddr class-info))))))

;not currently being used
(define getInstFieldsAndMethods
  (lambda (body state return)
    (cond
      ((null? body) state)
      (else (S_classInternalDeclarationEval (car body) state (lambda (s)
                                                                       (getInstFieldsAndMethods (cdr body) s return)))))))



;--------------------- Class Internal Declarations-------------------

;first pass of file execution - binds all functions and class variables and methods to state
;not currenlty being used
(define S_methodPass
  (lambda (filename return)
    (S_classInternalDeclarationList (parser filename) (list '()) return)))

;top level of global declarations - runs global declarations in declaration list in order
(define S_classInternalDeclarationList
  (lambda (lst state return)
    (cond
      ((null? lst) (return state))
      (else (S_classInternalDeclarationEval (car lst) state (lambda (s)
                                                       (S_classInternalDeclarationList (cdr lst) s return)))))))

;abstraction for declare type
(define dclrType car)
(define dclrBody cdr)

;processes individual class internal declaration
(define S_classInternalDeclarationEval
  (lambda (declaration state return)
    (cond
      ((or (eq? (dclrType declaration) 'function) (eq? (dclrType declaration) 'static-function))
       (S_bindFunction (dclrBody declaration) state return))
      ((eq? (dclrType declaration) 'var)      (declareStatement (dclrBody declaration) state return #f))
      (else (return state))
      ;todo - nested classes?
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
    (V_makeFunctionClosure (params f) (body f) (name f) state
      (lambda (closure) 
        (S_addBinding (name f) closure state return)))))

; Process a variable declaration (var) by only looking at the top (current) block.
(define S_declareAssign
  (lambda (name exp state return throw-k)
    (if (eq? exp 'undefined)
        (V_declaredVarInCurrentBlock? name state
          (lambda (declared?)
            (if declared?
                (S_replaceBindingInCurrentBlock name 'undefined state return)
                (S_addBinding name 'undefined state return))))
        (V_expressionEval exp state
          (lambda (val)
            (V_declaredVarInCurrentBlock? name state
              (lambda (declared?)
                (if declared?
                    (S_replaceBindingInCurrentBlock name val state return)
                    (S_addBinding name val state return)))))
          throw-k))))


;generate function closure
(define V_makeFunctionClosure
  (lambda (params body fname localState return)
    (return
     (list params body 
           (lambda (args selfClosure closureReturn)
             (S_bindParamsToArgs params args localState
               (lambda (paramState)
                 (S_addBinding fname selfClosure paramState closureReturn))))))))

;helper to bind list of formal params to function arguments
;ASSUMPTION - params and args are same length
(define S_bindParamsToArgs
  (lambda (params args localState return)
    (cond
      ((and (null? params) (null? args))
       (return localState))
      ((null? params)
       (error "Too many arguments provided"))
      ((null? args)
       (error "Too few arguments provided"))
      (else
       (S_addBinding (car params) (car args) localState
         (lambda (updatedState)
           (S_bindParamsToArgs (cdr params) (cdr args) updatedState return)))))))

;--------------------- Function Execution-------------------
;abstraction to get formal params from function closure - currently not needed
(define cParams car)
;abstraction to get body from function closure
(define cBody cadr)
;abstraction to get environment from function closure
(define cEnvFunction caddr)

;get runtime type from instance closure
(define runTimeType car)

;calls a function and returns its output value or nothing
(define V_callFunction
  (lambda (fname argExpressions state this cmplTimeType return throw-k)
    ;1. get object calling method on (this) - recall this is an instance closure, which has: a) type, b)instance field value
    ;2. get runtime type of this
    (if (null? this)
        (V_get fname cmplTimeType    ;if calling static (for main)
               (lambda (closure)
                 (V_evaluateArgs argExpressions state ;I think we eval in this state..?
                                 (lambda (args)
                                   (V_evaluateFunction args closure state cmplTimeType return throw-k)) throw-k))) ;New - pass compile time type to evalFunction
 
        (V_get fname (runTimeType this)     ;3. get method closure from runtimeType (change from state)
               (lambda (closure)
                 (V_evaluateArgs argExpressions state ;I think we eval in this state..?
                                 (lambda (args)
                                   (V_evaluateFunction args closure state cmplTimeType return throw-k)) throw-k))) ;New - pass compile time type to evalFunction
        )))
    

;helper to evaluate method arguments expressions and return list of their values
(define V_evaluateArgs
  (lambda (argExprs state return throw-k)
    (if (null? argExprs)
        (return '())
        (V_expressionEval (car argExprs) state 
          (lambda (v1)
            (V_evaluateArgs (cdr argExprs) state (lambda (v2)
                                                   (return (cons v1 v2))) throw-k))
          throw-k))))

; Check if a variable is declared in a single frame (the current block)
(define V_declaredInFrame?
  (lambda (name frame)
    (cond
      ((null? frame) #f)
      ((or (eq? name (car frame)) (and (list? (car frame))(eq? name (caar frame)))) #t)
      (else (V_declaredInFrame? name (cdr frame))))))

(define V_searchFrame
  (lambda (name bindings return)
    (cond
      ((null? bindings) (error "Should not happen: variable declared but binding not found"))
      ((eq? name (car bindings)) (return (cadr bindings))); foun
      ((eq? name (caar bindings)) (return (cadar bindings))) ; found it!
      (else (V_searchFrame name (cdr bindings) return)))))


(define V_declaredVarInCurrentBlock?
  (lambda (name state return)
    (if (null? state)
        (return #f)
        ; state’s car is the top frame.
        (return (V_declaredInFrame? name (car state))))))


; Helper: replace binding in one frame.
(define V_replaceInFrame
  (lambda (name val frame)
    (cond
      ((null? frame) '())
      ((eq? name (caar frame))
       (cons (list name (box val)) (cdr frame)))
      (else (cons (car frame) (V_replaceInFrame name val (cdr frame)))))))

; Replace binding for name in the current (top) frame only.
(define S_replaceBindingInCurrentBlock
  (lambda (name val state return)
    (if (null? state)
        (error "No block frame available")
        (let ((top (car state))
              (rest (cdr state)))
          (return (cons (V_replaceInFrame name val top) rest))))))

;evaluates a function given its closure - helper
(define V_evaluateFunction
  (lambda (args closure state cmplTimeType return throw-k)   ;compileTimeType passed but not used for now...
    (return 
     (call/cc
      (lambda (exit)
        (S_codeBlockStatement (cBody closure)
                               ((cEnvFunction closure) args closure (lambda (v) v))
                              (lambda (v) v)
                              exit #f #f throw-k))))))  ;call statement list on body of function


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
      ((eq? (stmtType statement) 'return)
       (V_returnStatement (stmtBody statement) state exit throw-k))
      ((eq? (stmtType statement) 'funcall)
       (V_callFunction (fName statement) (fParams statement) state (lambda (ignore) (return state)) throw-k))
      ;; NEW clause: Nested function declaration
      ((eq? (stmtType statement) 'function)
       (S_bindFunction (stmtBody statement) state return))
      ((eq? (stmtType statement) 'var)
       (declareStatement (stmtBody statement) state return throw-k))
      ((eq? (stmtType statement) '=)
       (S_assignStatement (stmtBody statement) state return throw-k))
      ((eq? (stmtType statement) 'if)
       (S_ifStatement (stmtBody statement) state return exit break-k continue-k throw-k))
      ((eq? (stmtType statement) 'while)
       (S_whileStatement (stmtBody statement) state return exit throw-k))
      ((eq? (stmtType statement) 'begin)
       (S_codeBlockStatement (stmtBody statement) state return exit break-k continue-k throw-k))
      ((eq? (stmtType statement) 'break)
       (if break-k (break-k state) (error "'break' used outside of loop")))
      ((eq? (stmtType statement) 'continue)
       (if continue-k (continue-k state) (error "'continue' used outside of loop")))
      ((eq? (stmtType statement) 'throw)
       (if throw-k (SV_throw (cadr statement) state throw-k)
           (error "'throw' used outside of try")))
      ((eq? (stmtType statement) 'try)
       (S_tryCatchFinallyStatement (stmtBody statement) state return exit break-k continue-k throw-k))
      )))
      

;----------------------------- Code Block -----------------------------

;handle entering and exiting code blocks
(define S_codeBlockStatement
  (lambda (stmts state return exit break-k continue-k throw-k)
    (S_statementList stmts (cons '() state)
      (lambda (v) (return (cdr v))) exit break-k continue-k throw-k)))


;--------------------- Throw Statement -------------------

(define SV_throw
  (lambda (value state throw-k)
    (V_expressionEval value state (lambda (v) (throw-k state v)) throw-k)))

; In S_statementEval, handle throw as follows:


;--------------------- Try Catch Finally Statements -------------------

(define try-block
  (lambda (stmts) (car stmts)))

(define catch-block
  (lambda (stmts) (cadr stmts)))  ; Returns the entire catch clause: (catch (e) ((= x e)))

(define has-finally?
  (lambda (stmts) (not (null? (caddr stmts)))))  ; True if a finally clause exists.

(define finally-block
  (lambda (stmts) (caddr stmts)))  ; Returns the entire finally clause: (finally ((= x (+ x 100))))

(define S_tryCatchFinallyStatement
  (lambda (stmts state return exit break-k continue-k throw-k)
    (S_statementList (try-block stmts) state
      ;; Normal completion:
      (lambda (try-state)
        (if (has-finally? stmts)
            (S_statementList (cadr (finally-block stmts))
                             try-state return exit break-k continue-k throw-k)
            (return try-state)))
      exit break-k continue-k
      ;; Exception caught:
      (lambda (state-k e)
        (S_addBinding (car (cadr (catch-block stmts))) e state
          (lambda (state-with-exn)
            (S_statementList (caddr (catch-block stmts))
                             state-with-exn
                             (lambda (catch-state)
                               (if (has-finally? stmts)
                                   (S_statementList (cadr (finally-block stmts))
                                                    catch-state return exit break-k continue-k throw-k)
                                   (return catch-state)))
                             exit break-k continue-k throw-k)))))))

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
                           ) throw-k)
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
                              (S_statementEval (caddr stmts) state return exit break-k continue-k throw-k)))) throw-k)))

;--------------------------- Return -----------------------------------

;handle returning
(define V_returnStatement
  (lambda (stmt state exit throw-k)
    (V_expressionEval (car stmt) state
                      (lambda (v)
                        (cond
                          ((eq? #t v) (exit 'true))
                          ((eq? #f v) (exit 'false))
                          (else (exit v))))
                      throw-k))) ; <-- jump directly to top-level

;--------------------------- Declare ----------------------------------

;handle declaration
(define declareStatement
  (lambda (statement state return throw-k)
    (if (null? (cdr statement))
        (S_declareAssign (car statement) 'undefined state return throw-k)
        (S_declareAssign (car statement) (cadr statement) state return throw-k))))


;--------------------------- Assigment --------------------------------

;handle assignment statement
(define S_assignStatement
  (lambda (stmt state return throw-k)
    (if (V_declaredVar? (car stmt) state (lambda (v) v))
        (S_assign (car stmt) (cadr stmt) state return throw-k)
        (error "variable not declared: " (car stmt)))))

;helper to check if variable declared
(define V_declaredVar?
  (lambda (name state cont)
    (if (null? state)
        (cont #f)
        (if (V_declaredInFrame? name (car state))
            (cont #t)
            (V_declaredVar? name (cdr state) cont)))))

;function to do an assignmnet
(define S_assign
  (lambda (name exp state return throw-k)
    (cond
      ((null? name)
       (return (error "No name given")))
      (else (V_expressionEval exp state
                    (lambda (val)
                      (if (V_declaredVar? name state (lambda (v) v))
                          (S_replaceBinding name val state return)
                          (S_addBinding name val state return)))
                    throw-k)))))

;--------------------------- State ------------------------------------
; Sample State ((x 10) (y 9) (z true))
; Sample State with Code Block : ( ( (x 10) (y 9) ) (z true) )

(define S_replaceBinding
  (lambda (name val state return)
    (S_replaceBinding* name val state
      (lambda (new-state)
        (remove-updated-cps new-state return)))))   ;in case of uncaught updated flag lingering in state

(define remove-updated-cps
  (lambda (lst return)
    (cond
      ;; 1) empty list → pass ’() to the continuation
      [(null? lst)
       (return '())]

      ;; 2) head is ’updated → skip it, recurse on the tail with the same continuation
      [(eq? (car lst) 'updated)
       (remove-updated-cps (cdr lst) return)]

      ;; 3) head is a sublist → clean it first, then clean the tail,
      ;;    and finally cons the two results
      [(pair? (car lst))
       (remove-updated-cps (car lst)
         (lambda (clean-head)
           (remove-updated-cps (cdr lst)
             (lambda (clean-tail)
               (return (cons clean-head clean-tail))))))]

      ;; 4) any other element → keep it, recurse on tail, then cons
      [else
       (remove-updated-cps (cdr lst)
         (lambda (clean-tail)
           (return (cons (car lst) clean-tail))))])))

;update a binding without changing order of state
(define S_replaceBinding*
  (lambda (name val state return)
    (cond
      ((null? state) (return '()))
      ((and (list? (car state)) (or (null? (car state)) (list? (caar state))))
            (S_replaceBinding* name val (car state) (lambda (v1)
                                                    (if (and (pair? v1) (eq? (car v1) 'updated))  ;check if this created the update
                                                         (return (cons (cdr v1) (cdr state)))    ;if so, stop calling S_replaceBinding on cdr and return updated state (with flag removed)
                                                         (S_replaceBinding* name val (cdr state) (lambda (v2)
                                                                                                      (return (cons v1 v2))))))))
      ((eq? name (caar state)) (return (begin (set-box! (cadar state) val) (cons 'updated (cons (list name (cadar state)) (cdr state)))))) ;add 'updated flag so we know to stop recursing

      (else (S_replaceBinding* name val (cdr state) (lambda (rest)
                                                     (if (and (pair? rest) (eq? (car rest) 'updated))  ;check if we processed the update
                                                              (return (cons 'updated (cons (car state) (cdr rest))))   ;if we processed update, move flag to front of returned state segment
                                                              (return (cons (car state) rest)))))))
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
    (if (null? state)
        (return '())  ; variable not found
        (let ((frame (car state)))
          (if (V_declaredInFrame? name frame)
              ;; Search this frame's bindings recursively
              (V_searchFrame name frame
                             (lambda (val)
                               (if (null? val)
                                   (error "Variable not assigned value")
                                   (return (unbox val)))))
              ;; Else, move to the outer frame
              (V_get* name (cdr state) return))))))




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
  (lambda (exp state return throw-k)
    (cond
      ((null? exp) (return null))
      ((number? exp) (return (exact-truncate exp)))
      ((eq? exp 'true) (return #t))
      ((eq? exp 'false) (return #f))
      ((symbol? exp) (V_get exp state (lambda (v) (return v))))
      ; Unary operations:
      ((and (list? exp) (and (eq? (car exp) '-) (null? (cddr exp))))
       (V_expressionEval (cadr exp) state (lambda (v) (return (- v))) throw-k))
      ((and (list? exp) (eq? (car exp) '!))
       (V_expressionEval (cadr exp) state (lambda (v) (return (not v))) throw-k))
      ; Multiply/divide/mod:
      ((and (list? exp) (or (eq? (car exp) '%)
                            (or (eq? (car exp) '*) (eq? (car exp) '/))))
       (V_multi exp state return throw-k))
      ; Plus and minus:
      ((and (list? exp) (or (eq? (car exp) '+) (eq? (car exp) '-)))
       (V_arith exp state return throw-k))
      ; Comparisons:
      ((or (eq? (car exp) '<)
           (or (eq? (car exp) '>)
               (or (eq? (car exp) '<=)
                   (eq? (car exp) '>=))))
       (V_comp exp state return throw-k))
      ; Equality:
      ((and (list? exp) (or (eq? (car exp) '==) (eq? (car exp) '!=)))
       (V_eql exp state return throw-k))
      ; And/Or:
      ((and (list? exp) (eq? (car exp) '&&))
       (V_expressionEval (cadr exp) state
         (lambda (v1)
           (V_expressionEval (caddr exp) state (lambda (v2) (return (and v1 v2))) throw-k))
         throw-k))
      ((and (list? exp) (eq? (car exp) '||))
       (V_expressionEval (cadr exp) state
         (lambda (v1)
           (V_expressionEval (caddr exp) state (lambda (v2) (return (or v1 v2))) throw-k))
         throw-k))
      ; Function call:
      ((eq? (car exp) 'funcall)
       (V_callFunction (fName exp) (fParams exp) state return throw-k)) ;will never be calling functions without dot operator anymore... so the dot operation is what should be passing us the info on type
                                                                            ;if this is the case, our exp goes from funCall -> obj.funCall. In this case, we should call our dot operator code from here, and that will extract the type to run V_callFunction
                                                                    ;**Maybe (see above first) TODO: params should now be name, args, state, this, compileTimeType, return throw-k
      )))

;evaluate equality 
(define V_eql
  (lambda (exp state return throw-k)
    (cond
      ((eq? (car exp) '==)
       (V_expressionEval (cadr exp) state 
         (lambda (v1)
           (V_expressionEval (caddr exp) state (lambda (v2)
                                                   (return (eq? v1 v2))) throw-k))
         throw-k))
      (else
       (V_expressionEval (cadr exp) state 
         (lambda (v1)
           (V_expressionEval (caddr exp) state (lambda (v2)
                                                   (return (not (eq? v1 v2))) ) throw-k))
         throw-k)))))

;evaluate comparison
(define V_comp
  (lambda (exp state return throw-k)
    (cond
      ((eq? (car exp) '<)
       (V_expressionEval (cadr exp) state
         (lambda (v1)
           (V_expressionEval (caddr exp) state
             (lambda (v2)
               (return (< v1 v2))) 
             throw-k))
         throw-k))
      ((eq? (car exp) '>)
       (V_expressionEval (cadr exp) state
         (lambda (v1)
           (V_expressionEval (caddr exp) state
             (lambda (v2)
               (return (> v1 v2))) 
             throw-k))
         throw-k))
      ((eq? (car exp) '<=)
       (V_expressionEval (cadr exp) state
         (lambda (v1)
           (V_expressionEval (caddr exp) state
             (lambda (v2)
               (return (<= v1 v2))) 
             throw-k))
         throw-k))
      (else
       (V_expressionEval (cadr exp) state
         (lambda (v1)
           (V_expressionEval (caddr exp) state
             (lambda (v2)
               (return (>= v1 v2))) 
             throw-k))
         throw-k)))))

;evaulate multiplication
(define V_multi
  (lambda (exp state return throw-k)
    (cond
      ((eq? (car exp) '*)
       (V_expressionEval (cadr exp) state 
         (lambda (v1)
           (V_expressionEval (caddr exp) state 
             (lambda (v2) (return (* v1 v2))) throw-k))
         throw-k))
      ((eq? (car exp) '%)
       (V_expressionEval (cadr exp) state 
         (lambda (v1)
           (V_expressionEval (caddr exp) state 
             (lambda (v2) (return (remainder v1 v2))) throw-k))
         throw-k))
      (else 
       (V_expressionEval (cadr exp) state 
         (lambda (v1)
           (V_expressionEval (caddr exp) state 
             (lambda (v2) (return (exact-truncate (/ v1 v2)))) throw-k))
         throw-k)))))

;evaluate arithmetic (+ -)
(define V_arith
  (lambda (exp state return throw-k)
    (cond
      ((eq? (car exp) '+)
       (V_expressionEval (cadr exp) state 
         (lambda (v1)
           (V_expressionEval (caddr exp) state (lambda (v2)
                                                   (return (+ v1 v2))) throw-k))
         throw-k))
      (else
       (V_expressionEval (cadr exp) state 
         (lambda (v1)
           (V_expressionEval (caddr exp) state (lambda (v2)
                                                   (return (- v1 v2))) throw-k))
         throw-k)))))
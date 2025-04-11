#lang racket
(require "Interpreter.rkt")
(require "simpleParser.rkt")

(define ubd_error "ERROR (Usage before declaring)")
(define uba_error "ERROR (Usage before assignment)")
(define ubr_error "ERROR (redeclaration)")
(define result_list (list 20 164 32 2 ubd_error 25 21 6 -1 789 ubd_error ubd_error uba_error 12 125 110 2000400 101 ubd_error))

(define dotest #t)
(define testnumstring "")
(define testnum 0)
(define testfile "")

(define testproject
  (lambda ()
    (displayln "Enter test number (1-20) OR")
    (displayln "Enter -1 for custom test (test.txt)")
    (set! testnum (read))
    (cond
      ((< testnum 0) (set! testnumstring ""))
      ((< testnum 10) (set! testnumstring (string-append "0" (number->string testnum))))
      (else (set! testnumstring (number->string testnum))))

    (set! testfile (string-append "tests2/test" testnumstring ".txt"))

    (displayln "Parser Result: ")
    (displayln (parser testfile))

    (if (> testnum 0)
      (begin
        (displayln "Expected Result: ")
        (displayln (list-ref result_list (- testnum 1))))
        (displayln "No expected result for custom test"))

    (displayln "Interpreter Result: ")
    (displayln(interpret testfile))

    (displayln "Run another test? (y/n)")
    (if (not (eq? (read) 'y))
      (set! dotest #f)
      (testproject))))

(testproject)

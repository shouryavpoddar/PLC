#lang racket
(require "Interpreter.rkt")
(require "functionParser.rkt")

(define mpa_error "ERROR (Mismatched parameters and arguments)")
(define oos_error "ERROR (Out of scope)")
(define ubr_error "ERROR (redeclaration)")
(define result_list (list 10 14 45 55 1 115 'true 20 24 2 35 mpa_error 90 69 87 64 oos_error 125 100 2000400 3421 20332 21))

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

    (set! testfile (string-append "tests3/test" testnumstring ".txt"))

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

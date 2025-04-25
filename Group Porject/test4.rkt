#lang racket
(require "Interpreter.rkt")
(require "classParser.rkt")

(define nothis_error "ERROR (This is not defined in current scope)")
(define absmeth_error "ERROR (Non overridden abstract method)")
(define result_list (list 15 12 125 36 54 110 26 117 32 15 123456 5285 -716 530 66 1026 2045 20 530 615 16 100 420 nothis_error 300 absmeth_error 417 10 48 1629))

(define dotest #t)
(define testnumstring "")
(define testnum 0)
(define testfile "")

(define testproject
  (lambda ()
    (displayln "Enter test number (1-13, 21-24, 31-37, 41-42, 51-54) OR")
    (displayln "Enter -1 for custom test (test.txt)")
    (set! testnum (read))
    (cond
      ((< testnum 0) (set! testnumstring ""))
      ((< testnum 10) (set! testnumstring (string-append "0" (number->string testnum))))
      (else (set! testnumstring (number->string testnum))))

    (set! testfile (string-append "tests4/test" testnumstring ".txt"))

    (displayln "Parser Result: ")
    (displayln (parser testfile))

    ; Adjusted index for result_list
    (cond
      ((> testnum 0)
       (let ((adjusted-testnum
              (cond
                ((<= testnum 13) testnum)
                ((<= testnum 24) (- testnum 7)) ; skip 14-20
                ((<= testnum 37) (- testnum 13)) ; skip 14-20, 25-30
                ((<= testnum 42) (- testnum 16)) ; skip more
                ((<= testnum 54) (- testnum 24)) ; and more
                (else testnum)))) ; fallback

         (displayln "Expected Result: ")
         (displayln (list-ref result_list (- adjusted-testnum 1)))))
      (else
       (displayln "No expected result for custom test")))

    (displayln "Interpreter Result: ")
    (displayln (interpret testfile))

    (displayln "Run another test? (y/n)")
    (if (not (eq? (read) 'y))
        (set! dotest #f)
        (testproject))))

(testproject)


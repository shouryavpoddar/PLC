#lang racket
;> (multiplyby 5 '(1 2 3 4 10 11))
;(5 10 15 20 50 55)

(define multiplyby
  (lambda (x lst)(
                 cond
                  ((null? lst) '())
                  (else (cons (* x (car lst)) (multiplyby x (cdr lst)))))))

;> (maxnumber '(3 1 5 2 7 5 3 8 1 2))
;8

(define maxnumber
  (lambda (lst)(
                cond
                 ((null? lst) 0)
                 ((null? (cdr lst))(car lst))
                 ((> (car lst ) (maxnumber(cdr lst))) (car lst))
                 (else (maxnumber(cdr lst))))))

;> (removelast '(a b c d e))
;(a b c d)

(define removelast
  (lambda (lst)(
                cond
                 ((null? lst)'())
                 ((null? (cdr lst)) '())
                 (else (cons (car lst) (removelast(cdr lst)))))))

;> (crossmultiply '(1 2 3) '(3 0 2))
;((3 0 2) (6 0 4) (9 0 6))
;> (crossmultiply '(8 -1 4 3) '(3 1))
;((24 8) (-3 -1) (12 4) (9 3))

; if both empty then just zero (base case)
; if car of lst1 real then multiply it with car of lst2 and append to new list

(define crossmultiply
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) '())
      (else (cons (multiplyby (car lst1) lst2) (crossmultiply (cdr lst1) lst2))))))


(define multiplyby
  (lambda (x lst)(
                 cond
                  ((null? lst) '())
                  (else (cons (* x (car lst)) (multiplyby x (cdr lst)))))))


;> (interleave3 '(a b c d) '(1 2) '(P Q R S T U))
;(a 1 P b 2 Q c R d S T U)


(define interleave3
  (lambda (lst1 lst2 lst3)
    (cond
      ((and (null? lst1) (null? lst2) (null? lst3)) '())
      ((null? lst1) (interleave3 lst2 lst3 '()))
      (else
       (cons (car lst1) (interleave3 lst2 lst3 (cdr lst1)))))))

;> (reverse* '(a b (c d e) ((f g) h)))
;((h (g f)) (e d c) b a)

(define reverse*
  (lambda (lst)
    (cond
      ((null? lst)'())
      ((list? (car lst)) (append (reverse* (cdr lst)) (list (reverse* (car lst)))))
      (else
       (append (reverse* (cdr lst)) (list (car lst)))))))

;> (reverselists '(a b c d))
;(a b c d)
;> (reverselists '(a b (c d e f) g h (i j k l)))
;(a b (f e d c) g h (l k j i))
;> (reverselists '(a b (c (d e) f) g h ((i (j k)) l)))
;(a b (f (d e) c) g h (l (i (k j))))

(define reverselistshelper
  (lambda (lst) (
                 if(null? lst)
                   '()
                   (append (myreverse (cdr lst)) (list (car lst))))))

(define reverselists
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((list? (car lst)) (append (list (reverselistshelper (car lst))) (reverselists(cdr lst))))
      (else
       (append (list (car lst)) (reverselists (cdr lst)))))))

;> (trimatoms '(a b c) '(1 2 3 4))
;(4)
;> (trimatoms '(((a)) (b ((c)))) '(1 2 3 4 5))
;(4 5)
;> (trimatoms '(((a)) () () (b ((c)))) '(1 2 3 4 5))
;(4 5)

(define trimatoms
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) lst2)
      ((list? (car lst1)) (trimatoms (cdr lst1) (trimatoms (car lst1) lst2)))
      (else (trimatoms (cdr lst1) (cdr lst2))))))

;> (partialsums* '(1 a 2 b))
;(3)
;> (partialsums* '((1) (2) (3)))
;(6 (1) (2) (3))
;> (partialsums* '((1 2) (10 20) (100 200)))
;(333 (3) (30) (300))
;> (partialsums* '(1 a (10 20 x y) c 2 (x y z) (a b 1 (c 200 d))))
;(234 (30) (0) (201 (200)))
(define partialsums*
  (lambda (lst)
    (cond
      ((null? lst) '(0))
      ((list? (car lst))
       (cons (+ (car (partialsums* (car lst))) (car (partialsums* (cdr lst)))) 
             (cons (partialsums* (car lst)) (cdr (partialsums* (cdr lst))))))
      ((number? (car lst))
       (cons (+ (car (partialsums* (cdr lst))) (car lst)) (cdr (partialsums* (cdr lst)))))
      (else
       (partialsums* (cdr lst))))))

;> (exchange '(((a)) (b ((c)))) '(1 2 3))
;(((1)) (2 ((3))))
;> (exchange '(((() a) b) c) '(1 2 3))
;(((() 1) 2) 3)

(define exchange-helper
  (lambda (lst atoms)
    (cond
      ((null? lst) (cons '() atoms))
      ((list? (car lst))
       (cons
        (cons (car (exchange-helper (car lst) atoms))
              (car (exchange-helper (cdr lst) (cdr (exchange-helper (car lst) atoms)))))
        (cdr (exchange-helper (cdr lst) (cdr (exchange-helper (car lst) atoms))))))
      (else
       (cons
        (cons (car atoms) (car (exchange-helper (cdr lst) (cdr atoms)))) 
        (cdr (exchange-helper (cdr lst) (cdr atoms))))))))

(define exchange
  (lambda (lst atoms)
    (car (exchange-helper lst atoms))))

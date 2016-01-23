#lang planet neil/sicp

; Q. What happens if we try to compute (last-pair z)?
; A. This makes a circular data structure. (last-pair z) does an infinite loop!

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define a-list (list 1 2 3))
(define b-list (list 4 5 6))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(make-cycle a-list)

(define (return-n n l)
  (cond
    [(= n 0) (car l)]
    [else (cons (car l) (return-n (- n 1) (cdr l)))]))

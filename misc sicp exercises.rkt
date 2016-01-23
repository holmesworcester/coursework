#lang racket

(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)
 
(define (car z) (z 0))
 
(define (cdr z) (z 1))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))

(define two
  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) f) x)))))

(equal? (add-1 one) two)

(add-1 one)
two
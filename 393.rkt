;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |393|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define inex (+ 1 #i1e-12))
(define exac (+ 1 1e-12))

; Number, Number -> Number
; raises a number x to the exponent e

(check-expect (my-expt 2 2) 4)
(check-expect (my-expt 2 -1) 1/2)
(check-expect (my-expt 2 -2) 1/4)
(check-expect (my-expt 2 3) 8)
(check-expect (my-expt 2 0) 1)

(define (my-expt x e)
  (local (; NumberZeroOrGreater -> Number
          ; does exponents on an x for greater than zero inputs
          (define (pos-expt e)
            (cond
              [(zero? e) 1]
              [else (* x (pos-expt (sub1 e)))])))
    ; -IN-
    (if (>= e 0) (pos-expt e) (/ 1 (pos-expt (* e -1)))))) ; a little special case for negative numbers.

(my-expt inex 30)
(my-expt exac 30)


(expt inex 30)
(expt exac 30)

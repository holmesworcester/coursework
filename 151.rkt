;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |151|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A N is one of: 
; – 0
; – (add1 N)
; interpretation represents the natural numbers or counting numbers

; N -> Number
; computes (+ n pi) without using +
 
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n) (add1 (add-to-pi (sub1 n)))]))

; test uses check-within because (+ 3 pi) doesn't return an exact number.

; N, N -> Number

(check-within (add 3 pi) (+ 3 pi) 0.001)

(define (add n x)
  (cond
    [(zero? n) x]
    [(positive? n) (add1 (add (sub1 n) x))]))

(add-to-pi 3)
(add 3 pi)
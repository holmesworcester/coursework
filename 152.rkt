;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |152|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
; adds a N n to an N x without using +

(check-within (add 3 pi) (+ 3 pi) 0.001)

(define (add n x)
  (cond
    [(zero? n) x]
    [(positive? n) (add1 (add (sub1 n) x))]))

(add-to-pi 3)
(add 3 pi)

; N, N -> Number
; multiply an N x by an N y without using *
; that is, it must do (add x x) y times.
; 

(check-expect (multiply 3 3) 9)
(check-expect (multiply 1 3) 3)
(check-expect (multiply 100 0) 0)

(define (multiply x y)
  (cond
    [(zero? y) 0] 
    [(positive? y) (add x (multiply x (sub1 y)))]))
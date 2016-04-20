;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |432|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define EPSILON 0.1)
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
 
(check-within (integrate (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)
 
(define (integrate f a b)
  (/ (* (- b a)(+ (f a) (f b))) 2))

; the polynomial fails beause it's curvy.

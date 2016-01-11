;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |419|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; N[>= 1] N[>= 1] -> N[>= 1]
; finds the largest common divisor

; (check-expect (gcd-structural 101135853 45014640) 177) ; can't even handle it!!
(check-expect (gcd-structural 64 128) 64)
(check-expect (gcd-structural 64 96) 32)

(define (gcd-structural small large)
  (largest-common (divisors small small) (divisors small large)))
 
; N[>= 1] N[>= 1] -> [List-of N]
; computes the list of divisors of l smaller or equal to k

(check-expect (divisors 8 4) '(1 2 4))

(define (divisors k l)
  (filter (lambda(x) (= 0 (remainder l x))) (build-list k (lambda (x) (+ 1 x))))) ; this is ugly but i'm trying to get more comfy with abstraction so...

; [List-of N] [List-of N] -> N
; finds the largest number common to both k and l

(check-expect (largest-common '(1 2 4 3) '(2 4)) 4)

(define (largest-common k l)
  (local (; [List-of N] -> Number
          ; finds the largest number in a list of numbers
          (define (largest l)
            (foldr max 0 l))
          ; [List-of N] [List-of N] -> [List-of N]
          ; returns the common items in two lists
          (define (common l1 l2)
            (filter (lambda (x) (member? x l2)) l1)))
    ;-IN- 
  (largest (common k l))))
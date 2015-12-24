;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |387|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(check-within (add 0) 0 0.0001)
(check-within (add 1) 1/185 0.0001)

(define (add n)
  (cond
    [(zero? n) 0]
    [else (+ #i1/185 (add (sub1 n)))]))

; What would you expect? What happens if you multiply the result with a large number? (The error gets bigger)

; Number -> Number
; counts how often 1/185 can be subtracted from the argument until it is 0.

(check-expect (sub 2/185) 2)
(check-expect (sub 1/185) 1)
(check-expect (sub 0) 0)

(define (sub n)
  (local ((define (subx x)
            (cond
              [(>= x n) 0]
              [else (+ 1 (sub (- n 1/185)))])))
    ;-IN-
    (subx 0)))
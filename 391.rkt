;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |391|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; ISL+ uses +inf.0 to deal with overflow. Determine the integer n such that (expt #i10.0 n)
; is an inexact number while (expt #i10. (+ n 1)) is approximated with +inf.0. Hint Design a function to compute n. 

; Number -> Number
; When given 0, determines the integer n such that (expt #i10.0 n) is an inexact number while (expt #i10. (+ n 1)) is approximated with +inf.0.

(define (threshold-find n)
  (local (; Number -> Boolean
          (define (infi? x)
            (equal? +inf.0 (expt #i10.0 x)))
          ; Number -> Boolean
          (define (hit-threshold? x)
            (and (not (infi? x)) (infi? (+ x 1)))))
    ;-IN- 
  (cond
    [(hit-threshold? n) n]
    [else (threshold-find (add1 n))])))

; Number, Function, Number -> Number
; to determine the smallest integer n such that (expt #i10.0 n) is still an inexact ISL+ number and (expt #i10. (- n 1)) is approximated with 0,
; We define a function that consumes a starting number n, a function + or -, and a threshold to hit, such as #i0.0 or #inf.0 and we call it for 0, - and #i0.0 

(check-expect (any-threshold-find 0 + +inf.0) 308) 

(define (any-threshold-find n plus-or-minus magic-n)
  (local (; Number -> Boolean
          (define (magic-number? x)
            (equal? magic-n (expt #i10.0 x)))
          ; Number -> Boolean
          (define (hit-threshold? x)
            (and (not (magic-number? x)) (magic-number? (plus-or-minus x 1)))))
    ;-IN- 
  (cond
    [(hit-threshold? n) n]
    [else (any-threshold-find (plus-or-minus n 1) plus-or-minus magic-n)])))

(any-threshold-find 0 - #i0.0)
(expt #i10.0 -323)
(expt #i10.0 -324)


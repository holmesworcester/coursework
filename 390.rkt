;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |390|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

(threshold-find 0)
(expt #i10.0 308)
(expt #i10.0 309)

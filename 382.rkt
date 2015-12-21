;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |382|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [List-of Number] -> Number
; consumes two equally long lists: a linear combination and a list of variable values. It produces the value of the combination for these values.

(check-expect (value '(1 1 1) '(1 2 3)) 6)
(check-expect (value '(1 2 3) '(1 2 3)) 14)

(define (value lin-comb var-vals)
  (cond
    [(empty? lin-comb) 0]
    [else (+ (* (first lin-comb)(first var-vals)) (value (rest lin-comb) (rest var-vals)))]))
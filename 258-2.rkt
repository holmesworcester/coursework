;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 258-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (inclusive) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons (sqrt n)
           (tab-sqrt (sub1 n)))]))

; Number, [Number -> X] -> [List-of X]
; tabulates any function [Number -> X] in order from Number to zero (inclusive) in a list.

(check-within (tabulate 5 sqrt) (tab-sqrt 5) 0.1)
(check-within (tabulate 0 sqrt) (tab-sqrt 0) 0.1)

(define (tabulate n f)
  (reverse (build-list (add1 n) f)))

(tabulate 5 sqrt)

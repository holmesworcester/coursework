;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |378|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of N], [List-of N] -> [List-of N]
; consumes two lists of numbers, sorted in ascending order.
; It produces a single sorted list of numbers that contains all the numbers on both inputs lists (and nothing else).
; A number occurs in the output as many times as it occurs on the two input lists together.

(check-expect (merge '(1 5 10) '(1 2 7 11)) '(1 1 2 5 7 10 11))
(check-expect (merge '(1 5 7 7 10) '(1 2 7 11)) '(1 1 2 5 7 7 7 10 11))
(check-expect (merge '(1 2) '(1)) '(1 1 2))

(define (merge lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [(empty? lon2) lon1]
    [else (cond
            [(<= (first lon1) (first lon2)) (cons (first lon1) (merge (rest lon1) lon2))]
            [(<= (first lon2) (first lon1)) (cons (first lon2) (merge lon1 (rest lon2)))])]))
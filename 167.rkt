;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |167|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Exercise 167. Design the function sum, which consumes a list of Posns
; and produces the sum of all of its x-coordinates.

; ListOfPosns -> Number
; sum, which consumes a list of Posns and produces the sum of all of its x-coordinates.

(check-expect (sum '()) 0)
(check-expect (sum (cons (make-posn 5 1) '())) 5)
(check-expect (sum (cons (make-posn 4 1) (cons (make-posn 5 1) '()))) 9)

(define (sum l)
  (cond
    [(empty? l) 0]
    [(cons? l) (+ (posn-x (first l)) (sum (rest l)))]))
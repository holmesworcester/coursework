;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 128|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
Exercise 128. Suppose the program contains
(define-struct ball [x y speed-x speed-y])

Predict the results of evaluating the following expression:

(number? (make-ball 1 2 3 4)) ; False

(ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3)) ; 3

(ball-y (make-ball (+ 1 2) (+ 3 3) 2 3)) ; 6

(ball-x (make-posn 1 2)) ; error. Expects a ball, given posn

(ball-speed-y 5) ; error. Expects a ball, given 5
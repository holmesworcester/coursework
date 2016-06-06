;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |453|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define QUEENS 8)
 
; QP is (make-posn CI CI)
; CI is a natural number in [0,QUEENS)
; interpretation a CI denotes a row or column index for a chess board, 
; (make-posn r c) specifies the square in the r-th row and the c-th column


; QP QP -> Boolean
; returns true if the queens threaten each other. Otherwise returns false. 

(check-expect (threatening? (make-posn 0 0) (make-posn 1 1)) #t)
(check-expect (threatening? (make-posn 0 0) (make-posn 0 1)) #t)
(check-expect (threatening? (make-posn 1 0) (make-posn 0 0)) #t)
(check-expect (threatening? (make-posn 4 4) (make-posn 5 5)) #t)
(check-expect (threatening? (make-posn 0 0) (make-posn 1 3)) #f)
(check-expect (threatening? (make-posn 5 5) (make-posn 9 1)) #t)


(define (threatening? q1 q2)
  (local (; useful constants
          (define q1x (posn-x q1))
          (define q1y (posn-y q1))
          (define q2x (posn-x q2))
          (define q2y (posn-y q2)))
    ;-IN-
    (cond
      [(equal? q1y q2y) #t] ; same row
      [(equal? q1x q2x) #t] ; same column
      [(equal? (- q1x q2x) (- q1y q2y)) #t] ; same down-right diagonal
      [(equal? (+ q1x q1y) (+ q2x q2y)) #t] ; same down-left diagonal
      [else #f])))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |454|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(define QUEENS 8)
(define MT (empty-scene 100 100))
(define HEIGHT 30)
(define SQUARE (square HEIGHT "outline" "black"))
(define QUEEN (circle (/ HEIGHT 2) "solid" "red"))
; (define QUEEN (above (circle (/ HEIGHT 5) "solid" "red") (triangle (/ HEIGHT 1.5) "solid" "red")))

; QP is (make-posn CI CI)
; CI is a natural number in [0,QUEENS)
; interpretation a CI denotes a row or column index for a chess board, 
; (make-posn r c) specifies the square in the r-th row and the c-th column

(define LQP (list (make-posn 0 0) (make-posn 1 1)))

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


; N -> N
; takes a QP and turns it into actual position on the board, for purposes of place-image

(check-expect (real-xy 0) (/ HEIGHT 2))
(check-expect (real-xy 10) (+ (* 10 HEIGHT) (/ HEIGHT 2)))

(define (real-xy n)
  (+ (* n HEIGHT) (/ HEIGHT 2)))

; N [List-of QP] Image -> Image
; consumes a natural number n, a list l of QPs, and an Image i representing a queen.
; It produces an image of an n by n chess board with images i placed according to l.
; this works. I think the lack of margin is something quirky about not having a background.
; it seems like there's a much easier way to do this. 

(define (render-queens n lqp i)
  (local (; N Image -> Image
          ; consumes a number N and an image of a row and draws n rows (a complete chess board of that size)
          (define (draw-all-rows n row)
            (cond
              [(<= n 1) row]
              [else (above row (draw-all-rows (- n 1) row))]))
          ; N -> Image
          ; consumes a number N and draws a chess board row of that size
          (define (draw-one-row n)
            (cond
              [(<= n 1) SQUARE]
              [else (beside SQUARE (draw-one-row (- n 1)))]))
          ; [List-of QP] Image -> Image)
          ; consumes a list of QP's and an image of a chess board.
          ; Places the QP's at the correct position on the board.
          (define (place-all-queens lqp chessboard-image)
            (local (; QP Image -> Image
                    ; consumes a QP and an image of a chessboard. produces a new image of a chessboard
                    (define (place-queen qp chessboard-image)
                      (place-image i (real-xy (posn-x qp)) (real-xy (posn-y qp)) chessboard-image))) ; i is the queen image
              ;-IN-
              (foldr place-queen chessboard-image lqp))))
    ;-IN-
    (place-all-queens lqp (draw-all-rows n (draw-one-row n)))))

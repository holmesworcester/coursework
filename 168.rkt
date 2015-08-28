;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |168|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

; ListOfPosns -> ListOfPosns
; for each (make-posn x y) in the former, the latter contains (make-posn x (+ y 1))

(check-expect (translate '()) '())
(check-expect (translate (cons (make-posn 5 1) '())) (cons (make-posn 5 2) '()))
(check-expect (translate (cons (make-posn 4 1) (cons (make-posn 5 1) '()))) (cons (make-posn 4 2) (cons (make-posn 5 2) '())))

(define (translate l)
    (cond
    [(empty? l) '()]
    [(cons? l) (cons (move (first l)) (translate (rest l)))]))

; Posn -> Posn
; moves posn up 1 (for each (make-posn x y), gives (make-posn x (+ y 1))

(define (move p)
  (make-posn (posn-x p) (+ 1 (posn-y p))))
         


    
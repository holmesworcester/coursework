;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |160|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; constants

; for testing

(define BLUESQUARE (rectangle 10 10 "solid" "blue"))
(define REDSQUARE (rectangle 10 10 "solid" "red"))
(define 10REDROW (rectangle 100 10 "solid" "red"))
(define 10REDCOL (rectangle 10 100 "solid" "red"))
(define NULLIMAGE (rectangle 0 0 "solid" "blue"))

; for the hall

(define X 10) ; grid basis
(define SQ (rectangle X X "outline" "black"))
(define HALL-X 8)
(define HALL-Y 18)
(define HEIGHT (* X HALL-Y))
(define WIDTH (* X HALL-X))

; for balloons

(define BALLOON (circle (/ X 5) "solid" "red"))
(define TESTBALLOON (make-posn X X))
(define TESTBALLOONDIAG2 (make-posn (* X 2) (* X 2))) 
(define TESTBALLOONDIAG3 (make-posn (* X 3) (* X 3))) 
(define TESTBALLOONDOWN2 (make-posn (* X 2) (* X 2))) 
(define TESTBALLOONDOWN3 (make-posn (* X 3) (* X 3))) 

; data definitions

; A N is one of: 
; – 0
; – (add1 N)
; interpretation represents the natural numbers or counting numbers

; A Pair is a structure (make-pair N List-of-posns)
(define-struct pair [balloon# lob])

; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n
; balloons must yet be thrown and the thrown balloons landed at lob

(define TESTLISTONEBALLOON (cons TESTBALLOON '()))
(define TESTLISTONETWOBALLOON (cons TESTBALLOON (cons TESTBALLOONDIAG2 '())))
(define TESTLISTPOSDIAG (cons TESTBALLOON (cons TESTBALLOONDIAG2 (cons TESTBALLOONDIAG3 '()))))
(define TESTLISTPOSDOWN (cons TESTBALLOON (cons TESTBALLOONDOWN2 (cons TESTBALLOONDOWN3 '()))))



(define 10EMPTY (make-pair 10 '()))
(define 100EMPTY (make-pair 100 '()))
(define 1EMPTY (make-pair 1 '()))
(define 0EMPTY (make-pair 0 '()))
(define 0TESTBALLOON (make-pair 0 TESTBALLOON))
(define 0POSDIAG (make-pair 0 TESTLISTPOSDIAG))

(define 10START 10EMPTY)
(define 100START 100EMPTY)

; (2) A big-bang expression is really just an expression. It is legitimate to nest it within an expression.

; functions

(define (main p)
  (big-bang p
   [on-tick tock 1]
   [to-draw render]))

; Pair -> Pair
; tock takes our world (a Pair) and reduces the number, while adding a random lob

(check-expect (tock 0EMPTY) 0EMPTY)
(check-expect (tock 0TESTBALLOON) 0TESTBALLOON)
(check-expect (tock 0POSDIAG) 0POSDIAG)

(define (tock p)
  (cond
    [(zero? (pair-balloon# p)) p] ;does nothing if there are no more balloons to throw
    [else (make-pair (- (pair-balloon# p) 1) (cons (make-posn (random WIDTH) (random HEIGHT)) (pair-lob p)))])) ; no recursion in structure definition, so none needed here. just tacking stuff on with cons.


; Pair -> Image
; render takes our world (a Pair) and produces an image
(define (render p)
  (add-balloons (pair-lob p)))


; Image, N -> Image
; (row i n) makes a row of i copies of the image n. If n is zero, (row i n) returns NULLIMAGE

(check-expect (image-width (row REDSQUARE 10)) (* (image-width REDSQUARE) 10))
(check-expect (row REDSQUARE 10) 10REDROW)
(check-expect (row REDSQUARE 1) REDSQUARE)
(check-expect (row REDSQUARE 0) NULLIMAGE)

(define (row i n)
  (cond
    [(zero? n) NULLIMAGE]
    [(positive? n) (beside i (row i (sub1 n)))]))

; ListOfPositions -> Image
; (add-balloons l) draws a BALLOON at all the Positions in list l, or HALL if empty.

(check-expect (add-balloons '()) HALL)
(check-expect (add-balloons TESTLISTONEBALLOON) (add-balloon TESTBALLOON HALL))
(check-expect (add-balloons TESTLISTPOSDIAG) (add-balloon TESTBALLOON (add-balloon TESTBALLOONDIAG2 (add-balloon TESTBALLOONDIAG3 HALL))))

(define (add-balloons l)
  (cond
    [(empty? l) HALL]
    [(cons? l)(add-balloon (first l) (add-balloons (rest l)))]))

; Image, N -> Image
; (col i n) makes a column of i copies of the image n. If n is zero, (row i n) returns NULLIMAGE

(check-expect (image-height (col REDSQUARE 10)) (* (image-height REDSQUARE) 10))
(check-expect (col REDSQUARE 10) 10REDCOL)
(check-expect (col REDSQUARE 1) REDSQUARE)
(check-expect (col REDSQUARE 0) NULLIMAGE)

(define (col i n)
  (cond
    [(zero? n) NULLIMAGE]
    [(positive? n) (above i (col i (sub1 n)))]))

; another constant. wish I could define this up top.

(define HALL (col (row SQ HALL-X) HALL-Y))

; Position -> Image
; (add-balloon p i) places a balloon image at Position p on top of image i using place-image.

(check-expect (add-balloon TESTBALLOON HALL) (place-image BALLOON (posn-x TESTBALLOON) (posn-y TESTBALLOON) HALL))

(define (add-balloon p i)
  (place-image BALLOON (posn-x p) (posn-y p) i))

(main 100START)
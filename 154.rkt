;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |154|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

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

; A ListOfPositions is one of:
; - '()
; - (cons Position ListOfPositions)
; interpretation: a list of positions of all the balloons thrown
; where position coordinates are within the bounds of the lecture hall,
; that is, greater than zero and less than (* HALL-X X) and (* HALL-Y X) respectively for posn-x and posn-y

(define TESTLISTONEBALLOON (cons TESTBALLOON '()))
(define TESTLISTPOSDIAG (cons TESTBALLOON (cons TESTBALLOONDIAG2 (cons TESTBALLOONDIAG3 '()))))
(define TESTLISTPOSDOWN (cons TESTBALLOON (cons TESTBALLOONDOWN2 (cons TESTBALLOONDOWN3 '()))))

; functions

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


; I will need this later: (check-expect (add-balloons '() HALL) HALL)

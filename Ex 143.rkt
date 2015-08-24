;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 143|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

; Exercise 143. Design ill-sized?.
; The function consumes a list of images loi and a positive number n.
; It produces the first image on loi that is not an n by n square;
; if it cannot find such an image, it produces #false.

; definitions

; ImageOrFalse is one of:
; – Image
; – #false

(define SQUARE (rectangle 10 10 "solid" "red"))
(define NOTSQUARE (rectangle 10 100 "solid" "blue"))

; List-of-images is one of:
; - '()
; - (cons ImageOrFalse List-of-images)
; interpretation: a list of images

(define EMPTY '())
(define FALSE (cons #f '()))
(define 1SQUARE (cons SQUARE '()))
(define 1NOTSQUARE (cons NOTSQUARE '()))
(define SQUARENOTSQUARE (cons SQUARE 1NOTSQUARE))
(define INCLUDESFALSE (cons SQUARE (cons #f (cons NOTSQUARE '()))))

; FUNCTIONS

; List-of-images -> ImageOrFalse
; Given a list of images, produces the first one on the list that is not square, or false if there are none.

(check-expect (ill-sized? EMPTY) #f)
(check-expect (ill-sized? FALSE) #f) ; means it got to the end and didn't find one
(check-expect (ill-sized? 1SQUARE) #f) ; no ill-sized
(check-expect (ill-sized? 1NOTSQUARE) NOTSQUARE) ; finds an ill-sized
(check-expect (ill-sized? SQUARENOTSQUARE) NOTSQUARE) ; finds it on the second pass
(check-expect (ill-sized? INCLUDESFALSE) NOTSQUARE) ; make sure it handles falses okay.

(define (ill-sized? li)
  (cond
    [(empty? li) #f]
    [(squareorfalse? (first li)) (ill-sized? (rest li))] ; is there a nicer / better way to do this?
    [else (first li)]))

; ImageOrFalse -> Boolean
; returns true if the ImageOrFalse is a square image, or false.

(check-expect (squareorfalse? #f) #t)
(check-expect (squareorfalse? SQUARE) #t)
(check-expect (squareorfalse? NOTSQUARE) #f)

(define (squareorfalse? i)
  (cond
    [(false? i) #t]
    [(= (image-width i) (image-height i)) #t]
    [else #f]))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |153|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

; constants

(define BLUESQUARE (rectangle 10 10 "solid" "blue"))
(define REDSQUARE (rectangle 10 10 "solid" "red"))
(define 10REDROW (rectangle 100 10 "solid" "red"))
(define 10REDCOL (rectangle 10 100 "solid" "red"))
(define NULLIMAGE (rectangle 0 0 "solid" "blue"))

; data definitions

; A N is one of: 
; – 0
; – (add1 N)
; interpretation represents the natural numbers or counting numbers

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

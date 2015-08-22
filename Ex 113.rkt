;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 113|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Any BSL value is one of: 
; – Number
; – Boolean
; – String
; – Image 
; – (make-posn Any Any)
; ...
; – (make-tank Any Any)
; ...

(define-struct vec [x y])

; A vec is
; (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector

; Any, Any -> vec
; (checked-make-vec x y) returns a vec (make-vec x y) with coordinates x and y if *both* x and y are positive numbers.
; otherwise returns an error

(define (checked-make-vec x y)
  (make-vec (check-positive x) (check-positive y)))

; Number -> Number
; makes sure a number is positive and returns an error if not.

(define (check-positive n)
  (cond
    [(> n 0) n]
    [else (error "positive number expected")]))

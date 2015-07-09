;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 66 - bouncing ball representation|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; WorldScene -> Structure
; A ball can move in two directions:
; - up
; - down
; and it has a position between 0 and HEIGHT
; and it has a velocity vel
; question: is this how you right a data definition for a structure?

(define HEIGHT 100)

(define SPEED 3)

(define-struct balld [location direction])
 
(make-balld 10 'up)

(make-balld 0 'down)

(define-struct vel [deltax deltay])

; Ex 66

(define-struct ballf [x y deltax deltay])

(define redball (make-ballf 30 40 -10 5))

(ballf-y redball)

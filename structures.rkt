;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname structures) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; graphical constants

(define HEIGHT 100)
(define WIDTH 100)
(define BG (empty-scene HEIGHT WIDTH))
(define SHIP (square 15 "solid "red))

; physical constants

(define SPEED 3)

; wishlist
; a data definition for position
; a function ke that handles key events for on-key
; a function render that takes in a WorldState and draws the ship in its position on background f
; a function move that takes in a WorldState and KeyEvent and produces a new WorldState

; maybe later I'd want
; a structure for all objects
; a render function that takes in the structure for all objects and draws them (this will let me add more objects without changing the render function)
; another function called by on-tick that checks if the hit is true for the ship and any other object
; a function that takes in the objects structure to see if any
; a function that moves all the objects that aren't the ship (asteroids)
; functions for moving different types of objects in the structure
; how would I make each object animate? I could make it park of what it means for an object to move

(define-struct velocity [dx dy])
; A Velocity is a structure: (make-velocity Number Number)
; interpretation (make-velocity a b) means that the object moves a steps along the horizontal
; and b steps along the vertical with each clock tick.

(define-struct ufo [posn velocity])
; A ufo is a structure: (make-ufo Posn Velocity)
; interpretation (make-ufo a v) means that the ufo is at location a
; and has velocity b
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |72 sample problem distance to zero|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; a constant that describes the dimensions of the world
(define WORLD-MAX 100)
(define WORLD-MIN (* WORLD-MAX -1))

; a structure that describes the 3 dimensional coordinates of an object
; R3 is (make-r3 Number Number Number)
; interpretation: coordinates in 3 dimensional space
; r3-x is the selector for the x coordinate 
; r3-y is the selector for the y coordinate
; r3-z is the selector for the z coordinate

(define-struct r3 [x y z])

(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 0))
(define ex3 (make-r3 1 0 0))
(define ex4 (make-r3 0 -1 -1))
(define ex5 (make-r3 -1 -7 -4))
(define corner1 (make-r3 WORLD-MAX WORLD-MAX WORLD-MAX))
(define corner2 (make-r3 WORLD-MIN WORLD-MAX WORLD-MAX))
(define corner3 (make-r3 WORLD-MIN WORLD-MIN WORLD-MAX))
(define corner4 (make-r3 WORLD-MIN WORLD-MIN WORLD-MIN))
(define origin (make-r3 0 0 0))


; R3 -> Number 
; determines the distance of p to the origin 
(define (r3-distance-to-0 p)
  (sqrt (+ (sqr (r3-x p)) (sqr (r3-y p)) (sqr (r3-z p)))))

(check-expect (r3-distance-to-0 origin) 0)
(check-expect (r3-distance-to-0 ex3) 1)
(check-expect (r3-distance-to-0 ex2) 1)



;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 94|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants

;world
(define WIDTH 500)
(define HEIGHT 500)
(define MIDDLE (/ WIDTH 2))
(define LAND-HEIGHT 20)
(define SKY (rectangle WIDTH HEIGHT "solid" "blue"))
(define LAND (rectangle WIDTH LAND-HEIGHT "solid" "green"))
(define BACKGROUND (place-image LAND (- HEIGHT (/ LAND-HEIGHT 2)) SKY))

;stuff

(define TANK (rectangle 20 13 "solid" "gray"))
(define TANK-Y (- (image-height TANK) HEIGHT))
(define TANK-SPEED 5)

(define MISSLE (rectangle 6 2 "solid" "orange"))
(define MISSLE-SPEED 10)

(define UFO-HEIGHT 20)
(define UFO (circle UFO-HEIGHT "solid" "red"))
(define UFO-SPEED 3)


; Data Definitions


; TankPosition is a Number in (0,WIDTH)
; interpretation: the position of the tank on the x-axis
; examples:

(define TANK-MIDDLE MIDDLE)
(define TANK-LEFT 0)
(define TANK-RIGHT WIDTH)

; TankDirection is one of:
; -- "left"
; -- "right"
; interpretation: the direction the tank is moving in

; TankPosition is an Integer in (0, WIDTH)
; interpretation: the position of the tank along the x axis.

; Tank is a structure (make-tank TankPosition TankDirection)
; examples:

(define tank-l-l (make-tank 0 "left"))
(define tank-l-r (make-tank 0 "right"))
(define tank-r-r (make-tank WIDTH "right"))
(define tank-m-r (make-tank MIDDLE "right"))
(define tank-m-l (make-tank MIDDLE "left"))

(define-struct tank (pos dir))

; UFO is a structure (make-ufo x y) 
; interpretation: the x,y coordinates of a UFO
; examples: 

(define ufo-start (make-ufo 0 MIDDLE))
(define ufo-halfway (make-ufo (/ HEIGHT 2) MIDDLE))
(define ufo-planet (make-ufo MIDDLE (- HEIGHT (+ LAND-HEIGHT (/ UFO-HEIGHT 2)))))

(define-struct ufo (x y))

; TankUFO is a structure (make-tankufo t u)
; interpretation: a world consisting of a Tank t and a UFO u.
; examples:

(define tankufo-start (make-tankufo tank-m-r ufo-start))
(define tankufo-planet (make-tankufo tank-m-r ufo-planet))
(define tankufo-halfway (make-tankufo tank-m-r ufo-halfway))
(define tankufo-lefttank (make-tankufo tank-l-r ufo-halfway))

(define-struct tankufo (t u))

; Missile is a structure (make-missle x y)
; interpretation: the x,y coordinates of a missle
; examples:

(define missle-start (make-pos MIDDLE MISSLE-STARTY))
(define missle-end (make-pos MIDDLE 0)) ; could be nicer than 0 but that's fine for now
(define missle-mid (make-pos MIDDLE (/ HEIGHT 2))) ; a missle halfway through its life.

; TankUFOMissile is a structure (make-tum Tank UFO Missle)
; interpretation: a world consisting of a Tank t, a UFO u, and a missle m.
; examples: 

(define tum-start (make-tum tank-m-r ufo-start missle-start))
(define tum-planet (make-tum tank-m-r ufo-planet missle-mid))

(define-struct tum (t u m))

; WorldState is one of:
; -- a TankUFO
; -- a TankUFOMissle
; see TankUFOMissle and TankUFO for examples <-- I could rewrite but "DRY"

; FUNCTIONS

; WorldState -> WorldState
; main function calls big-bang ontick, todraw, and onkey 
; to draw the world, handle key events, and move my objects on clock ticks

(define (main w)
(big-bang w
[todraw render]
[ontick tock]
[onkey key]))

; WorldState -> Image
; the function render draws everything in the world.
; note that (draw-missle w i) will return i if w is a not a TankUFOMissle 

(define (render w)
(draw-missle w (draw-tank w (draw-ufo w BACKGROUND)))) ; not including the conditional makes it cleaner and more obvious what I'm drawing.

; TankUFO, Image -> Image
; draws a tank and a UFO on the background for a given TankUFO state t
; annoying blocking question: where is the right place to put the conditional?
; should i try to push it down into the aux functions?
; or should i keep it in render and make the aux functions handle different states?
; that seems to make the aux functions less useful

;(define (draw-ufo w i)
;(place-image ( (draw-ufo t i)))

; TankUFO, Image

; TankUFO, Image -> Image
; draws a UFO on 

; tock
; key






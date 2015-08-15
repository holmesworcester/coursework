;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 98|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants

;world
(define WIDTH 200)
(define HEIGHT 200)
(define MIDDLE (/ WIDTH 2))
(define LAND-HEIGHT 20)
(define SKY (rectangle WIDTH HEIGHT "solid" "blue"))
(define LAND (rectangle WIDTH LAND-HEIGHT "solid" "green"))
(define BACKGROUND (place-image LAND (/ WIDTH 2) (- HEIGHT (/ LAND-HEIGHT 2)) SKY))

;stuff

(define TANK (rectangle 30 20 "solid" "gray"))
(define TANK-Y (- HEIGHT (image-height LAND)))
(define TANK-SPEED 5)

(define MISSLE (rectangle 6 2 "solid" "orange"))
(define MISSLE-SPEED 10)
(define MISSLE-STARTY (- TANK-Y (/ (image-height MISSLE) 2)))

(define UFO-HEIGHT 7)
(define UFO-WIDTH 30)
(define UFO (overlay (circle UFO-HEIGHT "solid" "red") (rectangle UFO-WIDTH 4 "solid" "red")))
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

(define-struct tank (x dir))

; examples of Tank:
(define tank-l-l (make-tank 0 "left"))
(define tank-l-r (make-tank 0 "right"))
(define tank-r-r (make-tank WIDTH "right"))
(define tank-m-r (make-tank MIDDLE "right"))
(define tank-m-l (make-tank MIDDLE "left"))

; UFO is a Position (make-posn x y) 
; interpretation: the x,y coordinates of a UFO
; examples of UFO: 

(define ufostart (make-posn MIDDLE 0))
(define ufohalfway (make-posn MIDDLE (/ HEIGHT 2)))
(define ufoplanet (make-posn MIDDLE (- HEIGHT (+ LAND-HEIGHT (/ UFO-HEIGHT 2)))))


; Missile is a Position (make-posn x y)
; interpretation: the x,y coordinates of a missle
; examples of Missle:

(define misslestart (make-posn MIDDLE MISSLE-STARTY))
(define missleend (make-posn MIDDLE 0)) ; could be nicer than 0 but that's fine for now
(define misslemid (make-posn MIDDLE (/ HEIGHT 2))) ; a missle halfway through its life.

; Aim is a structure (make-aim tank ufo) where tank is a Tank and ufo is a UFO.
; interpretation: the state of the world while the tank is aiming at the UFO but has not fired
; examples:

(define-struct aim (tank ufo))

(define aimstart (make-aim tank-m-r ufostart))
(define aimok (make-aim tank-r-r ufostart))
(define aimlose (make-aim tank-l-l ufoplanet))

; Fired is a structure (make-fired tank ufo missle) where tank is a Tank, ufo is a UFO and missle is a Missle.
; interpretation: the state of the world once the missle has been fired and is flying through the air.

(define-struct fired (tank ufo missle))

(define firedstart (make-fired tank-m-r ufostart misslestart))
(define firedmiss (make-fired tank-m-r ufohalfway missleend))
(define firedhit (make-fired tank-m-r ufohalfway (make-posn (/ (image-width UFO) 4) (/ HEIGHT 2)))) ;defined a hit in terms of 1/4 the width of the UFO

; A SIGS is one of:
; -- Aim
; -- Fired
; see TankUFOMissle and TankUFO for examples <-- I could rewrite but "DRY"

(define sigshit firedhit) ;question: I don't really need these examples here do i?
(define sigsfire firedstart)
(define sigsstart aimstart)
(define sigslose aimlose)

; FUNCTIONS

; SIGS -> SIGS
; main function calls big-bang ontick, todraw, and onkey 
; to draw the world, handle key events, and move my objects on clock ticks

(define (main s)
(big-bang s
[to-draw render]
[on-tick tock]
[on-key key]))

; SIGS -> Image
; the function render draws everything in the world.
; note that (draw-missle w i) will return i if w is a not a TankUFOMissle 

(define (render s)
  (cond
    [(aim? s)(draw-tank (aim-tank s) (draw-ufo (aim-ufo s) BACKGROUND))]
    [(fired? s)(draw-missle (fired-missle s) (draw-tank (fired-tank s) (draw-ufo (fired-ufo s) BACKGROUND)))]))

; SIGS -> SIGS
; tock moves Tank, UFO, and Missle in the ways that they are pre-determined to on each clock tick.
; let's think about this in a moment.

(define (tock s) s)

; SIGS, KeyEvent -> SIGS
; key handles all key events and changed the SIGS appropriately
; think more about what those changes are

(define (key s ke) s)

; Tank, Image -> Image
; draw-tank draws a Tank t over an image i.

(define (draw-tank t i)
  (place-image TANK (tank-x t) TANK-Y i))

; UFO, Image -> Image
; draw-ufo draws a UFO u over an image i.

(define (draw-ufo u i)
  (place-image UFO (posn-x u) (posn-y u) i))

; Missle, Image -> Image
; draw-missle draws a Missle m over an image i

(define (draw-missle m i)
  (place-image MISSLE (posn-x m) (posn-y m) i))

(main sigsstart)
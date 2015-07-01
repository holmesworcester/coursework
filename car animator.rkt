;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |car animator|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;graphical constants to create background and scenery
(define WIDTH-OF-WORLD 200)
(define SKY (rectangle WIDTH-OF-WORLD 50 "solid" "white"))
(define TREE
  (underlay/xy (circle 10 'solid 'green)
               9 15
               (rectangle 2 20 'solid 'brown)))
(define BACKGROUND (place-image TREE 40 25 SKY))

;graphical constants to create a car of relative sizes
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle (* WHEEL-RADIUS 3) 1 "solid" "white"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))
(define CAR-TOP (rectangle (* WHEEL-RADIUS 5) WHEEL-RADIUS "solid" "red"))
(define CAR-BOTTOM (rectangle (* WHEEL-RADIUS 9) (* 2 WHEEL-RADIUS) "solid" "red"))
(define BODY (above CAR-TOP CAR-BOTTOM))
(define CAR (above BODY BOTH-WHEELS))
(define CAR-WIDTH (image-width CAR))

;graphical constants on default car placement (maybe i should add a starting point here)
(define Y-CAR 25)
(define CAR-SPEED 3)

; WorldState -> Image
; render animation state as an image
(check-expect (render-as 10000) BACKGROUND)

(define (render-as as)
  (place-image CAR (- (x-position-of-car as) (/ (image-width CAR) 2)) Y-CAR BACKGROUND)) 


; WorldState -> Number
; A function that describes the distance of the right
; front of the car from the left edge of the screen

(check-expect (x-position-of-car 1) CAR-SPEED)
(check-expect (x-position-of-car 0) 0)

(define (x-position-of-car as)
  (* as CAR-SPEED))
;  (* WIDTH-OF-WORLD (sin as)))

; Number -> Number
; a function that takes a number from car distance and tells me where the bumper is,
; that is, the x coordinate of left-most pixel
; remember that distance is the right-most pixel of the car

(check-expect (car-rear 50) (- 50 CAR-WIDTH))
(check-expect (car-rear 0) (* CAR-WIDTH -1))

(define (car-rear d)
  (- d CAR-WIDTH))

; WorldState -> Boolean
; A function that stops the animation when the car
; rear edge goes off the background on the right side.
; that is, when x coordinate of car rear is greater than width of world

(check-expect (end?-as 0) false)
(check-expect (end?-as 1) false)
(check-expect (end?-as 10000) true)

(define (end?-as as)
  (> (car-rear (x-position-of-car as)) WIDTH-OF-WORLD))

; WorldState -> WorldState
; increments Animation State by one ever time the clock ticks
(check-expect (tock-as 0) 1)
(check-expect (tock-as 25) 26)
(check-expect (tock-as -25) -24)
(define (tock-as as)
  (add1 as))

; WorldState Number Number String -> WorldState
; places the car at position (x,y) 
; if the mouse event is "button-down"
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) (/ x-mouse CAR-SPEED)] ;using CAR-SPEED here is ugly but I'm not sure how to do it otherwise
    [else x-position-of-car])) 
; given: 21 10 20 "enter"
; wanted: 21

(check-expect (hyper 21 10 20 "enter") 21)

; given: 42 10 20 "button-down"
; wanted: 42

(check-expect (hyper 42 10 20 "button-down") (/ 10 CAR-SPEED)) ;again with the CAR-SPEED

; given: 42 10 20 "move"
; wanted: 42

(check-expect (hyper 42 10 20 "move") 42)



; WorldState -> WorldState
; launches the program from some initial state, based on animation state (time)
; use identical main function but make sure variables are new time-based ones

(define (main-as as)
  (big-bang as
            [on-tick tock-as]
            [to-draw render-as]
            [on-mouse hyper]
            [stop-when end?-as]))
          
; WorldState -> WorldState
; launches the program from some initial state

(main-as 0)


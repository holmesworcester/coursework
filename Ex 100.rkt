;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 100|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

;misc game aspects

(define WIN-TEXT "GAME OVER: YOU WIN!")
(define LOSE-TEXT "GAME OVER: YOU LOSE!")
(define FONT-SIZE 14)
(define FONT-COLOR "white")

(define R 10) ;The global hit radius of objects... how close objects can get measured from their "centers" or their point if one dimensional.

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

; Velocity is a Number.
; interpretation: the speed and direction of the tank (positive is to the right, negative to the left)

; TankPosition is an Integer in (0, WIDTH)
; interpretation: the position of the tank along the x axis.

; Tank is a structure (make-tank TankPosition Velocity)

(define-struct tank (x vel))

; examples of Tank:
(define tank-l-l (make-tank 0 (* TANK-SPEED -1)))
(define tank-l-r (make-tank 0 TANK-SPEED))
(define tank-r-r (make-tank WIDTH TANK-SPEED))
(define tank-m-r (make-tank MIDDLE TANK-SPEED))
(define tank-m-l (make-tank MIDDLE (* TANK-SPEED -1)))

; UFO is a Position (make-posn x y) 
; interpretation: the x,y coordinates of a UFO
; examples of UFO: 

(define ufostart (make-posn MIDDLE 0))
(define ufohalfway (make-posn MIDDLE (/ HEIGHT 2)))
(define ufoplanet (make-posn MIDDLE (- HEIGHT LAND-HEIGHT)))


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

; Location is one of:
; – Posn
; – Number
; interpretation Posn are positions on the Cartesian plane,
; Numbers are positions on either the x- or the y-axis.
; this definition helps me think about the function in-reach?


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
            [on-tick si-move]
            [on-key key]
            [stop-when si-game-over? si-render-final]))

; SIGS -> Image
; the function render draws everything in the world.
; note that (missle-render w i) will return i if w is a not a TankUFOMissle 

(define (render s)
  (cond
    [(aim? s)(tank-render (aim-tank s)
                          (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)(missle-render (fired-missle s)
                              (tank-render (fired-tank s)
                                           (ufo-render (fired-ufo s) BACKGROUND)))]))

; SIGS -> SIGS
; tock moves Tank, UFO, and Missle in the ways that they are pre-determined to on each clock tick.
; let's think about this in a moment.
; whether the state is aim or fired, the tank should move according to its velocity and direction
; whether the state is aim or fired, the UFO should move randomly as described. (look up)
; if the state is fired, the missle should move until it is off the screen.
; if the missle moves off the screen, the state should turn back to an aim state.
; write tests for each one of these cases, and add that to the function, and continue.

(define (si-move s)
  (cond
    [(fired? s)(tank-move (missle-move (ufo-move s)))]
    [(aim? s)(tank-move (ufo-move s))]))

; SIGS -> SIGS
; tank-move moves the tank on a clock tick, depending on direction and TANK-SPEED.

(define (tank-move s)
  (cond
    [(fired? s)(make-fired (tank-step (fired-tank s)) (fired-ufo s) (fired-missle s))]
    [(aim? s)(make-aim (tank-step (aim-tank s)) (aim-ufo s))])) ;having to do this conditional all the time seems like a real weakness of the language or the approach.

; Tank -> Tank
; tank-step moves the tank t (tank-vel t)

(check-expect (tank-step (make-tank 100 -2)) (make-tank 98 -2))
(check-expect (tank-step (make-tank 100 2)) (make-tank 102 2))
(check-expect (tank-step (make-tank WORLD-WIDTH 2)) (make-tank WORLD-WIDTH 2))
(check-expect (tank-step (make-tank 0 -2)) (make-tank 0 -2))

(define (tank-step t)
  (make-tank (x-limit (+ (tank-x t) (tank-vel t))) (tank-vel t)))

; Number -> Number
; x-limit keeps the x coordinate of objects inside (0, WORLD-WIDTH) when they move

(check-expect (x-limit -5) 0)
(check-expect (x-limit 0) 0)
(check-expect (x-limit 1) 1)
(check-expect (x-limit WORLD-WIDTH) WORLD-WIDTH)
(check-expect (x-limit (+ WORLD-WIDTH 5)) WORLD-WIDTH)
(check-expect (x-limit (- WORLD-WIDTH 1)) (- WORLD-WIDTH 1))

(define (x-limit x)
  (cond
    [(< x 0) 0]
    [(> x WORLD-WIDTH) WORLD-WIDTH]
    [else x]))


; SIGS -> SIGS
; ufo-move moves the tank on a clock tick, down the screen and randomly to the left and right.

(define (ufo-move s)
  (ufo-random (ufo-down s)))

; SIGS -> SIGS
; ufo-random moves the ufo in SIGS s left and right around the screen randomly
; as per the instructions (look them up)
; DO THIS NEXT!!!

(define (ufo-random s)
  (cond
    [(fired? s)(make-fired ... tank ufo missle)]
    [(aim? s)(make-aim tank ufo)]))

; SIGS -> SIGS
; ufo-down moves the ufo in SIGS s down the screen at a regular pace at a speed from constants.

(define (ufo-down s)
  (cond
    [(fired? s)(make-fired (fired-tank s) (ufo-step (fired-ufo s)) (fired-missle s))]
    [(aim? s)(make-aim (aim-tank s) (ufo-step (aim-ufo s)))]))

; UFO -> UFO
; ufo-step moves a UFO u down the screen by adding the constant UFO-SPEED to its y coordinate
; (posn-y u)

(define (ufo-step u)
  (make-ufo (posn-x u) (- (posn-y u) UFO-SPEED)))

; SIGS -> SIGS
; missle-move moves the missle on a clock tick, down the screen and randomly to the left and right.
; if the missle moves off the screen missle-move returns an Aim state.

(define (missle-move s)
  (cond
    [(missle-off-screen? s)(make-aim (fired-tank s) (fired-ufo s))] ;for the missle off screen condition
    [(fired? s)(make-fired (fired-tank s) (fired-ufo s) (missle-step (fired-missle s)))]
    [else s])) ;only fired states have a missle to move

; Fired -> Boolean
; finding its a lot harder to write tests for these than it is to write the function.

(define (missle-off-screen? s)
  (< (posn-y (fired-missle s)) 0))
  
; Position -> Position
; missle-step moves a missle m up the screen by the constant MISSLE-SPEED by subtracting
; from its y value (posn-y m) and leaving its x value (posn-x m) untouched.

(check-expect (missle-step (make-posn 100 100)) (make-posn 100 (- 100 MISSLE-SPEED)))

(define (missle-step m)
  (make-posn (posn-x (fired-missle m)) (- (posn-y (fired-missle m)) MISSLE-SPEED)))

; SIGS, KeyEvent -> SIGS
; key handles all key events and changed the SIGS appropriately
; think more about what those changes are

(define (key s ke) s)

; SIGS -> Boolean
; si-game-over? returns true when the game is over
; that is, either when the missle hits the UFO or when the UFO hits ("lands on") the planet.

(define (si-game-over? s)
  (or (ufo-hit-planet? s) (ufo-hit-missle? s)))

; SIGS -> Image
; si-render-final renders the final screen of the game when the game ends.
; it should say "game over: you win" if the missle hits the UFO.
; or "game over: you lose" if the missle hits the planet.

(define (si-render-final s)
  (cond
    [(ufo-hit-planet? s)(text-render WIN-TEXT (render s))]
    [(ufo-hit-missle? s)(text-render LOSE-TEXT (render s))]))

; String, Image -> Image
; text-render overlays the text for a given string s in constant font and color
; on a given image i.

(define (text-render s i)
  (overlay (text s FONT-SIZE FONT-COLOR) i))

; SIGS -> Boolean
; ufo-planet? tells me if the UFO has hit the planet for a given SIGS s.
; this is important beacuse the player loses.

(check-expect (ufo-hit-planet? firedhit) false)
(check-expect (ufo-hit-planet? aimstart) false)
(check-expect (ufo-hit-planet? aimok) false)
(check-expect (ufo-hit-planet? aimlose) true)
(check-expect (ufo-hit-planet? firedstart) false)
(check-expect (ufo-hit-planet? firedmiss) false)
(check-expect (ufo-hit-planet? ufoplanet) true)
               
(define (ufo-hit-planet? s)
  (cond
    [(fired? s)(ufo-landed? (fired-ufo s))]
    [(aim? s)(ufo-landed? (aim-ufo s))]))

; UFO -> Boolean
; ufo-landed? tells me if a given UFO u has landed, given some constants about where land is.

(check-expect (ufo-landed? (aim-ufo ufoplanet)) true)
(check-expect (ufo-landed? (aim-ufo aimlose)) true)
(check-expect (ufo-landed? (aim-ufo aimstart)) false)
(check-expect (ufo-landed? (aim-ufo ufoplanet)) true)

(define (ufo-landed? u)
  (in-reach? (- (- HEIGHT LAND-HEIGHT) (posn-y u))))

; SIGS -> Boolean
; ufo-hit-missle? tells me if the Missle has hit the UFO for a given SIGS s.
; this is important because the player wins.

(check-expect (ufo-hit-missle? firedhit) true)
(check-expect (ufo-hit-missle? aimstart) false)
(check-expect (ufo-hit-missle? aimok) false)
(check-expect (ufo-hit-missle? aimlose) false)
(check-expect (ufo-hit-missle? firedstart) false)
(check-expect (ufo-hit-missle? firedmiss) false)
               
(define (ufo-hit-missle? s)
  (cond
    [(fired? s)(in-reach? (distance (fired-missle s) (fired-ufo s)))]
    [else false]))

; Position, Position -> Position (where posn-x and posn-y are both greater than 0)
; distance determines the distance between two positions, p1 and p2 and returns a special Position.
; in which both posn-x and posn-y are greater than zero (using absolute value).

(check-expect (distance (make-posn 5 0) (make-posn 0 0)) (make-posn 5 0))
(check-expect (distance (make-posn 0 5) (make-posn 0 0)) (make-posn 0 5))
(check-expect (distance (make-posn 3 4) (make-posn 0 0)) (make-posn 3 4))
(check-expect (distance (make-posn -5 0) (make-posn 0 0)) (make-posn 5 0))
(check-expect (distance (make-posn 10 15) (make-posn 10 10)) (make-posn 0 5))
(check-expect (distance (make-posn -5 -5) (make-posn 0 0)) (make-posn 5 5))

(define (distance loc1 loc2)
  (make-posn (abs (- (posn-x loc1) (posn-x loc2))) (abs (- (posn-y loc1) (posn-y loc2)))))

; Location -> Boolean
; determines whether a location's distance to the origin is strictly less than some constant R

(check-expect (in-reach? (* 2 R)) false)
(check-expect (in-reach? R) false)
(check-expect (in-reach? (- R 1)) true)
(check-expect (in-reach? 0) true)

(check-expect (in-reach? (make-posn R R)) false)
(check-expect (in-reach? (make-posn (/ R 2) (/ R 2))) true)
(check-expect (in-reach? (make-posn 0 R)) false)
(check-expect (in-reach? (make-posn 0 0)) true)
                                                                   
(define (in-reach? loc)
  (cond
    [(posn? loc)(< (sqrt (+ (sqr (posn-x loc)) (sqr (posn-y loc)))) R)]
    [(number? loc)(< loc R)]))

; Tank, Image -> Image
; tank-render draws a Tank t over an image i.

(define (tank-render t i)
  (place-image TANK (tank-x t) TANK-Y i))

; UFO, Image -> Image
; ufo-render draws a UFO u over an image i.

(define (ufo-render u i)
  (place-image UFO (posn-x u) (posn-y u) i))

; Missle, Image -> Image
; missle-render draws a Missle m over an image i

(define (missle-render m i)
  (place-image MISSLE (posn-x m) (posn-y m) i))

(main sigsstart)
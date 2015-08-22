;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 114|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Data Definitions and Predicates

; TankPosition is a Number in (0,WIDTH)
; interpretation: the position of the Tank on the x-axis
; examples:

(define TANK-MIDDLE MIDDLE)
(define TANK-LEFT 0)

; out of range
(define TANK-RIGHT WIDTH)
(define TANK-TOO-WIDE (+ WIDTH 1))
(define TANK-NEG -1)
(define NOT-TANK (make-posn 1 5))

; TankPosition -> Boolean
; tankpos? predicate for TankPosition

(check-expect tankpos? TANK-MIDDLE true)
(check-expect tankpos? TANK-lEFT true)
(check-expect tankpos? TANK-RIGHT false)
(check-expect tankpos? TANK-TOO-WIDE false)
(check-expect tankpos? TANK-NEG false)
(check-expect tankpos? NOT-TANK false)

(define (tank-x? n)
  (cond
    [(and (number? n) (=< 0 n WIDTH)) #true]
    [else #false]))

; Velocity is a Number.
; interpretation: the speed and direction of the Tank (positive is to the right, negative to the left)

; Velocity -> Boolean

(define tank-vel? number?)

; Tank is a structure (make-tank TankPosition Velocity)

(define-struct tank (x vel))

; Tank -> Boolean
; tank? returns true if t is a Tank (make-tank TankPosition Velocity). Otherwise, returns false.
; Predicates for TankPosition and Velocity are tank-x? and tank-vel?


; examples of Tank:
(define tank-l-l (make-tank 0 (* TANK-SPEED -1)))
(define tank-l-r (make-tank 0 TANK-SPEED))
(define tank-r-r (make-tank WIDTH TANK-SPEED))
(define tank-m-r (make-tank MIDDLE TANK-SPEED))
(define tank-m-l (make-tank MIDDLE (* TANK-SPEED -1)))
(define tank-m-0 (make-tank MIDDLE 0))

; examples of not a Tank:

(define tank-not (make-tank NOT-TANK TANK-SPEED))
(define tank-wide (make-tank TANK-TOO-WIDE TANK-SPEED))
(define tank-neg (make-tank TANK-NEG TANK-SPEED))


(check-expect (tank? tank-l-l) true)
(check-expect (tank? tank-l-r) true)
(check-expect (tank? tank-r-r) true)
(check-expect (tank? tank-m-r) true)
(check-expect (tank? tank-m-l) true)
(check-expect (tank? tank-m-0) true)

(check-expect (tank? tank-not) false)
(check-expect (tank? tank-wide) false)
(check-expect (tank? tank-neg) false)

(define (tank? t)
  (cond
    [(and (tank-x? (tank-x t)) (tank-vel? (tank-vel t))) #true]
    [else #false]))

; UFO is a Position (make-posn x y) 
; interpretation: the x,y coordinates of a UFO
; examples of UFO: 

(define ufostart (make-posn MIDDLE 0))
(define ufodown1 (make-posn MIDDLE UFO-SPEED))
(define ufohalfway (make-posn MIDDLE (/ HEIGHT 2)))
(define ufoplanet (make-posn MIDDLE (- HEIGHT LAND-HEIGHT)))

; UFO -> Boolean
; ufo? tells me if something is a ufo. this is easy

(define ufo? posn?) ; I could figure out ranges but this overcomplicates things for now

; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation #false means the MissileOrNot hasn't been fired yet;
; Posn says the MissileOrNot has been fired and is at the specified location.

(define missilestart (make-posn MIDDLE MISSILE-STARTY))
(define missileupone (make-posn MIDDLE (- MISSILE-STARTY MISSILE-SPEED)))
(define missileend (make-posn MIDDLE 0)) ; could be nicer than 0 but that's fine for now
(define missilemid (make-posn MIDDLE (/ HEIGHT 2))) ; a Missile halfway through its life.
(define missile-close-hit (make-posn (+ MIDDLE (/ R 2)) (/ HEIGHT 2))) ; should be a hit, since (/ R 2) is less than R.
(define missile-close-miss (make-posn (+ MIDDLE R) (/ HEIGHT 2))) ; should be a miss, since in-reach? requires "closeness" to be strictly less than R
(define missilenot false)

(check-expect (missile-or-not? missilestart) true)
(check-expect (missile-or-not? missileupone) true)
(check-expect (missile-or-not? missileend) true)
(check-expect (missile-or-not? missilemid) true)
(check-expect (missile-or-not? missile-close-hit) true)
(check-expect (missile-or-not? missile-close-miss) true)
(check-expect (missile-or-not? missilenot) true)
(check-expect (missile-or-not? #true) #false)

(define (missile-or-not? v)
  (cond
    [(false? v) #true]
    [(posn? v) #true]
    [else #false]))


(define-struct sigs [tank ufo missile])

; SIGS
; is (make-sigs Tank UFO MissileOrNot)
; interpretation represents the state of the space invader game 
; Examples: (aim is the shorthand for situations with no missile, and fired is the shorthand for situations with a missile)

(define aimstart (make-sigs tank-m-0 ufostart false))
(define aimufodown1 (make-sigs tank-m-0 ufodown1 false))
(define aimok (make-sigs tank-r-r ufostart false))
(define aimlose (make-sigs tank-l-l ufoplanet false))
(define aimhalfway (make-sigs tank-l-l ufohalfway false))

(define firedstart (make-sigs tank-m-0 ufostart missilestart))
(define firedupone (make-sigs tank-m-0 ufostart missileupone))
(define firedmiss (make-sigs tank-m-r ufohalfway missileend))
(define fireddirecthit (make-sigs tank-m-r ufohalfway ufohalfway)) ;hit is totally direct (missile and ufo have the same position ufohalfway
(define firedclosehit (make-sigs tank-m-r ufohalfway missile-close-hit))
(define firedclosemiss (make-sigs tank-m-r ufohalfway missile-close-miss))
(define firedend (make-sigs tank-l-l ufohalfway (make-posn MIDDLE -1)))

(define sigshit fireddirecthit) 
(define sigsfire firedstart)
(define sigsstart aimstart)
(define sigslose aimlose)

; SIGS -> Boolean
; (sigs? s) returns true if s is a SIGS. Otherwise, it returns false

(check-expect (sigs? aimstart) #true)
(check-expect (sigs? aimufodown1) #true)
(check-expect (sigs? aimok) #true)
(check-expect (sigs? aimlose) #true)
(check-expect (sigs? aimhalfway) #true)
(check-expect (sigs? firedstart) #true)
(check-expect (sigs? firedupone) #true)
(check-expect (sigs? firedmiss) #true)
(check-expect (sigs? fireddirecthit) #true)
(check-expect (sigs? firedclosehit) #true)
(check-expect (sigs? firedclosemiss) #true)
(check-expect (sigs? firedend) #true)

(check-expect (sigs? 5) #false)
; i could include more here but the expression should be really simple now.

(define (sigs? s)
  (cond
    [(and () () () ) #true]
    [else #false]))
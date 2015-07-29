;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname happy-guage) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;happiness properties

(define HAPPY-DROP -0.1)
(define HAPPY-MIN 0)
(define HAPPY-MAX 100)

(define HAPPY-DOWN -1/5)
(define HAPPY-UP 1/3)

; physical constants

(define WORLD-WIDTH (* 0.1 HAPPY-MAX))
(define WORLD-HEIGHT HAPPY-MAX)
(define GUAGE-WIDTH (* WORLD-WIDTH 0.9))

; graphical constants
; there aren't really any graphical constants

; the only feature of this world is happiness. represented by a number ha.
  
; WISHLIST

; WorldState -> Image
; render function that draws happiness guage based on ha

(define (render ha)
  (place-image (rectangle GUAGE-WIDTH ha "solid" "red") (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2) (rectangle WORLD-WIDTH WORLD-HEIGHT "solid" "black")))

; WorldState -> WorldState
; tock function that decrements happiness by HAPPY-DROP

(check-expect (tock 100) (+ 100 HAPPY-DROP))
(check-expect (tock 0) 0) 

(define (tock ha)
  (max-min-check(+ ha HAPPY-DROP)))


; Number -> Number
; a function max-min-check to make sure change-happiness and tock never return more or less than max/min

(check-expect (max-min-check -0.1) 0)
(check-expect (max-min-check 100.1) 100)
(check-expect (max-min-check 105) 100)

(define (max-min-check ha)
  (cond
    [(< ha HAPPY-MIN) HAPPY-MIN]
    [(> ha HAPPY-MAX) HAPPY-MAX]
    [else ha]))

; KeyEvent and WorldState->WorldState
; takes in key a-key and changes happiness based on a conditional

(check-expect (change-happiness 90 "up") (+ 90 HAPPY-UP))
(check-expect (change-happiness 90 "down" ) (+ 90 HAPPY-DOWN))
(check-expect (change-happiness HAPPY-MAX "up") HAPPY-MAX)
(check-expect (change-happiness HAPPY-MIN "down") HAPPY-MIN)

(define (change-happiness ha a-key) ;HELP why is my key handler getting the world-state number instead of a key?
  (max-min-check
   (cond
     [(key=? a-key "up")(+ ha HAPPY-UP)]
     [(key=? a-key "down")(+ ha HAPPY-DOWN)])))
 

; WorldState -> WorldState
; Takes in the current state of happiness and events,
; Generates a new worldstate of happiness.

(define (main ha)
  (big-bang ha
            [to-draw render]
            [on-tick tock]
            [on-key change-happiness])) ; HELP. I think this is right


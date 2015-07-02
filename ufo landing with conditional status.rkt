;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |ufo landing with conditional status|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; WorldState is a Number 
; interpretation height of UFO (from top)
 
; constants: 
(define WIDTH 300)
(define HEIGHT 100)
(define CLOSE (/ HEIGHT 3))
 
; visual constants: 
(define MT (empty-scene WIDTH HEIGHT))
(define UFO
  (overlay (circle 10 "solid" "green")
           (rectangle 40 2 "solid" "green")))
 
; WorldState -> WorldState
(define (main y0)
  (big-bang y0
            [on-tick nxt]
            [to-draw render]))
 
; WorldState -> WorldState
; computes next location of UFO 
 
(check-expect (nxt 11) 14)
 
(define (nxt y)
  (+ y 3))

; WorldState -> Image
; draws the UFO on the background

(check-expect
 (place-ufo 11) (place-image UFO (/ WIDTH 2) 11 MT))

(define (place-ufo y) (place-image UFO (/ WIDTH 2) y MT))


; WorldState -> Image
; place UFO at given height into the center of MT
 
(define (render y)
  (cond
    [(< y (* 1/3 HEIGHT)) (overlay (text "descending" 12 "green") (place-ufo y))]
    [(> HEIGHT y (* 1/3 HEIGHT)) (overlay (text "closing in" 12 "green") (place-ufo y))]
    [else (overlay (text "landed" 12 "green") (place-ufo y))]))
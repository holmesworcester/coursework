;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |158|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants 
(define HEIGHT 80)
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))
 
; A ShotWorld is List-of-numbers. 
; interpretation the collection of shots fired and moving straight up

(define SHOT1 (cons HEIGHT '()))
(define SHOT2 (cons (- HEIGHT 1) '()))
(define SHOT3 (cons (- HEIGHT 2) '()))
(define 2SHOTS (cons (first SHOT2) SHOT1))
(define 2SHOTSUP1 (cons (first SHOT3) SHOT2))

; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
 
; ShotWorld -> ShotWorld 
; moves each shot up by one pixel

(check-expect (tock SHOT1) SHOT2)
(check-expect (tock 2SHOTS) 2SHOTSUP1)
(check-expect (tock '()) '())

(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))
 
; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar was hit

(check-expect (keyh SHOT1 "b") SHOT1)
(check-expect (keyh '() "left") '())
(check-expect (keyh '() " ") SHOT1)
 
(define (keyh w ke)
  (cond
    [(key=? ke " ") (cons HEIGHT w)]
    [else w]))
 
; ShotWorld -> Image 
; adds each y on w at (XSHOTS,y) to the background image

(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w) (to-image (rest w)))]))

(main '())
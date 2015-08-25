;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |159|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants 
(define HEIGHT 220)
(define WIDTH 30)
(define XSHOTS (- (/ WIDTH 2) 5))
 
; graphical constants 
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "green"))
(define SHOT (triangle 6 "outline" "black"))
 
; A ShotWorld is List-of-numbers. 
; interpretation the collection of shots fired and moving straight up

(define SHOT1 (cons HEIGHT '()))
(define SHOT2 (cons (- HEIGHT 1) '()))
(define SHOT3 (cons (- HEIGHT 2) '()))
(define 2SHOTS (cons (first SHOT2) SHOT1))
(define 2SHOTSUP1 (cons (first SHOT3) SHOT2))
(define NEGSHOT (cons (* HEIGHT -1) '()))
(define NEGSHOTSHOT1 (cons (* HEIGHT -1) SHOT1))

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
(check-expect (tock NEGSHOT) '())

(define (tock w)
  (cond
    [(empty? w) '()]
    [else (clean-shot (cons (sub1 (first w)) (tock (rest w))))]))

; ShotWorld -> Shotworld
; removes the first shot of s if it is less than zero. otherwise, returns s.

(check-expect (clean-shot NEGSHOT) '())
(check-expect (clean-shot NEGSHOTSHOT1) SHOT1)
(check-expect (clean-shot SHOT1) SHOT1)
(check-expect (clean-shot 2SHOTS) 2SHOTS)
(check-expect (clean-shot '()) '())

(define (clean-shot w)
  (cond
    [(empty? w) '()]
    [(cons? w) (if (< (first w) 0) (rest w) w)]))

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
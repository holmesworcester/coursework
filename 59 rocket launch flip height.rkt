;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |59 rocket launch flip height|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants 
(define HEIGHT 300)
(define WIDTH  100)
(define YDELTA 3)
(define CLOCKSPEED 0.1)
     
; graphical constants 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
(define ROCKET-CENTER (/ (image-height ROCKET) 2))
(define ROCKET-X 10)

; A LRCD (short for: launching rocket count down) is one of:
; – "resting"
; – a number in [-3,-1]
; – a non-negative number 
; interpretation a rocket resting on the ground, in count-down mode, 
; or the number of pixels from the bottom i.e. its height

; LRCD -> Image
; renders the state as a resting or flying rocket

(check-expect
 (show "resting")
 (place-image ROCKET
              ROCKET-X (- HEIGHT ROCKET-CENTER)
              BACKG))
 
(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              ROCKET-X (* 3/4 WIDTH)
              (place-image ROCKET
                           ROCKET-X (- HEIGHT ROCKET-CENTER)
                           BACKG)))
 
(check-expect
 (show 53)
 (place-image ROCKET ROCKET-X (- HEIGHT 53 ROCKET-CENTER) BACKG))

(define (show x)
  (cond
    [(string? x)
     (place-image ROCKET ROCKET-X (- HEIGHT ROCKET-CENTER) BACKG)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  ROCKET-X (* 3/4 WIDTH)
                  (place-image ROCKET
                               ROCKET-X (- HEIGHT ROCKET-CENTER)
                               BACKG))]
    [(>= x 0)
     (place-image ROCKET ROCKET-X (- HEIGHT x ROCKET-CENTER) BACKG)]))

; LRCD KeyEvent -> LRCD
; starts the count-down when space bar is pressed, 
; if the rocket is still resting 

(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA,
;  if it is moving already 

(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) 0)
(check-expect (fly 10) (+ 10 YDELTA))
(check-expect (fly 22) (+ 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (+ x 1)]
    [(>= x 0) (+ x YDELTA)]))

; LRCD -> Boolean
; a end? function that tells big-bang to stop
; if x is (-3 + YDELTA)

(check-expect (end? "resting") false)
(check-expect (end? -3) false)
(check-expect (end? -2) false)
(check-expect (end? -1) false)
(check-expect (end? 5) false)
(check-expect (end? HEIGHT) false)

(define (end? x)
  (cond
    [(string? x) false]
    [(<= -3 x -1) false]
    [(>= (+ HEIGHT ROCKET-CENTER) x 0) false]
    [(> x (+ HEIGHT ROCKET-CENTER)) true]))



; LRCD -> LRCD
; a main function that calls fly for on-tick

(define (main s)
  (big-bang s
            (to-draw show)
            (on-key launch)
            (on-tick fly CLOCKSPEED)
            (stop-when end?)))

(main "resting")

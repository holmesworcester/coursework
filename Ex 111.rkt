;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 111|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; constants

(define WORLD 100)

; data definitions

; ExpectsToSee is one of: 
; – AA
; – BC
; – DD 
; – ER 
 
(define AA "start, expect to see an 'a' next")
(define BC "expect to see: 'b', 'c', or 'd'")
(define DD "encountered a 'd', finished")
(define ER "error, user pressed illegal key")

; template example:

; ExpectsToSee -> ExpectsToSee

; (define (template e)
;   (cond
;    [(string=? AA)(...)]
;    [(string=? BC)(...)]
;    [(string=? DD)(...)]
;    [(string=? ER)(...)]))

; functions

(define (main e)
  (big-bang e
   [on-key update-state]
   [to-draw render]
   [stop-when stop?])) ; question: will stop-when run before render? i bet not. 

; ExpectsToSee, KeyEvent -> ExpectsToSee

(define (update-state e ke)
  (cond
    [(string=? e AA)(aa ke)]
    [(string=? e BC)(bc ke)]
    [(string=? e DD) DD] ; this is the end state. nothing else can happen.
    [(string=? e ER) ER])) ; this is an error state. nothing else can happen.

; KeyEvent -> ExpectsToSee
; (aa ke) updates KeyEvent for the ExpectsToSee state AA
; it only expects the letter a. if ke is "a", it will return the next state BC.
; if ke is anything else it will return ER.

(check-expect (aa "a") BC)
(check-expect (aa " ") ER)
(check-expect (aa "b") ER)

(define (aa ke)
  (cond
    [(string=? ke "a") BC]
    [else ER]))

; KeyEvent -> ExpectsToSee
; (bc ke) updates KeyEvent for the ExpectsToSee state BC
; it only expects the letters b or c or d. if ke is "b" or "c", it will return the current state BC.
; if ke is "d" it will advance to the final state DD.
; if ke is anything else it will return ER.

(check-expect (bc "a") ER)
(check-expect (bc " ") ER)
(check-expect (bc "b") BC)
(check-expect (bc "c") BC)
(check-expect (bc "d") DD)
(check-expect (bc "k") ER)

(define (bc ke)
  (cond
    [(string=? ke "b") BC]
    [(string=? ke "c") BC]
    [(string=? ke "d") DD]
    [else ER]))


; ExpectsToSee -> Image
; (render e) draws the appropriate colored rectangle for the current state e.

(define (render e)
  (cond
    [(string=? e AA)(draw-rectangle "white")]
    [(string=? e BC)(draw-rectangle "yellow")]
    [(string=? e DD)(draw-rectangle "green")]
    [(string=? e ER)(draw-rectangle "red")]))

; ExpectsToSee -> ExpectsToSee
; stop? is true when the program must exit, on DD or ER.
; too obvious to write tests for

(define (stop? e)
  (cond
    [(string=? e AA) false]
    [(string=? e BC) false]
    [(string=? e DD) true]
    [(string=? e ER) true]))

; String -> Image
; (draw-rectangle c) draws a rectangle of dimensions WORLD WORLD in color c.

(check-expect (draw-rectangle "red") (rectangle WORLD WORLD "solid" "red"))

(define (draw-rectangle c)
  (rectangle WORLD WORLD "solid" c))

; start the world
(main AA)
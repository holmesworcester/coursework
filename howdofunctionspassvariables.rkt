;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname howdofunctionspassvariables) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; Number -> Number
; a function that adds five

(define (add-five number)
  (+ 5 number))

; Number -> Number
; a function that multiplies by 2

(define (times-two number)
  (* 2 number))

; WorldState -> WorldState

(define (render s)
  (text (string-append "i don't care about the world state I'll say whatever" " " (number->string s)) 12 "red"))

; Number -> Number

;(check-expect (math 2 times-two) 4)
;(check-expect (math 2 add-five) 7)

; (define (math x function)
;  (function x)) this line doesn't work.

; (big-bang 5 [to-draw render]) ;big-bang can pass the number 5 to "render" here. I don't know how to build a function that does that.

; Number, Clause -> Number
; I'll try using a clause the way big bang does.

(define (my-big-bang x clause) (clause x)) ; seems fishy, but...

(my-big-bang 5 [to-draw render])


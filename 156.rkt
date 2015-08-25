;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |156|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct layer [color doll])
; interpretation the attributes of the current russian doll (in this case, just its color)
; and all the dolls within it. 

; An RD (russian doll) is one of: 
; – String 
; – (make-layer String RD)
; interpretation: a stack of russian dolls with the attribute color


; RD -> Number
; how many dolls are a part of an-rd 

(check-expect (depth (make-layer "yellow" (make-layer "green" "red")))
              3)
(check-expect (depth "red")
              1)

(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [(layer? an-rd) (+ (depth (layer-doll an-rd)) 1)]))

; RD -> String
; (colors an-rd) returns all the colors of the RD an-rd as a single string, with colors separated by commas.

(check-expect (colors (make-layer "yellow" (make-layer "green" "red")))
              "yellow, green, red")
(check-expect (colors "red")
              "red")

(define (colors an-rd)
    (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd) (string-append (layer-color an-rd) ", " (colors (layer-doll an-rd)))]))

; RD -> String
; returns the color of the inner most doll

(check-expect (inner (make-layer "yellow" (make-layer "green" "red")))
              "red")
(check-expect (inner "red")
              "red")

(define (inner an-rd)
    (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd) (inner (layer-doll an-rd))]))

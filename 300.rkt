;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |300|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A FF (family forest) is one of: 
; – '()
; – (cons FT FF)

;or... 

; [List-of FT] -> Boolean
; does the forest contain any child node with "blue" eyes
 
(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)
 
(define (blue-eyed-child-in-forest? loft)
  (ormap blue-eyed-child? loft))

; not including everything here but this is really straightforward...
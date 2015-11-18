;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |count problem intertwined|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; An S-expr (S-expression) is one of: 
; – Atom
; – SL
; An SL (S-list) is one of: 
; – '()
; – (cons S-expr SL)
; An Atom is one of: 
; – Number
; – String
; – Symbol

; S-expr -> Boolean
; returns true if given an atom otherwise returns false.

(check-expect (atom? 9) #t)
(check-expect (atom? "yo") #t)
(check-expect (atom? 't) #t)
(check-expect (atom? (list 1 2)) #f)

(define (atom? s)
  (or (number? s) (string? s) (symbol? s)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |229|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A [Maybe X] is one of: 
; – #false 
; – X

; [Maybe [List-of String]] is one of:
; - #false
; - List-of-strings
; interpretation: a List-of-strings or #false if there is no list of strings (not even empty).

; a List-of-strings is one of:
; - '()
; - (cons String List-of-strings)
; interpretation: a list of strings

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of the list los if it contains s 
; #false otherwise

(check-expect (occurs "a" (list "b" "a" "d")) (list "d"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(empty? los) #f]
    [else (cond
            [(string=? s (first los)) (rest los)] ; rest is the remainder
            [else (occurs s (rest los))])])) ; keep looking
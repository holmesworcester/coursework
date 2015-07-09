;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 68 - structures|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; is this the right way to talk about it?

; an area is a Number between 100 and 999
; interpretation: the area code of a phone number

; a switch is a Number between 100 and 999
; interpretation the first three digits of a seven digit number

; a phone is a Number between 1000 and 9999
; intepretation the last four digits of a seven digit number

(define-struct phone# [area switch phone])
; a Phone number is a structure: (make-phone# area switch phone)
; intepretation a complete phone number separated into its parts




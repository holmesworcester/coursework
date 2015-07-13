;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |70 - structure for since midnight|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct timeofday [hours minutes seconds])

; time24hr is a structure: (make-time24hr hours minutes seconds)
; interpretation: time passed since midnight, expressed as on a 24 hour clock ("military time")

; hours is a Number between 0 and 12 (what's the best way to say it's an integer, and an open interval)?
; interpretation: whole hours passed since midnight

; minutes is a Number between 0 and 60 
; interpretation: whole minutes passed since the last hour

; seconds is a Number from 0 to 60
; interpretation: seconds passed since the last minute mark
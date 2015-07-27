;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |77 - structure definition points in time since midnight|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; the function time->seconds, which consumes instances of time structures
; and produces the number of seconds that have passed since midnight.
; For example, if you are representing 12 hours, 30 minutes, and 2 seconds
; with one of these structures and if you then apply time->seconds to this instance,
; the correct result is 45002.

; 24time is a structure: (make-24time Hours Minutes Seconds)
; interpretation (make-24time h m s) means that h whole hours have passed since midnight,
; m minutes have passed since the last whole hour, and s seconds have passed since the last whole minute.

(define-struct 24time
  [hours minutes seconds])

; Time -> Number
; a function that takes in Time structure and returns time in seconds since midnight
(define (secondspassed t)
  (+ (* 60 60 (24time-hours t)) (* 60 (24time-minutes t)) (24time-seconds t)))

(check-expect (secondspassed (make-24time 12 30 2)) 45002)
(check-expect (secondspassed (make-24time 0 0 0)) 0)
(check-expect (secondspassed (make-24time 0 5 2)) (+ 0 (* 5 60) 2))
(check-expect (secondspassed (make-24time 4 59 58)) (+ (* 4 60 60) (* 59 60) 58))



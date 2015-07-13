;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |69 - structures|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; make data definitions for all of these.

(define-struct movie [title producer year])
; a movie is a structure: (make-movie String String String)
; interpretation: the name of a movie and some basic information about it:
; its producer and the year it was made.

(define-struct boyfriend [name hair eyes phone])
; a boyfriend is a structure: (make-boyfriend String String String Number)
; OR a boyfriend is a structure: (make-boyfriend name haircolor eyecolor phone) <-- should I define these in terms of variables
; interpretation: the name of the boyfriend, hair and eye colors, and his phone number

    (define-struct cheerleader [name number])

    (define-struct CD [artist title price])

    (define-struct sweater [material size producer])


; A Ball-1d is a structure:  (make-ball Number Number)
; interpretation 1 the position from top and the velocity 
; interpretation 2 the position from left and the velocity 
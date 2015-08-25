;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 148|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; data definitions

; NEList-of-booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-booleans)
; interpretation: a non-empty list of booleans
; examples:

(define ALL-TRUE (cons true (cons true (cons true '()))))
(define NONE-TRUE (cons false (cons false (cons false '()))))
(define SOME-TRUE (cons false (cons true (cons true '()))))
(define ONE-TRUE (cons false (cons false (cons true '()))))

; NEList-of-booleans -> Boolean
; all-true, which consumes a list of Boolean values and returns #true only if they are all true.

(check-expect (all-true ALL-TRUE) true)
(check-expect (all-true SOME-TRUE) false)
(check-expect (all-true ONE-TRUE) false)
(check-expect (all-true NONE-TRUE) false)

(define (all-true nelb)
  (cond
    [(empty? (rest nelb)) (first nelb)]
    [(cons? (rest nelb)) (and (first nelb) (all-true (rest nelb)))]))

; NEList-of-booleans -> Boolean
; one-true, which consumes a list of Boolean values and
; returns true if *at least one* item on the list is true. returns false if not.

(check-expect (one-true ALL-TRUE) true)
(check-expect (one-true SOME-TRUE) true)
(check-expect (one-true ONE-TRUE) true)
(check-expect (one-true NONE-TRUE) false)

(define (one-true nelb)
  (cond
    [(empty? (rest nelb)) (first nelb)]
    [(cons? (rest nelb)) (or (first nelb) (one-true (rest nelb)))]))

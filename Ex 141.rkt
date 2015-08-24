;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 141|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; data definitions

; List-of-booleans is one of:
; - '()
; - (cons Boolean List-of-booleans)
; interpretation: a list of booleans
; examples:

(define ALL-TRUE (cons true (cons true (cons true '()))))
(define NONE-TRUE (cons false (cons false (cons false '()))))
(define SOME-TRUE (cons false (cons true (cons true '()))))
(define ONE-TRUE (cons false (cons false (cons true '()))))

; List-of-booleans -> Boolean
; all-true, which consumes a list of Boolean values and returns #true only if they are all true, that is there are no false items.
; and #false otherwise.

(check-expect (all-true '()) true) ; the problem statement specifies that if none are false, it returns true.
(check-expect (all-true ALL-TRUE) true)
(check-expect (all-true SOME-TRUE) false)
(check-expect (all-true ONE-TRUE) false)
(check-expect (all-true NONE-TRUE) false)

(define (all-true lb)
  (cond
    [(empty? lb) #t]
    [else (and (first lb) (all-true (rest lb)))]))

; List-of-booleans -> Boolean
; one-true, which consumes a list of Boolean values and
; determines if *at least one* item on the list is true. returns false if not.

(check-expect (one-true '()) false) ; false because we need at least one true and this is the end of the line. or will return true as soon as it hits one. 
(check-expect (one-true ALL-TRUE) true)
(check-expect (one-true SOME-TRUE) true)
(check-expect (one-true ONE-TRUE) true)
(check-expect (one-true NONE-TRUE) false)

(define (one-true lb)
  (cond
    [(empty? lb) #f]
    [else (or (first lb) (one-true (rest lb)))]))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |286|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; An Set is a function:
; [X -> Boolean]
; interpretation: a set of Anything 

; examples:

(define two? (lambda (n) (or (equal? n 1) (equal? n 2))))

; X, Set -> Set
; adds an element X to a set.

(define (add-element x s)
  (lambda (x0) (or (s x0) (equal? x x0))))

; Set, Set -> Set
; creates a new set that is the union of two sets.

(define (union s1 s2)
  (lambda (x) (or (s1 x) (s2 x))))

; X, Set -> Set
; creates a new set that is the intersection of two sets

(define (intersection s1 s2)
  (lambda (x) (and (s1 x) (s2 x))))

(check-expect ((add-element 1 even?) 1) #t)
(check-expect ((union odd? even?) (random 1000)) #t)
(check-expect ((intersection odd? even?) (random 100)) #f)
(check-expect ((intersection odd? two?) 3) #f)
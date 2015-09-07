;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |186|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; List-of-numbers -> List-of-numbers
; produces a sorted version of alon

(check-expect (sort> '()) '())
 
(check-expect (sort> (list 12 20 -5)) (list 20 12 -5))
 
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
 
(check-expect (sort> (list 1 2 3)) (list 3 2 1))

(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [(cons? alon) (insert (first alon) (sort> (rest alon)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon

(check-expect (insert 5 '()) (list 5))

(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))

(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))

(check-satisfied (sort> (list 5 4 -10 1 0)) sorted>?)
(check-satisfied (sort> (list 1 4 -10 100 0 2)) sorted>?)


(define (insert n alon)
  (cond
    [(empty? alon) (cons n '())]
    [else (if (>= n (first alon))
              (cons n alon)
              (cons (first alon) (insert n (rest alon))))]))


(define ZERO (cons 0 '()))
(define ONEZERO (cons 1 (cons 0 '())))
(define ZEROONE (cons 0 (cons 1 '())))
(define THREEONETWO (cons 3 (cons 1 (cons 2 '()))))

; functions

; List-of-numbers -> Boolean
; returns true if all the temperatures are sorted in descending order
; that is, that each one is less than the next. Otherwise returns false.
; if a temperature is impossibly low, it also returns false.

(check-expect (sorted>? (list 5 4 3 1 0 -10)) #t)
(check-expect (sorted>? (list 5 4 -10 1 0)) #f)

(define (sorted>? nelt)
  (cond
    [(empty? (rest nelt)) #t]
    [(false? (rest nelt)) #f]
    [(cons? (rest nelt)) (and (> (first nelt) (first (rest nelt))) (sorted>? (rest nelt)))]))
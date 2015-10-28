;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |282|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define WIDTH 300)
(define HEIGHT 300)

; N -> [[List-of Posn] -> Boolean]
; generates a predicate that ensures that the length of the given list is k and that all Posns in this list are within a WIDTH by HEIGHT rectangle

(define (n-inside-playground? k)
  (lambda (l)
    (and
     (= k (length l)) ; make sure it's the right length
     (andmap (lambda (a-posn) (and (<= 0 (posn-x a-posn) WIDTH) (<= 0 (posn-y a-posn) HEIGHT))) l)))) ; and that the posns are all in range

; N -> [List-of Posn]
; generate n random Posns in a WIDTH by HEIGHT rectangle
(check-satisfied (random-posns 3) (n-inside-playground? 3))
 
(define (random-posns n)
  (build-list n (lambda (i) (make-posn (random WIDTH) (random HEIGHT)))))
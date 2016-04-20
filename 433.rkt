;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |433|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define EPSILON 0.01)
(define R 159)
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
 
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)
 
(define (integrate-kepler f a b)
  (/ (* (- b a)(+ (f a) (f b))) 2))


; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
; uses rectangle method

(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)

(define (integrate-rectangles f a b)
  (local (; define width as a local constant based on the constant R
          (define width (/ (- b a) R))
          ; define the constant S (half width) based on rect-width
          (define half-width (/ width 2))
          ; Number -> Number
          ; given an n (whose interpretation is the nth rectangle starting the count with zero)
          ; returns the area of that nth rectangle given the other constants
          (define (nth-rect-area n)
            (* width (f (+ a (* n width) half-width)))))
    ;-IN-
    (foldr + 0 (build-list R nth-rect-area))))

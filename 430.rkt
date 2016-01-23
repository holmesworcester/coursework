;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |430|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define EPSILON 0.2)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; [Number -> Number], Number -> Number
; consumes a number x, and a function that consumes numbers and returns numbers. Returns the slope of the function at x

(check-expect (slope (lambda (x) 1) 5) 0)
(check-expect (slope (lambda (x) x) 1000000) 1)
(check-expect (slope (lambda (x) (* 2 x)) 20) 2)
(check-within (slope (lambda (x) (sqr x)) 0) 0 EPSILON)
(check-within (slope (lambda (x) (sqr x)) 0.5) 1 EPSILON)

(define (slope f r)
  (/ (- (f (+ r EPSILON)) (f (- r EPSILON))) (* EPSILON 2)))

; [Number -> Number], Number -> Number
; maps function f and a number x to the root of the tangent through (x,(f x)).

(define (root-of-tangent f r)
  (- r (/ (f r) (slope f r))))

; [Number -> Number] Number -> Number
; finds a number r such that (<= (abs (f r)) EPSILON)
 
(check-within (newton poly 1) 2 EPSILON)
(check-within (newton poly 3.5) 4 EPSILON)
 
(define (newton f r1)
  (cond
    [(<= (abs (f r1)) EPSILON) r1]
    [else (newton f (root-of-tangent f r1))]))
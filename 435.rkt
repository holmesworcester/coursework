;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |435|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define EPSILON 0.0001)
(define R 159)
(define SMALL-ENOUGH 1/100)
 
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

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
; uses divide and conquer method until interval is less than SMALL-ENOUGH

(check-within (integrate-dc (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)

(define (integrate-dc f a b)
  (local (; define midpoint (b-a)/2 because we use it a couple times
          (define mid (+ a (/ (- b a) 2))))
    ;-IN-
    (cond
      [(< (- b a) SMALL-ENOUGH) (integrate-kepler f a b)]
      [else (+ (integrate-dc f a mid) (integrate-dc f mid b))])))

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
; uses adaptive method

(check-within (integrate-adaptive (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-adaptive (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)
; (check-within (integrate-adaptive (lambda (x) (sin (* x 1))) 0 10) 5 EPSILON)


(define (integrate-adaptive f a b)
    (local (; define midpoint (b-a)/2 because we use it a couple times
            (define mid (+ a (/ (- b a) 2))))
    ;-IN-
    (cond
      [(good-enough? f a b) (integrate-kepler f a b)]
      [else (+ (integrate-adaptive f a mid) (integrate-adaptive f mid b))])))

; [Number -> Number] Number Number -> Boolean
; this algorithm checks to make sure the function is flat and uninteresting in the given range using the following approach:
; the new algorithm computes the area of three trapezoids: the given one, and the two halves.
; If the difference between the two is less than the area of a small rectangle of height ε and width b - a,
; it is safe to assume that the overall area is a good approximation.

(check-expect (good-enough? (lambda (x) x) 1 10) #t)

(define (good-enough? f a b)
  (local (; midpoint
          (define mid (+ a (/ (- b a) 2)))
          ; "the two halves" is the same as a kepler integration
          (define two-halves (integrate-kepler f a b))
          ; "the given trapezoid"
          (define given-trapezoid (* (/ (+ (f a) (f b)) 2) (- b a)))
          ; acceptable difference is the area of a small rectangle of height ε and width b - a
          (define small-rectangle (* EPSILON (- b a))))
    ;-IN-
    ; If the difference between the two is less than the area of a small rectangle of height ε and width b - a,
  (< (abs (- given-trapezoid two-halves)) small-rectangle))) ; this is just a placeholder to make sure I'm thinking about it right.
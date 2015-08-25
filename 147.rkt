;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |147|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Design sorted>?. The function consumes a NEList-of-temperatures.
; It produces #true if the temperatures are sorted in descending order,
; that is, if the second is smaller than the first, the third smaller than the second,
; and so on. Otherwise it produces #false.

; constants

(define MIN-TEMP -273.15)

; data definitions

; A CTemperature is a Number greater or equal to MIN-TEMP
; interpretation: temperature in degrees Celsius

; A NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of measured temperatures


(define ZERO (cons 0 '()))
(define ONEZERO (cons 1 (cons 0 '())))
(define ZEROONE (cons 0 (cons 1 '())))
(define THREEONETWO (cons 3 (cons 1 (cons 2 '()))))

; functions

; NEList-of-temperatures -> Boolean
; tells me how many temperatures are in a list. helpful for average.

(check-expect (how-many ZERO) 1)
(check-expect (how-many ONEZERO) 2)
(check-expect (how-many THREEONETWO) 3)

(define (how-many nelt)
  (cond
    [(empty? (rest nelt)) 1]
    [else (+ 1 (how-many (rest nelt)))]))

; NEList-of-temperatures -> Number
; computes the sum of the given temperatures 

(check-expect (sum (cons 1 (cons 2 (cons 3 '())))) 6)

(define (sum anelot)
  (cond
    [(empty? (rest anelot)) (first anelot)]
    [(cons? (rest anelot)) (+ (first anelot) (sum (rest anelot)))]))

; NEList-of-temperatures -> Number
; computes the average temperature 
 
(check-expect (average (cons 1 (cons 2 (cons 3 '())))) 2)
 
(define (average anelot)
  (/ (sum anelot)
     (how-many anelot)))

; NEList-of-temperatures -> Boolean
; returns true if all the temperatures are sorted in descending order
; that is, that each one is less than the next. Otherwise returns false.
; if a temperature is impossibly low, it also returns false.

(check-expect (sorted>? ZERO) #t)
(check-expect (sorted>? ONEZERO) #t)
(check-expect (sorted>? ZEROONE) #f)
(check-expect (sorted>? THREEONETWO) #f)

(define (sorted>? nelt)
  (cond
    [(empty? (rest nelt)) #t]
    [(false? (rest nelt)) #f]
    [(cons? (rest nelt)) (and (> (first nelt) (first (rest nelt))) (sorted>? (rest nelt)))]))

; FOR SOME REASON 

; IF YOU WANT TO SEE SOME FLAIL KEEP READING!! 

; TempOrTrue, TempOrTrue -> Boolean, TempOrTrue
; compares two TempOrTrue tt1 and tt2 and returns tt2 if tt1 is greater than tt2
; or if tt2 is true.

;(check-expect (>ortrue? 1 0) 0)
;(check-expect (>ortrue? 0 1) #f)
;(check-expect (>ortrue? 0 0) #f)
;(check-expect (>ortrue? 1 #t) #t)
;(check-expect (>ortrue? #t #t) #t)

; (define (>ortrue? tt1 tt2)
;  (cond
;    [(and (boolean? tt2) (boolean=? tt2 #t)) #t] ; return true if it's a boolean and it's true?
;    [(and (number? tt1) (number? tt2) (> tt1 tt2)) tt2]
;    [(and (boolean? tt2) (boolean=? tt2 #f)) #f]
;    [else (error "something weird happened")]))

; this made sense to me earlier...     [(cons? (rest nelt)) (or (empty? (rest (rest nelt))) (> (first nelt) (sorted>? (rest nelt)))]))
; I can think of two solutions:
; 1) Make a function alllessthanx? that checks if all items in a list are less than x.
; 2) use > a b c d e f but return some stupidly small number if the list is empty so that greater than works.
; in this case I can take advantage of the fact that there is a minimum number -273. This solution
; seems the simplest <-- didn't work because it doesn't actually expand out it needs to process the thing it returns.


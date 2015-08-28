;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |163|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A ListOfFTemperatures is one of:
; - '()
; - (cons Number ListOfTemperatures)

; A ListOfCTemperatures is a ListOfTemperatures
; interpretation: a list of temperatures in degrees C.

; A ListOfFTemperatures is a ListOfTemperatures
; interpretation: a list of temperatures in degrees F.

; ListOfFTemperatures -> ListOfCTemperatures
; converts a list of Fahrenheit measurements to a list of Celsius measurements.

; (F -  32)  x  5/9 =

(check-expect (convertFC '()) '())
(check-expect (convertFC (cons 32 '())) (cons 0 '()))
(check-expect (first (convertFC (cons 32 (cons 32 (cons 32 '()))))) 0)
(check-expect (first (convertFC (cons 33 (cons 32 (cons 32 '()))))) 5/9)

(define (convertFC alot)
  (cond
    [(empty? alot) '()]
    [else (cons (f-to-c (first alot)) (convertFC (rest alot)))]))

; Number -> Number
; (ftoc f) turns degrees farenheit into degrees celsius.
(define (f-to-c f)
  (* 5/9 (- f 32)))

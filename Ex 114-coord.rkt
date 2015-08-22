;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 114-coord|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A Coordinate is one of: 
; – a negative number 
;    interpretation a point on the Y axis, distance from top
; – a positive number 
;    interpretation a point on the X axis, distance from left
; – a Posn
;    interpretation a point in a scene, usual interpretation

; Coordinate -> Boolean
; (coord? c) returns #true if c is a coordinate. otherwise, returns false.

(check-expect (coord? -1) #true)
(check-expect (coord? 1) #true)
(check-expect (coord? (make-posn 1 1)) #true)
(check-expect (coord? "yo") #false)
(check-expect (coord? 0) #false)

(define (coord? c)
  (cond
    [(posn? c) #true]
    [(number? c) (cond
                   [(> c 0) #true] ; is positive
                   [(< c 0) #true] ; is negative
                   [else #false])] ; is zero
    [else #false]))


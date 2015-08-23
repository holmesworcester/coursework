;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 116|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A TrafficLight shows one of three colors:
; – "red"
; – "green"
; – "yellow"
; interpretation each element of TrafficLight represents which colored
; bulb is currently turned on


; Any -> Boolean
; is the given value an element of TrafficLight
(define (light? x)
  (cond
    [(string? x) (or (string=? "red" x)
                     (string=? "green" x)
                     (string=? "yellow" x))]
    [else #false]))

; Any Any -> Boolean
; are the two values elements of TrafficLight and, 
; if so, are they equal
 
(check-expect (light=? "red" "red") #true)
(check-expect (light=? "red" "green") #false)
(check-expect (light=? "green" "green") #true)
(check-expect (light=? "yellow" "yellow") #true)
 
(define (light=? a-value another-value) ; this is ugly what's nicer way?
  (cond
    [(and (not (light? a-value)) (not (light? another-value)))(error "traffic light expected, given some other value for *both* parameters")]
    [(not (light? a-value))(error "traffic light expected, given some other value for the first parameter")]
    [(not (light? another-value))(error "traffic light expected, given some other value for the second parameter")]
    [(and (light? a-value) (light? another-value))(string=? a-value another-value)] ;if they're both valid compare them and return the result
    [else (error "error, I have no idea what happened")]))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |164|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A ListOfNumbers is one of:
; - '()
; - (cons Number ListOfNumbers)

; ListOfNumbers -> ListOfCNumbers
; converts a list of dollar amounts to euro amounts using the exchange rate 1 dollar = 0.87 euro

(check-expect (convertN '() .87) '())
(check-expect (convertN (cons 1 '()) .87) (cons .87 '()))

(define (convertN alon rate)
  (cond
    [(empty? alon) '()]
    [else (cons (dollar-to-euro* (first alon) rate) (convertN (rest alon) rate))]))

; Number -> Number
; turns $ to euro.
(define (dollar-to-euro x)
  (* .87 x))

; Number, Number -> Number
; turns $ to euro using the exchange rate, rate

(check-expect (dollar-to-euro* 0 100) 0)
(check-expect (dollar-to-euro* 0 100) 0)

(define (dollar-to-euro* x rate)
  (* rate x))

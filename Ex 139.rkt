;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 139|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)
; interpretation a List-of-amounts represents some amounts of money
; examples:

; base case

(define EMPTY '())

; short lists

(define ZERO (cons 0
                   '()))
(define ONE (cons 1
                  '()))

; long lists

(define TOTAL451 (cons 400
                       (cons 50
                             (cons 1
                                   '()))))

; List-of-amounts -> Number
; gives the total sum of all the amounts in a List-of-amounts

(check-expect (sum EMPTY) 0)
(check-expect (sum ZERO) 0)
(check-expect (sum ONE) 1)
(check-expect (sum TOTAL451) 451)

(define (sum l)
  (cond
    [(empty? l) 0]
    [else (+ (first l) (sum (rest l)))]))

(sum TOTAL451)


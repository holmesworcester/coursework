;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 140|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; data definition

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

; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)
; interpretation: a list of numbers
; examples:
; first, all the examples of List-of-amounts work here.
; I'll make some new examples with numbers that aren't amounts.

(define NEG-ONE (cons -1 '()))
(define 2-NEG-ONE (cons 2 NEG-ONE))

; functions

; List-of-numbers -> Boolean
; tells us whether a List-of-numbers is a List-of-amounts
; that is, that all the numbers in the list are Non-negative Numbers (>= n 0)
; It really doesn't make sense to not be able to sum zero.

(check-expect (pos? EMPTY) true)
(check-expect (pos? ZERO) true)
(check-expect (pos? NEG-ONE) false)
(check-expect (pos? 2-NEG-ONE) false)
(check-expect (pos? TOTAL451) true)

(define (pos? l)
  (cond
    [(empty? l) #true]
    [else (and (>= (first l) 0) (pos? (rest l)))]))

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

; Any -> Number
; gives the total sum of all the amounts, if given a List-of-amounts
; (that is, a list of positive numbers such that (pos? l) is true).
; otherwise, it returns an error ("expects a list of amounts, given something else")

(check-error (checked-sum 2-NEG-ONE) "expects a List-of-amounts (positive numbers); given another kind of list.")
(check-error (checked-sum "blah") "expects a list of amounts; given something that isn't a list") ; remember I have to check if something is a cons
(check-error (checked-sum 9) "expects a list of amounts; given something that isn't a list")
(check-expect (checked-sum EMPTY) 0)
(check-expect (checked-sum ZERO) 0)
(check-expect (checked-sum ONE) 1)
(check-expect (checked-sum TOTAL451) 451)

(define (checked-sum l)
  (cond
    [(empty? l) 0]
    [(cons? l)(cond
                [(pos? l)(sum l)]
                [else (error "expects a List-of-amounts (positive numbers); given another kind of list.")])]
    [else (error "expects a list of amounts; given something that isn't a list")]))

;Q. What does sum compute for an element of List-of-numbers?
;A. The sum of the list, including all the negative numbers!!



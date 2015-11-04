;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |292|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/abstraction)

; N -> [List-of N]
; creates the list 0 ... (n - 1) for any natural number n;

(check-expect (list0 5) '(0 1 2 3 4))

(define (list0 n)
  (for/list [(i n)] i))

; N -> [List-of N]
; creates the list 1 ... nfor any natural number n;

(check-expect (list1 5) '(1 2 3 4 5))

(define (list1 n)
  (for/list [(i n)] (+ i 1)))

; N -> [List-of N]
; creates the list (list 1 1/10 1/100 ...) of n numbers for any natural number n;
; offline and i don't know how to do 10^n. so replace raise with an exponent function

; (check-expect (list1/n 2) (list 1 1/10))

; (define (list1/n n)
;  (for/list [(i n)] (raise 10 i)))

; N -> [List-of N]
; creates the list of the first n even numbers;

(check-expect (list-even 5) '(0 2 4 6 8))

(define (list-even n)
  (for/list [(i n)] (* i 2))) 

; N -> Matrix
; creates a list of lists of 0 and 1 in a diagonal arrangement, e.g.,

(define (diagonal n)
  (for/list [(row n)]
    (for/list [(col n)] (if (= col row) 1 0))))
      
(check-expect (diagonal 3)
        (list
          (list 1 0 0)
          (list 0 1 0)
          (list 0 0 1)))

; Finally, use loops to define tabulate from exercise 238. See exercise 258.

; Number, [Number -> X] -> [List-of X]
; tabulates any function [Number -> X] in order from Number to zero (inclusive) in a list.

(define (tabulate n f)
  (reverse (build-list (add1 n) f)))

; Number, [Number -> X] -> [List-of X]
; tabulates any function [Number -> X] in order from Number to zero (inclusive) in a list.

(check-expect (tabulate-for 5 sqr) (tabulate 5 sqr))
(check-expect (tabulate-for 0 sqr) (tabulate 0 sqr))

(define (tabulate-for n f)
  (for/list [(i (+ n 1))] (f (- n i))))
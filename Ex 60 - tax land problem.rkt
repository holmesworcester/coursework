;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 60 - tax land problem|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Constants

(define TAX-THRESHOLD 1000)
(define LUX-TAX-THRESHOLD 10000)
(define TAX 0.05)
(define LUX-TAX 0.08)
  

; A Price falls into one of three intervals: 
; — 0 through TAX-THRESHOLD;
; — TAX-THRESHOLD through LUX-TAX-THRESHOLD;
; — LUX-TAX-THRESHOLD and above.
; interpretation the price of an item

; Price -> Number
; computes the amount of tax charged for price p

(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 1) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 999) 0)
(check-expect (sales-tax 1000) (* 0.05 1000))
(check-expect (sales-tax 9999) (* 0.05 9999))
(check-expect (sales-tax 12017) (* 0.08 12017))
(check-expect (sales-tax 10000) (* 0.08 10000))
(check-expect (sales-tax 1000000) (* 0.08 1000000))
(check-expect (sales-tax 1432) (* 0.05 1432))

(define (sales-tax p)
  (cond
    [(and (>= p 0) (< p TAX-THRESHOLD)) 0]
    [(and (>= p TAX-THRESHOLD) (< p LUX-TAX-THRESHOLD)) (* TAX p)]
    [(>= p LUX-TAX-THRESHOLD) (* LUX-TAX p)]))


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |431|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Number, Number -> Number
; computes how many months it takes to double a given amount of money when a savings account pays interest at a fixed rate on a monthly basis.
; by counting 

(define (double-amount initial interest)
  (local (; Number, Number -> Number
          ; consumes latest savings, number of months. Applies interest by calling itself.
          ; Increments the count of months by passing (+ months 1) to itself.
          ; terminates when latest is at least double initial savings and returns months.
          (define (apply-interest-until-double current months)
            (cond
              [(>= current (* 2 initial)) months]
              [else (apply-interest-until-double (* current (+ 1 (/ interest 100))) (add1 months))])))
    ;-IN-
    (apply-interest-until-double initial 0)))

(double-amount 100 .1)
(/ 72 .1)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |166|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct work [n name rate hours])
; Work is a structure: (make-work String Number Number). 
; interpretation (make-work n r h) combines the name (n) 
; with the pay rate (r) and the number of hours (h) worked.

; Low (list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the hours worked
; of a number of employees


; Paycheck is (make-paycheck N String Number)
; interpretation: a aycheck, where n is N, name is String, and amount is a Number
(define-struct paycheck [n name amount])

; A List-of-paychecks is one of:
; - ()'
; - (cons Paycheck List-of-paychecks)
; interpretation: a list of employee paychecks

; Low -> List-of-paychecks
; computes the weekly wages for all given weekly work records 
 
(check-expect (pay (cons (make-work 1 "Robby" 11.95 39) '()))
              (cons (make-paycheck 1 "Robby" (* 11.95 39)) '()))
 
(define (pay an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (pay1 (first an-low))
                          (pay (rest an-low)))]))

; Work -> Paycheck
; takes in a work entry and generates a paycheck entry, reproducing num and name,
; and figuring out amount.

(check-expect (pay1 (make-work 1 "Robby" 11.95 39))
              (make-paycheck 1 "Robby" (* 11.95 39)))

(define (pay1 w)
  (make-paycheck (work-n w) (work-name w) (* (work-rate w) (work-hours w))))
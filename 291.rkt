;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |291|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/abstraction)

; [List-of N] -> [List-of N]
; converts a list of US$ amounts into a list of € amounts based on an exchange rate of €1.08 per US$.

(check-expect (convert-euro '(0 1)) '(0 1.08))

(define (convert-euro l)
  (for/list [(usd-amount l)] (* usd-amount 1.08)))
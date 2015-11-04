;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |295|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/abstraction)

; [List-of [List-of String]] -> [List-of Number]
; determines the number of words on each line 

(check-expect (words-on-line '(("i" "am" "hungry")("hungry!"))) '(3 1))
; (check-expect (words-on-line '()) 0) ; I don't really care about this one
(check-expect (words-on-line '(())) '(0))

(define (words-on-line lls)
  (local
         ((define (words-on-one-line los)
            (match los
              [(? empty?) 0]
              [(cons s '()) 1]
              [(cons s rest) (+ 1 (words-on-one-line rest))])))
  (map words-on-one-line lls)))


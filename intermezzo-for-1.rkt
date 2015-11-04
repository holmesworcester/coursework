;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo-for-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/abstraction)

; [List-of X] -> [List-of [List N X]]
; The function consumes a list and produces a list of the same items paired with their relative index.

(check-expect (enumerate (list "a" "b")) '((1 "a")(2 "b"))) 

(define (enumerate l)
  (map (lambda (i x) (list i x)) (build-list (length l) (lambda (x) (+ x 1))) l))

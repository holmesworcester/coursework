;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |332|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Ex. 332 "N"

; a BSL-expr is one of:
; - Number
; - a Structure (make-add [BSL-expr BSL-expr])
; - a Structure (make-mul [BSL-expr BSL-expr])

(define-struct add [left right])
(define-struct mul [left right])

;examples:

;(+ 10 -10)
(define 10+10 (make-add 10 -10))

; (+ (* 20 3) 33)
(define 20x30+33 (make-add (make-mul 20 3) 33))

; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))

(define pi-math (make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9))))


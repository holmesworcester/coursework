;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |333|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Ex. 332 "N"

; a BSL-expr is one of:
; - Number
; - BSL-bool 
; - a Structure (make-add [BSL-expr BSL-expr])
; - a Structure (make-mul [BSL-expr BSL-expr])

(define-struct add [left right])
(define-struct mul [left right])

; a BSL-bool is one of:
; - [List-of Boolean]
; - a Structure (make-or [BSL-bool])
; - a Structure (make-and [BSL-bool])

(define-struct ory [list])
(define-struct andy [list])

(define andtrue (make-andy (list #t #t #t #t)))
(define andfalse (make-andy (list #t #f #t #t)))
(define ortrue (make-ory (list #f #f #f #t)))
(define orfalse (make-ory (list #f #f #f #f)))
(define nested-true (make-ory (list #f andtrue #f orfalse)))

;examples:

;(+ 10 -10)
(define 10-10 (make-add 10 -10))

; (+ (* 20 3) 33)
(define 20x3+33 (make-add (make-mul 20 3) 33))

; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))
(define pi-math (make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9))))

; BSL-expr -> N
; evaluates a BSL expression

(check-expect (eval-expression 10-10) 0)
(check-expect (eval-expression 20x3+33) 93)
(check-expect (eval-expression 5) 5)
(check-expect (eval-expression #t) #t)
(check-expect (eval-expression nested-true) #t)

(define (eval-expression bexp)
  (cond
    [(number? bexp) bexp]
    [(boolean? bexp) bexp]
    [(add? bexp) (+ (eval-expression (add-left bexp)) (eval-expression (add-right bexp)))]
    [(mul? bexp) (* (eval-expression (mul-left bexp)) (eval-expression (mul-right bexp)))]
    [(ory? bexp) (ormap (lambda (x) (eval-expression x)) (ory-list bexp))]
    [(andy? bexp) (andmap (lambda (x) (eval-expression x)) (andy-list bexp))]
    [else (error "not an expression")]))


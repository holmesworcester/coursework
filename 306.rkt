;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |306|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; An S-expr (S-expression) is one of: 
; – Number
; - String
; - Symbol
; - [List-of S-expr]

(define wing '(wing (wing (wing body wing) wing) wing))

(define table '((6 f)
    (5 e)
    (4 d)))

(define function '(define (f x)
     (+ (* 3 x x) (* -2 x) 55)))

(define hello '((hello 20.12 "world")))

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 

(check-expect (count wing 'wing) 6)
(check-expect (count hello 'hello) 1)
(check-expect (count hello 'dude) 0)

(define (count sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
    [(empty? sexp) 0]
    [else (+ (count (first sexp) sy) (count (rest sexp) sy))]))
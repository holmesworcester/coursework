;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |336|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)


(define WRONG "wrong kind of S-expression")
 
(define-struct add [left right])
(define-struct mul [left right])

; BSL-var-expr, Symbol, Number -> BSL-var-expr
; substitutes a variable defined by symbol s with a number x, in a BSL-var-expr b.

(check-expect (subst 2 'x 9) 2)
(check-expect (subst 'x 'x 9) 9)
(check-expect (subst (make-add 'x 1) 'x 9) (make-add 9 1))
(check-expect (subst (make-add (make-mul 'x 2) 3) 'x 3) (make-add (make-mul 3 2) 3))

(define (subst b s n)
  (cond
    [(number? b) b]
    [(symbol? b) n]
    [(add? b) (make-add (subst (add-left b) s n) (subst (add-right b) s n))]
    [(mul? b) (make-mul (subst (mul-left b) s n) (subst (mul-right b) s n))]
    [else (error "wrong kind of S-expression")]))

; BSL-var-expr

; X -> Boolean
; predicate that tells me if something is an atom

(define (atom? x)
  (or (number? x) (symbol? x) (string? x)))

(check-error (parse 'a) "symbols not allowed")
(check-error (parse "dude") "strings not allowed")
(check-expect (parse 9) 9)
(check-error (parse '(+ 1)) WRONG)
(check-error (parse '(+ 1 2 3)) WRONG)
(check-expect (parse '(+ 1 2)) (make-add 1 2))
(check-expect (parse '(* 1 2)) (make-mul 1 2))
(check-error (parse '(and #t #f)) WRONG)


; S-expr -> BSL-expr
; creates representation of a BSL expression for s (if possible)
(define (parse s)
  (local (; S-expr -> BSL-expr
          (define (parse s)
            (cond
              [(atom? s) (parse-atom s)]
              [else (parse-sl s)]))
 
          ; SL -> BSL-expr 
          (define (parse-sl s)
            (local ((define L (length s)))
              (cond
                [(< L 3)
                 (error WRONG)]
                [(and (= L 3) (symbol? (first s)))
                 (cond
                   [(symbol=? (first s) '+)
                    (make-add (parse (second s)) (parse (third s)))]
                   [(symbol=? (first s) '*)
                    (make-mul (parse (second s)) (parse (third s)))]
                   [else (error WRONG)])]
                [else
                 (error WRONG)])))
 
          ; Atom -> BSL-expr 
          (define (parse-atom s)
            (cond
              [(number? s) s]
              [(string? s) (error "strings not allowed")]
              [(symbol? s) (error "symbols not allowed")])))
    (parse s)))
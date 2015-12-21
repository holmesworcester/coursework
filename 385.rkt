;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |385|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol

; S-expr, S-expr -> Boolean
; determines whether two S-exprs are equal.

(check-expect (sexp=? 'a 'a) #t)
(check-expect (sexp=? 1 1) #t)
(check-expect (sexp=? "yo" "yo") #t)
(check-expect (sexp=? 'a 'b) #f)
(check-expect (sexp=? '(a b c d) '(a b c d)) #t)
(check-expect (sexp=? '(a b c) 'a) #f)
(check-expect (sexp=? '((a b) a) '(a b a)) #f)
(check-expect (sexp=? '((a b) a) '((a b) a)) #f)

(define (sexp=? s1 s2)
  (local (; Any -> Boolean
          ; tells me if something is an atom
          (define (atom? x) 
            (or (symbol? x) (number? x) (string? x)))
          ; Atom, Atom -> Boolean
          ; compares two atoms and returns true if they are equal, otherwise returns false
          (define (atoms-are-equal a1 a2)
            (or
              (and (symbol? a1) (symbol? a2) (symbol=? a1 a2))
              (and (string? a1) (string? a2) (string=? a1 a2))
              (and (number? a1) (number? a2) (= a1 a2)))))
    ; -IN-
    (cond
      [(and (atom? s1) (atom? s2)) (atoms-are-equal s1 s2)]
      [(and (cons? s1) (cons? s2)) (and (sexp=? (first s1) (first s2)) (sexp=? (rest s1) (rest s2)))]
      [else #f])))
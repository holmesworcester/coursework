;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |412|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (solve x) x)
(define (combine-solutions x y) y)

; take this template...

(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
      (combine-solutions
        P
        (special (rest P)))]))

; Define solve and combine-solutions so that

; special1 computes the length of its input,

(check-expect (special1 '(a b c d)) 4)
(check-expect (special1 '()) 0)

(define (special1 P)
  (cond
    [(empty? P) ((lambda(x)0) P)]
    [else
      ((lambda(l n)(+ 1 n))
        P
        (special1 (rest P)))]))

; special2 negates each number on the given list of numbers

(check-expect (special2 '(1 2 3)) '(-1 -2 -3))
(check-expect (special2 '()) '())

(define (special2 P)
  (cond
    [(empty? P) ((lambda(x)'()) P)]
    [else
      ((lambda(l1 l2)
         (cons (* -1 (first l1)) l2))
        P
        (special2 (rest P)))]))

; special3 uppercases the given list of strings.

(check-expect (special3 '("yo" "hey" "z")) '("YO" "HEY" "Z"))
(check-expect (special3 '("yO" "Hey" "Z")) '("YO" "HEY" "Z"))
(check-expect (special3 '("yO" " " "Hey" "Z")) '("YO" " " "HEY" "Z"))

    
;[List-of String] -> [List-of String]
; capitalizes a list of strings
(define (special3 P)
  (cond
    [(empty? P) ((lambda(x)'()) P)]
    [else
      ((lambda(l1 l2)
         (cons (cap-string (first l1)) l2))
        P
        (special3 (rest P)))]))

; String -> String
; capitalizes a string

(define (cap-string s)
  (local (; the int for "a"
          (define a (string->int "a"))
          ; the int for "z"
          (define z (string->int "z"))
          ; the int for "A"
          (define A (string->int "A"))
          ; define the magic conversion number from cap to lowercase as a constant
          (define magic-conversion-number (- a A))
          ;1String -> 1String
          ;capitalizes a 1String if it is a lowercase letter or leaves it alone otherwise
          (define (cap-1s s1)
            (local (; the int for the string we're looking at (to avoid repitition)
                    (define sint (string->int s1))
                    ; 1String -> Boolean
                    ; returns true if a lowercase letter
                    (define s1-lowercase?
                      (<= a sint z)))
              ; -IN-
              (cond
                [s1-lowercase? (int->string (- sint magic-conversion-number))] ; if lowercase then capitalize it.
                [else s1]))))
    ;-IN-
    (implode (map cap-1s (explode s)))))

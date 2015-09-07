;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |190|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Exercise 190. Design prefixes. The function consumes a list of 1Strings and produces the list of all prefixes. Recall that a list p is a prefix of l if p and l are the same up through all items in p. For example, (list 1 2 3) is a prefix of itself and (list 1 2 3 4).

; a Lo1s is one of
; - '()
; - (cons 1String Lo1s)

; a Lolo1s is one of
; - '()
; - (cons Lo1s Lolo1s)

; Lo1s -> Lolo1s
; prefixes consumes a list of 1Strings and produces the list of all prefixes.
; Recall that a list p is a prefix of l if p and l are the same up through all items in p. For example, (list 1 2 3) is a prefix of itself and (list 1 2 3 4).

(check-expect (prefixes (list "a" "b" "c")) (list (list "a" "b" "c") (list "a" "b") (list "a")))
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a")) (list (list "a")))
(check-expect (prefixes (list "a" "b")) (list (list "a" "b") (list "a")))

(define (prefixes l)
  (cond
    [(empty? l) '()]
    [else (cons l (prefixes (reverse (rest (reverse l)))))]))

; Lo1s -> Lolo1s
; suffixes consumes a list of 1Strings and produces the list of all suffixes.

(check-expect (suffixes (list "a" "b" "c")) (list (list "a" "b" "c") (list "b" "c") (list "c")))
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a")) (list (list "a")))

(define (suffixes l)
  (cond
    [(empty? l) '()]
    [else (cons l (suffixes (rest l)))]))

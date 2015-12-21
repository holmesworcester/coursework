;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |379|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N is
; - 0
; - (add1 N)

; [List-of X], N -> [List-of X]
; consumes a list l and a natural number n. Its result is l with the first n items removed or just the empty list if l is too short.

(check-expect (drop '(a b c d) 2) '(c d))
(check-expect (drop '() 5) '())
(check-expect (drop '(1 2) 0) '(1 2))
(check-expect (drop '(1 2) 5) '())

(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) '()]
    [else (drop (rest l) (sub1 n))]))

; [List-of X], N -> [List-of X]
; consumes a list l and a natural number n. Its result is the list of the first n items from l or all of l if it is too short.

(check-expect (take '(a b c) 2) '(a b))
(check-expect (take '(a b c) 10) '(a b c))
(check-expect (take '() 2) '())
(check-expect (take '(1 2 3 4 5) 0) '())

(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

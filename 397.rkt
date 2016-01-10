;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |397|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; [List-of 1String] N -> [List-of String]
; bundles sub-sequences of s into strings of length n

(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle (explode "abcdefg") 3) (list "abc" "def" "g"))
(check-expect (bundle '() 3) '())

(define (bundle s n)
  (map implode (list->chunks s n)))

; [List-of X], Number -> [List-of [List-of X]]
; It consumes a list l of arbitrary data and a natural number n.
; The function’s result is a list of list chunks of size n.
; Each chunk represents a sub-sequence of items in l.

(check-expect (list->chunks '(1 2 3 4 5 6) 2) '((1 2)(3 4)(5 6)))
(check-expect (list->chunks '() 2) '())
(check-expect (list->chunks '(1 2) 2) '((1 2)))
(check-expect (list->chunks '(1 2) 4) '((1 2)))
(check-expect (list->chunks '(1 2 3) 2) '((1 2)(3)))
(check-expect (list->chunks '(1 2 3) 0) '())

(define (list->chunks l n)
  (cond
    [(empty? l) '()]
    [(zero? n) '()]
    [else (append (list (take l n))(list->chunks (drop l n) n))]))

; [List-of X] N -> [List-of X]
; retrieves the first n items in l if possible or everything 
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; remove the first n items from l if possible or everything 
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
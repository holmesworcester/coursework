;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |409|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; [List-of Number] -> [List-of Number]
; creates a list of numbers with the same numbers as
; alon, sorted in ascending order
; no longer assumes the numbers are all distinct
; answer to question: once smaller accepts items that are <=, unless you pass (rest alon) to it,
; it will keep returning a list that includes the pivot and the function will loop.

(check-expect (quick-sort '(1 2 3)) '(1 2 3))
(check-expect (quick-sort '(1 3 2)) '(1 2 3))
(check-expect (quick-sort '(1)) '(1))
(check-expect (quick-sort '()) '())
(check-expect (quick-sort '(1 1 1 1 1)) '(1 1 1 1 1))
(check-expect (quick-sort '(1 2 1)) '(1 1 2))
(check-expect (quick-sort '(1 1 2)) '(1 1 2))

(define (quick-sort alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort (smaller-items (rest alon) pivot))
                    (list pivot)
                    (quick-sort (larger-items (rest alon) pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
; creates a list with all those numbers on alon  
; that are larger than n
(define (larger-items alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (larger-items (rest alon) n))
              (larger-items (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
; creates a list with all those numbers on alon  
; that are smaller than n
(define (smaller-items alon n)
  (cond
    [(empty? alon) '()]
    [else (if (<= (first alon) n)
              (cons (first alon) (smaller-items (rest alon) n))
              (smaller-items (rest alon) n))]))
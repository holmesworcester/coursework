;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |get-item function|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)

; LLS, N -> List-of-strings
; (AnyList -> Item-in-list)
; returns the list of strings that constitute the (- n 1)th item in the LLS (a line of text, e.g.)

(check-expect (get-item 2 '()) '())
(check-expect (get-item 0 (cons 1 (cons 2 (cons 3 '())))) 1)
(check-expect (get-item 2 (cons 1 (cons 2 (cons 3 '())))) 3)
(check-expect (get-item 5 (cons 1 (cons 2 (cons 3 '())))) '())


(define (get-item n l)
  (cond
    [(empty? l) '()]
    [(= n 0) (first l)]
    [else (get-item (- n 1) (rest l))]))

(get-item 1 (read-words/line "tt-emptymiddle.txt"))
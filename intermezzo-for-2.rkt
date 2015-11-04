;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo-for-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/abstraction)

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; consumes two lists, l1 and l2, and produces pairs of all items from l1 and l2.

(check-expect (cross '(a b c) '(1 2)) '((a 1) (b 1) (c 1) (a 2)(b 2)(c 2)))

(define (cross l1 l2)
  (local
    ((define (row l x)
       (map (lambda (an-l) (list an-l x)) l1))) ; makes a row of pairs of l1 and a given item.
  (map (lambda (an-l2) (row l1 an-l2)) l2))) ; makes those rows for each item in l2

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; consumes two lists, l1 and l2, and produces pairs of all items from l1 and l2.

; (check-expect (cross-for '(a b c) '(1 2)) '((a 1) (b 1) (c 1) (a 2)(b 2)(c 2)))
(check-satisfied (cross '(a b c) '(1 2)) (lambda (c) (= (length c) 6)))

(define (cross-for l1 l2)
  (for*/list ([i l1] [j l2]) (list i j)))
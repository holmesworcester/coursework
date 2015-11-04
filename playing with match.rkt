;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |playing with match|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/abstraction)

; [List-of Posn] -> [List-of Posn] 
; moves each object right by delta-x pixels
(check-expect (move-right-match-map (list (make-posn 1 1) (make-posn 10 14)) 3)
              (list (make-posn 4 1) (make-posn 13 14)))

(define (move-right-match-map lop delta-x)
  (map (lambda (p)
         (match p
           [(posn x y) (make-posn (+ x delta-x) y)]))
       lop))

(define bullets (list (make-posn 1 2) (make-posn 2 3)))
(define bullets-moved-1 (list (make-posn 2 2) (make-posn 3 3)))

; [List-of Posn] -> [List-of Posn] 
; moves each object right by delta-x pixels, using map only

(check-expect (move-right bullets 1) bullets-moved-1)

(define (move-right lop delta-x)
  (map (lambda (p) (make-posn (+ delta-x (posn-x p)) (posn-y p))) lop))

; [List-of Posn] -> [List-of Posn] 
; moves each object right by delta-x pixels using match only

(check-expect (move-right-match bullets 1) bullets-moved-1)
(check-expect (move-right-match (cons (make-posn 1 2) '()) 1) (cons (make-posn 2 2) '()))

(define (move-right-match lop delta-x)
  (match lop
    [(cons (posn x y) '()) (cons (make-posn (+ x delta-x) y) '())]
    [(cons (posn x y) rest) (cons (make-posn (+ x delta-x) y) (move-right-match rest delta-x))]))
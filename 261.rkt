;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |261|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; use fold to define map

;    (map f l ...) → (listof Z)

 ; f : (X ... -> Z)
  ;l : (listof X)

; [X -> Y], [List-of X] -> [List-of Y]
; works just like map, that is, creates a new list by applying a function f to every item in the list.

(check-expect (map-from-fold sqr (list 1 2 3 4 5)) (map sqr (list 1 2 3 4 5)))

(define (map-from-fold f l)
  (local (; X -> [List-of Y] (double check this)
          ; applies function to item and then cons it to a given list, returning a list.
          (define (cons-and-f x l)
            (cons (f x) l)))
  (foldr cons-and-f '() l)))

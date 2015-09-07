;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |189|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Number Sorted-by-Descending-List-of-numbers -> Boolean
; a more efficient search for a descending list of numbers

(check-expect (search-sorted 2 (list 3 2 1)) #true)
(check-expect (search-sorted 4 (list 3 2 1)) #false)
(check-expect (search-sorted 5 '()) #false)

(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [(> n (first alon)) #false]
    [else (or (= (first alon) n) (search-sorted n (rest alon)))]))

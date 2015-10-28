;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recent) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; X [List-of X] -> [Maybe N]
; determine the (0-based) index of the first occurrence of x in l, 
; #false otherwise

(check-expect (index 4 (list 4 1 2 3 5)) 0)
(check-expect (index 4 (list 1 4 3 5)) 1)

(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; 
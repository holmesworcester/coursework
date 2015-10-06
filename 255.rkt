;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |255|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Exercise 255. Use map to define the function convert-euro, which converts a list of US$ amounts into a list of € amounts based on an exchange rate of €1.22 per US$.

; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts

(check-expect (convert-euro (list 1)) (list 0.89))

(define (convert-euro lon)
  (local (; Number -> Number
           (define (convert-1-to-euro amount)
             (* amount 0.89)))
     (map convert-1-to-euro lon)))
  

; Also use map to define convertFC, which converts a list of Fahrenheit measurements to a list of Celsius measurements.

; [List-of Number] -> [List-of Number]
; converts a list of Fahrenheit measurements to a list of Celsius measurements.

(check-expect (convertFC (list 32)) (list 0))

(define (convertFC lon)
  (local (; Number -> Number
           (define (convertFC-one n)
             (* 5/9 (- n 32))))
     (map convertFC-one lon)))

; Finally, try your hands at translate, a function that translates a list of Posns into a list of list of pairs of numbers, i.e., [List-of [list Number Number]].

; [List-of Posn] -> [List-of [List-of Number]]
; a function that translates a list of Posns into a list of list of pairs of numbers

(check-expect (translate (list (make-posn 10 12))) (list (list 10 12)))

(define (translate lop)
  (local (; Posn -> [List-of Number]
          ; turns a posn into a list of a pair of numbers.
          (define (posn-to-list p)
            (list (posn-x p) (posn-y p))))
    (map posn-to-list lop)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 127|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
Exercise 127. Identify which of the following expressions are values:
(make-point 1 2 3)

(make-point (make-point 1 2 3) 4 5)

(make-point (+ 1 2) 3 4)

Explain why the expressions are values or not. image

; all except the last one, right? because since it requires evaluation it's an expression?
;but the second one is because structures are special and considered values?
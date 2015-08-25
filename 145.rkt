;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |145|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Exercise 145. Would sum and how-many work for NEList-of-temperatures even if they were designed for inputs from List-of-temperatures? If you think they don’t work, provide counter-examples. If you think they would, explain why. image

; yes. it works for both because they both return zero for lists that are '()
; and that is the only item in List-of-temperatures that is not in NEList-of-temperatures
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |175|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)

; Exercise 174. Design a program that encodes text files numerically.
; Each letter in a word;  should be encoded as a numeric three-letter
; string with a value between 0 and 256. Here is our encoding function for letters:

; data definitions

; A List is one of:
; – '()
; (cons Any '())
; interpretation: a list of some type of information.

; functions

; String -> F
; takes in a string s (a filename) and reads that file, and encodes the text numerically in a corresponding file
; the name of which is the appending of "numeric-" and s.

(define (wc f)
   (string-append "The file " f " has " (number->string (count (read-words/line f))) " lines, " (number->string (count (read-words f))) " words, and " (number->string (count (explode (read-file f)))) " 1strings (characters).")) ; seems less ugly to do it this way, right?

; List -> Number
; takes in a List, counts the number of items in the list

(check-expect (count '()) 0)
(check-expect (count (cons "a" (cons "b" (cons "c" '())))) 3)

(define (count l)
   (cond
     [(empty? l) 0]
     [else (+ 1 (count (rest l)))]))

(wc "ttt.txt")
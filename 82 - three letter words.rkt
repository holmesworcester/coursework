;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |82 - three letter words|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; how is this different from ex 75?

; Letter is a one of:
; - a string consisting of a lowercase letter
; - the Boolean #false

; A Word is a structure: (make-3letter Letter Letter Letter)
; interpretation: a word with three lowercase letters, or a boolean in place of each letter

(define-struct word
  [letter1 letter2 letter3])

(define wordcat (make-word "c" "a" "t"))
(define wordfalse (make-word false false false))
(define wordfalseat (make-word false "a" "t"))
(define wordbat (make-word "b" "a" "t"))
(define wordhim (make-word "h" "i" "m"))

; Word, Word -> Word
; compare is a function that compares each letter of a three letter word and returns a new word
; if the words are the same, the new word will be the same. if the words 

(define (compare w1 w2)
  (make-word (letter=? (word-letter1 w1) (word-letter1 w2)) (letter=? (word-letter2 w1) (word-letter2 w2)) (letter=? (word-letter3 w1) (word-letter3 w2)))) ;why does it say that w1-letter1 is not defined?

(check-expect (compare wordcat wordcat) wordcat)
(check-expect (compare wordbat wordcat) wordfalseat)
(check-expect (compare wordbat wordhim) wordfalse)
(check-expect (compare wordfalseat wordbat) wordfalseat)

; Letter -> Letter
; a function letter=? that compares letters and returns the same Letter or false if they are not the same

(define (letter=? l1 l2)
  (cond
    [(or (false? l1) (false? l2)) false]
    [(string=? l1 l2) l1]
    [else false])) ; I don't think this is the right way to write it. learn to use not instead?

(check-expect (letter=? "c" "c") "c")
(check-expect (letter=? false "c") false)
(check-expect (letter=? "c" "d") false)
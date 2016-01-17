;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Testing stuff|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Line -> Token
; extracts the rest of a word, meaning all letters up to a non-letter


(check-expect (rest-of-word '()) "")
(check-expect (rest-of-word (explode "ey?")) "ey")
(check-expect (rest-of-word (explode "ey ")) "ey")
(check-expect (rest-of-word (explode " yo")) "") ; space ends word. maybe it was just one letter.
(check-expect (rest-of-word (explode "?yo")) "") ; non letter ends word. maybe it was just one letter.

(define (rest-of-word line)
  (cond
    [(empty? line) ""]
    [(letter? (first line)) (string-append (first line) (rest-of-word (rest line)))]
    [else ""]))

; 1String -> Boolean
; returns true if given a letter a-z or A-Z

(check-expect (letter? "A") #t)
(check-expect (letter? "Z") #t)
(check-expect (letter? "b") #t)
(check-expect (letter? "a") #t)
(check-expect (letter? "z") #t)
(check-expect (letter? " ") #f)
(check-expect (letter? "[") #f)

(define (letter? 1s)
  (or (<= (string->int "A") (string->int 1s) (string->int "Z")) (<= (string->int "a") (string->int 1s) (string->int "z"))))

; 1String -> Boolean
; returns true if given something that is not a letter or whitespace. 

(check-expect (not-letter-not-whitespace? " ") #f)
(check-expect (not-letter-not-whitespace? "a") #f)
(check-expect (not-letter-not-whitespace? "?") #t)

(define (not-letter-not-whitespace? 1s)
  (not (or (string-whitespace? 1s) (letter? 1s))))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |293|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/abstraction)

; String, [List-of String] -> [Maybe String]
; consumes a name and a list of names.
; It retrieves the first name on the latter that is equal to, or an extension of, the former.
; If not found, returns false

(check-expect (find-name "bob" '("a" "bob" "c")) "bob")
(check-expect (find-name "bob" '("a" "bobby" "c")) "bobby")
(check-expect (find-name "bob" '("a" "bbby" "c")) #false)

(define (find-name name list)
  (for/or [(a-name list)]
    (if (for/and [(letter-a-name (explode a-name)) (letter-name (explode name))] (string=? letter-a-name letter-name))
        a-name #false)))

; [List-of String], N -> [Boolean]
; ensures that no name on some list of names exceeds some given width N.

(check-expect (length-check (list "holmes" "pat" "23akfjahfkasfdjhakfdsdfsafsfjahsdflasd") 12) #false)
(check-expect (length-check (list "holmes" "pat" "joe") 12) #t)

(define (length-check l n)
  (for/and [(name l)] (> n (length (explode name)))))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |171|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)

; A List-of-strings is one of:
; – '()
; (cons String List-of-strings)
; interpretation: a list of strings. Examples:

(define LOS-WORDS (cons "Put"
                        (cons "up"
                              (cons "in"
                                    (cons "a"
                                          (cons "place" '()))))))

(define LOS-LINES (cons "TTT"
  (cons ""
    (cons "Put up in a place"
        '()))))

; A List-of-list-of-strings is one of:
; – '()
; (cons List-of-strings List-of-list-of-strings)
; interpretation: a text file where each line is represented by a List-of-strings,
; and where each List-of-strings is a list of words and punctuation and/or lone punctuation
; separated by spaces, one per string, and an empty string "" for empty lines. 

(define LOLOS-WL (cons
                    (cons "TTT" '())
                    (cons
                     '()
                     (cons
                      (cons "Put" (cons "up" (cons "in" (cons "a" (cons "place" '())))))
                      '()))))


(read-file "ttt.txt")
"LINES"
(read-lines "ttt.txt")
"WORDS"
(read-words "ttt.txt")
"WORDS/LINE"
(read-words/line "ttt.txt")
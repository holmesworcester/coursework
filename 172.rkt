;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |172|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)

; Constants

(define EMPTY-LINE " \n")

; Data definitions

; A List-of-strings is one of:
; – '()
; (cons String List-of-strings)
; interpretation: a list of strings.


; A LLS is one of:
; – '()
; (cons List-of-strings LLS)
; interpretation: a text file where each line is represented by a List-of-strings


; A Text is and LLS
; interpretation: a text file where each line is represented by a List-of-strings, and where
; each string as the appropriate space or line break attached to it



; Functions

; LLS -> String
; turns an LLS into a Text and then a single string.

(check-expect (collapse-and-format '()) "")
; (check-expect (collapse-and-format LOLOS-WL) "TTT\n \nPut up in a place\nwhere it's easy to see") ;  I don't really trust my own examples. Let's not trust these tests and only trust tests that are the result of the read file functions.
(check-expect (collapse-and-format (read-words/line "tt-twolines.txt")) (read-file "tt-twolines.txt")) ; one break after first line, none after 2nd
(check-expect (collapse-and-format (read-words/line "tt-emptymiddle.txt")) (read-file "tt-emptymiddle.txt")) ; one break, then a space, then another break.
(check-expect (collapse-and-format (read-words/line "ttt.txt")) (read-file "ttt.txt")) ; one break, then a space, then another break.


(define (collapse-and-format l)
  (collapse (format-lls l)))

; LLS -> Text
; formats an LLS by adding spaces after words, and one per empty line, but not at the end of lines. And adding line breaks after each line but not
; after the final line.

; (check-expect (format-lls (cons LOS-WORDS '())) (cons LOS-SPACED '()))
(check-expect (format-lls '()) '())
(check-expect (format-lls (cons '() '())) (cons (cons "\n" '()) '()))
(check-expect (format-lls (cons (cons "Dude" '()) (cons '() (cons (cons "Dude" '()) '())))) (cons (cons "Dude\n" '()) (cons (cons " \n" '()) (cons (cons "Dude" '()) '()))))

(define (format-lls lls)
  (cond
    [(empty? lls) '()]
    [(empty? (rest lls)) (cons (format-line (first lls) true) '())] ; tells format line that this is the last line, doesn't do recursion
    [(empty? (first lls)) (cons (cons EMPTY-LINE '()) (format-lls (rest lls)))] ; this should tell me a line is empty, that is, that there's a double line break.
;    [(emptyline? (first lls)) (cons (cons "\n" '()) (format-lls (rest lls)))] ; should satisfy my linebreak situation. this conditional needs to go here to make sure I'm not passing empty to first
    [else (cons (format-line (first lls) false) (format-lls (rest lls)))])) ; tells format-line that this isn't the last line and does recursion.

; format-lls needs to return what happens on an empty line. when it reaches an item that is an empty line, it needs to return " \n".
;  I just need to figure out what an empty line looks like to this function

; LLS, N -> List-of-strings
; (AnyList -> Item-in-list)
; returns the list of strings that constitute the (- n 1)th item in the LLS (a line of text, e.g.)

(check-error (get-item 2 '()) "there aren't that many items in the list")
(check-error (get-item 5 (cons 1 (cons 2 (cons 3 '())))) "there aren't that many items in the list")
(check-expect (get-item 0 (cons 1 (cons 2 (cons 3 '())))) 1)
(check-expect (get-item 2 (cons 1 (cons 2 (cons 3 '())))) 3)

(define (get-item n l)
  (cond
    [(empty? l) (error "there aren't that many items in the list")]
    [(= n 0) (first l)]
    [else (get-item (- n 1) (rest l))]))

; List-of-strings -> Boolean
; tell me if a list of strings is an "empty line".

; (check-expect 

(define (emptyline? ls) #f)

; List-of-strings, Boolean -> List-of-strings
; adds spaces and line breaks to each line

(check-expect (format-line '() false) (cons " \n"'())) ;is this right? if it's an empty line that isn't the last line, it gets a space and a line break.
(check-expect (format-line '() true) '())
; (check-expect (format-line LOS-WORDS true) LOS-SPACED)
; (check-expect (format-line LOS-WORDS false) LOS-SPACED-NOT-LAST)


(define (format-line ls final-line?)
  (cond
    [(empty? ls) '()]
;    [(and (empty? ls) final-line?) '()] ; what should it return here?
;    [(and (empty? ls) (not final-line?)) (cons " \n" '())] ; what should it return here?
    [final-line? (cons (string-append (first ls) (if (empty? (rest ls)) "" " ")) (format-line (rest ls) final-line?))] ; if it's the final line just add spaces, except at the end where you add nothing.
    [else (cons (string-append (first ls) (if (empty? (rest ls)) "\n" " ")) (format-line (rest ls) final-line?))])) ; If it's not the final line add spaces after each word but add a line break and not a space after the last word.

; need a condition for when the line is empty that actually works.


; LLS -> String
; turns a list of list of strings into a single string

; (check-expect (collapse '()) "")
; (check-expect (collapse LOLOS-WL) "TTTPutupinaplacewhereit'seasytosee") ; I think I still trust these tests
                           
(define (collapse lines)
  (cond
    [(empty? lines) ""]
    [else (string-append (collapse-line (first lines)) (collapse (rest lines)))]))

; ListOfStrings -> String
; turns a line of text represented by a list of strings into a single string, with words
; separated by spaces except at the end of lines.

(check-expect (collapse-line '()) "") ;no line break here. I think I could've just put a line-break here.
; (check-expect (collapse-line LOS-WORDS) "Putupinaplace") ; no spaces unless specified
; (check-expect (collapse-line (first LOLOS-WL)) "TTT")

(define (collapse-line l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (collapse-line (rest l)))])) ;if clause gets rid of spaces at end of line
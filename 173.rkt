;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |173|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)

; Exercise 173. Design a program that removes all articles from a text file.
; The program consumes the name n of a file, reads the file, removes the articles,
; and writes the result out to a file whose name is the result of concatenating "no-articles-" with n.
; For this exercise, an article is one of the following three words: "a", "an", and "the".

; constants

(define EMPTY-LINE " \n")

; data definitions

; A List-of-strings is one of:
; – '()
; (cons String List-of-strings)
; interpretation: a list of strings.

; A LLS is one of:
; – '()
; (cons List-of-strings LLS)
; interpretation: a text file where each line is represented by a List-of-strings

; String -> File
; takes in a filename n, reads the file, removes the articles,
; and writes the result out to a file whose name is the result of concatenating "no-articles-" with n.

; A Text is an LLS
; interpretation: a text file where each line is represented by a List-of-strings, and where
; each string as the appropriate space or line break attached to it


; not sure how to write tests

; (check-expect (no-articles (read-words/line "tt-twolines.txt")) (read-file "no-articles-ttt.txt")) ; one break after first line, none after 2nd
; (check-expect (no-articles (read-words/line "tt-emptymiddle.txt")) (read-file "tt-emptymiddle.txt")) ; one break, then a space, then another break.
; (check-expect (no-articles (read-words/line "ttt.txt")) (read-file "ttt.txt")) ; one break, then a space, then another break.


(define (no-articles n)
   (write-file (string-append "no-articles-" n)  (collapse-and-format (remove-articles (read-words/line n)))))

; Text -> Text
; removes the articles a, an, and the from text.

(check-expect (remove-articles '()) '())
(check-expect (remove-articles (cons (cons "yo" '()) '())) (cons (cons "yo" '()) '()))
(check-expect (remove-articles (cons (cons "the" '()) '())) (cons '() '()))
(check-expect (remove-articles (cons (cons "a" '()) '())) (cons '() '()))
(check-expect (remove-articles (cons (cons "an" '()) '())) (cons '() '()))
(check-expect (remove-articles (cons (cons "yo" (cons "an" '())) '())) (cons (cons "yo" '()) '()))

(define (remove-articles lls)
  (cond
    [(empty? lls) '()]
    [else (cons (remove-articles-from-line (first lls)) (remove-articles (rest lls)))]))


; List-of-strings -> List-of-strings
; removes the articles a, an, and the from a line.

; tests upstream

(define (remove-articles-from-line ls)
  (cond
    [(empty? ls) '()]
    [(article? (first ls)) (remove-articles-from-line (rest ls))]
    [else (cons (first ls) (remove-articles-from-line (rest ls)))]))

; String -> Boolean
; is it an article (an a the)?

(check-expect (article? "the") #t)
(check-expect (article? "a") #t)
(check-expect (article? "an") #t)
(check-expect (article? "ufskskdfjh") #f)
(check-expect (article? "") #f)

(define (article? s)
  (cond
    [(or (string=? s "the") (string=? s "a") (string=? s "an")) #t]
    [else #f]))

; LLS -> String
; turns an LLS into a Text and then a single string.

(check-expect (collapse-and-format '()) "")
(check-expect (collapse-and-format (read-words/line "tt-twolines.txt")) (read-file "tt-twolines.txt")) ; one break after first line, none after 2nd
(check-expect (collapse-and-format (read-words/line "tt-emptymiddle.txt")) (read-file "tt-emptymiddle.txt")) ; one break, then a space, then another break.

(define (collapse-and-format l)
  (collapse (format-lls l)))

; LLS -> Text
; formats an LLS by adding spaces after words, and one per empty line, but not at the end of lines. And adding line breaks after each line but not
; after the final line.

(check-expect (format-lls '()) '())
(check-expect (format-lls (cons (cons "Dude" '()) (cons '() (cons (cons "Dude" '()) '())))) (cons (cons "Dude\n" '()) (cons (cons " \n" '()) (cons (cons "Dude" '()) '()))))

(define (format-lls lls)
  (cond
    [(empty? lls) '()]
    [(empty? (rest lls)) (cons (format-line (first lls) true) '())] ; tells format line that this is the last line, doesn't do recursion
    [(empty? (first lls)) (cons (cons EMPTY-LINE '()) (format-lls (rest lls)))] ; this should tell me a line is empty, that is, that there's a double line break.
    [else (cons (format-line (first lls) false) (format-lls (rest lls)))])) ; tells format-line that this isn't the last line and does recursion.

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

; List-of-strings, Boolean -> List-of-strings
; adds spaces and line breaks to each line

; (check-expect (format-line '() false) (cons " \n"'())) ;is this right? if it's an empty line that isn't the last line, it gets a space and a line break.
(check-expect (format-line '() true) '())
; (check-expect (format-line LOS-WORDS true) LOS-SPACED)
; (check-expect (format-line LOS-WORDS false) LOS-SPACED-NOT-LAST)


(define (format-line ls final-line?)
  (cond
    [(empty? ls) '()]
    [final-line? (cons (string-append (first ls) (if (empty? (rest ls)) "" " ")) (format-line (rest ls) final-line?))] ; if it's the final line just add spaces, except at the end where you add nothing.
    [else (cons (string-append (first ls) (if (empty? (rest ls)) "\n" " ")) (format-line (rest ls) final-line?))])) ; If it's not the final line add spaces after each word but add a line break and not a space after the last word.

; LLS -> String
; turns a list of list of strings into a single string
                           
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
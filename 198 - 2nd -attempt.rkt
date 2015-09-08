;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |198 - 2nd -attempt|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)

; On OS X: 
(define DICTIONARY-LOCATION "/usr/share/dict/words")
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

; definitions

; A Word is either 
; – '() or
; – (cons 1String Word)
; interpretation a String as a list of single Strings (letters)

(define DEAR (explode "dear"))
(define D (list "d"))
(define RAT (explode "rat"))
(define NOT-DICT (explode "aawrasfauluiawh"))
(define hello-word (explode "hello"))


; A List-of-words is one of:
; - '()
; - (cons Word List-of-words)

(define LOW1 (list DEAR D RAT))
(define ARRANGEMENTS-RAT (list (explode "rat") (explode "rta") (explode "tar") (explode "tra") (explode "art") (explode "atr")))
(define ARRANGEMENTS-RAT-DICT (list (explode "rat") (explode "tar") (explode "art")))

; Splitword is (make-splitword Word Word)
; interpretation: the "first part" and the "second part" of a word divided somewhere in the middle.

(define-struct splitword [pre post])

(define hel-lo-split (make-splitword (explode "hel") (explode "lo")))
(define hell-o-split (make-splitword (explode "hell") (explode "o")))

; functions

; String -> List-of-strings
; find all words that the letters of some given word spell
 
;(check-member-of (alternative-words "cat")
 ;                (list "act" "cat")
  ;               (list "cat" "act"))
 
(check-satisfied (alternative-words "rat") all-words-from-rat?)
 
(define (alternative-words s)
  (in-dictionary (words->strings (arrangements (explode s)))))
 
; List-of-words -> List-of-strings
; turn all Words in low into Strings

(check-expect (words->strings (list (explode "dude") (explode "yo"))) (list "dude" "yo"))

(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (implode (first low)) (words->strings (rest low)))]))
 
; Word -> Boolean
; for testing

; List-of-strings -> Boolean 

(define (all-words-from-rat? w)
  (and (member? "rat" w)
       (member? "art" w)
       (member? "tar" w)))

; Word -> List-of-words
; creates a list of all rearrangements of the letters in w

(check-expect (arrangements (cons "a" '())) (cons (cons "a" '()) '()))
(check-expect (arrangements (list "a" "b")) (list (list "a" "b") (list "b" "a")))
(check-expect (arrangements (list "1" "2" "3"))
              (list
               (list "1" "2" "3")
               (list "1" "3" "2")
               (list "2" "1" "3")
               (list "2" "3" "1")
               (list "3" "2" "1")
               (list "3" "1" "2")))


(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
            (arrangements (rest w)))]))

; 1String, Word -> List-of-words
; makes a list of words by inserting the 1String s into all positions of the word w

(check-expect (insert-everywhere/in-all-words "b" '()) (list "b"))
(check-expect (insert-everywhere/in-all-words "b" (list "a" "d")) (list (list "b" "a" "d") (list "a" "b" "d") (list "a" "d" "b")))
(check-expect (insert-everywhere/in-all-words "b" (list "a")) (list (list "b" "a") (list "a" "b")))

(define (insert-everywhere/in-all-words s w) 
  (insert-everywhere s (make-splitword '() w)))

; 1String, Splitword -> List-of-words
; Returns a list by moving a 1String s through a Splitword sw one letter at a time

(define (insert-everywhere s sw)
  (cond
    [(end-of-splitword? sw) (cons (add-in-middle s sw) '())]
    [else (cons (splitword->word (add-in-middle s sw)) (insert-everywhere s (advance-splitword sw)))]))

; Splitword -> Splitword
; advances the "split" in a splitword by adding (first (splitword-post sw)) to (splitword-pre sw)

(check-expect (advance-splitword hel-lo-split) hell-o-split)
(check-expect (advance-splitword (make-splitword (explode "hello") '())) (make-splitword (explode "hello") '()))

(define (advance-splitword sw)
  (cond
    [(end-of-splitword? sw) sw]
    [else (make-splitword (add-at-end (first (splitword-post sw)) (splitword-pre sw)) (rest (splitword-post sw)))]))

; Splitword -> Boolean
; tells me if I'm at the end of a splitword.

(check-expect (end-of-splitword? (make-splitword (list "a") (list '()))) #true)
(check-expect (end-of-splitword? (make-splitword (list "a") (list "b"))) #false)

(define (end-of-splitword? sw)
  (empty? (splitword-post sw)))

; 1String, Splitword
; add-in-middle ads the 1String s onto the "pre" element of a Splitword

(check-expect (add-in-middle "b" (make-splitword (list "a") (list "c"))) (make-splitword (list "a" "b") (list "c")))
(check-expect (add-in-middle "b" (make-splitword (list "a") '())) (make-splitword (list "a" "b") '()))

(define (add-in-middle s sw)
  (make-splitword (add-at-end (splitword-pre sw) s) (splitword-post sw)))

; List, 1String -> List
; add-at-end adds the 1string s onto the end of a list l

(check-expect (add-at-end 5 (list 1 2 3 4)) (list 1 2 3 4 5))
(check-expect (add-at-end 5 '()) 5)

(define (add-at-end s l)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l) (add-at-end s (rest l)))]))

; Splitword -> Word
; converts a splitword into a word

(check-expect (splitword->word hel-lo-split) hello-word)

(define (splitword->word sw)
  (append (splitword-pre sw) (splitword-post sw)))

; List-of-strings -> List-of-strings
; in-dictionary takes in a list of strings and returns the ones that are in the dictionary file DICTIONARY-LOCATION

(check-expect (in-dictionary (list "hello" "asfadfasdfkwkjwejk" "world")) (list "hello" "world"))
(check-expect (in-dictionary (list "asfasdfasdqweqqer")) '())

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (cond
            [(member? (first los) DICTIONARY-AS-LIST) (cons (first los) (in-dictionary (rest los)))]
            [else (in-dictionary (rest los))])]))
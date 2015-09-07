;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |197|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

; A List-of-words is one of:
; - '()
; - (cons Word List-of-words)

(define LOW1 (list DEAR D RAT))
(define ARRANGEMENTS-RAT (list (explode "rat") (explode "rta") (explode "tar") (explode "tra") (explode "art") (explode "atr")))
(define ARRANGEMENTS-RAT-DICT (list (explode "rat") (explode "tar") (explode "art")))

; functions

; String -> List-of-strings
; find all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
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
    [(empty? w) '()]
    [else (... (first w) ... (arrangements (rest w)) ...)]))

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
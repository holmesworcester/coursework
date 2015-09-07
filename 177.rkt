;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |177|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct editor [pre post])
; An Editor is (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – empty 
; – (cons 1String Lo1S)
; interpretation: all of the text in the editor separated into two strings
; the letters in pre precede the cursor in reverse order. in post they follow in normal order

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))
 
; data example 1: 
(define lla-good (make-editor all good))
 
; data example 2:
(define all-good (make-editor lla good))

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
 
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
 
(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))

(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l) (add-at-end (rest l) s))]))

; Editor -> String
; here's how we obtain the text in the editor, as a single string.

(check-expect (export all-good) "allgood")

(define (export e)
  (string-append (implode (rev (editor-pre e))) (implode (editor-post e))))

; String1, String2 -> Editor
; makes the starting text for an editor

(check-expect (create-editor "hey" "man") (make-editor (explode "yeh") (explode "man")))

(define (create-editor pre post)
  (make-editor (rev (explode pre)) (explode post)))

; 



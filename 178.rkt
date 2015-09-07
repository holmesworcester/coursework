;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |178|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; constants 
(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 


; graphical constants 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))


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

; functions

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))

; Editor -> Image
; renders an editor as an image of the two texts separated by the cursor 

(define (editor-render e)
  (place-image/align
    (beside (editor-text (reverse (editor-pre e)))
            CURSOR
            (editor-text (editor-post e)))
    1 1
    "left" "top"
    MT))

; Lo1s -> Image
; renders a list of 1string's as text without using implode

(define (editor-text s)
  (cond
    [(empty? s) (text "" FONT-SIZE FONT-COLOR)]
    [else (beside (text (first s) FONT-SIZE FONT-COLOR) (editor-text (rest s)))]))
 
; Editor KeyEvent -> Editor
; deals with a key event, given some editor

; adding letter for any key incl " "
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))

; backspace
(check-expect (editor-kh (create-editor "" "") "\b")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "\b")
              (create-editor "c" "fgh"))

; left and right

(check-expect (editor-kh (create-editor "" "e") "left")
              (create-editor "" "e"))
(check-expect (editor-kh (create-editor "cd" "fgh") "left")
              (create-editor "c" "dfgh"))

(check-expect (editor-kh (create-editor "e" "") "right")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "right")
              (create-editor "cdf" "gh"))

; up and down

(check-expect (editor-kh all-good "up") all-good) ; let's do nothing for now
(check-expect (editor-kh all-good "down") all-good)

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))


; insert the 1String k between pre and post

(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
 
(check-expect
  (editor-ins (make-editor (cons "d" '())
                           (cons "f" (cons "g" '())))
              "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed)) (editor-post ed)))

; Editor -> Editor
; moves the cursor position one 1String left, if possible


(check-expect (editor-lft (create-editor "" "e"))
              (create-editor "" "e"))
(check-expect (editor-lft (create-editor "ab" "cd"))
              (create-editor "a" "bcd"))

(define (editor-lft ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else (make-editor (rest (editor-pre ed)) (cons (first (editor-pre ed)) (editor-post ed)))]))


; Editor -> Editor
; moves the cursor position one 1String right, if possible 


(check-expect (editor-rgt (create-editor "e" ""))
              (create-editor "e" ""))
(check-expect (editor-rgt (create-editor "ab" "cd"))
              (create-editor "abc" "d"))

(define (editor-rgt ed)
  (cond
    [(empty? (editor-post ed)) ed]
    [else (make-editor (cons (first (editor-post ed)) (editor-pre ed)) (rest (editor-post ed)))]))

 
; Editor -> Editor
; deletes one 1String to the left of the cursor, if possible

(define (editor-del ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else (make-editor (rest (editor-pre ed)) (editor-post ed))]))

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






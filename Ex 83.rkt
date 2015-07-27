;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 83|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; graphical constants
(define CURSOR (rectangle 1 20 "solid" "red"))
(define MT (empty-scene 200 20))
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")

(define-struct editor [pre post])
; Editor = (make-editor String String)
; interpretation (make-editor s t) means the text in the editor is
; (string-append s t) with the cursor displayed between s and t

(define ex1 (make-editor "hello" "world"))
(define ex2 (make-editor "hello " "world"))
(define ex3 (make-editor "hello w" "orld"))

; Editor -> Image
; render takes in an editor worldstate and draws an image of it
; by placing the first string s, then placing CURSOR, then placing the second string t on the background MT
(define (render e)
  (overlay/align "left" "center" (beside (rendertext (editor-pre e)) CURSOR (rendertext (editor-post e))) MT))

; String -> Image
; takes a string and renders an image in the appropriate font size and color
(define (rendertext t)
  (text t TEXT-SIZE TEXT-COLOR))

(render ex1)
(render ex2)
(render ex3)


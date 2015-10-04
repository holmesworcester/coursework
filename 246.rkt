;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |246|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

; A Polygon is one of: 
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

(define POLYTEST (list (make-posn 10 10) (make-posn 20 20) (make-posn 10 20)))
 
(define MT (empty-scene 50 50))
 
; Polygon -> Image 
; adds an image of p to MT
(define (render-polygon p)
  (local (
    ; Polygon -> Posn
    ; extracts the last item from p
    (define (last p)
      (cond
        [(empty? (rest (rest (rest p)))) (third p)]
        [else (last (rest p))]))

    ; A NELoP is one of: 
    ; – (cons Posn '())
    ; – (cons Posn NELoP)
    ; NELoP -> Image
    ; connects the Posns in p in an image

    (define (connect-dots p)
      (cond
        [(empty? (rest p)) MT]
        [else
         (render-line
          (connect-dots (rest p)) (first p) (second p))])))
    ; -IN-
  (render-line (connect-dots p) (first p) (last p))))
 
; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
; (this is generally useful so I'm leaving it global)

(define (render-line im p q)
  (scene+line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

(render-polygon POLYTEST)

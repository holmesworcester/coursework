;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |191|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

; constants

(define MT (empty-scene 50 50))

; definitions

; a Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)


; A NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

; functions

; Polygon -> Image
; renders the given polygon p into MT

(check-expect
  (render-poly
    (list (make-posn 10 10) (make-posn 20 10)
          (make-posn 20 20) (make-posn 10 20)))
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))

(check-expect
  (render-poly
    (list (make-posn 20 0) (make-posn 10 10) (make-posn 30 10)))
  (scene+line
    (scene+line
      (scene+line MT 20 0 10 10 "red")
      10 10 30 10 "red")
    30 10 20 0 "red"))



(define (render-poly p)
  (cond
    [(empty? (rest (rest (rest p))))
     (render-line
       (render-line
         (render-line MT (first p) (second p))
         (second p) (third p))
       (third p) (first p))]
    [else
      (render-line
        (render-poly (rest p)) (first p) (second p))]))


; NELoP -> Image 
; connects the dots in p by rendering lines in MT

(check-expect (connect-dots (list (make-posn 20 0)
                                  (make-posn 10 10)
                                  (make-posn 30 10)))
              (scene+line
               (scene+line MT 20 0 10 10 "red")
               10 10 30 10 "red"))

(define (connect-dots p)
  (cond
    [(empty? (rest p)) MT]
    [else (render-line (connect-dots (rest p)) (first p) (second p))]))

; Image Posn Posn -> Image 
; renders a line from p to q into im
(define (render-line im p q)
  (scene+line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

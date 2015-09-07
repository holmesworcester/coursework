;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |193|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

; constants

(define MT (empty-scene 50 50))

; definitions

; A Polygon is one of: 
; – (list Posn Posn Posn)
; – (cons Posn Polygon)
  
; Polygon -> Image 
; adds an image of p to MT

(check-expect
  (render-polygon
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
  (render-polygon
    (list (make-posn 20 0) (make-posn 10 10) (make-posn 30 10)))
  (scene+line
    (scene+line
      (scene+line MT 20 0 10 10 "red")
      10 10 30 10 "red")
    30 10 20 0 "red"))

; (define (render-polygon p)
;  (render-line (connect-dots p) (first p) (last p)))

; (define (render-polygon p)
;  (connect-dots (cons (last p) p))) ; passed

 (define (render-polygon p)
  (connect-dots (add-at-end (first p) p))) ;also passed


; List, Any -> List
; adds item i to the end of list l

(check-expect (add-at-end 5 (list 1 2 3)) (list 1 2 3 5))
(check-expect (add-at-end 5 '()) (cons 5 '()))
              
(define (add-at-end i l)
  (cond
    [(empty? l) (cons i '())]
    [else (cons (first l) (add-at-end i (rest l)))]))

; A NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)
 
; NELoP -> Image
; connects the Posns in p in an image

(check-expect (connect-dots (list (make-posn 20 0)
                                  (make-posn 10 10)
                                  (make-posn 30 10)))
              (scene+line
               (scene+line MT 20 0 10 10 "red")
               10 10 30 10 "red"))

(define (connect-dots p)
  (cond
    [(empty? (rest p)) MT]
    [else
      (render-line
        (connect-dots (rest p)) (first p) (second p))]))
 
; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
 
; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))
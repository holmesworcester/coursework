;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |240|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

; [List-of THING] STARTING-THING2 [Thing, Thing2 -> Thing] -> PRODUCT
; they say: ; [List-of X] Y [X Y -> Y] -> Y so i was pretty close
; turns a list of items into some kind of composite based on an cumulative operation on all of them.
; I'm not sure the best way to describe the ins and outs of the function in the signature.
; I think the key thing about the function is that it can consume its own output.

(define (combine l start function)
  (cond
    [(empty? l) start]
    [else
     (function (first l) (combine (rest l) start function))]))

; [List-of Number] -> Number
; does the same thing as product but using the abstraction "combine"

(check-expect (product-from-combine '(1 2 3)) (* 1 2 3))

(define (product-from-combine lon)
  (combine lon 1 *))

; [List-of Posn] -> Image
; does the same thing as image* but using the abstraction "combine"

(check-expect (image*-from-combine (list (make-posn 50 50))) (place-image dot 50 50 emt))

(define (image*-from-combine lop)
  (combine lop emt place-dot))

; [List-of Number] -> Number
(check-expect (product '(1 2 3)) (* 1 2 3))

(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

 
; [List-of Posn] -> Image

(check-expect (image* (list (make-posn 50 50))) (place-image dot 50 50 emt))
 
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot (first l)
                (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image dot
               (posn-x p) (posn-y p)
               img))

; graphical constants:    
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))


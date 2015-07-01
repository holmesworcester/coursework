;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname car) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(define (wheel diameter)(circle diameter "solid" "black"))

(place-images
   (list
    (circle 10 "solid" "black")
         (circle 10 "solid" "black")
         (circle 30 "solid" "red")
         (rectangle 30 50 "solid" "white"))
   (list (make-posn 30 90)
         (make-posn 70 90)
         (make-posn 50 90)
         (make-posn 85 50))
   (rectangle 100 100 "solid" "white"))
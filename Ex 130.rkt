;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 130|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define PLANETS
  (cons "Neptune"
        (cons "Uranus"
              (cons "Saturn"
                    (cons "Jupiter"
                          (cons "Mars"
                                (cons "Earth"
                                      (cons "Venus"
                                            (cons "Mercury"
                                                 '()))))))))) ; probably should've done this the other way :) 

(define MEAL
  (cons "Salad"
        (cons "Chicken"
              (cons "Beans"
                    (cons "Rice"
                          (cons "French Fries"
                                '())))))) ; and this!

(define COLORS
  (cons "Red"
        (cons "Orange"
              (cons "Yellow"
                    (cons "Green"
                          (cons "Blue"
                                (cons "Indigo"
                                      (cons "Violet")))))))) ; same for this!




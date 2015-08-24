;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |misc work on 130s|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

; A List-of-strings is one of:
; - '()
; - (cons String List-of-booleans)
; interpretation: a list of Strings

; List-of-strings -> Number
; determines how many strings are on alos
(define (how-many alos)
  (cond
    [(empty? alos) 0]
    [else
     (+ 1 (how-many (rest alos)))]))

(how-many PLANETS)

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 131 - follow along work|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A List-of-names is one of: 
; – '()
; – (cons String List-of-names)
; interpretation a List-of-names represents a list of invitees by last name

(define LIST-OF-NAMES
  (cons "Holmes"
      (cons "Giseli"
            (cons "Martina"
                  '()))))

; (cons "1" (cons "2" '())) is a List-of-names because "1" and "2" are Strings
; and because somebody can name their daughter "2".

; (cons 2 '()) isn't because 2 is a Number not a String
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |294|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Exercise 294. Use match to design the function replace, which substitutes the area code 713 with 281
; in a list of phone records. For a structure type definition of phone records, see above.
; Formulate a suitable data definition first. If you are stuck, look up your solution for exercise 170.

(require 2htdp/abstraction)

; a Phone is a structure (make-phone N N N)
; interpretation: a US phone number divided into its area code, exchange ("switch") and 4-digit suffix.
(define-struct phone [area switch four])

(define 713phone (make-phone 713 555 1212))
(define 281phone (make-phone 281 555 1212))

(define phonebook (list 713phone 281phone))
(define phonebook-replaced (list 281phone 281phone))

; [List-of Phone] -> [List-of Phone]
; substitutes the area code 713 with 281 in a list of phone records

(check-expect (replace phonebook) phonebook-replaced)

(define (replace l)
  (map (lambda (p)
         (match p
           [(phone 713 s f) (make-phone 281 s f)]
           [p p])) l))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |256|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Exercise 256. An inventory record specifies the name of an item, a description, the acquisition price,
; and the recommended sales price.

; Define a function that sorts a list of inventory records by the difference between the two prices. 

; definitions

; An IR is (make-ir String String Number Number)
; interpretation: an inventory record that specifies the name of an item, a description, the acquisition price,
; and the recommended sales price.
(define-struct ir [name description acqprice saleprice])

(define hat (make-ir "hat" "nice hat" 5 20))
(define cat (make-ir "cat" "nice cat" 20 30))

; functions

; [List-of IR] -> [List-of IR]
; sorts a list of inventory records by the difference between the two prices, highest "markup" first.

(check-expect (sort-by-markup (list hat cat)) (list hat cat))
(check-expect (sort-by-markup (list cat hat)) (list hat cat))

(define (sort-by-markup loir)
  (local (; IR, IR -> Boolean
          ; compares two IRs and returns true if the markup of the first is greater than the second's
          (define (higher-markup? ir1 ir2)
            (> (markup ir1) (markup ir2)))
          ; IR -> Number
          ; figures out the markup e.g. difference between acquisition price and sale price
          (define (markup an-ir)
            (- (ir-saleprice an-ir) (ir-acqprice an-ir))))
    (sort loir higher-markup?)))
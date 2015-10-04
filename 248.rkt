;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |248|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; IR is a structure (make-ir String Number)
; interpretation: the name of the structure and the price.
(define-struct ir [item price])
(define gum (make-ir "gum" .5))
(define soda (make-ir "soda" 1.25))

; An Inventory is [List-of IR]
; interpretation: a list of inventory records aka an inventory

(define testinventory (list gum soda gum gum gum soda soda gum soda))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than $1
; 109 steps

(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else (cond
            [(<= (ir-price (first an-inv)) 1.0)
             (cons (first an-inv) (extract1 (rest an-inv)))]
            [else (extract1 (rest an-inv))])]))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than $1
; 111 steps

(define (extract1-local an-inv)
  (cond
    [(empty? an-inv) '()]
    [else (local
            (
             (define extract1-rest (extract1 (rest an-inv)))
             (define first-item (first an-inv))
             )         
           (cond
            [(<= (ir-price first-item) 1.0)
             (cons first-item extract1-rest)]
            [else extract1-rest]))]))

; the local definition does not improve speed.

(extract1-local testinventory)
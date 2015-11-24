;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |352|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require xml)

; An Xexpr.v2 is 
; – (cons Symbol [List-of Xexpr.v2])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))

; OR IN OTHER WORDS

; An Xexpr.v2 is one of:
; - (cons Symbol '())
; – (cons Symbol Xexpr.v2) 
; – (cons Symbol (cons AttributeList '())
; – (cons Symbol (cons AttributeList Xexpr.v2))

; An AttributeList is one of:
; - '()
; - (cons Attribute AttributeList)

; An Attribute is 
;   (cons Symbol (cons String '()))

; 1. <transition from="seen-e" to="seen-f" />

(cons 'transition (cons (cons (cons 'from (cons "seen-e" '())) (cons (cons 'to (cons "seen-f" '())) '())) '()))

; 2. <ul><li><word /><word /></li><li><word /></li></ul>

(list 'ul (list 'li (list 'word) (list 'word)) (list 'li (list 'word)))

; 3. <end></end>

(cons 'end '())

; Which one could be represented in Xexpr.v0 or Xexpr.v1? image

; #3 could be represented in Xexpr.v0 because it is just one element. 
; #2 could be represented in Xexpr.v1 because it has no attributes.


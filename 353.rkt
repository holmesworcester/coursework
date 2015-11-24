;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |353|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; An Xexpr.v0 (short for X-expression) is 
;   (cons Symbol '())

; An Xexpr.v1 is 
;   (cons Symbol [List-of Xexpr.v1])

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

; Which one could be represented in Xexpr.v0 or Xexpr.v1? 

; #3 could be represented in Xexpr.v0 because it is just one element. 
; #2 could be represented in Xexpr.v1 because it has no attributes.

; Ex 353

; 1. '(server ((name "example.org")))
; <server name="example.org" />

; 2. '(carcassonne (board (grass)) (player ((name "sam"))))
; <carcassone><board><grass /></board><player name="sam" /></carcassone>

; 3. '(start)
; <start /> or <start></start>

; Which ones are elements of Xexpr.v0 or Xexpr.v1?
; #3 is an element of Xexpr.v0 but the others have attributes so can only be elements of Xexpr.v2

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |308|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [number symbol left right])
; A BinaryTree (short: BT) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define tree1 (make-node
  15
  'd
  NONE
  (make-node 24 'i NONE NONE)))
     
(define tree2 (make-node
  15
  'd
  (make-node 87 'h NONE NONE)
  NONE))

; BT, N -> Boolean
; determines whether a given number occurs in some given BT

(check-expect (contains-bt? tree1 15) #t)
(check-expect (contains-bt? tree1 90) #f)
(check-expect (contains-bt? tree1 17) #f)
(check-expect (contains-bt? tree2 87) #t)

(define (contains-bt? bt n)
  (cond
    [(equal? bt NONE) #f]
    [else (or (= n (node-number bt)) (contains-bt? (node-left bt) n) (contains-bt? (node-right bt) n))]))

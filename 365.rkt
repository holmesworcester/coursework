;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |365|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)

; A FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))
; A FSM-State is a String that specifies color
 
; data examples 
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))
 
; FSM FSM-State -> FSM-State 
; match the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  ; State of the World: FSM-State
  (big-bang state0
    [to-draw
      (lambda (current)
        (square 100 "solid" current))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))
 
; [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in the association list
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "next state not found"))))

; [List-of [List X Y]], X -> [List X Y]
; finds the first pair in a list of pairs for which the first item matches X. If it finds nothing, returns false.

(check-expect (assoc "green" fsm-traffic) (list "green" "yellow"))

(define (assoc x a-list)
  (cond
    [(empty? a-list) #f]
    [else (if (equal? x (first (first a-list))) (first a-list) (assoc x (rest a-list)))]))

(simulate "green" fsm-traffic)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |367|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Exercise 367. Reformulate the data definition for 1Transition so that it is possible to restrict transitions to certain key strokes.
; Try to formulate the change so that find continues to work without change. What else do you need to change to get the complete program to work?
; Which part of the design recipe provides the answer(s)? See exercise 215 for the original exercise statement.

; one note is that I didn't make it "possible" to restrict transitions, I made it required. 

(require 2htdp/universe)
(require 2htdp/image)

; A FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons KeyState '()))
; A FSM-State is a String that specifies color

; A KeyState is [List String FSM-State]
; interpretation: the next state and the key required to advance to that state. a String of "" means any key will do.
 
; data examples 
(define fsm-traffic
  '(("red" ("g" "green")) ("green" ("y" "yellow")) ("yellow" ("" "red"))))
 
; FSM FSM-State -> FSM-State 
; match the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  ; State of the World: FSM-State
  (big-bang state0
    [to-draw
      (lambda (current)
        (place-image (text current 12 "black") 50 50 (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (local (; let's get the KeyState for this 1Transition
                (define keystate (find transitions current))
                ; and the key event for the KeyState
                (define the-key (first keystate))
                ; and the next state for the KeyState
                (define next-state (second keystate)))
          ;-IN-
          (if (or (string=? the-key "") (string=? key-event the-key)) next-state (error "wrong key pressed"))))])); empty strings mean anything will advance to the next state.)


; X, [List-of [List X Y]] -> [List X Y]
; finds the first pair in a list of pairs for which the first item matches X. If it finds nothing, returns false.

(check-expect (assoc "green" fsm-traffic) '("green" ("y" "yellow")))
(check-expect (assoc "violet" fsm-traffic) #f)

(define (assoc x a-list)
  (cond
    [(empty? a-list) #f]
    [else (if (equal? x (first (first a-list))) (first a-list) (assoc x (rest a-list)))]))

(check-expect (find fsm-traffic "green") '("y" "yellow"))
(check-error (find fsm-traffic "violet") "next state not found")

; [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in the association list
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "next state not found"))))


(simulate "green" fsm-traffic)
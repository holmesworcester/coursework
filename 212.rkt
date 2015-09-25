;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |212|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is 
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a String that specifies a color. 
 
; interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to key strokes 

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

; Exercise 212. Design state=?, an equality predicate for states. image

; huh? this is easy, right?

(define (state=? x)
  (string=? x))
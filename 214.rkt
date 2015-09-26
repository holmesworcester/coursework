;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |214|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; A FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is 
;   (make-transition FSM-State FSM-State)

; ; A FSM.v2 is one of:
;   – '()
;   – (cons Transition.v2 FSM.v2)

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

; FSM-State is a String
 
; interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to key strokes 

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

; Excercise 213.
; The BW Machine is a FSM that flips from black to white and back to black for every key event.
; Formulate a data representation for the BW Machine.

(define bw-machine
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

; functions

; FSM-State, FSM-State -> Boolean

(define (state=? s1 s2)
  (string=? s1 s2))
 
; SimulationState.v1 -> Image
; renders a world state as an image 
(define (render-state.v1 s)
  empty-image)
 
; SimulationState.v1 -> SimulationState.v1
; finds the next state from a key stroke ke and current state cs
(define (find-next-state.v1 cs ke)
   cs)

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

; SimulationState.v2 -> Image
; renders a world state as an image 
(define (render-state.v2 s)
  empty-image)

; SimulationState.v2 -> SimulationState.v2
; finds the next state from a key stroke ke and current state cs
(define (find-next-state.v2 cs ke)
   cs)

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed by a player with the given FSM 
(define (simulate.v2 a-fsm s0)
  (big-bang (make-fs a-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 

(define (state-as-colored-square a-fs)
  (square 100 "solid" (fs-current a-fs)))

(check-expect (state-as-colored-square (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from a key stroke ke and current state cs

(check-expect (find-next-state (make-fs fsm-traffic "red") "n")
              (make-fs fsm-traffic "green"))
(check-expect (find-next-state (make-fs fsm-traffic "red") "a")
              (make-fs fsm-traffic "green"))
(check-expect (find-next-state (make-fs fsm-traffic "green") "q")
              (make-fs fsm-traffic "yellow"))
(check-expect (find-next-state (make-fs fsm-traffic "yellow") "n")
              (make-fs fsm-traffic "red"))

(define (find-next-state a-fs ke)
  (make-fs (fs-fsm a-fs)
            (find (fs-fsm a-fs) (fs-current a-fs))))

; FSM FSM-State -> FSM-State
; finds the state matching current in the transition table
 
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "black") "not found: black")
 
(define (find transitions current)
  (cond
    [(empty? transitions) (error (string-append "not found: " current))]
    [else (cond
            [(state=? current (transition-current (first transitions))) (transition-next (first transitions))] ; if it's a match, return the next state
            [else (find (rest transitions) current)])])) ; else keep looking

; Exercise 215. Here is a revised data definition for Transition:
; Represent the FSM from exercise 111 using lists of Transition.v2s; ignore error and final states.

; 111: Specifically, it must accept any sequence that starts with "a", is followed by an arbitrarily long mix of "b" and "c", and ends in "d". As soon as it encounters this "d", the program stops running. If these four keys are out of order or if any other key is hit, the program must also shut down.
; Clearly, "acbd" is one example of an acceptable string; "ad" and "abcd" are two others. Of course, "da", "aa", or "d" do not match.

(define fsm-abcd
  (list (make-ktransition "white" "a" "yellow")
        (make-ktransition "yellow" "b" "yellow")
        (make-ktransition "yellow" "c" "yellow")
        (make-ktransition "yellow" "d" "green")))

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed by a player with the given FSM 
(define (simulate.v2 a-fsm ke s0)
  (big-bang (make-fs a-fsm ke s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

; (simulate.v2 fsm-traffic "red")
; (simulate.v2 bw-machine "black")

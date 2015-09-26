;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |216|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; FSM-State is a String
; interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to key strokes 

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])

; An FSM.v2 is a structure: 
;   (make-fsm FSM-State LOT FSM-State)
; A LOT is one of: 
; – '() 
; – (cons Transition.v3 LOT)
; A Transition.v3 is a structure: 
;   (make-transition FSM-State KeyEvent FSM-State)

(define abcd
  (list (make-transition "white" "a" "yellow")
        (make-transition "yellow" "b" "yellow")
        (make-transition "yellow" "c" "yellow")
        (make-transition "yellow" "d" "green")))

(define abcd-fsm (make-fsm "white" abcd "green"))

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

(define fsm-traffic
  (list (make-transition "red" "a" "green")
        (make-transition "green" "a" "yellow")
        (make-transition "yellow" "a" "red")))

; functions

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed by a player with the given FSM 
(define (simulate.v2 a-fsm)
  (big-bang (make-fs a-fsm (fsm-initial a-fsm))
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when final-state?])) ; this seems right, right?

; SimulationState.v2 -> Boolean
; tells me when I'm in the final state so that the program stops.

(define (final-state? ss)
  (state=? (fs-current ss) (fsm-final (fs-fsm ss))))

; FSM-State, FSM-State -> Boolean
(define (state=? s1 s2)
  (string=? s1 s2))
 
; SimulationState.v1 -> Image
; renders a world state as an image 
(define (render-state.v1 s)
  empty-image)
 
; SimulationState.v2 -> Image 
; renders current world state as a colored square 

(define (state-as-colored-square a-fs)
  (square 100 "solid" (fs-current a-fs)))

(check-expect (state-as-colored-square (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from a key stroke ke and current state cs

(define (find-next-state a-fs ke)
  (make-fs (fs-fsm a-fs)
            (find (fsm-transitions (fs-fsm a-fs)) (fs-current a-fs) ke))) ; added ke to the search.

; FSM, FSM-State, KeyEvent -> FSM-State
; finds the state matching current in the transition table
 
(check-expect (find abcd "white" "a") "yellow")
(check-expect (find abcd "yellow" "b") "yellow")
(check-expect (find abcd "yellow" "c") "yellow")
(check-expect (find abcd "yellow" "d") "green")
(check-error (find abcd "yellow" "x") "not found: yellow / x")

(define (find transitions current ke)
  (cond
    [(empty? transitions) (error (string-append "not found: " current " / " ke))] ; tells me the key it searched for
    [else (cond
            [(and (state=? current (transition-current (first transitions))) (string=? ke (transition-key (first transitions)))) (transition-next (first transitions))] ; if it's a match, return the next state
            [else (find (rest transitions) current ke)])])) ; else keep looking

; Exercise 215. Here is a revised data definition for Transition:
; Represent the FSM from exercise 111 using lists of Transition.v2s; ignore error and final states.

; 111: Specifically, it must accept any sequence that starts with "a", is followed by an arbitrarily long mix of "b" and "c", and ends in "d". As soon as it encounters this "d", the program stops running. If these four keys are out of order or if any other key is hit, the program must also shut down.
; Clearly, "acbd" is one example of an acceptable string; "ad" and "abcd" are two others. Of course, "da", "aa", or "d" do not match.


; (simulate.v2 fsm-traffic "red")
; (simulate.v2 bw-machine "black")
(simulate.v2 abcd-fsm)
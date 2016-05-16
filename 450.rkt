;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |450|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; A FSM is (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is 
;   (make-transition FSM-State 1String FSM-State)
; A FSM-State is String
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM-State String [List-of 1Transition] -> [Maybe FSM-State]
; returns the next fsm state given the current state, a 1String, and a-fsm as a constant

(check-expect (next-state "AA" "a" (fsm-transitions fsm-a-bc*-d)) "BC")
(check-expect (next-state "BC" "b" (fsm-transitions fsm-a-bc*-d)) "BC")
(check-expect (next-state "BC" "d" (fsm-transitions fsm-a-bc*-d)) "DD")
(check-expect (next-state "BC" "q" (fsm-transitions fsm-a-bc*-d)) #f)
(check-expect (next-state "GG" "q" (fsm-transitions fsm-a-bc*-d)) #f)
(check-expect (next-state "AA" "b" (fsm-transitions fsm-a-bc*-d)) #f)

;(define-struct transition [current key next])
;(define-struct fsm [initial transitions final])

(define (next-state current-state key lot)
  (cond
    [(empty? lot) #f]
    [(and (equal? (transition-current (first lot)) current-state) (equal? key (transition-key (first lot)))) (transition-next (first lot))]
    [else (next-state current-state key (rest lot))]))

; FSM String -> Boolean 
; does the given string match the regular expression expressed as fsm
;  Design the necessary auxiliary function locally to fsm-match?. In this context,
; represent the problem as a pair of parameters: the current state of the finite state machine
; and the remaining list of 1Strings.

(check-expect (fsm-match? fsm-a-bc*-d "abcbcbcbcbcccccbbbbbbcccd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "abd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "acd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "aa") #f)
(check-expect (fsm-match? fsm-a-bc*-d "aaaaaaaa") #f)
(check-expect (fsm-match? fsm-a-bc*-d "acccccccccccd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "addddd") #f)
(check-expect (fsm-match? fsm-a-bc*-d "abcddddd") #f) ; is this right? yes because there's no transition after d
(check-expect (fsm-match? fsm-a-bc*-d "q") #f)
(check-expect (fsm-match? fsm-a-bc*-d "qabd") #f)
(check-expect (fsm-match? fsm-a-bc*-d "abdaaaa") #f)
(check-expect (fsm-match? fsm-a-bc*-d "a") #f) ; I'm assuming it needs to make it all the way to the end to match
(check-expect (fsm-match? fsm-a-bc*-d "ab") #f) ; I'm assuming it needs to make it all the way to the end to match

(define (fsm-match? a-fsm a-string) ; switch fsm-dummy-just-uses-a-fsm-constant for a-fsm when all composed
  (local (; FSM-State [List-of 1String] -> Boolean
          ; tells me if, given a [List-of 1String] and a current state, the input from the string matches the pattern described in the FSM.
          ; that is, every 1String in the [List-of 1String] should transition until the final state is reached.
          ; takes the FSM as a constant, which is fine because I'll put this in a local definition once tested.
          (define (fsm-match-until-end? current-state a-lo1s)
            (cond
              [(empty? a-lo1s) ; ensures I'm not dealing with an empty list anywhee else
               (cond
                 [(equal? (fsm-final a-fsm) current-state) #t] ; fsm is final. probably should abstract out
                 [else #f])] ; didn't necessarily break the rules but didn't reach the final state of the FSM
              [(false? (next-state current-state (first a-lo1s) (fsm-transitions a-fsm))) #f] ; define next-state so that it returns false if the transition fails.
              [else (fsm-match-until-end? (next-state current-state (first a-lo1s) (fsm-transitions a-fsm)) (rest a-lo1s))]))) ;current transition is cool but what about all the others
          ;-IN-
  (fsm-match-until-end? (fsm-initial a-fsm) (explode a-string))))
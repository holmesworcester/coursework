#lang planet neil/sicp

; Nice. I'm proud of this one. 

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; Show that this procedure is not correct.
; In particular, draw box-and-pointer diagrams representing list structures
; made up of exactly three pairs for which Ben's procedure would
; return 3; return 4; return 7; never return at all.

; return 3 is easy. That's the structure the function expects, i.e. a typical list.
(count-pairs (list 1 2 3))

; return 4
; the car of the second pair points to the third pair (as well as the cdr)

(define returns4 (list 1 2 3))
(set-car! (cdr returns4) (cdr (cdr returns4)))

(count-pairs returns4)

; a structure of 3 pairs where the both the car and cdr refer to the next pair returns 7
; drawing: 2 boxes stacked on top of each other with arrows pointing from each box to the next pair.

(define a (list 1))
(define b (list 2))
(set-car! b a)
(set-cdr! b a)
(define 7pair (list 3))
(set-car! 7pair b)
(set-cdr! 7pair b)

(count-pairs 7pair)

; Never return at all. Structure: where the cdr of the last pair points to the first pair.

(define 3-pair-of-no-return (list 1 2 3))
(set-cdr! (cdr (cdr 3-pair-of-no-return)) 3-pair-of-no-return)

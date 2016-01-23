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

; Devise a correct version of the count-pairs procedure of Exercise 3-16 that
; returns the number of distinct pairs in any structure.
; (Hint: Traverse the structure, maintaining an auxiliary data structure
; that is used to keep track of which pairs have already been counted.)

(define (get-new-pair) '(() ()))

; X, [List-of X] -> Boolean
; tells me if a thing is a member of a list, using the comparison eq? 

(define (member? x l)
  (cond
    [(null? l) #f]
    [(eq? x (car l)) #t]
    [else (member? x (cdr l))]))

(check-expect (member? 4 (list 1 2 3 4 5 6 7)) #t)
(check-expect (member? 100 (list 1 2 3 4 5 6 7)) #f)

; Pair, [List-of Pair] -> Number (what is the signature here???)
; counts the number of unique pairs in a structure, even in all my weird examples
; excludes any pair in the given [List-of Pair] from the count, so that it can pass down a
; list of already counted pairs.

; Okay, I get why it's not working. It's because I'm just passing it down each branch. I need to use mutation to store a global list.

(define (pairs p)
  (let ((list-of-counted-pairs (get-new-pair)))
    (define (pair-counter p)
      (let ((zero-if-duplicate-else-1
             (cond
               [(not (pair? p)) 0] ; makes it zero so that we don't get an error if it's empty
               [(member? (list (car p) (cdr p)) list-of-counted-pairs) 0]
               [else 1]))) ; only counts it if it's not already counted.
        ;-IN-
        (if (pair? p) (set-cdr! list-of-counted-pairs (list (list (car p) (cdr p))))) ; I've already checked, so I can update the list. Needs to make sure it's a pair first.
        (cond
          [(not (pair? p)) 0]
          [else (+ (pairs (car p))
                   (pairs (cdr p))
                   zero-if-duplicate-else-1)])))
    ;-IN-
    (pair-counter p)))

(check-expect (pairs 7pair) 3)
(check-expect (pairs returns4) 3)
(check-expect (pairs (list 1 2 3)) 3)
(check-expect (pairs 3-pair-of-no-return) 3)


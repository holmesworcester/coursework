#lang planet neil/sicp

; constants

(define cycle (list 1 2 3 4))
(define lon (list 1 2 3 4))

; functions

; List -> Pair
; returns the last pair in a list

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; List -> List (circular list)
; makes a list into a cycle

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(make-cycle cycle)

(define short-circuit (append '(a b) cycle))

; X, [List-of X] -> Boolean
; tells me if a thing is a member of a list, using the comparison eq? 

(define (member? x l)
  (cond
    [(null? l) #f]
    [(eq? x (car l)) #t]
    [else (member? x (cdr l))]))

(check-expect (member? 4 (list 1 2 3 4 5 6 7)) #t)
(check-expect (member? 100 (list 1 2 3 4 5 6 7)) #f)

; Pair -> Boolean
; tells me if a Pair and all the pairs it points to has a cycle, that is if the CDR of a pair is equal to another item in the list.

(define (iscycle? l)
  (define (check-each-against-l? alox)
    (cond
      [(null? alox) #f]
      [(member? (car alox) l) #t] ; check this
      [else (check-each-against-l? (cdr l))]))
  ;-IN-
  (check-each-against-l? l))

     
(check-expect (iscycle? cycle) #t)
(check-expect (iscycle? lon) #f)
(check-expect (iscycle? short-circuit) #t)


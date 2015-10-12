;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |280|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; X [List-of X] -> [Maybe [List-of X]]
; produces the first sublist of l that starts with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x) l (find x (rest l)))]))


; Exercise 280. Develop found?, a specification for the find function:

; X, [List-of X], [Maybe [List-of X]] -> Boolean
; returns true if l2 is a possible result of (find x l1)

(check-expect (found? 5 '(1 2 3 4 5) '(5)) #true)
(check-expect (found? 10 '(1 2 3 4 5) #false) #true)
(check-expect (found? 1 '(1 2 3 4 5) '(1)) #false)
(check-expect (found? 1 '(1 2 3 4 5) '(1 2 3 4 5)) #true)
(check-expect (found? 2 '(1 2 3 4 5) '(2 3 4 5)) #true)
(check-expect (found? 10 '(1 2) '(10)) #false)
(check-expect (found? 1 '(1 1 1 1) '(1 1 1)) #false)
(check-expect (found? 1 '(1 1 1) '(1 1 1 1 1 1 1)) #false)
(check-expect (found? 5 '() #false) #true)
           
; should i use find for this?

(define (found? x l1 l2)
  (cond
    [(false? l2) (or ; when is it right for found to return false? either...
                  (empty? l1) ; when l1 is empty or...
                  (not (member? x l1)))] ; when x isn't a member of l1.
    [else
     (and ; when is l2 really a find from l1 given x? when alll of these are true:
      (>= (length l1) (length l2)) ; l2 is the smaller list, or they're both the same size.
      (match-up? (reverse l2) (reverse l1)) ; their last items are equal, up to the length of the smaller.
      (not (member? x (foldr remove l1 l2))))])) ; makes sure x does not exist outside the tail.

; [List-of X], [List-of X] -> Boolean
; returns true if l2 is a tail of l1, otherwise returns false.

(check-expect (match-up? '(1 2 3) '(1 2 3)) #true)
(check-expect (match-up? '(1 2) '(1 2 3)) #true)
(check-expect (match-up? '(1) '(1 2 3)) #true)
(check-expect (match-up? '(5 6) '(1 2 3)) #false)

(define (match-up? small long)
  (local (; set n to be the length of the small list, so I can use it to pull items
          (define (match-up-until-x? l1 l2 x)
            (cond
              [(zero? x) #true]
              [else (and (equal? (first l1) (first l2)) (match-up-until-x? (rest l1) (rest l2) (sub1 x)))])))
    (match-up-until-x? small long (length small))))

        
; Use found? to formulate a check-satisfied test for find.

(check-satisfied (find 10 (list 2 5 23 12 52345 5443 10 23 23 23 5 4526645 232)) (found? 10 (list 2 5 23 12 52345 5443 10 23 23 23 5 4526645 232)))
; okay, I don't understand this. Why can't I pass the result of find to found?
              
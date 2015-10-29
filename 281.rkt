;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |281|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; X [List-of X] -> [Maybe N]
; determine the (0-based) index of the first occurrence of x in l, 
; #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; X [List-of X] -> [[Maybe N] -> Boolean]
; takes in a list and an item, and returns a function of one argument that take in the results of an index function
; (a number or false) to see if the result is correct. if it is, returns boolean. if not, returns false.


(define (is-index? search-term list)
  (cond
    [(not (member? search-term list)) false?] ; if the search-term isn't in the list, it's always going to be false.
    [else
     (lambda (index-or-f)
       (and ; list all the things it has to be here.
           (number? index-or-f) ; is a number
           (< index-or-f (length list)) ; is a number less than (length list)
           (equal? search-term (nth-item index-or-f list)) ; search-term is in fact that nth item in the list
           (not (member? search-term (list-up-to index-or-f list))) ; does not occur before that in the list
           ))]))

; Number, [List-of X] -> [Maybe X]
; gives me the nth item in a list

(check-expect (nth-item 0 (list 1 2 3)) 1)
(check-expect (nth-item 1 (list 1 2 3)) 2)

(define (nth-item n l)
  (cond
    [(zero? n) (first l)]
    [else (nth-item (sub1 n) (rest l))]))

; Number, [List-of X] -> [List-of X]
; gives me a list of length of the Number n based on showing all the items in the list through the nth item.

(check-expect (list-up-to 5 '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (list-up-to 1 '("a" "b" "c")) (list "a"))

(define (list-up-to i l)
  (cond
    [(empty? l) '()]
    [(zero? i) '()]
    [else (cons (first l) (list-up-to (sub1 i) (rest l)))]))

; here's my check satisfied test:

(check-satisfied (index 5 (list 1 2 3 4 5)) (is-index? 5 (list 1 2 3 4 5)))
(check-satisfied (index 5 '()) (is-index? 5 '()))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |417|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; my mission: figure out at what list length quicksort becomes faster, and write a sort function that combines insertion
; sort and quicksort to get the advantages of both.
; I don't have a good programatic way to check the winner lots of times because I don't know how to grab the output of the time function
; It seems like the cut off is around 50, but it's unclear because the data varies and isn't very exact.
; It's a little hard to tell. My guess is that clever sort saves quicksort from being terrible on certain types of lists.

; the max random range of the test lists I'm creating
(define MAX 100)
(define CUTOFF 50)

; [List-of Number], N -> [List-of Number]
; sorts using a combination of quicksort and insertion sort. When a list is below length n, it uses insertion sort.
; sorts in descending order

(check-expect (clever-sort '()) '())
(check-expect (clever-sort (list 12 20 -5)) (list 20 12 -5))
(check-expect (clever-sort (list 3 2 1)) (list 3 2 1))
(check-expect (clever-sort (list 1 2 3)) (list 3 2 1))

(define (clever-sort alon)
  (cond
    [(empty? alon) '()]
    [(< (length alon) CUTOFF) (sort> alon)]
    [else (local ((define pivot (first alon)))
            (append (clever-sort (larger-items alon pivot))
                    (list pivot)
                    (clever-sort (smaller-items alon pivot))))]))

; SortFunction, SortFunction, N -> N
; consumes two sort functions, tries lengths of lists from 0 to n, and and tells me for what n the first one is faster) 

(define (race sort1 sort2 n)
  (cond
    [(winner? sort1 sort2 n) n]
    [else (race sort1 sort2 (add1 n))]))
 
; SortFunction, SortFunction, Number -> Boolean
; returns true if the first SortFunction is faster for a random test list of length n. Otherwise returns false.

(define (winner? sort1 sort2 n)
  (local (; define a test list
          (define test-list (create-tests n)))
    ;-IN-
    (< (time (sort1 test-list)) (time (sort2 test-list)))))

; N -> [List-of N]
; creates long random lists of numbers in the range (0, MAX] with length n.

(check-expect (length (create-tests 1000)) 1000)

(define (create-tests n)
  (build-list n (lambda (x) (random MAX))))

; [List-of Number] -> [List-of Number]
; creates a list of numbers with the same numbers as
; alon, sorted in ascending order
; assume the numbers are all distinct 

(check-expect (quick-sort '()) '())
(check-expect (quick-sort (list 12 20 -5)) (list 20 12 -5))
(check-expect (quick-sort (list 3 2 1)) (list 3 2 1))
(check-expect (quick-sort (list 1 2 3)) (list 3 2 1))

(define (quick-sort alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort (larger-items alon pivot))
                    (list pivot)
                    (quick-sort (smaller-items alon pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
; creates a list with all those numbers on alon  
; that are larger than n
(define (larger-items alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (larger-items (rest alon) n))
              (larger-items (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
; creates a list with all those numbers on alon  
; that are smaller than n
(define (smaller-items alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smaller-items (rest alon) n))
              (smaller-items (rest alon) n))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of alon

(check-expect (sort> '()) '())
(check-expect (sort> (list 12 20 -5)) (list 20 12 -5))
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))

(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [(cons? alon) (insert (first alon) (sort> (rest alon)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))

(define (insert n alon)
  (cond
    [(empty? alon) (cons n '())]
    [else (if (>= n (first alon))
              (cons n alon)
              (cons (first alon) (insert n (rest alon))))]))

; (race quick-sort sort> 10)
(define test-list (create-tests 100))

(time (sort> test-list))
(time (quick-sort test-list))
(time (clever-sort test-list))



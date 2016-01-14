;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |426|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Q. Consider (make-table 1024 a) and assume (= (a 1023) 0). How many recursive calls to find are needed in find-linear and find-binary respectively?
; A. ~1023 and ~10

(define-struct table [length array])
; A Table is a 
;   (make-table N [N -> Number])

(define table1 (make-table 3 (lambda (i) i)))
 
; N -> Number
(define (a2 i)
  (if (= i 0) pi (error "table2 is not defined for i =!= 0")))
 
(define table2 (make-table 1 a2))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; Table -> Number
; consumes a monotonically increasing table and finds the smallest index for a root of the table.
; (where the root is the number in (table-array t) that is closest to 0)

(check-expect (find-linear (make-table 10 (lambda (i) (- (expt 2 i) 16)))) 4) ; monotonic
(check-expect (find-linear (make-table 10 (lambda (i) (- i 4.1)))) 4) ; monotonic but on the edge
(check-expect (find-linear (make-table 10 (lambda (i) (- i 4.9)))) 5) ; monotonic but on the other edge
(check-expect (find-linear (make-table 100 (lambda (i) (- i 99)))) 99) ; root on right side
(check-expect (find-linear (make-table 100 (lambda (i) (- i 1000000)))) 99) ; root off the right side
(check-expect (find-linear table1) 0) ; root on left side
(check-expect (find-linear (make-table 10 (lambda (i) (- (* 10 i) 58)))) 6) ; two are close, one on right is closer

(define (find-linear table) 
  (local (; define table length as a local constant
          (define n (table-length table))
          ; N -> Number
          ; consumes a number N, and searches up to N in the table (given as a constant) to find the smallest index for a root of the table
          (define (find-to-n i)
            (local (; get the value for index i in table as a constant
                    (define ti (table-ref table i))
                    ; Number -> Number
                    ; consumes an index i and returns either i or (sub1 i) depending on which is closer.
                    ; if both equally close return (sub1 i) because that's the smallest.
                    ; I'm checking to make sure I haven't skipped over a closer root just on the negative side.
                    (define (i-or-sub1-i-closer-to-root i)
                      (cond
                        [(zero? i) i] ; if it's zero i didn't skip over anything and there's no preceding index to check.
                        [(< (abs (- ti 0)) (abs (- (table-ref table (sub1 i)) 0))) i] ; the value for i is closer than (sub1 i) so that's the root.
                        [else (sub1 i)]))) ; it's either equally close, or it's closer. 
              ;-IN-
              (cond ; add another condition for when the first is closer than the second.
               [(<= 0 ti) (i-or-sub1-i-closer-to-root i)] ; return i because i'm never getting closer because it's increasing, but I might have skipped past the closest one.
               [(>= i (- n 1)) i] ; done because I'm at the end so this is as big as it gets and the closest it gets.
               [else (find-to-n (add1 i))])))) ; try the next one up.
    ;-IN-
    (find-to-n 0)))

; Table -> Number
; consumes a monotonically increasing table and finds the smallest index for a root of the table.
; (where the root is the number in (table-array t) that is closest to 0)
; trivial solution: (= (- left right) 1) and they are right next to each other.
; another trivial solution: always increasing, so if t at 0 is greater than zero, return that.
; simplifying step: pick a midpoint and check both sides to see which is closer to the root
; composition: the solution of the simplified step is the solution.
; termination argument: eventually the interval will be 1 and the function will terminate.

(check-expect (find-binary (make-table 10 (lambda (i) (- (expt 2 i) 16)))) 4) ; monotonic
(check-expect (find-binary (make-table 10 (lambda (i) (- i 4.1)))) 4) ; monotonic but on the edge
(check-expect (find-binary (make-table 10 (lambda (i) (- i 4.9)))) 5) ; monotonic but on the other edge
(check-expect (find-binary (make-table 100 (lambda (i) (- i 99)))) 99) ; root on right side
(check-expect (find-binary (make-table 100 (lambda (i) (- i 1000000)))) 99) ; root off the right side
(check-expect (find-binary table1) 0) ; root on left side
(check-expect (find-binary (make-table 10 (lambda (i) (- (* 10 i) 58)))) 6) ; two are close, one on right is closer

(define (find-binary table)
  (local (; make a constant for the largest index i in table.
          (define endi
            (- (table-length table) 1))
          ; Number -> Number
          ; consumes an index i and gives me the value of table at i  
          (define (t i)
            (table-ref table i))
          ; Number, Number -> Number
          ; recursive function to find the root index of table t in an interval i-left i-right, see explanation above
          (define (find left right)
            (local (; defines the midpoint between two indexes as a constant.
                    (define mid
                      (floor (/ (+ left right) 2)))) ; always rounds down the average.
              ; -IN- 
              (cond
                [(<= (abs (- left right)) 1) (if (<= (abs (t left)) (abs (t right))) left right)] ; if left is closer or tied, then left, because it asks for the smaller of the two in case of tie.
                [(<= (t mid) 0 (t right)) (find mid right)]
                [else (find left mid)]))))
    ; -IN-
    (cond
      [(>= (t 0) 0) 0] ; root is on the left side
      [(>= 0 (t endi)) endi] ; root is on the right side
      [else (find 0 endi)]))) ; root is in the middle, gotta find it.

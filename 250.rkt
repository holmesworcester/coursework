;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |250|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; data definitions

; A Row is a [List-of Number]

; A Matrix is a [List-of Row] where all rows are the same length

; functions

; Number -> Matrix
; diagonal creates an identity matrix with N rows and N columns

(check-expect (diagonal 0) '())
(check-expect (diagonal 1) (list (list 1)))
(check-expect (diagonal 4)
              (list
               (list 1 0 0 0)
               (list 0 1 0 0)
               (list 0 0 1 0)
               (list 0 0 0 1)))
(check-expect  (diagonal 5)
               (list
                (list 1 0 0 0 0)
                (list 0 1 0 0 0)
                (list 0 0 1 0 0)
                (list 0 0 0 1 0)
                (list 0 0 0 0 1)))


(define (diagonal n)
  (local (; Number, Number -> [List-of Number]
          ; makes a list of length l and (- i 1) zeros (where n is <= l) followed by a one, followed by zeros.
          (define (irow i w)
            (cond
              [(zero? w) '()]
              [(= i w) (append (irow i (sub1 w)) (list 1))]
              [else (append (irow i (sub1 w)) (list 0))]))
          ; Number, Number -> Matrix
          (define (diagonal1 i width)
            (cond
              [(zero? i) '()]
              [else (append (diagonal1 (sub1 i) width) (list (irow i width)))])))
    ;-IN-
    (diagonal1 n n)))

          
; Number, Number -> [List-of Number]
; makes a list of length l and (- i 1) zeros (where n is <= l) followed by a one, followed by zeros.

(check-expect (irow 5 5) (list 0 0 0 0 1))
(check-expect (irow 3 5) (list 0 0 1 0 0))
(check-expect (irow 5 0) '())
(check-expect (irow 1 5) (list 1 0 0 0 0))

(define (irow i n)
  (cond
   [(zero? n) '()]
   [(= i n) (append (irow i (sub1 n)) (list 1))]
  [else (append (irow i (sub1 n)) (list 0))]))

; Number -> [List-of Number]
; makes a list of zeros of length n

(check-expect (list-of-zeros 0) '())
(check-expect (list-of-zeros 5) '(0 0 0 0 0))

(define (list-of-zeros n)
  (cond
    [(zero? n) '()]
    [else (cons 0 (list-of-zeros (sub1 n)))]))
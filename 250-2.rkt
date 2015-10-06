;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 250-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; data definitions

; A Row is a [List-of Number]

; A Matrix is a [List-of Row] where all rows are the same length

; functions

; Number -> Matrix
; diagonal creates an identity matrix of size N

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


(define (diagonal size)
  (local
    (; Number -> Matrix
     ; the recursive function that builds the identity matrix based on the number of rows.
     (define (build-diagonal i)
       (cond
         [(zero? i) '()]
         [else (cons (irow i) (build-diagonal (sub1 i)))]))
     ; Number, Number -> [List-of Number]
     ; makes a list of length l and (- i 1) zeros (where n is <= l) followed by a one, followed by zeros.
     (define (irow i)
       (append (list-of-zeros (- size i)) (list 1) (list-of-zeros (sub1 i))))) ; zeros, followed by a 1, followed by more zeros
     ;-IN-
    (build-diagonal size)))

; Number -> [List-of Number]
; makes a list of zeros of length n

(check-expect (list-of-zeros 0) '())
(check-expect (list-of-zeros 5) '(0 0 0 0 0))

(define (list-of-zeros n)
  (cond
    [(zero? n) '()]
    [else (cons 0 (list-of-zeros (sub1 n)))]))
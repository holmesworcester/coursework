;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |277|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(equal? (append (list 1 2 3) (list 4 5 6 7 8))
        (list 1 2 3 4 5 6 7 8))

; Use foldr to define append-from-fold. What happens if you replace foldr with foldl?

; [List-of X], [List-of X] -> [List-of X]
; sticks two lists together by adding the contents of one onto the end of the other.

(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8)) (append (list 1 2 3) (list 4 5 6 7 8)))
(check-expect (append-from-fold '() (list 4 5 6 7 8)) (list 4 5 6 7 8))
(check-expect (append-from-fold (list 4 5 6 7 8) '()) (list 4 5 6 7 8))

(define (append-from-fold l1 l2)
  (foldr (lambda (x alox) (cons x alox)) l2 l1))


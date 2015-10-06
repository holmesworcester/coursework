;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |258|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
  (local (; Number, Number -> [List-of Number]
          ; makes a list of length l and (- i 1) zeros (where n is <= l) followed by a one, followed by zeros.
          (define (row i)
            (append (build-list i makezero) (list 1) (build-list (- size i 1) makezero)))
          ; Number -> 0
          (define (makezero n) 0))
    ; -IN-
    (build-list size row)))
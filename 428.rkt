;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |428|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Number, [List-of Number] -> [List-of [List-of Number]]
; consumes a number n and a list of n2 numbers, and produces a list of n lists of n numbers OR
; it can consume a list of less than n2 numbers, in which case it returns lists of n (or possibly less) until it exhausts the lists of numbers provided.

(check-expect
  (create-matrix 2 (list 1 2 3 4))
  (list (list 1 2)
        (list 3 4)))

(check-expect
  (create-matrix 3 (list 2 4 6 8 10 12 14 16 18))
  (list (list 2 4 6)
        (list 8 10 12)
        (list 14 16 18)))

(define (create-matrix n alon)
  (local (; Number, [List-of X] -> [List-of X]
          ; Returns a list with the first n items of the given list removed
          (define (drop-n n l)
            (cond
              [(empty? l) '()]
              [(zero? n) l]
              [else (drop-n (sub1 n) (rest l))]))
          ; Number, [List-of X] -> [List-of X]
          ; Returns the first n items of the given list
          (define (first-n n l)
            (cond
              [(empty? l) '()]
              [(zero? n) '()]
              [else (cons (first l) (first-n (sub1 n) (rest l)))])))
    ;-IN-
    (cond
      [(empty? alon) '()]
      [else (cons (first-n n alon) (create-matrix n (drop-n n alon)))]))) 
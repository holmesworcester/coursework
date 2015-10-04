;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |249|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; a Lon is [List-of Number]
; interpretation: a list of numbers

(define lontest '( 5 3 2 7 8))
(define lontest-descending '(8 7 5 3 2))
(define lontest-ascending '(2 3 5 7 8))

; Lon -> Lon
; constructs a list from the items in l in descending order

(check-expect (sort> lontest) lontest-descending)

(define (sort> l0)
  (local (; Lon -> Lon
          (define (sort l)
            (cond
              [(empty? l) '()]
              [else (insert (first l) (sort (rest l)))]))
          ; Number Lon -> Lon 
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(> an (first l)) (cons an l)]
                 [else (cons (first l) (insert an (rest l)))])])))
    (sort l0)))

; Lon -> Lon
; constructs a list from the items in l in ascending order

(check-expect (sort-< lontest) lontest-ascending)

(define (sort-< l0)
  (local (; Lon -> Lon
          (define (sort l)
            (cond
              [(empty? l) '()]
              [else (insert (first l) (sort (rest l)))]))
          ; Number Lon -> Lon 
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(< an (first l)) (cons an l)]
                 [else (cons (first l) (insert an (rest l)))])])))
    (sort l0)))


; [List-of X] [X, X -> Boolean] -> [List-of X]
; constructs a list from the items in l sorted by a comparison function that can order two items

(check-expect (sort-a > lontest) lontest-descending)
(check-expect (sort-a < lontest) lontest-ascending)

(define (sort-a cmp l0)
  (local (; Lon -> Lon
          (define (sort l)
            (cond
              [(empty? l) '()]
              [else (insert (first l) (sort (rest l)))]))
          ; Number Lon -> Lon 
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(cmp an (first l)) (cons an l)]
                 [else (cons (first l) (insert an (rest l)))])])))
    (sort l0)))

; [List-of X], [X -> Number], [Number, Number -> Boolean] -> [List-of X]
; sorts a list of items using a function that turns those items into numbers, and a function that compares numbers

(check-expect (sort-lox (list "longlong" "short" "reallylong") string-length >) (list "reallylong" "longlong" "short"))
(check-expect (sort-lox (list "longlong" "short" "reallylong") string-length <) (list "short" "longlong" "reallylong"))

(define (sort-lox alox x->num cmp-num)
  (local (
    ; [X, X] -> Boolean
    (define (cmpxnum a b) (cmp-num (x->num a) (x->num b)))
    ); -IN-
  (sort-a cmpxnum alox)))
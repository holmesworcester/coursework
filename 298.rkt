;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |298|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define-struct no-parent [])
(define MTFT (make-no-parent))
; A FT (family tree) is one of: 
; – MTFT
; – (make-child FT FT String N String)

(define-struct child [father mother name date eyes])

; Oldest Generation:
(define Carl (make-child MTFT MTFT "Carl" 1926 "green"))
(define Bettina (make-child MTFT MTFT "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child MTFT MTFT "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> (List-of String)
; consumes a family tree node and produces a list of all eye colors in the tree.
; An eye color may occur more than once in the resulting list.

(check-expect (eye-colors Fred) (list "pink"))
(check-expect (length (eye-colors Gustav)) 5)

(define (eye-colors a-ftree)
  (cond
    [(no-parent? a-ftree) '()]
    [else (append (list (child-eyes a-ftree)) (eye-colors (child-father a-ftree)) (eye-colors (child-mother a-ftree)))]))

(eye-colors Gustav)

; FT, N -> N
; consumes a family tree node and the current year. It produces the average age of all child structures in the family tree.

(check-expect (average-age Gustav 2000) (/ (+ (- 2000 1926) (- 2000 1926) (- 2000 1965) (- 2000 1966) (- 2000 1988)) 5))
(check-expect (average-age Fred 2000) (- 2000 1966))

(define (average-age a-ftree year-now)
  (local ((define (total-age ft)
  (cond
    [(no-parent? ft) 0]  
    [else (+ (- year-now (child-date ft)) (total-age (child-father ft)) (total-age (child-mother ft)))])))
    (/ (total-age a-ftree) (count-persons a-ftree))))
  
; FT -> N
; consumes a family tree node and counts the child structures in the tree.

(check-expect (count-persons Gustav) 5)

(define (count-persons a-ftree)
  (cond
    [(empty? a-ftree) 0]
    [(no-parent? a-ftree) 0]  
    [else (+ 1 (count-persons (child-father a-ftree)) (count-persons (child-mother a-ftree)))]))
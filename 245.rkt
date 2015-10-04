;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |245|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
;   (my-build-list n f) == (list (f 0) ... (f (- n 1)))

(check-expect (my-build-list 2 add1) (list 1 2))

(define (my-build-list n f)
  (reverse (cond
                [(zero? n) '()]
                [else (cons (f (sub1 n)) (my-build-list (sub1 n) f))])))

; [List-of X] -> [List-of X]
; reverses the order of a list

; (check-expect (my-reverse (list 5 2)) (list 2 5))

(define (my-reverse l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rest l) (first l))]))

; [List-of X], X -> [List-of X]
; adds an item to the end of a list

; (check-expect (add-at-end (list 1 2 3) 8) (list 1 2 3 8))
; (check-expect (add-at-end '() 8) 8)

(define (add-at-end l x)
  (cond
    [(empty? l) x]
    [else (my-reverse (cons (first l) (my-reverse (rest l))))])) 


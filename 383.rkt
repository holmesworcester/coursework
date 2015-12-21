;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |383|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] -> X 
; returns a random item from the list 
; assume the list is not empty

(check-expect (member? (random-pick '(1 2 3 4)) '(1 2 3 4)) #t) 

(define (random-pick l)
  (local (; [List-of X], Number -> X
          ; returns the nth member of the list where 0 returns the first.
          (define (get-nth l n)
            (cond
              [(zero? n) (first l)]
              [else (get-nth (rest l) (sub1 n))])))
    ;-IN-
    (get-nth l (random (length l)))))
 
; [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
; produces the list of those lists in ll that do not agree 
; with names at any place 

(check-expect (non-same (explode "abcd") (list (explode "abcd") (explode "aasdfasd") (explode "ad") (explode "acbd"))) (list (explode "aasdfasd") (explode "ad") (explode "acbd")))

(define (non-same names ll)
  (cond
    [(empty? ll) '()]
    [else (if (equal? names (first ll)) (non-same names (rest ll)) (cons (first ll) (non-same names (rest ll))))]))
  
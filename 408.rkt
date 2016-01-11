;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |408|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; [List-of 1String] N -> [List-of String]
; bundles sub-sequences of s into strings of length n
; this is a checked function for the input n.
; i'm not bothering to make it a check function for l

(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle (explode "abcdefg") 3) (list "abc" "def" "g"))
(check-expect (bundle '() 3) '())
(check-error (bundle (explode "abcdefg") 0) "chunk length must be a number greater than zero")
(check-error (bundle (explode "abcdefg") "afasdf") "chunk length must be a number greater than zero")

(define (bundle l n)
  (cond
    [(empty? l) '()]
    [(or (not (number? n)) (<= n 0)) (error "chunk length must be a number greater than zero")]
    [else (append (list (implode (take l n)))(bundle (drop l n) n))]))

; [List-of X] N -> [List-of X]
; retrieves the first n items in l if possible or everything 
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; remove the first n items from l if possible or everything 
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
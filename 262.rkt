;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |262|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

; Exercise 190. Design prefixes. The function consumes a list of 1Strings and produces the list of all prefixes.
; Recall that a list p is a prefix of l if p and l are the same up through all items in p.
; For example, (list 1 2 3) is a prefix of itself and (list 1 2 3 4).

; Design the function suffixes, which consumes a list of 1Strings and produces all suffixes.
; A list s is a suffix of l if p and l are the same from the end, up through all items in s. For example, (list 2 3 4) is a suffix of itself and (list 1 2 3 4). image

; [List-of X] -> [List-of [List-of X]]
; consumes a list of 1Strings and produces the list of all prefixes.
; For example, (list 1 2 3) is a prefix of itself and (list 1 2 3 4).

(check-expect (prefixes '()) '())
(check-expect (prefixes (list 1)) (list (list 1)))
(check-expect (prefixes (list 1 2 3)) (list (list 1) (list 1 2) (list 1 2 3)))

(define (prefixes l)
  (local (; Number -> [List-of X]
          ; gives me the first n items in a list.
          (define (list-up-to-n-in-l n)
            (build-list (+ n 1) nth-in-l)) ; added a plus one here. not sure why but :)
          ; Number -> X
          ; gives me the nth item of the list l (the one we are working with)
          (define (nth-in-l n)
            (nth n l)))
  (build-list (length l) list-up-to-n-in-l)))


; Number, [List-of X] -> X
; gives me the nth item of a list, count starting from 0.

(check-expect (nth 1 (list 1 2 3)) 2)
 
(define (nth n l)
  (cond
    [(empty? l) (error "there aren't that many items in the list")]
    [(zero? n) (first l)]
    [else (nth (sub1 n) (rest l))]))
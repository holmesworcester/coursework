;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |376|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X], [List-of Y] -> [List-of [X and Y]]
; adds a list l2 onto the end of a list l1

(check-expect (replace-eol-with '() '(a b c)) '(a b c))
(check-expect (replace-eol-with '(a b c) '()) '(a b c))
(check-expect (replace-eol-with '(a b c) '(d e f)) '(a b c d e f))

(define (replace-eol-with l1 l2)
  (cond
    [(and (empty? l1)(empty? l2)) '()]
    [(and (empty? l1)(cons? l2)) l2]
    [(and (cons? l1)(empty? l2)) l1]
    [(and (cons? l1)(cons? l2)) (cons (first l1) (replace-eol-with (rest l1) l2))]))

; [List-of X], [List-of Y] -> [List-of [X and Y]]
; adds a list l2 onto the end of a list l1

(check-expect (replace-eol-with-simplified '() '(a b c)) '(a b c))
(check-expect (replace-eol-with-simplified '(a b c) '()) '(a b c))
(check-expect (replace-eol-with-simplified '(a b c) '(d e f)) '(a b c d e f))

(define (replace-eol-with-simplified l1 l2)
  (cond
;    [(and (empty? l1)(empty? l2)) l2] <-- this just returns l2 either way so i can combine with other case
    [(empty? l1) l2] ; does the same thing whether l2 is empty? or cons? so i can remove the second condition
;    [(and (cons? l1)(empty? l2)) l1] <-- i don't need this because if l2 is empty this happens naturally.
    [else (cons (first l1) (replace-eol-with-simplified (rest l1) l2))])) ; now I know l1 is cons? because I've already got a conditional above for the case where it's empty. Also I deliberately don't want to check if l2 is empty? or cons? because it doesn't matter and I want to combine those cases. 

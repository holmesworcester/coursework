;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |372|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; [List-of Number] [List-of Number] -> [List-of Number]
; constructs a new list by replacing '() in front with end
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else (cons (first front)
                (replace-eol-with (rest front) end))]))

; [List-of Symbol] [List-of Number] -> [List-of [List Symbol Number]]
; Consumes a list of symbols and a list of numbers and produces all possible ordered pairs of symbols and numbers.
; That is, when given '(a b c) and '(1 2), the expected result is '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)).
; I know there's a for abstraction that does this really well, but I'll leave it for now. 

(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else (replace-eol-with (pair-symbol-to-list (first los) lon) (cross (rest los) lon))]))

; Symbol [List-of Number] -> [List-of [List Symbol Number]]
; creates a new list of pairs by pairing a symbol with every number in a list of numbers.

(check-expect (pair-symbol-to-list 'a '(1 2)) '((a 1) (a 2)))

(define (pair-symbol-to-list s l)
  (map (lambda (x) (list s x)) l))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |220|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; definitions

; A Numbered-List-of-Strings is one of:
; - '()
; - (cons (list Number "String") Numbered-List-of-Strings
; interpretation: a list of items (strings) numbered in a ranked order of importance or value.

; List-of-numbers-or-strings -> ... nested list ...
; creates a row for an HTML table from a list of numbers or strings
 
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l)) (make-row (rest l)))]))
 
; Number-or-String -> ... nested list ...
; creates a cell for an HTML table from a number or a string 
(define (make-cell n)
  `(td ,(if (number? n) (number->string n) n))) ; added a check to deal with cells that are strings

; List-of-Strings -> List-of-Strings
; reverses the order of a list

(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-Strings -> Numbered-List-of-Strings
; creates a numbered list where each item is given the number of items left in the list, including itself
; (numbered in reverse order, where the numbers are returned as strings for easy rendering)

(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los)) 
                (add-ranks (rest los)))]))

; List-of-Strings -> ... nested list ... 
; renders a list of strings as a ranked list in a racket-style list that mirrors an HTML table

(check-expect (make-ranking '()) (html-table-template '()))
(check-expect (make-ranking (list "thing")) '(html
   (body
     (table ((border "1"))
       (tr (td "1") (td "thing"))))))
              
(define (make-ranking los)
  (html-table-template (make-all-rows (add-ranks los))))

; ... nested list ... -> ... nested list ...
; adds html boilerplate and table boilerplate to a list of rows.

(check-expect (html-table-template '((tr (td "1") (td "thing"))))
              '(html
                (body
                 (table ((border "1"))
                        (tr (td "1") (td "thing"))))))
              

(define (html-table-template rows)
  `(html
   (body
     (table ((border "1")) ,@rows)))) ; check whether it needs to be items or a list

; Numbered-List-of-Strings -> ... nested list ...
; makes a list of rows

(check-expect (make-all-rows '()) '())
(check-expect (make-all-rows '(("1" "thing"))) '((tr (td "1") (td "thing")))) ; check this.
(check-expect (make-all-rows '(("1" "thing")("2" "second thing"))) '((tr (td "1") (td "thing")) (tr (td "2") (td "second thing"))))

(define (make-all-rows ranking)
  (cond
    [(empty? ranking) '()]
    [else (cons `(tr ,@(make-row (first ranking))) (make-all-rows (rest ranking)))]))

; example:

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

(make-ranking one-list) ; the result of this looks right. but i can't render it using the suggested command; not sure why.


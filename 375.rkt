;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |375|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct branch [left right])
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path. 

; TOS, [List-of Direction] -> TOS 
; The function consumes a tree of symbols and a list of directions.
; a Direction tells the function whether to choose the left or the right branch in a non-symbolic tree.
; The function should return the first item at the end of the path.
; The function signals an error when it is given a symbol and a non-empty path.

(check-expect (tree-pick (make-branch (make-branch 'a 'b) 'c) '(left left)) 'a)
(check-expect (tree-pick (make-branch (make-branch 'a 'b) 'c) '(left right)) 'b)
(check-expect (tree-pick (make-branch (make-branch 'a 'b) 'c) '(right)) 'c)
(check-expect (tree-pick (make-branch (make-branch 'a 'b) 'c) '(left)) (make-branch 'a 'b))
(check-error (tree-pick (make-branch (make-branch 'a 'b) 'c) '(right left)) "tree too short for path")

(define (tree-pick tree path)
  (cond
    [(and (symbol? tree) (empty? path)) tree] ; return the symbol
    [(and (branch? tree) (empty? path)) tree] ; return the tree from where I'm at in it. (I could also throw an error now)
    [(and (symbol? tree) (cons? path)) (error "tree too short for path")]
    [(and (branch? tree) (cons? path)) (if (symbol=? 'left (first path)) (tree-pick (branch-left tree) (rest path)) (tree-pick (branch-right tree) (rest path)))]))
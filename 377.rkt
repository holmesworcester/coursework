;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |377|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
    [(empty? path) tree] ; if path is empty nothing happens, I'm left with the same old tree either way.
    [(symbol? tree) (error "tree too short for path")] ; and now I know path isn't empty. 
    [(branch? tree) (if (symbol=? 'left (first path)) (tree-pick (branch-left tree) (rest path)) (tree-pick (branch-right tree) (rest path)))]))
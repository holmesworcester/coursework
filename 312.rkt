;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |312|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Compare searching in binary search trees with searching in sorted lists from exercise 189.
; Searching a sorted list lets you stop looking once you "pass" the number's place in the list.
; But searching a binary search tree lets you start in the "middle" each time, minimizing the number of checks.



(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BinaryTree (short: BT) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define tree1 (make-node
  023999999
  'holmes
  NONE
  (make-node 024999999 'guillaume NONE NONE)))
     
(define tree2 (make-node
  023999998
  'giseli
  (make-node 023999999 'holmes NONE NONE)
  NONE))

(define tree3 (make-node
  023999998
  'giseli
  (make-node 023999999 'holmes tree2 tree1)
  tree1))

; BT -> [List-of N]
; takes a BT and gives me a list of numbers in the tree "from left to right"
; that is, start with the tail on the left side, end at the tail on the right side, and work your way across
; by going up and down the tree. Or just think of the tree as a list that gets unfolded internally.

(check-expect (inorder tree2) '(023999999 023999998))
(check-expect (inorder tree3) (list
 23999999
 23999998
 23999999
 23999999
 24999999
 23999998
 23999999
 24999999))

(define (inorder bt)
  (cond
    [(equal? NONE bt) '()]
    [else (append (inorder (node-left bt)) (list (node-ssn bt)) (inorder (node-right bt)))]))

; BST, N -> [Maybe [Symbol]]
; If the tree contains a node structure whose ssn field is n, the function produces the value of the name field
; in that node. Otherwise, the function produces NONE. We take advantage of how a BST is sorted to minimize
; the number of comparisons.

(check-expect (search-bst tree1 023999999) 'holmes)
(check-expect (search-bst tree1 024999999) 'guillaume)
(check-expect (search-bst tree2 99999999) NONE)
(check-expect (search-bst tree3 024999999) 'guillaume)

(define (search-bst bst n)
  (local ((define (search-n-bst bst) (search-bst bst n)))
  (cond
    [(equal? NONE bst) NONE]
    [(= n (node-ssn bst)) (node-name bst)]
    [else (if (< n (node-ssn bst)) (search-n-bst (node-left bst)) (search-n-bst (node-right bst)))])))

; BT, N -> [Maybe [Symbol]]
; If the tree contains a node structure whose ssn field is n, the function produces the value of the name field
; in that node. Otherwise, the function produces #false.

(check-expect (search-bt tree1 023999999) 'holmes)
(check-expect (search-bt tree1 024999999) 'guillaume)
(check-expect (search-bt tree2 99999999) #f)

(define (search-bt bt n)
  (local (;[Maybe[Symbol]], [Maybe[Symbol]] -> [Maybe[Symbol]]
          ; returns a symbol if one is a symbol. if both are, returns the first one.
          (define (return-sym ms1 ms2)
            (if (symbol? ms1) ms1 (if (symbol? ms2) ms2 #f)))) ; test this if i have a problem.
  (cond
    [(equal? NONE bt) #f]
    [else (if (= (node-ssn bt) n) (node-name bt) (return-sym (search-bt (node-left bt) n) (search-bt (node-right bt) n)))])))

; Hint Use either contains-bt? to produce #false if called for or boolean?
; to find out whether search-bt is successfully used on a subtree. image
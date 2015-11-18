;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |313|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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


; [List-of [List Number Symbol]] -> BST
; consumes a list of numbers and names and produces a binary search tree by repeatedly applying create-bst.

; (check-expect (create-bst-from-list '(())) NONE) ; let's assume it's a non-empty list for now, since folder won't give it empty.
(check-expect (create-bst-from-list '((25 a)(2 b))) (create-bst (create-bst NONE 25 'a) 2 'b))

(define (create-bst-from-list l)
  (foldl create-from-one-list NONE l))
;  (foldl (lambda (one-list bst) (create-bst bst (first one-list) (second (one-list)))) NONE l)) ; WHY DOESN'T THIS WORK?? 
; BST, [List Number Symbol] -> BST
; consumes a BST and a list of a number and a symbol and makes a new BST. Just like create-bst but it takes a list of a number and a symbol.

(check-expect (create-from-one-list '(2 g) NONE) (create-bst NONE 2 'g))

(define (create-from-one-list ol bst)
  (create-bst bst (first ol) (second ol)))

; BST, N, Symbol -> BST
; consumes a BST B, a number N, and a symbol S. It produces a BST that is just like B
; and that in place of one NONE subtree contains the node structure (make-node N S NONE NONE)

(check-expect (create-bst (make-node 5 'a NONE NONE) 2 'g) (make-node 5 'a (make-node 2 'g NONE NONE) NONE))
(check-expect (create-bst NONE 2 'g) (make-node 2 'g NONE NONE)) ; it ideally would do this too.
(check-expect (create-bst (make-node 5 'a NONE NONE) 10 'g) (make-node 5 'a NONE (make-node 10 'g NONE NONE)))


(define (create-bst bst n s)
  (cond
    [(equal? NONE bst) (make-node n s NONE NONE)] ; base case
    [else (cond
            [(< n (node-ssn bst)) (make-node (node-ssn bst) (node-name bst) (create-bst (node-left bst) n s) (node-right bst))] ; send it left
            [(> n (node-ssn bst)) (make-node (node-ssn bst) (node-name bst) (node-left bst) (create-bst (node-right bst) n s))])])) ; send it right


(define tree-a (create-bst (create-bst (create-bst (create-bst (create-bst (create-bst (create-bst (create-bst (make-node 63 'a NONE NONE) 10 'a) 15 'b) 24 'c) 29 'd) 77 'f) 89 'g) 95 'h) 99 'i))

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
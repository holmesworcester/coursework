;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |445|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A Node is a Symbol

; A Path is [List-of Node]
; interpretation The list of nodes specifies a sequence of
; immediate neighbors that leads from the first Node on the 
; list to the last one.

; A Graph is a [List-of [List-of Node]]
; interpretation: for each list, the latter nodes in the list are the only neighbors of the first node. 

(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

; Node, Graph -> [List-of Node]
; consumes a Node n and a Graph g and produces the list of immediate neighbors of n in g.

(check-expect (neighbors 'A sample-graph) '(B E))
(check-expect (neighbors 'C sample-graph) '(D))
(check-expect (neighbors 'G sample-graph) '())
(check-expect (neighbors 'D sample-graph) '())


(define (neighbors node G)
  (cond
    [(empty? G) '()]
    [(symbol=? node (first (first G))) (rest (first G))]
    [else (neighbors node (rest G))]))
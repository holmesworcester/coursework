;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |447|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
 (require racket/match)

; A Node is a Symbol

; A Path is [List-of Node]
; interpretation The list of nodes specifies a sequence of
; immediate neighbors that leads from the first Node on the 
; list to the last one.

; A Graph is a [List-of Path]
; interpretation: an expression of all the connections between a set of nodes,
; represented as paths from each initial node, each of which is represented once as the first item in each list. 

(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

(define cyclic-graph
  '((A B E)
    (B E F)
    (C B D)
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

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

(check-expect (find-path 'C 'D sample-graph) '(C D))
(check-member-of (find-path 'E 'D sample-graph) '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph) #false)

(define (find-path origination destination G)
  (local (; [List-of Node] Node Graph -> [Maybe Path]
          ; finds a path from some node on lo-Os to D
          ; if there is no path, the function produces #false
          (define (find-path/list lo-Os)
            (cond
              [(empty? lo-Os) #false]
              [else (local ((define candidate (find-path (first lo-Os) destination G)))
                      (cond
                        [(boolean? candidate) (find-path/list (rest lo-Os))]
                        [else candidate]))]))
          ; Node, Graph -> [List-of Node]
          ; consumes a Node n and a Graph g and produces the list of immediate neighbors of n in g.
          (define (neighbors node G)
            (cond
              [(empty? G) '()]
              [(symbol=? node (first (first G))) (rest (first G))]
              [else (neighbors node (rest G))])))
    ;-IN-
    (cond
      [(symbol=? origination destination) (list destination)]
      [else (local ((define next (neighbors origination G))
                    (define candidate (find-path/list next)))
              (cond
                [(boolean? candidate) #false]
                [else (cons origination candidate)]))])))
 
; Graph -> Boolean
; consumes a graph g and tries to find a path find-path between all pairs of nodes in g.
; If it succeeds, it produces #true.

(check-expect (test-on-all-nodes sample-graph) #f)
(check-expect (test-on-all-nodes '((A B)
                                   (B A))) #t)
; (check-expect (test-on-all-nodes cyclic-graph) #f)

(define (test-on-all-nodes G)
  (local (; Graph -> [List-of Node]
          ; Returns a list of all nodes in a graph
          (define (list-nodes G)
            (map first G))
          ;define this list as a constant so I can use it.
          (define list-of-nodes (list-nodes G))
          ; Node -> Boolean
          ; Checks to see if a node in a graph has a path to every other node. Returns true if so, false if not.
          (define (test-on-1-node node)
            (andmap (lambda (destination) (not (false? (find-path node destination G)))) list-of-nodes)))
    ;-IN-
    (andmap test-on-1-node list-of-nodes)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |301|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define-struct no-parent [])
(define MTFT (make-no-parent))
; A FT (family tree) is one of: 
; – MTFT
; – (make-child FT FT String N String)

(define-struct child [father mother name date eyes])

; Oldest Generation:
(define Carl (make-child MTFT MTFT "Carl" 1926 "green"))
(define Bettina (make-child MTFT MTFT "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child MTFT MTFT "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; A FF (family forest) is one of: 
; – '()
; – (cons FT FF)

; or... [List-of FT]

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))
(define ff4 (list Gustav))

; [List-of FT], N -> N
; returns the average age for all members of a FF, assuming we don't have to worry about overlap.

(check-expect (average-age-forest ff1 1926) 0)

(define (average-age-forest ff year)
  (- year (average (list-from-forest child-date ff))))

; TODO: Try reimplementing `average-age` by using your `list-from-forest` function, and a local helper function you will write called `child-age`

; [List-of FT], N -> N
; returns the average age for all members of a FF, assuming we don't have to worry about overlap.

(check-expect (average-age-forest.v2 ff1 1926) 0)

(define (average-age-forest.v2 ff year)
  (local (; Number, Structure -> Number
          ; consumes a structure (make-child father mother name date eyes) and returns the approximate age of the child given the current year.
          (define (child-age c)
            (- year (child-date c))))
    ; -IN-         
  (average (list-from-forest child-age ff)))) 

; [List-of N] -> N
; gives me the average of a list.

(check-expect (average '(1 1 3 3)) 2)
(check-expect (average '(1 1 1 1 1 1)) 1)

(define (average l)
  (/ (foldr + 0 l) (length l)))

; [List-of FT], [FT -> X] -> [List-of X]
; pulls all items of a certain selector from a family forest and puts them in a list,
; given a selector function that works on family trees.

(check-expect (list-from-forest child-eyes ff4) (list "brown" "blue" "green" "green" "pink"))
(check-expect (list-from-forest child-date ff1) (list 1926 1926))

(define (list-from-forest selector ff)
  (local ((define (list-from-ft ft)
            (cond
              [(no-parent? ft) '()]
              [else (append (list (selector ft)) (list-from-ft (child-mother ft)) (list-from-ft (child-father ft)))])))
  (cond
    [(empty? ff) '()]
    [else (append (list-from-ft (first ff)) (list-from-forest selector (rest ff)))])))


; FT -> Boolean
; returns true if one of the parents of a given node, or their parents and so on, have blue eyes.
; returns false otherwise, and returns false if that given node has blue eyes.

(check-expect (blue-eyed-ancestor? Carl) #f)
(check-expect (blue-eyed-ancestor? Gustav) #t)
(check-expect (blue-eyed-ancestor? Eva) #f)
               
(define (blue-eyed-ancestor? ft)
    (cond
      [(no-parent? ft) #f]
      [else (or (has-blue-eyes? (child-mother ft)) (blue-eyed-ancestor? (child-mother ft)) (has-blue-eyes? (child-father ft)) (blue-eyed-ancestor? (child-father ft)))])) ; either parent has blue eyes, or a blue eyed ancesotor, return true.

; FT -> Boolean
; returns true if the FT has blue eyes. Otherwise returns false.

(check-expect (has-blue-eyes? Eva) #t)
(check-expect (has-blue-eyes? Gustav) #f)

(define (has-blue-eyes? ft)
  (cond
    [(no-parent? ft) #false]
    [else (string=? "blue" (child-eyes ft))]))

; FT -> (List-of String)
; consumes a family tree node and produces a list of all eye colors in the tree.
; An eye color may occur more than once in the resulting list.

(check-expect (eye-colors Fred) (list "pink"))
(check-expect (length (eye-colors Gustav)) 5)

(define (eye-colors a-ftree)
  (cond
    [(no-parent? a-ftree) '()]
    [else (append (list (child-eyes a-ftree)) (eye-colors (child-father a-ftree)) (eye-colors (child-mother a-ftree)))]))

; FT, N -> N
; consumes a family tree node and the current year. It produces the average age of all child structures in the family tree.

(check-expect (average-age Gustav 2000) (/ (+ (- 2000 1926) (- 2000 1926) (- 2000 1965) (- 2000 1966) (- 2000 1988)) 5))
(check-expect (average-age Fred 2000) (- 2000 1966))

(define (average-age a-ftree year-now)
  (local ((define (total-age ft)
  (cond
    [(no-parent? ft) 0]  
    [else (+ (- year-now (child-date ft)) (total-age (child-father ft)) (total-age (child-mother ft)))])))
    (/ (total-age a-ftree) (count-persons a-ftree))))
  
; FT -> N
; consumes a family tree node and counts the child structures in the tree.

(check-expect (count-persons Gustav) 5)

(define (count-persons a-ftree)
  (cond
    [(empty? a-ftree) 0]
    [(no-parent? a-ftree) 0]  
    [else (+ 1 (count-persons (child-father a-ftree)) (count-persons (child-mother a-ftree)))]))
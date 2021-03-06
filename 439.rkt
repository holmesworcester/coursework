;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |439|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; An SOE is a non-empty Matrix
; constraint if its length is n (in N), each item has length (+ n 1)
; interpretation an SOE represents a system of linear equations
 
; An Equation is [List-of Number]
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, a1, ..., an are
; the left-hand side variable coefficients and b is the right-hand side

; A TM is [List-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

 
; A Solution is [List-of Number]
 
; examples: 
(define M
  (list (list 2 2  3 10)
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2))

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

; SOE Solution -> Boolean
; checks the solution of a matrix problem given an SOE and a Solution.

(check-expect (check-solution M S) #t)
(check-expect (check-solution M '(0 0 0)) #f)

(define (check-solution soe solution)
  (local (; Equation -> Boolean
          ; consumes an Equation and checks that a solution (taken as constant) satisfies an equation.
          (define (check-line eq)
            (= (plug-in (lhs eq) solution) (rhs eq))))
    ;-IN-
  (andmap check-line soe)))

; [List-of Number] Solution -> Number
; consumes the left-hand side of an Equation and a Solution and calculates out the value of the left-hand side
; when the numbers from the solution are plugged in for the variables.

(check-expect (plug-in (lhs (first M)) S) (rhs (first M)))
(check-expect (plug-in '(0 0 0) '(1 1 1)) 0)

(define (plug-in left-hand solution)
  (cond
    [(empty? left-hand) 0]
    [else (+ (* (first left-hand) (first solution)) (plug-in (rest left-hand) (rest solution)))]))

; Equation Equation -> Equation
; consumes two Equations of equal length. It subtracts the second from the first, item by item,
; as many times as necessary to obtain an Equation with a 0 in the first position. Since the leading
; coefficient is known to be 0, subtract returns the rest of the list that results from the subtractions.

(check-expect (subtract '(3  9   21) '(-3 -8  -19)) '(1 2))
(check-expect (subtract '(1 1 5) '(3 1 1)) '(-2 -14))

(define (subtract eq1 eq2)
  (local (; define the factor I'm multiplying by to cancel out the first item when added.
          (define multiply-by (* -1 (/ (first eq2) (first eq1))))
          ; Equation Equation -> Equation
          ; add two equations together by adding their respective items.
          (define (add-eq eq1 eq2)
            (cond
              [(empty? eq1) '()]
              [else (cons (+ (first eq1) (first eq2)) (add-eq (rest eq1) (rest eq2)))])))
    ;-IN-
    (add-eq (map (lambda (x) (* x multiply-by)) (rest eq1)) (rest eq2)))) ; adds the "rest" of eq's together because I know the first one is canceled out and should't be returned 

; SOE -> TM
; triangulates the given system of equations

(check-expect (triangulate
               (list (list 2 2  3 10)
                     (list 2 5 12 31)
                     (list 4 1 -2 1)))
              (list (list 2 2  3 10)
                    (list   3  9 21)
                    (list      1  2)))

(check-expect (triangulate
               (list (list  3  9   21)
                     (list -3 -8  -19)))
              (list (list 3  9 21)
                    (list    1  2)))

(define (triangulate soe)
  (cond
    [(empty? soe) '()]
    [else (cons (first soe) (triangulate (map (lambda (x) (subtract (first soe) x)) (rest soe))))]))
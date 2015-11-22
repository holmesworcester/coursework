;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |341|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fun Symbol BSL-fun-expr)

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)


(define WRONG "wrong kind of S-expression")
 
(define-struct add [left right])
(define-struct mul [left right])

; Ex. 332 "N"

; a BSL-expr is one of:
; - Number
; - BSL-bool 
; - a Structure (make-add [BSL-expr BSL-expr])
; - a Structure (make-mul [BSL-expr BSL-expr])

; a BSL-bool is one of:
; - [List-of Boolean]
; - a Structure (make-or [BSL-bool])
; - a Structure (make-and [BSL-bool])

(define-struct ory [list])
(define-struct andy [list])

(define andtrue (make-andy (list #t #t #t #t)))
(define andfalse (make-andy (list #t #f #t #t)))
(define ortrue (make-ory (list #f #f #f #t)))
(define orfalse (make-ory (list #f #f #f #f)))
(define nested-true (make-ory (list #f andtrue #f orfalse)))

;examples:

;(+ 10 -10)
(define 10-10 (make-add 10 -10))

; (+ (* 20 3) 33)
(define 20x3+33 (make-add (make-mul 20 3) 33))

; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))
(define pi-math (make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9))))

; An AL (association list) is [List-of Association].
; An Association is (cons Symbol (cons Number '())).

(define x2y4 (list (list 'x 2) (list 'y 4)))
(define x1 (list (list 'x 1)))
 
; BSL-expr -> N
; evaluates a BSL expression

(check-expect (eval-expression 10-10) 0)
(check-expect (eval-expression 20x3+33) 93)
(check-expect (eval-expression 5) 5)
(check-expect (eval-expression #t) #t)
(check-expect (eval-expression nested-true) #t)

(define (eval-expression bexp)
  (cond
    [(number? bexp) bexp]
    [(boolean? bexp) bexp]
    [(add? bexp) (+ (eval-expression (add-left bexp)) (eval-expression (add-right bexp)))]
    [(mul? bexp) (* (eval-expression (mul-left bexp)) (eval-expression (mul-right bexp)))]
    [(ory? bexp) (ormap (lambda (x) (eval-expression x)) (ory-list bexp))]
    [(andy? bexp) (andmap (lambda (x) (eval-expression x)) (andy-list bexp))]
    [else (error "not an expression")]))

; BSL-var-expr, Symbol, Number -> BSL-var-expr
; substitutes a variable defined by symbol s with a number x, in a BSL-var-expr b.

(check-expect (subst 2 'x 9) 2)
(check-expect (subst 'x 'x 9) 9)
(check-expect (subst (make-add 'x 1) 'x 9) (make-add 9 1))
(check-expect (subst (make-add (make-mul 'x 2) 3) 'x 3) (make-add (make-mul 3 2) 3))
(check-expect (eval-variable (subst (subst (make-add (make-mul 'x 'y) 3) 'x 2) 'y 4)) 11)

(define (subst b s n)
  (cond
    [(number? b) b]
    [(symbol? b) (if (equal? b s) n b)]
    [(add? b) (make-add (subst (add-left b) s n) (subst (add-right b) s n))]
    [(mul? b) (make-mul (subst (mul-left b) s n) (subst (mul-right b) s n))]
    [else (error "wrong kind of S-expression")]))

; BSL-var-expr -> Boolean
; determines whether a BSL-var-expr is also a BSL-expr, that is, whether it has no variables in it.

(check-expect (numeric? (make-add (make-mul 'x 2) 3)) #f)
(check-expect (numeric? 'x) #f)
(check-expect (numeric? (make-add (make-mul 2 3) 4)) #t)

(define (numeric? b)
  (cond
    [(number? b) #t]
    [(symbol? b) #f]
    [(add? b) (and (numeric? (add-left b)) (numeric? (add-right b)))]
    [(mul? b) (and (numeric? (mul-left b)) (numeric? (mul-right b)))]
    [else (error "wrong kind of S-expression")]))
  
; BSL-var-expr -> N
; consumes a BSL-var-expr and determines its value if numeric? is true.
; Otherwise it signals an error, saying that it is impossible to evaluate an expression that contains a variable.

(check-error (eval-variable 'x) "it is impossible to evaluate an expression that contains a variable")
(check-expect (eval-variable (make-add (make-mul 1 2) 1)) 3) 
 
(define (eval-variable b)
  (if (numeric? b) (eval-expression b) (error "it is impossible to evaluate an expression that contains a variable")))

; BSL-var-expr, AL -> N
; consumes a BSL-var-expr ex and an association list da. Starting from ex, it iteratively applies subst to all associations in da.
; If numeric? holds for the result, it determines its value; otherwise it signals the same error as eval-variable.

(check-expect (eval-variable* 'x x1) 1)
(check-expect (eval-variable* (make-add (make-mul 'x 2) 3) x1) 5)
(check-expect (eval-variable* (make-add (make-mul 'x 2) 3) x2y4) 7)
(check-expect (eval-variable* (make-add (make-mul 'x 'y) 3) x2y4) 11)

(define (eval-variable* b al)
  (cond
    [(empty? al) (eval-variable b)]
    [else (eval-variable* (subst b (first (first al)) (second (first al))) (rest al))]))

; AL, Symbol -> N
; consumes an AL da and a Symbol x. It produces the value of x in da—if there is a matching Association; otherwise it signals an error.

(check-expect (lookup-con x1 'x) 1)
(check-expect (lookup-con x2y4 'x) 2)
; (check-error (lookup-con x1 'y) (error "y is not defined"))

(define (lookup-con da x)
  (cond
    [(empty? da) (error (string-append (symbol->string x) " is not defined"))]
    [else (if (equal? x (first (first da))) (second (first da)) (lookup-con (rest da) x))]))

; BSL-var-expr, AL -> N
; consumes a BSL-var-expr ex and an association list da. Starting from ex, it iteratively applies subst to all associations in da.
; If numeric? holds for the result, it determines its value; otherwise it signals the same error as eval-variable.
; It does not use substitution, however. Instead, the function traverses the expression in the manner that the design recipe for BSL-var-expr
; suggests and “carries along” da. When it encounters a symbol x, the function looks up the value of x in da.

(check-expect (eval-var-lookup 'x x1) 1)
(check-expect (eval-var-lookup (make-add (make-mul 'x 2) 3) x2y4) 7)
(check-expect (eval-var-lookup (make-add (make-mul 'x 'y) 3) x2y4) 11)
(check-expect (eval-var-lookup (make-add (make-mul 'x 2) 3) x1) 5)

(define (eval-var-lookup b al)
  (local (; BSL-var-expr -> N
          ; passes through AL
          (define (eval-look b) (eval-var-lookup b al))) 
  (cond
    [(symbol? b) (lookup-con al b)]
    [(number? b) b]
    [(add? b) (+ (eval-look (add-left b)) (eval-look (add-right b)))]
    [(mul? b) (* (eval-look (mul-left b)) (eval-look (mul-right b)))]
    [else (error "not an expression")])))

; X -> Boolean
; predicate that tells me if something is an atom

(define (atom? x)
  (or (number? x) (symbol? x) (string? x)))

(check-expect (parse 'a) 'a)
(check-error (parse "dude") "strings not allowed")
(check-expect (parse 9) 9)
(check-error (parse '(+ 1)) WRONG)
(check-error (parse '(+ 1 2 3)) WRONG)
(check-expect (parse '(+ 1 2)) (make-add 1 2))
(check-expect (parse '(* 1 2)) (make-mul 1 2))
(check-error (parse '(and #t #f)) WRONG)
(check-expect (parse '(+ 1 x)) (make-add 1 'x))
(check-expect (parse '(+ 1 (* x y))) (make-add 1 (make-mul 'x 'y)))


; S-expr -> BSL-var-expr
; creates representation of a BSL expression for s (if possible)
(define (parse s)
  (local (; S-expr -> BSL-expr
          (define (parse s)
            (cond
              [(atom? s) (parse-atom s)]
              [else (parse-sl s)]))
 
          ; SL -> BSL-expr 
          (define (parse-sl s)
            (local ((define L (length s)))
              (cond
                [(< L 3)
                 (error WRONG)]
                [(and (= L 3) (symbol? (first s)))
                 (cond
                   [(symbol=? (first s) '+)
                    (make-add (parse (second s)) (parse (third s)))]
                   [(symbol=? (first s) '*)
                    (make-mul (parse (second s)) (parse (third s)))]
                   [else (error WRONG)])]
                [else
                 (error WRONG)])))
 
          ; Atom -> BSL-expr 
          (define (parse-atom s)
            (cond
              [(number? s) s]
              [(string? s) (error "strings not allowed")]
              [(symbol? s) s])))
    (parse s)))
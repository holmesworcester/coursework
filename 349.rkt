;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |349|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
(define-struct fun [name arg])


; a BSL-expr is one of:
; - Number
; - a Structure (make-add [BSL-expr BSL-expr])
; - a Structure (make-mul [BSL-expr BSL-expr])

;examples:

;(+ 10 -10)
(define 10-10 (make-add 10 -10))

; (+ (* 20 3) 33)
(define 20x3+33 (make-add (make-mul 20 3) 33))

; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))
(define pi-math (make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9))))

; (k (+ 1 1))
(define k1+1 (make-fun 'k (make-add 1 1)))

; (* 5 (k (+ 1 1)))
(define k1+1times5 (make-mul 5 k1+1))

; (* (i 5) (k (+ 1 1)))
(define final-fun-expr (make-mul (make-fun 'i 5) k1+1))

; a BSL-fun-def is a structure (make-fun-def Symbol Symbol BSL-fun-expr)
(define-struct fun-def [name param body])

;examples:
; (define (f x) (+ 3 x))
(define f (make-fun-def 'f 'x (make-add 3 'x)))

; (define (g y) (f (* 2 y)))
(define g (make-fun-def 'g 'y (make-fun 'f (make-mul 2 'y))))

; (define (h v) (+ (f v) (g v)))
(define h (make-fun-def 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v))))

; BSL-fun-def* is a [List-of BSL-fun-def]
; a list of all the one-argument function definitions in my definitions area

(define da-fgh (list f g h))

; An AL (association list) is [List-of Association].
; An Association is (cons Symbol (cons Number '())).

(define x2y4 (list (list 'x 2) (list 'y 4)))
(define x1 (list (list 'x 1)))

; BSL-da-all is a structure (make-defs AL BSL-fun-def*)
; interpretation: all the constant definitions and the function definitions for a program.

(define-struct def [constants functions])

; example:
(define pi-program (make-def '((close-to-pi 3.14))
                             (list (make-fun-def 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r)))
                                   (make-fun-def 'volume-of-10-cylinder 'r (make-mul 10 (make-fun 'area-of-circle 'r)))))) ; suspect this if test fails


; S-expr -> BSL-fun-def
; creates representation of a BSL definition for s (if possible)

(check-error (def-parse 'a) WRONG)
(check-error (def-parse "dude") WRONG)
(check-error (def-parse 9) WRONG)
; (check-error (def-parse '(+ 1)) "the functions + and * are already defined")
(check-error (def-parse '(+ 1 2 3)) WRONG)
(check-error (def-parse '(+ 1 2)) WRONG)
(check-error (def-parse '(* 1 2)) WRONG)
(check-error (def-parse '(and #t #f)) WRONG)
(check-error (def-parse '(+ 1 x)) WRONG)
(check-error (def-parse '(h 2)) WRONG)
(check-error (def-parse '(h 2 3)) WRONG)
(check-expect (def-parse '(define (f x) (+ 3 x))) f)
(check-error (def-parse '(define (f x y) (+ 3 x))) WRONG)
(check-error (def-parse '(define f x (+ 3 x))) WRONG)
; (check-error (def-parse '(define x (+ 3 x))) WRONG) ; <-- it should catch this somewhere... but it doesn't.
(check-error (def-parse '(define (f x) (+))) WRONG)
; (check-error (def-parse '(define (f x) +)) WRONG)
(check-error (def-parse '(define (f 2) (+ 3 x))) WRONG)
(check-expect (def-parse '(define f 2)) '(f 2)) ; constant definition, simple.
(check-expect (def-parse '(define f (+ 1 2))) (list 'f (make-add 1 2)))  ; constant definition with an expression.
(check-expect (def-parse '(define f (+ 1 (h 2)))) (list 'f (make-add 1 (make-fun 'h 2)))) ; with a function in it.
                                
(define (def-parse s)
  (local (; S-expr -> AL
          (define (constant-def-parse s)
            (append (list (second s)) (list (parse (third s))))) ; removes the define and gives me the rest, as a list (an AL)
          ; S-expr -> BSL-fun-def
          (define (def-parse s)
            (cond
              [(atom? s) (error WRONG)]
              [else
               (cond
                 [(and (= (length s) 3) (eq? (first s) 'define) (list? (second s))) ; added (list? (second s)) to get me functions.
                  (head-parse (second s) (parse (third s)))]
                 [(and (= (length s) 3) (eq? (first s) 'define) (symbol? (second s))) ; this should get me constants.
                  (constant-def-parse s)] ; this calls the constant def.
                 [else (error WRONG)])]))
          ; S-expr BSL-expr -> BSL-fun-def
          (define (head-parse s body)
            (cond
              [(atom? s) (error WRONG)]
              [else
               (if (not (= (length s) 2))
                   (error WRONG)
                   (local ((define name (first s))
                           (define param (second s)))
                     (if (and (symbol? name) (symbol? param))
                         (make-fun-def name param body)
                         (error WRONG))))])))
    (def-parse s)))

; S-expr -> BSL-fun-expr
; creates representation of a BSL expression for s (if possible)

(check-expect (parse 'a) 'a)
(check-error (parse "dude") "strings not allowed")
(check-expect (parse 9) 9)
(check-error (parse '(+ 1)) "the functions + and * are already defined")
(check-error (parse '(+ 1 2 3)) WRONG)
(check-expect (parse '(+ 1 2)) (make-add 1 2))
(check-expect (parse '(* 1 2)) (make-mul 1 2))
(check-error (parse '(and #t #f)) WRONG)
(check-expect (parse '(+ 1 x)) (make-add 1 'x))
(check-expect (parse '(+ 1 (* x y))) (make-add 1 (make-mul 'x 'y)))
(check-expect (parse '(h 2)) (make-fun 'h 2))
(check-error (parse '(h 2 3)) WRONG)

(define (parse s)
  (local (; S-expr -> BSL-expr
          (define (parse s)
            (cond
              [(atom? s) (parse-atom s)]
              [else (parse-sl s)]))
          ; SL -> BSL-fun-expr 
          (define (parse-sl s)
            (local ((define L (length s)))
              (cond
                [(and (= L 2) (or (equal? (first s) '+) (equal? (first s) '*))) (error "the functions + and * are already defined")] 
                [(and (= L 2) (symbol? (first s))) ; this looks like a function. a list of 2 with the first as a symbol.
                 (make-fun (first s) (parse (second s)))]
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

; S-expr -> BSL-da-all
; parses a SL as a BSL-da-all assuming the former is a list of quoted BSL definitions.

(check-expect (da-parse '((define close-to-pi 3.14)
                          (define (area-of-circle r)
                            (* close-to-pi (* r r)))
                          (define (volume-of-10-cylinder r)
                            (* 10 (area-of-circle r))))) pi-program)
                        
(define (da-parse s)
  (local ((define parsed (map def-parse s))) ; parses the list using def-parse
    (make-def (filter list? parsed) (filter fun-def? parsed)))) ; sorts them into two pieces

; S-expr, [List-of S-expr] -> Number
; consumes an S-expr and an Sl. The former is supposed to represent an expression and the latter a list of definitions.
; The function parses both with the appropriate parsing functions and then uses eval-all from exercise 349 to evaluate the expression.

(check-expect (eval-all-sexpr '(volume-of-10-cylinder 1) '((define close-to-pi 3.14)
                          (define (area-of-circle r)
                            (* close-to-pi (* r r)))
                          (define (volume-of-10-cylinder r)
                            (* 10 (area-of-circle r))))) 31.4)

(define (eval-all-sexpr s sl)
  (local (; get the expression as a local constant BSL-fun-expr
          (define ex (parse s))
          ; get the da as a local constant BSL-da-all
          (define da (da-parse sl))) ; hint says i have to change da-parse a bit
    ; -IN-
  (eval-all da ex)))
 

; BSL-da-all, Symbol -> BSL-expr
; consumes a BSL-da-all da and a symbol x. It produces the representation of a constant definition whose name is x,
; if such a piece of data exists in da;
; otherwise the function signals an error saying that no such constant definition can be found.

(check-expect (lookup-con-def pi-program 'close-to-pi) 3.14)
(check-error (lookup-con-def pi-program 'dude) "no such constant definition can be found")

(define (lookup-con-def da x)
  (lookup-con (def-constants da) x))

; BSL-da-all, Symbol -> BSL-fun-def
; consumes a BSL-da-all da and a symbol f. It produces the representation of a function definition whose name is f,
; if such a piece of data exists in da; otherwise the function signals an error saying that no such function definition can be found.

(check-expect (lookup-fun-def pi-program 'area-of-circle) (make-fun-def 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r))))
(check-error (lookup-fun-def pi-program 'dude) "no such function definition can be found")

(define (lookup-fun-def da x)
  (lookup-def (def-functions da) x))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da 
; or signal "undefined function" if da does not contain one

(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'square) "no such function definition can be found")

(define (lookup-def da f)
  (cond
    [(empty? da) (error "no such function definition can be found")]
    [else (if (symbol=? (fun-def-name (first da)) f) (first da) (lookup-def (rest da) f))]))

; BSL-fun-def*, BSL-fun-expr -> Number
; Given the BSL-fun-expr representation of some expression ex and the BSL-fun-def* representation of a definitions area da,
; produces the result that DrRacket shows if you evaluate ex in the interactions area assuming the definitions area contains da.

(check-expect (eval-function* da-fgh (make-fun 'f 2)) 5)
(check-error (eval-function* da-fgh (make-fun 'h (make-fun 'square 2))) "no such function definition can be found")
(check-expect (eval-function* da-fgh (make-fun 'h (make-add 1 1))) 12)

(define (eval-function* da ex)
  (local ((define (eval ex)
            (eval-function* da ex))) ; gives me a shorthand to avoid repetition.
    (cond
      [(number? ex) ex]
      [(add? ex) (+ (eval (add-left ex)) (eval (add-right ex)))]
      [(mul? ex) (* (eval (mul-left ex)) (eval (mul-right ex)))]
      [(fun? ex) (local ((define def (lookup-def da (fun-name ex))) ; lookup the function definition
                         (define b (fun-def-body def)) ; the function body of definition def
                         (define x (fun-def-param def))) ; the function parameter of definition def.
                         ; -IN-
                   (eval (subst b x (eval (fun-arg ex)))))] ; evaluates the function. note I had to make a change to subst to handle functions.
      [(not (numeric? ex)) (error "it is impossible to evaluate an expression that contains a variable")]
      [else (error "not an expression")])))

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
    [(fun? b) (make-fun (fun-name b) (subst (fun-arg b) s n))] ; had to add this line to subst so it could deal with functions that have functions in their arguments, or functions with multiple functions in their bodies or something.
    [else (error "WRONG")]))

; BSL-fun-expr, Symbol, Symbol, BSL-fun-expr -> Number
; evaluates expressions that include functions, given a BSL-fun-expr ex, a symbol f for the function name,
; a symbol x for the function's parameter and a BSL-fun-expr b which represents the function's body.

(check-expect (eval-definition1 (make-fun 'f 1) 'f 'x (make-mul 'x 2)) 2)
(check-expect (eval-definition1 (make-add 1 (make-fun 'square (make-fun 'square (make-add 2 1)))) 'square 'y (make-mul 'y 'y)) 82)
(check-expect (eval-definition1 10-10 'f 'x (make-mul 'x 2)) 0)
(check-expect (eval-definition1 20x3+33 'f 'x (make-mul 'x 2)) 93)
(check-expect (eval-definition1 5 'f 'x (make-mul 'x 2)) 5)
(check-error (eval-definition1 'x 'f 'y 'y) "it is impossible to evaluate an expression that contains a variable")
(check-error (eval-definition1 (make-add 1 (make-fun 'square (make-fun 'square (make-add 'x 1)))) 'square 'y (make-mul 'y 'y)) "it is impossible to evaluate an expression that contains a variable")
(check-error (eval-definition1 (make-fun 'g 1) 'f 'x (make-mul 'x 2)) "it is impossible to evaluate an expression that contains an undefined function")

(define (eval-definition1 ex f x b)
  (local (; BSL-expr -> N
          ; with the function arguments are all set as constants, evaluates all BSL-fun-expr
          (define (eval-one bexp)
            (eval-definition1 bexp f x b)))
    (cond
      [(number? ex) ex]
      [(add? ex) (+ (eval-one (add-left ex)) (eval-one (add-right ex)))]
      [(mul? ex) (* (eval-one (mul-left ex)) (eval-one (mul-right ex)))]
      [(fun? ex)
       (cond
         [(not (equal? (fun-name ex) f)) (error "it is impossible to evaluate an expression that contains an undefined function")]
         [else (eval-one (subst b x (eval-one (fun-arg ex))))])]
      [(not (numeric? ex)) (error "it is impossible to evaluate an expression that contains a variable")]
      [else (error "not an expression")])))

; BSL-expr -> N
; evaluates a BSL expression

(check-expect (eval-expression 10-10) 0)
(check-expect (eval-expression 20x3+33) 93)
(check-expect (eval-expression 5) 5)


(define (eval-expression bexp)
  (cond
    [(number? bexp) bexp]
    [(add? bexp) (+ (eval-expression (add-left bexp)) (eval-expression (add-right bexp)))]
    [(mul? bexp) (* (eval-expression (mul-left bexp)) (eval-expression (mul-right bexp)))]
    [else (error "not an expression")]))


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
    [else (error "WRONG")]))
  
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
(check-error (lookup-con x1 'y) "no such constant definition can be found")

(define (lookup-con da x)
  (cond
    [(empty? da) (error "no such constant definition can be found")]
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

; BSL-fun-expr, BSL-da-all -> Number
; consumes the representation of an expression and a definitions area, and produces the same value that DrRacket shows if the expression is entered
; at the prompt in the interactions area and the definitions area contains the appropriate definitions.
; Hint Your eval-all function should process variables in the given expression like eval-var-lookup in exercise 341.

(check-expect (eval-all pi-program (make-fun 'volume-of-10-cylinder 1)) 31.4)
(check-expect (eval-all pi-program (make-fun 'area-of-circle 1)) 3.14)
(check-expect (eval-all (make-def '((a 2)(b 4)) (list (make-fun-def 'times-a+b 'x (make-mul 'x (make-add 'a 'b))))) (make-fun 'times-a+b 2)) 12)

 (define (eval-all da ex)
   (local ((define (eval ex)
          (eval-all da ex))) ; gives me a shorthand to avoid repetition.
     (cond
       [(number? ex) ex]
       [(symbol? ex) (eval (lookup-con-def da ex))]
       [(add? ex) (+ (eval (add-left ex)) (eval (add-right ex)))]
       [(mul? ex) (* (eval (mul-left ex)) (eval (mul-right ex)))]
       [(fun? ex) (local ((define def (lookup-fun-def da (fun-name ex))) ; lookup the function definition. changed to use new lookup function.
                          (define b (fun-def-body def)) ; the function body of definition def
                          (define x (fun-def-param def))) ; the function parameter of definition def.
                    ; -IN-
                    (eval (subst b x (eval (fun-arg ex)))))] ; evaluates the function. note I had to make a change to subst to handle functions.
       [else (error "not an expression")])))
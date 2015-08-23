;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 117 - 121|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
Exercise 117. Explain why the following sentences are syntactically legal expressions

x ; because an expression can be a variable

(= y z) ; or a primitive followed by any number of expressions, which can be variables.

(= (= y z) 0) ; ditto, and expressions can be other primitives followed by other variables too. Also, 0 is an expression.

Exercise 118. Explain why the following sentences are syntactically illegal

(3 + 4) ; it's not a variable, or a value, or a definition, or a (primitive expression expression) or a (variable expression expression) or a conditional.

number? ; it's a primitive but without an expression. 

(l) ; it's not a definition, and expressions made of variables and values
; don't use parentheses according to the syntax

(x) ; ditto


Exercise 119. Explain why the following sentences are syntactically legal definitions

(define (f x) x) ; define (variable variable) expression 

(define (f x) y) ; define (variable variable) expression

(define (f x y) 3) ; ditto. 3 is an expression, and (define ...) takes as many variables as you want.

image
Exercise 120. Explain why the following sentences are syntactically illegal
(define (f "x") x) ; "x" is not a variable and (define (STUFF) expression) takes only variables in STUFF.

(define (f x y z) (x)) ; (x) is not an expression.  

(define (f) 10) ; (define (variable variable) expr) <-- needs more than one variable.
; if (f) were a constant it wouldn't have parentheses. This seems ommitted from the syntax in fig 29/30?

Exercise 121. Discriminate the legal from the illegal sentences in the following list:

(x) ; illegal.

(+ 1 (not x)) ; legal but will result in an error if not and + behave the way i think they do.

(+ 1 2 3) ; legal. also, 6.
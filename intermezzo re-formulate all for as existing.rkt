;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |intermezzo re-formulate all for as existing|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Stop! It is an instructive exercise to re-formulate all of the above examples using the existing abstractions in ISL+.
; Doing so also indicates how to design functions with for loops instead of abstract functions.
; Hint Design and-map and or-map, which work like andmap and ormap, respectively,
; but return the appropriate non-#false values.

(require 2htdp/abstraction)

.../and apply an operation like and to all of the generated values:
> (for/and ((i 10)) (> (- 9 i) 0))
#false
> (for/and ((i 10)) (if (>= i 0) i #false))
9

; Number, 


For pragmatic reasons, the loops return the last generated value or #false if any of the results are #false.
.../or apply an operation like or to all of the generated values:
> (for/or ((i 10)) (if (= (- 9 i) 0) i #false))
9
> (for/or ((i 10)) (if (< i 0) i #false))
#false
These loops return the first value that is not #false unless all the values are #false.
.../sum add up the numbers that the iterations generate:
> (for/sum ((c "abc")) (string->int c))
294
.../product multiply the numbers that the iterations generate
> (for/product ((c "abc")) (+ (string->int c) 1))
970200
.../string create Strings from the 1Strings that the iterations generate:
> (define a (string->int "a"))
> (for/string ((j 10)) (int->string (+ a j)))
"abcdefghij"
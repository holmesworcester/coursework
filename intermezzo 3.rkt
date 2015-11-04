;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |intermezzo 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Design and-map and or-map, which work like andmap and ormap, respectively,
; but return the appropriate non-#false values.

; For pragmatic reasons, the loops return the last generated value or #false if any of the results are #false.

; [List-of X], [X -> Boolean] -> [Maybe X]
; applies an "and" operation to a list of anything. Returns false if the boolean is false for any item.
; otherwise, returns the last item in the list.

(check-expect (and-map (lambda (x) (< x 1)) '(1 2 3)) #false)
(check-expect (and-map number? '(1 2 3)) 3)

(define (and-map f l)
  (cond
    [(false? (andmap f l)) #false]
    [else (first (reverse l))]))

; [List-of X], [X -> Boolean] -> [Maybe X]
; returns the first value that is not #false unless all the values are #false.

(check-expect (or-map (lambda (x) (< x 1)) '(1 2 3)) #false)
(check-expect (or-map number? '("a" 3 "b")) 3)

(define (or-map f l)
  (cond
    [(empty? l) #false] ;if I'm at the end of the list
    [else (if (f (first l)) (first l) (or-map f (rest l)))])) ; if true return the first true one found (that one) otherwise run on rest of list.




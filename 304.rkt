;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |304|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; An S-expr (S-expression) is one of: 
; – Atom
; – SL
; An SL (S-list) is one of: 
; – '()
; – (cons S-expr SL)
; An Atom is one of: 
; – Number
; – String
; – Symbol

(define wing '(wing (wing (wing body wing) wing) wing))

(define table '((6 f)
    (5 e)
    (4 d)))

(define function '(define (f x)
     (+ (* 3 x x) (* -2 x) 55)))

(define hello '((hello 20.12 "world")))


; S-expr -> Boolean
; returns true if given an atom otherwise returns false.

(check-expect (atom? 9) #t)
(check-expect (atom? "yo") #t)
(check-expect (atom? 't) #t)
(check-expect (atom? (list 1 2)) #f)

(define (atom? s)
  (or (number? s) (string? s) (symbol? s)))


; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 

(check-expect (count wing 'wing) 6)
(check-expect (count hello 'hello) 1)
(check-expect (count hello 'dude) 0)

(define (count sexp sy)
  (local (; S-expr Symbol -> N 
          ; the main function 
          (define (count-sexp sexp)
            (cond
              [(atom? sexp) (count-atom sexp)]
              [else (count-sl sexp)]))
          ; SL Symbol -> N 
          ; counts all occurrences of sy in sl 
           (define (count-sl sl)
             (cond
               [(empty? sl) 0]
               [else (+ (count-sexp (first sl)) (count-sl (rest sl)))]))
           ; Atom Symbol -> N 
           ; counts all occurrences of sy in at 
           (define (count-atom at)
             (if (equal? at sy) 1 0))
           )
    (count-sexp sexp)))
 

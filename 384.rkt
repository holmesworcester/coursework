;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |384|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Symbol] [List-of Symbol] -> Boolean
; Consumes two lists of symbols. The first list is called a pattern, the second one a search string.
; The function returns #true if the pattern is identical to the initial part of the search string. Otherwise the function returns #false.

(check-expect (DNAprefix '(c g a t g t g t g t) '(c g a t)) #t)
(check-expect (DNAprefix '(c g a t g t g t g t) '(c g t t)) #f)
               
(define (DNAprefix search pattern)
  (cond
    [(empty? pattern) #t]
    [else (and (equal? (first search) (first pattern)) (DNAprefix (rest search) (rest pattern)))]))

; [List-of Symbol] [List-of Symbol] -> [Maybe Symbol]
; Consumes two lists of symbols. The first list is called a pattern, the second one a search string.
; Returns the first item in the search string beyond the pattern. If the lists are identical and there is no DNA letter beyond the pattern,
; the function signals an error.
; If the pattern does not match the beginning of the search string, the function returns #false.
; The function must not process either of the lists more than once.

(check-expect (DNAdelta '(c g a t g t g t g t) '(c g a t)) 'g)
(check-expect (DNAdelta '(c g a t g t g t g t) '(c g t t)) #f)
(check-error (DNAdelta '(c g a t) '(c g a t)) "strings of same length")


(define (DNAdelta search pattern)
  (cond
    [(empty? pattern) (if (cons? search) (first search) (error "strings of same length"))]
    [else (if (false? (equal? (first search) (first pattern))) #f (DNAdelta (rest search) (rest pattern)))]))
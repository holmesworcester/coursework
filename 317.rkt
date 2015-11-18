;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |317|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a Symbol.

(define TS-1st-try '((part1 part2 part3) read! ((hang draw) (read!)))) ; I wasn't completely sure about this but it's right!

(define Docs (cons 'read! '()))
(define Code (cons 'hang (cons 'draw '())))
(define Libs (cons Docs (cons Code '())))
(define Text (cons 'part1 (cons 'part2 (cons 'part3 '()))))
(define TS (cons Text (cons 'read! (cons Libs '()))))

TS

TS-1st-try
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |318|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A Dir.v2 is a structure: 
;   (make-dir Symbol LOFD)
 
; A LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a Symbol.

(define Docs (make-dir 'Docs (cons 'read! '())))
(define Code (make-dir 'Code (cons 'hang (cons 'draw '()))))
(define Libs (make-dir 'Libs (cons Docs (cons Code '()))))
(define Text (make-dir 'Text (cons 'part1 (cons 'part2 (cons 'part3 '())))))
(define TS (make-dir 'TS (cons Text (cons 'read! (cons Libs '())))))


; Dir.v2 -> N
; counts how many files a given Dir.v1 contains.

(check-expect (how-many Docs) 1)
(check-expect (how-many TS) 7)
(check-expect (how-many '()) 0)

(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(symbol? (first dir)) (+ 1 (how-many (rest dir)))]
    [else (+ (how-many (first dir)) (how-many (rest dir)))]))
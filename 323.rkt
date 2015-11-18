;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |323|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A File.v3 is a structure: 
;   (make-file Symbol N String)

(define-struct file [name size content])

; A Dir.v3 is a structure: 
;   (make-dir.v3 Symbol [List-of Dir.v3] [List-of File.v3])

(define-struct dir.v3 [name dirs files])
 
(define Docs (make-dir.v3 'Docs '() (cons (make-file 'read! 19 "") '())))
(define Code (make-dir.v3 'Code '() (cons (make-file 'hang 8 "") (cons (make-file 'draw 2 "")'()))))
(define Libs (make-dir.v3 'Libs (cons Docs (cons Code '())) '()))
(define Text (make-dir.v3 'Text '() (cons (make-file 'part1 99 "") (cons (make-file 'part2 52 "") (cons (make-file 'part3 17 "") '())))))
(define TS (make-dir.v3 'TS (cons Text (cons Libs '())) (cons 'read! '())))
(define MT (make-dir.v3 'MT '() '()))

; Dir.v3 -> N
; counts how many files a given Dir.v3 contains.

(check-expect (how-many Docs) 1)
(check-expect (how-many TS) 7)
(check-expect (how-many MT) 0)

(define (how-many d)
  (+ (foldr + 0 (map how-many (dir.v3-dirs d))) (length (dir.v3-files d)))) ; can't believe this works!! first try!!
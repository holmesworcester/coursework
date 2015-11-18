;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |319|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A Dir.v2 is a structure: 
;   (make-dir Symbol LOFD)
(define-struct dir [name content])
 
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
(check-expect (how-many (make-dir 'Emptydir '())) 0)

(define (how-many d)
  (local ((define this-lofd (dir-content d)) ; pull out the content, which is always a LOFD
          ; LOFD -> N
          ; counts how many files in a LOFD
          (define (how-many-lofd l)
            (cond
              [(empty? l) 0]
              [(symbol? (first l)) (+ 1 (how-many-lofd (rest l)))] ; it's a file
              [else (+ (how-many (first l)) (how-many-lofd (rest l)))]))) ; it's a directory
          ; -IN-
          (how-many-lofd this-lofd)))
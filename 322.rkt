;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |322|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A Dir.v2 is a structure: 
;   (make-dir Symbol Boolean N LOFD)
; interpretation: the name of a directory, whether it is world readable, its size, and its contents.

(define-struct dir [name public? size content])
 
; A LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a Symbol.

; A File.v3 is a structure: 
;   (make-file Symbol N String)

(define-struct file [name size content])

; A Dir.v3 is a structure: 
;   (make-dir.v3 Symbol Dir* File*)

(define-struct dir.v3 [name dirs files])
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)

; A File* is one of: 
; – '()
; – (cons File.v3 File*)

(define Docs (make-dir.v3 'Docs '() (cons (make-file 'read! 19 "") '())))
(define Code (make-dir.v3 'Code '() (cons (make-file 'hang 8 "") (cons (make-file 'draw 2 "")'()))))
(define Libs (make-dir 'Libs (cons Docs (cons Code '())) '()))
(define Text (make-dir 'Text '() (cons (make-file 'part1 99 "") (cons (make-file 'part2 52 "") (cons (make-file 'part3 17 "") '())))))
(define TS (make-dir 'TS (cons Text (cons Libs '())) (cons 'read! '())))
(define MT (make-dir 'MT '() '()))

; Dir.v3 -> N
; counts how many files a given Dir.v3 contains.

(check-expect (how-many Docs) 1)
(check-expect (how-many TS) 7)
(check-expect (how-many MT) 0)

(define (how-many d)
  (





  
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
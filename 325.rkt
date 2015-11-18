;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |325|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/dir)
(define d0 (create-dir "/Users/holmes/Desktop")) ; on OS X
(define d1 (create-dir "/Users/holmes/Desktop/testonefile")) ; on OS X



; A File.v3 is a structure: 
;   (make-file Symbol N String)

; (define-struct file [name size content])

; A Dir.v3 is a structure: 
;   (make-dir.v3 Symbol [List-of Dir.v3] [List-of File.v3])

; Dir.v3 -> N
; counts how many files a given Dir.v3 contains.

(check-expect (how-many d1) 1)

(define (how-many d)
  (+ (foldr + 0 (map how-many (dir-dirs d))) (length (dir-files d)))) ; can't believe this works!! first try!!

(how-many d0)
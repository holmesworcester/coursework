;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |326|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/dir)
(define d0 (create-dir "/Users/holmes/Desktop")) ; on OS X
(define d1 (create-dir "/Users/holmes/Desktop/testonefile")) ; on OS X

; A File is a structure: 
;   (make-file Symbol N String)

; (define-struct file [name size content])

; A Dir is a structure: 
;   (make-dir Symbol [List-of Dir.v3] [List-of File.v3])

; Dir -> N
; counts how many files a given Dir.v3 contains.

(check-expect (how-many d1) 2)

(define (how-many d)
  (+ (foldr + 0 (map how-many (dir-dirs d))) (length (dir-files d)))) ; can't believe this works!! first try!!

; Dir, Symbol -> Boolean
; consumes a Dir and a file name and determines whether or not a file with this name occurs in the directory tree.

(check-expect (find? d1 'test.txt) #t)
(check-expect (find? d1 'best.txt) #f)

(define (find? d f-name)
  (or ; if i find a match anywhere, return true
   (ormap (lambda (one-dir) (find? one-dir f-name)) (dir-dirs d)) ; searches each directory in the list of directories 
   (ormap (lambda (one-file) (symbol=? f-name (file-name one-file))) (dir-files d)))) ; checks each file in the list of files

; Dir -> [List-of [Dir OR File]]
; lists the names of all files and directories in a given Dir

(check-expect (ls d1) (list
 '/Users/holmes/Desktop/testonefile/testdir
 '.DS_Store
 'test.txt))

(define (ls d)
  (local (; [List-of X] -> [List-of X]
          ; removes '() from a list
          (define (remove-empty l)
            (cond
              [(empty? l) '()]
              [else (if (empty? (first l)) (remove-empty (rest l)) (cons (first l) (remove-empty (rest l))))])))
          ; -IN-
  (remove-empty (append (map dir-name (dir-dirs d)) (map ls (dir-dirs d)) (map file-name (dir-files d))))))

; Dir -> N
; consumes a Dir and computes the total size of all the files in the entire directory tree.
; Assume that storing a directory in a Dir structure costs 1 file storage unit.
; note this counts the current directory in its size count.

(check-expect (du (make-dir
 '/Users/holmes/Desktop/testonefile
 (list (make-dir '/Users/holmes/Desktop/testonefile/testdir '() '()))
 (list (make-file '.DS_Store 6148 "") (make-file 'test.txt 4 "")))) 6154)

(define (du d)
  (+ 1 (foldr + 0 (map file-size (dir-files d))) (foldr + 0 (map du (dir-dirs d)))))
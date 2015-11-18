;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |330|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/dir)
(define d0 (create-dir "/Users/holmes/Desktop")) ; on OS X
(define d1 (create-dir "/Users/holmes/Desktop/testonefile")) ; on OS X

; A File is a structure: 
;   (make-file Symbol N String)

; (define-struct file [name size content])

; A Dir is a structure: 
;   (make-dir Symbol [List-of Dir] [List-of File])

; (define-struct dir.v3 [name dirs files])

(define Docs (make-dir 'Docs '() (cons (make-file 'read! 19 "") '())))
(define Code (make-dir 'Code '() (cons (make-file 'hang 8 "") (cons (make-file 'draw 2 "")'()))))
(define Libs (make-dir 'Libs (cons Docs (cons Code '())) '()))
(define Text (make-dir 'Text '() (cons (make-file 'part1 99 "") (cons (make-file 'part2 52 "") (cons (make-file 'part3 17 "") '())))))
(define TS (make-dir 'TS (cons Text (cons Libs '())) (cons (make-file 'read! 19 "") '())))
(define MT (make-dir 'MT '() '()))

; Path = [List-of Symbol]
; interpretation directions on how to find a file in a directory tree

; Dir -> N
; counts how many files a given Dir.v3 contains.

(check-expect (how-many d1) 2)

(define (how-many d)
  (+ (foldr + 0 (map how-many (dir-dirs d))) (length (dir-files d)))) ; can't believe this works!! first try!!

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

(check-expect (du Docs) 20)

(define (du d)
  (+ 1 (foldr + 0 (map file-size (dir-files d))) (foldr + 0 (map du (dir-dirs d)))))

; Dir -> [List-of Path]
; generates a list of paths to every file in a directory.

(check-expect (list-all-paths MT) '())
(check-expect (list-all-paths Docs) '((Docs read!)))
(check-expect (list-all-paths Text) '((Text part1)(Text part2)(Text part3)))
(check-expect (list-all-paths TS) '((TS read!)(TS Text part1)(TS Text part2)(TS Text part3)(TS Libs Docs read!)(TS Libs Code hang)(TS Libs Code draw)))

(define (list-all-paths d) 
    (append
     (map (lambda (file) (append (list (dir-name d)) (list (file-name file)))) (dir-files d)) ; list current files in directory with current dir name.
     (map (lambda (p) (append (list (dir-name d)) p)) (foldr append '() (map list-all-paths (dir-dirs d)))))) ; some crazy code to make lists of paths for all files without nested lists.
               

; Dir, Symbol -> [Maybe Path]
; consumes a directory d and a file name f. If (find? d f) is true, find produces a path to a file with name f; otherwise it produces #false.

(check-expect (find? Docs 'dude) #f)
(check-expect (find? Docs 'read!) '(Docs read!))
(check-expect (find? TS 'part1) (list 'TS 'Text 'part1))

(define (find? d f)
  (local (; Generate a [List-of Path] that represents all paths to all files.
          (define all-paths (list-all-paths d))
          ; Path -> [Boolean]
          ; Checks the last item of a path to see if it matches f. Returns true if it does, otherwise returns false.
          (define (find-in-one-path? path)
            (symbol=? f (first (reverse path))))
          ; [List-of Path], Boolean -> [Maybe Path]
          ; runs through a list of paths and returns the first one that matches Boolean. If it finds none, returns false.
          (define (find-in-all-paths? lop)
            (cond
              [(empty? lop) #f]
              [(find-in-one-path? (first lop)) (first lop)] ; if the last item of the first path in the list matches the symbol return that full path.
              [else (find-in-all-paths? (rest lop))]))
          ) ;-IN-
    (find-in-all-paths? all-paths)))

; Challenge: The find function discovers only one of the two files named read!
; file in figure 81. Design find-all, which is generalizes find and produces the
; list of all paths that lead to f in d. What should find-all produce when (find? f d) is #false?
; Is this part of the problem really a challenge compared to the basic problem?

; Dir, Symbol -> [List-of Path]
; returns a list of all paths that have a matching symbol at the end

(check-expect (find-all TS 'read!) '((TS read!)(TS Libs Docs read!)))
(check-expect (find-all TS 'asdfihaiiew) '())

(define (find-all d f)
  (local (
          ; Path -> [Boolean]
          ; Checks the last item of a path to see if it matches f. Returns true if it does, otherwise returns false.
          ; really i should pull this out and reuse since i copied / pasted.
          (define (find-in-one-path? path)
            (symbol=? f (first (reverse path)))))
    (filter find-in-one-path? (list-all-paths d))))

; Dir -> [List-of Path]
; Design the function ls-R, which lists the paths to all files in a given Dir.

(define ls-R list-all-paths)

; Dir -> [List-of Path]
; Design the function ls-R, which lists the paths to all files in a given Dir. Challenge, list dirs too.

(check-expect (ls-R-dir TS) (list
 (list 'TS 'read!)
 (list 'TS 'Text)
 (list 'TS 'Libs)
 (list 'TS 'Text 'part1)
 (list 'TS 'Text 'part2)
 (list 'TS 'Text 'part3)
 (list 'TS 'Libs 'Docs)
 (list 'TS 'Libs 'Code)
 (list 'TS 'Libs 'Docs 'read!)
 (list 'TS 'Libs 'Code 'hang)
 (list 'TS 'Libs 'Code 'draw)))

(define (ls-R-dir d)
    (append
     (map (lambda (file) (append (list (dir-name d)) (list (file-name file)))) (dir-files d)) ; list current files in directory with current dir name.
     (map (lambda (dir) (append (list (dir-name d)) (list (dir-name dir)))) (dir-dirs d)) ; list current dir names in directory with current dir name
     (map (lambda (p) (append (list (dir-name d)) p)) (foldr append '() (map ls-R-dir (dir-dirs d)))))) ; some crazy code to make lists of paths for all files without nested lists.

; Dir, Symbol -> [List-of Path]
; returns a list of all paths that have a matching symbol at the end, using ls-R-dir to search directories too. 

(check-expect (find-all.v2 TS 'read!) '((TS read!)(TS Libs Docs read!)))
(check-expect (find-all.v2 TS 'asdfihaiiew) '())
(check-expect (find-all.v2 TS 'Docs) '((TS Libs Docs)))

(define (find-all.v2 d f)
  (local (
          ; Path -> [Boolean]
          ; Checks the last item of a path to see if it matches f. Returns true if it does, otherwise returns false.
          ; really i should pull this out and reuse since i copied / pasted.
          (define (find-in-one-path? path)
            (symbol=? f (first (reverse path)))))
    (filter find-in-one-path? (ls-R-dir d))))
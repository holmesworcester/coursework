;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 181-185) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Exercise 181. Use list to construct the equivalent of the following lists:

(check-expect (cons "a" (cons "b" (cons "c" (cons "d" (cons "e" '()))))) (list "a" "b" "c" "d" "e"))

(check-expect (cons (cons 1 (cons 2 '())) '()) (list (list 1 2)))

(check-expect (cons "a" (cons (cons 1 '()) (cons #false '()))) (list "a" (list 1) #false))

(check-expect (cons (cons 1 (cons 2 '())) (cons (cons 2 '()) '())) (list (list 1 2) (list 2))) 

(check-expect (cons (cons "a" (cons 2 '())) (cons "hello" '())) (list (list "a" 2) "hello"))

; Start by determining how many items each list and each nested list contains. Use check-expect to express your answers; this ensures that your abbreviations are really the same as the long-hand. image
; Exercise 182. Use cons and '() to construct the equivalent of the following lists:

(check-expect (list (list "adam" 0) (list "eve" 1) (list "louisXIV" 2)) (cons (cons "adam" (cons 0 '())) (cons (cons "eve" (cons 1 '())) (cons (cons "louisXIV" (cons 2 '())) '()))))

(check-expect (list 1 (list 1 2) (list 1 2 3)) (cons 1 (cons (cons 1 (cons 2 '())) (cons (cons 1 (cons 2 (cons 3 '()))) '()))))  


; Use check-expect to express your answers. Exercise 183. On some occasions lists are formed with cons and list. Reformulate the following lists using cons and '() exclusively:

(check-expect (cons "a" (list 0 #false)) (cons "a" (cons 0 (cons #false '()))))

(check-expect (list (cons 1 (cons 13 '()))) (cons (cons 1 (cons 13 '())) '()))

(check-expect (cons (list 1 (list 13 '())) '()) (cons (cons 1 (cons (cons 13 (cons '() '())) '())) '()))


(check-expect (list '() '() (cons 1 '())) (cons '() (cons '() (cons (cons 1 '()) '()))))

(check-expect (cons "a" (cons (list 1) (list #false '()))) (cons "a" (cons (cons 1 '()) (cons #false (cons '() '())))))

; Then formulate the lists using only list. Use check-expect to express your answers. image
; Exercise 184. Determine the values of the following expressions:
(check-expect (list (string=? "a" "b") (string=? "c" "c") #false) (list #f #t #f))

(check-expect (list (+ 10 20) (* 10 20) (/ 10 20)) (list 30 200 1/2))

(check-expect (list "dana" "jane" "mary" "laura") (list "dana" "jane" "mary" "laura")) ;huh?

; Exercise 185. You know about first and rest from BSL, but BSL+ comes with even more selectors than that. Determine the values of the following expressions:

(check-expect (first (list 1 2 3)) 1)

(check-expect (rest (list 1 2 3)) (list 2 3))

(check-expect (second (list 1 2 3)) 2)

; Find out from the documentation whether third, fourth, and fifth exist. image

#true
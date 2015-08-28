;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |165|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A ListOfStrings is one of:
; - '()
; - (cons String ListOfStrings)

; ListOfStrings -> ListOfStrings
; subst-robot consumes a list of toy descriptions (strings) and
; replaces all occurrences of "robot" with "r2d2"; all other descriptions remain the same.

(check-expect (subst-robot (cons "robot" '())) (cons "r2d2" '()))
(check-expect (subst-robot (cons "dude" '())) (cons "dude" '()))
(check-expect (subst-robot (cons "dude" (cons "robot" '()))) (cons "dude" (cons "r2d2" '())))

(define (subst-robot l)
  (cond
    [(empty? l) '()]
    [(cons? l)(cons (if (string=? "robot" (first l)) "r2d2" (first l)) (subst-robot (rest l)))]))

; ListOfStrings, String, String -> ListOfStrings
; substitute consumes a list of toy descriptions (strings) and
; replaces all occurrences of "robot" with "r2d2"; all other descriptions remain the same.

(check-expect (substitute "robot" "r2d2" (cons "robot" '())) (cons "r2d2" '()))
(check-expect (substitute "robot" "r2d2" (cons "dude" '())) (cons "dude" '()))
(check-expect (substitute "robot" "r2d2" (cons "dude" (cons "robot" '()))) (cons "dude" (cons "r2d2" '())))

(define (substitute old new l)
  (cond
    [(empty? l) '()]
    [(cons? l)(cons (if (string=? old (first l)) new (first l)) (substitute old new (rest l)))]))

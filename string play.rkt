;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |string play|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define str "helloworld")
(define i 5)
(define (errornotenoughletters STRING i) (string-append "Sorry, the string " STRING " has less than " (number->string i) " letters!"))


(define (underscore STRING) (string-append (substring STRING 0 5) "_" (substring STRING 5 10)))
(define (removeithletter STRING i) (cond
  [(< (string-length STRING) i) (errornotenoughletters STRING i)]
  [(>= (string-length STRING) i) (string-append (substring STRING 0 (- i 1)) (substring STRING i (string-length STRING)))]))

str
(underscore str)
(removeithletter str 11)




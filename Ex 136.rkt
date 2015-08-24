;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 136|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; List-of-names -> Boolean
; determines whether "Flatt" occurs on a-list-of-names
(define (contains-flatt? a-list-of-names)
  (cond
    [(empty? a-list-of-names) #false]
    [else (cond
            [(string=? (first a-list-of-names) "Flatt") #true]
            [else (contains-flatt? (rest a-list-of-names))])]))

; (contains-flatt? (cons "Flatt" (cons "C" '())))

; (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))

(contains-flatt? (cons "A" (cons "B" (cons "C" '()))))
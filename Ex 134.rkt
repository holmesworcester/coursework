;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 134|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A List-of-names is one of: 
; – '()
; – (cons String List-of-names)
; interpretation a List-of-names represents a list of invitees by last name

(define LIST-OF-NAMES
  (cons "Holmes"
      (cons "Giseli"
            (cons "Martina"
                  '()))))

; List-of-names -> Boolean
; determines whether "Flatt" occurs on a-list-of-names
(define (contains-flatt? a-list-of-names)
  (cond
    [(empty? a-list-of-names) #false]
    [else (cond
            [(string=? (first a-list-of-names) "Flatt") #true]
            [else (contains-flatt? (rest a-list-of-names))])]))

; my version (the second version) is better. It's better because it's easier to understand,
; because figuring out the implications of the or in this context is a bit tricky,
; and because it lets me return other aspects of this list item. For example I could search
; by first name and return last name, at least for the first one I found
; (I did it with an "if" on my first pass instead of a cond)

(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Findler" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect
  (contains-flatt? (cons "Mur" (cons "Fish"  (cons "Find" '()))))
  #false)
(check-expect
  (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)
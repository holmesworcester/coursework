;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 135|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A List-of-strings is one of: 
; – '()
; – (cons String List-of-strings)
; interpretation a List-of-strings represents a list of Strings

(define LIST-OF-NAMES-TEST
  (cons "Fagan"
        (cons "Findler"
              (cons "Fisler"
                    (cons "Flanagan"
                          (cons "Flatt"
                                (cons "Felleisen"
                                      (cons "Friedman" '()))))))))


; String, List-of-strings -> Boolean
; determines whether a String s occurs on a List-of-strings l
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (cond
            [(string=? (first l) s) #true]
            [else (contains? s (rest l))])]))

(check-expect (contains? "Flatt" '()) #false)
(check-expect (contains? "Flatt" (cons "Findler" '())) #false)
(check-expect (contains? "Flatt" (cons "Flatt" '())) #true)
(check-expect
  (contains? "Flatt" (cons "Mur" (cons "Fish"  (cons "Find" '()))))
  #false)
(check-expect
  (contains? "Flatt" (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)
(check-expect (contains? "Flatt" LIST-OF-NAMES-TEST) #true)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 129|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(check-expect 3 4) ; 3 is not equal to 4
(check-member-of "green" "red" "yellow" "grey") ; green is not a member of the set of others
(check-within (make-posn #i1.0 #i1.1) (make-posn #i0.9 #i1.2) 0.01) ; i think it fails. epsilon too small
(check-range #i0.9 #i0.6 #i0.8) ; pass XXX oh, got that wrong. checks if the first is in the range of the second two
(check-error (/ 1 1)) ; fails. no error.
(check-random (make-posn (random 3) (random 9))
              (make-posn (random 9) (random 3))) ; fails, they use random differently.
(check-satisfied 4 odd?) ; fails, 4 not odd
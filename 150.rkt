;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |150|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; yes

; if you give v2 0.1 it will make a really long list since (sub1 0.1) is -0.9 on to -infinity.
; (zero? -0.9) is false

; if you give v1 0.1 it will write xyz once because (positive? -0.9) returns false and stops the show.


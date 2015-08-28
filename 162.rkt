;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |162|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (cond
    [(>= h 100) (error "reported wage exceeds 100 hours")]
    [else (* 12 h)]))

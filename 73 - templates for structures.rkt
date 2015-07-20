;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |73 - templates for structures|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct movie [title director year])

(define (moviecover m)
  (... (movie-title m) ... (movie-director m) ... (movie-year m)))

(define-struct boyfriend [name hair eyes phone])

(define (bfentry bf)
  (... (boyfriend-name bf) ... (boyfriend-hair bf) ... (boyfriend-eyes bf) ... (boyfriend-phone bf)))

(define-struct cheerleader [name number])

(define (cheerbook p)
  (... (cheerleader-name p) ... (cheerleader-number p)))

(define-struct CD [artist title price])

(define (listing c)
  (... (CD-artist c) ... (CD-title c) ... (CD-price c)))

(define-struct sweater [material size color])

(define (buy s)
  (... (sweater-material s) ... (sweater-size s) ... (sweater-color s)))


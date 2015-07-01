;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname movietheater2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;constants
(define baseline-audience 120)
(define baseline-price 5.0)
(define change-per-dollar (/ 15 0.1))
(define fixed-performance-cost 180)
(define cost-per-attendee 0.04)

;functions
(define (attendees ticket-price)
  (- baseline-audience (* (- ticket-price baseline-price) change-per-dollar)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ fixed-performance-cost (* cost-per-attendee (attendees ticket-price))))

;(define (profit ticket-price)
; (- (revenue ticket-price)
;     (cost ticket-price)))

(define (profit price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (+ 180
        (* 0.04
           (+ 120
              (* (/ 15 0.1)
                 (- 5.0 price)))))))

(profit 1)
(profit 2)
(profit 3)
(profit 4)
(profit 5)
(profit 2.5)
(profit 2.6)
(profit 2.7)
(profit 2.8)
(profit 2.9)
(profit 3.0)
(profit 3.1)
(profit 3.2)
(profit 3.3)
(profit 3.4)
(profit 3.5)
(profit 3.6)


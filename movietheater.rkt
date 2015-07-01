;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname movietheater) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;constants
(define baseline-audience 120)
(define baseline-price 5.0)
(define observed-negative-change 15)
(define correspondng-price-increase 0.1)
(define change-per-dollar (/ observed-negative-change corresponding-price-increase))

(define fixed-performance-cost 0)
(define cost-per-attendee 1.5)

;functions
(define (attendees ticket-price)
  (- baseline-audience (* (- ticket-price baseline-price) change-per-dollar)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ fixed-performance-cost (* cost-per-attendee (attendees ticket-price))))

(define (profit ticket-price)
 (- (revenue ticket-price)
     (cost ticket-price)))

(define (profitv2 price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (+ 180
        (* 1.50
           (+ 0
              (* (/ 15 0.1)
                 (- 5.0 price)))))))
"v1"
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
"v2"
(profitv2 1)
(profitv2 2)
(profitv2 3)
(profitv2 4)
(profitv2 5)
(profitv2 2.5)
(profitv2 2.6)
(profitv2 2.7)
(profitv2 2.8)
(profitv2 2.9)
(profitv2 3.0)
(profitv2 3.1)
(profitv2 3.2)
(profitv2 3.3)
(profitv2 3.4)
(profitv2 3.5)
(profitv2 3.6)
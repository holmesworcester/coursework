;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |421|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define TOLERANCE 0.001)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R TOLERANCE)]
; assume f is continuous 
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divide interval in half, the root is in one of the two
; halves, pick according to assumption

(check-within TOLERANCE (find-root poly 1 3) 2)
(check-within TOLERANCE (find-root poly 3 100) 4)

(define (find-root f left right)
  (cond
    [(<= (- right left) TOLERANCE) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
           (find-root f left mid)]
          [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
           (find-root f mid right)]))]))

(find-root poly 0 100) ; you can't because you'll violate assumptions
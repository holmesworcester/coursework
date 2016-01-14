;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |423|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
; termination argument: it will terminate because we know the line crosses zero and each step brings right and left closer together and when they're within TOLERANCE it terminates. 

(check-within TOLERANCE (find-root poly 1 3) 2)
(check-within TOLERANCE (find-root poly 3 100) 4)

(define (find-root f left right)
  (local (; [Number -> Number] Number Number Number Number -> Number
          ; consumes a left margin, a right margin, and the values of the function at those margins, and returns an approximation of the root of the function f (which it takes as a constant)
          (define (find-root-faster left right f@left f@right)
            (cond    
              [(<= (- right left) TOLERANCE) left]
              [else
               (local (; evaluate everything I know I need just once, up front.
                       ; define mid and f(mid) which is the only thing i need to recalculate each time. 
                       (define mid (/ (+ left right) 2))
                       (define f@mid (f mid)))
                 ;-IN-
                 (cond
                   [(or (<= f@left 0 f@mid) (<= f@mid 0 f@left))
                    (find-root-faster left mid f@left f@mid)]
                   [(or (<= f@mid 0 f@right) (<= f@right 0 f@mid))
                    (find-root-faster mid right f@mid f@right)]))])))
    ;-IN-
    (find-root-faster left right (f left) (f right))))

  
  



; (find-root poly 0 100) ; you can't because you'll violate assumptions


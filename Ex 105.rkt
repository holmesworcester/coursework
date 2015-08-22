;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 105|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Constants

(define SPIDER-VOL (* .5 .4 .2 1.5)) ; the top-end volume of a spider plus the extra room it needs to be happy
(define ELEPHANT-VOL (* 8 12 4 2)) ; ditto for elephants
(define ARMADILLO-VOL (* 3 1 1.2 2)) ; ditto for armadillos
(define BOA-WIGGLE 2) ; boas need wiggle room too. this is the proportion of cage to their own volume they need to wiggle. must be > 1

; A Spider is a Number in (0, 8)
; interpretation, the number of legs our spider has.

; A Boa Constrictor is a structure (make-boa length girth) where length is the length of the boa constrictor
; and girth is the diameter of its thickest cross section, measured in feet.

(define-struct boa (length girth))
(define LONG-BOA (make-boa 30 1))
(define SHORT-BOA (make-boa 4 .3))

; An Animal is one of:
; - Spider
; - BoaConstrictor
; - "elephant"
; - "armadillo"

; An example template for functions that consume representations of zoo animals:

(define (animal-function-template a)
  (cond
    [(string? a)(cond
                       [(string=? "elephant")(... ELEPHANT-VOL)] ; these constants might be useful
                       [(string=? "armadillo")(... ARMADILLO-VOL)])]
    [(number? a)(... a ... SPIDER-VOL)] ; spider is the only Number
    [(boa? a)(... (boa-length a) ... (boa-girth a))])) ; where a is the number of legs


; Animal, Volume -> Boolean
; Returns true if an Animal a will fit in a cage of Volume cage, assuming that cages are reasonably well-suited
; to the dimensions of the animal, and that animals need a "wiggle-room" factor of WIGGLE-ROOM.

; cages that are an exact fit (must be strictly greater)

(check-expect (fits? "elephant" ELEPHANT-VOL) false)
(check-expect (fits? 8 SPIDER-VOL) false)
(check-expect (fits? "armadillo" ARMADILLO-VOL) false)
(check-expect (fits? 0 SPIDER-VOL) false)
(check-expect (fits? LONG-BOA (* 30 BOA-WIGGLE)) false)
(check-expect (fits? SHORT-BOA (* 1.2 BOA-WIGGLE)) false)

; too small cages (well, infinitely too small)

(check-expect (fits? "elephant" 0) false)
(check-expect (fits? 8 0) false)
(check-expect (fits? "armadillo" 0) false)
(check-expect (fits? 0 0) false) ; a pretty unlucky spider. number of legs shouldn't matter for fits? seems like insult to injury.

; bigger cages

(check-expect (fits? "elephant" (+ ELEPHANT-VOL 1)) true)
(check-expect (fits? "armadillo" (+ ARMADILLO-VOL 1)) true)
(check-expect (fits? 8 (+ SPIDER-VOL 1)) true)
(check-expect (fits? 1 (+ SPIDER-VOL 1)) true) ; let's try another spider
(check-expect (fits? LONG-BOA (* 30 BOA-WIGGLE 1.2)) true) ; make the cage a bit bigger
(check-expect (fits? SHORT-BOA (* 1.2 BOA-WIGGLE 1.2)) true) ; make the cage a bit bigger


(define (fits? a cage)
  (cond
    [(string? a)(cond
                       [(string=? a "elephant")(> cage ELEPHANT-VOL)] ; these constants might be useful
                       [(string=? a "armadillo")(> cage ARMADILLO-VOL)])]
    [(number? a)(> cage SPIDER-VOL)]
    [(boa? a)(> cage (* (boa-length a) (boa-girth a) BOA-WIGGLE))])) 

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |411|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Posn -> Posn 
; makes a point within 0 - MAX on the x and y axes that isn't as the same location as the given position.
; the function will terminate because it has been given just one position, so eventually
; random will create a position that is not its position. 
 
(check-satisfied (food-create (make-posn 1 1)) not-equal-1-1?)
 
(define (food-create p)
  (local (; checks to make sure the result of random is not on the given point
          (define (food-check-create p candidate)
            (if (equal? p candidate) (food-create p) candidate)))
    ;-IN-
  (food-check-create p (make-posn (random MAX) (random MAX)))))

 
; Posn -> Boolean
; use for testing only 
(define (not-equal-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
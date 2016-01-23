#lang planet neil/sicp

; I drew it out and figured out how it worked, more or less.
; it wasn't obvious what v would be, but I think I get it. Oh yeah, it's
; because the first thing that happens to x is it gets its cdr set to '()
; and then all the action happens on temp.

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

(define w (mystery v))

v

w
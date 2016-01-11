;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |415|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (gcd-structural n m)
  (local (; N -> N
          ; determines the greatest divisor of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else (if (= (remainder n i) (remainder m i) 0)
                        i
                        (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd large small) == (gcd small (remainder large small)) 
          (define (clever-gcd large small)
            (cond
              [(= small 0) large]
              [else (clever-gcd small (remainder large small))])))
    (clever-gcd (max m n) (min m n))))

(time (gcd-structural 101135853 45014640))
(time (gcd-generative 101135853 45014640))
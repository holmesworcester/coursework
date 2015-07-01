;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname drawing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(define sky (rectangle 100 100 "solid" (color 56 189 255)))
(define sun (circle 20 "solid" "yellow"))
(define setting (place-image sun 100 0 sky))

;general car pieces
(define wheel (circle 10 "solid" "black"))
(define body (rectangle 70 20 "solid" "red"))
(define roof (ellipse 40 25 "solid" "black"))

;general boat pieces
(define hull (polygon (list (make-posn 0 0)
                            (make-posn 20 20)
                            (make-posn 60 20)
                            (make-posn 80 0)) "solid" "blue"))
(define sail (triangle 45 "solid" "gray"))

;additive car parts
(define (add-body image) (place-image body 45 80 image))
(define (add-front-wheel image) (place-image wheel 28 90 image))
(define (add-rear-wheel image) (place-image wheel 62 90 image))
(define (add-roof image) (place-image roof 40 68 image))

;tree parts
(define (draw-trunk image) (place-image (rectangle 20 50 "solid" "brown") 50 75 image))
(define (draw-leaves image) (place-image (circle 35 "solid" "green") 50 25 image))

;putting it together
(define (draw-car image)(add-rear-wheel (add-front-wheel (add-body (add-roof image)))))
(define (draw-boat image)(place-image sail 50 60 (place-image hull 50 90 image)))


;let's make some stuff
(draw-car setting)
(draw-boat setting)
(draw-leaves(draw-trunk setting))




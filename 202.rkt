;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |202|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; constants

(define D 10) ; diameter or segment height and width. 
(define HEIGHT 50) ; height of world in worm segments. should be an even number.
(define WIDTH HEIGHT) ; width of world in worm segments. should be an even number.
(define WORM-SEGMENT (circle D "solid" "red"))
(define MT (empty-scene (* WIDTH D) (* HEIGHT D)))

(define GAMEOVER-TEXT "worm hit border")
(define FONT-SIZE 16)
(define FONT-COLOR "black")
(define TEXT-POSX (* (/ WIDTH 2) D))
(define TEXT-POSY (* (- HEIGHT (* D 2)) D))

; definitions

; A Direction is one of:
; - "up"
; - "down"
; - "left"
; - "right"
; interpretation: the direction my worm is currently moving in.

(define START-DIR "left")

; A WormPosn is (make-posn x y) where x and y are the distance, ** in worm segments ** from the top and left of the screen, where 0,0 is the top most position.
; interpretation: the position of the worm on the grid of possible worm positions.

(define START-POS (make-posn (/ WIDTH 2) (/ HEIGHT 2)))

; A WorldState is a structure (make-world WormPosn Direction) 
;  interpretation: currently, everything about the worm we are moving around that changes
  
(define-struct world [posn dir])
(define START-WORLD (make-world START-POS START-DIR))

; functions / wishlist

; Number -> Number
; turns something from a logical number into a physical number.

(define (logical->physical n)
  (+ (* n D) (/ D 2)))

; WormPosn, Image -> Image
; renders the worm segment at position p on background i

(define (render-segment p i)
  (place-image WORM-SEGMENT (logical->physical (posn-x p)) (logical->physical (posn-y p)) i))

; WorldState, Image -> Image
; renders the worm

(define (render-whole-worm w i)
  (render-segment (world-posn w) i))

; WorldState -> Image
; renders our world state as an image

(define (render w)
  (render-whole-worm w MT))

; Posn, Direction -> Posn
; updates the position of a segment one step, given its direction, by moving it 1 in that direction.

(check-expect (update-posn (make-posn 10 10) "left") (make-posn 9 10))
(check-expect (update-posn (make-posn 10 10) "right") (make-posn 11 10))
(check-expect (update-posn (make-posn 10 10) "down") (make-posn 10 11))
(check-expect (update-posn (make-posn 10 10) "up") (make-posn 10 9))

(define (update-posn p dir)
  (cond
    [(string=? dir "left") (make-posn (- (posn-x p) 1) (posn-y p))]
    [(string=? dir "right") (make-posn (+ (posn-x p) 1) (posn-y p))]
    [(string=? dir "down") (make-posn (posn-x p) (+ (posn-y p) 1))]
    [(string=? dir "up") (make-posn (posn-x p) (- (posn-y p) 1))]
    [else p]))

; WorldState -> WorldState
; moves the worm 1 in the direction dir

(check-expect (move-segment (make-world (make-posn 10 10) "left")) (make-world (make-posn 9 10) "left"))
(check-expect (move-segment (make-world (make-posn 10 10) "right")) (make-world (make-posn 11 10) "right"))
(check-expect (move-segment (make-world (make-posn 10 10) "down")) (make-world (make-posn 10 11) "down"))
(check-expect (move-segment (make-world (make-posn 10 10) "up")) (make-world (make-posn 10 9) "up"))

(define (move-segment w)
  (make-world (update-posn (world-posn w) (world-dir w)) (world-dir w)))


; WorldState, Direction -> WorldState
; update-direction updates the world state on key presses. currently, changes the direction of the worm.

(check-expect (update-direction (make-world (make-posn 10 10) "left") "right") (make-world (make-posn 10 10) "right"))
(check-expect (update-direction (make-world (make-posn 10 10) "right") "left") (make-world (make-posn 10 10) "left"))
(check-expect (update-direction (make-world (make-posn 10 10) "down") "up") (make-world (make-posn 10 10) "up"))
(check-expect (update-direction (make-world (make-posn 10 10) "up") "down") (make-world (make-posn 10 10) "down"))

(define (update-direction w ke)
  (make-world (world-posn w) ke))

; WorldState -> WorldState 
; updates the world state on each clock tick (moves the worm)

(define (tock w)
  (move-segment w))

; WorldState -> WorldState
; key updates the world state on key presses. currently, changes the direction of the worm.

(check-expect (key (make-world (make-posn 10 10) "left") "right") (make-world (make-posn 10 10) "right"))
(check-expect (key (make-world (make-posn 10 10) "right") "left") (make-world (make-posn 10 10) "left"))
(check-expect (key (make-world (make-posn 10 10) "down") "up") (make-world (make-posn 10 10) "up"))
(check-expect (key (make-world (make-posn 10 10) "up") "down") (make-world (make-posn 10 10) "down"))

(define (key w ke)
  (cond
    [(or (string=? ke "left") (string=? ke "right") (string=? ke "up") (string=? ke "down")) (update-direction w ke)]
    [else w]))

; WorldState -> Image
; renders the final message of the game

(define (game-over-message w)
  (place-image (text GAMEOVER-TEXT FONT-SIZE FONT-COLOR) TEXT-POSX TEXT-POSY (render w)))

; WorldState -> WorldState
; ends the game by asking if the game is over, given the position.

(define (stop? w)
         (game-over? (world-posn w)))

; WormPosn -> Boolean
; ends the game when worm-posn is in a wall.
         
(check-expect (game-over? (make-posn 10 10)) #false)
(check-expect (game-over? (make-posn 0 10)) #true) 
(check-expect (game-over? (make-posn 10 0)) #true)
(check-expect (game-over? (make-posn 10 HEIGHT)) #true) 
(check-expect (game-over? (make-posn WIDTH 10)) #true)

(define (game-over? p)
  (cond
    [(<= (posn-y p) 0) #true]
    [(<= (posn-x p) 0) #true]
    [(>= (posn-y p) HEIGHT) #true]
    [(>= (posn-x p) WIDTH) #true]
    [else #false]))

; Number -> WorldState
; our main function. takes in the "speed" or clock tick interval t, a number in seconds.

(define (worm-main t)
  (big-bang START-WORLD
            [on-tick tock t]
            [on-key key]
            [to-draw render]
            [stop-when stop? game-over-message]))

(worm-main .1)
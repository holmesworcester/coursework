;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |204|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; constants

(define D 20) ; diameter or segment height and width. 
(define HEIGHT 20) ; height of world in worm segments. should be an even number.
(define WIDTH HEIGHT) ; width of world in worm segments. should be an even number.
(define WORM-SEGMENT (circle (/ D 2) "solid" "red"))
(define MT (empty-scene (* WIDTH D) (* HEIGHT D)))
(define FOOD (circle (/ D 2) "solid" "orange"))


(define GAMEOVER-TEXT "worm hit border")
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")
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
(define START-POS-1D (make-posn (/ WIDTH 2) (+ (/ HEIGHT 2) 1)))
(define START-POS-1L (make-posn (- (/ WIDTH 2) 1) (/ HEIGHT 2)))

; a WholeWorm is one of:
; - (cons WormPosn '())
; - (cons WormPosn WholeWorm)
; interpretation: all of the worms segments, where there is always at least one.

(define START-WORM (list START-POS))
(define START-WORM-LEFT (list START-POS-1L))
(define SHORT-WORM (list START-POS START-POS-1D))
(define SHORT-WORM-LEFT (list START-POS-1L START-POS))

; A WormState is a structure (make-wormstate Worm Direction) 
;  interpretation: currently, everything about the worm we are moving around that changes
  
(define-struct wormstate [worm dir])

(define START-WORMSTATE (make-wormstate SHORT-WORM START-DIR))

; A WorldState is a structure (make-world WormState Posn)
; interpretation: the worm in all its glory (pieces, diretion) and the piece of food, a Posn on the same logical scale as WormPosn

(define-struct world2 [worm food])

(define START-FOOD (make-posn 3 3))
  
(define START-WORLD2 (make-world2 START-WORMSTATE START-FOOD))
(define START-WORLD2-WORMEATS (make-world2 START-WORMSTATE START-POS)) ; puts the food at the worms start position so it eats right away.

; functions / wishlist

; update tock so new food is created
; update tock so worm can eat food and grow
; update food-create so food never appears on the worm

; fix my stop-when function stop? so it shows the message.
; rename WholeWorm to LPos
; rename WormState to Worm
; rename "world2" structure to "world"

; WorldState -> WormState
; returns the WormState in a given WorldState (tells me just stuff about the worm, for functions that only care about the worm
; and are not affected by the food)

(define (worldstate->wormstate w)
  (wormstate-worm (world2-worm w)))

; Number -> Number
; turns something from a logical number into a physical number.

(define (logical->physical n)
  (+ (* n D) (/ D 2)))

; WormPosn, Image -> Image
; renders the worm segment at position p on background i

(define (render-segment p i)
  (place-image WORM-SEGMENT (logical->physical (posn-x p)) (logical->physical (posn-y p)) i))

; WholeWorm, Image -> Image
; renders the worm

(define (render-whole-worm w i)
  (cond
    [(empty? w) i]
    [else (render-segment (first w) (render-whole-worm (rest w) i))]))

; WorldState, Image -> Image
; renders the food

(define (render-food w i)
  (place-image FOOD (logical->physical (posn-x (world2-food w))) (logical->physical (posn-y (world2-food w))) i))

; WorldState -> Image
; renders our world state as an image

(define (render w)
  (render-whole-worm (worldstate->wormstate w) (render-food w MT))) 

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

; AnyList -> AnyList
; removes the last segment in the worm (the last item in the list)

(check-expect (remove-last '()) '())
(check-expect (remove-last (list 1)) '())
(check-expect (remove-last (list 1 2)) (list 1))

(define (remove-last l)
  (cond
    [(empty? l) '()]
    [(empty? (rest l)) '()]
    [else (cons (first l) (remove-last (rest l)))]))

; WholeWorm, Direction -> WholeWorm
; adds a segment in the direction the worm is moving in, and removes the last segment.

(check-expect (move-worm SHORT-WORM "left") SHORT-WORM-LEFT)
(check-expect (move-worm START-WORM "left") START-WORM-LEFT)

(define (move-worm ww dir)
  (cons (update-posn (first ww) dir) (remove-last ww))) ; adds a moved first onto the same list, but with the last one removed.

; WholeWorm, Direction -> WholeWorm
; adds a segment in the direction the worm is moving in, without removing the last segment.

(define (move-grow-worm ww dir)
  (cons (update-posn (first ww) dir) ww))

; WormState, Direction -> WormState
; update-direction updates the worm's direction.

(check-expect (update-direction (make-wormstate SHORT-WORM "left") "right") (make-wormstate SHORT-WORM "right"))
(check-expect (update-direction (make-wormstate SHORT-WORM "up") "left") (make-wormstate SHORT-WORM "left"))
(check-expect (update-direction (make-wormstate SHORT-WORM "left") "up") (make-wormstate SHORT-WORM "up"))
(check-expect (update-direction (make-wormstate SHORT-WORM "down") "down") (make-wormstate SHORT-WORM "down"))

(define (update-direction w ke)
  (make-wormstate (wormstate-worm w) ke))

; WorldState, Direction -> WorldState
; updates the world state with a direction, for key.

(check-expect (update-direction-2 (make-world2 (make-wormstate SHORT-WORM "left") FOOD) "left") (make-world2 (make-wormstate SHORT-WORM "left") FOOD))

(define (update-direction-2 w ke)
  (make-world2 (update-direction (world2-worm w) ke) (world2-food w)))

; WorldState -> Posn
; gives me the position of the worm's head

(check-expect (worm-head START-WORLD2) START-POS)

(define (worm-head w)
  (first (wormstate-worm (world2-worm w))))

; WorldState -> Boolean
; returns true when the worm head hits the food. otherwise, false.

(check-expect (worm-hit-food? START-WORLD2) #false)
(check-expect (worm-hit-food? START-WORLD2-WORMEATS) #true)

(define (worm-hit-food? w)
  (equal? (world2-food w) (worm-head w)))

; WormState -> WormState
; works just like move-worm, on a wormstate, but doesn't remove the tail. sits between worldstate and wholeworm.

(define (move-grow-wormstate w)
  (make-wormstate (move-grow-worm (wormstate-worm w) (wormstate-dir w)) (wormstate-dir w)))
  
; WorldState -> WorldState
; works just like move-worm, on a world, but doesn't remove the tail.

(check-expect (length (wormstate-worm (world2-worm (grow-worm START-WORLD2)))) (+ 1 (length (wormstate-worm (world2-worm START-WORLD2)))))

(define (grow-worm w)
  (make-world2 (move-grow-wormstate (world2-worm w)) (world2-food w)))

; Posn -> Posn 
; Creates a new position for the food that is not the given position.
 
(check-satisfied (food-create (make-posn 1 1)) not-equal-1-1?)
 
(define (food-create p)
  (food-check-create p (make-posn (random WIDTH) (random HEIGHT))))
 
; Posn, Posn -> Posn 
; generative recursion 
; Compares two positions and if they are equal, calls food-create to try again to get one that is not equal.

(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Posn -> Boolean
; use for testing only 
(define (not-equal-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

; WorldState -> WorldState
; changes the food's position to something random that is not its current position.

(define (regenerate-food w)
  (make-world2 (world2-worm w) (food-create (world2-food w))))

; WorldState -> WorldState
; Makes all updates for when the worm eats (encounters food) Worm grows, and food is created at a new position.

(define (worm-eats w)
  (regenerate-food (grow-worm w)))

; WorldState -> WorldState
; updates the worm's position on each clock tick (moves the worm in the worm's direction)

(define (tock w)
  (cond
    [(worm-hit-food? w) (worm-eats w)] ; the worm eats and grows (and food is regenerated)
    [else (make-world2 (move-worm2 (world2-worm w)) (world2-food w))])) ; else the worm moves as normal

; WormState -> WormState 
; moves the worm in the direction it's traveling in

(define (move-worm2 w)
   (make-wormstate (move-worm (wormstate-worm w) (wormstate-dir w)) (wormstate-dir w))) ; passes the worm and the direction, makes a new wormstate with the old direction.

; WorldState -> WorldState
; key updates the world state on key presses. currently, changes the direction of the worm.

(define (key w ke)
  (cond
    [(or (string=? ke "left") (string=? ke "right") (string=? ke "up") (string=? ke "down")) (update-direction-2 w ke)]
    [else w]))

; WorldState -> Image
; renders the final message of the game, depending on the end state.

(define (game-over-message w)
  (cond
    [(hit-self? (worldstate->wormstate w)) (show-message w "the worm hit itself")]
    [(hit-wall? (first (worldstate->wormstate w))) (show-message w "the worm hit the wall")]
    [else (show-message w "something went wrong")]))

; WorldState, String -> Image

(define (show-message w t)
  (place-image (text t TEXT-SIZE TEXT-COLOR) TEXT-POSX TEXT-POSY (render w)))

; WormState -> Boolean
; tells me if the worm has hit itself, that is, if the first position is the same as any other position in the list.

(check-expect (hit-self? (list (make-posn 10 10) (make-posn 9 10) (make-posn 10 10))) #true)
(check-expect (hit-self? (list (make-posn 10 10) (make-posn 9 10) (make-posn 8 10))) #false)

(define (hit-self? w)
  (member? (first w) (rest w)))

; WormState -> Boolean
; ends the game by asking if the game is over, given the position.

(check-expect (stop? START-WORLD2) false)

(define (stop? w)
         (or (hit-self? (worldstate->wormstate w)) (hit-wall? (first (worldstate->wormstate w))))) ; write more tests!

; WormPosn -> Boolean
; ends the game when worm-posn is in a wall.
         
(check-expect (hit-wall? (make-posn 10 10)) #false)
(check-expect (hit-wall? (make-posn 0 10)) #true) 
(check-expect (hit-wall? (make-posn 10 0)) #true)
(check-expect (hit-wall? (make-posn 10 HEIGHT)) #true) 
(check-expect (hit-wall? (make-posn WIDTH 10)) #true)

(define (hit-wall? p)
  (cond
    [(<= (posn-y p) 0) #true]
    [(<= (posn-x p) 0) #true]
    [(>= (posn-y p) HEIGHT) #true]
    [(>= (posn-x p) WIDTH) #true]
    [else #false]))

; Number -> WorldState
; our main function. takes in the "speed" or clock tick interval t, a number in seconds.

(define (worm-main t)
  (big-bang START-WORLD2
            [on-tick tock t]
            [on-key key]
            [to-draw render]
            [stop-when stop? game-over-message]))

(worm-main .2)
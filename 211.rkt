;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |211|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; todo
; make a way for fire to spread more naturally
; make a way to win by putting out all fires
; make a way to run out of water, and to see how much water you have


(require 2htdp/image)
(require 2htdp/universe)

; physical constants

(define HEIGHT 200)
(define WIDTH HEIGHT)
(define LAND (rectangle HEIGHT WIDTH "solid" "beige"))
(define PLANE (triangle 10 "solid" "red"))
(define GRID 10)
(define FIRE (rectangle GRID GRID "solid" "orange"))
(define TOTAL-PATCHES 10)
(define WATER (rectangle (* GRID 1.5) (* GRID 1.5) "solid" "blue"))

(define PLANE-SPEED GRID)

(define LEFT (make-posn (* PLANE-SPEED -1)  0))
(define RIGHT (make-posn PLANE-SPEED 0))
(define UP (make-posn 0 (* PLANE-SPEED -1)))
(define DOWN (make-posn 0 PLANE-SPEED))

(define R GRID) ; for in-reach? function, defines closeness for water putting out a fire

; data definitions

; Plane is a Posn (make-posn x y)
; interpretation: the position of the airplane

(define start-plane (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
(define out-of-bounds-top-left (make-posn -10 -10))
(define plane-top-left (make-posn 0 0))
  
; FirePatch is a Posn (make-posn x y)
; interpretation: the position of a single patch of fire

(define fire-under-start-plane (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
(define fire-top-left (make-posn 0 0))

; Fire is a list-of-positions, or one of:
; - '()
; - (cons Posn Fire)
; interpretation: the location of all patches of fire, where each patch is a position

(define no-fire '())
(define some-fire (list fire-under-start-plane fire-top-left))

; WaterPatch is a Posn (make-posn x y)
; interpretation: the position of a single patch of water

(define water-under-start-plane (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
(define water-top-left (make-posn 0 0))

; Water is a list-of-positions, or one of:
; - '()
; - (cons Posn Water)
; interpretation: the location of all patches of water, where each patch is a position

(define no-water '())
(define some-water (list water-under-start-plane water-top-left))

; Time is a Number
; interpretation: the number of clock ticks since the start of the game.

; Fworld is a structure (make-fworld Plane Fire Water Time)
; interpretation: the world state of the firefighting game

(define-struct fworld [plane fire water time])

; template: (make-fworld (fworld-plane fw) (fworld-fire fw) (fworld-water fw) (fworld-time fw))

(define start-game (make-fworld start-plane some-fire no-water 0))

; functions

; Number -> Fworld
; main takes in a number (time for each clock tick) and must generate new fires, render the world, move the plane, and let me extinguish fires

(define (main speed)
  (big-bang start-game
            [on-tick tock speed]
            [on-key kh]
            [to-draw render-fworld]))

; Fworld, KeyEvent -> Fworld
; handles all key events, (moving the plane and dropping water)

(define (kh fw ke)
  (cond
    [(string=? "left" ke) (move-plane fw LEFT)]
    [(string=? "right" ke) (move-plane fw RIGHT)]
    [(string=? "up" ke) (move-plane fw UP)]
    [(string=? "down" ke) (move-plane fw DOWN)]
    [(string=? " " ke) (drop-water fw)]
    [else fw]))

; Fworld -> Fworld
; drops water in the position of the plane

(check-expect (first (fworld-water (drop-water start-game))) (fworld-plane start-game))

(define (drop-water fw)
  (make-fworld (fworld-plane fw) (fworld-fire fw) (cons (fworld-plane fw) (fworld-water fw)) (fworld-time fw)))

; Fworld -> Fworld
; moves the plane by adding a Posn dir to it

(check-expect (posn-x (fworld-plane (move-plane start-game RIGHT))) (+ (posn-x start-plane) PLANE-SPEED))
(check-expect (posn-x (fworld-plane (move-plane start-game LEFT))) (- (posn-x start-plane) PLANE-SPEED))
(check-expect (posn-y (fworld-plane (move-plane start-game UP))) (- (posn-y start-plane) PLANE-SPEED))
(check-expect (posn-y (fworld-plane (move-plane start-game DOWN))) (+ (posn-y start-plane) PLANE-SPEED))

(define (move-plane fw dir)
  (make-fworld (bounding-box 0 WIDTH 0 HEIGHT (add-posn (fworld-plane fw) dir)) (fworld-fire fw) (fworld-water fw) (fworld-time fw)))

; Number, Number, Number, Number, Posn -> Posn
; (bounding-box p xmin xmax ymin ymax) keeps 

(check-expect (bounding-box 0 WIDTH 0 HEIGHT out-of-bounds-top-left) plane-top-left)

(define (bounding-box xmin xmax ymin ymax p)
  (make-posn (limit (posn-x p) xmin xmax) (limit (posn-y p) ymin ymax)))

; Number, Min, Max -> Number
; forces a number to stay within the range (min, max)

(check-expect (limit -2 0 10) 0)
(check-expect (limit 22 0 10) 10)
(check-expect (limit 4 0 10) 4)

(define (limit x min max)
  (cond
    [(< x min) min]
    [(> x max) max]
    [else x]))

; Posn, Posn -> Posn
; adds two positions together

(check-expect (add-posn (make-posn 5 0) (make-posn 0 1)) (make-posn 5 1))

(define (add-posn p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

; Fworld -> Fworld
; tock updates everything, moves the plane, generates new fires, etc.

(define (tock fw)
  (water-dries-up (water-puts-out-fire (generate-fire fw))))

; Fworld -> Fworld
; removes the last water item from the Water list

(define (water-dries-up fw)
  (make-fworld (fworld-plane fw) (fworld-fire fw) (remove-last (fworld-water fw)) (fworld-time fw)))

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

; Fworld -> Fworld
; removes a patch of fire whenever it is in reach of a place where water has been dropped.

(check-expect (water-puts-out-fire start-game) start-game) ; no water yet

(define (water-puts-out-fire fw)
  (make-fworld (fworld-plane fw) (eliminate-close-positions (fworld-fire fw) (fworld-water fw)) (fworld-water fw) (fworld-time fw)))

; List-of-Positions, List-of-Positions -> List-of-Positions
; if any water drop in Water is in-reach of a fire patch, it puts it out, that is it removes it from the list Fire.
; remember that for now both Water and Fire are List-of-Position's

(check-expect (eliminate-close-positions some-fire some-fire) '())
(check-expect (eliminate-close-positions some-fire '()) some-fire)
(check-expect (eliminate-close-positions '() some-fire) '())
; maybe more tests about closeness if that's an issue

(define (eliminate-close-positions lop-eliminated lop-eliminator)
  (cond
    [(empty? lop-eliminated) '()]
    [(empty? lop-eliminator) lop-eliminated]
    [else (cond
            [(in-reach-any? (first lop-eliminated) lop-eliminator) (eliminate-close-positions (rest lop-eliminated) lop-eliminator)]
            [else (cons (first lop-eliminated) (eliminate-close-positions (rest lop-eliminated) lop-eliminator))])]))

; Posn, List-of-Positions -> Boolean
; tells us if the Posn p is in-reach? of any of the Posns in List-of-Positions lop
                                          
(define (in-reach-any? p lop)
  (cond
    [(empty? lop) #false]
    [(in-reach? (distance p (first lop))) #true]
    [else (in-reach-any? p (rest lop))]))

; Posn, Posn -> Posn (where posn-x and posn-y are both greater than 0)
; distance determines the distance between two positions, p1 and p2 and returns a special MissileOrNot.
; in which both posn-x and posn-y are greater than zero (using absolute value).

(check-expect (distance (make-posn 5 0) (make-posn 0 0)) (make-posn 5 0))
(check-expect (distance (make-posn 0 5) (make-posn 0 0)) (make-posn 0 5))
(check-expect (distance (make-posn 3 4) (make-posn 0 0)) (make-posn 3 4))
(check-expect (distance (make-posn -5 0) (make-posn 0 0)) (make-posn 5 0))
(check-expect (distance (make-posn 10 15) (make-posn 10 10)) (make-posn 0 5))
(check-expect (distance (make-posn -5 -5) (make-posn 0 0)) (make-posn 5 5))

(define (distance loc1 loc2)
  (make-posn (abs (- (posn-x loc1) (posn-x loc2))) (abs (- (posn-y loc1) (posn-y loc2)))))

; Location -> Boolean
; determines whether a location's distance to the origin is strictly less than some constant R
; since a Location can be a MissileOrNot (false or a Posn) or a Number we take these three cases into account

(check-expect (in-reach? (make-posn R R)) false)
(check-expect (in-reach? (make-posn (/ R 2) (/ R 2))) true)
(check-expect (in-reach? (make-posn 0 R)) false)
(check-expect (in-reach? (make-posn 0 0)) true)

(define (in-reach? loc)
  (< (sqrt (+ (sqr (posn-x loc)) (sqr (posn-y loc)))) R))

; Fworld -> Fworld
; makes fire until there are TOTAL-PATCHES patches of fire
; tests downstream

(define (generate-fire fw)
  (make-fworld (fworld-plane fw) (more-fire (fworld-fire fw)) (fworld-water fw) (fworld-time fw)))

; Fire -> Fire
; makes more fire patches if there's less than TOTAL-PATCHES patches of fire, otherwise returns the list it's given.

(check-expect (length (more-fire some-fire)) (+ 1 (length some-fire)))
(check-expect (more-fire (list 1 2 3 4 5 6 7 8 9 10 11)) (list 1 2 3 4 5 6 7 8 9 10 11)) ; just to check it doesn't add anything if the list is already longer than TOTAL-PATCHES (10)

(define (more-fire lpos)
  (if (< (length lpos) TOTAL-PATCHES) (cons (random-fire WIDTH HEIGHT) lpos) lpos))

; Number, Number -> Fire
; creates a fire at a random position determined by w-max and h-max, the max width and height respectively

(check-random (random-fire WIDTH HEIGHT) (make-posn (random WIDTH) (random HEIGHT))) ; i gotta get used to using this

(define (random-fire w-max h-max)
  (make-posn (random w-max) (random h-max)))

; Fworld, Image -> Image
; render draws my world

(define (render-fworld fw)
  (render-plane fw (render-water fw (render-fire fw LAND))))

; Fworld, Image -> Image
; renders the plane

(define (render-plane fw i)
  (place-image PLANE (posn-x (fworld-plane fw)) (posn-y (fworld-plane fw)) i))

; Fworld, Image -> Image
; renders all water in a world

(define (render-water fw i)
  (render-drops (fworld-water fw) i))

; Fire, Image -> Image
; renders all water drops on an image i.

(define (render-drops lpos i)
  (cond
    [(empty? lpos) i]
    [else (place-image WATER (posn-x (first lpos)) (posn-y (first lpos)) (render-drops (rest lpos) i))]))


; Fworld, Image -> Image
; renders all fire in a world

(define (render-fire fw i)
  (render-patches (fworld-fire fw) i))

; Fire, Image -> Image
; renders all fire patches on an image i.

(define (render-patches lpos i)
  (cond
    [(empty? lpos) i]
    [else (place-image FIRE (posn-x (first lpos)) (posn-y (first lpos)) (render-patches (rest lpos) i))]))

(main 1)
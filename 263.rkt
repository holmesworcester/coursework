;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |263|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants

;world
(define WIDTH 200)
(define HEIGHT 200)
(define MIDDLE (/ WIDTH 2))
(define LAND-HEIGHT 20)
(define SKY (rectangle WIDTH HEIGHT "solid" "blue"))
(define LAND (rectangle WIDTH LAND-HEIGHT "solid" "green"))
(define BACKGROUND (place-image LAND (/ WIDTH 2) (- HEIGHT (/ LAND-HEIGHT 2)) SKY))

;misc game aspects

(define WIN-TEXT "GAME OVER: YOU WIN!")
(define LOSE-TEXT "GAME OVER: YOU LOSE!")
(define FONT-SIZE 14)
(define FONT-COLOR "white")

(define R 10) ;The global hit radius of objects... how close objects can get measured from their "centers" or their point if one dimensional.

;stuff

(define TANK (rectangle 30 20 "solid" "gray"))
(define TANK-Y (- HEIGHT (image-height LAND)))
(define TANK-SPEED 3)

(define MISSILE (rectangle 2 6 "solid" "orange"))
(define MISSILE-SPEED 10)
(define MISSILE-VEL (make-posn 0 (* MISSILE-SPEED -1)))
(define MISSILE-STARTY (- TANK-Y (/ (image-height MISSILE) 2)))

(define BOMB (rectangle 2 4 "solid" "white"))
(define BOMBRATE 20) ; how often bomb drops, in a probability 1/x for each clock tick
(define BOMB-SPEED 5)
(define BOMB-VEL (make-posn 0 BOMB-SPEED))

(define UFO-HEIGHT 7)
(define UFO-WIDTH 30)
(define UFO (overlay (circle UFO-HEIGHT "solid" "red") (rectangle UFO-WIDTH 4 "solid" "red")))
(define UFO-SPEED 2)
(define UFO-VEL (make-posn 0 UFO-SPEED))
(define UFORANDOM 10)

; Data Definitions

; TankPosition is a Number in (0,WIDTH)
; interpretation: the position of the Tank on the x-axis
; examples:

(define TANK-MIDDLE MIDDLE)
(define TANK-LEFT 0)
(define TANK-RIGHT WIDTH)

; Velocity is a Number.
; interpretation: the speed and direction of the Tank (positive is to the right, negative to the left)

; TankPosition is an Integer in (0, WIDTH)
; interpretation: the position of the Tank along the x axis.

; Tank is a structure (make-tank TankPosition Velocity)

(define-struct tank (x vel))

; examples of Tank:
(define tank-l-l (make-tank 0 (* TANK-SPEED -1)))
(define tank-l-r (make-tank 0 TANK-SPEED))
(define tank-r-r (make-tank WIDTH TANK-SPEED))
(define tank-m-r (make-tank MIDDLE TANK-SPEED))
(define tank-m-l (make-tank MIDDLE (* TANK-SPEED -1)))
(define tank-m-0 (make-tank MIDDLE 0))

; UFO is a Position (make-posn x y) 
; interpretation: the x,y coordinates of a UFO
; examples of UFO: 

(define ufostart (make-posn MIDDLE 0))
(define ufodown1 (make-posn MIDDLE UFO-SPEED))
(define ufohalfway (make-posn MIDDLE (/ HEIGHT 2)))
(define ufoplanet (make-posn MIDDLE (- HEIGHT LAND-HEIGHT)))

; A List-of-Positions is one of:
; - '()
; - (cons Posn List-of-Positions)
; interpretation: a list of positions, or for now, all the missiles.

(define missilestart (list (make-posn MIDDLE MISSILE-STARTY)))
(define missileupone (list (make-posn MIDDLE (- MISSILE-STARTY MISSILE-SPEED))))
(define missileend (list (make-posn MIDDLE 0))) ; could be nicer than 0 but that's fine for now
(define missilealmostend (list (make-posn MIDDLE -1)))

(define missilemid (list (make-posn MIDDLE (/ HEIGHT 2)))) ; a Missile halfway through its life.
(define missile-close-hit (list (make-posn (+ MIDDLE (/ R 2)) (/ HEIGHT 2)))) ; should be a hit, since (/ R 2) is less than R.
(define missile-close-miss (list (make-posn (+ MIDDLE R) (/ HEIGHT 2)))) ; should be a miss, since in-reach? requires "closeness" to be strictly less than R
(define two-missile-starts (list (make-posn MIDDLE MISSILE-STARTY) (make-posn MIDDLE MISSILE-STARTY)))
(define two-missile-upones (list (make-posn MIDDLE (- MISSILE-STARTY MISSILE-SPEED)) (make-posn MIDDLE (- MISSILE-STARTY MISSILE-SPEED))))
(define missilenot '()) ; nothing fired yet.

; Bomb is a [List-of Posn]
; interpretation: the bombs dropped by UFOs on the tank.

(define bombstart (list ufostart))
(define bombplanet (list ufoplanet))
(define bombhittankstart (list (make-posn (tank-x tank-m-0) TANK-Y)))
(define bombnone '())

; Location is one of:
; – MissileOrNot
; – Number
; interpretation Posn are positions on the Cartesian plane,
; Numbers are positions on either the x- or the y-axis.
; False means the object doesn't exist at all.
; Note: this definition helps me think about the function in-reach?


(define-struct sigs [bomb tank ufo missile])

; SIGS
; is (make-sigs Bomb Tank UFO MissileOrNot)
; interpretation represents the state of the space invader game 
; Examples: (aim is the shorthand for situations with no missile, and fired is the shorthand for situations with a missile)

(define aimstart (make-sigs bombnone tank-m-0 ufostart missilenot))
(define aimufodown1 (make-sigs bombnone tank-m-0 ufodown1 missilenot))
(define aimok (make-sigs bombnone tank-r-r ufostart missilenot))
(define aimlose (make-sigs bombnone tank-l-l ufoplanet missilenot))
(define aimhalfway (make-sigs bombnone tank-l-l ufohalfway missilenot))

(define firedstart (make-sigs bombnone tank-m-0 ufostart missilestart))
(define firedupone (make-sigs bombnone tank-m-0 ufostart missileupone))
(define firedmiss (make-sigs bombnone tank-m-r ufohalfway missileend))
(define fireddirecthit (make-sigs bombnone tank-m-r ufohalfway (list ufohalfway))) ;hit is totally direct (missile and ufo have the same position ufohalfway
(define firedclosehit (make-sigs bombnone tank-m-r ufohalfway missile-close-hit))
(define firedclosemiss (make-sigs bombnone tank-m-r ufohalfway missile-close-miss))
(define firedend (make-sigs bombnone tank-l-l ufohalfway missilealmostend))

(define sigsbombstart (make-sigs bombstart tank-m-0 ufostart missilenot))
(define sigsbombtank (make-sigs bombhittankstart tank-m-0 ufostart missilenot))
(define sigsbombplanet (make-sigs bombplanet tank-l-l ufostart missilenot))



(define sigshit fireddirecthit) 
(define sigsfire firedstart)
(define sigsstart aimstart)
(define sigslose aimlose)


; FUNCTIONS

; SIGS -> SIGS
; main function calls big-bang ontick, todraw, and onkey 
; to draw the world, handle key events, and move my objects on clock ticks

(define (main s)
  (big-bang s
            [to-draw si-render]
            [on-tick si-move]
            [on-key si-control]
            [stop-when si-game-over? si-render-final]))


; SIGS -> Image 
; renders the given game state and added it to BACKGROUND

(define (si-render s)
  (bomb-render (sigs-bomb s)
   (tank-render (sigs-tank s)
               (ufo-render (sigs-ufo s)
                            (missile-render (sigs-missile s)
                                               BACKGROUND)))))

; SIGS -> SIGS
; si-move moves Tank, UFO, Bombs and Missiles in the ways that they are pre-determined to on each clock tick.
; this is just a composing function, so I'm putting all the tests in the sub functions.
; if problems make more tests here!!!
; (check-expect (si-move aimstart) (make-sigs (sigs-tank aimstart) ufodown1 false)) can't do this test because it's random.
(check-expect (> (posn-y (sigs-ufo (si-move aimstart))) (posn-y (sigs-ufo aimstart))) true) ; make sure UFO moves down.
(check-expect (- (posn-y (sigs-ufo (si-move aimstart))) (posn-y (sigs-ufo aimstart))) UFO-SPEED) ; make sure it moves it down by UFO-SPEED 

(define (si-move s)
  (tank-move (missile-move (bombs-move (random-bomb-drop (ufo-move s))))))

; SIGS -> SIGS
; si-control moves the tank left and right when user hits the respective arrow keys, and launches a Missile when spacebar is pressed.

(check-expect (si-control aimstart " ") firedstart) ; make sure space launches the missile
(check-expect (tank-vel (sigs-tank (si-control aimstart "right"))) TANK-SPEED) ; is this a valid way to write tests?
(check-expect (tank-vel (sigs-tank (si-control aimstart "left"))) (* TANK-SPEED -1))

(define (si-control s ke)
  (cond
    [(string=? ke " ")(fire s)] ; fires the missile
    [(string=? ke "left")(update-tank-vel s (* TANK-SPEED -1))] ;
    [(string=? ke "right")(update-tank-vel s TANK-SPEED)]
    [else s]))

; SIGS -> Boolean
; si-game-over? returns true when the game is over, whether the player won or lost,
; that is, either when the Missile hits the UFO or when the UFO hits ("lands on") the planet.

(check-expect (si-game-over? aimstart) false)
(check-expect (si-game-over? aimufodown1) false)
(check-expect (si-game-over? aimok) false)
(check-expect (si-game-over? aimlose) true)
(check-expect (si-game-over? firedstart) false)
(check-expect (si-game-over? firedmiss) false)
(check-expect (si-game-over? firedclosemiss) false)
(check-expect (si-game-over? firedclosehit) true)
(check-expect (si-game-over? fireddirecthit) true)

(define (si-game-over? s)
  (or (game-over-lose? s) (game-over-win? s)))


; SIGS -> SIGS
; fire will fire a missile,
; by adding a Position to the List-of-Positions (sigs-missile) that has the same x value as tank (tank-x (sigs-tank))
; and the y value MISSILE-STARTY.

(check-expect (rest (sigs-missile (fire firedmiss))) (sigs-missile firedmiss)) ; make sure it adds a new thing to the list
(check-expect (fire aimstart) firedstart) ; make sure it changes the state
(check-expect (posn-x (first (sigs-missile (fire aimstart)))) (tank-x (sigs-tank aimstart))) ; make sure the x value is the same as the tank
(check-expect (posn-y (first (sigs-missile (fire aimstart)))) MISSILE-STARTY) ; make sure the y value is MISSILE-STARTY

(define (fire s)
  (make-sigs (sigs-bomb s) (sigs-tank s) (sigs-ufo s) (cons (make-posn (tank-x (sigs-tank s)) MISSILE-STARTY) (sigs-missile s)))) ; messy, but copy/pastes logic from previous iteration so it should be fine. 

; SIGS -> SIGS
; tank-move moves the tank on a clock tick, depending on direction and TANK-SPEED.

(define (tank-move s)
  (make-sigs (sigs-bomb s) (tank-step (sigs-tank s)) (sigs-ufo s) (sigs-missile s)))

; Tank -> Tank
; tank-step moves the tank t (tank-vel t)

(check-expect (tank-step (make-tank 100 -2)) (make-tank 98 -2))
(check-expect (tank-step (make-tank 100 2)) (make-tank 102 2))
(check-expect (tank-step (make-tank WIDTH 2)) (make-tank WIDTH 2))
(check-expect (tank-step (make-tank 0 -2)) (make-tank 0 -2))

(define (tank-step t)
  (make-tank (x-limit (+ (tank-x t) (tank-vel t))) (tank-vel t)))

; SIGS, Number -> SIGS
; update-tank-vel updates the velocity of the SIGS s to the Number v

(check-expect (update-tank-vel aimstart TANK-SPEED) (make-sigs bombnone tank-m-r ufostart missilenot))
(check-expect (update-tank-vel aimstart (* TANK-SPEED -1)) (make-sigs bombnone tank-m-l ufostart missilenot))
(check-expect (update-tank-vel (make-sigs bombnone tank-m-l ufostart missilenot) TANK-SPEED) (make-sigs bombnone tank-m-r ufostart missilenot))

(define (update-tank-vel s v)
  (make-sigs (sigs-bomb s) (make-tank (tank-x (sigs-tank s)) v) (sigs-ufo s) (sigs-missile s))) ; leaves everything except the tank-vel which it updates.

; Number -> Number
; x-limit keeps the x coordinate of objects inside (0, WIDTH) when they move.

(check-expect (x-limit -5) 0)
(check-expect (x-limit 0) 0)
(check-expect (x-limit 1) 1)
(check-expect (x-limit WIDTH) WIDTH)
(check-expect (x-limit (+ WIDTH 5)) WIDTH)
(check-expect (x-limit (- WIDTH 1)) (- WIDTH 1))

(define (x-limit x)
  (cond
    [(< x 0) 0]
    [(> x WIDTH) WIDTH]
    [else x]))

; SIGS -> SIGS
; ufo-move moves the tank on a clock tick, down the screen and randomly to the left and right.
; this another composing function so I'm going to put tests in sub-functions.
; also, I don't know how to write tests for a function that involves a random

(define (ufo-move s)
  (ufo-random (ufo-down s)))

; SIGS -> SIGS
; ufo-random moves the ufo in SIGS s in "small random jumps to the left or right"
; note that it doesn't call the random function.

(define (ufo-random s)
  (make-sigs (sigs-bomb s) (sigs-tank s) (ufo-jump (sigs-ufo s)) (sigs-missile s))) 

; Position, Number -> Position
; xmove moves a Posn pos (e.g. a UFO or a Tank) left or right by adding a positive or negative number to (posn-x pos)
; xmove uses limitx to keep x inside the range (0, WIDTH)
; I could use this for my tank moving function at some point.

(check-expect (xmove (make-posn 100 100) 10) (make-posn 110 100))
(check-expect (xmove (make-posn 100 100) -10) (make-posn 90 100))
(check-expect (xmove (make-posn 0 100) -10) (make-posn 0 100))
(check-expect (xmove (make-posn WIDTH 100) 10) (make-posn WIDTH 100))

(define (xmove pos n)
  (make-posn (x-limit (+ n (posn-x pos))) (posn-y pos)))

; Posn -> Posn
; ufo-jump moves a Posn (UFO) u randomly left or right by adding a number in [-UFORANDOM,UFORANDOM] to it.
; I *could* make a random movement function.

(define (ufo-jump u)
  (xmove u (- (random (+ (* UFORANDOM 2) 1)) UFORANDOM))) ;this looks complicated, but it's just to take the range of random positive integers and center it around zero.

; SIGS -> SIGS
; drops a bomb by adding a Posn (a single bomb) with the same position as the ufo, to the list

(check-expect (bomb-drop aimstart) (make-sigs (list (sigs-ufo aimstart)) (sigs-tank aimstart) (sigs-ufo aimstart) (sigs-missile aimstart))) 
(check-expect (rest (sigs-bomb (bomb-drop (bomb-drop aimstart)))) (sigs-bomb (bomb-drop aimstart)))

(define (bomb-drop s)
  (make-sigs (cons (sigs-ufo s) (sigs-bomb s)) (sigs-tank s) (sigs-ufo s) (sigs-missile s)))

; SIGS -> SIGS
; randomly decides to drop a bomb or not on every clock tick based on probability 1/BOMBRATE.
; if this is the time to drop a bomb, it calls bomb-drop

(define (random-bomb-drop s)
  (if (= 1 (random BOMBRATE)) (bomb-drop s) s))

; [List-of Posn], Image, Image -> Image
; renders a list of similar things on a background, given an image for the thing.

(check-expect (lop-render (sigs-missile firedstart) MISSILE BACKGROUND) (missile-render (sigs-missile firedstart) BACKGROUND))
(check-expect (lop-render (sigs-bomb sigsbombstart) BOMB BACKGROUND) (bomb-render (sigs-bomb sigsbombstart) BACKGROUND))

(define (lop-render l image background)
    (foldr (lambda (p b) (draw-one-posn image p b)) background l))

; Posn, Image, Image -> Image
; draws an image image at Posn p on a background.

(define (draw-one-posn image p background)
  (place-image image (posn-x p) (posn-y p) background))

; [List-of Posn], Image -> Image
; renders the bombs the UFO drops on a background i.

(define (bomb-render b i)
  (lop-render b BOMB i))

; SIGS -> SIGS
; ufo-down moves the ufo in SIGS s down the screen at a regular pace at a speed from constants.

(check-expect (ufo-down aimstart) aimufodown1)

(define (ufo-down s)
  (make-sigs (sigs-bomb s) (sigs-tank s) (move-posn (sigs-ufo s) UFO-VEL) (sigs-missile s)))

; Posn, Posn -> Posn
; adds a posn p to another position (velocity) vel

(check-expect (move-posn (make-posn 1 1) (make-posn 2 1)) (make-posn 3 2))

(define (move-posn p vel)
  (make-posn (+ (posn-x p) (posn-x vel)) (+ (posn-y p) (posn-y vel))))

; SIGS -> SIGS
; missile-move moves all missiles on a clock tick.
; if the Missile moves off the screen missile-move removes it from the list.

(check-expect (missile-move firedstart) firedupone)
(check-expect (missile-move firedend) aimhalfway)
(check-expect (missile-move aimstart) aimstart)
; perhaps add another test here for multiple missles.

(define (missile-move s)
    (make-sigs (sigs-bomb s) (sigs-tank s) (sigs-ufo s) (move-lpos (lpos-limit (sigs-missile s)) MISSILE-VEL))) ; uses move-lpos to move all the missiles MISSILE-VEL


; List-of-Positions -> List-of-Positions
; takes a List-of-Positions m and returns removes all positions that are off the screen.

(check-expect (lpos-limit '()) '())
(check-expect (lpos-limit (list (make-posn 10 10))) (list (make-posn 10 10)))
(check-expect (lpos-limit (list (make-posn 10 -1))) '())

(define (lpos-limit m)
  (filter on-screen? m))

; Posn -> Boolean
; tells me if a posn p is on the screen defined by WIDTH, HEIGHT

(check-expect (on-screen? (make-posn (* HEIGHT 2) (/ WIDTH 2))) #false)

(define (on-screen? p)
  (and (<= 0 (posn-x p) WIDTH) (<= 0 (posn-y p) HEIGHT)))

; SIGS -> SIGS
; moves all the bombs down the screen

(define (bombs-move s)
  (make-sigs (lpos-limit (move-lpos (sigs-bomb s) BOMB-VEL)) (sigs-tank s) (sigs-ufo s) (sigs-missile s))) ; moves all the bombs BOMB-VEL keeping them within the limits of the screen

; List-of-Positions, Posn -> List-of-Positions
; moves a list of Posn a given velocity.

(check-expect (move-lpos (list (make-posn 100 100)) MISSILE-VEL) (list (make-posn 100 (- 100 MISSILE-SPEED))))
(check-expect (move-lpos '() MISSILE-VEL) '())

(define (move-lpos lpos vel)    
  (map (lambda (p) (move-posn p vel)) lpos))

; SIGS -> Image
; si-render-final renders the final screen of the game when the game ends.
; it should say "game over: you win" if the Missile hits the UFO.
; or "game over: you lose" if the Missile hits the planet.

(define (si-render-final s)
  (cond
    [(game-over-lose? s)(text-render LOSE-TEXT (si-render s))]
    [(game-over-win? s)(text-render WIN-TEXT (si-render s))]))

; String, Image -> Image
; text-render overlays the text for a given string s in constant font and color
; on a given image i.

(define (text-render s i)
  (overlay (text s FONT-SIZE FONT-COLOR) i))

; SIGS -> Boolean
; game-over-lose? tells me if the player has lost, that is, if the UFO has hit the planet for a given SIGS s.

(check-expect (game-over-lose? fireddirecthit) false)
(check-expect (game-over-lose? firedclosehit) false)
(check-expect (game-over-lose? aimstart) false)
(check-expect (game-over-lose? aimok) false)
(check-expect (game-over-lose? aimlose) true)
(check-expect (game-over-lose? firedstart) false)
(check-expect (game-over-lose? firedmiss) false)
(check-expect (game-over-lose? sigsbombtank) #true)
               
(define (game-over-lose? s)
  (or (ufo-landed? (sigs-ufo s)) (posn-hit-lpos? (make-posn (tank-x (sigs-tank s)) TANK-Y) (sigs-bomb s))))

; UFO -> Boolean
; ufo-landed? tells me if a given UFO u has landed, given some constants about where land is.

(check-expect (ufo-landed? (sigs-ufo aimlose)) true)
(check-expect (ufo-landed? (sigs-ufo aimstart)) false)
(check-expect (ufo-landed? (sigs-ufo aimok)) false)

(define (ufo-landed? u)
  (in-reach? (- (- HEIGHT LAND-HEIGHT) (posn-y u))))

; SIGS -> Boolean
; game-over-win? tells me if the player has won, that is if the MissileOrNot has hit the UFO for a given SIGS s.

(check-expect (game-over-win? fireddirecthit) true)
(check-expect (game-over-win? aimstart) false)
(check-expect (game-over-win? aimok) false)
(check-expect (game-over-win? aimlose) false)
(check-expect (game-over-win? firedstart) false)
(check-expect (game-over-win? firedmiss) false)
(check-expect (game-over-win? firedclosemiss) false)
(check-expect (game-over-win? firedclosehit) true)
               
(define (game-over-win? s)
  (posn-hit-lpos? (sigs-ufo s) (sigs-missile s)))

; Posn, List-of-Positions -> Boolean
; tells us if any one of the missles in List-of-Positions has hit the Posn of the UFO.
; requiring a direct hit for now, for simplicity. this will cause some tests to fail.

(define (posn-hit-lpos? posn lop)
  (ormap (lambda (p) (in-reach? (distance posn p))) lop))

; MissileOrNot, MissileOrNot -> MissileOrNot (where posn-x and posn-y are both greater than 0)
; distance determines the distance between two positions, p1 and p2 and returns a special MissileOrNot.
; in which both posn-x and posn-y are greater than zero (using absolute value).
; or in which the MissileOrNot is false, if one of the objects being compared is false.

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

(check-expect (in-reach? (* 2 R)) false)
(check-expect (in-reach? R) false)
(check-expect (in-reach? (- R 1)) true)
(check-expect (in-reach? 0) true)
(check-expect (in-reach? (make-posn R R)) false)
(check-expect (in-reach? (make-posn (/ R 2) (/ R 2))) true)
(check-expect (in-reach? (make-posn 0 R)) false)
(check-expect (in-reach? (make-posn 0 0)) true)
                                                                   
(define (in-reach? loc)
  (cond
    [(posn? loc)(< (sqrt (+ (sqr (posn-x loc)) (sqr (posn-y loc)))) R)]
    [(number? loc)(< loc R)]))

; Tank, Image -> Image
; tank-render draws a Tank t over an image i.

(define (tank-render t i)
  (draw-one-posn TANK (make-posn (tank-x t) TANK-Y) i))

; UFO, Image -> Image
; ufo-render draws a UFO u over an image i.

(define (ufo-render u i)
  (draw-one-posn UFO u i))

; Missile, Image -> Image
; missile-render draws a Missile m over an image i. If Missile is false it returns i.

(define (missile-render b i)
  (lop-render b MISSILE i))

(main aimstart)
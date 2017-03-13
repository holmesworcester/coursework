;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |483|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(define QUEENS 8)
(define MT (empty-scene 100 100))
(define HEIGHT 30)
(define SQUARE (square HEIGHT "outline" "black"))
(define QUEEN (circle (/ HEIGHT 2) "solid" "red"))
; (define QUEEN (above (circle (/ HEIGHT 5) "solid" "red") (triangle (/ HEIGHT 1.5) "solid" "red")))

; QP is (make-posn CI CI)
; CI is a natural number in [0,QUEENS)
; interpretation a CI denotes a row or column index for a chess board, 
; (make-posn r c) specifies the square in the r-th row and the c-th column

(define LQP (list (make-posn 0 0) (make-posn 1 1)))
(define 4-SOLUTION (list (make-posn 2 0) (make-posn 0 1) (make-posn 3 2) (make-posn 1 3)))

; QP QP -> Boolean
; returns true if the queens threaten each other. Otherwise returns false.
; for simplicity sake, a queen cannot threaten itself (another QP in an identical position)

(check-expect (threatening? (make-posn 0 0) (make-posn 1 1)) #t)
(check-expect (threatening? (make-posn 0 0) (make-posn 0 1)) #t)
(check-expect (threatening? (make-posn 1 0) (make-posn 0 0)) #t)
(check-expect (threatening? (make-posn 4 4) (make-posn 5 5)) #t)
(check-expect (threatening? (make-posn 0 0) (make-posn 1 3)) #f)
(check-expect (threatening? (make-posn 5 5) (make-posn 9 1)) #t)
(check-expect (threatening? (make-posn 5 5) (make-posn 5 5)) #f)


(define (threatening? q1 q2)
  (local (; useful constants
          (define q1x (posn-x q1))
          (define q1y (posn-y q1))
          (define q2x (posn-x q2))
          (define q2y (posn-y q2)))
    ;-IN-
    (cond
      [(equal? q1 q2) #f] ; same qp, no threaten.
      [(equal? q1y q2y) #t] ; same row
      [(equal? q1x q2x) #t] ; same column
      [(equal? (- q1x q2x) (- q1y q2y)) #t] ; same down-right diagonal
      [(equal? (+ q1x q1y) (+ q2x q2y)) #t] ; same down-left diagonal
      [else #f])))


; N -> N
; takes a QP and turns it into actual position on the board, for purposes of place-image

(check-expect (real-xy 0) (/ HEIGHT 2))
(check-expect (real-xy 10) (+ (* 10 HEIGHT) (/ HEIGHT 2)))

(define (real-xy n)
  (+ (* n HEIGHT) (/ HEIGHT 2)))

; N [List-of QP] Image -> Image
; consumes a natural number n, a list l of QPs, and an Image i representing a queen.
; It produces an image of an n by n chess board with images i placed according to l.
; this works. I think the lack of margin is something quirky about not having a background.
; it seems like there's a much easier way to do this, by drawing the queen in each square as it occurs or by breaking out the blank chessboard.

(define (render-queens n lqp i)
  (local (; N Image -> Image
          ; consumes a number N and an image of a row and draws n rows (a complete chess board of that size)
          (define (draw-all-rows n row)
            (cond
              [(<= n 1) row]
              [else (above row (draw-all-rows (- n 1) row))]))
          ; N -> Image
          ; consumes a number N and draws a chess board row of that size
          (define (draw-one-row n)
            (cond
              [(<= n 1) SQUARE]
              [else (beside SQUARE (draw-one-row (- n 1)))]))
          ; [List-of QP] Image -> Image)
          ; consumes a list of QP's and an image of a chess board.
          ; Places the QP's at the correct position on the board.
          (define (place-all-queens lqp chessboard-image)
            (local (; QP Image -> Image
                    ; consumes a QP and an image of a chessboard. produces a new image of a chessboard
                    (define (place-queen qp chessboard-image)
                      (place-image i (real-xy (posn-x qp)) (real-xy (posn-y qp)) chessboard-image))) ; i is the queen image
              ;-IN-
              (foldr place-queen chessboard-image lqp))))
    ;-IN-
    (place-all-queens lqp (draw-all-rows n (draw-one-row n))))) ; that last expression is a blank chessboard

; N -> [[List-of QP] -> Boolean]
; produces a predicate function that returns true if a given list of queen placements is a valid solution for
; an n-queens problem of order n. If it's not a solution, it returns false. That is, if the list of queens is not of length n,
; or if any queens threaten each other, it returns false. Otherwise it returns true.
; this works but it feels messy. what more can I do?
; this trick works because queens can't threaten themselves. so I don't have to remvove the item I'm checking from the list.
; the improvement would be to study the offered generative recursion trick at the outset and adapt that. 

(check-expect ((n-queens-solution? 2) LQP) #f)
(check-expect ((n-queens-solution? 4) 4-SOLUTION) #t)

(define (n-queens-solution? n)
  (lambda (lqp)
    (local (; [List-of QP] -> Boolean
            ; returns true if any of the QPs in the LQP threaten each other.
            ; otherwise returns false.
            (define (none-threaten-others? lqp)
              (local (; QP [List-of QP] -> Boolean
                      ; returns true if the QP threatens any in the LQP. otherwise returns false.
                      (define (qp-threatens-others? qp an-lqp)
                        (cond
                          [(empty? an-lqp) #f]
                          [(threatening? qp (first an-lqp)) #t]
                          [else (qp-threatens-others? qp (rest an-lqp))]))
                      ; QP -> Boolean
                      ; returns true if the qp doesn't threaten others
                      (define (qp-doesnt-threaten-others? qp) 
                        (not (qp-threatens-others? qp lqp))))
                ;-IN-                  
                (andmap qp-doesnt-threaten-others? lqp)))) 
      ;-IN-
      (and (equal? n (length lqp)) (none-threaten-others? lqp)))))

; N -> [Maybe [List-of QP]]
; find a solution to the n queens problem. If there is no solution returns false. If there is a solution, returns the solution
; as a list of QP

(check-expect (n-queens 2) #f)
(check-expect (n-queens 3) #f)
(check-satisfied (n-queens 4) (n-queens-solution? 4))
(check-satisfied (n-queens 5) (n-queens-solution? 5))
(check-satisfied (n-queens 6) (n-queens-solution? 6)) ; shouldn't i check a range or something? 
; Also, is it a problem here if the actual solution is false? my predicate will throw an error in that case!

(define (n-queens n)
  (place-queens (board0 n) n)) ; just a stub!

; [List-of X] [List-of X] -> Boolean
; compares two lists (considered as sets) and tells me if the sets are equal, that is the lists differ
; in nothing other than the order of their items

(check-expect (set=? '(1 2 3) '(3 2 1)) #t)
(check-expect (set=? '(1 1 3) '(1 3 1)) #t)
(check-expect (set=? '(1 2 3) '(3 2 2)) #f)
(check-expect (set=? '(1 2 3) '(1 2 3 4)) #f)
(check-expect (set=? '() '(1 2 3)) #f)

(define (set=? s1 s2)
  (ormap (lambda (a-set) (equal? a-set s1)) (arrangements s2)))

; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
      (foldr (lambda (item others)
               (local ((define without-item
                         (arrangements (remove item w)))
                       (define add-item-to-front
                         (map (lambda (a) (cons item a)) without-item)))
                 (append add-item-to-front others)))
        '()
        w)]))

; Board N -> [Maybe [List-of QP]]
; places n queens on board in a non-threatening manner. if possible; otherwise, returns #false

(check-expect ((n-queens-solution? 4) (place-queens (board0 4) 4)) #t)
(check-expect ((n-queens-solution? 5) (place-queens (board0 5) 5)) #t)
; (check-expect ((n-queens-solution? 2) (place-queens (board0 2) 2)) #f)


(define (place-queens board n)
  (cond
    [(zero? n) (board-to-loqp board)] ; I'm out of queens, so I may have reached a solution. let's assume I did for now (check this!)
    [(empty? (find-open-spots board)) #f] ; there are no more spots, so since I'm not out of queens, return false.
    [else ; there are open spots and I have more queens to place. So try each open spot. This clause must return false or the solution.
      (local (; get me the list of all other successful tries
              (define successful-tries
                (filter (lambda (result) (not (false? result))) (map (lambda (open-spot) (place-queens (add-queen board open-spot) (- n 1))) (find-open-spots board))))) ; try every open spot, return the first true.
        ;-IN-
        (if (cons? successful-tries) (first successful-tries) #false))])) ; if there are successful tries, return them.

; IMPORTANT: I think this gets easier if the definition of board includes two LQP's: possible positions and used positions.
; That way, if place-queens returns false on a board with a queen placed in a certain position,
; you call the function on a new board with that position removed as a possibility, even though it's not occupied by a queen.
; that way the function keeps throwing out paths that fail while recursively generating sets of LQP's that work.

; Data definition of board.
; A Board is a structure [List-of QP], [List-of QP]
(define-struct board (queens spots))
; interpretation: Queens is the LQP describing where queens have been placed. Spots is the LQP describing spaces
; on the board that are neither open nor unthreatened by a queen.

; Board -> [List-of QP]]
; returns the list of QP's of a given board
(define (board-to-loqp a-board)
  (board-queens a-board))

; N -> Board 
; creates the initial n by n board

(check-expect (board0 2) (make-board '() (list (make-posn 0 0) (make-posn 1 0) (make-posn 0 1) (make-posn 1 1))))

(define (board0 n)
  (local (; N -> [List of QP]
          (define (all-posn n)
            (foldr append '() (build-list n (lambda (y) (build-list n (lambda (x) (make-posn x y))))))))
    ;-IN-
    (make-board '() (all-posn n))))

; Board QP -> Board 
; places a queen at qp on a-board
; must update list of queens but also remove her new spot and all threatened spots from available.

(check-expect (add-queen (board0 2) (make-posn 0 0)) (make-board (list (make-posn 0 0)) '())) 

(define (add-queen a-board qp)
  (local (; [List-of QP] -> [List-of QP]
          ; removes the new QP and all new threatened QP's from a [List-of QP]
          (define (remove-unavailable lqp)
            (local (; QP -> Boolean
                    ; checks to see if it's still an available spot, that is, 
                    ; not threatened by the new QP, and not the same as the new QP
                    (define (still-available? a-qp)
                      (and (not (threatening? qp a-qp)) ; available qp isn't threatened by the new queen.
                           (not (equal? qp a-qp))))) ; available qp isn't the same as the new qp.
              ;-IN-
            (filter still-available? lqp))))
    (make-board
     (cons qp (board-queens a-board)) ; add queen to the list of queens
     (remove-unavailable (board-spots a-board))))) ; remove newly unavailable from the list of spots
 
; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  (board-spots a-board))



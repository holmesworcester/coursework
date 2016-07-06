;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |280|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
; takes in a search-term and a list of items L and returns a function that tells me whether a list of items R (or false) is a potential
; result for a correct "find" function given that search term and list of items L.


;; `found?` needs a test

(define (found? search-term list)
  (cond
    [(empty? list) false?] ; return the function "false?" if the list is empty, because that's all we have to check.
    [else
     (lambda (results)
       (cond
         [(false? results) (not (member? search-term list))] ; when x isn't a member of l1.
         [else
          (and ; when is results really a find from list given search-term? when alll of these are true:
           (>= (length list) (length results)) ; results is the smaller one, or they're both the same size.
           (match-up? (reverse results) (reverse list)) ; their last items are equal, up to the length of the smaller.

					; I'm not convinced this case is working correctly:
           (not (member? search-term (foldr remove list results))))]))])) ; makes sure x does not exist outside the tail.

; X [List-of X] -> [Maybe [List-of X]]
; produces the first sublist of l that starts with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x) l (find x (rest l)))]))

; [List-of X], [List-of X] -> Boolean
; returns true if l2 is a tail of l1, otherwise returns false.

(check-expect (match-up? '(1 2 3) '(1 2 3)) #true)  
(check-expect (match-up? '(2 3) '(1 2 3)) #true)  ;; a "tail" mean the right-end of the list
(check-expect (match-up? '(3) '(1 2 3)) #true)
(check-expect (match-up? '(5 6) '(1 2 3)) #false)

(define (match-up? small long)
  (local (; set n to be the length of the small list, so I can use it to pull items
          (define (match-up-until-x? l1 l2 x)
            (cond
              [(zero? x) #true]
              [else (and (equal? (first l1) (first l2)) (match-up-until-x? (rest l1) (rest l2) (sub1 x)))])))
    (match-up-until-x? small long (length small))))

        
; Use found? to formulate a check-satisfied test for find.

(check-satisfied (find 10 (list 2 5 23 12 52345 5443 10 23 23 23 5 4526645 232)) (found? 10 (list 2 5 23 12 52345 5443 10 23 23 23 5 4526645 232)))
(check-satisfied (find 999999 (list 2 5 23 12 52345 5443 10 23 23 23 5 4526645 232)) (found? 999999 (list 2 5 23 12 52345 5443 10 23 23 23 5 4526645 232)))
(check-satisfied (find 10 '()) (found? 10 '()))

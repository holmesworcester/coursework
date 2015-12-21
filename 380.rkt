;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |380|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/batch-io)
(require 2htdp/image)

(define DICTIONARY-LOCATION "/usr/share/dict/words") ; on Mac OS X
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))
(define DICTIONARY-SIZE (length DICTIONARY-AS-LIST))
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))
 
; A HM-Word is [List-of [Maybe Letter]]
; interpretation #false represents a letter to be guessed 
; A Letter is member? of LETTERS.
 
; HM-Word N -> String
; run a simplistic Hangman game, produce the current state of the game
; assume the-pick does not contain #false
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) #false))
 
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
 
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
 
    ; the state of the game is a HM-Word
 
    (implode ; i don't get it, why turn it into a string? so it returns the string after time finishes?
     (big-bang the-guess
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key checked-compare]))))

; HM-Word, HM-Word, Letter -> HM-Word
; consumes the word to be guessed, a word that represents how much/little the guessing player has discovered, and the current guess.

(check-expect (compare-word (explode "madman") (list #f #f "d" #f #f #f) "m") (list "m" #f "d" "m" #f #f))
(check-expect (compare-word (explode "madman") (list #f #f "d" #f #f #f) "q") (list #f #f "d" #f #f #f))

(define (compare-word the-word current-status ke)
  (cond
    [(empty? current-status) '()] ; both words are the same length so just follow the structure of one and deal with them in parallel
    [else (cons (cond
                  [(false? (first current-status)) (if (string=? ke (first the-word)) ke #f)] ; compare and return false for the first letter if not a match, consed on to rest of everything.
                  [else (first the-word)])
                (compare-word (rest the-word) (rest current-status) ke))]))

; HM-Word -> Image
; render the word, using "_" for places that are #false
(define (render-word w)
  (local ((define l (map (lambda (lt) (if (boolean? lt) "_" lt)) w))
          (define s (implode l)))
    (text s 22 "black")))

(play (list-ref DICTIONARY-AS-LIST (random DICTIONARY-SIZE)) 10)

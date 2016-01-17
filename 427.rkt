;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |427|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A Line is [List-of 1String]

; A Token is
; - 1String
; - String consisting of lowercase letters and nothing else.
; (That is, all white-space 1Strings are dropped; all other non-letters remain as is; and all consecutive letters are bundled into “words.”)

; Line -> Line
; removes the first token in a line

(check-expect (drop-first-token '()) '())
(check-expect (drop-first-token (explode "hey ho yo")) (explode " ho yo"))
(check-expect (drop-first-token (explode " hey ho yo")) (explode " ho yo"))
(check-expect (drop-first-token (explode "    hey ho yo")) (explode " ho yo"))
(check-expect (drop-first-token (explode " ho yo")) (explode " yo"))
(check-expect (drop-first-token (explode "hey? ho yo")) (explode "? ho yo"))
; (check-expect (drop-first-token (explode "   hey ho yo")) (explode " ho yo")) ; leading whitespace is fine or removing it is fine
(check-expect (drop-first-token (explode "h ho yo")) (explode " ho yo"))
(check-expect (drop-first-token (explode "h? ho yo")) (explode "? ho yo"))
(check-expect (drop-first-token (explode "?hey ho yo")) (explode "hey ho yo"))

(define (drop-first-token line)
  (local (; remove the leading whitespace
          (define line-no-lws (drop-leading-whitespace line)))
    ;-IN-
    (cond
      [(empty? line-no-lws) '()]
      [(letter? (first line-no-lws)) (drop-word (rest line-no-lws))]
      [else (rest line-no-lws)])))


; Line -> Line
; removes the leading whitespace in a line

(check-expect (drop-leading-whitespace '()) '())
(check-expect (drop-leading-whitespace (explode "hey ho ")) (explode "hey ho "))
(check-expect (drop-leading-whitespace (explode "    hey ho "))(explode "hey ho "))
(check-expect (drop-leading-whitespace (explode " hey ho ")) (explode "hey ho "))
(check-expect (drop-leading-whitespace (explode "       ")) '())

(define (drop-leading-whitespace line)
  (cond
    [(empty? line) '()]
    [(string-whitespace? (first line)) (drop-leading-whitespace (rest line))]
    [else line]))


; Line -> Line
; removes the first word in a line (continuous string of letters unbroken by space or non-letter)

(check-expect (drop-word '()) '())
(check-expect (drop-word (explode "ey ho yo")) (explode " ho yo"))
(check-expect (drop-word (explode "ey? ho yo")) (explode "? ho yo"))
(check-expect (drop-word (explode " ho yo")) (explode " ho yo"))
(check-expect (drop-word (explode "? ho yo")) (explode "? ho yo"))

(define (drop-word line)
  (cond
    [(empty? line) '()]
    [(letter? (first line)) (drop-word (rest line))]
    [else line]))

; Line -> Token
; Extract the first Token in a line

(check-expect (first-token '()) "")
(check-expect (first-token (explode "hey ho yo")) "hey")
(check-expect (first-token (explode "     hey    ho yo")) "hey") ; space doesn't matter
(check-expect (first-token (explode "?hey ho yo")) "?") ; non letters are on their own
(check-expect (first-token (explode "h ey ho yo")) "h") ; one char first word followed by space
(check-expect (first-token (explode "h?ey ho yo")) "h") ; one char first word followed by punctuation

(define (first-token line)
  (local (; remove the leading whitespace
          (define line-no-lws (drop-leading-whitespace line)))
    ;-IN-
    (cond
      [(empty? line-no-lws) ""]
      [(letter? (first line-no-lws)) (string-append (first line-no-lws) (rest-of-word (rest line-no-lws)))]
      [else (first line-no-lws)]))) ; not a letter, not whitespace

; Line -> Token
; extracts the rest of a word, meaning all letters up to a non-letter


(check-expect (rest-of-word '()) "")
(check-expect (rest-of-word (explode "ey?")) "ey")
(check-expect (rest-of-word (explode "ey ")) "ey")
(check-expect (rest-of-word (explode " yo")) "") ; space ends word. maybe it was just one letter.
(check-expect (rest-of-word (explode "?yo")) "") ; non letter ends word. maybe it was just one letter.

(define (rest-of-word line)
  (cond
    [(empty? line) ""]
    [(letter? (first line)) (string-append (first line) (rest-of-word (rest line)))]
    [else ""]))

; 1String -> Boolean
; returns true if given something that is not a letter or whitespace. 

(check-expect (not-letter-not-whitespace? " ") #f)
(check-expect (not-letter-not-whitespace? "a") #f)
(check-expect (not-letter-not-whitespace? "?") #t)

(define (not-letter-not-whitespace? 1s)
  (not (or (string-whitespace? 1s) (letter? 1s))))

; 1String -> Boolean
; returns true if given a letter a-z or A-Z

(check-expect (letter? "A") #t)
(check-expect (letter? "Z") #t)
(check-expect (letter? "b") #t)
(check-expect (letter? "a") #t)
(check-expect (letter? "z") #t)
(check-expect (letter? " ") #f)
(check-expect (letter? "[") #f)

(define (letter? 1s)
  (or (<= (string->int "A") (string->int 1s) (string->int "Z")) (<= (string->int "a") (string->int 1s) (string->int "z"))))

; Line -> [List-of Token]
; consumes a line and returns a list of Token (see above)
; (That is, all white-space 1Strings are dropped; all other non-letters remain as is; and all consecutive letters are bundled into “words.”)

(check-expect (tokenize '()) '()) 
(check-expect (tokenize (explode "hey ho yo")) (list "hey" "ho" "yo")) 
(check-expect (tokenize (explode "hey ho yo?")) (list "hey" "ho" "yo" "?")) ; leave non letter one strings alone.
(check-expect (tokenize (explode "HEY ho yo?")) (list "hey" "ho" "yo" "?")) ; demands lowercase
(check-expect (tokenize (explode "HEY           ho yo?")) (list "hey" "ho" "yo" "?")) ; space shouldn't matter

(define (tokenize line)
  (local (; Line -> Line
          ; tokenizes the line, after it has been turned to all lowercase.
          (define (tokenizer line)
            (cond
              [(empty? line) '()]
              [else (cons (first-token line) (tokenizer (drop-first-token line)))])))
    ;-IN-
    (tokenizer (line-lower line))))

; Line -> Line
; makes all uppercase letters in a line lowercase

(check-expect (line-lower (explode "HEY           ho yo?")) (explode "hey           ho yo?"))

(define (line-lower line)
  (local (; the int for "a"
          (define a (string->int "a"))
          ; the int for "z"
          (define z (string->int "z"))
          ; the int for "A"
          (define A (string->int "A"))
          ; the int for "Z"
          (define Z (string->int "Z"))
          ; define the magic conversion number from cap to lowercase as a constant
          (define magic-conversion-number (- a A))
          ;1String -> 1String
          ;capitalizes a 1String if it is a lowercase letter or leaves it alone otherwise
          (define (lc-1s s1)
            (local (; the int for the string we're looking at (to avoid repitition)
                    (define sint (string->int s1))
                    ; 1String -> Boolean
                    ; returns true if a lowercase letter
                    (define s1-upppercase?
                      (<= A sint Z)))
              ; -IN-
              (cond
                [s1-upppercase? (int->string (+ sint magic-conversion-number))] ; if uppercase then lowercase it.
                [else s1]))))
    ;-IN-
    (map lc-1s line)))
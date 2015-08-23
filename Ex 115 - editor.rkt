;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 115 - editor|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; graphical constants
(define CURSOR (rectangle 1 20 "solid" "red"))
(define WIDTH 200)
(define HEIGHT 20)
(define MT (empty-scene WIDTH HEIGHT))
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")

; data definitions

; Index is a PostiveInteger
; interpretation: the cursor position in a string s counting from 0
; as you move letter by letter from left to right.

(define-struct editor [s i])
; Editor = (make-editor String Index)
; interpretation (make-editor s i) means the text in the editor is
; string s with the cursor displayed t characters from the left

(define hello-world (make-editor "helloworld" 5))
(define hell-oworld (make-editor "helloworld" 4))
(define hell-world (make-editor "hellworld" 4))
(define hellow-orld (make-editor "helloworld" 6))
(define h-elloworld (make-editor "helloworld" 1))
(define -helloworld (make-editor "helloworld" 0))
(define helloworld- (make-editor "helloworld" 10))
(define helloworl- (make-editor "helloworl" 9))
(define helloworl (make-editor "helloworl" 9))
(define hello-orld (make-editor "helloorld" 5))
(define hellok-world (make-editor "hellokworld" 6))

; Editor -> Editor
; run is my main function (not sure why they say to call it run)
; run takes in world states and spits out new world states, rendering them along the way

(define (run e)
  (big-bang e
   [on-key edit]
   [to-draw render]
   [check-with check-editor?]))

; Editor -> Boolean
; (check-editor? e) tells me if my editor state stays within all my data definitions
; that is, (editor? e) is true, and (string? (editor-s e)) is true and (integer? (editor-i e))
; is true and (>= (editor-i e) 0)

(check-expect (check-editor? -helloworld) #true)
(check-expect (check-editor? helloworl-) #true)
(check-expect (check-editor? hellow-orld) #true)
(check-expect (check-editor? 9) #false)
(check-expect (check-editor? "yo") #false)
(check-expect (check-editor? (make-editor "helloworld" -1)) #false)              
(check-expect (check-editor? (make-editor 9 -1)) #false)

              
 
(define (check-editor? e)
  (cond
    [(editor? e)(cond
                  [(and (integer? (editor-i e)) (>= (editor-i e) 0)) #true]
                  [else #false])]
    [else #false]))


; Editor, KeyEvent -> Editor
; edit takes in an Editor e and a KeyEvent ke and produces another Editor.
; its task is to add a single-character KeyEvent ke to the end of the pre field of ed,
; unless ke denotes the backspace ("\b") key. In that case, it deletes the character immediately
; to the left of the cursor (if there are any).
; The function ignores the tab key ("\t") and the return key ("\r").

(define (edit e ke)
  (cond
    [(string=? ke "left") (move-left e)] 
    [(string=? ke "right") (move-right e)] 
    [(string=? ke "\b") (backspace e)]
    [(string=? ke "\t") e] ; is there a better way to exclude these?
    [(string=? ke "\r") e]
    [(= (string-length ke) 1) (add-letter e ke)]
    [else e])) ; not a key event we care about, so return the same Editor

(check-expect (edit hello-world "\b") hell-world)
(check-expect (edit hello-world "left") hell-oworld)
(check-expect (edit -helloworld "left") -helloworld)
(check-expect (edit helloworld- "right") helloworld-)
(check-expect (edit hello-world "right") hellow-orld)
(check-expect (edit -helloworld "\b") -helloworld)
(check-expect (edit helloworld- "\b") helloworl-)
(check-expect (edit hello-world "blah") hello-world)
(check-expect (edit hello-world "\t") hello-world)
(check-expect (edit hello-world "\r") hello-world)
(check-expect (edit hello-world "k") hellok-world)
(check-expect (edit helloworl- "d") helloworld-)

; Editor -> Image
; render takes in an editor worldstate and draws an image of it
; by placing the first string s, then placing CURSOR, then placing the second string t on the background MT
(define (render e)
  (overlay/align "left" "center" (draw-text e) MT))

; Editor -> String
; a function editor-pre that substitutes my earlier structure by showing all the text before the cursor
(define (editor-pre e)
  (substring (editor-s e) 0 (editor-i e)))

; Editor -> String
; a function editor-post that substitutes my earlier structure by showing all the text after the cursor
(define (editor-post e)
  (substring (editor-s e) (editor-i e)))
  
; Editor -> Image
; draw-text is a function that renders all the text in the editor window
; having this as a separate function is helpful for limiting the width of text
(define (draw-text e)
  (beside (draw-string (editor-pre e)) CURSOR (draw-string (editor-post e))))

; String -> Image
; takes a string and renders an image in the appropriate font size and color
(define (draw-string t)
  (text t TEXT-SIZE TEXT-COLOR))

; Editor -> Editor
; move-left moves the cursor left by decrementing the index if not zero
(define (move-left e)
  (cond 
    [(> (editor-i e) 0) (make-editor (editor-s e) (- (editor-i e) 1))]
    [else e]))

(check-expect (move-left hello-world) hell-oworld)
(check-expect (move-left -helloworld) -helloworld)

; Editor -> Editor
; move-right moves the cursor left by incrementing the index if less than string length
(define (move-right e)
  (cond 
    [(> (string-length (editor-s e)) (editor-i e)) (make-editor (editor-s e) (+ (editor-i e) 1))]
    [else e]))

(check-expect (move-right helloworld-) helloworld-)
(check-expect (move-right hello-world) hellow-orld)

; Editor -> Editor
; backspace removes the last letter of the first string and leaves the second string alone
; uses the function remove-last

(define (backspace e)
  (cond
    [(< (editor-i e) 1) e]
    [else (make-editor (string-append (remove-last (editor-pre e)) (editor-post e)) (- (editor-i e) 1))]))

(check-expect (backspace helloworld-) helloworl-)
(check-expect (backspace -helloworld) -helloworld)
(check-expect (backspace hello-world) hell-world)

; String -> String
; remove-last removes the last letter of a string
; if the string is "" it returns ""

(define (remove-last s)
  (cond
    [(string=? s "") s]
    [else (substring s 0 (- (string-length s) 1))]))

(check-expect (remove-last "") "")
(check-expect (remove-last "helloworld") "helloworl")

; Editor -> Boolean
; A function got-space? that checks to see if the string overlay image is already wider
; than the WIDTH minus TEXT-SIZE

(define (got-space? e)
  (<= (image-width (draw-text e)) (- WIDTH TEXT-SIZE)))

(check-expect (got-space? (make-editor "short words" 5)) true)
(check-expect (got-space? (make-editor "very long worlds that are ridiculously long" 5)) false)


; Editor, Keyevent -> Editor
; add-letter adds the key event character to the current cursor position of editor e
; by adding it to (editor-pre e) and returning the modified Editor
; it also uses got-space? to check to make sure there is room for the letter
; if there is no room, it doesn't add anything

(define (add-letter e ke)
  (cond
    [(got-space? e) (make-editor (string-append (editor-pre e) ke (editor-post e)) (+ (editor-i e) 1))]
    [else e]))

(check-expect (add-letter hello-world "k") hellok-world)
(check-expect (add-letter helloworl- "d") helloworld-)

(run (make-editor "helloworld" 5))

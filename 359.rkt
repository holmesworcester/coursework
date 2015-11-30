;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |359|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; data definitions

; An Attribute is 
;   (cons Symbol (cons String '()))

; An AttributeList is one of:
; - '()
; - (cons Attribute AttributeList)

(define a0 '((initial "red")))

; An XWord is '(word ((text String)))
; or '(word AttributeList) where there is one attribute with the name 'text and the value String

(define hello '(word ((text "hello"))))
(define helloworld '(word ((text "hello world"))))
(define blank '(word ((text ""))))
(define space '(word ((text " "))))

; An Xexpr is
; - XWord
; – (cons Symbol [List-of Xexpr])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr]))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))

(define enum0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))


; functions

; An XWord is '(word ((text String)))
; or '(word AttributeList) where there is one attribute with the name 'text and the value String

; X -> Boolean
; a predicate for XWord. If x is an XWord returns true, if not returns false.

(check-expect (word? hello) #t)
(check-expect (word? 9) #f)
(check-expect (word? #f) #f)
(check-expect (word? "string") #f)
(check-expect (word? '(world ((text "hello")))) #f)
(check-expect (word? '(word ((text hello)))) #f)
(check-expect (word? '(word ((dude "hello")))) #f)
(check-expect (word? '(word ((text "hello")(text "this isn't allowed")))) #f)
(check-expect (word? blank) #t)
(check-expect (word? helloworld) #t)
(check-expect (word? space) #t)

(define (word? x)
  (and
   (and (list? x) (list? (rest x)) (list? (first (rest x))) (list? (first (first (rest x))))) ; they are all lists, so I can use first and rest to see what's in them.
   (local (; define the pieces as they ideally should be
          (define content (rest x)) ; a list of 1
          (define loa (first content)) ; a list of 1
          (define attr (first loa)) ; a list of 2
          (define text (first attr))) ; the Symbol 'text
     ;-IN-
     (and
      (and (= 1 (length content)) (= 1 (length loa)) (= 2 (length attr))) ; they are all the right length, so I can use "second" and I know they aren't full of stuf that's not allowed.
      (eq? (first x) 'word) ; its first item is 'word
      (eq? 'text text)
      (string? (second attr))))))

; Word -> String
; extracts the value of the only attribute of an instance of XWord.

(check-expect (word-text hello) "hello")

(define (word-text w)
  (lookup-attribute (xexpr-attributes w) 'text))

; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe

(check-expect (xexpr-attributes e0) '())
(check-expect (xexpr-attributes e1) '((initial "red")))
(check-expect (xexpr-attributes e2) '())
(check-expect (xexpr-attributes e3) '())
(check-expect (xexpr-attributes e4) '((initial "red")))

(define (xexpr-attributes xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  loa-or-x
                  '()))])))

; [List-of Attribute] or Xexpr -> Boolean
; is the given value a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; Xexpr -> Symbol
; retrieves the name of the Xexpr x

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name (first (rest e2))) 'action)

(define (xexpr-name x)
  (first x))

; Xexpr -> Xexpr
; removes any attributes from the first list of Xexpr

(check-expect (remove-attributes e1) e0)
(check-expect (remove-attributes e0) e0)
(check-expect (remove-attributes e3) e2) ; that little parentheses is an empty attributes list.
(check-expect (remove-attributes e4) '(machine (action) (action))) ; this test might be wrong.

(define (remove-attributes ex)
  (cons (first ex) (cond
                     [(empty? (rest ex)) '()]
                     [(list-of-attributes? (first (rest ex))) (rest (rest ex))]
                     [else (rest ex)])))

; Xexpr -> [List-of Xexpr] <-- is this right?
; retrieves the content of the Xexpr

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action))) 
(check-expect (xexpr-content e4) '((action)(action)))

; An Xexpr is 
; – (cons Symbol [List-of Xexpr])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr]))

(define (xexpr-content xe)
  (cond
    [(empty? (rest xe)) '()]
    [(list-of-attributes? (first (rest xe))) (rest (rest xe))] ; returns the content.
    [else (rest xe)])) ; returns the content (which if there's no attributes is the rest after the symbol)

; AttributeList, Symbol -> [Maybe String]
; consumes a list of attributes and a symbol. If the attributes list associates the symbol with a string, the function retrieves this string; otherwise it returns #false.

(check-expect (lookup-attribute a0 'symbolnotthere) #f)
(check-expect (lookup-attribute a0 'initial) "red")

(define (lookup-attribute loa s)
  (local ((define matching-attribute (assq s loa)))
    ;-IN-
    (if (false? matching-attribute) #false (second matching-attribute)))) 
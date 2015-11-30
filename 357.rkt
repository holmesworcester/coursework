;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |357|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; data definitions

; An Attribute is 
;   (cons Symbol (cons String '()))

; An AttributeList is one of:
; - '()
; - (cons Attribute AttributeList)

(define a0 '((initial "red")))

; An Xexpr is 
; – (cons Symbol [List-of Xexpr])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr]))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; An XWord is '(word ((text String)))
; or '(word AttributeList) where there is one attribute with the name 'text and the value String

(define hello '(word ((text "hello"))))
(define helloworld '(word ((text "hello world"))))
(define blank '(word ((text ""))))
(define space '(word ((text " "))))

; functions

; An XWord is '(word ((text String)))
; or '(word AttributeList) where there is one attribute with the name 'text and the value String

; X -> Boolean
; a predicate for XWord. If x is an XWord returns true, if not returns false.

(check-expect (word? hello) #t)
(check-expect (word? 9) #f)
(check-expect (word? '(world ((text "hello")))) #f)
(check-expect (word? '(word ((text hello)))) #f)
(check-expect (word? '(word ((dude "hello")))) #f)
(check-expect (word? '(word ((text "hello")(text "this isn't allowed")))) #f)

(define (word? x)
  (and
   (list? x) ; it's a list
   (eq? (first x) 'word) ; its first item is 'word
   (list? (rest x)) ; it contains a list.
   (= 1 (length (rest x))) ; the list it contains has just one attribute
   (list? (first (rest x))) ; just one list (one attribute)
   (= 2 (length (first (rest (rest x))))) ; this list (the one attribute) is a list of two, a name value pair.
   (eq? 'text (first (first (rest x))))
   (string? (second (first (rest x))))))
 
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
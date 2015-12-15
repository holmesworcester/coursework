;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |371|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; An Attribute is 
;   (cons Symbol (cons String '()))

; An AttributeList is one of:
; - '()
; - (cons Attribute AttributeList)

; An XWord is '(word ((text String)))
; or '(word AttributeList) where there is one attribute with the name 'text and the value String

; An Xexpr.v3 is
; - XWord
; – (cons Symbol [List-of Xexpr])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr]))

; An AttributeList is one of:
; - '()
; - (cons Attribute AttributeList)

(define a0 '((initial "red")))

; An XWord is '(word ((text String)))
; or '(word AttributeList) where there is one attribute with the name 'text and the value String

(define hello '(word ((text "hello"))))
(define bye '(word ((text "bye"))))
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

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute for 
; a 'meta element with attribute "itemprop" and value s
 
(check-expect
  (get '(meta ((content "+0.11") (itemprop "delta"))) "delta")
  "+0.11")
(check-expect
  (get '(meta ((itemprop "price") (content "17.01"))) "price")
  "17.01")
(check-error
  (get '(meta ((itemprop "price") (content "17.01"))) "delta")
  "attribute not found: delta")
 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error (string-append "attribute not found: " s)))))

; Xexpr.v3, String -> [Maybe String]
; retrieves the value of the "content" attribute for 
; a 'meta element with attribute "itemprop" and value s

(check-expect
  (get-xexpr '(meta ((content "+0.11") (itemprop "delta"))) "delta")
  "+0.11")
(check-expect
  (get-xexpr '(meta ((itemprop "price") (content "17.01"))) "price")
  "17.01")
(check-expect
  (get-xexpr '(meta ((itemprop "price") (content "17.01"))) "delta")
  #f)
(check-expect
  (get-xexpr '(meta ((itemprop "price") (content "17.01")) ) "delta")
  #f)

; write a test for a nested x-expr

(check-expect
  (get-xexpr '(meta ((itemprop "price") (content "17.01")) ((meta ((itemprop "price") (content "17.01")) )(meta ((itemprop "delta") (content "22")) ))) "delta")
  "22")

; write a test for a long attribute list

(check-expect
  (get-xexpr '(meta ((dude "yo") (dude "yo") (dude "yo") (itemprop "price") (content "17.01")) ) "delta")
  #f)

; write a test for no attributes list

(check-expect
  (get-xexpr '(meta) "delta")
  #f)

; write a test for an attributes list with two elements like '(itemprop ,s) where the first one isn't a match <-- well maybe this complicates needlessly. I'm going to skip.

; make sure it handles things when the first symbol isn't meta.

(check-expect
  (get-xexpr '(info ((itemprop "price") (content "17.01")) ((meta ((itemprop "price") (content "17.01")) )(meta ((itemprop "delta") (content "22")) ))) "delta")
  "22")

(check-expect
  (get-xexpr '(info ((itemprop "delta") (content "17.01")) ((meta ((itemprop "price") (content "17.01")) )(meta ((itemprop "delta") (content "22")) ))) "delta")
  "22")

; check that it handles words

(check-expect
  (get-xexpr hello "hello")
  #f)

(define (get-xexpr x s)
  (local (; AttributeList -> [Maybe String]
          ; if a list of attributes has an attribute with the symbol 'itemprop and the string s
          ; and it has an attribute with the symbol 'content and the string c
          ; it returns the string c. otherwise, it returns false.
          (define (itemprop-content an-al)
            (local (; an expression that checks to see if an AttributeList has a pair with the symbol 'itemprop that matches the string s
                    (define itemprop-s? (equal? (assoc 'itemprop an-al) `(itemprop ,s)))
                    ; an expression that returns [Maybe Attribute], an Attribute if there is an Attribute with the symbol 'content and false if there is not.
                    (define content (assoc 'content an-al))
                    ; an expression that checks to see if an AttributeList has a pair with the symbol 'content.
                    (define has-content? (not (false? content))))
              ; -IN-
              (if (and itemprop-s? has-content?) (second content) #f))) ; if there's a matching itemprop t s, and if there's a content Attribute, return the string for the content Attribute. otherwise returns false
          ; [List-of XExpr.v3] -> [Maybe String]
          ; takes in a list of xexpr's and returns the first one for which (get-expr x s) doesn't return #false.
          ; If all return #false, returns #false.
          (define (get-xexpr-from-xlist list-of-xexpr)
            (cond
              [(empty? list-of-xexpr) #f]
              [(false? (get-xexpr (first list-of-xexpr) s)) (get-xexpr-from-xlist (rest list-of-xexpr))] ; If the first one is false, check the rest. How is this not covered by the test in line 81?
              [else (get-xexpr (first list-of-xexpr) s)]))) ; otherwise return the first one.           
          ; -IN-      
  (cond
    ; I don't have to worry about "word" because it isn't what I'm looking for and can't contain anything.
    [(and (equal? (xexpr-name x) 'meta) (not (false? (itemprop-content (xexpr-attributes x))))) (itemprop-content (xexpr-attributes x))]  ; I just have to worry about "meta" and then find the string for content if there is an attribute with the itemprop s
    [else (get-xexpr-from-xlist (xexpr-content x))]))) ; I can ignore the attributes and focus on all the other xexpr's in the list

; X, [List-of [List X Y]] -> [Maybe [List X Y]}
; finds the first pair in a list of pairs for which the first item matches X. If it finds nothing, returns false.

  (define fsm-traffic
  '(("red" ("g" "green")) ("green" ("y" "yellow")) ("yellow" ("" "red"))))
  
(check-expect (assoc "green" fsm-traffic) '("green" ("y" "yellow")))
(check-expect (assoc "violet" fsm-traffic) #f)

(define (assoc x a-list)
  (cond
    [(empty? a-list) #f]
    [else (if (equal? x (first (first a-list))) (first a-list) (assoc x (rest a-list)))]))


; Xexpr -> [List-of Xexpr] <-- is this right?
; retrieves the content of the Xexpr

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action))) 
(check-expect (xexpr-content e4) '((action)(action)))

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
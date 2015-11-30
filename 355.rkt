;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |355|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Xexpr.v0 (short for X-expression) is 
;   (cons Symbol '())

; An Xexpr.v1 is 
;   (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is 
; – (cons Symbol [List-of Xexpr.v2])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))

; OR IN OTHER WORDS

; An Xexpr.v2 is one of:
; - (cons Symbol '())
; – (cons Symbol Xexpr.v2) 
; – (cons Symbol (cons AttributeList '())
; – (cons Symbol (cons AttributeList Xexpr.v2))

; An AttributeList is one of:
; - '()
; - (cons Attribute AttributeList)

; An Attribute is 
;   (cons Symbol (cons String '()))

(define a0 '((initial "red")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

(check-expect (xexpr-attributes e0) '())
(check-expect (xexpr-attributes e1) '((initial "red")))
(check-expect (xexpr-attributes e2) '())
(check-expect (xexpr-attributes e3) '())
(check-expect (xexpr-attributes e4) '((initial "red")))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe

(define (xexpr-attributes xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  loa-or-x
                  '()))])))

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is the given value a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; Xexpr.v2 -> Symbol
; retrieves the name of the Xexpr.v2 x

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name (first (rest e2))) 'action)

(define (xexpr-name x)
  (first x))

; Xexpr.v2 -> Xexpr.v2
; removes any attributes from the first list of Xexpr.v2

(check-expect (remove-attributes e1) e0)
(check-expect (remove-attributes e0) e0)
(check-expect (remove-attributes e3) e2) ; that little parentheses is an empty attributes list.
(check-expect (remove-attributes e4) '(machine (action) (action))) ; this test might be wrong.

(define (remove-attributes ex)
  (cons (first ex) (cond
                     [(empty? (rest ex)) '()]
                     [(list-of-attributes? (first (rest ex))) (rest (rest ex))]
                     [else (rest ex)])))

; Xexpr.v2 -> [List-of Xexpr.v2] <-- is this right?
; retrieves the content of the Xexpr.v2

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action))) 
(check-expect (xexpr-content e4) '((action)(action)))

; An Xexpr.v2 is 
; – (cons Symbol [List-of Xexpr.v2])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))

(define (xexpr-content xe)
  (cond
    [(empty? (rest xe)) '()]
    [(list-of-attributes? (first (rest xe))) (rest (rest xe))] ; returns the content.
    [else (rest xe)])) ; returns the content (which if there's no attributes is the rest after the symbol)

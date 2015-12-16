;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |374|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct phone-record [name number])
; A PhoneRecord is (make-phone-record String String).

(define holmes (make-phone-record "Holmes" "614-555-1212"))
(define mary (make-phone-record "Mary" "774-555-1212"))

; [List-of String], [List-of String] -> [List-of PhoneRecord]
; consumes a list of names, represented as strings, and a list phone numbers, also strings.
; It combines those equally long lists into a list of phone records.

(check-expect (zip '("Holmes" "Mary") '("614-555-1212" "774-555-1212")) (list holmes mary))

(define (zip l-name l-number)
  (cond
    [(empty? l-name) '()]
    [else (cons (make-phone-record (first l-name) (first l-number)) (zip (rest l-name) (rest l-number)))]))
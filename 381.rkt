;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |381|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; In a factory, employees punch time cards as they arrive in the morning and leave in the evening.
; Electronic punch cards contain an employee number and record the number of hours worked per week.
; Employee records always contain the name of the employee, an employee number, and a pay rate.

; Data definitions

; An Employee is a structure:
; (make-employee String Number Number)
; interpretation: the name of the employee, their employee number, and their hourly pay rate .

(define-struct employee [name number rate])

; examples:
(define holmes (make-employee "Holmes Wilson" 1 15))
(define mary (make-employee "Mary Wilson" 2 30))
(define erroremployee (make-employee "Joe Dude" 5 30))

; A Punchcard is a structure:
; (make-punchcard Number Number)
; interpretation: the employee number of an employee, and the number of hours worked that week.

(define-struct punchcard [number hours])

; examples:
(define holmespunch (make-punchcard 1 40))
(define marypunch (make-punchcard 2 30))
(define errorpunch (make-punchcard 3 20))

; A WageRecord is a structure:
; (make-wagerecord String Number)
; interpretation: the name and weekly wage of an employee.

(define-struct wagerecord [name wage])

; examples:
(define holmeswage (make-wagerecord "Holmes Wilson" 600))
(define marywage (make-wagerecord "Mary Wilson" 900))

; functions

; [List-of Employee] [List-of Punchcard] -> [List-of WageRecord]
; consumes a list of employee records and a list of punch-card records and produces a list of wage records.
; The function signals an error if it cannot find an employee record for a punch-card record or vice versa.
; You may assume that there is at most one punch-card record per employee number.
; I'm also going to assume that neither list begins empty.

(check-error (wages.v3 (list holmes erroremployee) (list holmespunch marypunch)) "cannot find punch-card for employee record")
(check-error (wages.v3 (list holmes mary) (list holmespunch errorpunch)) "cannot find punch-card for employee record")
(check-expect (wages.v3 '() '()) '())
(check-error (wages.v3 (list holmes mary) '()) "cannot find punch-card for employee record")
(check-expect (wages.v3 (list holmes mary) (list holmespunch marypunch)) (list holmeswage marywage))

(define (wages.v3 e p)
  (local (; Employee -> WageRecord
          ; consumes an employee and returns that employee's wage record
          (define (wage-record-for-employee an-employee)
            (make-wagerecord (employee-name an-employee) (* (employee-rate an-employee) (hours-punched-for-employee-number (employee-number an-employee) p)))))
    ; -IN-
  (cond
    [(empty? e) '()]
    [else (cons (wage-record-for-employee (first e)) (wages.v3 (rest e) p))])))

; Number, [List-of Punchcard] -> Number
; consumes an employee number and returns the number of hours an employee has worked
; must return an error "cannot find punch-card for employee record" if it can't find the number in the [List-of Punchcard] p

(check-expect (hours-punched-for-employee-number 1 (list holmespunch marypunch)) 40)
(check-expect (hours-punched-for-employee-number 2 (list holmespunch marypunch)) 30)

(define (hours-punched-for-employee-number an-emp-num a-punchlist)
  (cond
    [(empty? a-punchlist)(error "cannot find punch-card for employee record")]
    [else (if (= an-emp-num (punchcard-number (first a-punchlist))) (punchcard-hours (first a-punchlist)) (hours-punched-for-employee-number an-emp-num (rest a-punchlist)))]))

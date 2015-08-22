;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Ex 106|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Exercise 106. The administration of your home town manages a fleet of vehicles:
; automobiles, vans, buses, SUVs, and trucks. Develop a data representation for vehicles.
; The representation of each vehicle must describe the number of passengers that it can comfortably accommodate,
; its license plate, and its fuel consumption (miles per gallon).

; Data Definitions

; VehicleType is one of:
; - "car"
; - "van"
; - "bus"
; - "suv"
; - "truck"
; interpretation: the type of vehicle

; Passengers is a PositiveInteger
; intepretation: the number of passengers a vehicle can comfortably accomodate

; Plate is a String
; interpretation: the license plate of a vehicle.

; MPG is a PositiveNumber
; interpretation: the fuel consumption of the vehicle measured in miles per gallon.

; Vehicle is (make-vehicle VehicleType Passeners Plate MPG)
; interpretation: a vehicle in the fleet our town administers

(define-struct vehicle (type passengers plate mpg))

(define CAR1 ("car" 5 "THX1138" 30))
(define TRUCK1 ("truck" 5 "BIGBERTH" 6))
(define VAN1 ("van" 12 "293HLSWQ" 14))

(define (template v)
  (... (vehicle-type v) ... (vehicle-passengers v) ... (vehicle-plate v) ... (vehicle-mpg v)))
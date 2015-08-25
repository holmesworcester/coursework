;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |149|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Is it better to work with data definitions that accommodate empty lists as opposed to definitions for non-empty lists?
; Why? Why not?

; pros for NE lists: no ambiguity about what to do in the case of empty. this is always a weak point and totally open to interpretation, requiring explanation in the definition.
; cons for NE lists: slightly less readable and intuitive. there's a reason they didn't start with them.

; what else?

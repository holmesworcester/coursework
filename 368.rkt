;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |368|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Exercise 368. Formulate an XML configuration for a machine that switches from white to black and back for every key event and then translate it into an XMachine representation. See exercise 213 for an implementation of the machine as a program. image

<machine initial="white">
   <action state="white"    next="black" />
   <action state="black"  next="white" />
  </machine>
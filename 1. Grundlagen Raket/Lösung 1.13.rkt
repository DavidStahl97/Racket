#lang racket

(define (celsius-to-fahrenheit celsius) (+ (* celsius 1.8) 32))

(celsius-to-fahrenheit 20)

(define (celsius-to-fahrenheit-string celsius) (string-append-immutable (number->string celsius) " Grad Celsius entsprechen " (number->string (celsius-to-fahrenheit celsius)) " Grad Fahrenheit"))

(celsius-to-fahrenheit-string 20)
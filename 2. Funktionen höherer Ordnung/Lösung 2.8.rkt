#lang racket

(define (f g) (g 2))

(f sqr)

(f sqrt)

(f f)

;; Der Wert 2 ist keine Prozedur
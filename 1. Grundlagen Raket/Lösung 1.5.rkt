#lang racket

(define (quad a) (* a a))

(define (distance x y) (sqrt (+ (quad x) (quad y))))

(distance 2 2)
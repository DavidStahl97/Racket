#lang racket

(define (my-length lst)
  (foldl (lambda (x y) (+ y 1)) 0 lst))

(my-length '(1 2 324 324 s 3))
#lang racket

(define (my-max lst)
  (define (choose-max x y)
    (cond
      ((> x y) x)
      (else y)))
  
  (foldr choose-max 0 lst))

(my-max '( 1 23 4 3 2 111 1))
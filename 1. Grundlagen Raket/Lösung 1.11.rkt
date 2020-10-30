#lang racket

(define (square-my-list lst)
  (cond
    ((null? lst) '())
    (else (cons (expt (first lst) 2) (square-my-list (rest lst))))))

(square-my-list (list 1 2 3 2))
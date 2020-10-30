#lang racket

(define (sum-my-list lst)
  (cond
    ((null? lst) 0)
    (else (+ (first lst) (sum-my-list (rest lst))))))

(sum-my-list (list 11 1 2))
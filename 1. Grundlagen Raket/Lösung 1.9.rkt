#lang racket

(define (my-find lst x)
  (cond
    ((null? lst) #f)
    ((equal? (first lst) x) #t)
    (else (my-find (rest lst) x))))

(my-find (list 1 2 "hallo" 4 2) "hallo")
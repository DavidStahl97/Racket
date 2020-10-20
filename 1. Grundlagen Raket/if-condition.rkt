#lang racket

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (> 1 0) 1 0)
(new-if (< 2 1) 1 0)

(+ 5 5 5 5)

(< 5 6 4)
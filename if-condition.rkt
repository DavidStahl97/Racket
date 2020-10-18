#lang racket

(define (if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(if (> 2 1) if 0)
(if (< 2 1) 1 0)
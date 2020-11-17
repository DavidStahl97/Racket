#lang racket

(define (endrec-sum lst)
  (define (sum-iter lst value)
    (if (null? lst) value (sum-iter (rest lst) (+ value (first lst)))))

  (sum-iter lst 0))

(endrec-sum '(1 4 5 2))  
    
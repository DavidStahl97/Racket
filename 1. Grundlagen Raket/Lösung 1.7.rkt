#lang racket

(define (get-numbers n)
  (cond
    ((= n 0) '())
    (else (cons n (get-numbers (- n 1))))
  )
)
  
(get-numbers 5)
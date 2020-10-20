#lang racket

(define (my-abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))
  )
)

(define (my-abs2 x)
  (if (< x 0) (- x) x)
)
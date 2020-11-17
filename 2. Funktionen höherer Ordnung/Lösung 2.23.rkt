#lang racket

(define (my-compose g h)
  (λ (x) (g (h x))))

((my-compose number->string sqr) 9)
#lang racket

(define (counter start)
  (let ([i start])
    (λ ()
      (set! i (add1 i))
      i)))

(define first-counter (counter 2))
(define second-counter (counter 10))

(first-counter)
(first-counter)
(second-counter)
(first-counter)

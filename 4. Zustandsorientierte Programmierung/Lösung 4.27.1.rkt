#lang racket

;; Understanding manipulating closures

(define (test f)
  (let ([table (make-hash)])
    (lambda (x)
      (let ([result (f x)])
        (hash-set! table x result)
        table))))

(define hallo (test (λ (x) (add1 x))))

(hallo 2)
(hallo 3)
(hallo 4)
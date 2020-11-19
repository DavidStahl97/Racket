#lang racket

(define memo
  (let ([x 2])
    (λ (y) (+ x y))))

;; has to be continued


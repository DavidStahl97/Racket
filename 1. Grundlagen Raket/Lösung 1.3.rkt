#lang racket

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; application order: endlosschleife
;; normal order: gibt 0 zurück
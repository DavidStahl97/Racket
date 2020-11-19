#lang racket

(define a 1)
(define b 1)

(define x (make-hash '(('a . 10) (b . 20))))

(hash-ref-key x 'b)

;; has to be continued
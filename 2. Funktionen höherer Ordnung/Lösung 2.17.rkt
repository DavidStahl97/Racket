#lang racket

(define (my-map func lst)
  (foldl (lambda (x y) (append y (list (func x)))) '() lst))

(my-map (lambda (x) (+ x 2)) '(1 2 3 2 4))
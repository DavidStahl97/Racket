#lang racket

(foldl / 1 '(2 3))

(foldr / 1 '(2 3))

(foldl cons '(x) '(1 2 3 4))

(foldr cons '(x) '(1 2 3 4))

;; damit foldr und foldl äquivalent sind, müssen sie kommutativ und assoziativ sein

(define (my-foldr func start lst)
  (if (null? lst) start (func (first lst) (my-foldr func start (rest lst)))))

;; (1 * (2 * (3 * (4 * start))))
(my-foldr cons '(x) '(1 2 3 4))

(define (my-foldl func start lst)
  (if (null? lst) start (my-foldl func (func (first lst) start) (rest lst))))

;; (4 * (3 * (2 * (1 * start)))
(my-foldl cons '(x) '(1 2 3 4))
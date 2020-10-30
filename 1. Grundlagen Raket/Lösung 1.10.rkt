#lang racket

(define (sum-my-list lst)
  (cond
    ((null? lst) 0)
    (else (+ (first lst) (sum-my-list (rest lst))))))

(sum-my-list (list 11 1 2))

(define (sum-my-list-end lst)
  (define (sum-my-list-end-rec lst number)
    (cond
      ((null? lst) number)
      (else (sum-my-list-end-rec (rest lst) (+ (first lst) number)))))

  (sum-my-list-end-rec lst 0))

(sum-my-list-end (list 11 1 2))
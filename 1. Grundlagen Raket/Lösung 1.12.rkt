#lang racket

(define (sum-my-list-end lst)
  (define (sum-my-list-end-rec lst number)
    (cond
      ((null? lst) number)
      (else (sum-my-list-end-rec (rest lst) (+ (first lst) number)))))

  (sum-my-list-end-rec lst 0))

(sum-my-list-end (list 11 1 2))

(define (square-my-list-end lst)
  (define (square-my-list-end-rec lst square-list)
    (cond
      ((null? lst) square-list)
      (else (square-my-list-end-rec (rest lst) (append square-list (list (expt (first lst) 2)))))))

  (square-my-list-end-rec lst '()))

(square-my-list-end (list 1 2 3 2 4))
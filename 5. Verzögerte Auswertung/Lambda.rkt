#lang racket

(require math)

(define head car)
(define (tail s) ((cdr s)))

(define the-empty-stream '())
(define empty-stream? null?)

(define (cons-stream x y)
  (cons x y))

(define (interval a b)
  (if (> a b)
      the-empty-stream
      (cons-stream a (λ () (interval (add1 a) b)))))

(define (filter-stream pred? s)
  (cond
    ((empty-stream? s) s)
    ((pred? (head s)) (cons-stream (head s) (λ () (filter-stream pred? (tail s)))))
    (else (filter-stream pred? (tail s)))))

(define (second-prime a b)
  (head (tail 
    (filter-stream (λ (x) (prime? x))
       (interval a b)))))

(second-prime 10 99999999999999999999999)
  
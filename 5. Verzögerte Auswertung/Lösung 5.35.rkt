#lang racket

 (require math)

;; force und delay gibt es in einem Racket
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)

(define (tail s) (force (cdr s)))

(define the-empty-stream '())
(define empty-stream? null?)

;; map
(define (map-stream f s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (f (head s))
                   (map-stream f (tail s)))))

;; filter
(define (filter-stream p s) ;; Procedure Stream -> Stream
  (cond
    ((empty-stream? s) the-empty-stream)
    ((p (head s)) (cons-stream (head s)
                               (filter-stream p (tail s))))
    (else (filter-stream p (tail s)))))

;; reduce
(define (right-reduce combine initial s)
  (if (empty-stream? s)
      initial
      (combine (head s)
               (right-reduce combine initial (tail s)))))

;; count
(define (count-stream stream)
  (right-reduce (λ (x y) (add1 y)) 0 stream))

;; Exercise list->stream
(define (to-stream lst)
  (if (empty? lst)
      the-empty-stream  
      (cons-stream (first lst) (rest lst))))

;; example
(define (count-primes lst)
  (count-stream
    (filter-stream prime?
      (to-stream lst))))

(define numbers (list 7 5 4 3 23 43 54 65 5 2 1))

(count-primes numbers)
#lang racket

;; force und delay gibt es in einem Racket
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)

(define (tail s) (force (cdr s)))

(define the-empty-stream '())
(define empty-stream? null?)

;; reduce
(define (right-reduce combine initial s)
  (if (empty-stream? s)
      initial
      (combine (head s)
               (right-reduce combine initial (tail s)))))

(define (sum s) (right-reduce + 0 s))

;; stream->list
(define (to-list stream)
  (if (empty-stream? stream)
      '()
      (cons (head stream) (to-list (tail stream)))))


;; exercise
(define (fak number)
  (if (equal? number 0)
      1
      (* number (fak (sub1 number)))))

(define (fak-stream start)
  (define (next i prev)
    (let ([current (* prev i)])
      (cons-stream current (next (add1 i) current))))

  (if (equal? start 0)
      (cons-stream 1 (next (add1 start) 1))
      (next start (fak (sub1 start)))))

;; example

(define (fak-interval x y)
  (define (next i stream)
    (if (> i y)
        the-empty-stream
        (cons-stream (head stream) (next (add1 i) (tail stream)))))

  (next x (fak-stream x)))

(sum (fak-interval 2 5))
(to-list (fak-interval 2 5))






  


    
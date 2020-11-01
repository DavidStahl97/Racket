#lang racket

(define (my-append one two)
  (define (append-rec lst atListOne)
    (if (null? lst)
      (if atListOne (append-rec two #f) '())
      (cons (first lst) (append-rec (rest lst) atListOne))))

  (append-rec one #t))

(my-append '(1 2 3) '(4 5))
       
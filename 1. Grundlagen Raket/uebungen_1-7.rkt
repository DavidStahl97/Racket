#lang racket



(append (list 1) (list 2))





(define (remove-last numbers)
  (remove (length numbers) numbers))

(define (my-reverse numbers)
  (cond
    ((= (length numbers) 0) '())
    (else (cons (last numbers) (my-reverse (remove-last numbers))))
  )
)






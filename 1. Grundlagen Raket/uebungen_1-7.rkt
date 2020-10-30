#lang racket



(append (list 1) (list 2))



(define (get-numbers n)
  (cond
    ((= n 0) '())
    (else (cons n (get-numbers (- n 1))))
  )
)
  
(get-numbers 5)

(define (remove-last numbers)
  (remove (length numbers) numbers))

(define (my-reverse numbers)
  (cond
    ((= (length numbers) 0) '())
    (else (cons (last numbers) (my-reverse (remove-last numbers))))
  )
)






#lang racket

(define (remove-last numbers)
  (cond
    ((= (length numbers) 1) '())
    (else (cons (first numbers) (remove-last (rest numbers))))))

(define (my-reverse numbers)
  (cond
    ((= (length numbers) 0) '())
    (else (cons (last numbers) (my-reverse (remove-last numbers))))
  )
)

(remove-last (list 3 4 3))

(my-reverse (list 3 1 5))

(my-reverse '())
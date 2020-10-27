#lang racket

(/
 (+ 5 4 (- 2 ( - 3 (+ 6 ( / 4 5)))))
 (* 3 (- 6 2) (- 2 7))
)

(define (quad a) (* a a))

(define (small a b c)
  (cond
    ((< a b c) a)
    ((< b a c) b)
    (else c)
  )
)

(define (groestesQuadratSum a b c)
  (+ (quad a) (quad b) (quad c) (- (quad (small a b c))))
)

(groestesQuadratSum 5 4 3)

(define (distance x y) (sqrt (+ (quad x) (quad y))))

(distance 2 2)

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






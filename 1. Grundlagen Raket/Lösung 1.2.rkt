#lang racket

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
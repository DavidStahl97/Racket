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



(define (get-numbers2 n numbers)
  (cond
    ((= n 0) (list))
    (else (append (get-numbers2 (- n 1) numbers) (list n)))
  )
)

(define (get-numbers n) (get-numbers2 n (list)))
  

(get-numbers 5)






(define (get-numbers3 n)
  (cond
    ((= n 1) 1)
    (else n (get-numbers3 (- n 1)))
  )
)

(get-numbers3 5)


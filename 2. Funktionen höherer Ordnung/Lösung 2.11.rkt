#lang racket

(define (repeated func times)
  (define (rec x times)
    (cond
      ((= times 0) x)
      (else (func (rec x (- times 1))))))

  (λ (x) (rec x times)))

((repeated sqr 2) 3)

((repeated add1 10) 1)

(define (repeated2 func times)
  (cond
    ((= times 0) (λ (x) x))
    (else (λ (x)
            (func ((repeated2 func (- times 1)) x))))))

;; (func ((x -> func ((repeated2 ..) x)) x))

((repeated2 add1 10) 1)

((repeated sqr 2) 3)
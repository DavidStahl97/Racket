#lang racket

(define (as-memo-function f)
  (let ([table (make-hash)])
    (lambda (x)
      (let ([prev (hash-ref table x #f)])
        (or prev
            (let ([result (f x)])
              (hash-set! table x result)
              result))))))

;; Fakultät
(define fac
  (as-memo-function
    (λ (x)
      (if (= x 0) 1 (* x (fac (sub1 x)))))))

(fac 5)

(fac 5)


          
                                

#lang racket

(define (as-memo-function f)
  (let ([table (make-hash)])
    (lambda (x . args)
      (let* ([key (cons x args)]
             [prev (hash-ref table key #f)])
        (or prev
            (let ([result (apply f x args)])
              (hash-set! table key result)
              result))))))

(define addition
  (as-memo-function
    (λ (x . args)
      (if (empty? args)
          x
          (+ x (apply addition (first args) (rest args)))))))

(addition 3 2 3 2)

(addition 3 2 3 2)
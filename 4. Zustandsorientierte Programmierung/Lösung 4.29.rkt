#lang racket

(define (as-memo-function f)
  (define (calculate table)
    (lambda (x . args)
      (let* ([key (cons x args)]
             [prev (hash-ref table key #f)])
        (or prev
            (let ([result (apply f x args)])
              (hash-set! table key result)
              result)))))
  
  (let ([table (make-hash)])
    (λ (func)
      (cond
        ((equal? func 'get-table) table)
        ((equal? func 'calculate) (calculate table))
        (else (error "unknown function"))))))

(define addition
  (as-memo-function
    (λ (x . args)
      (if (empty? args)
          x
          (+ x (apply (addition 'calculate) (first args) (rest args)))))))

((addition 'calculate) 4 3 2 1)

(addition 'get-table)

((addition 'calculate) 5 4 3 2 1)

(addition 'get-table)
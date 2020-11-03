#lang racket

(define (identity x) x)

(define (atom? x)
  (or (number? x)
      (symbol? x)
      (string? x)
      (boolean? x)))

(define (reduce func start empty t)
  (cond ((null? t) empty)
        ((atom? t) (start t))
        (else (func (reduce func start empty (car t)) (reduce func start empty (cdr t))))))

(define example-tree '((1 . (4 . (2 . 1))) . (2 . 5)))

;; 1. count leaves
(define (count-leaves t)
  (reduce (λ (x y) (+ x y)) (λ (x) 1) 0 t))

(count-leaves example-tree)


;; 2. sum leaves
(define (sum-leaves t)
  (reduce (λ (x y) (+ x y)) identity 0 t))

(sum-leaves example-tree)


;; 3. mean
(define (mean t)
  (/ (sum-leaves t) (count-leaves t)))

(mean example-tree)


;; 4. depth
(define (depth t)
  (reduce (λ (a b) (+ 1 (max a b))) (λ (a) 1) 0 t))
    
(depth example-tree)


;; 5. generate leaves list
(define (leaves-list t)
  (reduce (λ (x y) (append x y)) (λ (x) (list x)) '() t))

(leaves-list example-tree)
#lang racket

(define (pair-func pair first second)
  (cons (first (car pair)) (second (cdr pair))))

(pair-func (cons 1 2) (lambda (value) (+ 5 value)) (lambda (value) (* value value)))

(define (identity value) value)

;; example tree
(define example-tree
  (list
     5
     (list
        3
        (list
           2
           1
           4)
     )
     3
     5
     (list
       1
       3)
  )
)  

;; reduce list
(define (reduce-list lst start func)
  (cond
    ((null? lst) start)
    (else
     (reduce-list (rest lst) (func (first lst) start) func))))

;; test reduce list
(define (my-sum a b) (+ a b))
(reduce-list (list 1 4 2 3) 1 my-sum)

;; reduce tree
(define (reduce-tree tree start leave-func node-func)
  (define (calculate-node-value start)
    (pair-func start identity (lambda (value) (node-func value))))

  (define (calculate-leave-value start leave)
    (pair-func start (lambda (value) (leave-func value leave)) identity))
  
  (define (reduce-child child start)
    (cond
      ((list? child)
        (reduce-tree child (calculate-node-value start) leave-func node-func))

      (else (calculate-leave-value start child))))
  
  (define (iterate-children tree start)
    (reduce-list tree start reduce-child))

  (iterate-children tree start))

;; reduce nodes tree
(define (default-leave-func number leave) 0)

(define (reduce-tree-nodes tree node-start node-func)
  (cdr (reduce-tree tree (cons 0 node-start) default-leave-func node-func)))

;; reduce leaves tree
(define (default-nodes-func value) 0)

(define (reduce-tree-leaves tree leave-start leave-func)
  (car (reduce-tree tree (cons leave-start 0) leave-func default-nodes-func)))

    
    
;; count leaves
(define (count-leaves tree)
  (define (my-count number leave) (+ number 1))  
  (reduce-tree-leaves tree 0 my-count))

(count-leaves example-tree)

;; sum leaves
(define (sum-leaves tree)
  (reduce-tree-leaves tree 0 my-sum))

(sum-leaves example-tree)

;; calculate mean
(define (calculate-mean tree)
  (define (increment-mean value leave) (cons (+ (car value) leave) (+ (cdr value) 1)))
  (define (divide-sum-count value) (/ (car value) (cdr value)))
  
  (divide-sum-count (reduce-tree-leaves tree (cons 0 0) increment-mean)))

(calculate-mean example-tree)

;; calculate tree depth
(define (caluclate-tree-depth tree)
  (reduce-tree-nodes tree 0 (lambda (x) (+ x 1))))

(caluclate-tree-depth example-tree)

;; create flat list
(define (create-flat-list tree)
  (reduce-tree-leaves tree '() (lambda (lst leave) (append lst (list leave)))))

(create-flat-list example-tree)



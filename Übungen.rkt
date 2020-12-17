#lang racket

(require rackunit)

; 1.1 Einfügesort

;; fügt ein Element in eine schon sortierte Liste ein
(define (insert-element lst element)
  (if (null? lst)
      (list element)
      (if (< element (first lst))
          (cons element lst)
          (cons (first lst) (insert-element (rest lst) element)))))

(check-equal? (insert-element (list 1 2 4 5) 3) (list 1 2 3 4 5))
(check-equal? (insert-element '() 5) (list 5))
(check-equal? (insert-element (list 3 5 6) 7) (list 3 5 6 7))

;; sortiert eine Liste nach dem Einfügesort-Algorithmus:
;; nacheinander werden die Elemente aus der unsortierten Liste in eine sortierte eingefügt
(define (insert-sort unsorted-list)
  (define (insert-sort-iter lst sorted-list)
    (if (null? lst)
        sorted-list
        (insert-sort-iter (rest lst) (insert-element sorted-list (first lst)))))

  (insert-sort-iter unsorted-list '()))

(check-equal? (insert-sort (list 2 4 1 6 4)) (list 1 2 4 4 6))
(check-equal? (insert-sort '()) '())


; 1.2 Flatten

;; Rekursiv wird der Baum untersucht.
;; Die Blätter, die keine Liste sind, werden als Liste zurückgegeben
;; und dann werden die Blätter durch append zusammengeführt.
(define (flatten tree)
  (if (list? tree)
      (foldr append '()
             (map (λ (child) (flatten child)) tree))
      (list tree)))


(check-equal? (flatten '((a) b (c (d) e) ())) '(a b c d e))
(check-equal? (flatten '()) '())
(check-equal? (flatten '(a (b c (d)) e (f))) '(a b c d e f))


; 1.3 Ströme

;; cons-stream
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)
(define (tail s) (force (cdr s)))

(define the-empty-stream '())
(define empty-stream? null?)

;; Datenabstraktion zur Vereinfachung
(define (make-fib-pair n fib) (list n fib))
(define (part-fib fib-pair) (first (rest fib-pair)))
(define (part-n fib-pair) (first fib-pair))

(define (calc-fib-pair fib1 fib2)
  (make-fib-pair (add1 (part-n fib2))
                 (+ (part-fib fib1) (part-fib fib2))))

;; Zusammenführung der zwei Ströme
(define (add-fib-pair s1 s2)
  (cons-stream (calc-fib-pair (head s1) (head s2))
               (add-fib-pair (tail s1) (tail s2))))

;; Der eigentliche Fib-Stream
(define fib-stream2
    (cons-stream (make-fib-pair 0 0)
               (cons-stream (make-fib-pair 1 1)
                            (add-fib-pair fib-stream2 (tail fib-stream2)))))

(define fib-stream (λ () fib-stream2))

;; Tests
(define a (fib-stream))

(check-equal? (head a) '(0 0))
(check-equal? (head (tail a)) '(1 1))
(check-equal? (head (tail (tail a))) '(2 1))
(check-equal? (head (tail (tail (tail a)))) '(3 2))
(check-equal? (head (tail (tail (tail (tail a))))) '(4 3))
(check-equal? (head (tail (tail (tail (tail (tail a)))))) '(5 5))
(check-equal? (head (tail (tail (tail (tail (tail (tail a))))))) '(6 8))

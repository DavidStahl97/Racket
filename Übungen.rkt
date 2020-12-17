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



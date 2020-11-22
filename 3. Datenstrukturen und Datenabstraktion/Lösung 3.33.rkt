#lang racket

;; Key-Value pair for typed values

(define (labelled-data type data)
  (λ (func)
    (cond
      ((equal? func 'type) type)
      ((equal? func 'contents) data)
      (else (error "unknown function")))))

;; Konstruktoren für die jeweiligen Zahlen
(define real 1)
(define complex 2)

(define (make-real-number real-number)
  (labelled-data 'real real-number))

(define (make-complex-number complex-number)
  (labelled-data 'complex complex-number))

;; Komplexe Zahlen bilden wir auch mit einem Paar:

(define (complex-contents real-part imaginary-part)
  (cons real-part imaginary-part))

;; Bspw.

((make-real-number 2.4) 'contents)

;; 3+4i
((make-complex-number (complex-contents 3 4)) 'contents)

;; Selektoren

(define (re-part complex-number)
  (car (complex-number 'contents)))

(define (im-part complex-number)
  (cdr (complex-number 'contents)))


;; Types

(define (+real-real x y)
   (+ (x 'contents) (y 'contents)))

(define (+real-complex x y)
  (complex-contents
   (+ (x 'contents) (re-part y))
   (im-part y)))

(define (+complex-real x y)
  (complex-contents
   (+ (re-part x) (y 'contents))
   (im-part x)))

(define (+complex-complex x y)
  (complex-contents
   (+ (re-part x) (re-part y))
   (+ (im-part y) (im-part y))))

;; Hash-Table

(define OP-TABLE (make-hash))

(define (add-op op type-1 type-2 func res-type table)
  (hash-set! table (list op type-1 type-2) (list func res-type)))

(add-op 'plus 'real 'real +real-real 'real OP-TABLE)
(add-op 'plus 'real 'complex +real-complex 'complex OP-TABLE)
(add-op 'plus 'complex 'real +complex-real 'complex OP-TABLE)
(add-op 'plus 'complex 'complex +complex-complex 'complex OP-TABLE)

;; Analog dann für 'multiply ....

(define (op-res-type lst)
  (second lst))

(define (op-func lst)
  (first lst))

(define (find-op op type-1 type-2 table)
  (hash-ref table (list op type-1 type-2)))
  
(define (get-func op type-1 type-2 op-tbl)
  (op-func (find-op op type-1 type-2 op-tbl)))

(define (get-type op type-1 type-2 op-tbl)
  (op-res-type (find-op op type-1 type-2 op-tbl)))

;; Beispiel Werte
(define x (make-real-number 2.4))
(define y (make-complex-number (complex-contents 3 4)))

;;
;; Nun wird "plus" zu:
;;

(define (plus x y)
  (labelled-data
   (get-type 'plus (x 'type) (y 'type) OP-TABLE)
   ((get-func 'plus (x 'type) (y 'type) OP-TABLE) x y)))

((plus x y) 'contents)
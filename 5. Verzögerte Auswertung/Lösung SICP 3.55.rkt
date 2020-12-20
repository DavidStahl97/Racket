#lang racket

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)

(define (tail s) (force (cdr s)))

(define the-empty-stream '())
(define empty-stream? null?)


(define (ganze-zahlen-range a)
  (cons-stream a (ganze-zahlen-range (add1 a))))

(define ganze-zahlen (ganze-zahlen-range 1))

(define (map-stream func s1 s2)
  (cons-stream (func (head s1) (head s2)) (map-stream func (tail s1) (tail s2))))

(define (add-stream s1 s2)
  (map-stream + s1 s2))


(define (teil-summen stream)
  (cons-stream (head stream)
               (add-stream (tail stream) (teil-summen stream))))
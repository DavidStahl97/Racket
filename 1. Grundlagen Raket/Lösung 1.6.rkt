#lang racket

(define (replace str i)
  (string-append-immutable (substring str 0 (- i 1)) "_" (substring str i)))

(replace "Gutentag" 3)
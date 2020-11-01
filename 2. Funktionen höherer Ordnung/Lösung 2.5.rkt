#lang racket

(foldl / 1 '(2 3))

(foldr / 2 '(3))

(foldl cons '(x) '(1 2 3 4))

(foldr cons '(x) '(1 2 3 4))

;; damit foldr und foldl äquivalent sind, müssen sie kommutativ und assoziativ sein
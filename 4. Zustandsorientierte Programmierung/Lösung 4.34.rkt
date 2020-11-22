#lang racket

;; Ein Bankkonto
;; - Einrichten
;; - Einzahlen und Abheben

(define (make-account start password)
  (define money start)

  (define (withdraw amount)
    (cond
      ((>= money amount)
       (set! money (- money amount))
       money)
      (else "You're out of money!")))

  (define (deposit amount)
    (set! money (+ money amount))
    money)

  (define (dispatch entered m)
    (if (not (equal? password entered)) (error "invalid password")
        (cond
          ((equal? m 'withdraw) withdraw)
          ((equal? m 'deposit) deposit)
          ((equal? m 'balance) money)
          (else (error "Unknown request!")))))

  dispatch)

(define x (make-account 150 'secret))

(x 'secret 'balance)

((x 'secret 'deposit) 100)

(x 'secret 'balance)

(x 'wrong 'balance)
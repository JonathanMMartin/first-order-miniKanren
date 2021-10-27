#lang racket
(provide domain-satisfiable)
(require "microk-fo.rkt")

;; universal quantification
(define (domain-satisfiable domain state)
  (cond
    ((eq? domain '()) #t)
    (else (if (mature (pause state domain)) #t #f))))

  
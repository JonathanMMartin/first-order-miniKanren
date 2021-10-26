#lang racket
(provide domain-satisfiable)
(require "mk-fo.rkt")

;; universal quantification
(define (domain-satisfiable domain state)
  (if (mature (pause state domain)) #t #f))
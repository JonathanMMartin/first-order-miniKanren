#lang racket
(provide domain-satisfiable)
(require "microk-fo.rkt")

;; universal quantification
(define (domain-candidates domain state)
  (pause state domain))

(define (negate-domain g v)
  (match (g)
    ((conj g1 g2) (let ((g1 (negate-domain g1 v)) (g2 (negate-domain g2 v))) (if g1 (disj g1 g2) g2)))
    ((disj g1 g2) (let ((g1 (negate-domain g1 v)) (g2 (negate-domain g2 v))) (if g1 (conj g1 g2) g2)))
    ((== t1 t2) (if (or (eqv? t1 v) (eqv? t2 v)) #f (=/= t1 t2)))
    ((=/= t1 t2) (if (or (eqv? t1 v) (eqv? t2 v)) #f (== t1 t2)))
    (_ #f)
    ; TODO : Add type negations 
    ))

#|
(forall (v) domain body)

(mplus (pause st (negate-domain domain v))
       other-stuff)

|#


#|
    add some stuff to handle goal->state, state->goal so we can work with implication easily

    add some stuff to convert a for all into an implication

    startegies:
    1) falsify antecedent
    2) satisfy consequent
|#



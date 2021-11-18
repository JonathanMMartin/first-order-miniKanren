#lang racket
(provide domain-satisfiable)
(require "microk-fo.rkt")

;; universal quantification
(define (domain-candidates domain state)
  (pause state domain))

(define (negate-goal g)
  (match (g)
    ((conj g1 g2) (disj (negate-goal g1) (negate-goal g2)))
    ((disj g1 g2) (conj (negate-goal g1) (negate-goal g2)))
    ((== t1 t2) (=/= t1 t2))
    ((=/= t1 t2) (== t1 t2))
    (_ #f)
    ; TODO
    ; Add type negations 
    ; Give error with user defined relations
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
        a) create the substituion such that inner scope -> outer scope    a = v,  v->a
           so when we walk variables, it will always point to the outside
        b) if we can make one of the subs fail, we are done
        c) add the inequalities as subs?
    2) satisfy consequent

    deal with user defined relations later on
|#



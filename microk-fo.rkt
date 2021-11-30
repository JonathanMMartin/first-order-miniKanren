#lang racket
(provide
  (all-from-out "common.rkt")
  (struct-out disj)
  (struct-out conj)
  (struct-out relate)
  (struct-out ==)
  (struct-out =/=)
  (struct-out symbolo)
  (struct-out stringo)
  (struct-out numbero)
  (struct-out not-symbolo)
  (struct-out not-stringo)
  (struct-out not-numbero)
  (struct-out imply)
  (struct-out forallo)
  (struct-out mplus)
  (struct-out bind)
  (struct-out pause)
  step
  mature
  mature?)

(require "common.rkt")

;; first-order microKanren
(struct true     ()                      #:prefab)
(struct false    ()                      #:prefab)
(struct disj    (g1 g2)                  #:prefab)
(struct conj    (g1 g2)                  #:prefab)
(struct relate  (thunk description)      #:prefab)
(struct ==      (t1 t2)                  #:prefab)
(struct =/=     (t1 t2)                  #:prefab)
(struct symbolo (t)                      #:prefab)
(struct stringo (t)                      #:prefab)
(struct numbero (t)                      #:prefab)
(struct not-symbolo (t)                  #:prefab)
(struct not-stringo (t)                  #:prefab)
(struct not-numbero (t)                  #:prefab)
(struct imply   (g1 g2)                  #:prefab)
(struct forallo (v g)                    #:prefab)
(struct bind    (bind-s bind-g)          #:prefab)
(struct mplus   (mplus-s1 mplus-s2)      #:prefab)
(struct pause   (pause-state pause-goal) #:prefab)

(define (mature? s) (or (not s) (pair? s)))
(define (mature s)
  (if (mature? s) s (mature (step s))))

;; Implication
(define (negate-goal g)
  (match g
    ((true)  (false))
    ((false) (true))
    ((conj g1 g2) (disj (negate-goal g1) (negate-goal g2)))
    ((disj g1 g2) (conj (negate-goal g1) (negate-goal g2)))
    ((== t1 t2) (=/= t1 t2))
    ((=/= t1 t2) (== t1 t2))
    ((symbolo t) (not-symbolo t))
    ((stringo t) (not-stringo t))
    ((numbero t) (not-numbero t))
    ((not-symbolo t) (symbolo t))
    ((not-stringo t) (stringo t))
    ((not-numbero t) (numbero t))
    ((imply g1 g2) (conj g1 (negate-goal g2)))
    ;((forallo v g) (let ((v (var/fresh (quote v)))) (negate-goal g)))  ; TODO : Make sure this is right. Is quote right?
    (_ (error "unnegateable goal" g))
    ))

(define (state->goal st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (types (state-types st))
         (not-types (state-not-types st)))
    (sub->goal sub (true))))

(define (sub->goal sub acc)
  (match sub
    ('() acc)
    ((cons (cons x y) rest) (sub->goal rest (conj (== x y) acc)))))

(define (types->goal types acc)
  (match types
    ('() acc)
    ((cons (cons v type?) rest) (types->goal rest (conj type->goal-helper v type?) acc))))

(define (type->goal-helper u type?)
  (cond
    ((eq? type? symbol?) (symbolo u))
    ((eq? type? string?) (stringo u))
    ((eq? type? number?) (numbero u))
    (error "Invalid type")))

(define (start st g)
  (match g
    ((true) (state->stream st))
    ((false) (state->stream #f))
    ((disj g1 g2)
     (step (mplus (pause st g1)
                  (pause st g2))))
    ((conj g1 g2)
     (step (bind (pause st g1) g2)))
    ((relate thunk _)
     (pause st (thunk)))
    ((== t1 t2) (state->stream (unify t1 t2 st)))
    ((=/= t1 t2) (state->stream (disunify t1 t2 st)))
    ((symbolo t) (state->stream (typify t symbol? st)))
    ((stringo t) (state->stream (typify t string? st)))
    ((numbero t) (state->stream (typify t number? st)))
    ((not-symbolo t) (state->stream (not-typify t symbol? st)))
    ((not-stringo t) (state->stream (not-typify t string? st)))
    ((not-numbero t) (state->stream (not-typify t number? st)))
    ((imply g1 g2)
     (step (mplus (pause st (negate-goal g1))
                  (pause st (conj g1 g2)))))
    ; ((forallo v (imply g1 g2))
    ;  (let ((candidates (step (pause st (conj g1 g2)))))
    ;   (if candidates
    ;       (error "There are more candidates in the implies")
    ;       (error "forall failed, no candidates"))))
    ((forallo v g)
     (let ((candidates (step (pause st g))))
        (and candidates
             (let* ((can (car candidates))
                    (v-state (get-constraints can v))
                    (v-goal (state->goal (get-constraints can v))))
                (displayln (state->goal can))
                (displayln (state->goal v-state))
                (if (true? v-goal)
                    (state->stream can)
                    (if (equal? can v-state) ;; TODO basically fix this
                        (pause st (forallo v (conj (negate-goal v-goal) g)))
                        (pause st (forallo v (imply (negate-goal v-goal) g)))))))))
    ))

; constraints we get are identical to can --> don't care about ordering differences
  ; build conjunction --> (negate-goal v-goal) g
; else build imply --> (we care about both cases in the imply)

; true/false instead of all/none

; (forallo v (imply (=/= v 1) (== v 1)))
; (== v 1) --> v=1
; (conj (=/= v 1) (== v 1)) --> no answer

(define (step s)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (step s1))))
       (cond ((not s1) s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus s2 (cdr s1))))
             (else (mplus s2 s1)))))
    ((bind s g)
     (let ((s (if (mature? s) s (step s))))
       (cond ((not s) #f)
             ((pair? s)
              (step (mplus (pause (car s) g)
                           (bind (cdr s) g))))
             (else (bind s g)))))
    ((pause st g) (start st g))
    (_            s)))

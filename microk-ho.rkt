#lang racket
(provide
  (all-from-out "common.rkt")
  true
  false
  disj
  conj
  relate
  ==
  =/=
  symbolo
  stringo
  numbero
  not-symbolo
  not-stringo
  not-numbero

  existo

  mplus
  bind
  pause
  
  mature
  mature?)


(require "common.rkt")

;; higher-order microKanren








































(define (mature? s) (or (not s) (pair? s)))
(define (mature s)
  (if (mature? s) s (mature (s))))



(define (true) (lambda (st) (state->stream st)))
(define (false) (lambda (st) (state->stream #f)))
(define (disj g1 g2)
  (lambda (st) (mplus (pause st g1)
                      (pause st g2))))
(define (conj g1 g2)
  (lambda (st) (bind (pause st g1) g2)))
(define (relate thunk _)
  (lambda (st) (pause st (thunk))))
(define (== t1 t2) (lambda (st) (state->stream (unify t1 t2 st))))
(define (=/= t1 t2) (lambda (st) (state->stream (disunify t1 t2 st))))
(define (symbolo t) (lambda (st) (state->stream (typify t symbol? st))))
(define (stringo t) (lambda (st) (state->stream (typify t string? st))))
(define (numbero t) (lambda (st) (state->stream (typify t number? st))))
(define (not-symbolo t) (lambda (st) (state->stream (not-typify t symbol? st))))
(define (not-stringo t) (lambda (st) (state->stream (not-typify t string? st))))
(define (not-numbero t) (lambda (st) (state->stream (not-typify t number? st))))

(define (existo v g) (lambda (st) (pause (add-to-scope v 'e st) g)))







(define (mplus s1 s2)
  (let ((s1 (if (mature? s1) s1 (s1))))
    (cond ((not s1) s2)
          ((pair? s1)
           (cons (car s1)
                 (lambda () (mplus s2 (cdr s1)))))
          (else (lambda () (mplus s2 s1))))))
(define (bind s g)
  (let ((s (if (mature? s) s (s))))
    (cond ((not s) #f)
          ((pair? s)
           (mplus (pause (car s) g)
                  (lambda () (bind (cdr s) g))))
          (else (lambda () (bind s g))))))
(define (pause st g) (lambda () (g st)))
;

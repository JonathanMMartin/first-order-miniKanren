#lang racket
(provide
  ==
  =/=
  symbolo
  stringo
  numbero
  pairo
  listo
  not-symbolo
  not-stringo
  not-numbero
  not-pairo
  not-listo
  imply
  universal
  normalize-goal
  disj
  conj
  var

  define-relation
  fresh
  conde
  forall
  implication
  query
  run
  run*

  stream-take
  conj*
  disj*
  )
(require "microk-fo.rkt")
(include "mk-syntax.rkt")

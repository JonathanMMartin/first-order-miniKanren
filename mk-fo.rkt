#lang racket
(provide
  ==
  =/=
  symbolo
  stringo
  numbero
  pairo
  not-symbolo
  not-stringo
  not-numbero
  not-pairo
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
  query
  run
  run*

  stream-take
  conj*
  disj*
  )
(require "microk-fo.rkt")
(include "mk-syntax.rkt")

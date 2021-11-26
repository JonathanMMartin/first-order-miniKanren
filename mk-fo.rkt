#lang racket
(provide
  ==
  =/=
  symbolo
  numbero
  stringo
  not-symbolo
  not-numbero
  not-stringo
  imply
  forallo

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

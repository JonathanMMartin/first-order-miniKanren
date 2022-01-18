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
  

  define-relation
  fresh
  conde
  query
  run
  run*

  stream-take
  conj*
  disj*
  )
(require "microk-ho.rkt")
(include "mk-syntax.rkt")

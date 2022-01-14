(display "\nRunning Implication tests")
(newline)

(test 'implication-1
  (run* (x) (imply (== x 1) (=/= x 2)))
  '((_.0)))

(test 'implication-2
  (run* (x) (imply (=/= x 1) (== x 2)))
  '((1) (2)))

(test 'implication-3
  (run* (x) (imply (numbero x) (== x 501)))
  '((501) #s(Ans (_.0) ((not-types (_.0 num))))))

(test 'implication-4
  (run* (x) (imply (numbero x) (not-symbolo x)))
  '((_.0)))

(test 'implication-type-constraint-1
  (run* (x) (imply (=/= x 1) (numbero x)))
  '(#s(Ans (_.0) ((num _.0)))))

(test 'implication-mult-var-1
  (run* (x y z) (imply (== (list x y) (list y z)) (== z x)))
  '((_.0 _.1 _.2)))
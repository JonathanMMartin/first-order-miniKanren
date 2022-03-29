(display "\nRunning Implication tests")
(newline)

(test 'implication-1
  (run* (x) (implication ((== x 1)) ((=/= x 2))))
  '((_.0)))

(test 'implication-2
  (run* (x) (implication ((=/= x 1)) ((== x 2))))
  '((1) (2)))

(test 'implication-3
  (run* (x) (implication ((numbero x)) ((== x 501))))
  '(#s(Ans (_.0) ((not-types (_.0 num)))) (501)))

(test 'implication-4
  (run* (x) (implication ((numbero x)) ((not-symbolo x))))
  '((_.0)))

(test 'implication-type-constraint-1
  (run* (x) (implication ((=/= x 1)) ((numbero x))))
  '(#s(Ans (_.0) ((num _.0)))))

(test 'implication-mult-var-1
  (run* (x y z) (implication ((== (list x y) (list y z))) ((== z x))))
  '((_.0 _.1 _.2)))

(test 'implication-complicated-0
  (run* (x y) (implication ((numbero x) (symbolo y)) ((=/= x y))))
  '((_.0 _.1)))
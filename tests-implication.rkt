(display "\nRunning Implication tests")
(newline)

(test 'implication-1
  (run* (x) (imply (== x 1) (=/= x 2)))
  '(#s(Ans (_.0) ((=/= ((_.0 1))))) #s(Ans (_.0) ((=/= ((_.0 2)))))))

(test 'implication-2
  (run* (x) (imply (=/= x 1) (== x 2)))
  '((1) (2)))

(test 'implication-3
  (run* (x) (imply (numbero x) (== x 501)))
  '(#s(Ans (_.0) ((not-types (_.0 num)))) (501)))

(test 'implication-4
  (run* (x) (imply (numbero x) (not-symbolo x)))
  '(#s(Ans (_.0) ((not-types (_.0 num)))) #s(Ans (_.0) ((not-types (_.0 sym))))))

(test 'implication-type-constraint-1
  (run* (x) (imply (=/= x 1) (numbero x)))
  '((1) #s(Ans (_.0) ((num _.0)))))

(test 'implication-mult-var-1
  (run* (x y z) (imply (== (list x y) (list y z)) (== z x)))
  '((_.0 _.1 _.2)))
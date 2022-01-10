(display "\nRunning Universal Quantitification tests")
(newline)

(test 'forall-test-0
  (run* (x) (forall (v) (== 0 1)))
  '())

(test 'forall-test-1
  (run* (x) (forall (v) (== x 1)))
  '((1)))

(test 'forall-test-3
  (run 1 (x) (forall (v) (== x x)))
  '((_.0)))

(test 'forall-test-3 
  (run 1 (x) (forall (v) (conj (== x 0) (== x 1))))
  '())

(test 'forall-test-4
  (run 2 (x) (forall (v) (disj (== x 0) (== x 1))))
  '((0) (1)))

(test 'forall-test-5
 (run 1 (x) (forall (v) (== v 1)))
 '())

(test 'forall-test-6
 (run 1 (x) (forall (v) (=/= v 1)))
 '())

(test 'forall-test-7
 (run 1 (x) (forall (v) (disj (== v 1) (=/= v 1))))
 '((_.0)))

(test 'forall-test-8
 (run 1 (x) (forall (v) (disj (== v 1) (=/= 1 v))))
 '((_.0)))

(test 'forall-test-9
 (run 1 (x) (forall (v) (conj (== v 1) (=/= 1 v))))
 '())

(test 'forall-test-10
 (run 1 (x) (forall (v) (disj (== v 1) (== x 1))))
 '((1)))

(test 'forall-test-11
 (run 1 (x) (forall (v) (conj (=/= x x) (== v 1))))
 '())

(test 'forall-test-12
 (run 1 (x) (forall (v) (== x v)))
 '())

(test 'forall-test-13
 (run 1 (x) (forall (v) (== v x)))
 '())

(test 'forall-test-14
 (run 1 (x) (forall (v) (=/= x v)))
 '())

(test 'forall-test-15
 (run 1 (x) (forall (v) (=/= v x)))
 '())

(test 'forall-test-16
 (run 1 (x) (forall (v) (disj (== v 1) (imply (== v 1) (== 2 3)))))
 '((_.0)))

(test 'forall-test-17
 (run 1 (x) (forall (v) (disj (== v 1) (disj (numbero v) (imply (== v 1) (== 2 3))))))
 '((_.0)))

(test 'forall-test-18
  (run 1 (x) (forall (v) (imply (=/= x v) (=/= 1 v))))
  '((1)))

(test 'forall-test-19
  (run 1 (x) (forall (v) (imply (== x v) (== v 1))))
  '((1)))
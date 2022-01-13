(display "\nRunning Universal Quantitification tests")
(newline)

(test 'forall-no-var-test-0
  (run* (x) (forall (v) (== 0 1)))
  '())

(test 'forall-no-var-test-1
  (run* (x) (forall (v) (== x 1)))
  '((1)))

(test 'forall-no-var-test-2
  (run 1 (x) (forall (v) (== x x)))
  '((_.0)))

(test 'forall-no-var-test-3 
  (run 1 (x) (forall (v) (conj (== x 0) (== x 1))))
  '())

(test 'forall-no-var-test-4
  (run 2 (x) (forall (v) (disj (== x 0) (== x 1))))
  '((0) (1)))

(test 'forall-test-0
 (run 1 (x) (forall (v) (== v 1)))
 '())

(test 'forall-test-1
 (run 1 (x) (forall (v) (=/= v 1)))
 '())

(test 'forall-test-2
  (run 1 (x) (forall (v) (== v v)))
  '((_.0)))

(test 'forall-test-3
 (run 1 (x) (forall (v) (disj (== v 1) (=/= v 1))))
 '((_.0)))

(test 'forall-test-4
 (run 1 (x) (forall (v) (disj (== v 1) (=/= 1 v))))
 '((_.0)))

(test 'forall-test-5
 (run 1 (x) (forall (v) (conj (== v 1) (=/= 1 v))))
 '())

(test 'forall-test-6
 (run 1 (x) (forall (v) (disj (== v 1) (== x 1))))
 '((1)))

(test 'forall-test-7
 (run 1 (x) (forall (v) (conj (=/= x x) (== v 1))))
 '())

(test 'forall-test-8
 (run 1 (x) (forall (v) (== x v)))
 '())

(test 'forall-test-9
 (run 1 (x) (forall (v) (== v x)))
 '())

(test 'forall-test-10
 (run 1 (x) (forall (v) (=/= x v)))
 '())

(test 'forall-test-11
 (run 1 (x) (forall (v) (=/= v x)))
 '())

(test 'forall-imply-test-0
 (run 1 (x) (forall (v) (disj (== v 1) (imply (== v 1) (== 2 3)))))
 '((_.0)))

(test 'forall-imply-test-1
 (run 1 (x) (forall (v) (disj (== v 1) (disj (numbero v) (imply (== v 1) (== 2 3))))))
 '((_.0)))

(test 'forall-imply-test-2
  (run 1 (x) (forall (v) (imply (=/= x v) (=/= 1 v))))
  '((1)))

(test 'forall-imply-test-3
  (run 1 (x) (forall (v) (imply (== x v) (== v 1))))
  '((1)))

(test 'forall-multi-var-0
   (run 1 (x) (forall (v w) (disj (== v w) (=/= v w))))
   '((_.0)))

(test 'forall-multi-var-1
   (run 1 (x) (forall (v w) (disj (=/= v w) (conj (== x v) (== x w)))))
   '())

(test 'forall-multi-var-2
   (run 1 (x) (forall (v w) (disj (conj (== x v) (== x w)) (=/= v w))))
   '())

(test 'forall-exists-0
   (run 1 (x) (forall (v) (fresh (u) (== v 1))))
   '())

(test 'forall-exists-1
   (run 1 (x) (forall (v) (fresh (u) (== u 1))))
   '((_.0)))

(test 'forall-exists-2
   (run 1 (x) (fresh (v) (forall (u) (== v u))))
   '())

(test 'forall-exists-3
   (run 1 (x) (fresh (v) (forall (u) (=/= v u))))
   '())

(test 'forall-exists-4
   (run 1 (x) (forall (v) (fresh (u) (=/= v u))))
   '((_.0)))

(test 'forall-exists-5
   (run 1 (x) (forall (v) (fresh (u) (== v u))))
   '((_.0)))

(test 'forall-disj-0
   (run 1 (x) (forall (v) (disj (numbero v) (=/= v 1))))
   '((_.0)))

(test 'forall-disj-1
   (run 1 (x) (forall (v) (disj (symbolo v) (numbero v))))
   '())

(test 'forall-forall-0
   (run 1 (x) (forall (v) (forall (u) (== v u))))
   '())

(test 'forall-forall-1
   (run 1 (x) (forall (v) (forall (u) (=/= v u))))
   '())

(test 'forall-forall-imply-0
   (run 1 (v) (forall (x) (forall (y) (imply (conj (numbero x) (symbolo y)) (=/= x y)))))
   '((_.0)))
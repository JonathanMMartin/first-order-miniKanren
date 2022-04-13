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
  (run 1 (x) (forall (v) (== x 0) (== x 1)))
  '())

(test 'forall-no-var-test-4
  (run 2 (x) (forall (v) (conde ((== x 0)) ((== x 1)))))
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
 (run 1 (x) (forall (v) (conde ((== v 1)) ((=/= v 1)))))
 '((_.0)))

(test 'forall-test-4
 (run 1 (x) (forall (v) (conde ((== v 1)) ((=/= 1 v)))))
 '((_.0)))

(test 'forall-test-5
 (run 1 (x) (forall (v) (== v 1) (=/= 1 v)))
 '())

(test 'forall-test-6
 (run 1 (x) (forall (v) (conde ((== v 1)) ((== x 1)))))
 '((1)))

(test 'forall-test-7
 (run 1 (x) (forall (v) (=/= x x) (== v 1)))
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
 (run 1 (x) (forall (v) 
               (conde ((== v 1)) 
                      ((implication ((== v 1)) ((== 2 3)))))))
 '((_.0)))

(test 'forall-imply-test-1
 (run 1 (x) (forall (v) 
               (conde ((== v 1)) 
                      ((numbero v)) 
                      ((implication ((== v 1)) ((== 2 3)))))))
 '((_.0)))

(test 'forall-imply-test-2
 (run 1 (x) (forall (v) (implication ((=/= x v)) ((=/= 1 v)))))
 '((1)))

(test 'forall-imply-test-3
 (run 1 (x) (forall (v) (implication ((== x v)) ((== v 1)))))
 '((1)))

(test 'forall-multi-var-0
 (run 1 (x) (forall (v w) (conde ((== v w)) ((=/= v w)))))
 '((_.0)))

(test 'forall-multi-var-1
 (run 1 (x) (forall (v w) 
               (conde ((=/= v w)) 
                      ((== x v) (== x w)))))
 '())

(test 'forall-multi-var-2
 (run 1 (x) (forall (v w) 
               (conde ((== x v) (== x w)) 
                      ((=/= v w)))))
 '())

(test 'forall-existential-0
 (run 1 (x) (forall (v) (fresh (u) (== v 1))))
 '())

(test 'forall-existential-1
 (run 1 (x) (forall (v) (fresh (u) (== u 1))))
 '((_.0)))

(test 'forall-existential-2
 (run 1 (x) (fresh (v) (forall (u) (== v u))))
 '())

(test 'forall-existential-3
 (run 1 (x) (fresh (v) (forall (u) (=/= v u))))
 '())

(test 'forall-existential-4
 (run 1 (x) (forall (v) (fresh (u) (=/= v u))))
 '((_.0)))

(test 'forall-existential-5
 (run 1 (x) (forall (v) (fresh (u) (== v u))))
 '((_.0)))

(test 'forall-disj-0
 (run 1 (x) (forall (v) (conde ((numbero v)) ((=/= v 1)))))
 '((_.0)))

(test 'forall-disj-1
 (run 1 (x) (forall (v) (conde ((symbolo v)) ((numbero v)))))
 '())

(test 'forall-disj-2
 (run 1 (x) (forall (v) 
               (conde ((symbolo v))
                      ((numbero v)) 
                      ((stringo v)))))
 '())

(test 'forall-disj-3
 (run 1 (x) (forall (v) (conde ((not-symbolo v)) ((not-numbero v)))))
 '((_.0)))

(test 'universal-disj-4
 (run 1 (x) (forall (v) 
               (conde ((not-symbolo v)) 
                      ((not-numbero v)) 
                      ((not-stringo v)))))
 '((_.0)))

(test 'forall-disj-5
   (run 1 (x) (forall (v) (conde ((not-stringo v)) ((numbero v)))))
   '())

(test 'forall-disj-6
   (run 1 (x) (forall (v) (conde ((== 1 v)) ((== 2 v)))))
   '())

(test 'forall-disj-7
   (run 1 (x) (forall (v) (conde ((=/= 1 v)) ((=/= 2 v)))))
   '((_.0)))

(test 'forall-conj-0
   (run 1 (x) (forall (v) (numbero v) (=/= v 1)))
   '())

(test 'forall-conj-1
   (run 1 (x) (forall (v) (symbolo v) (numbero v)))
   '())

(test 'forall-conj-2
   (run 1 (x) (forall (v) (symbolo v) (numbero v) (stringo v)))
   '())

(test 'forall-conj-3
   (run 1 (x) (forall (v) (not-symbolo v) (not-numbero v)))
   '())

(test 'universal-conj-4
   (run 1 (x) (forall (v) (not-symbolo v) (not-numbero v) (not-stringo v)))
   '())

(test 'forall-conj-5
   (run 1 (x) (forall (v) (not-stringo v) (numbero v)))
   '())

(test 'forall-conj-6
   (run 1 (x) (forall (v) (== 1 v) (== 2 v)))
   '())

(test 'forall-conj-7
   (run 1 (x) (forall (v) (=/= 1 v) (=/= 2 v)))
   '())

(test 'forall-forall-0
   (run 1 (x) (forall (v) (forall (u) (== v u))))
   '())

(test 'forall-forall-1
   (run 1 (x) (forall (v) (forall (u) (=/= v u))))
   '())

(test 'forall-forall-imply-0
   (run 1 (v) (forall (x) (forall (y) (implication ((numbero x) (symbolo y)) ((=/= x y))))))
   '((_.0)))

(test 'forall-nested-disj-0
   (run 1 (v) (forall (x) (forall (y) (conde ((symbolo x)) ((numbero y))))))
   '())

(test 'ende-basic-0
   (run 1 (q) (forall (x) (== x q)))
   '())

(test 'ende-basic-1
   (run 1 (q) (forall (x) (fresh (y) (== x y))))
   '((_.0)))

(test 'ende-basic-2
   (run 1 (q) (forall (x) (fresh (y) (== x y) (== y q))))
   '())

(test 'ende-basic-3
   (run 1 (q) (forall (x) (== q (cons 1 x))))
   '())

(test 'ende-basic-4
   (run 1 (q) (forall (x) (fresh (y) (== y (cons 1 x)))))
   '((_.0)))

(test 'ende-basic-5
   (run 1 (q) (forall (x) (fresh (y) (== x (cons 1 y)))))
   '())

(test 'ende-basic-6
   (run 1 (q) (forall (x) (=/= x q)))
   '())

(test 'ende-basic-7
   (run 1 (q) (forall (x) (fresh (y) (=/= x y))))
   '((_.0)))

(test 'ende-basic-8
   (run 1 (q) (forall (x) (fresh (y) (=/= x y) (== y q))))
   '())

(test 'ende-basic-9
   (run 1 (q) (forall (x) (=/= q (cons 1 x))))
   '())

(test 'ende-basic-10
   (run 1 (q) (conj* (fresh (x) (== q (cons 1 x))) (forall (x) (=/= q (cons 1 x)))))
   '())

(test 'ende-basic-11
   (run 1 (q) (forall (x) (=/= (cons x x) (cons 0 1))))
   '((_.0)))

(test 'ende-basic-12
   (run 1 (q) (forall (x) (=/= (cons x x) (cons 1 1))))
   '())

(test 'ende-basic-13
   (run 1 (q) (forall (x) (=/= (cons x x) (cons q 1))))
   '(#s(Ans (_.0) ((=/= ((_.0 1)))))))

(test 'ende-basic-14
   (run 1 (q) (fresh (a b) (== q (cons a b)) (forall (x) (=/= (cons x x) (cons a b))) ))
   '(#s(Ans ((_.0 . _.1)) ((=/= ((_.0 _.1)))))))

(test 'ende-basic-15
   (run 1 (q) (forall (z) (fresh (x y) (== (cons z y) x))))
   '((_.0)))

(test 'ende-basic-16
   (run 1 (q) (forall (v) (disj* (== v 1) (=/= v 1) (== v 2))))
   '((_.0)))

(test 'ende-basic-17
   (run 1 (q) (fresh (a b) (forall (v)  (disj* (== v a) (=/= v b)))))
   '((_.0)))

(test 'ende-basic-18
   (run 1 (a b) (forall (v) (implication ((== v a) (=/= v b)) ((=/= v v)))))
   '((_.0 _.0)))

(test 'ende-basic-19
   (run 1 (a) (conj* (stringo a) (forall (z) (not-stringo a))))
   '())

(test 'ende-basic-20
   (run 1 (a b) (forall (z) (disj* (== z a) (=/= z b))))
   '((_.0 _.0)))

(test 'ende-basic-21
 (run 1 (R) (forall (x y) 
                (conde ((=/= y (cons 'a 'b))) 
                       ((=/= x y)) 
                       ((== y R)))))
 '(((a . b))))
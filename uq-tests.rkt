; domain satisfiablity
(test 'domain-sat-no-restriction
  (domain-satisfiable '() empty-state)
  #t)

(test 'domain-sat-trival-restriction
  (domain-satisfiable (== 1 1) empty-state)
  #t)

(test 'domain-sat-contradiction
  (domain-satisfiable (=/= 1 1) empty-state)
  #f)

(test 'domain-sat-x=x
  (let ((x (var/fresh 'x)))
    (domain-satisfiable (== x x) empty-state))
  #t)

(test 'domain-sat-x=/=x
  (let ((x (var/fresh 'x)))
    (domain-satisfiable (=/= x x) empty-state))
  #f)

(test 'domain-sat-x=y-no-restriction
  (let ((x (var/fresh 'x))
        (y (var/fresh 'y)))
    (domain-satisfiable (== x y) empty-state))
  #t)

(test 'domain-sat-x=/=y-no-restriction
  (let ((x (var/fresh 'x))
        (y (var/fresh 'y)))
    (domain-satisfiable (=/= x y) empty-state))
  #t)

(test 'domain-sat-x=/y-in-state-with-x=y
  (let ((x (var/fresh 'x))
        (y (var/fresh 'y)))
    (domain-satisfiable (=/= x y) (car (unify x y empty-state))))
  #f)

(test 'domain-sat-x=y-in-state-with-x=/=y
  (let ((x (var/fresh 'x))
        (y (var/fresh 'y)))
    (domain-satisfiable (== x y) (car (disunify x y empty-state))))
  #f)

(test 'domain-sat-conde-unify-1
   (let ((x (var/fresh 'x)))
     (domain-satisfiable (conde ((== x 6)) ((== x 90))) (car (unify x 6 empty-state))))
   #t)

(test 'domain-sat-conde-unify-2
   (let ((x (var/fresh 'x)))
     (domain-satisfiable (conde ((== x 6)) ((== x 90))) (car (unify x 90 empty-state))))
   #t)

(test 'domain-sat-conde-disunify-1
   (let ((x (var/fresh 'x)))
     (domain-satisfiable (conde ((== x 6)) ((== x 90))) (car (disunify x 6 empty-state))))
   #t)

(test 'domain-sat-conde-disunify-2
   (let ((x (var/fresh 'x)))
     (domain-satisfiable (conde ((== x 6)) ((== x 90))) (car (disunify x 90 empty-state))))
   #t)

(test 'domain-sat-conde-disunify-3
   (let ((x (var/fresh 'x)))
     (domain-satisfiable (conde ((== x 6)) ((== x 90))) (car (disunify x 6 (car (disunify x 90 empty-state))))))
   #f)
    
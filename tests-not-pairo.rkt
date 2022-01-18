(display "\nRunning Not pairo tests")
(newline)

(test 'not-pairo-0
  (run* (x) (not-pairo x))
  '(#s(Ans (_.0) ((not-types (_.0 pair))))))

(test 'not-pairo-1
  (run 1 (x) (=/= x (cons 1 2)) (not-pairo x))
  '(#s(Ans (_.0) ((not-types (_.0 pair))))))

(test 'not-pairo-2
  (run 1 (x) (not-pairo x) (=/= x (cons 45 65)))
  '(#s(Ans (_.0) ((not-types (_.0 pair))))))

(test 'not-pairo-fail-0
  (run 1 (x) (not-pairo (cons 748 935)))
  '())
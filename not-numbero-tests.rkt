(display "\nRunning Not numbero tests")
(newline)

(test 'not-numbero-0
  (run* (x) (not-numbero x))
  '(#s(Ans (_.0) ((not-types (_.0 num))))))

(test 'not-numbero-1
  (run 1 (x) (=/= x 678) (not-numbero x))
  '(#s(Ans (_.0) ((not-types (_.0 num))))))

(test 'not-numbero-2
  (run 1 (x) (not-numbero x) (=/= x 678))
  '(#s(Ans (_.0) ((not-types (_.0 num))))))

(test 'not-numbero-fail-0
  (run 1 (x) (not-numbero 1729))
  '())
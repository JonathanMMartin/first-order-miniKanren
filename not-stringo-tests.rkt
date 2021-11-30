(display "\nRunning Not stringo tests")
(newline)

(test 'not-stringo-0
  (run* (x) (not-stringo x))
  '(#s(Ans (_.0) ((not-types (_.0 str))))))

(test 'not-stringo-1
  (run 1 (x) (== x 5) (not-stringo x))
  '((5)))

(test 'not-stringo-2
  (run 1 (x) (not-stringo x) (== x 5))
  '((5)))

(test 'not-stringo-3
  (run 1 (x) (=/= x 43) (not-stringo x))
  '(#s(Ans (_.0) ((=/= ((_.0 43))) (not-types (_.0 str))))))

(test 'not-stringo-4
  (run 1 (x) (not-stringo x) (=/= x 43))
  '(#s(Ans (_.0) ((=/= ((_.0 43))) (not-types (_.0 str))))))

(test 'not-stringo-5
  (run 1 (x) (numbero x) (not-stringo x))
  '(#s(Ans (_.0) ((num _.0)))))

(test 'not-stringo-6
  (run 1 (x) (not-stringo x) (numbero x))
  '(#s(Ans (_.0) ((num _.0)))))

(test 'not-stringo-fail-0
  (run 1 (x) (not-stringo "world"))
  '())

(test 'not-stringo-fail-1
  (run 1 (x y) (stringo x) (not-stringo y) (== x y))
  '())

(test 'not-stringo-fail-2
  (run 1 (x y) (not-stringo y) (stringo x) (== x y))
  '())
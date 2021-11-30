(display "\nRunning Not symbolo tests")
(newline)

(test 'not-symbolo-0
  (run 1 (x) (not-symbolo x))
  '(#s(Ans (_.0) ((not-types (_.0 sym))))))

(test 'not-symbolo-1
  (run 1 (x y) (numbero y) (not-symbolo x) (== x y))
  '(#s(Ans (_.0 _.0) ((num _.0)))))

(test 'not-symbolo-2
  (run 1 (x y z) (== x y) (not-symbolo z) (=/= y 7) (== x z))
  '(#s(Ans (_.0 _.0 _.0) ((=/= ((_.0 7))) (not-types (_.0 sym))))))

(test 'not-symbolo-fail-0
  (run 1 (x) (not-symbolo 'hello))
  '())
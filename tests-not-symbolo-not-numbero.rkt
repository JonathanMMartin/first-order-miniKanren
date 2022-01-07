(display "\nRunning Not symbolo not numbero tests")
(newline)

(test 'not-symbolo-not-numbero-0
  (run 1 (x y) (== x 5) (not-symbolo x) (not-numbero y) (not-symbolo y))
  '(#s(Ans (5 _.0) ((not-types (_.0 num sym))))))

(test 'not-symbolo-not-numbero-1
  (run 1 (x y) (not-numbero x) (not-symbolo y) (== x y))
  '(#s(Ans (_.0 _.0) ((not-types (_.0 num sym))))))

(test 'not-symbolo-not-numbero-2
  (run 1 (x y) (=/= x 7) (not-numbero y) (=/= y 'h) (not-symbolo x) (== x y))
  '(#s(Ans (_.0 _.0) ((not-types (_.0 num sym))))))
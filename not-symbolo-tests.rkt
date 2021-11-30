(display "\nRunning Not symbolo tests")
(newline)

(test 'not-symbolo-0
  (run 1 (x) (not-symbolo x))
  '(#s(Ans (_.0) ((not-types (_.0 sym))))))

(test 'not-symbolo-fail-0
  (run 1 (x) (not-symbolo 'hello))
  '())
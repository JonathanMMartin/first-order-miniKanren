(display "\nRunning Not listo tests")
(newline)

(test 'not-listo-0
  (run* (x) (not-listo x))
  '(#s(Ans (_.0) ((not-types (_.0 list))))))

(test 'not-listo-1
  (run 1 (x) (=/= x (cons 1 '())) (not-listo x))
  '(#s(Ans (_.0) ((not-types (_.0 list))))))

(test 'not-listo-2
  (run 1 (x) (not-listo x) (=/= x (cons 45 '())))
  '(#s(Ans (_.0) ((not-types (_.0 list))))))

(test 'not-listo-3
  (run 1 (x) (not-listo (cons 748 935)))
  '((_.0)))

(test 'not-listo-4
  (run 1 (x) (=/= x (cons 1 2)) (not-listo x))
  '(#s(Ans (_.0) ((=/= ((_.0 (1 . 2)))) (not-types (_.0 list))))))

(test 'not-listo-5
  (run 1 (x) (not-listo x) (=/= x (cons 45 65)))
  '(#s(Ans (_.0) ((=/= ((_.0 (45 . 65)))) (not-types (_.0 list))))))

(test 'not-listo-fail-0
  (run 1 (x) (not-listo '()))
  '())

(test 'not-listo-fail-1
  (run 1 (x) (not-listo (cons 748 '())))
  '())
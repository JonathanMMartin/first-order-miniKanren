(display "\nRunning listo tests")
(newline)

(test 'listo-0
  (run* (x) (listo x))
  '(#s(Ans (_.0) ((list _.0)))))

(test 'listo-1
  (run* (x) (== (cons 1 '()) x) (listo x))
  '(((1))))

(test 'listo-2
  (run* (x) (listo x) (== (cons 1 '()) x))
  '(((1))))

(test 'listo-3
  (run* (x) (=/= x (cons 5 '())) (listo x)) 
  '(#s(Ans (_.0) ((=/= ((_.0 (5)))) (list _.0)))))

(test 'listo-4
  (run* (x) (listo x) (== (cons 64 (cons 39 (cons 18 '()))) x))
  '(((64 39 18))))

(test 'listo-fail-0
  (run* (x) (== (cons 1 2) x) (listo x))
  '())

(test 'listo-fail-1
  (run* (x) (listo x) (== (cons 1 2) x))
  '())
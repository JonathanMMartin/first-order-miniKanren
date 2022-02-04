(display "\nRunning pairo tests")
(newline)

(test 'pairo-0
  (run* (x) (pairo x))
  '(#s(Ans (_.0) ((pair _.0)))))

(test 'pairo-1
  (run* (x) (== (cons 1 2) x) (pairo x))
  '(((1 . 2))))

(test 'pairo-2
  (run* (x) (pairo x) (== (cons 1 2) x))
  '(((1 . 2))))

(test 'pairo-3
  (run* (x) (=/= x (cons 5 6)) (pairo x)) 
  '(#s(Ans (_.0) ((=/= ((_.0 (5 . 6)))) (pair _.0)))))
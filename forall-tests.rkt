(display "\nRunning Universal Quantitification tests")
(newline)

(test 'forall-test-0
  (run* (x) (forall (v) (== x 1)))
  '((1)))

(test 'forall-test-1
  (run* (x) (forall (v) (== 0 1)))
  '())

(test 'forall-test-2
  (run 1 (x) (forall (v) (== v 1)))
  '())

; (test 'forall-test-3
;   (run 1 (x) (forall (v) (== x v)))
;   '())

; (test 'forall-test-4
;   (run 1 (x) (forall (v) (imply (=/= x v) (=/= 1 v))))
;   '(1))
(display "\nRunning Undecidable Universal Quantitification tests")
(newline)

(test 'undeciable-0
   (run 1 (q) (forall (v) (appendo '() '() '())))
   '((_.0)))

(test 'undeciable-1
   (run 1 (q) (forall (v) (appendo '() '() v)))
   '())

(test 'undeciable-2
   (run 1 (q) (forall (v) (appendo '() v '())))
   '())

(test 'undeciable-3
   (run 1 (q) (forall (v) (appendo v '() '())))
   '())

(test 'undeciable-4a 
   (run 1 (q) (forall (v) (appendo '() v v)))
   '((_.0)))

(test 'undeciable-4b 
   (run 1 (q) (forall (v) (appendo q v v)))
   '((())))

(test 'undeciable-5a
   (run 1 (q) (forall (v) (appendo v '() v)))
   '())

(test 'undeciable-5b
   (run 1 (q) (forall (v) (fresh (a) (imply (== v (cons a '())) (appendo v '() v)))))
   '((_.0)))

(test 'undeciable-5b-0.5
    (run 1 (q) (forall (v) (fresh (a) (imply (== v (cons a '())) (appendo v 1 v)))))
    '((_.0)))

(test 'undeciable-5b-1
   (run 1 (q) (forall (v) (fresh (a) (imply (== v (cons a '())) (appendo v q v)))))
   '((_.0)))

(test 'undeciable-5b-1.5
   (run 1 (q) (forall (v) (forall (a) (=/= v (cons a '())))))
   '())

(test 'undeciable-5b-2
   (run 1 (q) (forall (v) (forall (a) (imply (== v (cons a '())) (appendo v q v)))))
   '((())))

(test 'undeciable-5c                         
   (run 1 (q) (forall (v) (imply (fresh (a) (== v (cons a '()))) (appendo v '() v))))
   '((_.0)))

(test 'undeciable-5c-1
   (run 1 (q) (forall (v) (imply (fresh (a) (== v (cons a '()))) (appendo v q v))))
   '((())))

(test 'undeciable-5d
   (run 1 (q) (forall (v) (imply (listo v) (appendo v '() v))))
   '((_.0)))

(test 'undeciable-5d-1
   (run 1 (q) (forall (v) (imply (listo v) (appendo v q v))))
   '((())))

(test 'undeciable-5d-2
   (run 2 (q) (forall (v) (imply (listo v) (appendo v q v))))
   '((())))

(test 'undeciable-6
   (run 1 (q) (forall (v) (appendo v v '())))
   '())

(test 'undeciable-7
   (run 1 (q) (forall (v) (appendo v v v)))
   '())

(test 'undeciable-8
   (run 1 (q) (forall (x y z) (appendo x y z)))
   '())


(display "\nRunning Rewrite Rules tests")
(newline)

(test 'DNF-basic-0
  (normalize-goal (== 1 2))
  #s(== 1 2))

(test 'DNF-basic-1
  (normalize-goal (conj (== 2 34) (=/= 7 2)))
  #s(conj #s(== 2 34) #s(=/= 7 2)))

(test 'DNF-basic-2
  (normalize-goal (disj (numbero 3) (== 6 10)))
  #s(disj #s(numbero 3) #s(== 6 10)))

(test 'DNF-basic-3
  (normalize-goal (disj 1 1))
  1)

(test 'DNF-basic-4
  (normalize-goal (conj 1 1))
  1)

(test 'DNF-test-0
  (normalize-goal (disj (disj (numbero 'a) (symbolo 1)) (== 6 10)))
  #s(disj #s(numbero a) #s(disj #s(symbolo 1) #s(== 6 10))))

(test 'DNF-test-1
  (normalize-goal (conj (conj (not-numbero 21) (symbolo 'l)) (=/= 98 60)))
  #s(conj #s(not-numbero 21) #s(conj #s(symbolo l) #s(=/= 98 60))))

(test 'DNF-test-2
  (normalize-goal (conj (== 1 2) (disj (== 3 4) (== 5 6))))
  #s(disj #s(conj #s(== 1 2) #s(== 3 4)) #s(conj #s(== 1 2) #s(== 5 6))))

(test 'DNF-test-3
  (normalize-goal (disj (conj 1 2) 3))
  #s(disj 3 #s(conj 1 2)))

(test 'DNF-test-4
  (normalize-goal (disj 3 (conj 1 2)))
  #s(disj 3 #s(conj 1 2)))

(test 'DNF-test-5
  (normalize-goal (disj (conj (disj 1 2) 3) 4))
  #s(disj #s(conj 1 3) #s(disj #s(conj 2 3) 4)))

(test 'DNF-test-6
  (normalize-goal (disj (conj (disj 1 2) 3) (conj (disj 4 5) 6)))
  #s(disj #s(conj 1 3) #s(disj #s(conj 2 3) #s(disj #s(conj 4 6) #s(conj 5 6)))))

(test 'DNF-test-7
  (normalize-goal (disj (disj 1 2) (conj (disj 3 4) 5)))
  #s(disj 1 #s(disj 2 #s(disj #s(conj 3 5) #s(conj 4 5)))))

(test 'DNF-test-8
  (normalize-goal (disj (conj (disj (disj 1 2) (conj (disj 3 4) 5)) 6) (conj (disj 7 8) 9)))
  #s(disj #s(conj 1 6) #s(disj #s(conj 2 6) #s(disj #s(conj 3 #s(conj 5 6)) #s(disj #s(conj 4 #s(conj 5 6)) #s(disj #s(conj 7 9) #s(conj 8 9)))))))

(test 'DNF-test-9
  (normalize-goal (disj (conj 1 2) (conj 1 2)))
  #s(conj 1 2))
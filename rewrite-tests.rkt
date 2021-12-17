(display "\nRunning Rewrite Rules tests")
(newline)

(test 'DNF-test-0
  (normalize-goal (disj (disj (== 3 4) (=/= 5 6)) (conj (disj (== 'a 'a) (symbolo 'b)) (== 'c 'd))))
  #s(disj #s(== 3 4) #s(disj #s(=/= 5 6) #s(disj #s(conj #s(== a a) #s(== c d)) #s(conj #s(symbolo b) #s(== c d))))))

(test 'DNF-test-1
  (normalize-goal (disj (conj (disj (disj (== 1 2) (== 3 4)) (conj (disj (== 7 8) (== 9 10)) (== 6 5))) (=/= 5 6)) (conj (disj (== 'a 'a) (symbolo 'b)) (== 'c 'd))))
  '())

(test 'DNF-test-2
  (normalize-goal (disj (conj (disj (== 1 2) (== 3 4)) (=/= 5 6)) (conj (disj (== 'a 'a) (symbolo 'b)) (== 'c 'd))))
  '())

(test 'DNF-test-3
  (normalize-goal (disj (conj (disj (== 1 2) (== 3 4)) (=/= 5 6)) (== 5 6)))
  '())

;  #s(disj #s(disj #s(conj #s(== 1 2) #s(=/= 5 6)) #s(conj #s(== 3 4) #s(=/= 5 6))) #s(disj #s(conj #s(== a a) #s(== c d)) #s(conj #s(symbolo b) #s(== c d))))


; #s(disj #s(disj #s(disj #s(conj #s(== 1 2) #s(=/= 5 6)) #s(conj #s(== 3 4) #s(=/= 5 6))) #s(disj #s(conj #s(== 7 8) #s(conj #s(== 6 5) #s(=/= 5 6))) #s(conj #s(== 9 10) #s(conj #s(== 6 5) #s(=/= 5 6))))) #s(disj #s(conj #s(== a a) #s(== c d)) #s(conj #s(symbolo b) #s(== c d))))


; #s(disj #s(== 3 4) #s(disj #s(=/= 5 6) #s(disj #s(conj #s(== a a) #s(== c d)) #s(conj #s(symbolo b) #s(== c d)))))
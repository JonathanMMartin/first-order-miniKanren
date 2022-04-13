; (define (badgoal? g)
;     (displayln "checking for bad goal")
;     (match g
;         ((universal (list (var v 2)) (existential (list (var v.lhs 7) (var e.then 10) (var e.else 11)) 
;                                 (conj (list 
;                                         a 
;                                         b 
;                                         c)))) #t)
;         (_ #f)))
(define (badgoal? g) #t)
#lang racket
(provide
  (all-from-out "common.rkt")
  (struct-out true)
  (struct-out false)
  (struct-out disj)
  (struct-out conj)
  (struct-out relate)
  (struct-out ==)
  (struct-out =/=)
  (struct-out symbolo)
  (struct-out stringo)
  (struct-out numbero)
  (struct-out pairo)
  (struct-out listo)
  (struct-out not-symbolo)
  (struct-out not-stringo)
  (struct-out not-numbero)
  (struct-out not-pairo)
  (struct-out not-listo)
  (struct-out imply)
  (struct-out existential)
  (struct-out universal)
  (struct-out mplus)
  (struct-out bind)
  (struct-out pause)
  step
  mature
  mature?
  normalize-goal)

(require "common.rkt")

(define verbose? #f)
(define logs? #f)
(define badgoal-check? #t)

;; first-order microKanren
(struct true        ()                       #:prefab)
(struct false       ()                       #:prefab)
(struct disj        (glst)                   #:prefab)
(struct conj        (glst)                   #:prefab)
(struct relate      (thunk description)      #:prefab)
(struct not-relate  (thunk description)      #:prefab)
(struct ==          (t1 t2)                  #:prefab)
(struct =/=         (t1 t2)                  #:prefab)
(struct symbolo     (t)                      #:prefab)
(struct stringo     (t)                      #:prefab)
(struct numbero     (t)                      #:prefab)
(struct pairo       (t)                      #:prefab)
(struct listo       (t)                      #:prefab)
(struct not-symbolo (t)                      #:prefab)
(struct not-stringo (t)                      #:prefab)
(struct not-numbero (t)                      #:prefab)
(struct not-pairo   (t)                      #:prefab)
(struct not-listo   (t)                      #:prefab)
(struct imply       (g1 g2)                  #:prefab)
(struct existential (vlst g)                 #:prefab)
(struct universal   (vlst g)                 #:prefab)
(struct bind        (bind-s bind-g)          #:prefab)
(struct mplus       (mplus-s1 mplus-s2)      #:prefab)
(struct pause       (pause-state pause-goal) #:prefab)

(include "badgoals.rkt")

#|
  Input: g is a goal
  Output: negation of g
|#
(define (negate-goal g)
  (cond
    ((true? g)        (false))
    ((false? g)       (true))
    ((disj? g)        (conj (map negate-goal (disj-glst g))))
    ((conj? g)        (disj (map negate-goal (conj-glst g))))
    ((==? g)          (=/= (==-t1 g) (==-t2 g)))
    ((=/=? g)         (== (=/=-t1 g) (=/=-t2 g)))
    ((typeo? g)       (type->goal (typeo-t g) (typeo->type? g) #t))
    ((not-typeo? g)   (type->goal (typeo-t g) (typeo->type? g)))
    ((imply? g)       (conj (list (imply-g1 g) (negate-goal (imply-g2 g)))))
    ((existential? g) (universal (existential-vlst g) (negate-goal (existential-g g))))
    ((universal? g)   (existential (universal-vlst g) (negate-goal (universal-g g))))
    ((relate? g)      (not-relate (relate-thunk g) (relate-description g)))
    ((not-relate? g)  (relate (not-relate-thunk g) (not-relate-description g)))
    (else             (error "Unnegateable goal" g))))

(define (mature? s) (or (not s) (pair? s)))
(define (mature s)
  (if (mature? s) s (mature (step s))))

(define (start st g)
  (let* ((g (simplify g))
         (g (combine-diseqs g)))
    (match g
      ((true) (state->stream st))
      ((false) (state->stream #f))
      ((disj (cons g1 '())) 
        (step (pause st g1)))
      ((disj (cons g1 g2))
        (step (mplus (pause st g1)
                     (pause st (disj g2)))))
      ((conj (cons g1 '())) 
        (step (pause st g1)))
      ((conj (cons g1 g2))
        (step (bind (pause st g1) (conj g2))))
      ((relate thunk _)
        (pause st (thunk)))
      ((not-relate thunk _)
        (pause st (negate-goal (thunk))))
      ((== t1 t2) (state->stream (unify t1 t2 st)))
      ((=/= t1 t2) (state->stream (disunify t1 t2 st)))
      ((symbolo t) (state->stream (typify t symbol? st)))
      ((stringo t) (state->stream (typify t string? st)))
      ((numbero t) (state->stream (typify t number? st)))
      ((pairo t)   (state->stream (typify t pair? st)))
      ((listo t)   (state->stream (typify t list? st)))
      ((not-symbolo t) (state->stream (not-typify t symbol? st)))
      ((not-stringo t) (state->stream (not-typify t string? st)))
      ((not-numbero t) (state->stream (not-typify t number? st)))
      ((not-pairo t)   (state->stream (not-typify t pair? st)))
      ((not-listo t)   (state->stream (not-typify t list? st)))
      ((imply g1 g2)
        (step (mplus (pause st (negate-goal g1))
                     (pause st (conj (list g1 g2))))))
      ((existential v g) (step (pause st g)))
      ((universal v g) (if (and badgoal-check? (badgoal? (universal v g))) (start st (false)) (error "not enough rules: forall" (universal v g))))
      )))

(define (step s)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (step s1))))
       (cond ((not s1) s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus s2 (cdr s1))))
             (else (mplus s2 s1)))))
    ((bind s g)
     (let ((s (if (mature? s) s (step s))))
       (cond ((not s) #f)
             ((pair? s)
              (step (mplus (pause (car s) g)
                           (bind (cdr s) g))))
             (else (bind s g)))))
    ((pause st g) (start st g))
    (_            s)))

#|
  Input: g is a goal
  Output: True if and only if g does not use any user-defined relations
|#
(define (decidable? g)
  (cond
    ((true? g)         #t)
    ((false? g)        #t)
    ((==? g)           #t)
    ((=/=? g)          #t)
    ((disj/conj? g)    (andmap decidable? (disj/conj-glst g)))
    ((typeo? g)        #t)
    ((not-typeo? g)    #t)
    ((imply? g)        (and (decidable? (imply-g1 g)) (decidable? (imply-g2 g))))
    ((existential? g)  (decidable? (existential-g g)))
    ((universal? g)    (decidable? (universal-g g)))
    ((relate? g)       #f)
    ((not-relate? g)   #f)
    (else              #f)))

#|
  Input: g is a goal
  Explores different branches of disjunctions available in g to see if
  we can create a goal that does not use any user-defined relations.
  Output: True if and only if g has a branch that does not use any user-defined relations
|#
(define (has-answer? g)
  (cond
    ((true? g)         #t)
    ((false? g)        #t)
    ((==? g)           #t)
    ((=/=? g)          #t)
    ((disj? g)         (ormap has-answer? (disj-glst g)))
    ((conj? g)         (andmap has-answer? (conj-glst g)))
    ((typeo? g)        #t)
    ((not-typeo? g)    #t)
    ((imply? g)        (and (has-answer? (imply-g1 g)) (has-answer? (imply-g2 g))))
    ((existential? g)  (has-answer? (existential-g g)))
    ((universal? g)    (has-answer? (universal-g g)))
    ((relate? g)       #f)
    ((not-relate? g)   #f)
    (else              #f)))

#|
  Input: g is a goal such that (has-answer g) is true.
  Output: Returns a branch of g that does not use any user-defined relations.
|#
(define (extract-answer g)
  (cond
    ((true? g)         (cons (true) (false)))
    ((false? g)        (cons (false) (false)))
    ((==? g)           (cons g (false)))
    ((=/=? g)          (cons g (false)))
    
    ((disj? g)         (let* ((glst (disj-glst g))
                              (g1 (car glst))
                              (g2 (disj (cdr glst)))
                              (a (if (has-answer? g1) (extract-answer g1) (extract-answer g2)))
                              (A (car a))
                              (B (cdr a))
                              (C (if (has-answer? g1) g2 g1)))
                          (cons A (disj (list B C)))))
    
    ((conj? g)         (let* ((glst (conj-glst g))
                              (g1 (car glst))
                              (g2 (cdr glst))
                              (g2 (if (null? g2) (true) (conj g2)))
                              (g1 (extract-answer g1))
                              (g2 (extract-answer g2))
                              (A (car g1))
                              (B (cdr g1))
                              (C (car g2))
                              (D (cdr g2)))
                          (cond
                            ((and (false? B) (false? D)) (cons (conj (list A C)) (false)))
                            ((false? B)                  (cons (conj (list A C)) (conj (list A D))))
                            ((false? D)                  (cons (conj (list A C)) (conj (list B C))))
                            (else                        (cons (conj (list A C)) (disj (list (conj (list A D)) (conj (list B C)) (conj (list B D)))))))))    
    
    ((typeo? g)        (cons g (false)))
    ((not-typeo? g)    (cons g (false)))
    
    ((imply? g)        (let* ((g1 (imply-g1 g))
                              (g2 (imply-g2 g))
                              (a (extract-answer g2)))
                          (cons (imply g1 (car a)) (imply g1 (cdr a))))) 
    
    ((existential? g)  (let ((a (extract-answer (existential-g g))))
                         (cons (existential (existential-vlst g) (car a)) (existential (existential-vlst g) (cdr a)))))
    
    ((universal? g)    (let ((a (extract-answer (universal-g g))))
                         (cons (universal (universal-vlst g) (car a)) (universal (universal-vlst g) (cdr a)))))
    
    ((relate? g)       (error "relate does not have an answer to extract"))
    
    ((not-relate? g)   (error "relate does not have an answer to extract"))
    (else              (displayln g) (error "how did you get here?"))))

#|
  Input: g is a goal
  Output: Return g with one each user-defined relation unfolded only once
|#
(define (unfold g)
  (match g
    ((disj glst)          (disj (map unfold glst)))
    ((conj glst)          (conj (map unfold glst)))
    ((imply g1 g2)        (imply (unfold g1) (unfold g2)))
    ((existential v h)    (existential v (unfold h)))
    ((universal v h)      (universal v (unfold h)))
    ((relate thunk _)     (thunk))
    ((not-relate thunk _) (negate-goal (thunk)))
    (_                    g)))

#|
  Used for debugging purposes in verbose mode.
|#
(define (display-and-continue msg v [continue identity] [display-v? #f])
  (cond
    (display-v?   (display msg) (displayln v) (continue v))
    (else         (displayln msg) (continue v))))

#|
  Input: g is a goal
  Pretty-print g and write it into a file logs/goal-log#.txt
|#
(define pretty-g
  (let ((count 0))
    (lambda (g)
      (set! count (+ count 1))
      (call-with-output-file (string-append "logs/goal-log" (number->string count) ".txt")
        (lambda (out) (pretty-write g out))))))

#|
  Input: g is a goal
  Output: a goal that is equivalent to g and solvable by MiniKanren.
|#
(define (simplify g)
  (begin
    (if logs? (pretty-g g) #f)
    (if verbose? (display-and-continue "\nsimp g: " g (const #f) #t) #f)
    (let* ((g (float-existential g))
          (g (normalize-goal g))
          (inner-g (get-inner-goal g)))
      (begin
        (if logs? (pretty-g g) #f)
        (if verbose? (display-and-continue "\nnorm g: " g (const #f) #t) #f)
        (cond
          ((decidable? g) (if verbose? (display-and-continue "g is dec" g) g))
          ((has-answer? inner-g) (let* ((extracted (extract-answer inner-g))
                                        (answer (normalize-goal (car extracted)))
                                        (remaining (cdr extracted))
                                        (g1 (set-inner-goal g (normalize-extracted answer)))
                                        (g2 (set-inner-goal g remaining))) ;(conj (negate-goal answer) remaining))))
                                    (if verbose? (display-and-continue "\ngoal after extraction: " (disj (list g1 g2)) identity #t) (disj (list g1 g2)))))
          (verbose?     (display-and-continue "nothing is good, we unfold" g (lambda (x) (simplify (unfold x))))) 
          (else         (simplify (unfold g))))))))

#|
  Input: g is a goal
         doublenegate? is true if and onlt if we want to use the double negation
                       rewrite rule.
  Applies all rewrite rules to g in a bottom up manner.
  Output: A goal that is equivalent to g.
|#
(define (normalize-goal g [double-negate? #t])
  (begin
    (if verbose? (display-and-continue "\nNormalizing: " g (const #f) #t) #f)
    (match g
      ;; Remove lists/pairs
      ((== (cons f1 r1) (cons f2 r2)) (normalize-goal (conj (list (== f1 f2) (== r1 r2)))))
      ((== (cons f1 r1) t2) (if (var? t2) (== (cons f1 r1) t2) (false)))
      ((== t1 (cons f2 r2)) (if (var? t1) (== (cons f2 r2) t1) (false)))

      ((=/= (cons f1 r1) (cons f2 r2)) (normalize-goal (disj (list (=/= f1 f2) (=/= r1 r2)))))
      ((=/= (cons f1 r1) t2) (if (var? t2) (=/= (cons f1 r1) t2) (true)))
      ((=/= t1 (cons f2 r2)) (if (var? t1) (=/= (cons f2 r2) t1) (true)))

      ((== t1 t2)             (cond
                                ((equal? t1 t2) (true))
                                ((and (var? t1) (term-use-var? t2 t1) (not (var? t2))) (false))
                                ((and (var? t2) (term-use-var? t1 t2) (not (var? t1))) (false))
                                ((and (or (contains-fresh? t1) (contains-fresh? t2)) (term<? t1 t2)) (== t1 t2))
                                ((or (contains-fresh? t1) (contains-fresh? t2)) (== t2 t1))
                                (else (false))))
      ((=/= t1 t2)            (cond
                                ((equal? t1 t2) (false))
                                ((and (var? t1) (term-use-var? t2 t1) (not (var? t2))) (true))
                                ((and (var? t2) (term-use-var? t1 t2) (not (var? t1))) (true))
                                ((and (or (contains-fresh? t1) (contains-fresh? t2)) (term<? t1 t2)) (=/= t1 t2))
                                ((or (contains-fresh? t1) (contains-fresh? t2)) (=/= t2 t1))
                                (else (true))))

      ((listo (cons _ y))    (normalize-goal (listo y)))

      ((disj (cons h '()))   (normalize-goal h))
      ((conj (cons h '()))   (normalize-goal h))


      ;; Recursive normalization
      ((disj glst)          (let* ((glst (map/shortcircuit normalize-goal '() glst (true) (false)))
                                   (glst (flatten-disj glst '()))
                                   (glst (sort glst goal-diseq-first<?)))
                              (match glst
                                ('() (false))
                                ((cons h '()) h)
                                ((cons h hlst)
                                  (if (or (=/=? h) (typeo? h) (not-typeo? h))
                                      (let* ((hlst (if (=/=? h)
                                                       (map (lambda (x) (substitute-term x (=/=-t2 h) (=/=-t1 h))) hlst)
                                                       (map (lambda (x) (apply-type x (typeo-t h) (typeo->type? h) (typeo? h))) hlst)))
                                             (hlst (normalize-goal (disj hlst))))
                                        (match hlst
                                          ((disj qlst) (disj (cons h qlst)))
                                          ((true) (true))
                                          ((false) h)
                                          (hlst   (disj (list h hlst)))))
                                      (disj (cons h hlst))))
                                (h h))))

      ;; Normalize conjunction                      
      ((conj glst)          (let* ((glst (map/shortcircuit normalize-goal '() glst (false) (true)))
                                   (glst (flatten-conj glst '()))
                                   (glst (sort glst goal<?)))
                              (match glst
                                ('() (true))
                                ((cons h '()) h)
                                ((cons h hlst)
                                  (if (or (==? h) (typeo? h) (not-typeo? h))
                                      (let* ((hlst (if (==? h)
                                                       (map (lambda (x) (substitute-term x (==-t2 h) (==-t1 h))) hlst)
                                                       (map (lambda (x) (apply-type x (typeo-t h) (typeo->type? h) (not-typeo? h))) hlst)))
                                             (hlst (normalize-goal (conj hlst))))
                                        (match hlst
                                          ((conj qlst) (conj (cons h qlst)))
                                          ((true) h)
                                          ((false) (false))
                                          (hlst   (conj (list h hlst)))))
                                      (conj (cons h hlst))))
                                (h h))))
      
      ((imply g1 g2)         (normalize-imply g1 g2))
      ((existential vlst h)  (normalize-existential vlst h double-negate?))
      ((universal vlst h)    (normalize-universal vlst h double-negate?))
      
      ;; Rewrites for user defined relations
      ((relate     thunk des) (normalize-relate thunk des #f))
      ((not-relate thunk des) (normalize-relate thunk des #t))
      
      (g                      (cond
                                ((typeo? g)     (let ((t (typeo-t g))) (if (var? t) g (if ((typeo->type? g) t) (true) (false))))) ;; Generalized type constraint
                                ((not-typeo? g) (let ((t (typeo-t g))) (if (var? t) g (if ((typeo->type? g) t) (false) (true))))) ;; Generalized not-type constraint
                                (else           g)))))) ;; No simplification possible

;; A version of map that allows for early stoping.
;; If (proc (car lst)) evaluates to the stop value, then we return a list containing just the stop value
;; If (proc (car lst)) evaluates to the skip value, then (proc (car lst)) is ommited from the returned list.
(define (map/shortcircuit proc acc lst stop skip)
  (if (null? lst)
      (reverse acc)
      (let ((g (proc (car lst))))
        (cond
          ((equal? g stop) (list stop))
          ((equal? g skip) (map/shortcircuit proc acc (cdr lst) stop skip))
          (else            (map/shortcircuit proc (cons g acc) (cdr lst) stop skip))))))

#|
  Turns binary disjunctions into a list of goals.
|#
(define (flatten-disj glst acc)
  (match glst
    ('()                     (reverse acc))
    ((cons (disj qlst) hlst) (flatten-disj (append qlst hlst) acc))
    ((cons h hlst)           (flatten-disj hlst (cons h acc)))
    (g                       g)))

#|
  Turns binary conjunctions into a list of goals.
|#
(define (flatten-conj glst acc)
  (match glst
    ('()                     (reverse acc))
    ((cons (conj qlst) hlst) (flatten-conj (append qlst hlst) acc))
    ((cons h hlst)           (flatten-conj hlst (cons h acc)))
    (g                       g)))

#|
  Input: ant (antecedent) is a goal
         con (consequent) is a goal
  Looks for any instances of ant in con and replace it with true.
  Output: con with instances of ant replaced with true.
|#
(define (replace-implication ant con)
  (if (conj? ant)
      (foldl (lambda (x acc) (replace-implication x acc)) con (conj-glst ant))
      (replace-assumption-with-true ant con)))

#|
  Input: ant (antecedent) is a goal
         con (consequent) is a goal
  Looks for any instances of ant in con and replace it with true.
  Output: con with instances of ant replaced with true.
|#
(define (replace-assumption-with-true ant con)
  (if (goal=? ant con)
      (true)
      (match con
        ((disj glst)        (disj (map (lambda (x) (replace-assumption-with-true ant x)) glst)))
        ((conj glst)        (conj (map (lambda (x) (replace-assumption-with-true ant x)) glst)))
        ((imply g1 g2)      (imply (replace-assumption-with-true ant g1) (replace-assumption-with-true ant g2)))
        ((existential v h)  (existential v (replace-assumption-with-true ant h)))
        ((universal v h)    (universal v (replace-assumption-with-true ant h)))
        (_                  con))))

#|
  Input: g1 is a goal
         g2 is a goal
  Apply implication normalization rules to g1 and g2.
  Output: A goal equivalent to (g1 => g2) with normalization rules applied bottom up.
|#
(define (normalize-imply g1 g2)
  (let ((g1 (normalize-goal g1)))
    (match g1
      ((true)  (normalize-goal g2))  ;; True -> A  = A
      ((false) (true))               ;; False -> A = True
      (_       (let* ((g2 (normalize-goal g2))
                      (g2 (normalize-goal (replace-implication g1 g2))))
                  (cond
                    ((true? g2) (true))
                    ((universal? g1) (normalize-goal (existential (universal-vlst g1) (imply (universal-g g1) g2)))) ;; (forall v A) -> B = exists v (A -> B)
                    ((existential? g1) (normalize-goal (universal (existential-vlst g1) (imply (existential-g g1) g2)))) ;; (forall v A) -> B = exists v (A -> B)
                    (else (let* ((g1 (normalize-goal (negate-goal g1))))
                      (normalize-goal (disj (list g1 g2)))))))))))  ;; A -> B = ~A or B

#|
  Input: t is a term
         vlst is a list of variables
  Output: Returns true if an only if at least one of the variables in vlst are used in term.
|#
(define (term-use-vlst? t vlst)
  (match vlst
    ('() #f)
    ((cons v r) (or (term-use-var? t v) (term-use-vlst? t r)))))

#|
  Input: vlst is a list of variables
         g is a goal
         double-negate? is true if and only if we want to apply double negation rules.
  Apply existential normalization rules to vlst and g.
  Output: A goal equivalent to (existential vlst g) with normalization rules applied bottom up.
|#
(define (normalize-existential vlst g [double-negate? #t])
  (let* ((g (normalize-goal g))
         (vlst (filter (lambda (x) (goal-use-var? g x)) vlst))
         (vlst (sort vlst var<?))
         (vlst (remove-duplicates vlst var=?)))
    (if (null? vlst)
        g
        (match g
          ((== _ t) (if (member t vlst) (true) (existential vlst g)))
          ((=/= _ _) (true))
          ((disj (cons (== _ t) glst)) (if (member t vlst) (true) (existential vlst g)))
          ((conj (cons (== _ t) glst)) (if (member t vlst) (normalize-goal (existential vlst (conj glst)) double-negate?) (existential vlst g)))
          ((disj glst) (let-values (((stay go) (partition (lambda (h) (ormap (lambda (x) (goal-use-var? h x)) vlst)) glst)))
                                 (if (null? go)
                                     (existential vlst g)
                                     (disj (cons (normalize-goal (existential vlst (disj stay)) double-negate?) go)))))
          ((existential ulst h) (normalize-goal (existential (append vlst ulst) h) double-negate?))
          (_ (cond
               ((or (typeo? g) (not-typeo? g)) (true))
               ((contains-equality-on-universal g vlst '()) (false))
               (else (existential vlst g))))))))

#|
  Input: g is a goal
         vlst is a list of variables
         acc is the accumulator (should start as empty list)
  Output: Returns true if and only if g forces an equality constraint on variables of vlst
          or any universally quantified variables in g.
|#
(define (contains-equality-on-universal g vlst acc)
  (match g
    ((== t1 t2) (or (and (term-use-vlst? t1 vlst) (term-use-vlst? t2 acc)) (and (term-use-vlst? t2 vlst) (term-use-vlst? t1 acc))))
    ((disj glst) (andmap (lambda (x) (contains-equality-on-universal x vlst acc)) glst))
    ((conj glst) (ormap (lambda (x) (contains-equality-on-universal x vlst acc)) glst))
    ((universal ulst h) (contains-equality-on-universal h vlst (append acc ulst)))
    ((existential ulst h) (contains-equality-on-universal h vlst acc))
    (_ #f)))

#|
  Input: vlst is a list of variables
         g is a goal
         double-negate? is true if and only if we want to apply double negation rules.
  Apply universal normalization rules to vlst and g.
  Output: A goal equivalent to (universal vlst g) with normalization rules applied bottom up.
|#
(define (normalize-universal vlst g [double-negate? #t] [induction? #t])
  (cond
    ((and #f induction? (imply? g)) ;; List induction rewrite rules
      (let* ((g1 (normalize-goal (imply-g1 g)))
             (g2 (normalize-goal (imply-g2 g))))
        (cond
          ((goal=? g1 g2) (true))
          ((and (listo? g1) (member (listo-t g1) vlst))
            (if verbose? (display-and-continue "\nUsing induction on: " (universal vlst (imply g1 g2)) (const #f) #t) #f)
            (let* ((t (listo-t g1))
                   (a (var/new 'a))
                   (base-case (substitute-term g2 t '()))
                   (antecedent g2)
                   (consequent (universal (cons a vlst) (substitute-term g2 t (cons a t))))
                   (inductive-step (universal vlst (imply antecedent (unfold consequent)))))
              (normalize-goal (conj (list base-case inductive-step)))))
          (else (normalize-universal vlst (imply g1 g2) double-negate? #f)))))
    (else 
      (let* ((g (normalize-goal g))
             (vlst (filter (lambda (x) (goal-use-var? g x)) vlst))
             (vlst (sort vlst var<?))
             (vlst (remove-duplicates vlst var=?)))
        (cond
          ((null? vlst) g)
          ((foldl (lambda (v acc) (or acc (contains-equality-on-v? g v) (contains-typeo-on-v? g v))) #f vlst) (false))
          ((or (typeo? g) (not-typeo? g)) (false))
          (else (match g
                  ((== _ _) (false))
                  ((=/= _ _) (false)) ;;* Is this bad? why or why not? 
                  ((disj (cons (=/= _ t) glst)) (if (member t vlst) (normalize-goal (universal vlst (disj glst))) (universal vlst g)))
                  ((conj (cons (=/= _ t) glst)) (if (member t vlst) (false) (universal vlst g)))
                  ((disj glst) (let-values (((stay go) (partition (lambda (h) (ormap (lambda (x) (goal-use-var? h x)) vlst)) glst)))
                                 (if (null? go)
                                     (universal vlst g)
                                     (disj (cons (normalize-goal (universal vlst (disj stay)) double-negate?) go)))))
                  ((conj glst) (normalize-goal (conj (map (lambda (x) (universal vlst x)) glst)) double-negate?))
                  ((universal ulst h) (normalize-goal (universal (append vlst ulst) h) double-negate?))
                  (_ (universal vlst g)))))))))

#|
  Input: thunk is a thunk containing user-defined relation or its negation
         des is description of the user-defined relation
         not-relation? is true if and only if this is a negation of a user-defined relation
  The normalization rules here are specific to user-relation defined in this project.
  e.g. appendo, matcho, accesso
  Output: A relation (or not-relation) with normalization rules applied bottom up.
|#
(define (normalize-relate thunk des [not-relation? #f])
  (let* ((h (match des
              ((list func 'appendo (cons x xs) ys (cons z zs)) 
                (conj (list (== x z) (func xs ys zs))))
              ((list func 'appendo xs ys xsys)
                (cond
                  ((null? xs)         (== ys xsys))
                  ((null? ys)         (conj (list (listo xs) (== xs xsys))))
                  ((null? xsys)       (conj (list (== xs '()) (== ys '()))))
                  ((equal? xs xsys)   (conj (list (listo xs) (== ys '()))))
                  ((equal? ys xsys)   (== xs '()))
                  (else #f)))
              ((list func 'matcho scrutinee clauses result)
                (cond
                  ((null? clauses)    (== result 'FAILURE))
                  (else #f)))
              ((list func 'accesso (cons input unused) (cons 'fst accessors) value)
                (func input accessors value))
              ((list func 'accesso (cons unused input) (cons 'snd accessors) value)
                (func input accessors value))
              ((list func 'accesso input accessors value)
                (cond
                  ((null? accessors)  (== value input))
                  (else #f)))
              (_  #f))))
    (cond
      ((and h not-relation?) (normalize-goal (negate-goal h)))
      (h                     (normalize-goal h))
      (not-relation?         (not-relate thunk des))
      (else                  (relate thunk des)))))

#|
  Input: g is a goal
  Assume g is a result of the extrac-answer function. i.e., there are no disjunctions
  in this goal.
  Output: Returns a goal equivalent to g with normalization rules applied.
|#
(define (normalize-extracted g)
  (remove-if-used-once g))

#|
  Input: g is a goal
         v is a variable
         acc is the accumulator (start at 0)
  Output: Number of times v is used in g in an equality or disequality goal.
|#
(define (num-used-v-in-g g v acc)
  (cond
    ((true? g)         acc)
    ((false? g)        acc)
    ((==? g)           (if (goal-use-var? g v)
                           (+ 1 acc)
                           acc))
    ((=/=? g)          (if (goal-use-var? g v)
                           (+ 1 acc)
                           acc))
    ((conj? g)         (foldl + acc (map (lambda (x) (num-used-v-in-g x v 0)) (conj-glst g))))
    ((typeo? g)        (if (goal-use-var? g v) (+ 1 acc) acc))
    ((not-typeo? g)    (if (goal-use-var? g v) (+ 1 acc) acc))
    ((existential? g)  (num-used-v-in-g (existential-g g) v acc))
    ((universal? g)    (num-used-v-in-g (universal-g g) v acc))
    (else              (error "should not reach here!"))))

#|
  Helper function for remove-if-used-once
|#
(define (eliminate-var g v)
  (cond
    ((true? g)         g)
    ((false? g)        g)
    ((==? g)           (if (or (equal? v (==-t1 g)) (equal? v (==-t2 g)))
                           (true)
                           g))
    ((=/=? g)          (if (or (equal? v (=/=-t1 g)) (equal? v (=/=-t2 g)))
                           (true)
                           g))
    ((conj? g)         (conj (map (lambda (x) (eliminate-var x v)) (conj-glst g))))
    ((typeo? g)        (if (goal-use-var? g v) (true) g))
    ((not-typeo? g)    (if (goal-use-var? g v) (true) g))
    ((existential? g)  (existential (existential-vlst g) (eliminate-var (existential-g g) v)))
    ((universal? g)    (universal (universal-vlst g) (eliminate-var (universal-g g) v)))
    (else              (error "should not reach here!"))))

#|
  Helper function for remove-if-used-once
|#
(define (eliminate-vlst g)
  (foldl (lambda (v h) (if (equal? 1 (num-used-v-in-g h v 0))
                           (eliminate-var h v)
                           h))
         g
         (existential-vlst g)))

#|
  Input: g is a goal
  If an existential variable is only used once in an equality/disequality, it is
  inconsequential to the goal and can be removed.
  Output: Returns a goal equivalent to g with inconsequential existential variables removed.
|#
(define (remove-if-used-once g)
  (match g
    ((conj glst)             (conj (map remove-if-used-once glst)))
    ((existential vlst h)    (eliminate-vlst (existential vlst (remove-if-used-once h))))
    ((universal vlst h)      (universal vlst (remove-if-used-once h)))
    (_                       g)))

#|
  Combines disjunction of disequalities into disequality of lists.
  This is used right before the goal is passed to MiniKanren.
|#
(define (combine-diseqs g)
  (match g
    ((disj (cons (=/= t1 t2) (cons (=/= s1 s2) '())))  (=/= (cons t1 s1) (cons t2 s2)))
    ((disj (cons (=/= t1 t2) (cons (=/= s1 s2) glst))) (combine-diseqs (disj (cons (=/= (cons t1 s1) (cons t2 s2)) glst))))
    ((disj glst)                                       (disj (map combine-diseqs glst)))
    ((conj glst)                                       (conj (map combine-diseqs glst)))
    ((imply g1 g2)                                     (imply (combine-diseqs g1) (combine-diseqs g2)))
    ((existential v h)                                 (existential v (combine-diseqs h)))
    ((universal v h)                                   (universal v (combine-diseqs h)))
    (_                                                 g)))

(define (disj/conj? g)
  (or (disj? g) (conj? g)))

(define (disj/conj-glst g)
  (if (disj? g)
      (disj-glst g)
      (conj-glst g)))

(define (typeo? g)
  (or (symbolo? g) (stringo? g) (numbero? g) (pairo? g) (listo? g)))

(define (not-typeo? g)
  (or (not-symbolo? g) (not-stringo? g) (not-numbero? g) (not-pairo? g) (not-listo? g)))

(define (typeo-t g)
  (match g
    ((symbolo t)      t)
    ((stringo t)      t)
    ((numbero t)      t)
    ((pairo t)        t)
    ((listo t)        t)
    ((not-symbolo t)  t)
    ((not-stringo t)  t)
    ((not-numbero t)  t)
    ((not-pairo t)    t)
    ((not-listo t)    t)
    (_ (error "typeo-t: invalid goal" g))))

(define (typeo->type? g)
  (match g
    ((symbolo _)      symbol?)
    ((not-symbolo _)  symbol?)
    ((stringo _)      string?)
    ((not-stringo _)  string?)
    ((numbero _)      number?)
    ((not-numbero _)  number?)
    ((pairo _)        pair?)
    ((not-pairo _)    pair?)
    ((listo _)        list?)
    ((not-listo _)    list?)
    (_ (error "type->type?: Invalid type" g))))

(define (type->goal t type? [not-type? #f])
  (cond
    ((and not-type? (eq? type? symbol?)) (not-symbolo t))
    ((eq? type? symbol?)                 (symbolo t))
    ((and not-type? (eq? type? string?)) (not-stringo t))
    ((eq? type? string?)                 (stringo t))
    ((and not-type? (eq? type? number?)) (not-numbero t))
    ((eq? type? number?)                 (numbero t))
    ((and not-type? (eq? type? pair?))   (not-pairo t))
    ((eq? type? pair?)                   (pairo t))
    ((and not-type? (eq? type? list?))   (not-listo t))
    ((eq? type? list?)                   (listo t))
    (else (error "type->goal: Invalid type"))))

(define (relate-name g)
  (match g
    ((relate _ des)     (car (cdr des)))
    ((not-relate _ des) (car (cdr des)))
    (_                  (error "relate-name: g is not a relate or not-relate" g))))

(define (relate-params g)
  (match g
    ((relate _ des)     (cdr (cdr des)))
    ((not-relate _ des) (cdr (cdr des)))
    (_                  (error "relate-params: g is not a relate or not-relate" g))))

#|
  Input: g is a goal
         v is a variable
  Output: Returns true if and only g uses v.
|#
(define (goal-use-var? g v)
  (if verbose? (display-and-continue "goal-use-var? " (cons v g) (const #f) #t) #f)
  (match g
    ((true)             #f)
    ((false)            #f)
    ((disj glst)        (ormap (lambda (x) (goal-use-var? x v)) glst))
    ((conj glst)        (ormap (lambda (x) (goal-use-var? x v)) glst))
    ((== t1 t2)         (or (term-use-var? t1 v) (term-use-var? t2 v)))
    ((=/= t1 t2)        (or (term-use-var? t1 v) (term-use-var? t2 v)))
    ((imply g1 g2)      (or (goal-use-var? g1 v) (goal-use-var? g2 v)))
    ((existential _ h)  (goal-use-var? h v))
    ((universal _ h)    (goal-use-var? h v))
    ((relate _ des)     (term-use-var? (relate-params g) v))
    ((not-relate _ des) (term-use-var? (relate-params g) v))
    (_                  (if (or (typeo? g) (not-typeo? g))
                            (term-use-var? (typeo-t g) v)
                            (error "goal-use-var?: Can't check goal" g)))))

;; replace v with term everywhere in g
#|
  Input: g is a goal
         v is a variable
         term is a term or another variable
  Output: Return a goal equivalent to g, where v is replaced by term everywhere.
|#
(define (substitute-term g v term)
  (begin
    (if verbose? (display-and-continue "\nsubstitution (v term g): " (list v term g) (const #f) #t) #f)
    (match g
      ((true)             (true))
      ((false)            (false))
      ((disj glst)        (disj (map (lambda (x) (substitute-term x v term)) glst)))
      ((conj glst)        (conj (map (lambda (x) (substitute-term x v term)) glst)))
      ((== t1 t2)         (== (term-replace t1 v term) (term-replace t2 v term)))
      ((=/= t1 t2)        (=/= (term-replace t1 v term) (term-replace t2 v term)))
      ((imply g1 g2)      (imply (substitute-term g1 v term) (substitute-term g2 v term)))
      ((existential q h)  (existential q (substitute-term h v term)))
      ((universal q h)    (universal q (substitute-term h v term)))
      ((relate _ des)     (apply (car des) (map (lambda (arg) (if (equal? arg v) term arg)) (relate-params g))))
      ((not-relate _ des) (negate-goal (apply (car des) (map (lambda (arg) (if (equal? arg v) term arg)) (relate-params g)))))
      (_                  (if (or (typeo? g) (not-typeo? g))
                              (type->goal (if (equal? (typeo-t g) v) term (typeo-t g)) (typeo->type? g) (not-typeo? g))
                              (error "substitute-term: Coudldn't parse goal" g)))))) 

#|
  Input: t is a term
         v is variable
         term is a term or variable
  Output: Return a term equivalent to t, where v is replaced by term everywhere.
|#
(define (term-replace t v term)
  (if (pair? t)
      (cons (term-replace (car t) v term) (term-replace (cdr t) v term))
      (if (equal? t v) term t)))

#|
  Input: g is a goal
         v is a variable
         type? is the type function we should be considering
         not-type? is true if and only if the constrain on v is a not-type constraint
  Output: Enforce type? (or not-type?) on v by applying the appropriate rewrite rules.
|#
(define (apply-type g v type? [not-type? #f])
  (match g
    ((true)             (true))
    ((false)            (false))
    ((disj glst)        (disj (map (lambda (x) (apply-type x v type? not-type?)) glst)))
    ((conj glst)        (conj (map (lambda (x) (apply-type x v type? not-type?)) glst)))
    ((== t1 t2)         (if (and (equal? v t2) (not (var? t1)) (eq? not-type? (type? t1)))
                            (false)
                            g))
    ((=/= t1 t2)        (if (and (equal? v t2) (not (var? t1)) (eq? not-type? (type? t1)))
                            (true)
                            g))
    ((imply g1 g2)      (imply (apply-type g1 v type? not-type?) (apply-type g2 v type? not-type?)))
    ((existential q h)  (existential q (apply-type h v type? not-type?)))
    ((universal q h)    (universal q (apply-type h v type? not-type?)))
    (_                  (cond
                          ((and (typeo? g) (equal? v (typeo-t g)) (equal? (typeo->type? g) type?)) (if not-type? (false) (true)))
                          ((and (typeo? g) (equal? v (typeo-t g))) (if not-type? g (false)))
                          ((typeo? g) g)
                          ((and (not-typeo? g) (equal? v (typeo-t g)) (equal? (typeo->type? g) type?)) (if not-type? (true) (false)))
                          ((and (not-typeo? g) (equal? v (typeo-t g))) (if not-type? g (true)))
                          ((not-typeo? g) g)
                          (else g)))))

(define (float-existential-helper vacc gacc glst [d? #t])
  (match glst
    ('() (cond
           ((and d? (null? vacc)) (disj (reverse gacc)))
           ((null? vacc)          (conj (reverse gacc)))
           (d?                    (existential (reverse vacc) (disj (reverse gacc))))
           (else                  (existential (reverse vacc) (conj (reverse gacc))))))
    ((cons (existential vars g) glst) (float-existential-helper (append vars vacc) (cons g gacc) glst d?))
    ((cons g glst) (float-existential-helper vacc (cons g gacc) glst d?))))

#|
  Input: g is a goal
  Output: A goal equivalent to g where existential quantifiers are floated to the top.
|#
(define (float-existential g)
  (match g
    ((disj glst) (let ((glst (map float-existential glst)))
                   (float-existential-helper '() '() glst #t)))
    ((conj glst) (let ((glst (map float-existential glst)))
                   (float-existential-helper '() '() glst #f)))
    ((imply g1 g2) (let* ((g1 (float-existential g1))
                          (g2 (float-existential g2))
                          (g (imply g1 g2)))
                    (match g
                      ((imply (existential v h1) (existential v h2))  (existential v (imply h1 h2)))
                      ((imply h1 (existential v h2))                  (existential v (imply h1 h2)))
                      (_                                              g))))
    ((existential vlst h) (let* ((h (float-existential h)))
                            (if (existential? h)
                                (existential (append vlst (existential-vlst h)) (existential-g h))
                                (existential vlst h))))
    ((universal vlst h)   (universal vlst (float-existential h)))
    (_                     g)))

(define (get-inner-goal g)
  (if (existential? g) 
      (existential-g g) 
      g))

(define (set-inner-goal g new-inner)
  (if (existential? g)
      (existential (existential-vlst g) new-inner)
      new-inner))

#|
  Returns true if and only if two user-defined relations are equal.
|#
(define (relate=? g1 g2)
  (and (equal? (relate-name g1) (relate-name g2)) (equal? (relate-params g1) (relate-params g2))))

#|
  Returns true if and only if two goals g1 and g2 are equal (up to variable name).
|#
(define (goal=? g1 g2)    ;;* Assumes that g1 and g2 are normalized
  (match g1
    ((existential v h)  (and (existential? g2) (goal=? h (substitute-term (existential-g g2) (existential-vlst g2) v))))
    ((universal v h)    (and (universal? g2) (goal=? h (substitute-term (universal-g g2) (universal-vlst g2) v))))
    ((relate _ des)     (and (relate? g2) (relate=? g1 g2)))
    ((not-relate _ des) (and (not-relate? g2) (relate=? g1 g2)))
    (_                  (equal? g1 g2))))

; (define (negation? g1 g2) ;;* Assumes that g1 and g2 are normalized
;   (match g1
;     ((disj _ _)         (and (conj? g2) (goal=? g1 (negate-goal g2)))) ;;! Due to the sorted orders of disj and conj being different in normalized form, this will not always work
;     ((conj _ _)         (and (disj? g2) (goal=? g1 (negate-goal g2)))) ;;! Due to the sorted orders of disj and conj being different in normalized form, this will not always work
;     ((imply h1 h2)      (and (imply? g2) (goal=? h1 (imply-g1 g2)) (negation? h2 (imply-g2 g2))))
;     ((existential v h)  (and (universal? g2) (negation? h (substitute-term (universal-g g2) (universal-vlst g2) v))))
;     ((universal v h)    (and (existential? g2) (negation? h (substitute-term (existential-g g2) (existential-vlst g2) v))))
;     ((relate _ _)       (and (not-relate? g2) (relate=? g1 g2)))
;     (_                  (and (not (or (disj/conj? g2) (imply? g2) (existential? g2) (universal? g2) (relate? g2))) (goal=? g1 (negate-goal g2))))))

#|
  Input: g is a goal
         v is a variable
  Output: Returns true if and only if there is an equality constraint on v in g.
|#
(define (contains-equality-on-v? g v)
  (match g
    ((== t1 t2)  (equal? t2 v))
    ((conj glst) (ormap (lambda (x) (contains-equality-on-v? x v)) glst))
    ((disj glst) (andmap (lambda (x) (contains-equality-on-v? x v)) glst))
    ((existential _ h) (contains-equality-on-v? h v))
    ((universal _ h)  (contains-equality-on-v? h v))
    (_ #f)))

#|
  Input: g is a goal
         v is a variable
  Output: Returns true if and only if there is a disequality constraint on v in g.
|#
(define (contains-disequality-on-v? g v)
  (match g
    ((=/= t1 t2)  (or (equal? t1 v) (equal? t2 v)))
    ((conj glst) (ormap (lambda (x) (contains-disequality-on-v? x v)) glst))
    ((disj glst) (andmap (lambda (x) (contains-disequality-on-v? x v)) glst))
    ((existential _ h) (contains-disequality-on-v? h v))
    ((universal _ h)  (contains-disequality-on-v? h v))
    (_ #f)))

#|
  Input: g is a goal
         v is a variable
         not-type? is true if and only if we are looking for not-type constraints
                   instead of type constraints
  Output: Returns true if and only if there is a type or (not type) constraint
          on v in g.
|#
(define (contains-typeo-on-v? g v [not-type? #f])
  (match g
    ((conj glst) (ormap (lambda (x) (contains-typeo-on-v? x v not-type?)) glst))
    ((disj glst) (andmap (lambda (x) (contains-typeo-on-v? x v not-type?)) glst))
    ((existential _ h) (contains-typeo-on-v? h v not-type?))
    ((universal _ h)  (contains-typeo-on-v? h v not-type?))
    (_  (if not-type?
            (and (not-typeo? g) (equal? (typeo-t g) v))
            (and (typeo? g) (equal? (typeo-t g) v))))))

(define (goal<? g1 g2)
  (eqv? (goal-compare g1 g2) -1))

(define (goal-diseq-first<? g1 g2)
  (eqv? (goal-diseq-first-compare g1 g2) -1))

(define (goal-list<? glst hlst)
  (eqv? (goal-list-compare glst hlst) -1))

#|
  Comparison function for goal lists to enforce canonical ordering.
|#
(define (goal-list-compare glst hlst)
  (cond
    ((< (length glst) (length hlst)) -1)
    ((> (length glst) (length hlst)) 1)
    ((null? glst) 0)
    (else (let ((compared-cars (goal-compare (car glst) (car hlst))))
            (if (eqv? compared-cars 0)
                (goal-list-compare (cdr glst) (cdr hlst))
                compared-cars)))))

#|
  Comparison function for goals to enforce canonical ordering.
|#
(define (goal-compare g1 g2)
  (cond
    ((true? g1)         -1)
    ((true? g2)         1)
    ((false? g1)        -1)
    ((false? g2)        1)
    ((==? g1)           (cond
                          ((and (==? g2) (equal? (==-t1 g1) (==-t1 g2))) (term-compare (==-t2 g1) (==-t2 g2)))
                          ((==? g2) (term-compare (==-t1 g1) (==-t1 g2)))
                          (else -1)))
    ((==? g2)           1)
    ((symbolo? g1)      (if (symbolo? g2) (term-compare (symbolo-t g1) (symbolo-t g2)) -1))
    ((symbolo? g2)      1)
    ((stringo? g1)      (if (stringo? g2) (term-compare (stringo-t g1) (stringo-t g2)) -1))
    ((stringo? g2)      1)
    ((numbero? g1)      (if (numbero? g2) (term-compare (numbero-t g1) (numbero-t g2)) -1))
    ((numbero? g2)      1)
    ((pairo? g1)        (if (pairo? g2) (term-compare (pairo-t g1) (pairo-t g2)) -1))
    ((pairo? g2)        1)
    ((listo? g1)        (if (listo? g2) (term-compare (listo-t g1) (listo-t g2)) -1))
    ((listo? g2)        1)
    ((not-symbolo? g1)  (if (not-symbolo? g2) (term-compare (not-symbolo-t g1) (not-symbolo-t g2)) -1))
    ((not-symbolo? g2)  1)
    ((not-stringo? g1)  (if (not-stringo? g2) (term-compare (not-stringo-t g1) (not-stringo-t g2)) -1))
    ((not-stringo? g2)  1)
    ((not-numbero? g1)  (if (not-numbero? g2) (term-compare (not-numbero-t g1) (not-numbero-t g2)) -1))
    ((not-numbero? g2)  1)
    ((not-pairo? g1)    (if (not-pairo? g2) (term-compare (not-pairo-t g1) (not-pairo-t g2)) -1))
    ((not-pairo? g2)    1)
    ((not-listo? g1)    (if (not-listo? g2) (term-compare (not-listo-t g1) (not-listo-t g2)) -1))
    ((not-listo? g2)    1)
    ((=/=? g1)          (cond
                          ((and (=/=? g2) (equal? (=/=-t1 g1) (=/=-t1 g2))) (term-compare (=/=-t2 g1) (=/=-t2 g2)))
                          ((=/=? g2) (term-compare (=/=-t1 g1) (=/=-t1 g2)))
                          (else -1)))
    ((=/=? g2)          1)
    ((imply? g1)        (if (imply? g2)
                            (let ((compared-g1 (goal-compare (imply-g1 g1) (imply-g1 g2))))
                              (if (eqv? 0 compared-g1) (goal-compare (imply-g2 g1) (imply-g2 g2)) compared-g1))
                            -1))
    ((imply? g2)        1)
    ((existential? g1)  (if (existential? g2) (if (term<? (existential-vlst g1) (existential-vlst g2)) -1 1) -1))
    ((existential? g2)  1)
    ((universal? g1)    (if (universal? g2) (if (term<? (universal-vlst g1) (universal-vlst g2)) -1 1) -1))
    ((universal? g2)    1)
    ((conj? g1)         (if (conj? g2) (goal-list-compare (conj-glst g1) (conj-glst g2)) -1))
    ((conj? g2)         1)
    ((disj? g1)         (if (disj? g2) (goal-list-compare (disj-glst g1) (disj-glst g2)) -1))
    ((disj? g2)         1)
    ((relate? g1)       (if (relate? g2) (term-compare (cdr (relate-description g1)) (cdr (relate-description g2))) -1))
    ((relate? g2)       1)
    ((not-relate? g1)   (if (not-relate? g2) (term-compare (cdr (not-relate-description g1)) (cdr (not-relate-description g2))) -1))
    ((not-relate? g2)   1)
    (else               0)))

#|
  Comparison function for goals to enforce canonical ordering.
  This function puts disequalities before equalities.
|#
(define (goal-diseq-first-compare g1 g2)
  (cond
    ((true? g1)         -1)
    ((true? g2)         1)
    ((false? g1)        -1)
    ((false? g2)        1)
    ((=/=? g1)          (cond
                          ((and (=/=? g2) (equal? (=/=-t1 g1) (=/=-t1 g2))) (term-compare (=/=-t2 g1) (=/=-t2 g2)))
                          ((=/=? g2) (term-compare (=/=-t1 g1) (=/=-t1 g2)))
                          (else -1)))
    ((=/=? g2)          1)
    ((not-listo? g1)    (if (not-listo? g2) (term-compare (not-listo-t g1) (not-listo-t g2)) -1))
    ((not-listo? g2)    1)
    ((not-pairo?   g1)  (if (not-pairo? g2) (term-compare (not-pairo-t g1) (not-pairo-t g2)) -1))
    ((not-pairo?   g2)  1)
    ((not-numbero? g1)  (if (not-numbero? g2) (term-compare (not-numbero-t g1) (not-numbero-t g2)) -1))
    ((not-numbero? g2)  1)
    ((not-stringo? g1)  (if (not-stringo? g2) (term-compare (not-stringo-t g1) (not-stringo-t g2)) -1))
    ((not-stringo? g2)  1)
    ((not-symbolo? g1)  (if (not-symbolo? g2) (term-compare (not-symbolo-t g1) (not-symbolo-t g2)) -1))
    ((not-symbolo? g2)  1)
    ((listo? g1)        (if (listo? g2) (term-compare (listo-t g1) (listo-t g2)) -1))
    ((listo? g2)        1)
    ((pairo?   g1)      (if (pairo? g2) (term-compare (pairo-t g1) (pairo-t g2)) -1))
    ((pairo?   g2)      1)
    ((numbero? g1)      (if (numbero? g2) (term-compare (numbero-t g1) (numbero-t g2)) -1))
    ((numbero? g2)      1)
    ((stringo? g1)      (if (stringo? g2) (term-compare (stringo-t g1) (stringo-t g2)) -1))
    ((stringo? g2)      1)
    ((symbolo? g1)      (if (symbolo? g2) (term-compare (symbolo-t g1) (symbolo-t g2)) -1))
    ((symbolo? g2)      1)
    ((==? g1)           (cond
                          ((and (==? g2) (equal? (==-t1 g1) (==-t1 g2))) (term-compare (==-t2 g1) (==-t2 g2)))
                          ((==? g2) (term-compare (==-t1 g1) (==-t1 g2)))
                          (else -1)))
    ((==? g2)           1)
    (else               (goal-compare g1 g2))))
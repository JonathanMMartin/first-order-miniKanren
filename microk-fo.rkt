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

;; first-order microKanren
(struct true        ()                       #:prefab)
(struct false       ()                       #:prefab)
(struct disj        (g1 g2)                  #:prefab)
(struct conj        (g1 g2)                  #:prefab)
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

(define (negate-goal g)
  (cond
    ((true? g)        (false))
    ((false? g)       (true))
    ((disj? g)        (conj (negate-goal (disj-g1 g)) (negate-goal (disj-g2 g))))
    ((conj? g)        (disj (negate-goal (conj-g1 g)) (negate-goal (conj-g2 g))))
    ((==? g)          (=/= (==-t1 g) (==-t2 g)))
    ((=/=? g)         (== (=/=-t1 g) (=/=-t2 g)))
    ((typeo? g)       (type->goal (typeo-t g) (typeo->type? g) #t))
    ((not-typeo? g)   (type->goal (typeo-t g) (typeo->type? g)))
    ((imply? g)       (conj (imply-g1 g) (negate-goal (imply-g2 g))))
    ((existential? g) (universal (existential-vlst g) (negate-goal (existential-g g))))
    ((universal? g)   (existential (universal-vlst g) (negate-goal (universal-g g))))
    ((relate? g)      (not-relate (relate-thunk g) (relate-description g)))
    ((not-relate? g)  (relate (not-relate-thunk g) (not-relate-description g)))
    (else             (error "Unnegateable goal" g))))

(define (mature? s) (or (not s) (pair? s)))
(define (mature s)
  (if (mature? s) s (mature (step s))))

(define (start st g)
  (let* ((g (simplify g #f #t))
         (g (combine-diseqs g)))
    (match g
      ((true) (state->stream st))
      ((false) (state->stream #f))
      ((disj g1 g2)
        (step (mplus (pause st g1)
                     (pause st g2))))
      ((conj g1 g2)
        (step (bind (pause st g1) g2)))
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
                     (pause st (conj g1 g2)))))
      ((existential v g) (step (pause st g)))
      ((universal v g) (error "not enough rules: forall" (universal v g)))
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

(define (decidable? g)
  (cond
    ((true? g)         #t)
    ((false? g)        #t)
    ((==? g)           #t)
    ((=/=? g)          #t)
    ((disj/conj? g)    (and (decidable? (disj/conj-g1 g)) (decidable? (disj/conj-g2 g))))
    ((typeo? g)        #t)
    ((not-typeo? g)    #t)
    ((imply? g)        (and (decidable? (imply-g1 g)) (decidable? (imply-g2 g))))
    ((existential? g)  (decidable? (existential-g g)))
    ((universal? g)    (decidable? (universal-g g)))
    ((relate? g)       #f)
    ((not-relate? g)   #f)
    (else              #f)))

(define (has-answer? g)
  (cond
    ((true? g)         #t)
    ((false? g)        #t)
    ((==? g)           #t)
    ((=/=? g)          #t)
    ((disj? g)         (or (has-answer? (disj-g1 g)) (has-answer? (disj-g2 g))))
    ((conj? g)         (and (has-answer? (conj-g1 g)) (has-answer? (conj-g2 g))))
    ((typeo? g)        #t)
    ((not-typeo? g)    #t)
    ((imply? g)        (and (has-answer? (imply-g1 g)) (has-answer? (imply-g2 g))))
    ((existential? g)  (has-answer? (existential-g g)))
    ((universal? g)    (has-answer? (universal-g g)))
    ((relate? g)       #f)
    ((not-relate? g)   #f)
    (else              #f)))

(define (extract-answer g)
  (cond
    ((true? g)         (cons (true) (false)))
    ((false? g)        (cons (false) (false)))
    ((==? g)           (cons g (false)))
    ((=/=? g)          (cons g (false)))
    
    ((disj? g)         (let* ((g1 (disj-g1 g))
                              (g2 (disj-g2 g))
                              (a (if (has-answer? g1) (extract-answer g1) (extract-answer g2)))
                              (B (if (has-answer? g1) g2 g1)))
                          (cons (car a) (disj (cdr a) B))))
                          
                          ; (cond
                          ;   ((and g1-ha g2-ha) (disj (extract-answer g1) (extract-answer g2)))
                          ;   (g1-ha (extract-answer g1))
                          ;   (g2-ha  (extract-answer g2))
                          ;   (else   (error "I don't... WHAT")))))
    
    ((conj? g)         (let* ((g1 (conj-g1 g))
                              (g2 (conj-g2 g))
                              (g1 (extract-answer g1))
                              (g2 (extract-answer g2))
                              (A (car g1))
                              (B (cdr g1))
                              (C (car g2))
                              (D (cdr g2)))
                          (cond
                            ((and (false? B) (false? D)) (cons (conj A C) (false)))
                            ((false? B)                  (cons (conj A C) (conj A D)))
                            ((false? D)                  (cons (conj A C) (conj B C)))
                            (else                        (cons (conj A C) (disj (conj A D) (disj (conj B C) (conj B D))))))))    
    
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
    (else              (error "how did you get here?"))))


(define (unfold g)
  (match g
    ((disj g1 g2)         (disj (unfold g1) (unfold g2))) ;; overly eager? Unfold one side?
    ((conj g1 g2)         (conj (unfold g1) (unfold g2)))
    ((imply g1 g2)        (imply (unfold g1) (unfold g2)))
    ((existential v h)    (existential v (unfold h)))
    ((universal v h)      (universal v (unfold h)))
    ((relate thunk _)     (thunk))
    ((not-relate thunk _) (negate-goal (thunk)))
    (_                    g)))

(define (display-and-continue msg v [continue identity] [display-v? #f])
  (cond
    (display-v?   (display msg) (displayln v) (continue v))
    (else         (displayln msg) (continue v))))

(define pretty-g
  (let ((count 0))
    (lambda (g)
      (set! count (+ count 1))
      (call-with-output-file (string-append "logs/goal-log" (number->string count) ".txt")
        (lambda (out) (pretty-write g out))))))

(define bad-goal1
"'#s(universal
    (#s(var v 2))
    #s(existential
       (#s(var v.lhs 7) #s(var e.then 10))
       #s(conj
          #s(==
             (if (equal? '#s(var v.lhs 7) (access)) #s(var e.then 10) '1)
             #s(var program 1))
          #s(=/= #s(var v 2) #s(var v.lhs 7)))))"
)

(define bad-goal2
"'#s(universal
    (#s(var v 2))
    #s(existential
       (#s(var v.lhs 7) #s(var e.then 10) #s(var v.lhs 17) #s(var e.then 20))
       #s(conj
          #s(==
             (if (equal? '#s(var v.lhs 7) (access))
               #s(var e.then 10)
               (if (equal? '#s(var v.lhs 17) (access)) #s(var e.then 20) '1))
             #s(var program 1))
          #s(conj
             #s(=/= #s(var v 2) #s(var v.lhs 7))
             #s(=/= #s(var v 2) #s(var v.lhs 17))))))"
)

(define (simplify g [verbose? #f] [logs? #f])
  ; (displayln (pretty-format g))
  (if (or (equal? (pretty-format g) bad-goal1)
          (equal? (pretty-format g) bad-goal2))
      (false)
      ; other wise proceed normally:
  (begin
    (if logs? (pretty-g g) #f)
    (if verbose? (display-and-continue "\nsimp g: " g (const #f) #t) #f)
    (let* ((g (float-existential g))
           (g (normalize-goal g verbose?))
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
                                    (disj g1 g2)))
          (verbose?     (display-and-continue "nothing is good, we unfold" g (lambda (x) (simplify (unfold x) verbose? logs?)))) 
          (else         (simplify (unfold g) verbose? logs?))))))))
        
        ; (cond
        ;   ((decidable? g) (if verbose? (display-and-continue "g is dec" g) g))
        ;   ((and (disj? inner-g) (or (decidable? (disj-g1 inner-g)) (decidable? (disj-g2 inner-g))))
        ;     (let* ((g1 (disj-g1 inner-g))
        ;            (g2 (disj-g2 inner-g))
        ;            (decidable-g1? (decidable? g1))
        ;            (h1 (if decidable-g1? g1 g2))
        ;            (h2 (if decidable-g1? g2 g1))
        ;            (h2 (conj (negate-goal h1) h2))
        ;            (h (disj (set-inner-goal g h1) (set-inner-goal g h2))))
        ;       (if verbose? (display-and-continue "one of them is dec" h) h)))
        ;   ((and (imply? inner-g) (decidable? (imply-g1 inner-g)))
        ;     (let* ((g1 (imply-g1 inner-g))
        ;            (g2 (imply-g2 inner-g))
        ;            (h (disj (set-inner-goal g g1) (set-inner-goal g (conj (negate-goal g1) g2)))))
        ;     (if verbose? (display-and-continue "imply-g1 is dec" h) h)))
        ;   (else
        ;     (if verbose? 
        ;         (display-and-continue "nothing is good, we unfold" g (lambda (x) (simplify (unfold x) verbose? logs?))) 
        ;         (simplify (unfold g) verbose? logs?))))))))

(define (normalize-goal g [verbose? #f] [double-negate? #t])
  (begin
    (if verbose? (display-and-continue "\nNormalizing: " g (const #f) #t) #f)
    (match g
      ;; Remove lists/pairs
      ((== (cons f1 r1) (cons f2 r2)) (normalize-goal (conj (== f1 f2) (== r1 r2)) verbose?))
      ((== (cons f1 r1) t2) (if (var? t2) (== (cons f1 r1) t2) (false)))
      ((== t1 (cons f2 r2)) (if (var? t1) (== (cons f2 r2) t1) (false)))

      ((=/= (cons f1 r1) (cons f2 r2)) (normalize-goal (disj (=/= f1 f2) (=/= r1 r2)) verbose?))
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

      ((listo (cons _ y)) (normalize-goal (listo y) verbose?))

      ;; Recursive normalization
      ((disj g1 g2)           (normalize-disj g1 g2 verbose?))
      ((conj g1 g2)           (normalize-conj g1 g2 verbose?))
      ((imply g1 g2)          (normalize-imply g1 g2 verbose?))
      ((existential vlst h)   (normalize-existential vlst h verbose? double-negate?))
      ((universal vlst h)     (normalize-universal vlst h verbose? double-negate?))
      
      ;; Rewrites for user defined relations
      ((relate     thunk des) (normalize-relate thunk des #f verbose?))
      ((not-relate thunk des) (normalize-relate thunk des #t verbose?))
      
      (g                      (cond
                                ((typeo? g)     (let ((t (typeo-t g))) (if (var? t) g (if ((typeo->type? g) t) (true) (false))))) ;; Generalized type constraint
                                ((not-typeo? g) (let ((t (typeo-t g))) (if (var? t) g (if ((typeo->type? g) t) (false) (true))))) ;; Generalized not-type constraint
                                (else           g)))))) ;; No simplification possible

(define (normalize-disj g1 g2 [verbose? #f])
  (let ((g1 (normalize-goal g1 verbose?)))
    (cond
      ((true? g1)  (true))                      ;; True or A = True
      ((false? g1) (normalize-goal g2 verbose?))    ;; False or A = A
      (else (let* ((g2 (normalize-goal g2 verbose?))
                   (g (align-goal (disj g1 g2)))
                   (g1 (disj-g1 g))
                   (g2 (disj-g2 g)))
              (begin
                (if verbose? (display-and-continue "\nnormalizing aligned " g identity #t) #f)
                (cond
                  ((true? g2)   (true))           ;; A or True = True
                  ((false? g2)  g1)               ;; A or False = A
                  ((or (=/=? g1) (typeo? g1) (not-typeo? g1))
                    (let ((g2 (if (=/=? g1)
                                  (substitute-term g2 (=/=-t2 g1) (=/=-t1 g1) verbose?)
                                  (apply-type g2 (typeo-t g1) (typeo->type? g1) (typeo? g1) verbose?))))
                      (match g2
                        ((true)   (true))
                        ((false)  g1)
                        (_        (disj g1 g2)))))
                  ;((and (disj? g2) (goal=? g1 (disj-g1 g2))) (disj g1 (disj-g2 g2))) ;; A or (A or B) = A or B
                  ((negation? g1 g2) (true))      ;; A or ~A = True
                  (else (disj g1 g2)))))))))

(define (normalize-conj g1 g2 [verbose? #f])
  (let ((g1 (normalize-goal g1 verbose?)))
    (cond
      ((true? g1)   (normalize-goal g2 verbose?))   ;; True and A = A
      ((false? g1)  (false))                    ;; False and A = False
      (else (let* ((g2 (normalize-goal g2 verbose?))
                   (g (align-goal (conj g1 g2)))
                   (g1 (conj-g1 g))
                   (g2 (conj-g2 g)))
              (cond
                ((true? g2)   g1)               ;; A and True = A
                ((false? g2)  (false))          ;; A and False = False
                ((or (==? g1) (typeo? g1) (not-typeo? g1))
                  (let* ((g1 (if (==? g1) (== (prep-subsititution (==-t1 g1) g2) (==-t2 g1)) g1))
                         (g2 (if (==? g1)
                                 (substitute-term g2 (==-t2 g1) (==-t1 g1) verbose?) ;(substitute-term g2 (==-t2 g1) (==-t1 g1) verbose?)
                                 (apply-type g2 (typeo-t g1) (typeo->type? g1) (not-typeo? g1) verbose?))))
                    (match g2
                      ((true)   g1)
                      ((false)  (false))
                      (_        (conj g1 g2)))))
                ;((and (conj? g2) (goal=? g1 (conj-g1 g2))) (conj g1 (conj-g2 g2))) ;; A and (A and B) = A and B
                ((negation? g1 g2) (false))     ;; A and ~A = False
                (else (conj g1 g2))))))))

(define (align-goal g)
  (match g
    ((disj (disj g1 g2) g3) (align-goal (disj g1 (disj g2 g3))))
    ((disj g1 (disj g2 g3)) (if (goal-diseq-first<? g2 g1) (disj g2 (align-goal (disj g1 g3))) (disj g1 (disj g2 g3))))
    ((disj g1 g2)           (if (goal-diseq-first<? g2 g1) (disj g2 g1) (disj g1 g2)))
    ((conj (conj g1 g2) g3) (align-goal (conj g1 (conj g2 g3))))
    ((conj g1 (conj g2 g3)) (if (goal<? g2 g1) (conj g2 (align-goal (conj g1 g3))) (conj g1 (conj g2 g3))))
    ((conj g1 g2)           (if (goal<? g2 g1) (conj g2 g1) (conj g1 g2)))
    (_                      g)))

(define (replace-implication ant con)
  (if (conj? ant)
      (replace-implication (conj-g2 ant) (replace-assumption-with-true (conj-g1 ant) con))
      (replace-assumption-with-true ant con)))

(define (normalize-imply g1 g2 [verbose? #f])
  (let ((g1 (normalize-goal g1 verbose?)))
    (match g1
      ((true) (normalize-goal g2 verbose?))  ;; True -> A  = A
      ((false) (true))                       ;; False -> A = True
      (_       (let* ((g2 (normalize-goal g2 verbose?))
                      (g2 (normalize-goal (replace-implication g1 g2) verbose?)))
                  (cond
                    ((true? g2) (true))
                    ((universal? g1) (normalize-goal (existential (universal-vlst g1) (imply (universal-g g1) g2)) verbose?)) ;; (forall v A) -> B = exists v (A -> B)
                    ((existential? g1) (normalize-goal (universal (existential-vlst g1) (imply (existential-g g1) g2)) verbose?)) ;; (forall v A) -> B = exists v (A -> B)
                    (else (let* ((g1 (normalize-goal (negate-goal g1) verbose?)))
                      (normalize-goal (disj g1 g2) verbose?)))))))))  ;; A -> B = ~A or B

(define (term-use-vlst? t vlst)
  (match vlst
    ('() #f)
    ((cons v r) (or (term-use-var? t v) (term-use-vlst? t r)))))


(define (normalize-existential vlst g [verbose? #f] [double-negate? #t])
  (let* ((g (normalize-goal g verbose?))
         (vars-used (filter (lambda (x) (goal-use-var? g x)) vlst))
         (vars-used (sort vars-used var<?))
         (vars-used (remove-duplicates vars-used var=?)))
    (cond
      ((null? vars-used) g)
      ((and (==? g) (member (==-t2 g) vars-used)) (true))
      ;((==? g)        (if (term-use-vlst? (==-t2 g) vars-used) (true) (existential vars-used g)))
      ((=/=? g)       (true));(if (term-use-vlst? (=/=-t2 g) vars-used) (true) (existential vars-used g)))
      ((and (disj? g) (==? (disj-g1 g)) (member (==-t2 (disj-g1 g)) vars-used)) (true))
      ((and (conj? g) (==? (conj-g1 g)) (member (==-t2 (conj-g1 g)) vars-used)) (normalize-goal (existential vars-used (conj-g2 g)) verbose? double-negate?))
      ((or (typeo? g) (not-typeo? g)) (true))
      ((disj? g)      (let* ((g1 (disj-g1 g))
                             (g2 (disj-g2 g))
                             (g1-use-var? (ormap (lambda (x) (goal-use-var? g1 x)) vars-used))
                             (g2-use-var? (ormap (lambda (x) (goal-use-var? g2 x)) vars-used)))
                        (cond
                          ;((and double-negate? g1-use-var? g2-use-var?) (normalize-goal (negate-goal (normalize-goal (negate-goal (universal vars-used g)))) verbose? #f))
                          ((and g1-use-var? g2-use-var?) (existential vars-used g))
                          (g1-use-var?                   (disj g2 (normalize-goal (existential vars-used g1) verbose? double-negate?)))
                          (else                          (disj g1 (normalize-goal (existential vars-used g2) verbose? double-negate?))))))
      ((contains-equality-on-universal g vlst '()) (false))
      ((existential? g) (normalize-goal (existential (append vars-used (existential-vlst g)) (existential-g g)) verbose? double-negate?))
      (else             (existential vars-used g)))))

(define (contains-equality-on-universal g vlst acc)
  (match g
    ((== t1 t2) (or (and (term-use-vlst? t1 vlst) (term-use-vlst? t2 acc)) (and (term-use-vlst? t2 vlst) (term-use-vlst? t1 acc))))
    ((disj g1 g2) (and (contains-equality-on-universal g1 vlst acc) (contains-equality-on-universal g2 vlst acc)))
    ((conj g1 g2) (or (contains-equality-on-universal g1 vlst acc) (contains-equality-on-universal g2 vlst acc)))
    ((universal ulst h) (contains-equality-on-universal h vlst (append acc ulst)))
    ((existential ulst h) (contains-equality-on-universal h vlst acc))
    (_ #f)))

(define (normalize-universal vlst g [verbose? #f] [double-negate? #t] [induction? #t])
  (cond
    ((and induction? (imply? g)) ;; List induction rewrite rules
      (let* ((g1 (normalize-goal (imply-g1 g) verbose?))
             (g2 (normalize-goal (imply-g2 g) verbose?)))
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
              (normalize-goal (conj base-case inductive-step) verbose?)))
          (else (normalize-universal vlst (imply g1 g2) verbose? double-negate? #f)))))
    (else 
      (let* ((g (normalize-goal g verbose?))
             (vars-used (filter (lambda (x) (goal-use-var? g x)) vlst))
             (vars-used (sort vars-used var<?))
             (vars-used (remove-duplicates vars-used var=?)))
        (cond
          ((null? vars-used) g)
          ((foldl (lambda (v acc) (or acc (contains-equality-on-v? g v) (contains-typeo-on-v? g v))) #f vlst) (false))
          ((==? g)    (false)) ;(if (or (member (==-t1 g) vars-used) (member (==-t2 g) vars-used)) (false) (universal vars-used g)))
          ((=/=? g)   (false)) ;;* Is this bad? why or why not? ;(if (or (member (=/=-t1 g) vars-used) (member (=/=-t2 g) vars-used)) (false) (universal vars-used g)))
          ((or (typeo? g) (not-typeo? g)) (false))
          ((disj? g)  (let* ((g1 (disj-g1 g))
                             (g2 (disj-g2 g))
                             (g1-use-var? (ormap (lambda (x) (goal-use-var? g1 x)) vars-used))
                             (g2-use-var? (ormap (lambda (x) (goal-use-var? g2 x)) vars-used)))
                        (cond
                          ((and double-negate? g1-use-var? g2-use-var?) (normalize-goal (negate-goal (normalize-goal (negate-goal (universal vars-used g)))) verbose? #f))
                          ((and g1-use-var? g2-use-var?) (universal vars-used g))
                          (g1-use-var?                   (disj g2 (normalize-goal (universal vars-used g1) verbose? double-negate?)))
                          (else                          (disj g1 (normalize-goal (universal vars-used g2) verbose? double-negate?))))))
          ((conj? g)  (let* ((g1 (conj-g1 g))
                             (g2 (conj-g2 g)))
                        (normalize-goal (conj (universal vars-used g1) (universal vars-used g2)) verbose? double-negate?)))
          ((universal? g) (normalize-goal (universal (append vars-used (universal-vlst g)) (universal-g g)) verbose? double-negate?)) 
          (else (universal vars-used g)))))))    

(define (normalize-relate thunk des [not-relation? #f] [verbose? #f])
  (let* ((body (if not-relation? negate-goal identity))
         (contin (lambda (x) (normalize-goal (body x) verbose?)))
         (h (match des
             ((list func 'appendo (cons x xs) ys (cons z zs)) 
               (conj (== x z) (func xs ys zs)))
             ((list func 'appendo xs ys xsys)
               (cond
                 ((null? xs)         (== ys xsys))
                 ((null? ys)         (conj (listo xs) (== xs xsys)))
                 ((null? xsys)       (conj (== xs '()) (== ys '())))
                 ((equal? xs xsys)   (conj (listo xs) (== ys '())))
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
    (if h (contin h) (body (relate thunk des)))))

(define (normalize-extracted g)
  (remove-if-used-once g))

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
    ((conj? g)         (+ (num-used-v-in-g (conj-g1 g) v 0) (num-used-v-in-g (conj-g2 g) v acc)))
    ((typeo? g)        (if (goal-use-var? g v) (+ 1 acc) acc))
    ((not-typeo? g)    (if (goal-use-var? g v) (+ 1 acc) acc))
    ((existential? g)  (num-used-v-in-g (existential-g g) v acc))
    ((universal? g)    (num-used-v-in-g (universal-g g) v acc))
    (else              (error "should not reach here!"))))

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
    ((conj? g)         (conj (eliminate-var (conj-g1 g) v) (eliminate-var (conj-g2 g) v)))
    ((typeo? g)        (if (goal-use-var? g v) (true) g))
    ((not-typeo? g)    (if (goal-use-var? g v) (true) g))
    ((existential? g)  (existential (existential-vlst g) (eliminate-var (existential-g g) v)))
    ((universal? g)    (universal (universal-vlst g) (eliminate-var (universal-g g) v)))
    (else              (error "should not reach here!"))))

(define (eliminate-vlst g)
  (foldl (lambda (v h) (if (equal? 1 (num-used-v-in-g h v 0))
                           (eliminate-var h v)
                           h))
         g
         (existential-vlst g)))

(define (remove-if-used-once g)
  (match g
    ((conj g1 g2)            (conj (remove-if-used-once g1) (remove-if-used-once g2)))
    ((existential vlst h)    (eliminate-vlst (existential vlst (remove-if-used-once h))))
    ((universal vlst h)      (universal vlst (remove-if-used-once h)))
    (_                       g)))

(define (combine-diseqs g)
  (match g
    ((disj (=/= t1 t2) (=/= s1 s2))           (=/= (cons t1 s1) (cons t2 s2)))
    ((disj (=/= t1 t2) (disj (=/= s1 s2) h))  (combine-diseqs (disj (=/= (cons t1 s1) (cons t2 s2)) h)))
    ((conj g1 g2)                             (conj (combine-diseqs g1) (combine-diseqs g2)))
    ((imply g1 g2)                            (imply (combine-diseqs g1) (combine-diseqs g2)))
    ((existential v h)                        (existential v (combine-diseqs h)))
    ((universal v h)                          (universal v (combine-diseqs h)))
    (_                                        g)))

(define (disj/conj? g)
  (or (disj? g) (conj? g)))

(define (disj/conj-g1 g)
  (if (disj? g)
      (disj-g1 g)
      (conj-g1 g)))

(define (disj/conj-g2 g)
  (if (disj? g)
      (disj-g2 g)
      (conj-g2 g)))

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

(define (goal-use-var? g v [verbose? #f])
  (if verbose? (display-and-continue "goal-use-var? " (cons v g) (const #f) #t) #f)
  (match g
    ((true)             #f)
    ((false)            #f)
    ((disj g1 g2)       (or (goal-use-var? g1 v) (goal-use-var? g2 v)))
    ((conj g1 g2)       (or (goal-use-var? g1 v) (goal-use-var? g2 v)))
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

(define (prep-subsititution t h)
  (if (list? t)
      (match h
        ((== s1 s2) (if (var? s2) (term-replace t s2 s1) t))
        ((conj h1 h2) (prep-subsititution (prep-subsititution t h1) h2))
        (_ t))
      t))

(define (substitute-simplify g h) ;; This fucntion looks kinda sus, don't use it anywhere - Maryam
  (match h
    ((true) g)
    ((false) g)
    ((== t1 t2) (if (var? t2) (substitute-simplify (substitute-term g t2 t1) (true)) g)) ;; var? might be redudent
    ((conj g1 g2) (substitute-simplify (substitute-simplify g g1) g2))
    (_ g)))

;; substitute v with term everywhere in g
(define (substitute-term g v term [verbose? #f]) ;; TODO : Try to reduce the number of times that we call normalize-goal.
  (begin
    (if verbose? (display-and-continue "\nsubstitution: " (list v term g) (const #f) #t) #f)
    (match g
      ((true)             (true))
      ((false)            (false))
      ((disj g1 g2)       (normalize-goal (disj (substitute-term g1 v term verbose?) (substitute-term g2 v term verbose?)) verbose?))
      ((conj g1 g2)       (normalize-goal (conj (substitute-term g1 v term verbose?) (substitute-term g2 v term verbose?)) verbose?))
      ((== t1 t2)         (normalize-goal (== (term-replace t1 v term) (term-replace t2 v term)) verbose?))
      ((=/= t1 t2)        (normalize-goal (=/= (term-replace t1 v term) (term-replace t2 v term)) verbose?))
      ((imply g1 g2)      (normalize-goal (imply (substitute-term g1 v term verbose?) (substitute-term g2 v term verbose?)) verbose?))
      ((existential q h)  (normalize-goal (existential q (substitute-term h v term verbose?)) verbose?))
      ((universal q h)    (normalize-goal (universal q (substitute-term h v term verbose?)) verbose?))
      ((relate _ des)     (normalize-goal (apply (car des) (map (lambda (arg) (if (equal? arg v) term arg)) (relate-params g))) verbose?)) ;; This seems like it would be time consuming if v is not in the relate, maybe check for it first?
      ((not-relate _ des) (normalize-goal (negate-goal (apply (car des) (map (lambda (arg) (if (equal? arg v) term arg)) (relate-params g)))) verbose?)) ;; See above note
      (_                  (if (or (typeo? g) (not-typeo? g))
                              (normalize-goal (type->goal (if (equal? (typeo-t g) v) term (typeo-t g)) (typeo->type? g) (not-typeo? g)) verbose?)
                              (error "substitute-term: Coudldn't parse goal" g)))))) 

(define (term-replace t v term)
  (if (pair? t)
      (cons (term-replace (car t) v term) (term-replace (cdr t) v term))
      (if (equal? t v) term t)))

(define (apply-type g v type? [not-type? #f] [verbose? #f]) ;; TODO : Try to reduce the number of times that we call normalize-goal.
  (match g
    ((true)             (true))
    ((false)            (false))
    ((disj g1 g2)       (normalize-goal (disj (apply-type g1 v type? not-type? verbose?) (apply-type g2 v type? not-type? verbose?)) verbose?))
    ((conj g1 g2)       (normalize-goal (conj (apply-type g1 v type? not-type? verbose?) (apply-type g2 v type? not-type? verbose?)) verbose?))
    ((== t1 t2)         (if (and (equal? v t2) (not (var? t1)) (eq? not-type? (type? t1)))
                            (false)
                            g))
    ((=/= t1 t2)        (if (and (equal? v t2) (not (var? t1)) (eq? not-type? (type? t1)))
                            (true)
                            g))
    ((imply g1 g2)      (normalize-goal (imply (apply-type g1 v type? not-type? verbose?) (apply-type g2 v type? not-type? verbose?)) verbose?))
    ((existential q h)  (normalize-goal (existential q (apply-type h v type? not-type? verbose?)) verbose?))
    ((universal q h)    (normalize-goal (universal q (apply-type h v type? not-type? verbose?)) verbose?))
    (_                  (cond
                          ((and (typeo? g) (equal? v (typeo-t g)) (equal? (typeo->type? g) type?)) (if not-type? (false) (true)))
                          ((and (typeo? g) (equal? v (typeo-t g))) (if not-type? g (false)))
                          ((typeo? g) g)
                          ((and (not-typeo? g) (equal? v (typeo-t g)) (equal? (typeo->type? g) type?)) (if not-type? (true) (false)))
                          ((and (not-typeo? g) (equal? v (typeo-t g))) (if not-type? g (true)))
                          ((not-typeo? g) g)
                          (else g)))))

(define (float-existential g)
  (match g
    ((disj g1 g2) (let* ((g1 (float-existential g1))
                         (g2 (float-existential g2))
                         (g (disj g1 g2)))
                    (match g
                      ((disj (existential v1 h1) (existential v2 h2)) (existential (append v1 v2) (disj h1 h2)))
                      ((disj (existential v h1) h2)                   (existential v (disj h1 h2)))
                      ((disj h1 (existential v h2))                   (existential v (disj h1 h2)))
                      (_                                              g))))
    ((conj g1 g2) (let* ((g1 (float-existential g1))
                         (g2 (float-existential g2))
                         (g (conj g1 g2)))
                    (match g
                      ((conj (existential v1 h1) (existential v2 h2)) (existential (append v1 v2) (conj h1 h2)))
                      ((conj (existential v h1) h2)                   (existential v (conj h1 h2)))
                      ((conj h1 (existential v h2))                   (existential v (conj h1 h2)))
                      (_                                              g))))
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

(define (relate=? g1 g2)
  (and (equal? (relate-name g1) (relate-name g2)) (equal? (relate-params g1) (relate-params g2))))

(define (goal=? g1 g2)    ;;* Assumes that g1 and g2 are normalized
  (match g1
    ((existential v h)  (and (existential? g2) (goal=? h (substitute-term (existential-g g2) (existential-vlst g2) v))))
    ((universal v h)    (and (universal? g2) (goal=? h (substitute-term (universal-g g2) (universal-vlst g2) v))))
    ((relate _ des)     (and (relate? g2) (relate=? g1 g2)))
    ((not-relate _ des) (and (not-relate? g2) (relate=? g1 g2)))
    (_                  (equal? g1 g2))))

(define (negation? g1 g2) ;;* Assumes that g1 and g2 are normalized
  (match g1
    ((disj _ _)         (and (conj? g2) (goal=? g1 (negate-goal g2)))) ;;! Due to the sorted orders of disj and conj being different in normalized form, this will not always work
    ((conj _ _)         (and (disj? g2) (goal=? g1 (negate-goal g2)))) ;;! Due to the sorted orders of disj and conj being different in normalized form, this will not always work
    ((imply h1 h2)      (and (imply? g2) (goal=? h1 (imply-g1 g2)) (negation? h2 (imply-g2 g2))))
    ((existential v h)  (and (universal? g2) (negation? h (substitute-term (universal-g g2) (universal-vlst g2) v))))
    ((universal v h)    (and (existential? g2) (negation? h (substitute-term (existential-g g2) (existential-vlst g2) v))))
    ((relate _ _)       (and (not-relate? g2) (relate=? g1 g2)))
    (_                  (and (not (or (disj/conj? g2) (imply? g2) (existential? g2) (universal? g2) (relate? g2))) (goal=? g1 (negate-goal g2))))))

(define (contains-equality-on-v? g v)
  (match g
    ((== t1 t2)   (equal? t2 v))
    ((conj g1 g2) (or (contains-equality-on-v? g1 v) (contains-equality-on-v? g2 v)))
    ((disj g1 g2) (and (contains-equality-on-v? g1 v) (contains-equality-on-v? g2 v)))    
    ((existential _ h) (contains-equality-on-v? h v))
    ((universal _ h)  (contains-equality-on-v? h v))
    (_ #f)))

(define (contains-disequality-on-v? g v)
  (match g
    ((=/= t1 t2)  (or (equal? t1 v) (equal? t2 v)))
    ((conj g1 g2) (or (contains-disequality-on-v? g1 v) (contains-disequality-on-v? g2 v)))
    ((disj g1 g2) (and (contains-disequality-on-v? g1 v) (contains-disequality-on-v? g2 v)))    
    ((existential _ h) (contains-disequality-on-v? h v))
    ((universal _ h)  (contains-disequality-on-v? h v))
    (_ #f)))

(define (contains-typeo-on-v? g v [not-type? #f])
  (match g
    ((conj g1 g2) (or (contains-typeo-on-v? g1 v not-type?) (contains-typeo-on-v? g2 v not-type?)))
    ((disj g1 g2) (and (contains-typeo-on-v? g1 v not-type?) (contains-typeo-on-v? g2 v not-type?)))
    ((existential _ h) (contains-typeo-on-v? h v not-type?))
    ((universal _ h)  (contains-typeo-on-v? h v not-type?))
    (_  (if not-type?
            (and (not-typeo? g) (equal? (typeo-t g) v))
            (and (typeo? g) (equal? (typeo-t g) v))))))

(define (replace-assumption-with-true ant con)
  (if (goal=? ant con)
      (true)
      (match con
        ((disj g1 g2)       (disj (replace-assumption-with-true ant g1) (replace-assumption-with-true ant g2)))
        ((conj g1 g2)       (conj (replace-assumption-with-true ant g1) (replace-assumption-with-true ant g2)))
        ((imply g1 g2)      (imply (replace-assumption-with-true ant g1) (replace-assumption-with-true ant g2)))
        ((existential v h)  (existential v (replace-assumption-with-true ant h)))
        ((universal v h)    (universal v (replace-assumption-with-true ant h)))
        (_                  con))))

(define (goal<? g1 g2)
  (eqv? (goal-compare g1 g2) -1))

(define (goal-diseq-first<? g1 g2)
  (eqv? (goal-diseq-first-compare g1 g2) -1))

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
    ((conj? g1)         (if (conj? g2)
                            (let ((compared-g1 (goal-compare (conj-g1 g1) (conj-g1 g2))))
                              (if (eqv? compared-g1 0) 
                                  (goal-compare (conj-g2 g1) (conj-g2 g2)) 
                                  compared-g1))
                            -1))
    ((conj? g2)         1)
    ((disj? g1)         (if (disj? g2)
                            (let ((compared-g1 (goal-compare (disj-g1 g1) (disj-g1 g2))))
                              (if (eqv? compared-g1 0) 
                                  (goal-compare (disj-g2 g1) (disj-g2 g2)) 
                                  compared-g1))
                            -1))
    ((disj? g2)         1)
    ((relate? g1)       (if (relate? g2) (term-compare (cdr (relate-description g1)) (cdr (relate-description g2))) -1))
    ((relate? g2)       1)
    ((not-relate? g1)   (if (not-relate? g2) (term-compare (cdr (not-relate-description g1)) (cdr (not-relate-description g2))) -1))
    ((not-relate? g2)   1)
    (else               0)))

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
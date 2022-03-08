(define-relation (matcho scrutinee clauses result)
  (conde ((== clauses '()) (== result 'FAILURE))
         ((fresh (pattern rhs clauses.remaining)
            (== clauses (cons (list pattern rhs) clauses.remaining))
            (conde ((==  '_ pattern) (== result rhs))
                   ((=/= '_ pattern)
                    (conde ((==  scrutinee pattern) (== result rhs))
                           ((=/= scrutinee pattern) (matcho scrutinee clauses.remaining result)))))))))

(define-relation (program-evalo input expr value)
  (conde ((== expr `(quote ,value)))
         ((fresh (v.lhs accessors.rhs v.rhs e.then e.else)
            (== expr `(if (equal? (quote ,v.lhs)
                                  (access . ,accessors.rhs))
                        ,e.then
                        ,e.else))
            (accesso input accessors.rhs v.rhs)
            (conde ((==  v.lhs v.rhs)
                    (program-evalo input e.then value))
                   ((=/= v.lhs v.rhs)
                    (program-evalo input e.else value)))))))

(define-relation (accesso input accessors value)
  (conde ((== accessors '()) (== value input))
         ((fresh (accessor accessors.remaining input.next unused)
            (== accessors (cons accessor accessors.remaining))
            (conde ((== accessor 'fst) (== input (cons input.next unused)))
                   ((== accessor 'snd) (== input (cons unused input.next))))
            (accesso input.next accessors.remaining value)))))

(define (compile-match clauses)
  (run 2 (program)
    (forall (v) (fresh (result)
                    (matcho v clauses result)
                    (program-evalo v program result)))))

(define (compile-match/value n v clauses)
  (run n (program)
    (fresh (result)
      (matcho v clauses result)
      (program-evalo v program result))))

(define-syntax-rule
  (PBE (in out) ...)
  (begin (pretty-write '(example: (PBE (in out) ...)))
         (pretty-write
           (time (run 1 (program) (program-evalo 'in program 'out) ...)))))

(define (example-compile-match clauses)
  (pretty-write `(example: (compile-match ,clauses)))
  (pretty-write (time (compile-match clauses))))

(define (example-compile-match/value . args)
  (pretty-write `(example: (compile-match/value . ,args)))
  (pretty-write (time (apply compile-match/value args))))

#;(pretty-write
  (time
    (run 2 (result)
      (matcho #t
              '((t 1)
                (#f 2)
                (_  3))
              result))))

#;(pretty-write
  (time
    (run 20 (i e v)
      (program-evalo i e v))))

#;(example-compile-match/value
  10
  ;#t
  ;#f
  'foo
  '((#t 1)
    (#f 2)
    (_  3)))

(pretty-write 'EXAMPLES:)

;; ~5 milliseconds
(example-compile-match
  '((_ 1)))

;; ~5 seconds
; (example-compile-match
;   '((#t 1)
;     (#f 2)
;     (_  3)))

; (define (compile-match-2 clauses)
;   (run 2 (program)
;     (fresh (v) (fresh (result)
;                     (matcho v clauses result)
;                     (program-evalo v program result)))))

; (define (example-compile-match-2 clauses)
;   (pretty-write `(example: (compile-match-2 ,clauses)))
;   (pretty-write (time (compile-match-2 clauses))))

; (example-compile-match-2
;   '((_ 1)))

; (define null-access '(access))
; (define symb '1)

; (define (compile-match-2)
;   (run 1 (x)
;       (program-evalo 1
;                     `(if (equal? (quote 1) (access))
;                          (quote a)
;                          (quote a))
;                     'a)))

; (define (example-compile-match-2)
;   (pretty-write `(example: (compile-match-2)))
;   (pretty-write (time (compile-match-2))))

; (example-compile-match-2)

; (define (compile-match-2)
;   (run 1 (x)
;       (accesso symb
;                 '()
;                 symb)))

; (define (example-compile-match-2)
;   (pretty-write `(example: (compile-match-2)))
;   (pretty-write (time (compile-match-2))))

; (example-compile-match-2)

;; expand every user relation in that formula --> do this instead of counting depth
;; exponential explosion :(
;; heuristic for DFS
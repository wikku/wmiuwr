#lang racket
(require redex "implicit.rkt" "explicit.rkt")

; based on
; A modal analysis of staged computation [2001]
; Rowan Davies, Frank Pfenning

(define-union-language MLIE (i. MLI) (e. MLE))

(define-extended-language MLtr MLIE
  (r ::= (box e.u = e.E))
  (ρ ::= (r ...))
  (R ::= (ρ ...)))

(default-language MLtr)
(caching-enabled? #f) ; gensym

(module+ test
  (test-match MLtr r (term (box x = z)))
  (test-no-match MLtr r (term ()))
  (test-match MLtr ρ (term ((box x = z))))
  (test-match MLtr ρ (term ()))
  (test-match MLtr ρ (term ((box x = (s z)) (box u = z))))
  (test-match MLtr R (term ()))
  (test-match MLtr i.E (term (λ u : nat u)))
  (test-match MLtr i.E (term (λ x : nat x)))
  (test-match MLtr e.E (term (λ u : nat u)))
  (test-match MLtr e.E (term (λ x : nat x))))

(define-metafunction MLtr
  Let : ρ e.E -> e.E
  [(Let () e.E) e.E]
  [(Let ((box e.u = e.E_1) r ...) e.E_2)
   (Let (r ...) (letbox e.u = e.E_1 in e.E_2))])

(module+ test
  (test-equal (term (Let () (s z))) (term (s z)))
  (test-equal (term (Let ((box u = z)) z)) (term (letbox u = z in z)))
  (test-match MLtr e.E (term (Let ((box u = z) (box u = z)) ()))))

(define (map2-append lst1 lst2)
  (match (cons lst1 lst2)
    [(cons '() ys) ys]
    [(cons xs '()) xs]
    [(cons (cons x xs) (cons y ys)) (cons (append x y) (map2-append xs ys))]))

(define-metafunction MLtr
  bar : R_1 R_2 -> R
  [(bar R_1 R_2) ,(map2-append (term R_1) (term R_2))])

(module+ test
  (test-equal
   (term (bar (((box u = z))) (() ())))
   (term (((box u = z)) ()))))


(define (nulls n)
  (build-list (sub1 n) (const (term ()))))

(define-metafunction MLtr
  vars : R -> (e.x ...)
  [(vars ()) ()]
  [(vars (() ρ ...)) (vars (ρ ...))]
  [(vars (((box e.u = e.E) r ...) ρ ...))
   ,(cons (term e.u) (term (vars ((r ...) ρ ...))))])

(module+ test
  (test-equal (term (vars (((box u = z)) ()))) (term (u)))
  (test-equal (term (vars (((box u = z)) ((box u2 = z))))) (term (u u2)))
  )

(define-relation MLtr
  unique ⊆ (any ...)
  [(unique (any_!_1 ...))])


(define-judgment-form MLtr
  #:mode (tr I O O)
  #:contract (tr i.E R e.E)

  [------"tr_var"
   (tr i.x () i.x)]

  [(tr i.E R e.E)
   -----"tr_lam"
   (tr (λ i.x : i.A i.E) R (λ i.x : i.A e.E))]

  [-----"tr_z"
   (tr z () z)]

  [-----"tr_s"
   (tr (s i.E) () (s i.E))]

  [-----"tr_unit"
   (tr () () ())]

  [(tr i.E_1 R_1 e.E_1) (tr i.E_2 R_2 e.E_2); (unique (vars (bar R_1 R_2)))
   -----"tr_pair"
   (tr (pair i.E_1 i.E_2) (bar R_1 R_2) (e.E_1 e.E_2))]

  [(tr i.E_1 R_1 e.E_1) (tr i.E_2 R_2 e.E_2); (unique (vars (bar R_1 R_2)))
   -----"tr_app"
   (tr (i.E_1 i.E_2) (bar R_1 R_2) (e.E_1 e.E_2))]

  [(tr i.E () e.E)
   -----"tr_box0"
   (tr (box i.E) () (box e.E))]

  [(tr i.E (ρ_1 ρ ...) e.E)
   -----"tr_box1"
   (tr (box i.E) (ρ ...) (Let ρ_1 (box e.E)))]

  [(tr i.E R e.E) (where e.u ,(gensym 'u))
   -----"tr_unbox0"
   (tr (unbox 0 i.E) R (letbox e.u = e.E in e.u))]

  [(tr i.E R e.E) (side-condition ,(> (term i.n) 0)) (where e.u ,(gensym 'u))
   -----"tr_unbox1"
   (tr (unbox i.n i.E)
       ,(append (nulls (term i.n)) (term (((box e.u = e.E)))) (term R))
       e.u)]

  [(tr i.E R e.E)
   -----"tr_fix"
   (tr (fix i.x : i.A i.E) R (fix i.x : i.A e.E))]

  [(tr i.E_1 R_1 e.E_1) (tr i.E_2 R_2 e.E_2) (tr i.E_3 R_3 e.E_3); (unique (vars (bar (bar R_1 R_2) R_3)))
   -----"tr_case"
   (tr (case i.E_1 of [z i.E_2] [s i.x i.E_3])
       (bar (bar R_1 R_2) R_3)
       (case e.E_1 of [z e.E_2] [s i.x e.E_3]))]

  )

(define (MLI->MLE t)
  (car (judgment-holds (tr ,t R e.E) e.E)))

(define trpoweri (MLI->MLE poweri))
(define trpoweri-2 (term (,trpoweri (s (s z)))))

(define trackeri (MLI->MLE ackeri))

(module+ test
  (test-equal (MLI->MLE (term z)) (term z))
  (test-equal (MLI->MLE (term (s z))) (term (s z)))
  (test-equal (MLI->MLE (term x)) (term x))

  (define pred (term (λ x : nat (case x of [z z] [s x x]))))
  (test-equal (MLI->MLE pred) pred)

  (test-judgment-holds (tr x () x))
  (test-judgment-holds (tr (λ u : nat x) R e.E))
  (test-judgment-holds (tr (case z of [z (s z)] [s x z]) R e.E))
  (test-judgment-holds (tr (λ x : nat x) R any))

  (test-equal (stage-and-normalize (term (,trackeri z)))
              (term (λ n : nat (s n))))

  (test-match MLE E trpoweri-2)

  (test-equal (stage-and-normalize trpoweri-2)
              (term (λ n : nat ((times n) ((times n) (s z))))))
  )

(module+ test
  (test-results))

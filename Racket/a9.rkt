#lang racket
(require "parenthec.rkt")
(require "c311/pmatch.rkt")

;; Step 11 -- Everything Works!!!

(define-registers num c a k v env expr)
(define-program-counter pc)

(define-union una
  (empty-k jumpout^)
  (if-inner conseq^ alt^ env^ k^)
  (mult-inner k^ v^)
  (mult-outer rand2^ env^ k^)
  (sub-inner k^)
  (zero-inner k^)
  (return-inner vexp^ env^)
  (let-inner body^ env^ k^)
  (app-inner v^ k^)
  (app-outer rand^ env^ k^)
  )

 
(define-union exp
  (const n)
  (var v)
  (if test conseq alt)
  (mult rand1 rand2)
  (sub1 rand)
  (zero rand)
  (capture body)
  (return vexp kexp)
  (let vexp body)
  (lambda body)
  (app rator rand))

(define-label app-k
    (union-case k una
                ((empty-k jumpout^) (dismount-trampoline jumpout^))
                ((if-inner conseq^ alt^ env^ k^) (if v
                                                     (begin
                                                       (set! expr conseq^)
                                                       (set! env env^)
                                                       (set! k k^)
                                                       (set! pc value-of)) 
                                                     (begin
                                                       (set! expr alt^)
                                                       (set! env env^)
                                                       (set! k k^)
                                                       (set! pc value-of))))
                ((mult-inner k^ v^) (begin
                                      (set! k k^)
                                      (set! v (* v^ v))
                                      (set! pc app-k)))
                ((mult-outer rand2^ env^ k^) (begin
                                               (set! expr rand2^)
                                               (set! env env^)
                                               (set! k (una_mult-inner k^ v))
                                               (set! pc value-of)))
                ((sub-inner k^) (begin
                                  (set! k k^)
                                  (set! v (- v 1))
                                  (set! pc app-k)))
                ((zero-inner k^) (begin
                                   (set! k k^)
                                   (set! v (zero? v))
                                   (set! pc app-k)))
                ((return-inner vexp^ env^) (begin
                                             (set! expr vexp^)
                                             (set! env env^)
                                             (set! k v)
                                             (set! pc value-of)))
                ((let-inner body^ env^ k^) (begin
                                             (set! expr body^)
                                             (set! env (envr_extend v env^))
                                             (set! k k^)
                                             (set! pc value-of)))
                ((app-inner v^ k^) (begin
                                     (set! c v^)
                                     (set! a v)
                                     (set! k k^)
                                     (set! pc apply-closure)))
                ((app-outer rand^ env^ k^) (begin
                                             (set! expr rand^)
                                             (set! env env^)
                                             (set! k (una_app-inner v k^))
                                             (set! pc value-of)))
                ))
    

    
(define-label value-of
    (union-case expr exp
                [(const n) (begin
                             (set! v n)
                             (set! pc app-k))]
                [(var v) (begin
                           (set! num v)
                           (set! pc apply-env))]
                [(if test conseq alt) (begin
                                        (set! expr test)
                                        (set! k (una_if-inner conseq alt env k))
                                        (set! pc value-of))]
                [(mult rand1 rand2) (begin
                                      (set! expr rand1)
                                      (set! k (una_mult-outer rand2 env k))
                                      (set! pc value-of))]
                [(sub1 rand) (begin
                               (set! expr rand)
                               (set! k (una_sub-inner k))
                               (set! pc value-of))]
                [(zero rand) (begin
                               (set! expr rand)
                               (set! k (una_zero-inner k))
                               (set! pc value-of))]
                [(capture body) (begin
                                  (set! expr body)
                                  (set! env (envr_extend k env))
                                  (set! pc value-of))]
                [(return vexp kexp) (begin
                                      (set! expr kexp)
                                      (set! k (una_return-inner vexp env))
                                      (set! pc value-of))]
                [(let vexp body) (begin
                                   (set! expr vexp)
                                   (set! k (una_let-inner body env k))
                                   (set! pc value-of))]
                [(lambda body) (begin
                                 (set! v (clos_closure body env))
                                (set! pc app-k))]
                [(app rator rand) (begin
                                    (set! expr rator)
                                    (set! k (una_app-outer rand env k))
                                    (set! pc value-of))]))

(define-union envr
  (empty)
  (extend arg env^))
 
(define-label apply-env
    (union-case env envr
                [(empty) (error 'env "unbound variable")]
                [(extend arg env^) (if (zero? num)
                                      (begin
                                        (set! v arg)
                                        (set! pc app-k))
                                      (begin
                                        (set! env env^)
                                        (set! num (sub1 num))
                                        (set! pc apply-env)))]))

(define-union clos
  (closure code env))
 
(define-label apply-closure
    (union-case c clos
      [(closure code env^)
       (begin
         (set! expr code)
         (set! env (envr_extend a env^))
         (set! pc value-of))]))



(define-label main
  (begin
    (set! env (envr_empty))
    (set! expr (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6)))
    (set! pc value-of)
    (mount-trampoline una_empty-k k pc)
    (pretty-print v)
    (set! env (envr_empty))
    (set! expr (exp_app
                (exp_lambda
                 (exp_app
                  (exp_app (exp_var 0) (exp_var 0))
                  (exp_const 5)))
                (exp_lambda
                 (exp_lambda
                  (exp_if (exp_zero (exp_var 0))
                          (exp_const 1)
                          (exp_mult (exp_var 0)
                                    (exp_app
                                     (exp_app (exp_var 1) (exp_var 1))
                                     (exp_sub1 (exp_var 0)))))))))
    (set! pc value-of)
    (mount-trampoline una_empty-k k pc)
    (pretty-print v)
    (set! env (envr_empty))
    (set! expr (exp_mult (exp_const 2)
                         (exp_capture
                          (exp_mult (exp_const 5)
                                    (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                                (exp_var 0))))))
    (set! pc value-of)
    (mount-trampoline una_empty-k k pc)
    (pretty-print v)
    (set! env (envr_empty))
    (set! expr (exp_let
                (exp_lambda
                 (exp_lambda
                  (exp_if
                   (exp_zero (exp_var 0))
                   (exp_const 1)
                   (exp_mult
                    (exp_var 0)
                    (exp_app
                     (exp_app (exp_var 1) (exp_var 1))
                     (exp_sub1 (exp_var 0)))))))
                (exp_app (exp_app (exp_var 0) (exp_var 0)) (exp_const 5))))
    (set! pc value-of)
    (mount-trampoline una_empty-k k pc)
    (pretty-print v)))
    
    
(main)

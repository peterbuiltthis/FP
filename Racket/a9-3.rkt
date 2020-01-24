#lang racket
(require "parenthec.rkt")
(require "c311/pmatch.rkt")

;; Step 3 -- DS

(define empty-k
  (lambda ()
    `(empty-k)))

 
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

(define app-k
  (lambda (k v)
    (pmatch k
            (`(empty-k) v)
            (`(if-inner ,conseq^ ,alt^ ,env^ ,k^) (if v
                                                      (value-of conseq^ env^ k^) 
                                                      (value-of alt^ env^ k^)))
            (`(multi-inner ,k^ ,v^) (app-k k^ (* v^ v)))
            (`(multi-outer ,rand2^ ,env^ ,k^) (value-of rand2^ env^ (mult-inner k^ v)))
            (`(sub-inner ,k^) (app-k k^ (- v 1)))
            (`(zero-inner ,k^) (app-k k^ (zero? v)))
            (`(return-inner ,vexp^ ,env^) (value-of vexp^ env^ v))
            (`(let-inner ,body^ ,env^ ,k^) (value-of body^ (envr_extend v env^) k^))
            (`(app-inner ,v^ ,k^) (apply-closure v^ v k^))
            (`(app-outer ,rand^ ,env^ ,k^) (value-of rand^ env^ (app-inner v k^)))
            (else (k v)))))
    
(define if-inner
  (lambda (conseq^ alt^ env^ k^)
    `(if-inner ,conseq^ ,alt^ ,env^ ,k^)))

(define mult-inner
  (lambda (k^ v^)
    `(multi-inner ,k^ ,v^)))

(define mult-outer
  (lambda (rand2^ env^ k^)
    `(multi-outer ,rand2^ ,env^ ,k^)))

(define sub-inner
  (lambda (k^)
    `(sub-inner ,k^)))

(define zero-inner
  (lambda (k^)
    `(zero-inner ,k^)))

(define return-inner
  (lambda (vexp^ env^)
    `(return-inner ,vexp^ ,env^)))

(define let-inner
  (lambda (body^ env^ k^)
    `(let-inner ,body^ ,env^ ,k^)))

(define app-inner
  (lambda (v^ k^)
    `(app-inner ,v^ ,k^)))

(define app-outer
  (lambda (rand^ env^ k^)
    `(app-outer ,rand^ ,env^ ,k^)))
    
(define value-of
  (lambda (expr env k)
    (union-case expr exp
                [(const n) (app-k k n)]
                [(var v) (apply-env env v k)]
                [(if test conseq alt) (value-of test env (if-inner conseq alt env k))]
                [(mult rand1 rand2) (value-of rand1 env (mult-outer rand2 env k))]
                [(sub1 rand) (value-of rand env (sub-inner k))]
                [(zero rand) (value-of rand env (zero-inner k))]
                [(capture body)
                 (value-of body (envr_extend k env) k)]
                [(return vexp kexp)
                 (value-of kexp env (return-inner vexp env))]
                [(let vexp body)
                 (value-of vexp env (let-inner body env k))]
                [(lambda body) (app-k k (clos_closure body env))]
                [(app rator rand) (value-of rator env (app-outer rand env k))])))

(define-union envr
  (empty)
  (extend arg env))
 
(define apply-env
  (lambda (env num k)
    (union-case env envr
      [(empty) (error 'env "unbound variable")]
      [(extend arg env)
       (if (zero? num)
	   (app-k k arg)
	   (apply-env env (sub1 num) k))])))
 
(define-union clos
  (closure code env))
 
(define apply-closure
  (lambda (c a k)
    (union-case c clos
      [(closure code env)
       (value-of code (envr_extend a env) k)])))
 
                                        ; Basic test...should be 5.
(pretty-print
 (value-of (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6))
           (envr_empty)
           (empty-k)))
 
					; Factorial of 5...should be 120.
(pretty-print
 (value-of (exp_app
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
				 (exp_sub1 (exp_var 0))))))))
	   (envr_empty)
           (empty-k)))
 
					; Test of capture and return...should evaluate to 24.
(pretty-print
 (value-of
  (exp_mult (exp_const 2)
	    (exp_capture
	     (exp_mult (exp_const 5)
		       (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                   (exp_var 0)))))
  (envr_empty)
  (empty-k))) 
 
;; (let ([fact (lambda (f)                                                      
;;               (lambda (n)                                                    
;;                 (if (zero? n)                                                
;;                     1                                                        
;;                     (* n ((f f) (sub1 n))))))])                              
;;   ((fact fact) 5))                                                           
 
(pretty-print
 (value-of (exp_let
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
	    (exp_app (exp_app (exp_var 0) (exp_var 0)) (exp_const 5)))
	   (envr_empty)
           (empty-k)))
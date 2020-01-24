#lang racket
(require "parenthec.rkt")
(require "c311/pmatch.rkt")

;; Step 5 -- A normal (abnormal) form


(define-union una
  (empty-k)
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

(define app-k
  (lambda (k v)
    (union-case k una
                ((empty-k) v)
                ((if-inner conseq^ alt^ env^ k^) (if v
                                                     (let* ([expr conseq^]
                                                            [env env^]
                                                            [k k^])
                                                       (value-of expr env k)) 
                                                     (let* ([expr alt^]
                                                            [env env^]
                                                            [k k^])
                                                       (value-of expr env k))))
                ((mult-inner k^ v^) (let* ([k k^]
                                           [v (* v^ v)])
                                      (app-k k v)))
                ((mult-outer rand2^ env^ k^) (let* ([expr rand2^]
                                                    [env env^]
                                                    [k (una_mult-inner k^ v)])
                                               (value-of expr env k)))
                ((sub-inner k^) (let* ([k k^]
                                       [v (- v 1)])
                                  (app-k k v)))
                ((zero-inner k^) (let* ([k k^]
                                        [v (zero? v)])
                                   (app-k k v)))
                ((return-inner vexp^ env^) (let* ([expr vexp^]
                                                  [env env^]
                                                  [k v])
                                             (value-of expr env v)))
                ((let-inner body^ env^ k^) (let* ([expr body^]
                                                  [env (envr_extend v env^)]
                                                  [k k^])
                                             (value-of expr env k)))
                ((app-inner v^ k^) (let* ([c v^]
                                          [a v]
                                          [k k^])
                                     (apply-closure c a k)))
                ((app-outer rand^ env^ k^) (let* ([expr rand^]
                                                  [env env^]
                                                  [k (una_app-inner v k^)])
                                             (value-of expr env k)))
                )))
    

    
(define value-of
  (lambda (expr env k)
    (union-case expr exp
                [(const n) (let* ([v n])
                             (app-k k v))]
                [(var v) (let* ([num v])
                           (apply-env env num k))]
                [(if test conseq alt) (let* ([expr test]
                                             [k (una_if-inner conseq alt env k)])
                                        (value-of expr env k))]
                [(mult rand1 rand2) (let* ([expr rand1]
                                           [k (una_mult-outer rand2 env k)])
                                           (value-of expr env k))]
                [(sub1 rand) (let* ([expr rand]
                                    [k (una_sub-inner k)])
                               (value-of expr env k))]
                [(zero rand) (let* ([expr rand]
                                    [k (una_zero-inner k)])
                               (value-of expr env k))]
                [(capture body) (let* ([expr body]
                                       [env (envr_extend k env)])
                                  (value-of expr env k))]
                [(return vexp kexp) (let* ([expr kexp]
                                           [k (una_return-inner vexp env)])
                                      (value-of kexp env (una_return-inner vexp env)))]
                [(let vexp body) (let* ([expr vexp]
                                        [k (una_let-inner body env k)])
                                   (value-of expr env k))]
                [(lambda body) (let* ([v (clos_closure body env)])
                                (app-k k v))]
                [(app rator rand) (let* ([expr rator]
                                         [k (una_app-outer rand env k)])
                                    (value-of expr env k))])))

(define-union envr
  (empty)
  (extend arg env^))
 
(define apply-env
  (lambda (env num k)
    (union-case env envr
                [(empty) (error 'env "unbound variable")]
                [(extend arg env^) (if (zero? num)
                                      (let* ([v arg])
                                        (app-k k v))
                                      (let* 
                                          ([num (sub1 num)]
                                           [env env^])
                                        (apply-env env num k)))])))

(define-union clos
  (closure code env))
 
(define apply-closure
  (lambda (c a k)
    (union-case c clos
      [(closure code env^)
       (let* ([expr code]
              [env (envr_extend a env^)])
         (value-of expr env k))])))
 
                                        ; Basic test...should be 5.
(pretty-print
 (value-of (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6))
           (envr_empty)
           (una_empty-k)))
 
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
           (una_empty-k)))
 
					; Test of capture and return...should evaluate to 24.
(pretty-print
 (value-of
  (exp_mult (exp_const 2)
	    (exp_capture
	     (exp_mult (exp_const 5)
		       (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                   (exp_var 0)))))
  (envr_empty)
  (una_empty-k))) 
 
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
           (una_empty-k)))
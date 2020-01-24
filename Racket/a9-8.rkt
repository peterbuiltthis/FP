#lang racket
(require "parenthec.rkt")
(require "c311/pmatch.rkt")

;; Step 8 -- Main

(define-registers num c a k v env expr)

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
  (lambda ()
    (union-case k una
                ((empty-k) v)
                ((if-inner conseq^ alt^ env^ k^) (if v
                                                     (begin
                                                       (set! expr conseq^)
                                                       (set! env env^)
                                                       (set! k k^)
                                                       (value-of)) 
                                                     (begin
                                                       (set! expr alt^)
                                                       (set! env env^)
                                                       (set! k k^)
                                                       (value-of))))
                ((mult-inner k^ v^) (begin
                                      (set! k k^)
                                      (set! v (* v^ v))
                                      (app-k)))
                ((mult-outer rand2^ env^ k^) (begin
                                               (set! expr rand2^)
                                               (set! env env^)
                                               (set! k (una_mult-inner k^ v))
                                               (value-of)))
                ((sub-inner k^) (begin
                                  (set! k k^)
                                  (set! v (- v 1))
                                  (app-k)))
                ((zero-inner k^) (begin
                                   (set! k k^)
                                   (set! v (zero? v))
                                   (app-k)))
                ((return-inner vexp^ env^) (begin
                                             (set! expr vexp^)
                                             (set! env env^)
                                             (set! k v)
                                             (value-of)))
                ((let-inner body^ env^ k^) (begin
                                             (set! expr body^)
                                             (set! env (envr_extend v env^))
                                             (set! k k^)
                                             (value-of)))
                ((app-inner v^ k^) (begin
                                     (set! c v^)
                                     (set! a v)
                                     (set! k k^)
                                     (apply-closure)))
                ((app-outer rand^ env^ k^) (begin
                                             (set! expr rand^)
                                             (set! env env^)
                                             (set! k (una_app-inner v k^))
                                             (value-of)))
                )))
    

    
(define value-of
  (lambda ()
    (union-case expr exp
                [(const n) (begin
                             (set! v n)
                             (app-k))]
                [(var v) (begin
                           (set! num v)
                           (apply-env))]
                [(if test conseq alt) (begin
                                        (set! expr test)
                                        (set! k (una_if-inner conseq alt env k))
                                        (value-of))]
                [(mult rand1 rand2) (begin
                                      (set! expr rand1)
                                      (set! k (una_mult-outer rand2 env k))
                                      (value-of))]
                [(sub1 rand) (begin
                               (set! expr rand)
                               (set! k (una_sub-inner k))
                               (value-of))]
                [(zero rand) (begin
                               (set! expr rand)
                               (set! k (una_zero-inner k))
                               (value-of))]
                [(capture body) (begin
                                  (set! expr body)
                                  (set! env (envr_extend k env))
                                  (value-of))]
                [(return vexp kexp) (begin
                                      (set! expr kexp)
                                      (set! k (una_return-inner vexp env))
                                      (value-of))]
                [(let vexp body) (begin
                                   (set! expr vexp)
                                   (set! k (una_let-inner body env k))
                                   (value-of))]
                [(lambda body) (begin
                                 (set! v (clos_closure body env))
                                (app-k))]
                [(app rator rand) (begin
                                    (set! expr rator)
                                    (set! k (una_app-outer rand env k))
                                    (value-of))])))

(define-union envr
  (empty)
  (extend arg env^))
 
(define apply-env
  (lambda ()
    (union-case env envr
                [(empty) (error 'env "unbound variable")]
                [(extend arg env^) (if (zero? num)
                                      (begin
                                        (set! v arg)
                                        (app-k))
                                      (begin
                                        (set! env env^)
                                        (set! num (sub1 num))
                                        (apply-env)))])))

(define-union clos
  (closure code env))
 
(define apply-closure
  (lambda ()
    (union-case c clos
      [(closure code env^)
       (begin
         (set! expr code)
         (set! env (envr_extend a env^))
         (value-of))])))



(define-label main
  (begin
    (set! k (una_empty-k))
    (set! env (envr_empty))
    (set! expr (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6)))
    (pretty-print (value-of))
    (set! k (una_empty-k))
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
    (pretty-print (value-of))
    (set! k (una_empty-k))
    (set! env (envr_empty))
    (set! expr (exp_mult (exp_const 2)
                         (exp_capture
                          (exp_mult (exp_const 5)
                                    (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                                (exp_var 0))))))
    (pretty-print (value-of))
    (set! k (una_empty-k))
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
    (pretty-print (value-of))))
    
    
(main)









#|(begin
  (set! k (una_empty-k))
  (set! env (envr_empty))
  (set! expr (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6)))
  (pretty-print (value-of)))

(begin
  (set! k (una_empty-k))
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
  (pretty-print (value-of)))
  
(begin
  (set! k (una_empty-k))
  (set! env (envr_empty))
  (set! expr (exp_mult (exp_const 2)
                       (exp_capture
                        (exp_mult (exp_const 5)
                                  (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                              (exp_var 0))))))
  (pretty-print (value-of))) 

(begin
  (set! k (una_empty-k))
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
  (pretty-print (value-of)))



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
           (una_empty-k))) |#

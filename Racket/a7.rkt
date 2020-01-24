#lang racket
(require "c311/pmatch.rkt")
(require racket/trace)

;; collaborated with jacebart

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))


;; 1.

(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (lambda (v)
                                 (k (+ (car n) (* 2 v)))))])))

;; 2.

(define rember*1-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
         (rember*1-cps (car ls) (lambda (v)
                                   (if (eqv? (car ls) v)
                                       (rember*1-cps (cdr ls) (lambda (v)
                                                                (k (cons (car ls) v))))
                                       (rember*1-cps (car ls) (lambda (v)
                                                                (k (cons v (cdr ls))))))))]
      [(eqv? (car ls) '?) (k (cdr ls))]
      [else (rember*1-cps (cdr ls) (lambda (v)
                         (k (cons (car ls) v))))])))


(trace rember*1-cps)

;; 3.

;; value-of-cps

(define value-of-cps
  (lambda (expr env k)
    (pmatch expr
      [`,n (guard (or (number? n) (boolean? n))) (k n)]
      [`(+ ,x1 ,x2) (value-of-cps x1 env (lambda (v)
                                           (value-of-cps x2 env (lambda (v*)
                                                              (k (+ v v*))))))]
      [`(* ,x1 ,x2) (value-of-cps x1 env (lambda (v)
                                           (value-of-cps x2 env (lambda (v*)
                                                              (k (* v v*))))))]
      [`(sub1 ,x) (value-of-cps x env (lambda (v)
                                        (k (sub1 v))))]
      [`(zero? ,x) (value-of-cps x env (lambda (v)
                                         (k (zero? v))))]
      [`(if ,test ,conseq ,alt)   (value-of-cps test env (lambda (v)
                                                           (if v
                                                               (value-of-cps conseq env k)
                                                               (value-of-cps alt env k))))]
      [`(capture ,k-id ,body) (value-of-cps body (lambda (y k*)
                                                         (if (eqv? y k-id) 
                                                           (k* k)
                                                           (env y k*))) k)]
      [`(return ,v-exp ,k-exp) (value-of-cps k-exp env (lambda (k)
                                                         (value-of-cps v-exp env k)))]
      [`,x (guard (symbol? x)) (env x k)]
      [`(lambda (,id) ,body) (k (lambda (a k^)
                               (value-of-cps body (lambda (y k*)
                                                         (if (eqv? y id) 
                                                           (k* a)
                                                           (env y k*))) k^)))]
      [`(,rator ,rand) (value-of-cps rator env (lambda (closure)
                          (value-of-cps rand env (lambda (arg)
                                                    (closure arg k)))))])))

(define empty-env
  (lambda ()
    (lambda (y k^^) 
       (lambda (y) (error 'value-of "unbound variable ~s" y)))))

(define fact-5
    '((lambda (f)
	((f f) 5))
      (lambda (f)
	(lambda (n)
	  (if (zero? n)
	      1
	      (* n ((f f) (sub1 n))))))))

(define capture-fun
    '(* 3 (capture q (* 2 (return 4 q)))))


;; value-of-cps-fn

(define app-k-fn
  (lambda (k v)
    (k v)))

(define app-env-fn
  (lambda (env y k)
    (env y k)))

(define extend-env-fn
  (lambda (x a env)
    (lambda (y k*)
      (if (eqv? x y)
          (app-k-fn k* a)
          (app-env-fn env y k*)))))

(define closure-fn
  (lambda (x body env)
    (lambda (a k^)
      (value-of-cps-fn body (extend-env-fn x a env) k^))))

(define apply-closure-fn
  (lambda (p a k)
    (p a k)))

(define +-inner-k-fn
  (lambda (v k)
    (lambda (w)
      (app-k-fn k (+ v w)))))

(define *-inner-k-fn
  (lambda (v k)
    (lambda (w)
      (app-k-fn k (* v w)))))

(define sub1-inner-k-fn
  (lambda (k)
    (lambda (v)
      (app-k-fn k (sub1 v)))))

(define zero-inner-k-fn
  (lambda (k)
    (lambda (v)
      (app-k-fn k (zero? v)))))

(define if-inner-k-fn
  (lambda (conseq alt env k)
    (lambda (v)
      (if v
          (value-of-cps-fn conseq env k)
          (value-of-cps-fn alt env k)))))

(define return-inner-k-fn
  (lambda (v-exp env)
    (lambda (k)
      (value-of-cps-fn v-exp env k))))

(define rand-inner-k-fn
  (lambda (closure k)
    (lambda (arg)
      (apply-closure-fn closure arg k))))

(define rator-outer-k-fn
  (lambda (rand env k)
    (lambda (closure)
      (value-of-cps-fn rand env (rand-inner-k-fn closure k)))))

(define +-outer-k-fn
  (lambda (x2 env k)
    (lambda (v)
      (value-of-cps-fn x2 env (+-inner-k-fn v k)))))

(define *-outer-k-fn
  (lambda (x2 env k)
    (lambda (v)
      (value-of-cps-fn x2 env (*-inner-k-fn v k)))))
          
      

(define value-of-cps-fn
  (lambda (expr env k)
    (pmatch expr
            [`,n (guard (or (number? n) (boolean? n))) (app-k-fn k n)]
            [`(+ ,x1 ,x2) (value-of-cps-fn x1 env (+-outer-k-fn x2 env k))]
            [`(* ,x1 ,x2) (value-of-cps-fn x1 env (*-outer-k-fn x2 env k))]
            [`(sub1 ,x) (value-of-cps-fn x env (sub1-inner-k-fn k))]
            [`(zero? ,x) (value-of-cps-fn x env (zero-inner-k-fn k))]
            [`(if ,test ,conseq ,alt)   (value-of-cps-fn test env (if-inner-k-fn conseq alt env k))]
            [`(capture ,k-id ,body) (value-of-cps-fn body (extend-env-fn k-id k env) k)]
            [`(return ,v-exp ,k-exp) (value-of-cps-fn k-exp env (return-inner-k-fn v-exp env))]
            [`,x (guard (symbol? x)) (app-env-fn env x k)]
            [`(lambda (,id) ,body) (app-k-fn k (closure-fn id body env))]
            [`(,rator ,rand) (value-of-cps-fn rator env (rator-outer-k-fn rand env k))])))

(define empty-k-fn
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k-fn "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))


;; value-of-cps-ds

(define app-env-ds
  (lambda (env y k)
    (env y k)))

(define extend-env-ds
  (lambda (x a env)
    (lambda (y k*)
      (if (eqv? x y)
          (app-k-ds k* a)
          (app-env-ds env y k*)))))

(define closure-ds
  (lambda (x body env)
    (lambda (a k^)
      (value-of-cps-ds body (extend-env-ds x a env) k^))))

(define apply-closure-ds
  (lambda (p a k)
    (p a k)))


(define +-inner-k-ds
  (lambda (v k)
      `(+-inner-k-ds ,v ,k)))

(define *-inner-k-ds
  (lambda (v k)
    `(*-inner-k-ds ,v ,k)))

(define sub1-inner-k-ds
  (lambda (k)
    `(sub1-inner-k-ds ,k)))

(define zero-inner-k-ds
  (lambda (k)
    `(zero-inner-k-ds ,k)))

(define if-inner-k-ds
  (lambda (conseq alt env k)
    `(if-inner-k-ds ,conseq ,alt ,env ,k)))

(define return-inner-k-ds
  (lambda (v-exp env)
    `(return-inner-k-ds ,v-exp ,env)))

(define rand-inner-k-ds
  (lambda (closure k)
    `(rand-inner-k-ds ,closure ,k)))

(define rator-outer-k-ds
  (lambda (rand env k)
    `(rator-outer-k-ds ,rand ,env ,k)))

(define +-outer-k-ds
  (lambda (x2 env k)
    `(+-outer-k-ds ,x2 ,env ,k)))

(define *-outer-k-ds
  (lambda (x2 env k)
    `(*-outer-k-ds ,x2 ,env ,k)))


(define app-k-ds
  (lambda (k^ v^)
    (pmatch k^
            [`(+-inner-k-ds ,v ,k) (app-k-ds k (+ v v^))]
            [`(+-outer-k-ds ,x2 ,env ,k) (value-of-cps-ds x2 env (+-inner-k-ds v^ k))]
            [`(*-inner-k-ds ,v ,k) (app-k-ds k (* v v^))]
            [`(*-outer-k-ds ,x2 ,env ,k) (value-of-cps-ds x2 env (*-inner-k-ds v^ k))]
            [`(sub1-inner-k-ds ,k) (app-k-ds k (sub1 v^))]
            [`(zero-inner-k-ds ,k) (app-k-ds k (zero? v^))]
            [`(if-inner-k-ds ,conseq ,alt ,env ,k) (if v^
                                                      (value-of-cps-ds conseq env k)
                                                      (value-of-cps-ds alt env k))]
            [`(return-inner-k-ds ,v-exp ,env) (value-of-cps-ds v-exp env v^)]
            [`(rand-inner-k-ds ,closure ,k) (apply-closure-ds closure v^ k)]
            [`(rator-outer-k-ds ,rand ,env ,k) (value-of-cps-ds rand env (rand-inner-k-ds v^ k))]
            [else (k^ v^)])))
                                      
(define value-of-cps-ds
  (lambda (expr env k)
    (pmatch expr
            [`,n (guard (or (number? n) (boolean? n))) (app-k-ds k n)]
            [`(+ ,x1 ,x2) (value-of-cps-ds x1 env (+-outer-k-ds x2 env k))]
            [`(* ,x1 ,x2) (value-of-cps-ds x1 env (*-outer-k-ds x2 env k))]
            [`(sub1 ,x) (value-of-cps-ds x env (sub1-inner-k-ds k))]
            [`(zero? ,x) (value-of-cps-ds x env (zero-inner-k-ds k))]
            [`(if ,test ,conseq ,alt)   (value-of-cps-ds test env (if-inner-k-ds conseq alt env k))]
            [`(capture ,k-id ,body) (value-of-cps-ds body (extend-env-ds k-id k env) k)]
            [`(return ,v-exp ,k-exp) (value-of-cps-ds k-exp env (return-inner-k-ds v-exp env))]
            [`,x (guard (symbol? x)) (app-env-ds env x k)]
            [`(lambda (,id) ,body) (app-k-ds k (closure-ds id body env))]
            [`(,rator ,rand) (value-of-cps-ds rator env (rator-outer-k-ds rand env k))])))

(define empty-k-ds
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k-ds "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))


    

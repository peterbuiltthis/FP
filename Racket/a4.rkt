#lang racket
(require "c311/pmatch.rkt")

(define value-of-fn 
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (number? n)) n]
            [`,g (guard (symbol? g)) (apply-env-fn env g)]
            [`(zero? ,x) (zero? (value-of-fn x env))]
            [`(sub1 ,y1) (sub1 (value-of-fn y1 env))]
            [`(* ,x1 ,x2) (* (value-of-fn x1 env) (value-of-fn x2 env))]
            [`(if ,test-exp ,then-exp ,else-exp) 
             (if (value-of-fn test-exp env) 
                 (value-of-fn then-exp env)
                 (value-of-fn else-exp env))]      
            [`(let ([,x ,v]) ,e) (value-of-fn e (lambda (a) 
                                                  (if (eqv? a x)
                                                   (value-of-fn v env)
                                                  (apply-env-fn env a))))]
             [`(lambda (,z) ,body) (lambda (a) (value-of-fn body 
                                                          (lambda (c)
                                                          (if 
                                                            (eqv? c z)
                                                            a 
                                                            (apply-env-fn env c)))))]
            [`(,rator ,rand) ((value-of-fn rator env)
                              (value-of-fn rand env))])))  

(define apply-env-fn
  (lambda (env y)
    (env y)))

(define extend-env-fn
  (lambda (x a env)
    (lambda (y) 
      (if (eqv? x y) 
          a
          (apply-env-fn env y)))))

(define empty-env-fn
  (lambda ()
    (lambda (y) 
       (lambda (y) (error 'value-of "unbound variable ~s" y)))))

;;(define apply-closure-fn)

;;(define closure-fn
 ;; (lambda ()))






(define value-of-ds 
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (number? n)) n]
            [`,g (guard (symbol? g)) (apply-env-ds env g)]
            [`(zero? ,x) (zero? (value-of-ds x env))]
            [`(sub1 ,y1) (sub1 (value-of-ds y1 env))]
            [`(* ,x1 ,x2) (* (value-of-ds x1 env) (value-of-ds x2 env))]
            [`(if ,test-exp ,then-exp ,else-exp) 
             (if (value-of-fn test-exp env) 
                 (value-of-fn then-exp env)
                 (value-of-fn else-exp env))]      
            [`(let ([,x ,v]) ,e) (value-of-ds e (lambda (a) 
                                                  (if (eqv? a x)
                                                      (value-of-ds v env)
                                                      (apply-env-ds env a))))]
             [`(lambda (,z) ,body) (lambda (a) (value-of-ds body 
                                                          (lambda (c)
                                                          (if (eqv? c z)
                                                            a 
                                                            (apply-env-ds env c)))))]
            [`(,rator ,rand) ((value-of-ds rator env)
                              (value-of-ds rand env))]))) 


(define apply-env-ds
  (lambda (env y)
    (pmatch env
            [`(empty-env-ds) (error 'value-of "unbound variable ~s" y)]
            [`(entend-env-ds ,x ,a ,env)(if (eqv? y x) a (apply-env-ds env y))]
            [`(let ([,x ,v]) ,e) (value-of-ds e (lambda (a) 
                                                  (if (eqv? a x)
                                                   (value-of-ds v env)
                                                  (apply-env-ds env a))))])))

(define extend-env-ds
  (lambda (x a env)
    `(extend-env-ds ,x ,a ,env)))

(define empty-env-ds 
  (lambda ()
    `(empty-env-ds)))

;;(define apply-closure-ds)

;; (define closure-ds)

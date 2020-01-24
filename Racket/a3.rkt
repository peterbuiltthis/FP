#lang racket
(require "c311/pmatch.rkt")
(require racket/trace)

;; worked with jacebrat

;; Assignment #3

; 1. value-of , value-of-fn , value-of-ds 
(define value-of
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (number? n)) n]
            [`,g (guard (symbol? g)) (env g)]
            [`(zero? ,x) (zero? (value-of x env))]
            [`(sub1 ,y1) (sub1 (value-of y1 env))]
            [`(* ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
            [`(if ,test-exp ,then-exp ,else-exp) 
             (if (value-of test-exp env) 
                 (value-of then-exp env)
                 (value-of else-exp env))]
            [`(let ([,x ,v]) ,e) (value-of e (lambda (a) 
                                               (if (eqv? a x)
                                                   (value-of v env)
                                                   (env a))))]
            [`(lambda (,z) ,body) (lambda (a) (value-of body 
                                                        (lambda (c)
                                                           (if (eqv? c z)
                                                            a 
                                                            (env c)))))]
            ;;code for part 2
           [`(lambda (,y1 ,y2) ,body) (lambda (a b) (value-of body
                                                              (lambda (c)
                                                                (if
                                                                 (eqv? c y1)
                                                                 a
                                                                 (if 
                                                                  (eqv? c y2)
                                                                  b
                                                                  (env c))))))]
            [`(,rator ,rand) ((value-of rator env)
                              (value-of rand env))]
            [`(,rator ,rand1 ,rand2) ((value-of rator env) (value-of rand1 env) (value-of rand2 env))])))  




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



;; part 3 fo-eulav

(define fo-eulav 
  (lambda (exp env)
    (pmatch exp
            [`,n (guard (number? n)) n]
            [`,g (guard (symbol? g)) (env g)]
            [`(,x ?orez) (zero? (fo-eulav x env))]
            [`(,y1 1bus) (sub1 (fo-eulav y1 env))]
            [`(,x2 ,x1 *) (* (fo-eulav x1 env) (fo-eulav x2 env))]
            [`(,else-exp ,then-exp ,test-exp fi) 
             (if (fo-eulav test-exp env) 
                 (fo-eulav then-exp env)
                 (fo-eulav else-exp env))]
            [`(let ([,x ,v]) ,e) (fo-eulav e (lambda (a) 
                                               (if (eqv? a x)
                                                   (fo-eulav v env)
                                                   (env a))))]
            [`( ,body (,z) adbmal) (lambda (a) (fo-eulav body 
                                                        (lambda (c)
                                                           (if 
                                                            (eqv? c z)
                                                            a 
                                                            (env c)))))]
           [`(lambda (,y1 ,y2) ,body) (lambda (a b) (fo-eulav body
                                                              (lambda (c)
                                                                (if
                                                                 (eqv? c y1)
                                                                 a
                                                                 (if 
                                                                  (eqv? c y2)
                                                                  b
                                                                  (env c))))))]                                       
            [`(,rator ,rand) ((fo-eulav rand env)
                              (fo-eulav rator env))]
            [`(,rator ,rand1 ,rand2) ((fo-eulav rator env)
                                      (fo-eulav rand1 env)
                                      (fo-eulav rand2 env))])))

(define empty-env
  (lambda ()
    (lambda (y) 
       (lambda (y) (error 'value-of "unbound variable ~s" y)))))

(trace value-of-ds)
(trace value-of)
(trace fo-eulav)
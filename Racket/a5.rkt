#lang racket
(require "c311/pmatch.rkt")
(require racket/trace)

(worked with jacebart and vjoshua)


(define val-of-cbv
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(set! ,x ,rhs) (set! x (val-of-cbv rhs env))]
      [`,x (guard (symbol? x)) (apply-env env x)]
      [`(lambda (,x) ,body) (closure-cbv x body env)]
      [`(,rator ,rand) (apply-closure-cbv (val-of-cbv rator env)
                                      (val-of-cbv rand env))])))

(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (x a env)
    (lambda (y) 
      (if (eqv? x y) 
          a
          (apply-env env y)))))

(define empty-env
  (lambda ()
    (lambda (y) 
       (lambda (y) (error 'value-of "unbound variable ~s" y)))))


(define apply-closure-cbv
  (lambda (p a)
    (p a)))

(define closure-cbv
  (lambda (x body env)
    (lambda (a)
      (val-of-cbv body (extend-env x a env)))))



(define val-of-cbr
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env) (val-of-cbr conseq env) (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`(set! ,x ,rhs) (let [(value (val-of-cbr rhs env))]
                         (set-box! (apply-env env x) value))]
      [`,x (guard (symbol? x)) (unbox (apply-env env x))]
      [`(lambda (,x) ,body) (closure-cbr x body env)]
      [`(,rator ,x) (guard (symbol? x)) ((val-of-cbr rator env) (apply-env env x))]
      [`(,rator ,rand) (guard (not (symbol? rand))) ((val-of-cbr rator env) (box (val-of-cbr rand env)))])))



(define apply-closure-cbr
  (lambda (p a)
    (p a)))

(define closure-cbr
  (lambda (x body env)
    (lambda (a)
      (val-of-cbr body (extend-env x a env)))))

(define val-of-cbname
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env) (val-of-cbname conseq env) (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(set! ,x ,rhs) (let [(value (val-of-cbname rhs env))]
                         (set-box! (apply-env env x) value))]
      [`,x (guard (symbol? x)) ((unbox (apply-env env x)))]
      [`(lambda (,x) ,body) (closure-cbname x body env)]
      [`(,rator ,x) (guard (symbol? x)) (((val-of-cbname rator env) (apply-env env x)))]
      [`(,rator ,rand) (guard (not (symbol? rand))) ((val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))))])))

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))

(define apply-closure-cbname
  (lambda (p a)
    (p a)))

(define closure-cbname
  (lambda (x body env)
    (lambda (a)
      (val-of-cbname body (extend-env x a env)))))

(define val-of-cbneed
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env) (val-of-cbneed conseq env) (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(set! ,x ,rhs) (let [(value (val-of-cbneed rhs env))]
                         (set-box! (apply-env env x) value))]
      [`,x (guard (symbol? x)) (unbox/need (apply-env env x))]
      [`(lambda (,x) ,body) (closure-cbneed x body env)]
      [`(,rator ,x) (guard (symbol? x)) ((val-of-cbneed rator env) (apply-env env x))]
      [`(,rator ,rand) (guard (not (symbol? rand))) ((val-of-cbneed rator env) (box (lambda () (val-of-cbneed rand env))))])))

(define unbox/need
  (lambda (b)
    (let ([val ((unbox b))])
      (set-box! b (lambda () val))
      val)))

(define apply-closure-cbneed
  (lambda (p a)
    (p a)))

(define closure-cbneed
  (lambda (x body env)
    (lambda (a)
      (val-of-cbneed body (extend-env x a env)))))
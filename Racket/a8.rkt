#lang racket
(require "c311/pmatch.rkt")
(require racket/trace)

;; CPS

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))
 
(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth (car ls)
	      (lambda (l)
		(depth (cdr ls)
		       (lambda (r)
			 (let ((l (add1 l)))
			   (k (if (< l r) r l)))))))]
      [else (depth (cdr ls) k)])))
 
(define fact
  (lambda (n k)
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
         [(zero? n) (k 1)]
         [else (fact fact (sub1 n) (lambda (v) (k (* n v))))]))
     k)))
 
(define pascal
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (k (lambda (m a k)
		  (cond
		    [(> m n) (k '())]
		    [else (let ((a (+ a m)))
			    (pascal pascal (lambda (f) (f (add1 m) a (lambda (v) (k (cons a v)))))))]))))))
      (pascal pascal (lambda (f) (f 1 0 k))))))


;; RI ack

(define empty-k-ri
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

(define app-k-ri
  (lambda (k v)
    (k v)))

(define ack-ri-k
  (lambda (v^ k^)
    (lambda (v)
      (ack-ri (sub1 v^) v k^))))

(define ack-ri
  (lambda (m n k)
    (cond
      [(zero? m) (app-k-ri k (add1 n))]
      [(zero? n) (ack-ri (sub1 m) 1 k)]
      [else (ack-ri m (sub1 n) (ack-ri-k m k))])))



;; RI depth

(define depth-inner-k-ri
  (lambda (v^ k^)
    (lambda (v)
      (let ((v^ (add1 v^)))
        (app-k-ri k^ (if (< v^ v) v v^))))))

(define depth-outer-k-ri
  (lambda (v^ k^)
    (lambda (v)
      (depth-ri (cdr v^) (depth-inner-k-ri v k^)))))
 
(define depth-ri
  (lambda (ls k)
    (cond
      [(null? ls) (app-k-ri k 1)]
      [(pair? (car ls))
       (depth-ri (car ls) (depth-outer-k-ri ls k))]
      [else (depth-ri (cdr ls) k)])))

;; RI fact

(define fact-ri-k
  (lambda (v^ k^)
    (lambda (v)
      (app-k-ri k^ (* v^ v)))))
  
(define fact-outer-k-ri
  (lambda (v^)
    (lambda (v k)
      (v v v^ k))))
  
(define fact-ri
  (lambda (n k)
    ((lambda (fact k)
      (fact fact n k))
     (lambda (fact v k)
      (cond
        [(zero? v) (app-k-ri k 1)]
        [else (fact fact (sub1 v) (fact-ri-k v k))]))
     k)))
  
;; RI Pascal

(define p-innerk-ri
  (lambda (v^ k^)
    (lambda (v) 
      (app-k-ri k^ (cons v^ v)))))

(define p-outerk-ri
  (lambda (m^ a^ k^)
    (lambda (v) 
      (v (add1 m^) a^ (p-innerk-ri a^ k^)))))

(define p-lastk-ri
  (lambda (k^)
    (lambda (v) 
      (v 1 0 k^))))
    
(define pascal-ri
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (app-k-ri k (lambda (m a k)
		  (cond
		    [(> m n) (app-k-ri k '())]
		    [else (let ((a (+ a m)))
			    (pascal pascal (p-outerk-ri m a k)))]))))))
      (pascal pascal (p-lastk-ri k)))))


;; DS ack

(define empty-k-ds
  (lambda ()
    `(empty-k-ds)))

(define ack-ds-k
  (lambda (v^ k^)
    `(ack-ds-k ,v^ ,k^)))

(define ack-app-k-ds
  (lambda (k v)
    (pmatch k
            (`(empty-k-ds) v)
            (`(ack-ds-k ,v^ ,k^) (ack-ds (sub1 v^) v k^))
            (else (k v)))))

(define ack-ds
  (lambda (m n k)
    (cond
      [(zero? m) (ack-app-k-ds k (add1 n))]
      [(zero? n) (ack-ds (sub1 m) 1 k)]
      [else (ack-ds m (sub1 n) (ack-ds-k m k))])))

;; DS depth

(define depth-app-k-ds
  (lambda (k v)
    (pmatch k
            (`(depth-inner-k-ds ,v^ ,k^) (let ((v^ (add1 v^)))
                                           (depth-app-k-ds k^ (if (< v^ v) v v^))))
            (`(depth-outer-k-ds ,v^ ,k^) (depth-ds (cdr v^) (depth-inner-k-ds v k^)))
            (`(empty-k-ds) v)
            (else (k v)))))

(define depth-inner-k-ds
  (lambda (v^ k^)
    `(depth-inner-k-ds ,v^ ,k^)))

(define depth-outer-k-ds
  (lambda (v^ k^)
    `(depth-outer-k-ds ,v^ ,k^)))
 
(define depth-ds
  (lambda (ls k)
    (cond
      [(null? ls) (depth-app-k-ds k 1)]
      [(pair? (car ls))
       (depth-ds (car ls) (depth-outer-k-ds ls k))]
      [else (depth-ds (cdr ls) k)])))

;; DS fact

(define fact-app-k-ds
  (lambda (k v)
    (pmatch k
            (`(empty-k-ds) v)
            (`(fact-ds-k ,v^ ,k^) (fact-app-k-ds k^ (* v^ v)))
            (`(fact-outer-k-ds ,v^) (v v v^ k))
            (else (k v)))))

(define fact-ds-k
  (lambda (v^ k^)
    `(fact-ds-k ,v^ ,k^)))
  
(define fact-outer-k-ds
  (lambda (v^)
    `(fact-outer-k-ds ,v^)))
  
(define fact-ds
  (lambda (n k)
    ((lambda (fact k)
      (fact fact n k))
     (lambda (fact v k)
      (cond
        [(zero? v) (fact-app-k-ds k 1)]
        [else (fact fact (sub1 v) (fact-ds-k v k))]))
     k)))


;; DS Pascal

(define pascal-app-k-ds
  (lambda (k v)
    (pmatch k
            (`(empty-k-ds) v)
            (`(p-innerk-ds ,v^ ,k^) (pascal-app-k-ds k^ (cons v^ v)))
            (`(p-outerk-ds ,m^ ,a^ ,k^) (v (add1 m^) a^ (p-innerk-ds a^ k^)))
            (`(p-lastk-ds ,k^) (v 1 0 k^))
            (else (k v)))))

(define p-innerk-ds
  (lambda (v^ k^)
    `(p-innerk-ds ,v^ ,k^)))

(define p-outerk-ds
  (lambda (m^ a^ k^)
    `(p-outerk-ds ,m^ ,a^ ,k^)))

(define p-lastk-ds
  (lambda (k^)
    `(p-lastk-ds ,k^)))
    
(define pascal-ds
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (pascal-app-k-ds k 
                              (lambda (m a k)
                                (cond
                                  [(> m n) (pascal-app-k-ds k '())]
                                  [else (let ((a (+ a m)))
                                          (pascal pascal (p-outerk-ds m a k)))]))))))
      (pascal pascal (p-lastk-ds k)))))

;; Trp ack

(define empty-k-trp
  (lambda (jumpout^)
    `(empty-k-trp ,jumpout^)))

(define ack-trp-k
  (lambda (v^ k^)
    `(ack-trp-k ,v^ ,k^)))

(define ack-app-k-trp
  (lambda (k v)
    (pmatch k
            (`(ack-trp-k ,v^ ,k^) (lambda ()
                                    (ack-trp (sub1 v^) v k^)))
            (`(empty-k-trp ,jumpout^) (jumpout^ v)))))

(define ack-trp
  (lambda (m n k)
    (cond
      [(zero? m) (ack-app-k-trp k (add1 n))]
      [(zero? n) (ack-trp (sub1 m) 1 k)]
      [else (lambda ()
              (ack-trp m (sub1 n) (ack-trp-k m k)))])))

(define trampoline
  (lambda (th)
    (trampoline (th))))

(define ack-tramp-driver
  (lambda (n^ n2^)
    (call/cc
     (lambda (jumpout^)
       (trampoline
        (ack-trp
         n^
         n2^
         (empty-k-trp jumpout^)))))))



;; trp depth

(define depth-app-k-trp
  (lambda (k v)
    (pmatch k
            (`(depth-inner-k-trp ,v^ ,k^) (let ((v^ (add1 v^)))
                                           (depth-app-k-trp k^ (if (< v^ v) v v^))))
            (`(depth-outer-k-trp ,v^ ,k^) (lambda ()
                                           (depth-trp (cdr v^) (depth-inner-k-trp v k^))))
            (`(empty-k-trp ,jumpout^) (jumpout^ v)))))

(define depth-inner-k-trp
  (lambda (v^ k^)
    `(depth-inner-k-trp ,v^ ,k^)))

(define depth-outer-k-trp
  (lambda (v^ k^)
    `(depth-outer-k-trp ,v^ ,k^)))
 
(define depth-trp
  (lambda (ls k)
    (cond
      [(null? ls) (depth-app-k-trp k 1)]
      [(pair? (car ls))
       (lambda () 
         (depth-trp (car ls) (depth-outer-k-trp ls k)))]
      [else (lambda ()
              (depth-trp (cdr ls) k))])))

(define depth-tramp-driver
  (lambda (n^)
    (call/cc
     (lambda (jumpout^)
       (trampoline
        (depth-trp
         n^
         (empty-k-trp jumpout^)))))))

;; trp fact

(define fact-app-k-trp
  (lambda (k v)
    (pmatch k
            (`(empty-k-trp ,jumpout^) (jumpout^ v))
            (`(fact-trp-k ,v^ ,k^) (fact-app-k-trp k^ (* v^ v)))
            (`(fact-outer-k-trp ,v^) (lambda ()
                                      (v v v^ k)))
            (else (k v)))))

(define fact-trp-k
  (lambda (v^ k^)
    `(fact-trp-k ,v^ ,k^)))
  
(define fact-outer-k-trp
  (lambda (v^)
    `(fact-outer-k-trp ,v^)))
  
(define fact-trp
  (lambda (n k)
    ((lambda (fact k)
      (fact fact n k))
     (lambda (fact v k)
      (cond
        [(zero? v) (fact-app-k-trp k 1)]
        [else (lambda ()
                (fact fact (sub1 v) (fact-trp-k v k)))]))
     k)))

(define fact-tramp-driver
  (lambda (n^)
    (call/cc
     (lambda (jumpout^)
       (trampoline
        (fact-trp
          n^
          (empty-k-trp jumpout^)))))))

;; trp pascal

(define pascal-app-k-trp
  (lambda (k v)
    (pmatch k
            (`(empty-k-trp ,jumpout^) (jumpout^ v))
            (`(p-innerk-trp , v^ ,k^) (pascal-app-k-trp k^ (cons v^ v)))
            (`(p-outerk-trp ,m^ ,a^ ,k^) (v (add1 m^) a^ (p-innerk-trp a^ k^)))
            (`(p-lastk-trp ,k^)  (v 1 0 k^)))))

(define p-innerk-trp
  (lambda (v^ k^)
    `(p-innerk-trp ,v^ ,k^)))

(define p-outerk-trp
  (lambda (m^ a^ k^)
    `(p-outerk-trp ,m^ ,a^ ,k^)))

(define p-lastk-trp
  (lambda (k^)
    `(p-lastk-trp ,k^)))
    
(define pascal-trp
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (pascal-app-k-trp k 
                              (lambda (m a k)
                                (cond
                                  [(> m n) (pascal-app-k-trp k '())]
                                  [else (let ((a (+ a m)))
                                          (pascal pascal (p-outerk-trp m a k)))]))))))
      (pascal pascal (p-lastk-trp k)))))
  
(define pascal-tramp-driver
  (lambda (n^)
    (call/cc
     (lambda (jumpout^)
       (trampoline
        (pascal-trp
          n^
          (empty-k-trp jumpout^)))))))




;; reg ack

(define v 'hukarz)
(define k 'hukarz)
(define m 'hukarz)
(define a 'hukarz)

(define empty-k-reg 
  (lambda ()
    `(empty-k-reg)))

(define ack-reg-k
  (lambda (v^ k^)
    `(ack-reg-k ,v^ ,k^)))

(define ack-app-k-reg
  (lambda () ;; k v
    (pmatch k
            (`(empty-k-reg) v)
            (`(ack-reg-k ,v^ ,k^) 
             (begin 
               (set! m (sub1 v^))
               (set! k k^)
               (ack-reg)))
            (else (k v)))))

(define ack-reg
  (lambda () ;; m n k
    (cond
      [(zero? m) (begin
                   (set! v (add1 v))
                   (ack-app-k-reg))]
      [(zero? v)
       (begin 
         (set! m (sub1 m))
         (set! v 1)
         (ack-reg))]
      [else (begin 
              (set! v (sub1 v))
              (set! k (ack-reg-k m k))
              (ack-reg))])))


(define ack-reg-driver
  (lambda (num1 num2)
    (begin
      (set! k (empty-k-reg))
      (set! m num1)
      (set! v num2)
      (ack-reg))))


;; reg depth

(define inner-depth-reg-k
  (lambda (v^ k^)
    `(inner-depth-reg-k ,v^ ,k^)))

(define outer-depth-reg-k
  (lambda (v^ k^)
    `(outer-depth-reg-k ,v^ ,k^)))

(define depth-app-k-reg
  (lambda ()
    (pmatch k
            [`(empty-k-reg) v]
            [`(inner-depth-reg-k ,v^ ,k^) (begin
                                            (set! k k^)
                                            (set! v (if (< (add1 v^) v) v (add1 v^)))
                                            (depth-app-k-reg))]
            [`(outer-depth-reg-k ,v^ ,k^) (begin
                                            (set! k (inner-depth-reg-k v k^))
                                            (set! v (cdr v^))
                                            (depth-reg))]
            [else (k v)])))

(define depth-reg
  (lambda ()
    (cond
      [(null? v) (begin
                   (set! v 1)
                   (depth-app-k-reg))]
      [(pair? (car v)) (begin
                         (set! k (outer-depth-reg-k v k))
                         (set! v (car v))
                         (depth-reg))]
      [else (begin
              (set! v (cdr v))
              (depth-reg))])))

(define depth-reg-driver
  (lambda (ls)
    (begin
      (set! v ls)
      (set! k (empty-k-reg))
      (depth-reg))))

;; reg fact


(define fact-app-k-reg
  (lambda ()
    (pmatch k
            (`(empty-k-reg) v)
            (`(fact-reg-k ,v^ ,k^) (begin (set! k k^)
                                         (set! v (* v^ v))
                                        (fact-app-k-reg)))
            (`(fact-outer-k-reg ,v^) (v v v^ k))
            (else (k v)))))

(define fact-reg-k
  (lambda (v^ k^)
    `(fact-reg-k ,v^ ,k^)))
  
(define fact-outer-k-reg
  (lambda (v^)
    `(fact-outer-k-reg ,v^)))
  
(define fact-reg
  (lambda ()
    ((lambda (fact)
      (fact fact))
     (lambda (fact)
      (cond
        [(zero? v) (begin (set! v 1)
                         (fact-app-k-reg))]
        [else (begin (set! v (sub1 v))
                     (set! k (fact-reg-k k v))
                (fact fact))])))))

(define fact-reg-driver
  (lambda (n)
    (begin
      (set! v n)
      (set! k (empty-k-reg))
      (fact-reg))))

;; an pascal

(define pascal-app-k-reg
  (lambda ()
    (pmatch k
            (`(empty-k-reg) v)
            (`(p-innerk-reg ,v^ ,k^) (begin
                                      (set! k k^)
                                      (set! v (cons v^ v))
                                      (pascal-app-k-reg)))
            (`(p-outerk-reg ,m^ ,a^ ,k^) (begin
                                          (set! m (add1 m^))
                                          (set! a a^)
                                          (set! k (p-innerk-reg a^ k^))
                                          (v)))
            (`(p-lastk-reg ,k^) (begin
                                 (set! m 1)
                                 (set! a 0)
                                 (set! k k^)
                                 (v)))
            (else (k v)))))

(define p-innerk-reg
  (lambda (v^ k^)
    `(p-innerk-reg ,v^ ,k^)))

(define p-outerk-reg
  (lambda (m^ a^ k^)
    `(p-outerk-reg ,m^ ,a^ ,k^)))

(define p-lastk-reg
  (lambda (k^)
    `(p-lastk-reg ,k^)))
    
(define pascal-reg
  (lambda (n)
    (let ((pascal
           (lambda (pascal)
             (pascal-app-k-reg k 
                              (lambda (m a k)
                                (cond
                                  [(> m n) (pascal-app-k-reg k '())]
                                  [else (let ((a (+ a m)))
                                          (pascal pascal (p-outerk-reg m a k)))]))))))
      (pascal pascal (p-lastk-reg k)))))

(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! v n)
      (set! a 0)
      (set! k (empty-k-reg))
      (pascal-reg))))
#lang racket
(require "c311/pmatch.rkt")
(require "c311/let-pair.rkt")
(require "c311/trace.rkt")


(define filter-sps
   (lambda (bool ls sp)
     (cond
       [(null? ls) `(,'() . ,sp)]
       [else
        (let-pair ((ls-ac . s^) (filter-sps bool (cdr ls) sp))
                  (if (bool (car ls))
                      `(,(cons (car ls) ls-ac) . ,s^)
                      `(,ls-ac . ,(cons (car ls) s^))))])))



(define filter*-sps
  (lambda (bool ls sp)
    (cond
      [(null? ls) `(,'() . ,sp)]
      [(pair? (car ls)) (let ((nex (car ls)))
                          (let-pair ((ls-ac . s^) (filter*-sps bool (cdr ls) sp))
                                    (let-pair ((ls-ac^ . s^^) (filter*-sps bool nex s^))
                                              (if (bool (car nex))
                                                  `(,(cons `(,(cons (car nex) ls-ac) . ,s^^) ls-ac) . s^)
                                                  `(,ls-ac . ,(cons `(,ls-ac . ,(cons (car nex) s^^)) s^))))))]
      [else 
       (let-pair ((ls-ac . s^) (filter*-sps bool (cdr ls) sp))
                 (if (bool (car ls))
                     `(,(cons (car ls) ls-ac) . ,s^)
                     `(,ls-ac . ,(cons (car ls) s^))))])))


(define fib-sps
  (lambda (num s)
    (cond
      ((assv num s) => (lambda (pr) `(,(cdr pr) . ,s)))
      ((< num 2) `(,num . ((,num . ,num) . ,s)))
      (else
       (let-pair ((sec . s^) (fib-sps (sub1 (sub1 num)) s))
         (let-pair ((fir . s^^) (fib-sps (sub1 num) s^))
           (let ((sec+fir (+ sec fir)))
             `(,sec+fir . ((,num . ,sec+fir) . ,s^^)))))))))
                                  

(define-syntax and*
  (syntax-rules ()
    ((_) #t)
    ((_ e) e)
    ((_ e0 e1 e ...)
     (if e0
         (and* e1 e ...)
         #f))))

(define-syntax cons*
  (syntax-rules ()
    ((_) (raise-syntax-error "Incorrect argument-count to cons*"))
    ((_ e) e)
    ((_ e0 e1 e ...)
     `(,e0 . ,(cons* e1 e ...)))))

(define-syntax macro-list
  (syntax-rules ()
    ((_) '())
    ((_ e) '(e))
    ((_ e0 e1 e ...)
     `(,e0 . ,(macro-list e1 e ...)))))

(define-syntax mcond
  (syntax-rules (else)
    ((_) '())
    ((_ `(else . ,e)) e)
    ((_ `(,i . e)) (if i
                        e))
    ((_  `(,e0 . 0E) e1 e ...)
     (if e0
         0E
         (mcond e1 e ...)))))

(define-syntax macro-map
  (syntax-rules ()
    ((_) (raise-syntax-error "Incorrect argument-count to macro-map"))
    ((_ e) (raise-syntax-error "Incorrect argument-count to macro-map"))
    ((_ lam ls) (if (eqv? (cdr ls) '())
                    (cons (lam (car ls)) '())
                    `(,(lam (car ls)) . ,(macro-map lam (cdr ls)))))))
                          


    
    
               

                      
                                  
                
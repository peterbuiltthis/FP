#lang racket
(require racket/trace)
(require "c311/pmatch.rkt")


(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

;; Part 1:
;; 1.
(define last-non-zero
  (lambda (ls) 
    (call/cc
     (lambda (k)
       (letrec
         ([lnz
           (lambda (ls)
             (cond
               [(null? ls) '()]
               [(zero? (car ls)) (k (lnz (cdr ls)))]
               [else (cons (car ls) (lnz (cdr ls)))]))])
           (lnz ls))))))

(define my-*
    (lambda (m n)
      (* m n)))

;; 2.
(define mult/cc
    (lambda (n*)
      (call/cc
       (lambda (k)
         (letrec
           ((m/cc
             (lambda (n*)
               (cond
                 [(null? n*) 1]
                 [(zero? (car n*)) (k 0)]
                 [else (my-* (car n*) (m/cc (cdr n*)))]))))
           (m/cc n*))))))

;; Part 2.

;; 3.
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (v)
                              (k (* (car ls) v))))])))

;; 4.
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (v)
                              (k (* (car ls) v))))])))

;; 5.

(define plus-cps
  (lambda (m k)
    (lambda (n)
      (k (+ m n)))))

;; 6.

(define count-syms*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [(pair? (car ls)) (count-syms*-cps (car ls) (lambda (v) 
                                                (count-syms*-cps (cdr ls) (lambda (x)
                                                                        (k (+ v x))))))]
      [(symbol? (car ls)) (count-syms*-cps (cdr ls) (lambda (v)
                                                        (k (add1 v))))]
      [else (count-syms*-cps (cdr ls) k)])))

;; 7.

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls) (cons-cell-count-cps (car ls) (lambda (v)
                                              (cons-cell-count-cps (cdr ls) (lambda (x)
                                                                          (k (add1 (+ v x)))))))]
      [else (k 0)])))

;; 8.

(define walk-cps
  (lambda (v ls k)
    (cond
      [(symbol? v)
       (let ((p (assq v ls)))
         (cond
           [p (walk-cps (cdr p) ls k)]
           [else (k v)]))]
      [else (k v)])))

;; 9.

(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v)
                                  (ack-cps (sub1 m) v k)))])))

                     
;; 10.

(define fib-cps
  (lambda (n k)
    ((lambda (fib k)
       (fib fib n k))
     (lambda (fib n k)
       (cond
	 [(zero? n) (k 0)]
	 [(= 1 n) (k 1)]
	 [else (fib fib (sub1 n) (lambda (v)
                                   (fib fib (sub1 (sub1 n)) (lambda (x)
                                                              (k (+ v x))))))])) k)))


;; 11.

(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k)
       ((h h k) seed '() k))
     (lambda (h k)
       (lambda (seed ans k)
	 (if (p seed k)
	     ans
	     ((h h k) (g seed k) (cons (f seed k) ans) k)))) k)))

(define null?-cps
    (lambda (ls k)
      (k (null? ls))))
(define car-cps
    (lambda (pr k)
      (k (car pr))))
(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

;; 12.

(define empty-s
  (lambda ()
    '()))
 
(define extend-s-cps
  (lambda (x v s k)
    (k (cons `(,x . ,v) s))))
 
(define unify-cps
  (lambda (v w s k)
    (let ([v (walk-cps v s k)])
      (let ([w (walk-cps w s k)])
        (cond
          [(eqv? v w) (k s)]
          [(symbol? v) (extend-s-cps v w s k)]
          [(symbol? w) (extend-s-cps w v s k)]
          [(and (pair? v) (pair? w))
           (let ((s (unify-cps (car v) (car w) s k)))
             (cond
               [s (unify-cps  (cdr v) (cdr w) s k)]
               [else #f]))]
          [(equal? v w) (k s)]
          [else #f])))))


;; 13.

(define M-cps
  (lambda (f k)
    (lambda (ls)
      (cond
        ((null? ls) (k '()))
        (else ((M-cps f (lambda (v) (k (cons (f (car ls)) v)))) (cdr ls)))))))




;; 14. 

(define use-of-M-cps
  ((M-cps (lambda (n) (add1 n)) (empty-k)) '(1 2 3 4 5)))




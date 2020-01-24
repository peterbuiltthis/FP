#lang racket
(require "c311/pmatch.rkt")
(require "c311/mk.rkt")
(require "c311/numbers.rkt")
(provide (all-defined-out))

;; worked with jacebrat and vjoshua

;; 1. Listo
(define caro
  (lambda (p a)
    (fresh (d)
           (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
           (== (cons a d) p))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define nullo
  (lambda (p)
    (== p '())))

(define pairo
  (lambda (p)
    (fresh (a d)
           (conso a d p))))


(define listo
  (lambda (ls)
    (conde
     ((nullo ls) (== #t #t))
     ((pairo ls) (fresh (d)
                        (cdro ls d)
                        (listo d)))
     ((== #f #t)))))

;; 2. facto

(define bin->nat
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [else (+ (* (bin->nat (cdr ls)) 2) (car ls))])))


(define bit-nando
  (lambda (x y r)
    (conde
     ((== 0 x) (== 0 y) (== 1 r))
     ((== 1 x) (== 0 y) (== 1 r))
     ((== 0 x) (== 1 y) (== 1 r))
     ((== 1 x) (== 1 y) (== 0 r))
     ((== #t #f)))))

(define fact
  (lambda (num)
    (cond
      [(zero? (sub1 num)) 1]
      [else (* num (fact (sub1 num)))])))


(define bit-xoro
  (lambda (x y r)
    (fresh (s t u)
           (bit-nando x y s)
           (bit-nando x s t)
           (bit-nando s y u)
           (bit-nando t u r))))

(define bit-noto
  (lambda (x r)
    (bit-nando x x r)))

(define bit-ando
  (lambda (x y r)
    (fresh (s)
           (bit-nando x y s)
           (bit-noto s r))))

(define facto
  (lambda (bin1 bin2)
    (conde
     ((== bin1 '()) (== bin2 '(1)))
     ((fresh (n factn)
            (minuso bin1 '(1) n)
            (facto n factn)
            (*o factn bin1 bin2))))))
           

;; 3. fibso
(define fibs
  (lambda (n)
      (cond
        ((eqv? n 0) (values 1 1))
        (else
         (let ((n- (- n 1)))
           (let-values (((u v) (fibs n-)))
             (let ((u+v (+ u v)))
               (values v u+v))))))))

(define fibso
  (lambda (n o1 o2)
    (conde
     ((== n '()) (== o1 '(1)) (== o2 '(1)))
     ((fresh (n- u v) 
             (minuso n '(1) n-)
             (fibso n- u v)
             (== o1 v)
             (minuso o2 v u))))))
      

;; 4. fo-lavo

(define lookupo
  (lambda (x vars vals o)
    (fresh (y vars^ v vals^)
      (== `((,y . ,vars^) . (,v . ,vals^))  `(,vars . ,vals))
      (conde
        ((== x y) (== v o))
        ((=/= x y) (lookupo x vars^ vals^ o))))))

(define valof*o
  (lambda (rands vars vals o)
    (conde
      [(== `() rands) (== `() o)]
      [(fresh (rand rands^)
         (== `(,rand . ,rands^) rands)
         (fresh (v vs)
           (== `(,v . ,vs) o)
           (valofo rand vars vals v)
           (valof*o rands^ vars vals vs)))])))

(define valofo
  (lambda (exp vars vals o)
    (conde
      [(== `(quote ,o) exp) (absento 'closure o)]
      [(fresh (rands)
         (== `(list . ,rands) exp)
         (valof*o rands vars vals o))]
;;    [(numbero exp) (== exp o)]
      [(symbolo exp) (lookupo exp vars vals o)]
      [(fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (== `(closure ,x ,body ,vars ,vals) o))]
      [(fresh (rator rand)
         (== `(,rator ,rand) exp)
         (fresh (x body vars^ vals^)
           (valofo rator vars vals `(closure ,x ,body ,vars^ ,vals^))
           (fresh (v)
             (valofo rand vars vals v)
             (valofo body `(,x . ,vars^) `(,v . ,vals^) o))))])))


(define fo-lav*o
  (lambda (args vars vals o)
    (conde
     [(== args '()) (== o '())]
     [(fresh (arg args^ v vs)
             (== args `(,arg . ,args^))
             (== o `(,v . ,vs))
             (fo-lav*o args^ vars vals vs)
             (fo-lavo arg vars vals v))])))

(define fo-lavo
  (lambda (exp vars vals o)
   (absento 'closure vars)
   (absento 'etouq vars)
    (conde
     [(symbolo exp) (lookupo exp vars vals o)]
     [(== `(,o etouq) exp) (absento 'closure o)
                           (absento 'etouq vars)]
     [(fresh (rands)
             (appendo rands '(tsil) exp)
             (fo-lav*o rands vars vals o))]
     [(fresh (x body)
             (== `(,body (,x) adbmal) exp)
             (absento 'adbmal vars)
             (symbolo x)
             (absento 'closure x)
             (absento 'etouq x)
             (== `(closure ,x ,body ,vars ,vals) o))]
     [(fresh (rator rand)
             (== `(,rand ,rator) exp)
             (fresh (x body vars^ vals^)
             (fo-lavo rand vars vals `(closure ,x ,body ,vars^ ,vals^))
             (fresh (v)
                    (fo-lavo rator vars vals v)
                    (fo-lavo body `(,x . ,vars^) `(,v . vals^) o))))])))

;; 5. Color Middle Earth

(define middle-earth
    '((lindon eriador forodwaith)
      (forodwaith lindon rhovanion eriador)
      (eriador lindon forodwaith rhovanion enedwaith)
      (rhovanion forodwaith eriador enedwaith rohan rhun)
      (enedwaith eriador rhovanion rohan gondor)
      (rohan enedwaith rhovanion rhun gondor mordor)
      (gondor enedwaith rohan mordor)
      (rhun rohan rhovanion khand mordor)
      (mordor gondor rohan rhun khand harad)
      (khand mordor rhun harad)
      (harad mordor khand)))

(define membero
  (lambda (x ls)
    (=/= '() ls)
    (fresh (a d)
           (== `(,a . ,d) ls)
           (conde
            [(== a x)]
            [(membero x d)]))))


(define color-middle-earth
  (lambda (col)
     (run 1 (q) (cme col q))))

(define cme 
  (lambda (colors o)
    (fresh (l f e r e2 r2 g r3 m k h)
            (== `((lindon . ,l) (forodwaith . ,f) (eriadior . ,e)
                  (rhovanion . ,r) (enedwaith . ,e2) (rohan . ,r2)
                  (gondor . ,g) (rhun . ,r3) (mordor . ,m)
                  (khand . ,k) (harad . ,h)) o)
           (membero l colors) (=/= l e) (=/= l f)
           (membero f colors) (=/= f l) (=/= f r) (=/= f e)
           (membero e colors) (=/= e l) (=/= e f) (=/= e r) (=/= e e2)
           (membero r colors) (=/= r f) (=/= r e) (=/= r e2) (=/= r r2) (=/= r r3)
           (membero e2 colors) (=/= e2 e) (=/= e2 r) (=/= e2 r2) (=/= e2 g)
           (membero r2 colors) (=/= r2 e2) (=/= r2 r) (=/= r2 r3) (=/= r2 g) (=/= r2 m)
           (membero g colors) (=/= g e2) (=/= g r2) (=/= g m)
           (membero r3 colors) (=/= r3 r2) (=/= r3 r) (=/= r3 k) (=/= r3 m)
           (membero m colors) (=/= m g) (=/= m r2) (=/= m r3) (=/= m k) (=/= m h)
           (membero k colors) (=/= k m) (=/= k r3) (=/= k h)
           (membero h colors) (=/= h m) (=/= h k))))





;; just dessert

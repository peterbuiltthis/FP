#lang racket
(require "C311/mk.rkt")
(require "C311/numbers.rkt")
(require "c311/let-pair.rkt")
(provide (all-defined-out) (all-from-out "C311/mk.rkt") (all-from-out "C311/numbers.rkt"))

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 
(run* (q)
  (== 5 q)
  (conde
    [(conde [(== 5 q)
	     (== 6 q)])
     (== 5 q)]
    [(== q 5)]))

;; This returns '(5) because before anything else happens the program creates a relation
;; between 5 and q which is the output. and since the remaining relational questions create
;; no other valid output the only output is the relation of 5 to q
;;


;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a))) 

;; this returns '(((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1))))
;; because when fresh creates 2 new variables associated to q it assigns
;; _.0 and _.1 to be a and d because these are fresh variables and unbound
;; , but there is a value there we just don't know it yet. Next are in the output is the answer
;; that states q can't be associated with the at term that is tagged term. The final part
;; output states that a must be a symbol but that it can't be the symbol tag.


;; 3 What do the following miniKanren constraints mean?
;; a not-pairo: restricts an association to not be a pair (l
;; b =/=: restrics an association to not be a specific value
;; c absento: makes sure a symbol does not apear within a term
;; d numbero: restricts the variable to a numer
;; e symbolo: restrics the variable to e a symbol

;; Part II goes here.


;; Assoco

(define assoco
  (lambda (x ls out)
    (fresh (a d)
       (== `(,a . ,d) ls)
       (fresh (aa da)
         (== `(,aa . ,da) a)
         (conde
           ((== aa x) (== a out))
           ((=/= aa x) (assoco x d out)))))))

;; reverso

(define reverseo
  (lambda (ls out)
    (conde
     ((== '() ls) (== ls out))
     ((=/= '() ls) (fresh (a d)
                          (== `(,a . ,d) ls)
                          (fresh (res)
                                 (reverseo d res)
                                 (appendo res `(,a) out)))))))

;; stuttero

(define stuttero
  (lambda (ls out)
    (conde
     ((== '() ls) (== ls out))
     ((=/= '() ls) (fresh (a d)
                         (== `(,a . ,d) ls)
                         (fresh (res)
                                (== `(,a ,a . ,res) out)
                                (stuttero d res)))))))

;; lengtho

(define lengtho
  (lambda (ls out)
    (conde
     ((== '() ls) (== ls out))
     ((fresh (a d)
             (== `(,a . ,d) ls)
             (fresh (res)
                    (lengtho d res)
                    ))))))

      




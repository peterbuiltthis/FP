#lang racket
(require "c311/monads.rkt")
(require "c311/pmatch.rkt")


(define traverse
    (lambda (return bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (do bind
                  (a <- (trav (car tree)))
                  (d <- (trav (cdr tree)))
                  (return (cons a d)))]
               [else (f tree)]))))
        trav)))


;; 1. Assv-Maybe
(define assv-maybe
  (lambda (ltr ls)
    (cond
      [(null? ls) (fail)]
      [(eqv? (car (car ls)) ltr) (return-maybe (cdr (car ls)))]
      [else (assv-maybe ltr (cdr ls))])))

;; 2. Partition-Writer
(define partition-writer
  (lambda (bool ls)
    (cond
      [(null? ls) (return-writer '())]
      [(bool (car ls)) (bind-writer
                        (tell-writer (car ls))
                        (lambda (_)
                          (partition-writer bool (cdr ls))))]
      [else
       (bind-writer
        (partition-writer bool (cdr ls))
        (lambda (d)
          (return-writer
           (cons (car ls) d))))])))

;; 3. Power

(define powerXpartials
  (lambda (num pow)
    (cond
      [(zero? pow) (return-writer '())]
      [(= pow 1) (return-writer num)]
      [(odd? pow) (bind-writer
                   (powerXpartials num (sub1 pow))
                   (lambda (d)
                     (bind-writer
                      (tell-writer d)
                      (lambda (v)
                        (return-writer (* d num))))))]
      [else (bind-writer
             (powerXpartials num (/ pow 2))
             (lambda (d)
               (bind-writer
                (tell-writer d)
                (lambda (v)
                  (return-writer (* d d))))))])))


               


;; 4. abc game

(define abc-game
  (λ (l)
    (cond
      [(null? l) (return-state '_)]
      [(eqv? (car l) 'a)
       (do bind-state
         (s <- get-state)
         (put-state (add1 s))
         (abc-game (cdr l)))]
      [(eqv? (car l) 'b)
       (do bind-state
         (s <- get-state)
         (put-state (sub1 s))
         (abc-game (cdr l)))]
      [else
       (do bind-state
         (s <- get-state)
         (put-state s)
         (abc-game (cdr l)))])))
  
  
;; 5. reciprocal

(define reciprocal
  (lambda (num)
    (cond
      [(zero? num) (fail)]
      [else (return-maybe (/ 1 num))])))

(define traverse-reciprocal
  (traverse return-maybe bind-maybe reciprocal))

;; 6. halve/traverse-halve

(define halve
  (lambda (num)
    (cond
      [(zero? (remainder num 2)) (return-writer (/ num 2))]
      [else (do bind-writer
             (tell-writer num)
             (return-writer num))])))
  
(define traverse-halve
    (traverse return-writer bind-writer halve))

;; 7. state/sum

(define state/sum
  (lambda (num)
    (do bind-state
       (s <- get-state)
       (put-state (+ num s))
       (return-state s))))
      
(define traverse-state/sum
    (traverse return-state bind-state state/sum))      
#lang racket

;; Escribe aquí tus soluciones

;; 1.

(define (countdown n)
  (if (= n -1)
	null
	(cons n (countdown (- n 1)))))

;; 2.

(define (insertL x y ls)
  (if (empty? ls)
      null
      (if (eq? (first ls) x)
          (cons y (cons x (insertL x y (rest ls))))
          (cons (first ls) (insertL x y (rest ls))))))

;; 3.

(define (remv-1st x ls)
  (cond
    ((empty? ls) null)
    ((eqv? (first ls) x) (rest ls))
    (else (cons (first ls) (remv-1st x (rest ls))))))

(provide (all-defined-out))

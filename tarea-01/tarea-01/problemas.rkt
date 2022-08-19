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

;; 4.

(define (map x ls)
  (if (empty? ls)
      null
      (cons (x (first ls)) (map x (rest ls)))))

;; 5.

(define (filter x ls)
  (cond
    [(empty? ls) null]
    [(x (first ls)) (cons (first ls) (filter x (rest ls)))]
    [else  (filter x (rest ls))]))

;; 6.

(define (zip ls1 ls2)
  (if (or (empty? ls1) (empty? ls2))
      null
      (cons (list (first ls1) (first ls2))
            (zip  (rest ls1) (rest ls2)))))

;; 9.

(define (reverse ls)
  (if (empty? ls)
      null
      (append (reverse (rest ls))
              (list (first ls)))))

;; 18.

(define (powerset ls)
  (if (empty? ls)
      (list ls)
      (let ([ps (powerset (rest ls))])
        (append (f (first ls) ps)
        ps))))

(define (f x ls)
  (if (empty? ls)
      null
      (cons (cons x (first ls))
            (f x (rest ls)))))

(provide (all-defined-out))

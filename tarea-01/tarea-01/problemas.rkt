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

;; 7.

(define (list-index-ofv x ls)
  (cond
    [(empty? ls) null]
    [(eqv? x (first ls)) 0]
    [else (+ (list-index-ofv x (rest ls)) 1)]))

;; 8.

(define (append ls1 ls2)
  (cond
    [(and (empty? ls1)(empty? ls2)) null]
    [(empty? ls1) ls2]
    [(empty? ls2) ls1]
    [else (cons (first (first (list ls1 ls2)))
                (append (rest ls1) ls2))]))

;; 9.

(define (reverse ls)
  (if (empty? ls)
      null
      (append (reverse (rest ls))
              (list (first ls)))))

;; 10.

(define (repeat ls x)
  (cond [(= x 0) null]
        [(>= x 0) (append ls (repeat ls (- x 1)))]))

;; 11.

(define (same-lists* ls1 ls2)
  (cond
    [(and (empty? ls1) (empty? ls2)) #t]
    [(or (empty? ls1) (empty? ls2)) #f]
    [(equal? (first ls1) (first ls2)) (same-lists* (rest ls1) (rest ls2))]
    [else #f]))

;; 12.

;; 13.

(define (binary->natural n)
  (if (null? n)
      0 
      (+ (first n) (* 2 (binary->natural (rest n))))))

;; 14.

(define (div a b)
  (cond
    [(< a b) 0]
    [(= b 0) "Division entre 0"]
    [(= (modulo a b) 0) (+ (div (- a b) b) 1)]
    [else "Solo numeros enteros"]))
  

;; 15.

(define append-map (lambda (proc ls)
                     (cond
                       [(empty? ls) ls]
                       [else (append (proc(first ls)) (append-map proc(rest ls)))])))


;; 16.

(define (set-difference ls1 ls2)
  (cond
    [(empty? ls1) null]
    [(empty? ls2) ls1]
    [(eq? (member (first ls1) ls2) #f) (cons (first ls1) (set-difference (rest ls1) ls2))]
    [else (set-difference (rest ls1) ls2)]))

;; 17.

(define (foldr op n ls)
  (cond
    [(empty? ls) n]
    [else (op (first ls) (foldr op n (rest ls)))]))

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

;; 19.

;; 20.

;; 21.

;; 22.

(provide (all-defined-out))

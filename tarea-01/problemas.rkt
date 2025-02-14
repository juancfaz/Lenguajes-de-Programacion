#lang racket

;; Escribe aquí tus soluciones

;; 1.

(define (countdown n)
  (cond
    [(= n -1) null]
    [else (cons n (countdown (- n 1)))]))

;; 2.

(define (insertL x y ls)
  (cond
    [(empty? ls) null]
    [(eq? (first ls) x) (cons y (cons x (insertL x y (rest ls))))]
    [else (cons (first ls) (insertL x y (rest ls)))]))

;; 3.

(define (remv-1st x ls)
  (cond
    [(empty? ls) null]
    [(eqv? (first ls) x) (rest ls)]
    [else (cons (first ls) (remv-1st x (rest ls)))]))

;; 4.

(define (map x ls)
  (cond
    [(empty? ls) null]
    [else (cons (x (first ls)) (map x (rest ls)))]))

;; 5.

(define (filter x ls)
  (cond
    [(empty? ls) null]
    [(x (first ls)) (cons (first ls) (filter x (rest ls)))]
    [else  (filter x (rest ls))]))

;; 6.

(define (zip ls1 ls2)
  (cond
    [(or (empty? ls1) (empty? ls2)) null]
    [else (cons (cons (first ls1) (first ls2))
                (zip (rest ls1) (rest ls2)))]))
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
  (cond
    [(empty? ls) null]
    [else (append (reverse (rest ls))
              (list (first ls)))]))

;; 10.

(define (repeat ls x)
  (cond
    [(= x 0) null]
    [(>= x 0) (append ls (repeat ls (- x 1)))]))

;; 11.

(define (same-lists* ls1 ls2)
  (cond
    [(and (empty? ls1) (empty? ls2)) #t]
    [(or (empty? ls1) (empty? ls2)) #f]
    [(equal? (first ls1) (first ls2)) (same-lists* (rest ls1) (rest ls2))]
    [else #f]))

;; 12.

#|

(a b) y (a . (b . ()))

siguiendo los mismos pasos del pdf
conseguí el siguiente resultado:

(w x) y (z) = (w. (x . ())) y (z . ())

|#

;; 13.

(define (binary->natural n)
  (cond
    [(null? n) 0]
    [else (+ (first n) (* 2 (binary->natural (rest n))))]))

;; 14.

(define (div a b)
  (cond
    [(< a b) 0]
    [(= b 0) -1]
    [(= (modulo a b) 0) (+ (div (- a b) b) 1)]
    [else -1]))
  

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

(define (foldr op s ls)
  (cond
    [(empty? ls) s]
    [else (op (first ls) (foldr op s (rest ls)))]))

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

(define (cartesian-product ls)
  (combine-cartesian (first ls) (first (rest ls))))

(define (combine-cartesian ls1 ls2)
  (append-map (lambda (x)
                (map (lambda (y)
                       (list x y))
                     ls2))
              ls1))

;; 20.

;; 21.

(define snowball
  (letrec
    ((odd-case
       (lambda (fix-odd)
	 (lambda (x)
	   (cond
	     ((and (exact-integer? x) (positive? x) (odd? x))
	      (snowball (add1 (* x 3))))
	     (else (fix-odd x))))))
     (even-case
       (lambda (fix-even)
	 (lambda (x)
	   (cond
	     ((and (exact-integer? x) (positive? x) (even? x))
	      (snowball (/ x 2)))
	     (else (fix-even x))))))
     (one-case
       (lambda (fix-one)
	 (lambda (x)
	   (cond
	     ((zero? (sub1 x)) 1)
	     (else (fix-one x))))))
     (base
       (lambda (x)
	 (error 'error "Invalid value ~s~n" x))))
    (one-case (even-case (odd-case base)))))

(define quine "F")

(provide (all-defined-out))

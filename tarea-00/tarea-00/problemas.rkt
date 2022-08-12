 #lang racket

;; 1.
(define pi 3.14)

;; 2.
(define (area-circle r)
  (* pi (expt r 2)))


;; 3.
(define (circle-properties r)
  (list (area-circle r) (* 2 pi r)))

;; 4.
(define (rectangle-properties rec)
  (list (* (first rec) (second rec))
        (* 2 (+ (first rec) (second rec)))))

;; 5.
(define (find-needle ls)
  ...)

;; 6.
(define (abs x)
  (cond
    ((>= x 0) x)
    ((< x 0) (* x -1))))

;; 7.
(define (inclis1 ls)
  (map add1 ls))

;; 8.
(define (even? x)
  ...)

;; 9.
(define another-add
  (lambda (n m)
    ...))

(provide (all-defined-out))
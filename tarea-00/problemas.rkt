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
   (let loop ((ls ls)
              (idx 0))
   (cond ((empty? ls)-1)
         ((equal? (first ls) 'needle) idx)
         (else (loop (rest ls) (add1 idx))))))

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
  (if (= (modulo x 2) 0) #t #f))

;; 9.
(define another-add
  (lambda (n m)
    (cond
      ((zero? n) m)
      (else (add1 (another-add (sub1 n) m))))))

(provide (all-defined-out))
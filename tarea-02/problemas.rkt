#lang racket

(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (first x))
           (= (string-length (first x)) 1) (unit-string-list? (rest x)))))

(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

(define (take l n)
  (cond
    [(empty? l) null]
    [(= n 0) null]
    [else (cons (first l) (take (rest l) (- n 1)))]))

(define (drop l n)
  (cond
    [(= n 0) l]
    [(empty? l) null]
    [else (drop (rest l) (- n 1))]))

(define (list->chunks ls n)
  (cond
    [(empty? ls) null]
    [else (cons (implode (take ls n)) (list->chunks (drop ls n) n))]))

(define (bundle s n)
  (cond
    [(null? s) null]
    [else (list->chunks s n)]))

(define (partition s n)
  (cond
    [(= (string-length s) 0) null]
    [else (cons (substring (mod s n) 0 n) (partition (substring (mod s n) n (string-length (mod s n)) ) n))]))

(define (mod s n)
  (cond
    [(not (zero? (modulo (string-length s) n))) (mod (string-append s " ") n)]
    [else s]))

(define (remv-wh ls)
  (cond
    [(empty? ls) null]
    [(not (eq? (rest ls) " ")) (cons (first ls) (remv-wh (rest ls)))]
    [else null]))
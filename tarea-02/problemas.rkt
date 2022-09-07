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

(define (empty ls)
  (cond
    [(empty? ls) null]
    [else (cons (first ls) (empty (rest ls)))]))

(define (remv-wh ls)
  (cond
    [(empty? ls) null]
    [(not (eq? (rest ls) " ")) (cons (first ls) (remv-wh (rest ls)))]
    [else null]))

(define (isort ls predicado)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) predicado) predicado)))
 

(define (insert n ls predicado)
  (cond
    [(empty? ls) (list n)]
    [(predicado n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls) predicado))]))

(define (quicksort ls arg)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort (smallers ls pivot arg) arg)
             (list pivot) (quicksort (largers ls pivot arg) arg))]))

#|Problema 10|#
(define (smallers ls pivot arg)
  (filter (lambda (x) (arg x pivot)) ls))

(define (largers ls pivot arg)
  (filter (lambda (x) (and (not (arg x pivot)) (not (equal? x pivot)))) ls))

#|
Problema 11

'(1 2 1)
si tomamos el pivote, y lo comparamos
el segundo elemento, el segundo elemento
es mayor que el pivote, por lo tanto
hay una lista con el elemento 2 '(2)
donde el pivote ahora es 2. Con
el siguiente elemento pasa que el 1
es igual y no menor, ni mayor que el
privote, por lo tanto tenemos '0.


por lo que al final nuestro algoritmo
juntara los pivotes y el resultado es:

'(1 2)
|#
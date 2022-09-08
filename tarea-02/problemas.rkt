#lang racket

; unit-string? : -> boolean
(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))

; unit-string-list? : list? -> boolean
(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (first x))
           (= (string-length (first x)) 1)
           (unit-string-list? (rest x)))))

; explode : string? -> list
(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

; implode : list? -> string
(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

; take : list?, integer? -> list
(define (take l n)
  (cond
    [(empty? l) null]
    [(= n 0) null]
    [else (cons (first l) (take (rest l) (- n 1)))]))

; drop : list?, integer? -> list
(define (drop l n)
  (cond
    [(= n 0) l]
    [(empty? l) null]
    [else (drop (rest l) (- n 1))]))

; list->chunks : list?, integer? -> list
(define (list->chunks ls n)
  (cond
    [(empty? ls) null]
    [else (cons (implode (take ls n)) (list->chunks (drop ls n) n))]))

; bundle : (list-of-symbol?), integer? -> list
(define (bundle s n)
  (unless (unit-string-list? s)
	(error 'bundle "esperaba una lista de cadenas unitarias, pero recibí: ~e" s))
  (unless (and (> n 0) (list? s))
    (error 'bundle "n debe ser mayor a 0, pero recibí: ~e" n))
  (cond
    [(null? s) null]
    [else (cons (implode (take s n)) (bundle (drop s n) n))]))

; partition :
(define (partition s n)
  (append (reverse (rest (reverse (partition-aux s n)))) (remv-wh (partition-aux s n))))

; partition-aux : string?, integer? -> list
(define (partition-aux s n)
  (cond
    [(= (string-length s) 0) null]
    [else (cons (substring (mod s n) 0 n) (partition-aux (substring (mod s n) n (string-length (mod s n)) ) n))]))

; remv-wh : list? -> list
(define (remv-wh ls)
  (cond
    [(empty? ls) null]
    [else (cons (string-trim-both (last ls)) null)]))

; mod : string?, integer? -> string
(define (mod s n)
  (cond
    [(not (zero? (modulo (string-length s) n))) (mod (string-append s " ") n)]
    [else s]))

; string-trim-both : string? -> string
(define string-trim-both
  (let ((r (regexp "^[ \t\r]*(.*?)[ \t\r]*$")))
    (lambda (s)
      (cadr (regexp-match r s)))))

; isort : list?, proc? -> list
(define (isort ls predicado)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) predicado) predicado)))
 
; insert : integer?, list?, proc? -> list
(define (insert n ls arg)
  (cond
    [(empty? ls) (list n)]
    [(arg n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls) arg))]))

; quicksort : list?, proc? -> list
(define (quicksort ls arg)
  (unless (procedure? arg) (error 'quicksort "Esperaba un predicado valido, recibi ~e" arg))
  (cond
    [(empty? ls) null]
    [(< (length ls) 50) (isort ls arg)]
    [else
     (define pivot (first ls))
     (define smallers (filter (lambda (x) (arg x pivot)) ls))
     (define largers  (filter (lambda (x) (and (not (arg x pivot)) (not (equal? x pivot)))) ls))
     (append (quicksort (smallers ls pivot arg) arg)
             (list pivot) (quicksort (largers ls pivot arg) arg))]))

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
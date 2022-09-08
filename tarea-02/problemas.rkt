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

;; Problema 3.

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

;; Problema 6.

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

;; Problema 7.

; partition : string?, integer? -> list
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

;; Problema 8.

; isort : list?, proc? -> list
(define (isort ls arg)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) arg) arg)))
 
; insert : integer?, list?, proc? -> list
(define (insert n ls arg)
  (cond
    [(empty? ls) (list n)]
    [(arg n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls) arg))]))

;; Problema 10 - 15

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

(define (gcd-structural n m)
  (define (find-largest-divisor k)
    (cond [(= k 1) 1]
          [(= (remainder n k) (remainder m k) 0) k]
          [else (find-largest-divisor (- k 1))]))
  (find-largest-divisor (min n m)))

(define (gcd-generative n m)
  (define (find-largest-divisor max min)
    (if (= min 0)
        max
        (find-largest-divisor min (remainder max min))))
  (find-largest-divisor (max n m) (min n m)))

(require pict)
(require racket/draw)

(define (triangle side width color)
  (define w side)
  (define h (* side (sin (/ pi 3))))
  (define (draw-it ctx dx dy)
	(define prev-pen (send ctx get-pen))
	(define path (new dc-path%))
	(send ctx set-pen (new pen% [width width] [color color]))
	(send path move-to 0 h)
	(send path line-to w h)
	(send path line-to (/ w 2) 0)
	(send path close)
	(send ctx draw-path path dx dy)
	(send ctx set-pen prev-pen))
	(dc draw-it w h))

(define (sierpinski side)
  (cond [(<= side 4) (triangle side 1 "red")]
        [else (define half (sierpinski (/ side 2)))
              (vc-append half (hc-append half half))]))

(provide (all-defined-out))
#lang eopl

(define the-store 'uninitialized)
(define size 100)
(define index 0)

;; empty-store : () → Sto
(define empty-store
  (lambda () (make-vector size)))

;; get-store : () → Sto
(define get-store
  (lambda () the-store))

;; initialize-store! : () → Unspecified
(define initialize-store!
  (lambda ()
    (set! the-store (make-vector size))))

;; reference? : SchemeVal → Bool
(define (reference? v)
  (integer? v))

(define (report-invalid-reference ref store)
  (eopl:error 'setref
              "illegal reference ~s in store ~s"
              ref
              store))

;; newref : Expval -> Ref
(define newref
  (lambda (val)
    (begin
      (vector-set! the-store index val)
      (set! index (+ index 1))
      (- index 1))))

;; deref : Ref -> ExpVal
(define (deref ref)
  (vector-ref the-store ref))

;; setref! : Ref * ExpVal -> Unspecified
(define (setref! ref val)
  (if (and (reference? ref)
           (< ref (vector-length the-store)))
      (vector-set! the-store ref val)
      (report-invalid-reference ref the-store)))

(initialize-store!)
(newref 10)
(deref 0)
(setref! 0 20)
(deref 0)
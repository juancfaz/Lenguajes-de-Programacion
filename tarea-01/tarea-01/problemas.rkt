#lang racket

;; Escribe aquí tus soluciones

;; 1.

(define (countdown n)
  (if (= n -1)
	null
	(cons n (countdown (- n 1)))))

(provide (all-defined-out))

#lang racket

(require rackunit
         rackunit/text-ui
         "arithlang.rkt")

(define-test-suite pruebas
  (test-case "interp"
    (check-equal? (interp (plusC (numC 1) (numC 2)))
                  (numV 3))))

(run-tests pruebas 'verbose)
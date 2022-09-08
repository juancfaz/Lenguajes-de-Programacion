#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  (test-case "bundle"
    (check-equal? (bundle (explode "abcdefg") 3) (list "abc" "def" "g"))
    (check-equal? (bundle '("a" "b") 3) (list "ab"))
    (check-equal? (bundle '() 3) '()))

  (test-case "list->chunks"
    (check-equal? (list->chunks '("1" "2" "3") 2) (list "12" "3"))
    (check-equal? (list->chunks '("1" "1") 3) (list "11"))
    (check-equal? (list->chunks '() 3) '()))

  (test-case "partition"
    (check-equal? (partition "12345" 2) (list "12" "34" "5"))
    (check-equal? (partition "12345" 5) (list "12345"))
    (check-equal? (partition " " 1) '("")))

  (test-case "bundle-partition"
    (check-equal? (partition "1234567" 5) (bundle (explode "1234567") 5)))

  (test-case "quicksort"
    (check-equal? (quicksort '() >) '())
    (check-equal? (quicksort '(1) >) '(1))
    (check-equal? (quicksort '(1) <) '(1))
    (check-equal? (quicksort '(1 1 1 1 1 1 1) <) '(1 1 1 1 1 1 1))
    (check-equal? (quicksort '(5 4 3 2 1) <) '(1 2 3 4 5)))
  )
(run-tests pruebas 'verbose)

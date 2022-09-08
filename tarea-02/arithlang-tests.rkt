#lang plait

(require "arithlang.rkt")

;; Numeros
(test (eval `-2) -2)
(test (eval `-1.5) -1.5)
(test (eval `3/2) 3/2)

;; Suma
(test (eval `{+ 1 1}) 2)
(test (eval `{+ 1 -1}) 0)
(test (eval `{+ 0 0}) 0)

;; Resta
(test (eval `{- -1 1}) -2)
(test (eval `{- 1 -1}) 2)
(test (eval `{- 4/3 1/3}) 1)

;; Multitplicacion
(test (eval `{* 0 1}) 0)
(test (eval `{* 1 -1}) -1)
(test (eval `{* 1/2 2}) 1)

;; Malformadas
(test/exn (eval `{+ 1 2 3}) "Expresion con mas de dos argumentos.")
(test/exn (eval `{3 + 1}) "Expresion malformada.")
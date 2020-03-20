#lang plai

;; longitud: list -> number
(define (longitud l)
  (if (empty? l)
      0
      (+ 1 (longitud (cdr l)))))

;; toma: list number -> list
(define (toma l n)
  (if (zero? n)
      '()
      (cons (car l) (toma (cdr l) (sub1 n)))))

;; quita: (listof any) number -> (listof any)
(define (quita l n)
  (if (zero? n)
      l
      (quita (cdr l) (sub1 n))))

;; contiene: any (listof any) -> boolean
(define (contiene e l)
  (if (empty? l)
      #f
      (or (equal? e (car l)) (contiene e (cdr l)))))

;; Caza de patrones:
;; match recive una expresion y luego se listan los patrones
;; (match <expresion>
;;   [<patron> <resultado>]
;;   ...
;;   [else <resultado>])

;; suma-digitos: number -> number
(define (suma-digitos n)
  (match n
    [0 0]
    [n (+ (remainder n 10) (suma-digitos (quotient n 10)))]))

(test (suma-digitos 10) 1)
(test (suma-digitos 123) 6)
(test (suma-digitos 5) 5)
(test (suma-digitos 0) 0)

;; suma-lista: (listof number) -> number
(define (suma-lista l)
  (match l
    ['() 0]
    [(cons x xs) (+ x (suma-lista xs))]))

;; mapear: function (listof any) -> (listof any)
(define (mapear f l)
  (match l
    ['() l]
    [(cons x xs) (cons (f x) (mapear f xs))]))

;; filtrar: pred (listof any) -> (listof any)
(define (filtrar p l)
  (match l
    ['() l]
    [(cons x xs) (if (p x)
                     (cons x (filtrar p xs))
                     (filter p xs))]))

(filtrar even? '(1 2 3 4))

;; foldr hace la operación (recorre la lista) hacia la derecha (r)
;; foldr se le pasa la operación que se quiere aplicar y el valor que se va a 
;; aplicar en el caso base y la lista
;; (foldr <funcion> <caso-base> <lista>)

;; my-foldr: function any (listof any) -> any
(define (my-foldr f v l)
  (match l
    ['() v]
    [(cons x xs) (f x (my-foldr f v xs))]))

(my-foldr + 0 '(1 2 3))

;; foldl hace la operecion (recorre la lista) hacia la izquierda
;; my-foldl: function any (listof any) -> any
(define (my-foldl f v l)
  (match l
    ['() v]
    [(cons x xs) (my-foldl f (f x v) xs)]))

;; Lambdas
;; Funciones anónimas
;; (lambda (<p1> <p2> ... <pn>) <expresion>)
(map (lambda (n) (+ n 13)) '(1 2 3))

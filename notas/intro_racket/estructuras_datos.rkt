#lang plai
;; Estructuras de datos - Racket

;; Pares
;; Se crean con (cons)
;; car obtiene el primer elemento del par
;; cdr obtiene el segundo elemento del par

;; suma: (pairof decimal) (pairof decimal) -> (pairof decimal)
(define (suma u v)
	(cons (+ (car u) (car v)) (+ (cdr u) (cdr v))))

(test (suma (cons 1 2) (cons 1 2)) '(2 . 4))

;; producto-punto: (pairof decimal) (pairof decimal) -> decimal
(define (producto-punto u v)
	(+ (* (car u) (car v)) (* (cdr u) (cdr v))))

;; producto-escalar: (pairof decimal) decimal -> (pairof decimal)
(define (producto-escalar u k)
	(cons (* k (car u)) (* k (cdr u))))

;; Listas
;; empty, null, '() <- son listas vacías
;; cons, list, '(quote) <- para construir listas

empty
null
'()

(cons 1 (cons 2 (cons 3 empty)))
'(1 2 3)
(list 1 2 3)

;; Diferencia entre list y quote

;; list evalua los valores
(list (+ 2 3) (+ 4 3))
;; quote no los evalua
'((+ 2 3) (+ 4 3))

;; empty? dice si la lista está vacía
;; length longitud de la lista
;; take toma los primeros n elementos de una lista
(take '(1 2 3) 2)

;; drop elimina los primeros n elementos de una lista
(drop '(1 2 3) 2)

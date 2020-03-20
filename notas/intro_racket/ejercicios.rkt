;; plai es el lenguaje que se va a utilizar en todo el curso
#lang plai
; Vamos a utilizar en medida de lo posible a racket como lenguaje puro
; es decir que vamos a mantener la referencia transparencial


; Tipos básicos

; Los números (number) en Racket son
; 	- Exactos (integer, rational, complex)
; 	- Inexactos (decima, complex)

; Las cadenas (string)
; "cadena"

; Booleanos (boolean)
; #t ::= True
; #f ::= False
; Nota: Se pueden usar true y false

; Símbolos (symbol)
; 'hola
; Ese símbolo es distinto a una cadena por que no los trabaja de caracter a caracter
; lo toma como una sola unidad

; Caracter (character)
; #\a
; #\b
; Los caracteres no se van a utilizar mucho en este curso


; Funciones sobre booleanos
; Nota: Todo lo que no es falso es verdadero
; not
; and
; or
;
; Nota: (and #t 1) = 1 (Por que devuelve el último valor que no es false)
; 
; and y or no son funciones, son macros

; Funciones aritméticas
; (+ 1 2)
; > 3
; (- 2/3 1/3)
; > 1/3

; Funciones de cadenas
; (string-length "hola")
; > 4
; (string-ref "Manzana" 3)
; > #\z
; (substring "Manzana" 1 3)
; > "an"

; Predicados. Son expresiones que se evaluan a #t o #f
; number?
; symbol?
; zero?
; empty?
; char?

; Conversores. Nos permiten transformar un tipo de dato a otro
; Por convención tienen de separador una flecha (->)
; (string->symbol "Hola")
; (number->string 42)

; Definir funciones.
; 1) Analisis del problema
; 2) Escribir una descripcion de el funcionamiento de la función como comentario
; 3) Escribir el contrato. También va en un comentario.
; 	<nombre>: <param1> <param2> ... -> <resultado>
; 4) Escribir pruebas unitarias. Se usa la primitiva (test <expresion> <expresion>)
; 5) Implementación

;; Función que calcula el promedio de 3 números dados.
;; promedio: number number number -> number 

(define (promedio a b c)
	(/ (+ a b c) 3))

;; Función que calcula la suma de los pesos correspondientes a monedas de 
;; 50 centavos, 1, 2, 5 y 10 pesos.
;; suma-monedas: number number number number number -> number
(define (suma-monedas a b c d e)
	(+ (* a 0.5) b (* c 2) (* d 5) (* e 10)))

;; Función que calcula el voluen de una esfera.
;; volumen-esfera: number -> number
(define (volumen-esfera r)
	(* 4/3 pi (expt r 3)))

;; Función que calcula el área de un círculo dado su diámetro
;; area-circulo: number -> number
(define (area-circulo d)
	(* pi (/ d 2) (/ d 2)))

;; Función que calcula el área de un círculo dado su diámetro pero usando let
;; area-circulo-let: number -> number
(define (area-circulo-let d)
	(let ([r (/ d 2)])
		(* pi (expt r 2))))

;; Función que calcula el valor absoluto de un número
;; absoluto: number -> number
(define (absoluto n)
	(if (> n 0)
		n
		(* n -1)))

;; Función que diga qué mes está representando un número dado
;; mes: number -> string
(define (mes n)
	(cond
		[(= n 1) "Enero"]
		[(= n 2) "Febrero"]
		[(= n 12) "Diciembre"]
		[else (error 'mes "No existe ese mes.")]))

; Con (text/exn (mes 25) "No existe ese mes") se casan mensajes de exepción.

(test (+ 1 1) 2)
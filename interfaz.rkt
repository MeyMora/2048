#lang racket
(require 2htdp/image)

;------------------------------------------------------------
; CONSTANTES GLOBALES
;------------------------------------------------------------

(define TAMANO-CELDA 100)
(define ESPACIO 10)
(define FILAS 4)
(define COLS 4)
(define ANCHO-TABLERO (+ (* COLS TAMANO-CELDA) (* (+ COLS 1) ESPACIO)))
(define ALTO-TABLERO  (+ (* FILAS TAMANO-CELDA) (* (+ FILAS 1) ESPACIO)))

;------------------------------------------------------------
; Funcion: hex->color
; Descripcion: Convierte un string de color hexadecimal
;              al formato de color que acepta 2htdp/image.
;              Extrae los componentes R, G y B del string
;              y construye un color con make-color.
;
; Parametros:
;   hex: string en formato "#rrggbb"
;
; Retorna: objeto color compatible con 2htdp/image
;
; Ejemplo: (hex->color "#f2b179")
; Resultado: (make-color 242 177 121)
;------------------------------------------------------------
(define (hex->color hex)
  (make-color
   (string->number (substring hex 1 3) 16)
   (string->number (substring hex 3 5) 16)
   (string->number (substring hex 5 7) 16)))

;------------------------------------------------------------
; Funcion: color-fondo-celda
; Descripcion: Devuelve el color de fondo correspondiente
;              al valor de una celda del tablero.
;              Cada valor del juego 2048 tiene un color unico
;              que permite al jugador identificar rapidamente
;              el estado del tablero.
;
; Parametros:
;   valor: numero entero que contiene la celda (0, 2, 4, 8...)
;
; Retorna: objeto color compatible con 2htdp/image
;
; Ejemplo: (color-fondo-celda 4)
; Resultado: (make-color 237 224 200)
;------------------------------------------------------------
(define (color-fondo-celda valor)
  (cond
    [(= valor 0)    (hex->color "#cdc1b4")]
    [(= valor 2)    (hex->color "#eee4da")]
    [(= valor 4)    (hex->color "#ede0c8")]
    [(= valor 8)    (hex->color "#f2b179")]
    [(= valor 16)   (hex->color "#f59563")]
    [(= valor 32)   (hex->color "#f67c5f")]
    [(= valor 64)   (hex->color "#f65e3b")]
    [(= valor 128)  (hex->color "#edcf72")]
    [(= valor 256)  (hex->color "#edcc61")]
    [(= valor 512)  (hex->color "#edc850")]
    [(= valor 1024) (hex->color "#edc53f")]
    [(= valor 2048) (hex->color "#edc22e")]
    [else           (hex->color "#3c3a32")]))

;------------------------------------------------------------
; Funcion: color-texto-celda
; Descripcion: Devuelve el color del texto segun el valor
;              de la celda. Los valores bajos (2 y 4) usan
;              texto oscuro, el resto usa texto claro para
;              mantener contraste y legibilidad.
;
; Parametros:
;   valor: numero entero que contiene la celda
;
; Retorna: objeto color compatible con 2htdp/image
;
; Ejemplo: (color-texto-celda 2)
; Resultado: (make-color 119 110 101)
;------------------------------------------------------------
(define (color-texto-celda valor)
  (cond
    [(= valor 2) (hex->color "#776e65")]
    [(= valor 4) (hex->color "#776e65")]
    [else        (hex->color "#f9f6f2")]))

;------------------------------------------------------------
; Funcion: tamano-fuente-celda
; Descripcion: Devuelve el tamaño de fuente adecuado segun
;              la cantidad de digitos del valor, para que el
;              numero siempre quepa dentro de la celda sin
;              desbordarse.
;
; Parametros:
;   valor: numero entero que contiene la celda
;
; Retorna: numero entero con el tamaño de fuente en pixeles
;
; Ejemplo: (tamano-fuente-celda 1024)
; Resultado: 28
;------------------------------------------------------------
(define (tamano-fuente-celda valor)
  (cond
    [(< valor 100)  44]
    [(< valor 1000) 36]
    [else           28]))

;------------------------------------------------------------
; Funcion: texto-celda
; Descripcion: Genera la imagen del texto que se muestra
;              dentro de una celda. Si el valor es 0, la
;              celda aparece vacia (sin texto visible).
;
; Parametros:
;   valor: numero entero que contiene la celda
;
; Retorna: imagen con el texto del valor, o imagen vacia
;          si el valor es 0
;
; Ejemplo: (texto-celda 8)
; Resultado: imagen con el texto "8" en color claro
;------------------------------------------------------------
(define (texto-celda valor)
  (cond
    [(= valor 0) (square 0 "solid" "white")]
    [else        (text (number->string valor)
                       (tamano-fuente-celda valor)
                       (color-texto-celda valor))]))

;------------------------------------------------------------
; Funcion: dibujar-celda
; Descripcion: Dibuja una celda completa del tablero 2048.
;              Combina el fondo de color con el numero
;              centrado encima, usando overlay para superponer
;              el texto sobre el rectangulo de fondo.
;
; Parametros:
;   valor: numero entero que contiene la celda (0, 2, 4, 8...)
;
; Retorna: imagen cuadrada de TAMANO-CELDA x TAMANO-CELDA
;          con el color y numero correspondientes al valor
;
; Ejemplo: (dibujar-celda 512)
; Resultado: imagen de celda amarilla con "512" en blanco
;------------------------------------------------------------
(define (dibujar-celda valor)
  (overlay
   (texto-celda valor)
   (square TAMANO-CELDA "solid" (color-fondo-celda valor))))

;------------------------------------------------------------
; Funcion: separador-celda
; Descripcion: Crea una imagen rectangular que sirve como
;              espacio visual entre celdas dentro de una fila.
;              Usa el mismo color de fondo del tablero para
;              integrarse con el diseno general.
;
; Parametros:
;   ninguno
;
; Retorna: imagen rectangular de ESPACIO x TAMANO-CELDA
;          con el color de fondo del tablero
;
; Ejemplo: (separador-celda)
; Resultado: imagen de 10x100 en color cafe oscuro
;------------------------------------------------------------
(define (separador-celda)
  (rectangle ESPACIO TAMANO-CELDA "solid" (hex->color "#bbada0")))

;------------------------------------------------------------
; Funcion: dibujar-fila
; Descripcion: Dibuja una fila completa del tablero uniendo
;              sus celdas horizontalmente con separadores
;              entre ellas. Recorre la lista de forma
;              recursiva sin usar map ni funciones de orden
;              superior.
;
; Parametros:
;   fila: lista de numeros enteros con los valores de
;         cada celda (ej: '(2 0 4 8))
;
; Retorna: imagen con todas las celdas de la fila unidas
;          de izquierda a derecha con separadores
;
; Ejemplo: (dibujar-fila '(2 0 512 4))
; Resultado: imagen horizontal con 4 celdas y separadores
;------------------------------------------------------------
(define (dibujar-fila fila)
  (cond
    [(null? fila)
     empty-image]
    [(null? (cdr fila))
     (dibujar-celda (car fila))]
    [else
     (beside
      (dibujar-celda (car fila))
      (separador-celda)
      (dibujar-fila (cdr fila)))]))

;------------------------------------------------------------
; Funcion: separador-fila
; Descripcion: Crea una imagen rectangular que sirve como
;              espacio visual entre filas del tablero.
;              Tiene el ancho total del tablero y la altura
;              del espacio definido entre celdas.
;
; Parametros:
;   ninguno
;
; Retorna: imagen rectangular de ANCHO-TABLERO x ESPACIO
;          con el color de fondo del tablero
;
; Ejemplo: (separador-fila)
; Resultado: imagen horizontal delgada color cafe
;------------------------------------------------------------
(define (separador-fila)
  (rectangle ANCHO-TABLERO ESPACIO "solid" (hex->color "#bbada0")))

;------------------------------------------------------------
; Funcion: dibujar-filas
; Descripcion: Apila todas las filas del tablero de forma
;              vertical con separadores entre ellas.
;              Recorre la lista de filas recursivamente
;              sin usar map ni funciones de orden superior.
;
; Parametros:
;   tablero: lista de listas de numeros (4x4)
;
; Retorna: imagen con todas las filas apiladas verticalmente
;
; Ejemplo: (dibujar-filas '((2 0 4 0)(0 8 0 4)(2 2 0 0)(0 0 4 4)))
; Resultado: imagen con 4 filas y separadores entre ellas
;------------------------------------------------------------
(define (dibujar-filas tablero)
  (cond
    [(null? tablero)
     empty-image]
    [(null? (cdr tablero))
     (dibujar-fila (car tablero))]
    [else
     (above
      (dibujar-fila (car tablero))
      (separador-fila)
      (dibujar-filas (cdr tablero)))]))

;------------------------------------------------------------
; Funcion: dibujar-tablero
; Descripcion: Dibuja el tablero completo del juego 2048
;              incluyendo el fondo cafe y un borde uniforme
;              alrededor de todas las celdas. Envuelve las
;              filas en un rectangulo de fondo del color
;              caracteristico del juego.
;
; Parametros:
;   tablero: lista de listas de numeros enteros (4x4)
;            que representa el estado actual del tablero
;
; Retorna: imagen completa del tablero con fondo y celdas
;
; Ejemplo: (dibujar-tablero (list (list 2 0 4 0)
;                                 (list 0 8 0 4)
;                                 (list 2 2 0 0)
;                                 (list 0 0 4 4)))
; Resultado: imagen del tablero 4x4 con fondo cafe
;------------------------------------------------------------
(define (dibujar-tablero tablero)
  (overlay
   (above
    (separador-fila)
    (dibujar-filas tablero)
    (separador-fila))
   (rectangle ANCHO-TABLERO ALTO-TABLERO "solid" (hex->color "#bbada0"))))


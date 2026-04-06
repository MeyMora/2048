#lang racket
(require 2htdp/image)
 
;------------------------------------------------------------
; CONSTANTES GLOBALES
;------------------------------------------------------------
 
(define TAMANO-CELDA 100)
 
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
 
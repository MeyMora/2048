#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "tablero_base.rkt")

;------------------------------------------------------------
; CONSTANTES GLOBALES
;------------------------------------------------------------

(define TAMANO-CELDA 100)
(define ESPACIO 10)
(define FILAS 4)
(define COLS 4)
(define ANCHO-TABLERO (+ (* COLS TAMANO-CELDA) (* (+ COLS 1) ESPACIO)))
(define ALTO-TABLERO  (+ (* FILAS TAMANO-CELDA) (* (+ FILAS 1) ESPACIO)))
(define ALTO-HEADER 90)

(define (hex->color hex)
  (make-color
   (string->number (substring hex 1 3) 16)
   (string->number (substring hex 3 5) 16)
   (string->number (substring hex 5 7) 16)))

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

(define (color-texto-celda valor)
  (cond
    [(= valor 2) (hex->color "#776e65")]
    [(= valor 4) (hex->color "#776e65")]
    [else        (hex->color "#f9f6f2")]))

(define (tamano-fuente-celda valor)
  (cond
    [(< valor 100)  44]
    [(< valor 1000) 36]
    [else           28]))

(define (texto-celda valor)
  (cond
    [(= valor 0) (square 0 "solid" "white")]
    [else        (text (number->string valor)
                       (tamano-fuente-celda valor)
                       (color-texto-celda valor))]))

(define (dibujar-celda valor)
  (overlay
   (texto-celda valor)
   (square TAMANO-CELDA "solid" (color-fondo-celda valor))))

(define (separador-celda)
  (rectangle ESPACIO TAMANO-CELDA "solid" (hex->color "#bbada0")))

(define (dibujar-fila fila)
  (cond
    [(null? fila)       empty-image]
    [(null? (cdr fila)) (dibujar-celda (car fila))]
    [else
     (beside
      (dibujar-celda (car fila))
      (separador-celda)
      (dibujar-fila (cdr fila)))]))

(define (separador-fila)
  (rectangle ANCHO-TABLERO ESPACIO "solid" (hex->color "#bbada0")))

(define (dibujar-filas tablero)
  (cond
    [(null? tablero)       empty-image]
    [(null? (cdr tablero)) (dibujar-fila (car tablero))]
    [else
     (above
      (dibujar-fila (car tablero))
      (separador-fila)
      (dibujar-filas (cdr tablero)))]))

(define (dibujar-tablero tablero)
  (overlay
   (above
    (separador-fila)
    (dibujar-filas tablero)
    (separador-fila))
   (rectangle ANCHO-TABLERO ALTO-TABLERO "solid" (hex->color "#bbada0"))))

(define (caja-puntaje puntaje)
  (overlay
   (above
    (text "SCORE" 14 (hex->color "#f0ebe3"))
    (rectangle 1 4 "solid" "transparent")
    (text (number->string puntaje) 22 "white"))
   (rectangle 100 58 "solid" (hex->color "#bbada0"))))

(define (dibujar-header puntaje)
  (overlay/align
   "left" "middle"
   (beside
    (rectangle 14 10 "solid" "transparent")
    (text "2048" 52 (hex->color "#776e65"))
    (rectangle (- ANCHO-TABLERO 14 52 100 14) 10 "solid" "transparent")
    (caja-puntaje puntaje)
    (rectangle 14 10 "solid" "transparent"))
   (rectangle ANCHO-TABLERO ALTO-HEADER "solid" (hex->color "#faf8ef"))))

(define (render estado)
  (above
   (dibujar-header (cadr estado))
   (rectangle ANCHO-TABLERO ESPACIO "solid" (hex->color "#faf8ef"))
   (dibujar-tablero (car estado))))

;------------------------------------------------------------
; ESTADO INICIAL
;------------------------------------------------------------

;------------------------------------------------------------
; Funcion: tablero-inicial
; Descripcion: Define el tablero de inicio del juego con
;              dos fichas colocadas en posiciones fijas.
;              El juego clasico 2048 siempre empieza con
;              dos fichas de valor 2 en el tablero.
;
; Parametros:
;   ninguno
;
; Retorna: lista de listas 4x4 con dos fichas de valor 2
;          y el resto en cero
;
; Ejemplo: (tablero-inicial)
; Resultado: ((2 0 0 0)(0 0 0 0)(0 0 0 0)(0 0 2 0))
;------------------------------------------------------------
(define (tablero-inicial)
  (reemplazar-celda
   (reemplazar-celda
    (crear-tablero 4 4) 0 0 2)
   3 2 2))

;------------------------------------------------------------
; Funcion: estado-inicial
; Descripcion: Construye el estado inicial completo del
;              juego, que es una lista con el tablero y
;              el puntaje en cero. Este estado es el punto
;              de partida que recibe big-bang.
;
; Parametros:
;   ninguno
;
; Retorna: lista de dos elementos (tablero puntaje)
;          donde tablero es 4x4 y puntaje es 0
;
; Ejemplo: (estado-inicial)
; Resultado: (((2 0 0 0)...(0 0 2 0)) 0)
;------------------------------------------------------------
(define (estado-inicial)
  (list (tablero-inicial) 0))

;------------------------------------------------------------
; BIG-BANG - punto de entrada del juego
;------------------------------------------------------------

(big-bang (estado-inicial)
  (to-draw render)
  (name "2048"))

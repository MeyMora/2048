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
(define ALTO-HEADER 90)

;------------------------------------------------------------
; Funcion: hex->color
; Descripcion: Convierte un string de color hexadecimal
;              al formato de color que acepta 2htdp/image.
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

;------------------------------------------------------------
; Funcion: caja-puntaje
; Descripcion: Construye la caja visual del puntaje que se
;              muestra en el header. Contiene la etiqueta
;              "SCORE" arriba y el valor numerico abajo,
;              sobre un fondo cafe redondeado.
;
; Parametros:
;   puntaje: numero entero con el puntaje actual del jugador
;
; Retorna: imagen de la caja de puntaje lista para mostrar
;
; Ejemplo: (caja-puntaje 1024)
; Resultado: imagen con "SCORE" y "1024" sobre fondo cafe
;------------------------------------------------------------
(define (caja-puntaje puntaje)
  (overlay
   (above
    (text "SCORE" 14 (hex->color "#f0ebe3"))
    (rectangle 1 4 "solid" "transparent")
    (text (number->string puntaje) 22 "white"))
   (rectangle 100 58 "solid" (hex->color "#bbada0"))))

;------------------------------------------------------------
; Funcion: dibujar-header
; Descripcion: Construye el encabezado del juego con el
;              titulo "2048" a la izquierda y la caja de
;              puntaje a la derecha, sobre el fondo crema
;              del juego.
;
; Parametros:
;   puntaje: numero entero con el puntaje actual del jugador
;
; Retorna: imagen del header con titulo y puntaje alineados
;
; Ejemplo: (dibujar-header 256)
; Resultado: imagen de encabezado con "2048" y "256"
;------------------------------------------------------------
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

;------------------------------------------------------------
; Funcion: render
; Descripcion: Funcion principal de renderizado. Combina
;              el header y el tablero en una sola imagen
;              final que representa el estado visual
;              completo del juego en cada momento.
;
; Parametros:
;   estado: lista de dos elementos:
;           - primer elemento: tablero (lista de listas 4x4)
;           - segundo elemento: puntaje (numero entero)
;
; Retorna: imagen completa del juego lista para mostrar
;
; Ejemplo: (render (list tablero 512))
; Resultado: imagen con header y tablero combinados
;------------------------------------------------------------
(define (render estado)
  (above
   (dibujar-header (cadr estado))
   (rectangle ANCHO-TABLERO ESPACIO "solid" (hex->color "#faf8ef"))
   (dibujar-tablero (car estado))))

;------------------------------------------------------------
; PRUEBAS
;------------------------------------------------------------

; (render (list (list (list 2 0 4 0)
;                     (list 0 8 0 4)
;                     (list 2 2 0 0)
;                     (list 0 0 4 4))
;               512))

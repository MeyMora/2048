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

(define (mi-list-ref lista indice)
  (cond
    [(= indice 0) (car lista)]
    [else         (mi-list-ref (cdr lista) (- indice 1))]))

(define (celdas-vacias-fila fila fila-i col-i)
  (cond
    [(null? fila) '()]
    [(= (car fila) 0)
     (cons (list fila-i col-i)
           (celdas-vacias-fila (cdr fila) fila-i (+ col-i 1)))]
    [else
     (celdas-vacias-fila (cdr fila) fila-i (+ col-i 1))]))

(define (celdas-vacias tablero fila-i)
  (cond
    [(null? tablero) '()]
    [else
     (append
      (celdas-vacias-fila (car tablero) fila-i 0)
      (celdas-vacias (cdr tablero) (+ fila-i 1)))]))

(define (valor-nueva-ficha)
  (cond
    [(< (random 10) 9) 2]
    [else              4]))

(define (colocar-ficha-aleatoria tablero)
  (cond
    [(null? (celdas-vacias tablero 0)) tablero]
    [else
     (reemplazar-celda
      tablero
      (car  (mi-list-ref (celdas-vacias tablero 0)
                         (random (mi-length (celdas-vacias tablero 0)))))
      (cadr (mi-list-ref (celdas-vacias tablero 0)
                         (random (mi-length (celdas-vacias tablero 0)))))
      (valor-nueva-ficha))]))

(define (tablero-inicial)
  (colocar-ficha-aleatoria
   (colocar-ficha-aleatoria
    (crear-tablero 4 4))))

(define (estado-inicial)
  (list (tablero-inicial) 0))

(define (tiene-2048-fila fila)
  (cond
    [(null? fila)        #f]
    [(= (car fila) 2048) #t]
    [else (tiene-2048-fila (cdr fila))]))

(define (tiene-2048 tablero)
  (cond
    [(null? tablero) #f]
    [(tiene-2048-fila (car tablero)) #t]
    [else (tiene-2048 (cdr tablero))]))

(define (juego-terminado estado)
  (cond
    [(tiene-2048 (car estado))              #t]
    [(null? (celdas-vacias (car estado) 0)) #t]
    [else                                   #f]))

(define (pantalla-fin estado)
  (overlay
   (above
    (text (cond
            [(tiene-2048 (car estado)) "¡Ganaste!"]
            [else                      "Game Over"])
          44
          (cond
            [(tiene-2048 (car estado)) (hex->color "#776e65")]
            [else                      (hex->color "#f9f6f2")]))
    (rectangle 1 12 "solid" "transparent")
    (text "Presiona R para reiniciar" 18 (hex->color "#776e65")))
   (rectangle ANCHO-TABLERO ALTO-TABLERO "solid"
              (make-color 237 224 200 190))
   (dibujar-tablero (car estado))))

(define (render estado)
  (above
   (dibujar-header (cadr estado))
   (rectangle ANCHO-TABLERO ESPACIO "solid" (hex->color "#faf8ef"))
   (cond
     [(juego-terminado estado) (pantalla-fin estado)]
     [else                     (dibujar-tablero (car estado))])))

;------------------------------------------------------------
; Funcion: aplicar-movimiento
; Descripcion: Aplica un movimiento al tablero y coloca una
;              nueva ficha solo si el tablero cambio. Usa
;              tableros-iguales? de tablero_base.rkt para
;              evitar generar fichas en movimientos invalidos
;              donde el tablero no cambia en absoluto.
;
; Parametros:
;   tablero:        estado actual del tablero
;   tablero-movido: resultado de aplicar el movimiento
;
; Retorna: tablero con nueva ficha si hubo cambio, o el
;          tablero-movido sin cambio si no hubo efecto
;
; Ejemplo: (aplicar-movimiento t (mover-tablero-izquierda t))
; Resultado: tablero con o sin ficha nueva segun cambio
;------------------------------------------------------------
(define (aplicar-movimiento tablero tablero-movido)
  (cond
    [(tableros-iguales? tablero tablero-movido) tablero-movido]
    [else (colocar-ficha-aleatoria tablero-movido)]))

;------------------------------------------------------------
; Funcion: manejar-tecla
; Descripcion: Maneja todas las teclas del juego. Ahora usa
;              aplicar-movimiento para que no se genere ficha
;              nueva cuando el movimiento no hace nada.
;              R reinicia, las 4 flechas mueven el tablero.
;              Si el juego termino solo responde a R.
;
; Parametros:
;   estado: lista (tablero puntaje)
;   tecla:  string con la tecla presionada
;
; Retorna: nuevo estado segun la accion
;
; Ejemplo: (manejar-tecla estado "left")
; Resultado: estado con tablero movido a la izquierda
;------------------------------------------------------------
(define (manejar-tecla estado tecla)
  (cond
    [(string=? tecla "r")
     (estado-inicial)]
    [(juego-terminado estado)
     estado]
    [(string=? tecla "left")
     (list (aplicar-movimiento (car estado)
                               (mover-tablero-izquierda (car estado)))
           (cadr estado))]
    [(string=? tecla "right")
     (list (aplicar-movimiento (car estado)
                               (mover-tablero-derecha (car estado)))
           (cadr estado))]
    [(string=? tecla "up")
     (list (aplicar-movimiento (car estado)
                               (mover-tablero-arriba (car estado)))
           (cadr estado))]
    [(string=? tecla "down")
     (list (aplicar-movimiento (car estado)
                               (mover-tablero-abajo (car estado)))
           (cadr estado))]
    [else estado]))

;------------------------------------------------------------
; BIG-BANG
;------------------------------------------------------------

(big-bang (estado-inicial)
  (to-draw render)
  (on-key  manejar-tecla)
  (name    "2048"))

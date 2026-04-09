#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "tablero_base.rkt")

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
  (overlay (texto-celda valor)
           (square TAMANO-CELDA "solid" (color-fondo-celda valor))))

(define (separador-celda)
  (rectangle ESPACIO TAMANO-CELDA "solid" (hex->color "#bbada0")))

(define (dibujar-fila fila)
  (cond
    [(null? fila)       empty-image]
    [(null? (cdr fila)) (dibujar-celda (car fila))]
    [else
     (beside (dibujar-celda (car fila))
             (separador-celda)
             (dibujar-fila (cdr fila)))]))

(define (separador-fila)
  (rectangle ANCHO-TABLERO ESPACIO "solid" (hex->color "#bbada0")))

(define (dibujar-filas tablero)
  (cond
    [(null? tablero)       empty-image]
    [(null? (cdr tablero)) (dibujar-fila (car tablero))]
    [else
     (above (dibujar-fila (car tablero))
            (separador-fila)
            (dibujar-filas (cdr tablero)))]))

(define (dibujar-tablero tablero)
  (overlay
   (above (separador-fila) (dibujar-filas tablero) (separador-fila))
   (rectangle ANCHO-TABLERO ALTO-TABLERO "solid" (hex->color "#bbada0"))))

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
    [else (celdas-vacias-fila (cdr fila) fila-i (+ col-i 1))]))

(define (celdas-vacias tablero fila-i)
  (cond
    [(null? tablero) '()]
    [else
     (append (celdas-vacias-fila (car tablero) fila-i 0)
             (celdas-vacias (cdr tablero) (+ fila-i 1)))]))

(define (valor-nueva-ficha)
  (cond [(< (random 10) 9) 2] [else 4]))

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
   (colocar-ficha-aleatoria (crear-tablero 4 4))))

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
    [(tiene-2048 (car estado)) #t]
    [(perdio? (car estado))    #t]
    [else                      #f]))

(define (calcular-puntos-fila fa fd)
  (cond
    [(or (null? fa) (null? fd)) 0]
    [(and (not (= (car fa) 0))
          (= (car fd) (* 2 (car fa))))
     (+ (car fd) (calcular-puntos-fila (cdr fa) (cdr fd)))]
    [else (calcular-puntos-fila (cdr fa) (cdr fd))]))

(define (calcular-puntos ta td)
  (cond
    [(or (null? ta) (null? td)) 0]
    [else (+ (calcular-puntos-fila (car ta) (car td))
             (calcular-puntos (cdr ta) (cdr td)))]))

;------------------------------------------------------------
; ESTADO: (tablero puntaje mejor-puntaje)
; El tercer elemento guarda el record de la sesion.
;------------------------------------------------------------

(define (estado-inicial)
  (list (tablero-inicial) 0 0))

(define (estado-tablero e) (car   e))
(define (estado-puntaje e) (cadr  e))
(define (estado-mejor   e) (caddr e))

;------------------------------------------------------------
; Funcion: caja-score
; Descripcion: Crea una caja visual de puntaje con etiqueta
;              y valor numerico sobre fondo cafe. Se usa
;              para SCORE y BEST en el header.
;
; Parametros:
;   etiqueta: string con el nombre de la caja
;   valor:    numero entero a mostrar
;
; Retorna: imagen de la caja con etiqueta y numero
;
; Ejemplo: (caja-score "BEST" 4096)
; Resultado: caja cafe con "BEST" y "4096"
;------------------------------------------------------------
(define (caja-score etiqueta valor)
  (overlay
   (above (text etiqueta 13 (hex->color "#f0ebe3"))
          (rectangle 1 4 "solid" "transparent")
          (text (number->string valor) 20 "white"))
   (rectangle 94 58 "solid" (hex->color "#bbada0"))))

;------------------------------------------------------------
; Funcion: dibujar-header
; Descripcion: Header con titulo y dos cajas de puntaje:
;              SCORE con el puntaje actual y BEST con el
;              mejor puntaje de la sesion.
;
; Parametros:
;   puntaje: numero entero del puntaje actual
;   mejor:   numero entero del mejor puntaje de la sesion
;
; Retorna: imagen del header completo
;
; Ejemplo: (dibujar-header 512 2048)
; Resultado: header con SCORE 512 y BEST 2048
;------------------------------------------------------------
(define (dibujar-header puntaje mejor)
  (overlay/align
   "left" "middle"
   (beside
    (rectangle 14 10 "solid" "transparent")
    (text "2048" 52 (hex->color "#776e65"))
    (rectangle (- ANCHO-TABLERO 14 52 94 8 94 14) 10 "solid" "transparent")
    (caja-score "SCORE" puntaje)
    (rectangle 8 10 "solid" "transparent")
    (caja-score "BEST" mejor)
    (rectangle 14 10 "solid" "transparent"))
   (rectangle ANCHO-TABLERO ALTO-HEADER "solid" (hex->color "#faf8ef"))))

(define (pantalla-fin estado)
  (overlay
   (above
    (text (cond [(tiene-2048 (estado-tablero estado)) "¡Ganaste!"]
                [else "Game Over"])
          44
          (cond [(tiene-2048 (estado-tablero estado)) (hex->color "#776e65")]
                [else (hex->color "#f9f6f2")]))
    (rectangle 1 12 "solid" "transparent")
    (text "Presiona R para reiniciar" 18 (hex->color "#776e65")))
   (rectangle ANCHO-TABLERO ALTO-TABLERO "solid" (make-color 237 224 200 190))
   (dibujar-tablero (estado-tablero estado))))

(define (render estado)
  (above
   (dibujar-header (estado-puntaje estado) (estado-mejor estado))
   (rectangle ANCHO-TABLERO ESPACIO "solid" (hex->color "#faf8ef"))
   (cond
     [(juego-terminado estado) (pantalla-fin estado)]
     [else (dibujar-tablero (estado-tablero estado))])))

;------------------------------------------------------------
; Funcion: actualizar-mejor
; Descripcion: Compara puntaje actual con el mejor y devuelve
;              el mayor. Se usa al final de cada movimiento
;              para mantener el record actualizado.
;
; Parametros:
;   puntaje: puntaje del movimiento actual
;   mejor:   mejor puntaje registrado hasta ahora
;
; Retorna: el mayor de los dos valores
;
; Ejemplo: (actualizar-mejor 1024 512)
; Resultado: 1024
;------------------------------------------------------------
(define (actualizar-mejor puntaje mejor)
  (cond [(> puntaje mejor) puntaje]
        [else              mejor]))

;------------------------------------------------------------
; Funcion: puntos-del-movimiento
; Descripcion: Calcula cuantos puntos gano el jugador en
;              un movimiento comparando el tablero antes y
;              despues. Se extrae como funcion separada para
;              poder reutilizarla sin duplicar codigo.
;
; Parametros:
;   estado:         estado actual con tablero y puntaje
;   tablero-movido: tablero resultado del movimiento
;
; Retorna: numero entero con los puntos del movimiento
;
; Ejemplo: (puntos-del-movimiento estado t-movido)
; Resultado: suma de combinaciones realizadas
;------------------------------------------------------------
(define (puntos-del-movimiento estado tablero-movido)
  (+ (estado-puntaje estado)
     (calcular-puntos (estado-tablero estado) tablero-movido)))

;------------------------------------------------------------
; Funcion: aplicar-con-mejor
; Descripcion: Aplica un movimiento, calcula puntos, actualiza
;              el mejor puntaje y coloca nueva ficha. Si el
;              tablero no cambio devuelve el estado sin tocar.
;
; Parametros:
;   estado:          estado actual (tablero puntaje mejor)
;   tablero-movido:  tablero resultado del movimiento
;
; Retorna: estado nuevo con tablero, puntaje y mejor
;          actualizados
;
; Ejemplo: (aplicar-con-mejor estado t-movido)
; Resultado: estado con todos los campos actualizados
;------------------------------------------------------------
(define (aplicar-con-mejor estado tablero-movido)
  (cond
    [(tableros-iguales? (estado-tablero estado) tablero-movido)
     estado]
    [else
     (cond
       [#t
        (cond
          [#t
           (list (colocar-ficha-aleatoria tablero-movido)
                 (puntos-del-movimiento estado tablero-movido)
                 (actualizar-mejor
                  (puntos-del-movimiento estado tablero-movido)
                  (estado-mejor estado)))])])]))

; version sin cond anidados innecesarios:
(define (aplicar-con-mejor-v2 estado tablero-movido)
  (cond
    [(tableros-iguales? (estado-tablero estado) tablero-movido)
     estado]
    [else
     (list (colocar-ficha-aleatoria tablero-movido)
           (puntos-del-movimiento estado tablero-movido)
           (actualizar-mejor (puntos-del-movimiento estado tablero-movido)
                             (estado-mejor estado)))]))

(define (manejar-tecla estado tecla)
  (cond
    [(string=? tecla "r")
     (list (tablero-inicial) 0 (estado-mejor estado))]
    [(juego-terminado estado) estado]
    [(string=? tecla "left")
     (aplicar-con-mejor-v2 estado (mover-tablero-izquierda (estado-tablero estado)))]
    [(string=? tecla "right")
     (aplicar-con-mejor-v2 estado (mover-tablero-derecha (estado-tablero estado)))]
    [(string=? tecla "up")
     (aplicar-con-mejor-v2 estado (mover-tablero-arriba (estado-tablero estado)))]
    [(string=? tecla "down")
     (aplicar-con-mejor-v2 estado (mover-tablero-abajo (estado-tablero estado)))]
    [else estado]))

(big-bang (estado-inicial)
  (to-draw render)
  (on-key  manejar-tecla)
  (name    "2048"))

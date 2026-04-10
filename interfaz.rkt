#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "tablero_base.rkt")

;============================================================
; CONSTANTES
;============================================================

(define TAMANO-CELDA 100)
(define ESPACIO      10)
(define FILAS        4)
(define COLS         4)
(define ANCHO-TABLERO   (+ (* COLS  TAMANO-CELDA) (* (+ COLS  1) ESPACIO)))
(define ALTO-TABLERO    (+ (* FILAS TAMANO-CELDA) (* (+ FILAS 1) ESPACIO)))
(define ALTO-HEADER     90)
(define ALTO-TOTAL      (+ ALTO-HEADER ESPACIO ALTO-TABLERO))

; Panel de fichas comprimido para caber en ALTO-TOTAL
; ALTO-TOTAL = ALTO-HEADER + ESPACIO + ALTO-TABLERO
; En modo colocar: header + panel + tablero-reducido
; El tablero se escala para que todo quepa en ALTO-TOTAL.
; Tamano del panel: 56px
(define ALTO-PANEL      56)
; Alto disponible para el tablero en modo colocar
(define ALTO-TAB-COLOCAR (- ALTO-TOTAL ALTO-HEADER ESPACIO ALTO-PANEL ESPACIO))
; Factor de escala del tablero en modo colocar
(define ESCALA-COLOCAR  (/ ALTO-TAB-COLOCAR ALTO-TABLERO))

(define FICHAS-DISPONIBLES '(2 4 8 16 32 64 128 256 512 1024 2048))

;============================================================
; COLORES Y DIBUJO (igual que paso 14)
;============================================================

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
  (cond [(< valor 100) 44] [(< valor 1000) 36] [else 28]))

(define (texto-celda valor)
  (cond
    [(= valor 0) (square 0 "solid" "white")]
    [else (text (number->string valor) (tamano-fuente-celda valor) (color-texto-celda valor))]))

(define (dibujar-celda valor)
  (overlay (texto-celda valor)
           (square TAMANO-CELDA "solid" (color-fondo-celda valor))))

(define (separador-celda)
  (rectangle ESPACIO TAMANO-CELDA "solid" (hex->color "#bbada0")))

(define (dibujar-fila fila)
  (cond
    [(null? fila)       empty-image]
    [(null? (cdr fila)) (dibujar-celda (car fila))]
    [else (beside (dibujar-celda (car fila)) (separador-celda) (dibujar-fila (cdr fila)))]))

(define (separador-fila)
  (rectangle ANCHO-TABLERO ESPACIO "solid" (hex->color "#bbada0")))

(define (dibujar-filas tablero)
  (cond
    [(null? tablero)       empty-image]
    [(null? (cdr tablero)) (dibujar-fila (car tablero))]
    [else (above (dibujar-fila (car tablero)) (separador-fila) (dibujar-filas (cdr tablero)))]))

(define (dibujar-tablero tablero)
  (overlay
   (above (separador-fila) (dibujar-filas tablero) (separador-fila))
   (rectangle ANCHO-TABLERO ALTO-TABLERO "solid" (hex->color "#bbada0"))))

(define (caja-score etiqueta valor)
  (overlay
   (above (text etiqueta 13 (hex->color "#f0ebe3"))
          (rectangle 1 4 "solid" "transparent")
          (text (number->string valor) 20 "white"))
   (rectangle 94 58 "solid" (hex->color "#bbada0"))))

(define (dibujar-header puntaje mejor)
  (define fondo  (rectangle ANCHO-TABLERO ALTO-HEADER "solid" (hex->color "#faf8ef")))
  (define titulo (text "2048" 52 (hex->color "#776e65")))
  (define score  (caja-score "SCORE" puntaje))
  (define best   (caja-score "BEST"  mejor))
  (place-image titulo (+ 14 (/ (image-width titulo) 2)) (/ ALTO-HEADER 2)
               (place-image score (- ANCHO-TABLERO 14 94 8 47) (/ ALTO-HEADER 2)
                            (place-image best (- ANCHO-TABLERO 14 47) (/ ALTO-HEADER 2)
                                         fondo))))

(define (pantalla-fin tablero)
  (overlay
   (above
    (text (cond [(gano? tablero) "¡Ganaste!"] [else "Game Over"]) 44
          (cond [(gano? tablero) (hex->color "#776e65")] [else (hex->color "#f9f6f2")]))
    (rectangle 1 12 "solid" "transparent")
    (text "Esc para volver al menu" 18 (hex->color "#776e65")))
   (rectangle ANCHO-TABLERO ALTO-TABLERO "solid" (make-color 237 224 200 190))
   (dibujar-tablero tablero)))

(define (dibujar-boton texto resaltado)
  (overlay
   (text texto 22
         (cond [resaltado "white"] [else (hex->color "#776e65")]))
   (rectangle 260 60 "solid"
              (cond [resaltado (hex->color "#f65e3b")] [else (hex->color "#bbada0")]))))

(define (render-menu opcion)
  (overlay
   (above
    (rectangle 1 40 "solid" "transparent")
    (text "2048" 82 (hex->color "#776e65"))
    (rectangle 1 10 "solid" "transparent")
    (text "Combina fichas para llegar al 2048" 16 (hex->color "#bbada0"))
    (rectangle 1 50 "solid" "transparent")
    (dibujar-boton "Jugar" (= opcion 0))
    (rectangle 1 16 "solid" "transparent")
    (dibujar-boton "Modo Colocar" (= opcion 1))
    (rectangle 1 50 "solid" "transparent")
    (text "↑ ↓ navegar   Enter seleccionar" 13 (hex->color "#bbada0")))
   (rectangle ANCHO-TABLERO ALTO-TOTAL "solid" (hex->color "#faf8ef"))))

;============================================================
; PANEL DE FICHAS — cabe en ALTO-PANEL (56px)
;============================================================

;------------------------------------------------------------
; Funcion: tamano-mini-ficha
; Descripcion: Fuente pequena para fichas del panel.
;
; Parametros:
;   valor: numero de la ficha
;
; Retorna: numero entero con tamaño en pixeles
;
; Ejemplo: (tamano-mini-ficha 1024)
; Resultado: 10
;------------------------------------------------------------
(define (tamano-mini-ficha valor)
  (cond
    [(< valor 100)  15]
    [(< valor 1000) 12]
    [else           10]))

;------------------------------------------------------------
; Funcion: dibujar-mini-ficha
; Descripcion: Ficha pequena de 40x40px para el panel.
;              Si coincide con la seleccionada tiene borde
;              blanco de 4px para destacarla.
;
; Parametros:
;   valor: numero de la ficha
;   sel:   numero de la ficha seleccionada (0=ninguna)
;
; Retorna: imagen de 40x40 (o 48x48 si seleccionada)
;
; Ejemplo: (dibujar-mini-ficha 64 64)
; Resultado: ficha 64 con borde blanco
;------------------------------------------------------------
(define (dibujar-mini-ficha valor sel)
  (cond
    [(= valor sel)
     (overlay
      (overlay (text (number->string valor) (tamano-mini-ficha valor) (color-texto-celda valor))
               (square 40 "solid" (color-fondo-celda valor)))
      (square 46 "solid" "white"))]
    [else
     (overlay (text (number->string valor) (tamano-mini-ficha valor) (color-texto-celda valor))
              (square 40 "solid" (color-fondo-celda valor)))]))

;------------------------------------------------------------
; Funcion: dibujar-fichas-panel
; Descripcion: Fila de todas las fichas disponibles con la
;              seleccionada resaltada. Recursiva, sin map.
;
; Parametros:
;   fichas: lista de valores disponibles
;   sel:    valor de la ficha seleccionada
;
; Retorna: imagen horizontal con todas las fichas
;
; Ejemplo: (dibujar-fichas-panel FICHAS-DISPONIBLES 32)
; Resultado: fila de fichas con 32 resaltada
;------------------------------------------------------------
(define (dibujar-fichas-panel fichas sel)
  (cond
    [(null? fichas) empty-image]
    [(null? (cdr fichas)) (dibujar-mini-ficha (car fichas) sel)]
    [else
     (beside (dibujar-mini-ficha (car fichas) sel)
             (rectangle 3 10 "solid" "transparent")
             (dibujar-fichas-panel (cdr fichas) sel))]))

;------------------------------------------------------------
; Funcion: dibujar-panel-fichas
; Descripcion: Panel compacto de ALTO-PANEL px con instruccion
;              arriba y fichas disponibles abajo.
;              Todo cabe dentro del mismo ALTO-TOTAL del juego
;              normal porque el tablero se escala con scale.
;
; Parametros:
;   sel: valor seleccionado (0 = ninguna aun)
;
; Retorna: imagen del panel de ANCHO-TABLERO x ALTO-PANEL
;
; Ejemplo: (dibujar-panel-fichas 128)
; Resultado: panel con ficha 128 resaltada
;------------------------------------------------------------
(define (dibujar-panel-fichas sel)
  (overlay
   (above
    (text (cond
            [(= sel 0) "← → ficha   Enter confirmar   J jugar   Esc menu"]
            [else (string-append "Ficha: " (number->string sel)
                                 "   Enter colocar   Esc volver")])
          11 (hex->color "#776e65"))
    (rectangle 1 4 "solid" "transparent")
    (dibujar-fichas-panel FICHAS-DISPONIBLES sel))
   (rectangle ANCHO-TABLERO ALTO-PANEL "solid" (hex->color "#faf8ef"))))

;============================================================
; ESTADO ANIDADO
;============================================================

(define (sub-pantalla e) (car   e))
(define (sub-juego    e) (cadr  e))
(define (sub-colocar  e) (caddr e))
(define (pantalla  e) (car  (sub-pantalla e)))
(define (opcion    e) (cadr (sub-pantalla e)))
(define (tablero   e) (car   (sub-juego e)))
(define (puntaje   e) (cadr  (sub-juego e)))
(define (mejor     e) (caddr (sub-juego e)))
(define (ficha-sel e) (car  (sub-colocar e)))
(define (cursor    e) (cadr (sub-colocar e)))
(define (sp p o)   (list p o))
(define (sj t p m) (list t p m))
(define (sc f c)   (list f c))
(define (hacer-estado sp sj sc) (list sp sj sc))

(define (estado-inicial-menu)
  (hacer-estado
   (sp "menu" 0)
   (sj (agregar-ficha-aleatoria
        (agregar-ficha-aleatoria (crear-tablero 4 4))) 0 0)
   (sc 0 0)))

(define (juego-terminado? e)
  (cond [(gano? (tablero e)) #t] [(perdio? (tablero e)) #t] [else #f]))

(define (actualizar-mejor p m)
  (cond [(> p m) p] [else m]))

(define (mover-en-juego e direccion)
  (define resultado (jugar (tablero e) direccion (puntaje e)))
  (cond
    [(tableros-iguales? (tablero e) (car resultado)) e]
    [else
     (hacer-estado
      (sub-pantalla e)
      (sj (car resultado) (cadr resultado)
          (actualizar-mejor (cadr resultado) (mejor e)))
      (sub-colocar e))]))

;============================================================
; RENDER CON PANEL Y TABLERO ESCALADO
;============================================================

;------------------------------------------------------------
; Funcion: render-colocar
; Descripcion: Pantalla de modo colocar dentro del mismo
;              canvas ALTO-TOTAL del juego normal.
;              El tablero se escala con ESCALA-COLOCAR para
;              que todo quepa: header + panel + tablero.
;              Asi la ventana nunca cambia de tamano.
;
; Parametros:
;   e: estado completo en modo colocar o elegir-celda
;
; Retorna: imagen de ANCHO-TABLERO x ALTO-TOTAL
;
; Ejemplo: (render-colocar estado)
; Resultado: header + panel + tablero escalado
;------------------------------------------------------------


(define (render-juego e)
  (define canvas (rectangle ANCHO-TABLERO ALTO-TOTAL "solid" (hex->color "#faf8ef")))
  (define tablero-img
    (cond [(juego-terminado? e) (pantalla-fin (tablero e))]
          [else                 (dibujar-tablero (tablero e))]))
  (place-image (dibujar-header (puntaje e) (mejor e))
               (/ ANCHO-TABLERO 2) (/ ALTO-HEADER 2)
               (place-image tablero-img
                            (/ ANCHO-TABLERO 2)
                            (+ ALTO-HEADER ESPACIO (/ ALTO-TABLERO 2))
                            canvas)))

;------------------------------------------------------------
; Funcion: render
; Descripcion: Despachador de renderizado. Todas las pantallas
;              producen imagenes del mismo tamano ALTO-TOTAL,
;              por lo que la ventana nunca cambia de tamano.
;
; Parametros:
;   e: estado completo
;
; Retorna: imagen de ANCHO-TABLERO x ALTO-TOTAL
;
; Ejemplo: (render estado)
; Resultado: imagen del mismo tamano siempre
;------------------------------------------------------------

;============================================================
; TECLADO
;============================================================


;============================================================
; PASO 17 — CURSOR SOBRE TABLERO ESCALADO
;============================================================

;------------------------------------------------------------
; Funcion: dibujar-celda-cursor
; Descripcion: Celda con o sin resaltado de cursor. Cuando
;              es-cursor es #t superpone un borde blanco.
;
; Parametros:
;   valor:     numero de la celda
;   es-cursor: #t si esta celda tiene el cursor
;
; Retorna: imagen de la celda con o sin cursor
;
; Ejemplo: (dibujar-celda-cursor 0 #t)
; Resultado: celda vacia con borde blanco
;------------------------------------------------------------
(define (dibujar-celda-cursor valor es-cursor)
  (cond
    [es-cursor
     (overlay (square TAMANO-CELDA "outline" "white")
              (dibujar-celda valor))]
    [else (dibujar-celda valor)]))

;------------------------------------------------------------
; Funcion: dibujar-fila-cursor
; Descripcion: Fila del tablero con cursor activo. Resalta
;              la celda cuya posicion lineal es cursor-pos.
;
; Parametros:
;   fila:       lista de numeros
;   fila-i:     indice de la fila actual
;   cursor-pos: posicion lineal del cursor (0..15)
;   col-i:      indice de columna actual en la recursion
;
; Retorna: imagen de la fila con cursor
;
; Ejemplo: (dibujar-fila-cursor '(2 0 4 0) 1 6 0)
; Resultado: fila con cursor en posicion 6
;------------------------------------------------------------
(define (dibujar-fila-cursor fila fila-i cursor-pos col-i)
  (cond
    [(null? fila) empty-image]
    [(null? (cdr fila))
     (dibujar-celda-cursor (car fila) (= cursor-pos (+ (* fila-i COLS) col-i)))]
    [else
     (beside
      (dibujar-celda-cursor (car fila) (= cursor-pos (+ (* fila-i COLS) col-i)))
      (separador-celda)
      (dibujar-fila-cursor (cdr fila) fila-i cursor-pos (+ col-i 1)))]))

;------------------------------------------------------------
; Funcion: dibujar-filas-cursor
; Descripcion: Apila todas las filas con cursor activo.
;
; Parametros:
;   tablero:    lista de listas 4x4
;   cursor-pos: posicion lineal del cursor (0..15)
;   fila-i:     indice de fila actual
;
; Retorna: imagen con filas apiladas y cursor
;
; Ejemplo: (dibujar-filas-cursor tablero 5 0)
; Resultado: tablero con cursor en fila 1 col 1
;------------------------------------------------------------
(define (dibujar-filas-cursor tablero cursor-pos fila-i)
  (cond
    [(null? tablero) empty-image]
    [(null? (cdr tablero))
     (dibujar-fila-cursor (car tablero) fila-i cursor-pos 0)]
    [else
     (above
      (dibujar-fila-cursor (car tablero) fila-i cursor-pos 0)
      (separador-fila)
      (dibujar-filas-cursor (cdr tablero) cursor-pos (+ fila-i 1)))]))

;------------------------------------------------------------
; Funcion: dibujar-tablero-cursor
; Descripcion: Tablero completo con cursor visible. El cursor
;              se dibuja a escala normal y luego la imagen
;              completa se escala con ESCALA-COLOCAR para
;              caber en el espacio disponible.
;
; Parametros:
;   tablero:    lista de listas 4x4
;   cursor-pos: posicion lineal del cursor (0..15)
;
; Retorna: imagen del tablero con cursor escalada
;
; Ejemplo: (dibujar-tablero-cursor tablero 10)
; Resultado: tablero escalado con celda 10 resaltada
;------------------------------------------------------------
(define (dibujar-tablero-cursor tablero cursor-pos)
  (scale ESCALA-COLOCAR
         (overlay
          (above (separador-fila)
                 (dibujar-filas-cursor tablero cursor-pos 0)
                 (separador-fila))
          (rectangle ANCHO-TABLERO ALTO-TABLERO "solid" (hex->color "#bbada0")))))

;------------------------------------------------------------
; Funciones de movimiento del cursor (posiciones 0..15)
;------------------------------------------------------------

;------------------------------------------------------------
; Funcion: cursor-arriba
; Descripcion: Cursor una fila arriba, salta a la ultima
;              si esta en la primera fila.
;
; Parametros:
;   pos: posicion lineal actual (0..15)
;
; Retorna: nueva posicion
;
; Ejemplo: (cursor-arriba 2)
; Resultado: 14
;------------------------------------------------------------
(define (cursor-arriba pos)
  (cond [(< pos COLS) (+ pos (* COLS (- FILAS 1)))]
        [else         (- pos COLS)]))

;------------------------------------------------------------
; Funcion: cursor-abajo
; Descripcion: Cursor una fila abajo, salta a la primera
;              si esta en la ultima fila.
;
; Parametros:
;   pos: posicion lineal actual (0..15)
;
; Retorna: nueva posicion
;
; Ejemplo: (cursor-abajo 14)
; Resultado: 2
;------------------------------------------------------------
(define (cursor-abajo pos)
  (remainder (+ pos COLS) (* FILAS COLS)))

;------------------------------------------------------------
; Funcion: cursor-izquierda
; Descripcion: Cursor una columna a la izquierda. Circular.
;
; Parametros:
;   pos: posicion lineal actual (0..15)
;
; Retorna: nueva posicion
;
; Ejemplo: (cursor-izquierda 0)
; Resultado: 15
;------------------------------------------------------------
(define (cursor-izquierda pos)
  (cond [(= pos 0) (- (* FILAS COLS) 1)] [else (- pos 1)]))

;------------------------------------------------------------
; Funcion: cursor-derecha
; Descripcion: Cursor una columna a la derecha. Circular.
;
; Parametros:
;   pos: posicion lineal actual (0..15)
;
; Retorna: nueva posicion
;
; Ejemplo: (cursor-derecha 15)
; Resultado: 0
;------------------------------------------------------------
(define (cursor-derecha pos)
  (remainder (+ pos 1) (* FILAS COLS)))

;------------------------------------------------------------
; Funcion: render-colocar
; Descripcion: Version actualizada que muestra tablero con
;              cursor cuando pantalla es "elegir-celda".
;
; Parametros:
;   e: estado completo en modo colocar o elegir-celda
;
; Retorna: imagen de ANCHO-TABLERO x ALTO-TOTAL
;
; Ejemplo: (render-colocar estado)
; Resultado: header + panel + tablero (con cursor si aplica)
;------------------------------------------------------------
(define (render-colocar e)
  (define canvas (rectangle ANCHO-TABLERO ALTO-TOTAL "solid" (hex->color "#faf8ef")))
  (define tablero-img
    (cond
      [(string=? (pantalla e) "elegir-celda")
       (dibujar-tablero-cursor (tablero e) (cursor e))]
      [else
       (scale ESCALA-COLOCAR (dibujar-tablero (tablero e)))]))
  (define panel-img (dibujar-panel-fichas (ficha-sel e)))
  (place-image (dibujar-header (puntaje e) (mejor e))
               (/ ANCHO-TABLERO 2) (/ ALTO-HEADER 2)
               (place-image panel-img
                            (/ ANCHO-TABLERO 2)
                            (+ ALTO-HEADER ESPACIO (/ ALTO-PANEL 2))
                            (place-image tablero-img
                                         (/ ANCHO-TABLERO 2)
                                         (+ ALTO-HEADER ESPACIO ALTO-PANEL ESPACIO
                                            (/ ALTO-TAB-COLOCAR 2))
                                         canvas))))

;------------------------------------------------------------
; Funcion: manejar-tecla-elegir-celda
; Descripcion: Las 4 flechas mueven el cursor. Enter coloca
;              la ficha en la celda del cursor y vuelve a
;              modo colocar para seguir poniendo fichas.
;              Escape vuelve a seleccion de ficha.
;              J inicia la partida con el tablero actual.
;
; Parametros:
;   e:     estado en pantalla elegir-celda
;   tecla: string de la tecla
;
; Retorna: nuevo estado
;
; Ejemplo: (manejar-tecla-elegir-celda estado "return")
; Resultado: ficha colocada, vuelve a modo colocar
;------------------------------------------------------------
(define (manejar-tecla-elegir-celda e tecla)
  (cond
    [(string=? tecla "escape")
     (hacer-estado (sp "colocar" 0) (sub-juego e) (sc (ficha-sel e) 0))]
    [(string=? tecla "j")
     (hacer-estado (sp "juego" 0) (sub-juego e) (sc 0 0))]
    [(or (string=? tecla "return") (string=? tecla "\r"))
     (hacer-estado
      (sp "colocar" 0)
      (sj (reemplazar-celda (tablero e)
                            (quotient  (cursor e) COLS)
                            (remainder (cursor e) COLS)
                            (ficha-sel e))
          (puntaje e) (mejor e))
      (sc 0 0))]
    [(string=? tecla "up")
     (hacer-estado (sp "elegir-celda" 0) (sub-juego e)
                   (sc (ficha-sel e) (cursor-arriba (cursor e))))]
    [(string=? tecla "down")
     (hacer-estado (sp "elegir-celda" 0) (sub-juego e)
                   (sc (ficha-sel e) (cursor-abajo (cursor e))))]
    [(string=? tecla "left")
     (hacer-estado (sp "elegir-celda" 0) (sub-juego e)
                   (sc (ficha-sel e) (cursor-izquierda (cursor e))))]
    [(string=? tecla "right")
     (hacer-estado (sp "elegir-celda" 0) (sub-juego e)
                   (sc (ficha-sel e) (cursor-derecha (cursor e))))]
    [else e]))

;============================================================
; FUNCIONES AUXILIARES PARA NAVEGACION DE FICHAS
;============================================================

;------------------------------------------------------------
; Funcion: mi-length-local
; Descripcion: Calcula la longitud de una lista recursivamente.
;
; Parametros:
;   lst: lista
;
; Retorna: numero entero con la cantidad de elementos
;
; Ejemplo: (mi-length-local '(2 4 8))
; Resultado: 3
;------------------------------------------------------------
(define (mi-length-local lst)
  (cond [(null? lst) 0]
        [else (+ 1 (mi-length-local (cdr lst)))]))

;------------------------------------------------------------
; Funcion: mi-list-ref
; Descripcion: Retorna el elemento en la posicion i de la lista.
;
; Parametros:
;   lst: lista
;   i:   indice (0-based)
;
; Retorna: elemento en la posicion i
;
; Ejemplo: (mi-list-ref '(2 4 8 16) 2)
; Resultado: 8
;------------------------------------------------------------
(define (mi-list-ref lst i)
  (cond [(= i 0) (car lst)]
        [else (mi-list-ref (cdr lst) (- i 1))]))

;------------------------------------------------------------
; Funcion: siguiente-ficha
; Descripcion: Retorna la ficha siguiente a val en la lista.
;              Si val es el ultimo, retorna el primero (circular).
;
; Parametros:
;   fichas: lista de fichas disponibles
;   val:    valor actual seleccionado
;
; Retorna: siguiente valor en la lista
;
; Ejemplo: (siguiente-ficha '(2 4 8 16) 8)
; Resultado: 16
;------------------------------------------------------------
(define (siguiente-ficha fichas val)
  (cond
    [(null? fichas) (car FICHAS-DISPONIBLES)]
    [(null? (cdr fichas)) (car FICHAS-DISPONIBLES)]
    [(= (car fichas) val) (cadr fichas)]
    [else (siguiente-ficha (cdr fichas) val)]))

;------------------------------------------------------------
; Funcion: ficha-anterior
; Descripcion: Retorna la ficha anterior a val en la lista.
;              Si val es el primero, retorna el ultimo (circular).
;
; Parametros:
;   fichas: lista de fichas disponibles
;   val:    valor actual seleccionado
;   prev:   valor previo acumulado en la recursion (iniciar en 0)
;
; Retorna: valor anterior en la lista
;
; Ejemplo: (ficha-anterior '(2 4 8 16) 8 0)
; Resultado: 4
;------------------------------------------------------------
(define (ficha-anterior fichas val prev)
  (cond
    [(null? fichas) prev]
    [(= (car fichas) val)
     (cond [(= prev 0) (mi-list-ref FICHAS-DISPONIBLES
                                    (- (mi-length-local FICHAS-DISPONIBLES) 1))]
           [else prev])]
    [else (ficha-anterior (cdr fichas) val (car fichas))]))

(define (manejar-tecla-colocar e tecla)
  (cond
    [(string=? tecla "escape")
     (hacer-estado (sp "menu" 0) (sub-juego e) (sc 0 0))]
    [(string=? tecla "j")
     (hacer-estado (sp "juego" 0) (sub-juego e) (sc 0 0))]
    [(or (string=? tecla "return") (string=? tecla "\r"))
     (cond
       [(= (ficha-sel e) 0)
        (hacer-estado (sp "colocar" 0) (sub-juego e) (sc (car FICHAS-DISPONIBLES) 0))]
       [else
        (hacer-estado (sp "elegir-celda" 0) (sub-juego e) (sc (ficha-sel e) 0))])]
    [(string=? tecla "right")
     (cond
       [(= (ficha-sel e) 0)
        (hacer-estado (sp "colocar" 0) (sub-juego e) (sc (car FICHAS-DISPONIBLES) 0))]
       [else
        (hacer-estado (sp "colocar" 0) (sub-juego e)
                      (sc (siguiente-ficha FICHAS-DISPONIBLES (ficha-sel e)) 0))])]
    [(string=? tecla "left")
     (cond
       [(= (ficha-sel e) 0)
        (hacer-estado (sp "colocar" 0) (sub-juego e)
                      (sc (mi-list-ref FICHAS-DISPONIBLES
                                       (- (mi-length-local FICHAS-DISPONIBLES) 1)) 0))]
       [else
        (hacer-estado (sp "colocar" 0) (sub-juego e)
                      (sc (ficha-anterior FICHAS-DISPONIBLES (ficha-sel e) 0) 0))])]
    [else e]))

(define (manejar-tecla-menu e tecla)
  (cond
    [(string=? tecla "up")
     (hacer-estado (sp "menu" 0) (sub-juego e) (sub-colocar e))]
    [(string=? tecla "down")
     (hacer-estado (sp "menu" 1) (sub-juego e) (sub-colocar e))]
    [(or (string=? tecla "return") (string=? tecla "\r"))
     (cond
       [(= (opcion e) 0)
        (hacer-estado (sp "juego" 0)
                      (sj (agregar-ficha-aleatoria
                           (agregar-ficha-aleatoria (crear-tablero 4 4))) 0 (mejor e))
                      (sc 0 0))]
       [else
        (hacer-estado (sp "colocar" 0)
                      (sj (crear-tablero 4 4) 0 (mejor e))
                      (sc 0 0))])]
    [else e]))

(define (manejar-tecla-juego e tecla)
  (cond
    [(string=? tecla "escape")
     (hacer-estado (sp "menu" 0) (sub-juego e) (sub-colocar e))]
    [(juego-terminado? e) e]
    [(string=? tecla "left")  (mover-en-juego e 'izquierda)]
    [(string=? tecla "right") (mover-en-juego e 'derecha)]
    [(string=? tecla "up")    (mover-en-juego e 'arriba)]
    [(string=? tecla "down")  (mover-en-juego e 'abajo)]
    [else e]))

;------------------------------------------------------------
; Funcion: render
; Descripcion: Todas las pantallas producen imagenes del
;              mismo tamano ALTO-TOTAL. La ventana nunca
;              cambia de tamano al cambiar de pantalla.
;
; Parametros:
;   e: estado completo
;
; Retorna: imagen de ANCHO-TABLERO x ALTO-TOTAL siempre
;
; Ejemplo: (render estado)
; Resultado: imagen del mismo tamano en cualquier pantalla
;------------------------------------------------------------
(define (render e)
  (cond
    [(string=? (pantalla e) "menu")         (render-menu (opcion e))]
    [(or (string=? (pantalla e) "colocar")
         (string=? (pantalla e) "elegir-celda")) (render-colocar e)]
    [else                                        (render-juego e)]))

(define (manejar-tecla e tecla)
  (cond
    [(string=? (pantalla e) "menu")         (manejar-tecla-menu         e tecla)]
    [(string=? (pantalla e) "juego")        (manejar-tecla-juego        e tecla)]
    [(string=? (pantalla e) "colocar")      (manejar-tecla-colocar      e tecla)]
    [(string=? (pantalla e) "elegir-celda") (manejar-tecla-elegir-celda e tecla)]
    [else e]))

;============================================================
; BIG-BANG
;============================================================

(big-bang (estado-inicial-menu)
  (to-draw render)
  (on-key  manejar-tecla)
  (name    "2048"))

#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "tablero_base.rkt")

;============================================================
; CONSTANTES FIJAS DE LAYOUT
;============================================================

(define TAMANO-CELDA  80)   ; px por celda
(define ESPACIO        8)   ; separador entre celdas
(define ALTO-HEADER   90)   ; altura del header score/best
(define ALTO-PANEL    56)   ; altura del panel de fichas (modo colocar)
(define VENTANA-W    550)   ; ancho fijo de la ventana
(define VENTANA-H    700)   ; alto fijo de la ventana
(define MIN-FILAS      4)
(define MAX-FILAS     10)
(define MIN-COLS       4)
(define MAX-COLS      10)

(define FICHAS-DISPONIBLES '(2 4 8 16 32 64 128 256 512 1024 2048))

;============================================================
; COLORES
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

;============================================================
; DIMENSIONES DINAMICAS SEGUN FILAS Y COLS DEL ESTADO
;============================================================

;------------------------------------------------------------
; Funcion: ancho-tablero
; Descripcion: Calcula el ancho total del tablero en pixeles
;              segun el numero de columnas.
;
; Parametros:
;   cols: numero de columnas
;
; Retorna: ancho en pixeles
;
; Ejemplo: (ancho-tablero 6)
; Resultado: 528
;------------------------------------------------------------
(define (ancho-tablero cols)
  (+ (* cols TAMANO-CELDA) (* (+ cols 1) ESPACIO)))

;------------------------------------------------------------
; Funcion: alto-tablero
; Descripcion: Calcula el alto total del tablero en pixeles
;              segun el numero de filas.
;
; Parametros:
;   filas: numero de filas
;
; Retorna: alto en pixeles
;
; Ejemplo: (alto-tablero 4)
; Resultado: 352
;------------------------------------------------------------
(define (alto-tablero filas)
  (+ (* filas TAMANO-CELDA) (* (+ filas 1) ESPACIO)))

;------------------------------------------------------------
; Funcion: escala-tablero
; Descripcion: Factor de escala para que el tablero MxN quepa
;              en la ventana fija en modo juego normal.
;              Toma el minimo entre escala horizontal y vertical
;              para que nunca se corte en ninguna dimension.
;
; Parametros:
;   filas: numero de filas
;   cols:  numero de columnas
;
; Retorna: factor de escala (numero entre 0.0 y 1.0)
;
; Ejemplo: (escala-tablero 10 10)
; Resultado: un numero menor a 1 para que quepa en VENTANA-W x VENTANA-H
;------------------------------------------------------------
(define (escala-tablero filas cols)
  (define espacio-h (- VENTANA-H ALTO-HEADER ESPACIO))
  (define espacio-w VENTANA-W)
  (define at (alto-tablero filas))
  (define aw (ancho-tablero cols))
  (define esc-v (/ espacio-h at))
  (define esc-h (/ espacio-w aw))
  (min esc-v esc-h 1.0))

;------------------------------------------------------------
; Funcion: escala-colocar
; Descripcion: Factor de escala en modo colocar, donde ademas
;              del header hay un panel de fichas encima del tablero.
;              Se calcula para que todo quepa sin cortarse.
;
; Parametros:
;   filas: numero de filas
;   cols:  numero de columnas
;
; Retorna: factor de escala
;
; Ejemplo: (escala-colocar 8 8)
; Resultado: numero menor a 1
;------------------------------------------------------------
(define (escala-colocar filas cols)
  (define espacio-h (- VENTANA-H ALTO-HEADER ESPACIO ALTO-PANEL ESPACIO))
  (define espacio-w VENTANA-W)
  (define at (alto-tablero filas))
  (define aw (ancho-tablero cols))
  (define esc-v (/ espacio-h at))
  (define esc-h (/ espacio-w aw))
  (min esc-v esc-h 1.0))

;============================================================
; DIBUJO DE CELDAS Y TABLERO
;============================================================

(define (tamano-fuente-celda valor)
  (cond [(< valor 100) 32] [(< valor 1000) 24] [else 18]))

(define (texto-celda valor)
  (cond
    [(= valor 0) (square 0 "solid" "white")]
    [else (text (number->string valor)
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
    [else (beside (dibujar-celda (car fila))
                  (separador-celda)
                  (dibujar-fila (cdr fila)))]))

(define (separador-fila cols)
  (rectangle (ancho-tablero cols) ESPACIO "solid" (hex->color "#bbada0")))

(define (dibujar-filas tablero cols)
  (cond
    [(null? tablero)       empty-image]
    [(null? (cdr tablero)) (dibujar-fila (car tablero))]
    [else (above (dibujar-fila (car tablero))
                 (separador-fila cols)
                 (dibujar-filas (cdr tablero) cols))]))

(define (dibujar-tablero tablero filas cols)
  (overlay
   (above (separador-fila cols)
          (dibujar-filas tablero cols)
          (separador-fila cols))
   (rectangle (ancho-tablero cols) (alto-tablero filas)
              "solid" (hex->color "#bbada0"))))

;============================================================
; HEADER, PANTALLA FIN
;============================================================

(define (caja-score etiqueta valor)
  (overlay
   (above (text etiqueta 13 (hex->color "#f0ebe3"))
          (rectangle 1 4 "solid" "transparent")
          (text (number->string valor) 20 "white"))
   (rectangle 94 58 "solid" (hex->color "#bbada0"))))

(define (dibujar-header puntaje mejor)
  (define fondo  (rectangle VENTANA-W ALTO-HEADER "solid" (hex->color "#faf8ef")))
  (define titulo (text "2048" 52 (hex->color "#776e65")))
  (define score  (caja-score "SCORE" puntaje))
  (define best   (caja-score "BEST"  mejor))
  (place-image titulo (+ 14 (/ (image-width titulo) 2)) (/ ALTO-HEADER 2)
               (place-image score (- VENTANA-W 14 94 8 47) (/ ALTO-HEADER 2)
                            (place-image best (- VENTANA-W 14 47) (/ ALTO-HEADER 2)
                                         fondo))))

(define (pantalla-fin tablero filas cols)
  (overlay
   (above
    (text (cond [(gano? tablero) "iGanaste!"] [else "Game Over"]) 44
          (cond [(gano? tablero) (hex->color "#776e65")] [else (hex->color "#f9f6f2")]))
    (rectangle 1 12 "solid" "transparent")
    (text "Esc para volver al menu" 18 (hex->color "#776e65")))
   (rectangle (ancho-tablero cols) (alto-tablero filas)
              "solid" (make-color 237 224 200 190))
   (dibujar-tablero tablero filas cols)))

;============================================================
; PANEL DE FICHAS (modo colocar)
;============================================================

(define (tamano-mini-ficha valor)
  (cond
    [(< valor 100)  13]
    [(< valor 1000) 10]
    [else            8]))

(define (dibujar-mini-ficha valor sel)
  (cond
    [(= valor sel)
     (overlay
      (overlay (text (number->string valor) (tamano-mini-ficha valor) (color-texto-celda valor))
               (square 36 "solid" (color-fondo-celda valor)))
      (square 42 "solid" "white"))]
    [else
     (overlay (text (number->string valor) (tamano-mini-ficha valor) (color-texto-celda valor))
              (square 36 "solid" (color-fondo-celda valor)))]))

(define (dibujar-fichas-panel fichas sel)
  (cond
    [(null? fichas) empty-image]
    [(null? (cdr fichas)) (dibujar-mini-ficha (car fichas) sel)]
    [else
     (beside (dibujar-mini-ficha (car fichas) sel)
             (rectangle 3 10 "solid" "transparent")
             (dibujar-fichas-panel (cdr fichas) sel))]))

(define (dibujar-panel-fichas sel)
  (overlay
   (above
    (text (cond
            [(= sel 0) "<- -> ficha   Enter confirmar   J jugar   Esc menu"]
            [else (string-append "Ficha: " (number->string sel)
                                 "   Enter colocar   Esc volver")])
          10 (hex->color "#776e65"))
    (rectangle 1 4 "solid" "transparent")
    (dibujar-fichas-panel FICHAS-DISPONIBLES sel))
   (rectangle VENTANA-W ALTO-PANEL "solid" (hex->color "#faf8ef"))))

;============================================================
; ESTADO ANIDADO
; Estado = (list sub-pantalla sub-juego sub-colocar sub-config)
; sub-pantalla = (list nombre opcion)
; sub-juego    = (list tablero puntaje mejor)
; sub-colocar  = (list ficha-sel cursor)
; sub-config   = (list filas cols)
;============================================================

(define (sub-pantalla e) (car    e))
(define (sub-juego    e) (cadr   e))
(define (sub-colocar  e) (caddr  e))
(define (sub-config   e) (cadddr e))

(define (pantalla  e) (car  (sub-pantalla e)))
(define (opcion    e) (cadr (sub-pantalla e)))
(define (tablero   e) (car   (sub-juego e)))
(define (puntaje   e) (cadr  (sub-juego e)))
(define (mejor     e) (caddr (sub-juego e)))
(define (ficha-sel e) (car  (sub-colocar e)))
(define (cursor    e) (cadr (sub-colocar e)))
(define (cfg-filas e) (car  (sub-config e)))
(define (cfg-cols  e) (cadr (sub-config e)))

(define (sp p o)     (list p o))
(define (sj t p m)   (list t p m))
(define (sc f c)     (list f c))
(define (scfg fi co) (list fi co))
(define (hacer-estado sp sj sc scfg) (list sp sj sc scfg))

(define (estado-inicial-menu)
  (hacer-estado
   (sp "menu" 0)
   (sj (agregar-ficha-aleatoria
        (agregar-ficha-aleatoria (crear-tablero 4 4))) 0 0)
   (sc 0 0)
   (scfg 4 4)))

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
      (sub-colocar e)
      (sub-config e))]))

;============================================================
; FUNCIONES AUXILIARES DE NAVEGACION DE FICHAS
;============================================================

;------------------------------------------------------------
; Funcion: mi-length-local
; Descripcion: Calcula la longitud de una lista recursivamente.
;
; Parametros:
;   lst: lista
;
; Retorna: numero de elementos
;
; Ejemplo: (mi-length-local '(2 4 8))
; Resultado: 3
;------------------------------------------------------------
(define (mi-length-local lst)
  (cond [(null? lst) 0]
        [else (+ 1 (mi-length-local (cdr lst)))]))

;------------------------------------------------------------
; Funcion: mi-list-ref
; Descripcion: Retorna el elemento en la posicion i (0-based).
;
; Parametros:
;   lst: lista
;   i:   indice
;
; Retorna: elemento en posicion i
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
;              Si es la ultima vuelve a la primera (circular).
;
; Parametros:
;   fichas: lista de fichas disponibles
;   val:    valor actual seleccionado
;
; Retorna: siguiente valor
;
; Ejemplo: (siguiente-ficha FICHAS-DISPONIBLES 64)
; Resultado: 128
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
;              Si es la primera vuelve a la ultima (circular).
;
; Parametros:
;   fichas: lista de fichas disponibles
;   val:    valor actual seleccionado
;   prev:   acumulador del valor previo (iniciar en 0)
;
; Retorna: valor anterior
;
; Ejemplo: (ficha-anterior FICHAS-DISPONIBLES 64 0)
; Resultado: 32
;------------------------------------------------------------
(define (ficha-anterior fichas val prev)
  (cond
    [(null? fichas) prev]
    [(= (car fichas) val)
     (cond [(= prev 0) (mi-list-ref FICHAS-DISPONIBLES
                                    (- (mi-length-local FICHAS-DISPONIBLES) 1))]
           [else prev])]
    [else (ficha-anterior (cdr fichas) val (car fichas))]))

;============================================================
; CURSOR SOBRE TABLERO
;============================================================

;------------------------------------------------------------
; Funcion: dibujar-celda-cursor
; Descripcion: Celda con o sin borde blanco de cursor.
;
; Parametros:
;   valor:     numero de la celda
;   es-cursor: #t si esta celda tiene el cursor
;
; Retorna: imagen de la celda
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
; Descripcion: Dibuja una fila resaltando la celda del cursor.
;              Usa fila-i y col-i para calcular posicion lineal.
;
; Parametros:
;   fila:       lista de numeros de la fila
;   fila-i:     indice de la fila actual
;   cursor-pos: posicion lineal del cursor
;   col-i:      indice de columna actual
;   cols:       numero total de columnas del tablero
;
; Retorna: imagen de la fila con cursor
;
; Ejemplo: (dibujar-fila-cursor '(2 0 4 0) 1 6 0 4)
; Resultado: fila con cursor en posicion 6
;------------------------------------------------------------
(define (dibujar-fila-cursor fila fila-i cursor-pos col-i cols)
  (cond
    [(null? fila) empty-image]
    [(null? (cdr fila))
     (dibujar-celda-cursor (car fila) (= cursor-pos (+ (* fila-i cols) col-i)))]
    [else
     (beside
      (dibujar-celda-cursor (car fila) (= cursor-pos (+ (* fila-i cols) col-i)))
      (separador-celda)
      (dibujar-fila-cursor (cdr fila) fila-i cursor-pos (+ col-i 1) cols))]))

;------------------------------------------------------------
; Funcion: dibujar-filas-cursor
; Descripcion: Apila todas las filas del tablero con cursor activo.
;
; Parametros:
;   tablero:    lista de listas MxN
;   cursor-pos: posicion lineal del cursor
;   fila-i:     indice de fila actual
;   cols:       numero total de columnas
;
; Retorna: imagen del tablero con cursor
;
; Ejemplo: (dibujar-filas-cursor tablero 5 0 4)
; Resultado: tablero con cursor en fila 1 col 1
;------------------------------------------------------------
(define (dibujar-filas-cursor tablero cursor-pos fila-i cols)
  (cond
    [(null? tablero) empty-image]
    [(null? (cdr tablero))
     (dibujar-fila-cursor (car tablero) fila-i cursor-pos 0 cols)]
    [else
     (above
      (dibujar-fila-cursor (car tablero) fila-i cursor-pos 0 cols)
      (separador-fila cols)
      (dibujar-filas-cursor (cdr tablero) cursor-pos (+ fila-i 1) cols))]))

;------------------------------------------------------------
; Funcion: dibujar-tablero-cursor
; Descripcion: Tablero completo con cursor, escalado para caber
;              en el espacio disponible segun el modo actual.
;
; Parametros:
;   tablero:    lista de listas MxN
;   cursor-pos: posicion lineal del cursor
;   filas:      numero de filas
;   cols:       numero de columnas
;   modo:       "colocar" usa escala-colocar, otro usa escala-tablero
;
; Retorna: imagen escalada del tablero con cursor
;
; Ejemplo: (dibujar-tablero-cursor tablero 10 6 6 "colocar")
; Resultado: tablero 6x6 escalado con celda 10 resaltada
;------------------------------------------------------------
(define (dibujar-tablero-cursor tablero cursor-pos filas cols modo)
  (define esc (cond [(string=? modo "colocar") (escala-colocar filas cols)]
                    [else (escala-tablero filas cols)]))
  (scale esc
         (overlay
          (above (separador-fila cols)
                 (dibujar-filas-cursor tablero cursor-pos 0 cols)
                 (separador-fila cols))
          (rectangle (ancho-tablero cols) (alto-tablero filas)
                     "solid" (hex->color "#bbada0")))))

;------------------------------------------------------------
; Funciones de movimiento del cursor — circular — usan filas y cols dinamicos
;------------------------------------------------------------

;------------------------------------------------------------
; Funcion: cursor-arriba
; Descripcion: Sube una fila. Si esta en la primera fila
;              salta a la ultima (circular).
;
; Parametros:
;   pos:   posicion lineal actual
;   filas: numero de filas del tablero
;   cols:  numero de columnas del tablero
;
; Retorna: nueva posicion lineal
;
; Ejemplo: (cursor-arriba 2 4 4)
; Resultado: 14
;------------------------------------------------------------
(define (cursor-arriba pos filas cols)
  (cond [(< pos cols) (+ pos (* cols (- filas 1)))]
        [else         (- pos cols)]))

;------------------------------------------------------------
; Funcion: cursor-abajo
; Descripcion: Baja una fila. Si esta en la ultima
;              salta a la primera (circular).
;
; Parametros:
;   pos:   posicion lineal actual
;   filas: numero de filas del tablero
;   cols:  numero de columnas del tablero
;
; Retorna: nueva posicion lineal
;
; Ejemplo: (cursor-abajo 14 4 4)
; Resultado: 2
;------------------------------------------------------------
(define (cursor-abajo pos filas cols)
  (remainder (+ pos cols) (* filas cols)))

;------------------------------------------------------------
; Funcion: cursor-izquierda
; Descripcion: Mueve una columna a la izquierda. Circular.
;
; Parametros:
;   pos:   posicion lineal actual
;   filas: numero de filas
;   cols:  numero de columnas
;
; Retorna: nueva posicion lineal
;
; Ejemplo: (cursor-izquierda 0 4 4)
; Resultado: 15
;------------------------------------------------------------
(define (cursor-izquierda pos filas cols)
  (cond [(= pos 0) (- (* filas cols) 1)] [else (- pos 1)]))

;------------------------------------------------------------
; Funcion: cursor-derecha
; Descripcion: Mueve una columna a la derecha. Circular.
;
; Parametros:
;   pos:   posicion lineal actual
;   filas: numero de filas
;   cols:  numero de columnas
;
; Retorna: nueva posicion lineal
;
; Ejemplo: (cursor-derecha 15 4 4)
; Resultado: 0
;------------------------------------------------------------
(define (cursor-derecha pos filas cols)
  (remainder (+ pos 1) (* filas cols)))

;============================================================
; RENDER
;============================================================

;------------------------------------------------------------
; Funcion: render-menu
; Descripcion: Pantalla de menu con 4 opciones navegables:
;              Jugar (0), Modo Colocar (1), Filas (2), Columnas (3).
;              Con flechas izq/der se ajusta el valor de Filas o Cols
;              cuando esa opcion esta seleccionada.
;              El tablero se crea con el tamano elegido al confirmar.
;
; Parametros:
;   e: estado completo
;
; Retorna: imagen de VENTANA-W x VENTANA-H
;------------------------------------------------------------
(define (render-menu e)
  (define fi (cfg-filas e))
  (define co (cfg-cols  e))
  (define op (opcion e))
  (define (resalt? n) (= op n))
  (define (btn txt n)
    (overlay
     (text txt 20 (cond [(resalt? n) "white"] [else (hex->color "#776e65")]))
     (rectangle 280 52 "solid"
                (cond [(resalt? n) (hex->color "#f65e3b")] [else (hex->color "#bbada0")]))))
  (define (selector etiqueta valor seleccionado?)
    (define color-txt (cond [seleccionado? "white"] [else (hex->color "#776e65")]))
    (define color-bg  (cond [seleccionado? (hex->color "#8f7a66")] [else (hex->color "#bbada0")]))
    (overlay
     (beside
      (text "<  " 16 color-txt)
      (text (string-append etiqueta ": " (number->string valor)) 18 color-txt)
      (text "  >" 16 color-txt))
     (rectangle 280 52 "solid" color-bg)))
  (overlay
   (above
    (rectangle 1 30 "solid" "transparent")
    (text "2048" 72 (hex->color "#776e65"))
    (rectangle 1 6 "solid" "transparent")
    (text "Combina fichas para llegar al 2048" 14 (hex->color "#bbada0"))
    (rectangle 1 28 "solid" "transparent")
    (btn "Jugar" 0)
    (rectangle 1 10 "solid" "transparent")
    (btn "Modo Colocar" 1)
    (rectangle 1 10 "solid" "transparent")
    (selector "Filas" fi (resalt? 2))
    (rectangle 1 10 "solid" "transparent")
    (selector "Columnas" co (resalt? 3))
    (rectangle 1 28 "solid" "transparent")
    (text "Arriba/Abajo: navegar   Enter: seleccionar" 12 (hex->color "#bbada0"))
    (rectangle 1 4 "solid" "transparent")
    (text "Cuando Filas/Cols esta marcado: <- -> cambia valor" 12 (hex->color "#bbada0"))
    (rectangle 1 4 "solid" "transparent")
    (text (string-append "Tablero actual: "
                         (number->string fi) " x " (number->string co)
                         "   (min 4x4 / max 10x10)")
          11 (hex->color "#f65e3b")))
   (rectangle VENTANA-W VENTANA-H "solid" (hex->color "#faf8ef"))))

;------------------------------------------------------------
; Funcion: render-juego
; Descripcion: Pantalla de juego. El tablero MxN se escala
;              dinamicamente para que nunca se corte en la
;              ventana fija VENTANA-W x VENTANA-H.
;
; Parametros:
;   e: estado completo
;
; Retorna: imagen de VENTANA-W x VENTANA-H
;------------------------------------------------------------
(define (render-juego e)
  (define fi  (cfg-filas e))
  (define co  (cfg-cols  e))
  (define esc (escala-tablero fi co))
  (define canvas (rectangle VENTANA-W VENTANA-H "solid" (hex->color "#faf8ef")))
  (define tablero-img
    (cond [(juego-terminado? e)
           (scale esc (pantalla-fin (tablero e) fi co))]
          [else
           (scale esc (dibujar-tablero (tablero e) fi co))]))
  (define alto-tab-escalado (* esc (alto-tablero fi)))
  (place-image (dibujar-header (puntaje e) (mejor e))
               (/ VENTANA-W 2) (/ ALTO-HEADER 2)
               (place-image tablero-img
                            (/ VENTANA-W 2)
                            (+ ALTO-HEADER ESPACIO (/ alto-tab-escalado 2))
                            canvas)))

;------------------------------------------------------------
; Funcion: render-colocar
; Descripcion: Pantalla de modo colocar. Muestra header, panel
;              de fichas y tablero escalado (con o sin cursor).
;              El tablero nunca se corta gracias a escala-colocar
;              que considera el espacio que ocupa el panel.
;
; Parametros:
;   e: estado completo en modo "colocar" o "elegir-celda"
;
; Retorna: imagen de VENTANA-W x VENTANA-H
;------------------------------------------------------------
(define (render-colocar e)
  (define fi  (cfg-filas e))
  (define co  (cfg-cols  e))
  (define esc (escala-colocar fi co))
  (define canvas (rectangle VENTANA-W VENTANA-H "solid" (hex->color "#faf8ef")))
  (define tablero-img
    (cond
      [(string=? (pantalla e) "elegir-celda")
       (dibujar-tablero-cursor (tablero e) (cursor e) fi co "colocar")]
      [else
       (scale esc (dibujar-tablero (tablero e) fi co))]))
  (define alto-tab-escalado (* esc (alto-tablero fi)))
  (define panel-img (dibujar-panel-fichas (ficha-sel e)))
  (place-image (dibujar-header (puntaje e) (mejor e))
               (/ VENTANA-W 2) (/ ALTO-HEADER 2)
               (place-image panel-img
                            (/ VENTANA-W 2)
                            (+ ALTO-HEADER ESPACIO (/ ALTO-PANEL 2))
                            (place-image tablero-img
                                         (/ VENTANA-W 2)
                                         (+ ALTO-HEADER ESPACIO ALTO-PANEL ESPACIO
                                            (/ alto-tab-escalado 2))
                                         canvas))))

;------------------------------------------------------------
; Funcion: render
; Descripcion: Despachador de renderizado. Todas las pantallas
;              producen imagenes del mismo tamano VENTANA-W x VENTANA-H.
;              La ventana nunca cambia de tamano al cambiar de pantalla
;              ni al cambiar el tamano del tablero.
;
; Parametros:
;   e: estado completo
;
; Retorna: imagen de VENTANA-W x VENTANA-H siempre
;
; Ejemplo: (render estado)
; Resultado: imagen del mismo tamano en cualquier pantalla
;------------------------------------------------------------
(define (render e)
  (cond
    [(string=? (pantalla e) "menu")          (render-menu e)]
    [(or (string=? (pantalla e) "colocar")
         (string=? (pantalla e) "elegir-celda")) (render-colocar e)]
    [else                                        (render-juego e)]))

;============================================================
; TECLADO
;============================================================

;------------------------------------------------------------
; Funcion: manejar-tecla-menu
; Descripcion: Navega las 4 opciones del menu con flechas
;              arriba/abajo. Cuando Filas (opcion 2) o Columnas
;              (opcion 3) esta marcado, las flechas izq/der
;              cambian el valor entre MIN y MAX.
;              Enter lanza el modo elegido con el tamano configurado.
;
; Parametros:
;   e:     estado en pantalla "menu"
;   tecla: string de la tecla presionada
;
; Retorna: nuevo estado
;------------------------------------------------------------
(define (manejar-tecla-menu e tecla)
  (define fi (cfg-filas e))
  (define co (cfg-cols  e))
  (define op (opcion e))
  (cond
    [(string=? tecla "up")
     (hacer-estado (sp "menu" (remainder (+ op 3) 4))
                   (sub-juego e) (sub-colocar e) (sub-config e))]
    [(string=? tecla "down")
     (hacer-estado (sp "menu" (remainder (+ op 1) 4))
                   (sub-juego e) (sub-colocar e) (sub-config e))]
    [(and (string=? tecla "left") (= op 2))
     (hacer-estado (sub-pantalla e) (sub-juego e) (sub-colocar e)
                   (scfg (max MIN-FILAS (- fi 1)) co))]
    [(and (string=? tecla "right") (= op 2))
     (hacer-estado (sub-pantalla e) (sub-juego e) (sub-colocar e)
                   (scfg (min MAX-FILAS (+ fi 1)) co))]
    [(and (string=? tecla "left") (= op 3))
     (hacer-estado (sub-pantalla e) (sub-juego e) (sub-colocar e)
                   (scfg fi (max MIN-COLS (- co 1))))]
    [(and (string=? tecla "right") (= op 3))
     (hacer-estado (sub-pantalla e) (sub-juego e) (sub-colocar e)
                   (scfg fi (min MAX-COLS (+ co 1))))]
    [(or (string=? tecla "return") (string=? tecla "\r"))
     (cond
       [(= op 0)
        (hacer-estado (sp "juego" 0)
                      (sj (agregar-ficha-aleatoria
                           (agregar-ficha-aleatoria (crear-tablero fi co))) 0 (mejor e))
                      (sc 0 0)
                      (sub-config e))]
       [(= op 1)
        (hacer-estado (sp "colocar" 0)
                      (sj (crear-tablero fi co) 0 (mejor e))
                      (sc 0 0)
                      (sub-config e))]
       [else e])]
    [else e]))

;------------------------------------------------------------
; Funcion: manejar-tecla-juego
; Descripcion: Flechas mueven el tablero en la direccion
;              correspondiente. Escape vuelve al menu.
;              Ignora teclas si el juego ya termino.
;
; Parametros:
;   e:     estado en pantalla "juego"
;   tecla: string de la tecla
;
; Retorna: nuevo estado
;------------------------------------------------------------
(define (manejar-tecla-juego e tecla)
  (cond
    [(string=? tecla "escape")
     (hacer-estado (sp "menu" 0) (sub-juego e) (sub-colocar e) (sub-config e))]
    [(juego-terminado? e) e]
    [(string=? tecla "left")  (mover-en-juego e 'izquierda)]
    [(string=? tecla "right") (mover-en-juego e 'derecha)]
    [(string=? tecla "up")    (mover-en-juego e 'arriba)]
    [(string=? tecla "down")  (mover-en-juego e 'abajo)]
    [else e]))

;------------------------------------------------------------
; Funcion: manejar-tecla-elegir-celda
; Descripcion: Flechas mueven el cursor sobre el tablero MxN.
;              Enter coloca la ficha en la celda del cursor
;              usando quotient/remainder sobre cfg-cols.
;              Escape vuelve a seleccion de ficha. J inicia juego.
;
; Parametros:
;   e:     estado en pantalla "elegir-celda"
;   tecla: string de la tecla
;
; Retorna: nuevo estado
;------------------------------------------------------------
(define (manejar-tecla-elegir-celda e tecla)
  (define fi (cfg-filas e))
  (define co (cfg-cols  e))
  (cond
    [(string=? tecla "escape")
     (hacer-estado (sp "colocar" 0) (sub-juego e) (sc (ficha-sel e) 0) (sub-config e))]
    [(string=? tecla "j")
     (hacer-estado (sp "juego" 0) (sub-juego e) (sc 0 0) (sub-config e))]
    [(or (string=? tecla "return") (string=? tecla "\r"))
     (hacer-estado
      (sp "colocar" 0)
      (sj (reemplazar-celda (tablero e)
                            (quotient  (cursor e) co)
                            (remainder (cursor e) co)
                            (ficha-sel e))
          (puntaje e) (mejor e))
      (sc 0 0)
      (sub-config e))]
    [(string=? tecla "up")
     (hacer-estado (sp "elegir-celda" 0) (sub-juego e)
                   (sc (ficha-sel e) (cursor-arriba    (cursor e) fi co)) (sub-config e))]
    [(string=? tecla "down")
     (hacer-estado (sp "elegir-celda" 0) (sub-juego e)
                   (sc (ficha-sel e) (cursor-abajo     (cursor e) fi co)) (sub-config e))]
    [(string=? tecla "left")
     (hacer-estado (sp "elegir-celda" 0) (sub-juego e)
                   (sc (ficha-sel e) (cursor-izquierda (cursor e) fi co)) (sub-config e))]
    [(string=? tecla "right")
     (hacer-estado (sp "elegir-celda" 0) (sub-juego e)
                   (sc (ficha-sel e) (cursor-derecha   (cursor e) fi co)) (sub-config e))]
    [else e]))

;------------------------------------------------------------
; Funcion: manejar-tecla-colocar
; Descripcion: Flechas izq/der cambian la ficha seleccionada
;              de forma circular. Enter confirma y va a elegir-celda.
;              J inicia la partida con el tablero actual. Esc al menu.
;
; Parametros:
;   e:     estado en pantalla "colocar"
;   tecla: string de la tecla
;
; Retorna: nuevo estado
;------------------------------------------------------------
(define (manejar-tecla-colocar e tecla)
  (cond
    [(string=? tecla "escape")
     (hacer-estado (sp "menu" 0) (sub-juego e) (sc 0 0) (sub-config e))]
    [(string=? tecla "j")
     (hacer-estado (sp "juego" 0) (sub-juego e) (sc 0 0) (sub-config e))]
    [(or (string=? tecla "return") (string=? tecla "\r"))
     (cond
       [(= (ficha-sel e) 0)
        (hacer-estado (sp "colocar" 0) (sub-juego e)
                      (sc (car FICHAS-DISPONIBLES) 0) (sub-config e))]
       [else
        (hacer-estado (sp "elegir-celda" 0) (sub-juego e)
                      (sc (ficha-sel e) 0) (sub-config e))])]
    [(string=? tecla "right")
     (cond
       [(= (ficha-sel e) 0)
        (hacer-estado (sp "colocar" 0) (sub-juego e)
                      (sc (car FICHAS-DISPONIBLES) 0) (sub-config e))]
       [else
        (hacer-estado (sp "colocar" 0) (sub-juego e)
                      (sc (siguiente-ficha FICHAS-DISPONIBLES (ficha-sel e)) 0)
                      (sub-config e))])]
    [(string=? tecla "left")
     (cond
       [(= (ficha-sel e) 0)
        (hacer-estado (sp "colocar" 0) (sub-juego e)
                      (sc (mi-list-ref FICHAS-DISPONIBLES
                                       (- (mi-length-local FICHAS-DISPONIBLES) 1)) 0)
                      (sub-config e))]
       [else
        (hacer-estado (sp "colocar" 0) (sub-juego e)
                      (sc (ficha-anterior FICHAS-DISPONIBLES (ficha-sel e) 0) 0)
                      (sub-config e))])]
    [else e]))

;------------------------------------------------------------
; Funcion: manejar-tecla
; Descripcion: Despachador de teclado segun la pantalla activa.
;
; Parametros:
;   e:     estado completo
;   tecla: string de la tecla presionada
;
; Retorna: nuevo estado
;
; Ejemplo: (manejar-tecla estado "up")
; Resultado: estado con cursor o menu actualizado
;------------------------------------------------------------
(define (manejar-tecla e tecla)
  (cond
    [(string=? (pantalla e) "menu")          (manejar-tecla-menu         e tecla)]
    [(string=? (pantalla e) "juego")         (manejar-tecla-juego        e tecla)]
    [(string=? (pantalla e) "colocar")       (manejar-tecla-colocar      e tecla)]
    [(string=? (pantalla e) "elegir-celda")  (manejar-tecla-elegir-celda e tecla)]
    [else e]))

;============================================================
; BIG-BANG
;============================================================

(big-bang (estado-inicial-menu)
  (to-draw  render)
  (on-key   manejar-tecla)
  (name     "2048"))

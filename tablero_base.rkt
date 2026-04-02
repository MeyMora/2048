#lang racket


;------------------------------------------------------------
; Funcion: crear fila
; Descripcion: crea una fila de longitud n y se rellena con ceros           
;
; Parametros:
;  n: representa el tamaño de la fila que queremos crear.
;
; Retorna:
;   Una fila llena de ceros
; Ejemplo: (crear-fila 4)
; resultado: (0 0 0 0)
;
(define (crear-fila n) ; Funcion para crear una fila 
  (if(= n 0) '() ;pregunta si n es igual a cero, si n es cero devuelve la lista vacia, condicion de parada
     (cons 0(crear-fila(- n 1))) ;construye una nueva lista colocando el elemento 0 al inicio de otra lista.
     ) ;La otra lista se obtiene llamando recursivamente a crear-fila con n-1.
)


;------------------------------------------------------------
; Funcion: Crear tablero
; Descripcion: Se crea un tablero vacio de m filas y n columnas          
;
; Parametros:
;  m: el número de filas.
;  n: el número de columnas.
; Retorna:
;   Un tablero de tamaño mxn
; Ejemplo: (crear-tablero 3 4)
; resultado: ((0 0 0 0)
;             (0 0 0 0)
;             (0 0 0 0))
;
(define(crear-tablero m n) ;funcion crear tablero de tamano mxn 
  (if( = m 0 ) '() ; si m es igual a cero devuelve la lista vacia, sino pasa a crear fila
     (cons(crear-fila n) ; crear-fila n genera una fila de longitud n llena de ceros
          (crear-tablero(- m 1) n )) ;crea el resto del tablero con una fila menos.
     ;cons coloca la fila recién creada al inicio de la lista de filas que forman el tablero.
     )
  )

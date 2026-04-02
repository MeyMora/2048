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

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


;------------------------------------------------------------
; Funcion: elemento-lista.
; Descripcion: Obtiene un elemento de una lista simple           
;
; Parametros:
;  lista: la lista de la cual se quiere obtener un elemento.
;  indice: la posición del elemento que se quiere (empezando desde 0).
; Retorna:
;   El elemento de la lista que se quiere obtener de acuerdo al indice
; Ejemplo: (elemento-lista '(10 20 5 40) 2)
; resultado: 5
;                     
(define(elemento-lista lista indice) ; funcion para obtener un elemento de una lista 
  (if(= indice 0) ; si el indice es igual a cero, devuelve el primer elemento de la lista
     (car lista) ; devuelve el primer elemento de la lista
     (elemento-lista(cdr lista) (- indice 1)); como el indice no es cero, avanzamos en la lista, cdr devuelve el primer elemento de la lista
     ); se llama a elemento-lista pero con la lista reducida y disminuyendo el indice
  )

;------------------------------------------------------------
; Funcion: Obtener-celda.
; Descripcion: Obtener valor en posicion fila-columna de una celda en el tablero.        
;
; Parametros:
;  tablero: la lista de listas que representa el tablero.
;  fila: el índice de la fila que se quiere obtener.
;  col: el índice de la columna dentro de esa fila. 
; Retorna:
;   El valor en la posicion fila-columna del tablero
; Ejemplo: (obtener-celda (crear-tablero 3 4) 1 2)
; resultado: 0
;
(define (obtener-celda tablero fila col) ; funcion para obtener la posicion de una celda 
  (elemento-lista(elemento-lista tablero fila)col) ;obtiene la fila número fila del tablero (ya que elemento-lista devuelve el elemento en la posición indicada).
  ) ;Como el tablero es una lista de filas, esto nos devuelve una lista que representa esa fila.
    ;Ahora, sobre esa fila obtenida, se busca el elemento en la posición col.
    ;Ese elemento es el valor de la celda en la posición (fila, col).

;------------------------------------------------------------
; Funcion: reemplazar-en-lista
; Descripcion: Funcion para reemplazar una posición en una lista       
;
; Parametros:
;  lista: la lista original.
;  indice: la posición del elemento que se quiere reemplazar (empezando en 0).
;  valor: el nuevo valor que se quiere poner en esa posición.
; Retorna:
;   La lista con el nuevo valor, en la posicion que se queria cambiar, sin el valor viejo. 
;
; Ejemplo: colocar un 4 en vez de 2
; (reemplazar-en-lista '(10 20 30 40) 2 99)

(define(reemplazar-en-lista lista indice valor)
  (cond
    [(null? lista)'()] ; si la lista esta vacia, devuelve la lista vacia
    [(= indice 0)(cons valor(cdr lista))]; si el indice es cero, significa que queremos reemplazar el primer elemento, entonces se construye una nueva lista con valor como primer elemento y el resto de la lista (cdr lista) igual.
    [else (cons(car lista); si el índice no es 0, mantienes el primer elemento (car lista) igual.
               (reemplazar-en-lista(cdr lista)(- indice 1) valor))]; luego se llama recursivamente a reemplazar-en-lista sobre el resto de la lista (cdr lista), reduciendo el índice en 1.
    ); Así se va avanzando hasta llegar a la posición correcta.
  )


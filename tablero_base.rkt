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
; Ejemplo: colocar un 99 en vez de 30
; (reemplazar-en-lista '(10 20 30 40) 2 99)

(define(reemplazar-en-lista lista indice valor)
  (cond
    [(null? lista)'()] ; si la lista esta vacia, devuelve la lista vacia
    [(= indice 0)(cons valor(cdr lista))]; si el indice es cero, significa que se quiere reemplazar el primer elemento, entonces se construye una nueva lista con valor como primer elemento y el resto de la lista (cdr lista) igual.
    [else (cons(car lista); si el índice no es 0, se mantiene el primer elemento (car lista) igual.
               (reemplazar-en-lista(cdr lista)(- indice 1) valor))]; luego se llama recursivamente a reemplazar-en-lista sobre el resto de la lista (cdr lista), reduciendo el índice en 1.
    ); Se avanza asi hasta llegar a la posición correcta.
  )

;------------------------------------------------------------
; Funcion: reemplazar-celda
; Descripcion: Reemplaza una celda del tablero y coloca en ella un nuevo valor, es decir modifica un elemento dentro del tablero, accediendo primero a la fila y luego a la columna.   
;
; Parametros:
;  tablero: lista de listas (el tablero).
;  fila: índice de la fila donde se quiere reemplazar el valor.
;  col: índice de la columna dentro de esa fila.
;  valor: el nuevo valor que quieres poner en esa celda.

; Retorna: Devuelve el tablero con el nuevo valor en la celda especifica que se queria cambiar
;   
; Ejemplo: (reemplazar-celda (crear-tablero 4 4) 1 2 2)
; Resultado: ((0 0 0 0)
;             (0 0 2 0)
;             (0 0 0 0)
;             (0 0 0 0))

(define(reemplazar-celda tablero fila col valor)
  (cond
    [(null? tablero) '()] ;si el tablero está vacío, devuelve la lista vacía.
    [(= fila 0)  ;si la fila es 0, significa que se quiere modificar la primera fila.
     (cons(reemplazar-en-lista (car tablero) col valor); (car tablero),  obtiene la primera fila, luego reemplazar-en-lista reemplaza el valor en la columna col de esa fila.
          (cdr tablero))]  ; cons construye un nuevo tablero con esa fila en especifico modificada y el resto del tablero igual.
    [else
     (cons(car tablero) ; si la fila no es 0, se mantiene la primera fila igual (car tablero).
          (reemplazar-celda(cdr tablero)(- fila 1)col valor)) ;Luego se llama recursivamente a reemplazar-celda sobre el resto del tablero (cdr tablero), reduciendo fila en 1.
     ]; y se hace esto repetidamente hasta llegar a la fila correcta 
    )
  )
;------------------------------------------------------------
; Funcion: quitar-ceros 
; Descripcion: Elimina los ceros de una fila 
;
; Parametros:
;  fila: Fila a la que se desea eliminar los ceros

; Retorna: Devuelve la fila mas corta, sin los ceros 
;   
; Ejemplo: (quitar-ceros '(2 0 2 4))
; Resultado: '(2 2 4)


(define (quitar-ceros fila) ; crear una funcion llamada quitar-ceros
  (cond
    [(null? fila) '()] ; pregunta si la fila esta vacia, si es asi devuelve la lista vacia
    [(= (car fila) 0)(quitar-ceros(cdr fila))]; car obtiene  primer elemento de la lista, si el primer elemento es cero, se ignora y se llama a quitar-ceros recursivamente con el resto de la fila 
    [else ;sino
     (cons(car fila)(quitar-ceros(cdr fila)))]; se construye una nueva lista, colocando el primer elemento de la lista al frente, seguido del resto de la fila 
    )
  )
;Como se ocupa combinar numeros iguales contiguos para el juego, se realiza la siguiente funcion:
;------------------------------------------------------------
; Funcion: combinar-fila
; Descripcion: Combina los numeros que son iguales en una fila, es decir esta función recorre una lista y combina pares consecutivos de números iguales sumándolos en uno solo.
;
; Parametros:
;  fila: Fila a la que se combina elementos

; Retorna: Devuelve la fila pero con los elementos iguales combinados
;   
; Ejemplo: (combinar-fila '(2 2 4))
; Resultado: '(4 4)

(define(combinar-fila fila)
  (cond
    [(null?  fila)'()] ; pregunta si la fila esta vacia, si es asi devuelve la lista vacia
    [(null? (cdr fila))(cons(car fila) '() )] ; si el resto de la fila esta vacio entonces quiere decir que hay solo un elemento en las fila, por lo cual se devuelve solo ese elemento
    [(= (car fila) (car(cdr fila))); pregunta si el primer elemento y el segundo elemento son iguales
     (cons (+ (car fila)(car(cdr fila))); si son iguales se suman y se colocan como un solo elemento en la nueva lista.
           (combinar-fila (cdr(cdr fila))))]; se continua la recursion pero sin contar esos dos elementos
   [else     ; si los dos primeros elementos no son iguales
     (cons(car fila) ;se conserva el primer elemento
          (combinar-fila (cdr fila)))]  ;Se llama recursivamente a la función con el resto de la lista
   )
  )

;Funcion propia de length para una lista
; Funcion: mi-length
; Descripcion: Es una funcion que cuenta cuantos elementos contiene una lista
;
; Parametros:
;  lista: lista a la cual le vamos a contar cuantos elementos tiene

; Retorna: Numero de elementos que tiene esa lista
;   
; Ejemplo: (mi-length '(7 9 11))
; Resultado: 3

(define(mi-length lista)
  (if (null? lista) ; pregunta si la lista esta vacia
      0  ; si esta vacia devuelve cero
      (+ 1 (mi-length(cdr lista))) ; si la lista no esta vacia, nos devuelve la lista sin el primer elemento, llamamos recursivamente mi-length
      ); se suma uno para ir contando los elementos en la lista 
  )

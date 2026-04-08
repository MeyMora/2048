#lang racket
(provide 
 crear-fila
 crear-tablero
 elemento-lista
 obtener-celda
 reemplazar-en-lista
 reemplazar-celda
 quitar-ceros
 combinar-fila
 mover-tablero-izquierda
 mover-tablero-derecha)


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

; Funcion: rellenar-con-ceros
; Descripcion: Es una funcion que rellena con ceros una fila, hasta un tamano especifico
;
; Parametros:
;  fila: fila a la cual le queremos colocar ceros
;  tamano: el tamaño deseado de la lista
;
; Retorna: La lista rellena con ceros, hasta el tamano especifico
;   
; Ejemplo: (rellenar-con-ceros '(1 2 3 4) 5)
; Resultado: 3
; Para rellenar una fila con ceros hacemos:

(define (rellenar-con-ceros fila tamano)
  (cond
    [(= (mi-length fila) tamano) fila] ;Si la longitud actual de la lista (mi-length fila) es igual al tamaño deseado (tamano), simplemente devuelve la lista tal como está.
    [else (rellenar-con-ceros (append fila '(0)) tamano)]; Si la lista aún no tiene el tamaño deseado, se le agrega un 0 al final con append, y se vuelve a llamar a la función.
  )
)

;----------Movimiento hacia la izquierda en una fila---------------------------
;Las funciones anteriores nos sirven ahora para poder hacer un movimiento de fila hacia la izquierda

; Funcion: mover-fila-izquierda
; Descripcion: Es una funcion que mueve la fila fila hacia la izquierda, combinando numeros de igual valor y despues rellenando con ceros si hace falta
;
; Parametros:
;  fila: fila que queremos mover hacia la izquierda
;
; Retorna: La lista movida hacia la izquierda, si hay valores iguales los suma y combina, y la rellena de ceros si hace falta
;   
; Ejemplo: (mover-fila-izquierda '(1 2 2 3 4 5 6 6))
;
; Resultado: '(1 4 3 4 5 12 0 0)

(define (mover-fila-izquierda fila)
  (rellenar-con-ceros ; primero se elimina todos los ceros de la lista.
   (combinar-fila  ;luego combina números iguales consecutivos sumándolos.
    (quitar-ceros fila))  ;Rellena con ceros al final hasta que la lista tenga el mismo tamaño que la original
   (mi-length fila)
   )
  )
;----------------------------Aplicando movimiento hacia la izquierda en el tablero--------------------

; Funcion: mover-tablero-izquierda
; Descripcion: Funcion que mueve todas las filas del tablero hacia la izquierda
;
; Parametros:
;  fila: fila que queremos mover hacia la izquierda
;
; Retorna: La lista movida hacia la izquierda, si hay valores iguales los suma y combina, y la rellena de ceros si hace falta
;   
; Ejemplo: (mover-tablero-izquierda '((1 2 2 3)(1 2 3 4)(4 4 5 6)(8 8 8 2)))
;
; Resultado: '(1 4 3 4 5 12 0 0)
(define(mover-tablero-izquierda tablero)
  (if (null? tablero) '() ;Si el tablero está vacío (null?), devuelve la lista vacía.
      (cons(mover-fila-izquierda(car tablero)); car tablero quiere decir la primera fila del tablero, y se aplica mover-fila-izquierda a esa fila.
           (mover-tablero-izquierda (cdr tablero))); cdr tablero , quiere decir que luego agarramos el resto de las filas y luego llamamos recursivamente a la función para procesar las demás filas.
      )
  )
;----------Movimiento hacia la derecha en una fila---------------------------
;como tenemos que hacer el movimiento hacia la derecha, primero tenemos que hacer invertir fila para cambiar le orden.

; Funcion: invertir lista
; Descripcion: Invierte la lista 
;
; Parametros:
;  lista: lista de elementos los cuales queremos invertir para que tengan un orden diferente
;
; Retorna: La lista invertida
;   
; Ejemplo: (invertir'(1 2 3 4))
;
; Resultado: '(4 3 2 1)

(define(invertir lista)
  (if (null? lista)'() ; pregunta si la lista está vacía, si es asi devuelve la lista vacia, no hay nada que invertir
      (append ;  concatena la lista invertida de la cola con el primer elemento al final
       (invertir(cdr lista)); saca el resto de la lista y lo invierte utilizando invertir
       (list(car lista))) ; convierte el primer elemento en una lista de un solo elemento.
      )

  )
 
; Funcion: mover-fila-derecha
; Descripcion: La función mueve los elementos de la lista hacia la derecha. Para lograrlo: Invierte la lista (invertir fila) y aplica la función mover-fila-izquierda sobre esa lista invertida. Al final vuelve a invertir el resultado.
;
; Parametros:
; fila: es una lista (por ejemplo, una fila de números o elementos en un tablero). La función espera recibir una lista como entrada.
;
; Retorna: Retorna una nueva lista con los elementos de fila desplazados hacia la derecha.
;   
; Ejemplo: (mover-fila-derecha '(0 0 4 4))
;
; Resultado: '(0 0 0 8)

(define (mover-fila-derecha fila)
  (invertir (mover-fila-izquierda(invertir fila))) ;  primero se aplica la función mover-fila-izquierda sobre la lista invertida. Mueve los elementos, pero en la posicion invertida
  ) ; luego se hace invertir al final para volver a la posicion original, pero ya habiendo movido los elementos 


;----------------------------Aplicando movimiento hacia la derecha en el tablero--------------------


; Funcion: mover-fila-derecha
; Descripcion: Funcion que desplaza todas las filas del tablero hacia la derecha, las combina y rellena con ceros 
;
; Parametros:
; fila: es una lista (por ejemplo, una fila de números o elementos en un tablero). La función espera recibir una lista como entrada.
;
; Retorna: Retorna una nueva lista con los elementos de fila desplazados hacia la derecha.
;   
; Ejemplo: (mover-tablero-derecha '((2 0 2 4)(0 2 2 0)(4 4 0 4)(2 0 0 2)))
;
; Resultado: '((0 0 4 4) (0 0 0 4) (0 0 4 8) (0 0 0 4))


(define(mover-tablero-derecha tablero)
  (if (null? tablero) '() ;Si el tablero está vacío (null?), devuelve la lista vacía.
      (cons(mover-fila-derecha(car tablero)); car tablero quiere decir la primera fila del tablero, y se aplica mover-fila-derecha a esa fila.
           (mover-tablero-derecha (cdr tablero))); cdr tablero , quiere decir que luego agarramos el resto de las filas y luego llamamos recursivamente a la función para procesar las demás filas.
      )
  )
;-----------------------Transpuesta del tablero -----------------------------------------------------

; Funcion: primera-columna
; Descripcion:  Extrae el primer elemento de cada fila del tablero, construyendo una nueva lista con esos elementos.
;
; Parametros:
; tablero : Se espera que sea una lista de listas (por ejemplo, una matriz representada como lista de filas).
;
; Retorna: Una lista que contiene la primera columna del tablero.
;   
; Ejemplo: (primera-columna '((2 4 8) (0 4 2) (2 0 2)))
;
; Resultado: '(2 0 2)
(define(primera-columna tablero)
  (if (null? tablero) '()   ;Si tablero está vacío, devuelve la lista vacía '().
      (cons(car(car tablero)) ; se toma la primera fila del tablero, luego se toma el primer elemento de esa fila (es decir, el elemento de la primera columna).
           (primera-columna (cdr tablero)) ;luego el resto del tablero (todas las filas menos la primera),  llamada recursiva para obtener la primera columna del resto del tablero
           ) ; cons, construye una nueva lista colocando el primer elemento de la primera fila al inicio, seguido de los elementos de la primera columna del resto.
      )
  )
; Funcion: quitar-primera-columna
; Descripcion: Elimina el primer elemento de cada fila del tablero.
;
; Parametros:
; tablero : tablero que es una lista de listas (cada sublista representa una fila de un tablero).
;
; Retorna:  Un nuevo tablero (lista de listas) donde cada fila ya no tiene su primer elemento.
;   
; Ejemplo: (quitar-primera-columna '((1 2 3) (4 5 6) (7 8 9)))
;
; Resultado: '((2 3)(5 6)(8 9))
(define(quitar-primera-columna tablero)
  (if (null? tablero) '() ;Caso base: si el tablero está vacío, devuelve la lista vacía '().
      (cons(cdr(car tablero)); (car tablero),toma la primera fila del tablero, devuelve esa fila sin su primer elemento, y el (cdr tablero), nos devuelve el resto del tablero (todas las filas menos la primera).
           (quitar-primera-columna(cdr tablero))) ;  llamada recursiva para procesar el resto de filas 
      ) ; (cons ... ...) → construye el nuevo tablero fila por fila, quitando la primera columna.
  )

;Como ya tenemos las funciones auxiliares ya podemos hacer la transpuesta

;La idea de la transpuesta es si ya no hay columnas, terminar, si no, se toma la primera columna y se sigue con el resto

; Funcion: transponer
; Descripcion: Devuelve la matriz transpuesta del tablero. Es decir, convierte las filas en columnas.
;
; Parametros:
; tablero : una lista de listas (cada sublista es una fila).
;
; Retorna: Devuelve una nueva lista de listas, que corresponde al tablero transpuesto.
;   
; Ejemplo: (transponer '((1 2 3) (4 5 6) (7 8 9)))
;
; Resultado: '((1 4 7 )(2 5 8)(3 6 9))
(define(transponer tablero)
  (if (null? (car tablero))'() ;Caso base: si la primera fila está vacía ((car tablero) es ()), significa que ya no quedan columnas por procesar.
      (cons ; construye la nueva matriz columna por columna.
       (primera-columna tablero); obtiene la primera columna del tablero.
           (transponer ; llamada recursiva para transponer el resto del tablero.
            (quitar-primera-columna tablero))) ; devuelve el tablero sin la primera columna.
      )
  )
;----------------------------Movimiento arriba en el tablero------------------------------------------------------------------------------------------

; Funcion: mover-tablero-arriba
; Descripcion: Mueve todas las filas del tablero hacia arriba, combinando números iguales según las reglas del juego (como 2048).
;
; Parametros:
; Parámetro: tablero que es una lista de listas que representa el tablero del juego (cada sublista es una fila).
;
; Retorna: Un nuevo tablero con los valores desplazados hacia arriba.
;   
; Ejemplo: (mover-tablero-arriba '((0 2 0 )(0 2 0)(0 0 0)))
;
; Resultado: '((0 4 0) (0 0 0) (0 0 0))
(define(mover-tablero-arriba tablero)
  (transponer ; Se vuelve a transponer el resultado para restaurar la orientación original del tablero.Ahora el tablero está en su forma normal, pero con los valores movidos hacia arriba.
   (mover-tablero-izquierda ; Aplica la función de mover a la izquierda sobre esas “filas” (que en realidad son las columnas originales).
    (transponer tablero))) ;Convierte las columnas en filas.
  )
;----------------------------Movimiento abajo en el tablero---------------------------------------------------------------------------------------------
; Funcion: mover-tablero-abajo
; Descripcion: Mueve todas las columnas del tablero hacia abajo, combinando números iguales según las reglas del juego (como en 2048).
;
; Parametros:
; Parámetro: tablero que es una lista de listas que representa el tablero del juego (cada sublista es una fila).
;
; Retorna: Un nuevo tablero con los valores desplazados hacia abajo.
;   
; Ejemplo: (mover-tablero-abajo '((4 2 0 )(4 2 0)(0 0 0)))
;
; Resultado: '((0 0 0) (0 0 0) (8 4 0))

(define(mover-tablero-abajo tablero)
  (transponer ; Se vuelve a transponer el resultado para restaurar la orientación original del tablero.
   (mover-tablero-derecha ; Aplica la función de mover a la derecha sobre esas “filas” (que en realidad son las columnas originales).
    (transponer tablero))) ; Convierte las columnas en filas.
  )

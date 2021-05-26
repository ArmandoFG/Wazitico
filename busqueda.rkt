#lang racket/gui

(provide buscar)
;(buscar 'B 'C '((A (B 5) (C 7) (J 6)) (B (C 5) (J 7)) (J (A 6) (B 3) (D 7) (C 9))))


;Revisa si el grafo no está vacio

(define (buscar origen destino grafo)
  (cond
    [(null? grafo) "Error grafo vacío"]
   [else (cond
           [(equal? #t (bool-origen origen grafo)) ;Revisa que el origen exista
            (cond
              [(equal? #t (bool-hijos origen grafo)) (qs(Busqueda origen destino grafo '() '() '() '() '()))] ;Revisa que el origen tenga rutas
              [else "Origen no tiene mas rutas"])]
           [else "Origen no existe"])]))


;Función principal de la busqueda
(define (Busqueda g_origen g_destino grafo vecinos ruta costo visitados resultado)
  (cond
       [(equal? g_origen g_destino) (Borrado g_origen g_destino grafo (agregar-elemento g_origen vecinos) ruta costo visitados (cons (agregar-resultado (agregar-elemento g_origen vecinos) costo) resultado))]
       [(equal? #t (analizado g_origen visitados)) (Borrado g_origen g_destino grafo (agregar-elemento g_origen vecinos) ruta costo visitados resultado)]
       [(equal? #t (bool-hijos g_origen grafo)) (Busqueda (primer-hijo g_origen grafo) g_destino grafo (agregar-elemento g_origen vecinos) (concatena (hijos-origen g_origen grafo) ruta) (agregar-elemento (obtener-costo (primer-hijo g_origen grafo) (obtener-lista-hijos g_origen grafo)) costo) (agregar-elemento g_origen visitados) resultado)]
       [(equal? #f (bool-hijos g_origen grafo))
        (cond
          [(null? ruta) resultado]
          [else (Borrado g_origen g_destino grafo (agregar-elemento g_origen vecinos) ruta costo (agregar-elemento g_origen visitados) resultado)])]
       ))

;Función para borrar rutas invalidas
(define (Borrado g_origen g_destino grafo vecinos ruta costo visitados  resultado)
  (cond
    [(null? ruta) resultado]
    [(equal? (car vecinos) (car ruta)) (Borrado (ultimo ruta) g_destino grafo (cdr vecinos) (cdr ruta) (cdr costo) visitados  resultado)]
    [(equal? #t (analizado g_origen visitados)) (Borrado g_origen g_destino grafo (agregar-elemento g_origen vecinos) ruta (agregar-elemento (obtener-costo g_origen (obtener-lista-hijos (car vecinos) grafo)) costo) visitados (resultados_calculados grafo g_origen vecinos (agregar-elemento (obtener-costo g_origen (obtener-lista-hijos (car vecinos) grafo)) costo) resultado resultado))]
    [(equal? g_origen g_destino) (Borrado g_origen g_destino grafo (agregar-elemento g_origen vecinos) ruta (agregar-elemento (obtener-costo g_origen (obtener-lista-hijos (car vecinos) grafo)) costo) visitados (cons (agregar-resultado (agregar-elemento g_origen vecinos) (agregar-elemento (obtener-costo g_origen (obtener-lista-hijos (car vecinos) grafo)) costo)) resultado))]
    [(equal? #t (bool-origen g_origen grafo)) (Busqueda g_origen g_destino grafo vecinos ruta (agregar-elemento (obtener-costo g_origen (obtener-lista-hijos (car vecinos) grafo)) costo) visitados resultado)]
    [else (Borrado g_origen g_destino grafo (agregar-elemento g_origen vecinos) ruta (agregar-elemento (obtener-costo g_origen (obtener-lista-hijos (car vecinos) grafo)) costo) visitados resultado)]))

;Proceso agregar resultados y seguir la busqueda por mas rutas

(define (agregar-resultado vecinos costo)
  (cons (sumar-costos costo) (reverse vecinos)))




;Revisa si el grafo posicionado es un grafo anterior
(define (analizado nodo lista)
  (cond
    [(null? lista) #f]
    [(equal? nodo (car lista)) #t]
    [else (analizado nodo (cdr lista))]))

;Función obtiene el primer hijo
(define (primer-hijo origen grafo)
  (cond
    [(equal? origen (car(car grafo))) (Obtener-hijo (cdr(car grafo)))]
    [else (primer-hijo origen (cdr grafo))])
 )
(define (Obtener-hijo lista)
  (car(car lista)))

;Verifica si es el unico elemento en la lista
(define (ultimo lista)
  (cond
    [(null? (cdr lista)) 'vacio]
    [else (car(cdr lista))]))


;Revisa que un grafo tenga mas hijos
(define (bool-hijos origen grafo)
  (cond
    [(null? grafo) #f]
    [(equal? origen (car(car grafo)))
     (cond
       [(null? (cdr(car grafo))) #f]
       [else #t])]
    [else (bool-hijos origen (cdr grafo))]))

;Obtiene los hijos de un grafo

(define (hijos-origen origen grafo)
  (cond
    [(null? grafo) "El origen no existe"]
    [(equal? origen (car(car grafo))) (Obtener (cdr(car grafo)) '())]
    [else (hijos-origen origen (cdr grafo))]))

(define (Obtener lista lista-unida)
  (cond
    [(null? lista) lista-unida]
    [else (Obtener (cdr lista) (concatena lista-unida (cons (car(car lista)) '())))]))

;Revisa que el origen exista
(define (bool-origen origen grafo)
  (cond
    [(null? grafo) #f]
    [(equal? origen (car(car grafo))) #t]
    [else (bool-origen origen (cdr grafo))]))

;Suma los elementos de la lista
(define (sumar-costos lista)
  (cond
    [(null? lista) 0]
    [else (+ (car lista) (sumar-costos (cdr lista)))]))

;Agrega un grafo a una lista
(define (agregar-elemento vecino lista)
  (cons vecino lista))

;Quita el primer elemento de la lista
(define (quitar-elemento lista)
  (cdr(car lista)))

;Obtiene hijos y los costos de un grafo
(define (obtener-lista-hijos origen grafo)
  (cond
    [(null? grafo) '()]
    [(equal? origen (car(car grafo))) (cdr(car grafo))]
    [else (obtener-lista-hijos origen (cdr grafo))]))

;Obtiene costo de un arco
(define (obtener-costo destino grafo)
  (cond
    [(equal? destino (car(car grafo))) (car(cdr(car grafo)))]
    [else (obtener-costo destino (cdr grafo))]))
  

;Une dos listas, para rutas
(define (concatena X Y)
                    (if (null? X)
                        Y
                        (cons (car X) (concatena (cdr X) Y))))

;Metodo principal para buscar resultados ya obtenidos en un grafo y no volver a recorrer

(define (resultados_calculados grafo inicio vecinos costos resultados resultado_final)
  (cond
    [(null? resultados) resultado_final]
    [(equal? #t (recorrer (car resultados) inicio)) (resultados_calculados grafo inicio vecinos costos (cdr resultados) (unir vecinos (resto_del_camino inicio (car resultados)) (+ (sumar-costos costos) (costo_calculados grafo (resto_del_camino inicio (car resultados)) 0)) resultado_final))]
    [else (resultados_calculados grafo inicio vecinos costos (cdr resultados) resultado_final)]))

; Obtiene parte del camino de la solucion ya obtenida
(define (resto_del_camino origen lista)
  (cond
    [(equal? (car lista) origen) lista]
    [else (resto_del_camino origen (cdr lista))]))

;Comprueba que el grafo de interés tenga un resultado agregado en la lista de resultados
(define (recorrer lista inicio)
  (cond
    [(null? lista) #f]
    [(equal? (car lista) inicio) #t]
    [else (recorrer (cdr lista) inicio)]))

;Suma el costo de las aristas del camino encontrado

(define (costo_calculados grafo lista resultado)
  (cond
    [(null? (cdr lista)) resultado]
    [else (costo_calculados grafo (cdr lista) (+ (obtener-costo (car(cdr lista)) (obtener-lista-hijos (car lista) grafo)) resultado))]))

;Une las respuesta encontrada con los resultados ya obtenidos
(define (unir vecinos lista costo resultado)
  (cons (cons costo (concatena (reverse vecinos) lista)) resultado))

;quicksort para ordenar los elementos
(define (qs list)
  (if (null? list) '()
  (concatena
   (concatena
    (qs (menor (caar list) (cdr list))) (cons (car list) '())) (qs (mayor(caar list) (cdr list)))))
  )

(define(mayor x list);mayores de la lista
  (if (null? list)
      '()
  (if (< x (caar list))
      (cons (car list) (mayor x (cdr list)))
      (mayor x (cdr list)))))

(define(menor x list);menores de la lista
  (if (null? list)
      '()
  (if (>= x (caar list))
      (cons (car list) (menor x (cdr list)))
      (menor x (cdr list)))))


#lang racket

(require "busqueda.rkt")
(require graphics/graphics)
(open-graphics)

;(wazitico 'A 'C '((A (B 5) (C 7) (J 6)) (B (C 5)) (J (B 3) (D 7) (C 9))))

(define ventana 0)
(define prueba 1)


(define circulo1(make-posn 350 100))
(define circulo2(make-posn 280 160))
(define circulo3(make-posn 420 160))
(define circulo4(make-posn 310 230))
(define circulo5(make-posn 390 230))

(define lb1 '())
(define lb2 '())
(define lb3 '())
(define lb4 '())
(define lb5 '())


(define grafo '())
(define nin '())
(define nde '())

;(wazitico 'A 'C '((A (B 5) (C 7) (J 6)) (B (C 5)) (J (B 3) (D 7) (C 9))))


;Ventana Inicial


; Se crea una ventana grafica
(define (wazitico n n2 arcos)

  (set! prueba 1)
  (set! nin n)
  (set! nde n2)
  (set! grafo arcos)

  
  (set! ventana (open-viewport "wazitico" 700 700))
  ((draw-viewport ventana) "Light blue" )
  

  ((draw-solid-rectangle ventana)(make-posn 10 10) 97 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 10 60) 97 40 "gray")
  ;((draw-line ventana)(make-posn 10 10) (make-posn 107 50)"red")
  ((draw-string ventana)(make-posn 20 35) "Calcular ruta")
  (cond
    ;((equal? (length grafo) 1)"Error inserte al menos 2 nodos")
    ((equal? (length grafo) 2)(dosNodos)(arc grafo))
    ((equal? (length grafo) 3)(tresNodos)(arc grafo))
    ((equal? (length grafo) 4)(cuatroNodos)(arc grafo))
    ((equal? (length grafo) 5)(cincoNodos)(arc grafo))
    )

  (control)
)

;Movimientos del mouse
(define (control)
           (cond((= prueba 2)
                   (close-viewport ventana))
             
           (else(cond
           ((and (<= 10(posn-x(mouse-click-posn (get-mouse-click ventana)))107)
                 (<= 10(posn-y(mouse-click-posn (get-mouse-click ventana)))50))
            (resultado nin nde grafo)))
           (control))))


;Llama la ventana con el resultado (**SOLO ESTA DE PRUEBA**)
(define (resultado ni nf arcos)
  ((draw-solid-rectangle ventana)(make-posn 10 110) 150 350 "white")


  (resRutas (buscar ni nf arcos) 90)
  
  )

;Funcion que dibuja los resultados de la ruta
(define (resRutas lista num)
  (cond
    ((null? lista)((draw-string ventana)(make-posn 15 (+ num 40)) "Ruta obtenida"))
    (else
     ((draw-string ventana)(make-posn 15 (+ num 40)) (~a(car lista)))
     (resRutas (cdr lista)(+ num 40))
     ))
  
  )


;Funcion de dibujado en caso de ser 2 nodos
(define (dosNodos)
  (set! circulo1 (make-posn 350 100))
  (set! circulo2 (make-posn 350 300))
  
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo1) 20) (- (posn-y circulo1) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo2) 20) (- (posn-y circulo2) 30))  50 50 "gray")
  
  ((draw-string ventana) circulo1  (~a (caar grafo)))
  ((draw-string ventana) circulo2  (~a (caadr grafo)))

  (set! lb1 (caar grafo)) 
  (set! lb2 (caadr grafo)) 
  )


;Funcion de dibujado en caso de ser 3 nodos
(define (tresNodos)
  (set! circulo1 (make-posn 350 100))
  (set! circulo2 (make-posn 250 300))
  (set! circulo3 (make-posn 450 300))

  
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo1) 20) (- (posn-y circulo1) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo2) 20) (- (posn-y circulo2) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo3) 20) (- (posn-y circulo3) 30))  50 50 "gray")
  
  ((draw-string ventana) circulo1  (~a (caar grafo)))
  ((draw-string ventana) circulo2  (~a (caadr grafo)))
  ((draw-string ventana) circulo3  (~a (caaddr grafo)))

  (set! lb1 (caar grafo)) 
  (set! lb2 (caadr grafo)) 
  (set! lb3 (caaddr grafo))
  )

;Funcion de dibujado en caso de ser 4 nodos 
(define (cuatroNodos)
  
  (set! circulo1 (make-posn 350 100))
  (set! circulo2 (make-posn 250 200))
  (set! circulo3 (make-posn 450 200))
  (set! circulo4 (make-posn 350 300))

  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo1) 20) (- (posn-y circulo1) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo2) 20) (- (posn-y circulo2) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo3) 20) (- (posn-y circulo3) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo4) 20) (- (posn-y circulo4) 30))  50 50 "gray")
  
  ((draw-string ventana) circulo1  (~a (caar grafo)))
  ((draw-string ventana) circulo2  (~a (caadr grafo)))
  ((draw-string ventana) circulo3  (~a (caaddr grafo)))
  ((draw-string ventana) circulo4  (~a (car(cadddr grafo))))

  (set! lb1 (caar grafo)) 
  (set! lb2 (caadr grafo)) 
  (set! lb3 (caaddr grafo)) 
  (set! lb4 (car(cadddr grafo)))
  )



;Funcion de dibujado en caso de ser 5 nodos
(define (cincoNodos)

  (set! circulo1 (make-posn 350 100))
  (set! circulo2 (make-posn 280 160))
  (set! circulo3 (make-posn 420 160))
  (set! circulo4 (make-posn 310 230))
  (set! circulo5 (make-posn 390 230))
  
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo1) 20) (- (posn-y circulo1) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo2) 20) (- (posn-y circulo2) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo3) 20) (- (posn-y circulo3) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo4) 20) (- (posn-y circulo4) 30))  50 50 "gray")
  ((draw-solid-ellipse ventana) (make-posn (- (posn-x circulo5) 20) (- (posn-y circulo5) 30))  50 50 "gray")
  
  ((draw-string ventana) circulo1  (~a (caar grafo)))
  ((draw-string ventana) circulo2  (~a (caadr grafo)))
  ((draw-string ventana) circulo3  (~a (caaddr grafo)))
  ((draw-string ventana) circulo4  (~a (car(cadddr grafo))))
  ((draw-string ventana) circulo5  (~a (caar(cddddr grafo))))

  (set! lb1 (caar grafo)) 
  (set! lb2 (caadr grafo)) 
  (set! lb3 (caaddr grafo)) 
  (set! lb4 (car(cadddr grafo))) 
  (set! lb5 (caar(cddddr grafo)))

  )

;funcion que dibuja los arcos
(define (arc grf)
  (cond
    ((null? grf)'())
    ((equal? (caar grf) lb1)(arcAux1 (cdar grf) circulo1 "red")   (arc (cdr grf)))
    ((equal? (caar grf) lb2)(arcAux1 (cdar grf) circulo2 "blue")  (arc (cdr grf)))
    ((equal? (caar grf) lb3)(arcAux1 (cdar grf) circulo3 "green") (arc (cdr grf)))
    ((equal? (caar grf) lb4)(arcAux1 (cdar grf) circulo4 "orange")(arc (cdr grf)))
    ((equal? (caar grf) lb5)(arcAux1 (cdar grf) circulo5 "violet")(arc (cdr grf)))   
    ))

(define (arcAux1 vec circ color)
  (cond
    ((null? vec)'())
    ((equal? (caar vec) lb1)((draw-line ventana) circ circulo1 color)(arcAux1 (cdr vec) circ color))
    ((equal? (caar vec) lb2)((draw-line ventana) circ circulo2 color)(arcAux1 (cdr vec) circ color))
    ((equal? (caar vec) lb3)((draw-line ventana) circ circulo3 color)(arcAux1 (cdr vec) circ color))
    ((equal? (caar vec) lb4)((draw-line ventana) circ circulo4 color)(arcAux1 (cdr vec) circ color))
    ((equal? (caar vec) lb5)((draw-line ventana) circ circulo5 color)(arcAux1 (cdr vec) circ color))
    ))
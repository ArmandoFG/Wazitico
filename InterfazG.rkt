#lang racket
(require racket/gui)
(require "busqueda.rkt")
(require graphics/graphics)
(open-graphics)

(define ventana 0)
(define prueba 1)


;Ventana Inicial
(define mainwindow(new frame%
                       [label "wazitico"]
                       [width 300]
                       [height 100]
                       [style '(fullscreen-button)]
                       [alignment '(left top)]))

(define panel (new horizontal-panel% [parent mainwindow]
                                     [alignment '(left top)]
                                     [min-height 30]
                                     [stretchable-width #f]
                                     [stretchable-height #f]))

;Campo de texto para el grafo (*)
(define entrada(new text-field% [parent panel] [label "Ingrese su grafo"]
                                     [min-width 600]
                                     [min-height 30]
                                     [vert-margin 10]
                                     [horiz-margin 10]
                                     [stretchable-width #f]
                                     [stretchable-height #f]))
 
;Boton para ingresar el grafo (*)
(new button% [parent panel] [label "Añadir"]
                            [horiz-margin 5]
                            [vert-margin 14]
                            [callback (lambda (button event)
                                           (wazitico))])
                                            ;(send entrada get-value) text-output))])



;texto
(define txt (new text% [auto-wrap #t]))

;Funcion que envia y calcula el grafo
(define (salida grafo out)
  (send txt insert "MLP")
  (send out set-editor txt))

;Inicia la ventana
(send mainwindow show #t)


;-------------------------------------------------------------------------------------


;Funcion que llama a la ventana que dibuja el grafo
(define (abrirVentana x)
  (cond
    ((equal? x "#t")(wazitico))
    
  ))

; Se crea una ventana grafica
(define (wazitico)
  (send mainwindow show #f)
  (set! prueba 1)
  
  (set! ventana (open-viewport "wazitico" 700 700))
  ((draw-viewport ventana) "Light blue" )
  

  ((draw-solid-rectangle ventana)(make-posn 10 10) 97 40 "gray")
  ((draw-solid-rectangle ventana)(make-posn 10 60) 97 40 "gray")
  ;((draw-line ventana)(make-posn 10 10) (make-posn 107 50)"red")
  ((draw-string ventana)(make-posn 20 35) "Calcular ruta")
  (control)
)

;Movimientos del mouse
(define (control)
           (cond((= prueba 2)
                   (close-viewport ventana))
             
           (else(cond
           ((and (<= 10(posn-x(mouse-click-posn (get-mouse-click ventana)))107)
                 (<= 10(posn-y(mouse-click-posn (get-mouse-click ventana)))50))
            (resultado)))
           (control))))



;Funcion que dibuja los resultados de la ruta
(define (resRutas lista num)
  (cond
    ((null? lista)((draw-string ventana)(make-posn 15 (+ num 40)) "Ruta obtenida"))
    (else
     ((draw-string ventana)(make-posn 15 (+ num 40)) (~a(car lista)))
     (resRutas (cdr lista)(+ num 40))
     ))
  
  )



;Llama la ventana con el resultado (**SOLO ESTA DE PRUEBA**)
(define (resultado)
  ((draw-solid-rectangle ventana)(make-posn 10 110) 97 240 "white")


  (resRutas (buscar 'A 'C '((A (B 5) (C 7) (J 6)) (B (C 5)) (J (B 3) (D 7) (C 9)))) 90)
  
  ;(send resWindow show #t)
  ;(set! prueba 2)
  )

;ventana de resultado(define mainwindow(new frame%
(define resWindow(new frame%
                       [label "wazitico"]
                       [width 300]
                       [height 100]
                       [style '(fullscreen-button)]
                       [alignment '(left top)]))

;Campo de texto de salida
(define text-output (new editor-canvas%
                         [parent resWindow]
                         [label "Permutations"]
                         [min-width 200]
                         [min-height 100]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [style '(no-hscroll auto-vscroll)]
                         [stretchable-width #t]
                         [stretchable-height #t]))
#lang racket
(require racket/gui)
;Ventana principal
(define mainwindow(new frame%
                       [label "wazitico"]
                       [width 800]
                       [height 900]
                       [style '(fullscreen-button)]
                       [alignment '(left top)]))


;Panel para el ingreso del grafo
(define panel (new horizontal-panel% [parent mainwindow]
                                     [alignment '(left top)]
                                     [min-height 30]
                                     [stretchable-width #f]
                                     [stretchable-height #f]))



;Campo de texto para el grafo (*)
(new text-field% [parent panel] [label "Ingrese su grafo"]
                                     [min-width 600]
                                     [min-height 30]
                                     [vert-margin 10]
                                     [horiz-margin 10]
                                     [stretchable-width #f]
                                     [stretchable-height #f])
 
;Boton para ingresar el grafo (*)
(new button% [parent panel] [label "Añadir"]
                            [horiz-margin 5]
                            [vert-margin 14])

;Panel que contiene la parte de calcular el destino
(define panel2 (new horizontal-panel% [parent mainwindow]
                                      [alignment '(left top)]))

;Campo de texto para el lugar de salida (*)
(new text-field% [parent panel2] [label "Ingrese su lugar de salida"]
                                     [min-width 100]
                                     [min-height 30]
                                     [vert-margin 10]
                                     [horiz-margin 10]
                                     [stretchable-width #f]
                                     [stretchable-height #f])


;Campo de texto para ingresar el destino (*)
(new text-field% [parent panel2] [label "Ingrese su destino"]
                                     [min-width 100]
                                     [min-height 30]
                                     [vert-margin 10]
                                     [horiz-margin 10]
                                     [stretchable-width #f]
                                     [stretchable-height #f])


;boton para calcular el trayecto (*)
(new button% [parent panel2] [label "Calcular trayecto"]
                             [horiz-margin 10]
                             [vert-margin 14])

;Inicia la ventana
(send mainwindow show #t)
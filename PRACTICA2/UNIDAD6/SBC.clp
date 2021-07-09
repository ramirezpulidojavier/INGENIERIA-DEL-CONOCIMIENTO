(defmodule EleccionAsesoramiento )
   
   ;PREGUNTAMOS SOBRE QUE QUIERE ASESORAMIENTO
   (defrule decision_asesoramiento
      (declare (auto-focus TRUE))
   =>
      (printout t "-------------------------------------------------------------------------------" crlf )
      (printout t "-------------------------------------------------------------------------------" crlf )
      (printout t "-------------------------------------------------------------------------------" crlf )
      (printout t "-------------------------------------------------------------------------------" crlf )
      (printout t "-------------------------------------------------------------------------------" crlf )
      (printout t "-------------------------------------------------------------------------------" crlf )
      (printout t "Sobre que quiere asesoramiento? Sobre 'ramas' o sobre 'asignaturas'? (Para terminar introduzca 'nada')" crlf )
      (assert (asesoramiento (read)))
      
   )

   ;COMPROBAMOS QUE LA RESPUESTA SEA ALGUNA DE LAS DOS OPCIONES
   (defrule comprobacion_decision
      ?borrar <- (asesoramiento ?asesora)
      (test (and 
               (neq ?asesora ramas)
               (neq ?asesora asignaturas)
               (neq ?asesora nada)
            )
      )
   =>
      (retract ?borrar)
      (printout t "Lo siento, la opcion introducida no es correcta, escriba 'asignaturas', 'ramas' o 'nada'" crlf )
      (assert (asesoramiento (read)))
   )

   ;SI EL USUARIO QUIERE ASESORAMIENTO SOBRE RAMAS
   (defrule comenzamos_ramas
      ?eliminar <- (asesoramiento ramas)
   =>
      (retract ?eliminar)
      (pop-focus)
      (focus VolvemosAPrimera)
   )

   ;SI EL USUARIO QUIERE ASESORAMIENTO SOBRE RAMAS
   (defrule comenzamos_asignaturas
      ?eliminar <- (asesoramiento asignaturas)
   =>
      (retract ?eliminar)
      (pop-focus)
      (focus primeras_preguntas) 
   )

   ;SI EL USUARIO NO QUIERE ASESORAMIENTO
   (defrule no_comenzamos
      (asesoramiento nada)
   =>
      (pop-focus)
      (printout t "Perfecto, muchas gracias!" crlf )
   )






;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------

(defmodule DeclaracionRamas
   (export deftemplate ?ALL))
   (deffacts Ramas
      (Rama Computacion_y_Sistemas_Inteligentes)
      (Rama Ingenieria_del_Software)
      (Rama Ingenieria_de_Computadores)
      (Rama Sistemas_de_Informacion)
      (Rama Tecnologias_de_la_Informacion)
   )

;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------
(defmodule PreguntasPrevias (export deftemplate ?ALL))
   
   (deffacts asignaturas
      (primero MP) 
      (primero FP)
      (primero IES)
      (primero TOC)
      (primero CAL)
      (primero ALEM)
      (primero ES)
      (primero FS)
      (primero FFT)
      (primero LMD)
      (segundo ED)        
      (segundo AC)
      (segundo EC)
      (segundo SCD)
      (segundo FBD)
      (segundo ALG)
      (segundo SO)
      (segundo PDOO)
      (segundo FIS)
      (segundo IA)
      (Consejo NADA) 
   )


   

   ;;;;;;;;;;;PARTE EN LA QUE PREGUNTA CUAL ES LA ASIGNATURA DE PRIMERO FAVORITA
   (defrule titulo_asig_primero
      (declare (salience 9999))
   =>
      (printout t "Cual de las siguientes es tu asignatura favorita de primero?:" crlf)
      (printout t "        Ninguna" crlf)
      (assert (pedir_asignatura_primero))
   )

   ;;;;;;;;;;;PARTE EN LA MUESTRA CADA ASIGNATURA DE PRIMERO 
   (defrule asignaturas_de_primero
      (declare (salience 9998))
      (primero ?f)
   =>
      (printout t "        " ?f crlf)
   )

   ;;;;;;;;;;;PARTE EN LA QUE SE INTRODUCE LA FAVORITA DE PRIMERO 
   (defrule favorita_de_primero
      ?g <- (pedir_asignatura_primero)
   =>
      (assert (favorita_primero (read)))
      (assert (pedir_asignatura_segundo))
      (retract ?g)
   )


   ;;;;;;;;;;COMPRUEBA QUE LA ASIGNATURA ES CORRECTA
   (defrule comprobacion_asignatura_primero
      ?g <- (favorita_primero ?f)
      ?w <- (pedir_asignatura_segundo)
      (test (and (neq ?f Ninguna) (neq ?f MP) (neq ?f FP) (neq ?f IES) (neq ?f TOC) (neq ?f ALEM) (neq ?f ES) (neq ?f FS) (neq ?f FFT) (neq ?f LMD) (neq ?f CAL)))
   =>
      (printout t ?f " no es una opcion correcta, introduce otra"crlf)
      (assert (pedir_asignatura_primero))
      (retract ?g)
      (retract ?w)
   )

   ;-------------------------------------------------------------------------------------------------------------------

   ;;;;;;;;;;;PARTE EN LA QUE PREGUNTA CUAL ES LA ASIGNATURA DE SEGUNDO FAVORITA
   (defrule titulo_asig_segundo
      (pedir_asignatura_segundo)
   =>
      (printout t crlf "Cual de las siguientes es tu asignatura favorita de segundo?:" crlf )
      (printout t "        Ninguna" crlf)
      (assert (mostrar_segundo))
   )


   ;;;;;;;;;;;PARTE EN LA MUESTRA CADA ASIGNATURA DE SEGUNDO
   (defrule asignaturas_de_segundo
      (segundo ?f)
      (mostrar_segundo)
   =>
      (printout t "        " ?f crlf)
   )

   ;;;;;;;;;;;PARTE EN LA QUE SE INTRODUCE LA FAVORITA DE SEGUNDO
   (defrule favorita_de_segundo
      ?g <- (pedir_asignatura_segundo)
      ?w <- (mostrar_segundo)
   =>
      (assert (favorita_segundo (read)))
      (retract ?g)
      (retract ?w)
      (assert (pedir_nota))
   )

   ;;;;;;;;;;COMPRUEBA QUE LA ASIGNATURA ES CORRECTA
   (defrule comprobacion_asignatura_segundo
      ?g <- (favorita_segundo ?f)
      ?w <- (pedir_nota)
      (test (and (neq ?f Ninguna) (neq ?f AC) (neq ?f EC) (neq ?f SCD) (neq ?f FBD) (neq ?f ALG) (neq ?f SO) (neq ?f PDOO) (neq ?f FIS) (neq ?f IA) (neq ?f ED)))
   =>
      (printout t ?f " no es una opcion correcta, introduce otra"crlf)
      (assert (pedir_asignatura_segundo))
      (retract ?g)
      (retract ?w)
   )

   ;-------------------------------------------------------------------------------------------------------------------

   ;;;;;;;;;;PREGUNTO POR LA NOTA MEDIA 
   (defrule nota_media_usuario
      (declare (salience 9901))
      ?g <- (pedir_nota)
   =>
      (printout t "Cual es tu nota media actual?: (SUFICIENTE (5), BIEN (6), NOTABLE (7-8), SOBRESALIENTE (9-10))" crlf)
      (assert (nota_media (read)))
      (retract ?g)
      (assert (pedir_esfuerzo))
   )

   ;;;;;;;;;;COMPRUEBA QUE LA NOTA MEDIA INTRODUCIDA ES CORRECTA
   (defrule comprobacion_nota_media
      (declare (salience 9900))
      ?g <- (nota_media ?f)
      ?w <- (pedir_esfuerzo)
      (test (and (neq ?f BIEN) (neq ?f NOTABLE) (neq ?f SUFICIENTE) (neq ?f SOBRESALIENTE)))
   =>
      (printout t ?f " no es una opcion correcta, introduce otra"crlf)
      (assert (pedir_nota))
      (retract ?g)
      (retract ?w)
   )

   ;-------------------------------------------------------------------------------------------------------------------

   ;;;;;;;;;;PREGUNTO POR SU ESFUERZO EN LA CARRERA
   (defrule esfuerzo_usuario
      (declare (salience 9900))
      ?g <- (pedir_esfuerzo)
   =>
      (printout t "Como consideras tu esfuerzo en la carrera? (ALTO/MEDIO/BAJO)"crlf)
      (assert (esfuerzo (read)))
      (retract ?g)
      (assert (pedir_rama_comienzo))
   )

   ;;;;;;;;;;COMPRUEBA QUE EL ESFUERZO INTRODUCIDO ES CORRECTO
   (defrule comprobacion_esfuerzo
      (declare (salience 9900))
      ?g <- (esfuerzo ?f)
      (test (and (neq ?f ALTO) (neq ?f MEDIO) (neq ?f BAJO)))
   =>
      (printout t ?f " no es una opcion correcta, introduce otra"crlf)
      (assert (pedir_esfuerzo))
      (retract ?g)
   )


   ;;;;;;;;;;SALTAMOS AL MODULO EN EL QUE SE DECIDE LA RAMA POR LA QUE EMPEZAR A RAIZ DE LAS PREGUNTAS DE ESTE MODULO
   (defrule esfuerzo_correcto
      (esfuerzo ?f)
      (test (or (eq ?f ALTO) (eq ?f MEDIO) (eq ?f BAJO)))
   =>
      (printout t "Muy bien, vamos a elegir la rama con la que mas concuerdas..."crlf)
      (pop-focus) (focus DecisionPrimerCamino)
   )


;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------
(defmodule DecisionPrimerCamino (import PreguntasPrevias deftemplate ?ALL)
                                (export deftemplate ?ALL))

   ;;;;;;;;;;DECIDO POR QUE RAMA EMPEZAR PORQUE SUS DOS ASIGNATURAS ELEGIDAS SON DE LA MISMA
   (defrule si_gusta_csi
      (declare (salience 9899))
      ?g <- (pedir_rama_comienzo)
      (favorita_primero ?m)
      (test (or (eq ?m MP) (eq ?m FP) (eq ?m CAL) (eq ?m ALEM) (eq ?m ES) (eq ?m LMD)))
      (favorita_segundo ?n)
      (test (or (eq ?n IA) (eq ?n PDOO) (eq ?n ALG) (eq ?n ED)))
   =>
      (assert (empezar_por CSI))
      (assert (primero_fue Computacion_y_Sistemas_Inteligentes))
      (retract ?g)
   )


   (defrule si_gusta_is
      (declare (salience 9898))
      ?g <- (pedir_rama_comienzo)
      (favorita_primero ?m)
      (test (eq ?m FS))
      (favorita_segundo ?n)
      (test (or (eq ?n FIS) (eq ?n SO) (eq ?n SCD)))
   =>
      (assert (empezar_por IS))
      (assert (primero_fue Ingenieria_del_Software))
      (retract ?g)
   )


   (defrule si_gusta_si
      (declare (salience 9897))
      ?g <- (pedir_rama_comienzo)
      (favorita_primero ?m)
      (test (eq ?m IES))
      (favorita_segundo ?n)
      (test (eq ?n FBD))
   =>
      (assert (empezar_por SI))
      (assert (primero_fue Sistemas_de_Informacion))
      (retract ?g)
   )

   (defrule si_gusta_ic
      (declare (salience 9896))
      ?g <- (pedir_rama_comienzo)
      (favorita_primero ?m)
      (test (or (eq ?m TOC) (eq ?m FFT)))
      (favorita_segundo ?n)
      (test (or (eq ?n AC) (eq ?n EC)))
   =>
      (assert (empezar_por IC))
      (assert (primero_fue Ingenieria_de_Computadores))
      (retract ?g)
   )



   ;-------------------------------------------------------------------------------------------------------------------

   ;;;;;;;;;;SUS ASIGNATURAS FAVORITAS SON DE RAMAS DIFERENTES ASI QUE ME BASO EN LA MEDIA (PRIORIZO MEDIAS ALTAS)
   (defrule si_media_alta_csi
      (declare (salience 9799))
      ?g <- (pedir_rama_comienzo)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (favorita_primero ?m)
      (favorita_segundo ?n)
      (test (or (eq ?m MP) (eq ?m FP) (eq ?m CAL) (eq ?m ALEM) (eq ?m ES) (eq ?m LMD) (eq ?n IA) (eq ?n PDOO) (eq ?n ALG) (eq ?n ED)))
   =>
      (assert (empezar_por CSI))
      (assert (primero_fue Computacion_y_Sistemas_Inteligentes))
      (retract ?g)
   )


   (defrule si_media_alta_is
      (declare (salience 9798))
      ?g <- (pedir_rama_comienzo)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (favorita_primero ?m)
      (favorita_segundo ?n)
      (test (or (eq ?m FS) (eq ?n FIS) (eq ?n SO) (eq ?n SCD)))
   =>
      (assert (empezar_por IS))
      (assert (primero_fue Ingenieria_del_Software))
      (retract ?g)
   )


   (defrule si_media_no_alta_si
      (declare (salience 9797))
      ?g <- (pedir_rama_comienzo)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      (favorita_primero ?m)
      (favorita_segundo ?n)
      (test (or (eq ?m IES) (eq ?n FBD)))
   =>
      (assert (empezar_por SI))
      (assert (primero_fue Sistemas_de_Informacion))
      (retract ?g)
   )



   (defrule si_media_no_alta_ti
      (declare (salience 9795))
      ?g <- (pedir_rama_comienzo)
   =>
      (assert (empezar_por TI))
      (assert (primero_fue Tecnologias_de_la_Informacion))
      (retract ?g)

   )


   (defrule si_media_no_alta_ic
      (declare (salience 9796))
      ?g <- (pedir_rama_comienzo)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      (favorita_primero ?m)
      (favorita_segundo ?n)
      (test (or (eq ?m TOC) (eq ?m FFT) (eq ?n AC) (eq ?n EC)))
   =>
      (assert (empezar_por IC))
      (assert (primero_fue Ingenieria_de_Computadores))
      (retract ?g)
   )

   (defrule vamos_a_Modulocontrol
      (empezar_por ?g)
      (primero_fue ?f)
   =>
      (pop-focus) (focus ModuloControlDeBloque)
   )

;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------
(defmodule ModuloControlDeBloque (import DecisionPrimerCamino deftemplate ?ALL)
                                 (import DeclaracionRamas deftemplate ?ALL))

   ;PARA CONDUCIR EL CAMINO POR CSI
   (defrule decision_csi
      (empezar_por CSI)
      ?f <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (pop-focus) (focus ModuloCSI)
      (retract ?f)
   )

   ;PARA CONDUCIR EL CAMINO POR IS
   (defrule decision_is
      (empezar_por IS)
      ?f <- (Rama Ingenieria_del_Software)
   =>
      (pop-focus) (focus ModuloIS)
      (retract ?f)
   )

   ;PARA CONDUCIR EL CAMINO POR SI
   (defrule decision_si
      (empezar_por SI)
      ?f <- (Rama Sistemas_de_Informacion)
   =>
      (pop-focus) (focus ModuloSI)
      (retract ?f)
   )

   ;PARA CONDUCIR EL CAMINO POR IC
   (defrule decision_ic
      (empezar_por IC)
      ?f <- (Rama Ingenieria_de_Computadores)
   =>
      (pop-focus) (focus ModuloIC)
      (retract ?f)
   )

   ;PARA CONDUCIR EL CAMINO POR TI
   (defrule decision_ti
      (empezar_por TI)
      ?f <- (Rama Tecnologias_de_la_Informacion)
   =>
      (pop-focus) (focus ModuloTI)
      (retract ?f)
   )
;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------------
(defmodule ModuloIC (import DeclaracionRamas deftemplate ?ALL)
                    (import DecisionPrimerCamino deftemplate ?ALL)
                    (export deftemplate ?ALL)) 

   ;PRIMERA PREGUNTA
   (defrule empezamos_por_ic
      (declare (salience 9684))
      (empezar_por IC)
   =>
      (printout t "Te gustan la arquitectura del sistema? (SI/NO/NO SE)"crlf)
      (assert (arquitectura_sistema (read)))
   )


   ;SEGUNDA PREGUNTA SI
   (defrule segunda_pregunta_si
      (arquitectura_sistema SI)
      (empezar_por IC)
   =>
      (printout t "Te gusta el hardware digital? (SI/NO/NO SE)"crlf)
      (assert (hardware_digital2 (read)))
   )


   ;SEGUNDA PREGUNTA NO
   (defrule segunda_pregunta_no
      (arquitectura_sistema NO)
      (empezar_por IC)
   =>
      (printout t "Te gusta el hardware digital? (SI/NO/NO SE)"crlf)
      (assert (hardware_digital1 (read)))
   )


   ;TERCERA PREGUNTA 1
   (defrule tercera_pregunta_1
      (arquitectura_sistema NO)
      (hardware_digital1 SI)
      (empezar_por IC)
   =>
      (printout t "Te gustan los microprocesadores y los elementos fisicos del sistema? (SI/NO/NO SE)"crlf)
      (assert (microprocesador1 (read)))
   )


   ;TERCERA PREGUNTA 1 OTRO CAMINO
   (defrule tercera_pregunta_1_otro_camino
      (arquitectura_sistema SI)
      (hardware_digital2 NO)
      (empezar_por IC)
   =>
      (printout t "Te gustan los microprocesadores y los elementos fisicos del sistema? (SI/NO/NO SE)"crlf)
      (assert (microprocesador1 (read)))
   )


   ;TERCERA PREGUNTA 2 
   (defrule tercera_pregunta_2
      (arquitectura_sistema SI)
      (hardware_digital2 SI)
      (empezar_por IC)
   =>
      (printout t "Te gustan los microprocesadores y los elementos fisicos del sistema? (SI/NO/NO SE)"crlf)
      (assert (microprocesador2 (read)))
   )

   ;LLEGO A CONCLUSION 1
   (defrule llego_a_conclusion1
      (microprocesador2 SI)
      ?f <- (empezar_por IC)
      ?g <- (Consejo NADA)
   =>
      (retract ?g)
      (retract ?f)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )


   ;LLEGO A LA MEDIA1
   (defrule llego_a_la_media_1
      (hardware_digital1 NO)
      (empezar_por IC)
   =>
      (assert (media1_ic))
   )

   ;LLEGO A LA MEDIA1
   (defrule llego_a_la_media_1_otro_camino
      (microprocesador1 NO)
      (empezar_por IC)
   =>
      (assert (media1_ic))
   )



   ;USO A LA MEDIA1 PARA BAJA O MEDIA
   (defrule uso_la_media_1_bajamedia
      (media1_ic)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      ?g <- (empezar_por IC)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (retract ?p)
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
   )

   ;USO A LA MEDIA1 PARA BAJA O MEDIA
   (defrule uso_la_media_1_bajamedia_2
      (media1_ic)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      ?g <- (empezar_por IC)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (retract ?p)
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
   )

   ;USO A LA MEDIA1 PARA BAJA O MEDIA
   (defrule uso_la_media_1_bajamedia_3
      (media1_ic)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      ?g <- (empezar_por IC)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (retract ?p)
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
   )

   ;USO A LA MEDIA1 PARA BAJA O MEDIA
   (defrule uso_la_media_1_bajamedia_4
      (media1_ic)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      ?g <- (empezar_por IC)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (retract ?p)
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
   )

   ;USO LA MEDIA1 PARA ALTA Y PREGUNTO POR LAS MATEMATICAS 
   (defrule uso_la_media_1_alta
      (media1_ic)
      (nota_media ?f)
      (hardware_ic ?g)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE) (eq ?g NO)))
      (empezar_por IC)
   =>
      (printout t "Te gustan las matematicas? (SI/NO/NO SE)"crlf)
      (assert (matematicas_ic (read)))
   )


   ;SI LE GUSTAN LAS MATEMATICAS
   (defrule si_le_gustan_las_mates_ic
      (matematicas_ic SI)
      ?g <- (empezar_por IC)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTAN LAS MATEMATICAS
   (defrule si_le_gustan_las_mates_ic_2
      (matematicas_ic SI)
      ?g <- (empezar_por IC)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTAN LAS MATEMATICAS
   (defrule si_le_gustan_las_mates_ic_3
      (matematicas_ic SI)
      ?g <- (empezar_por IC)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )


   ;SI LE GUSTAN LAS MATEMATICAS
   (defrule si_le_gustan_las_mates_ic_4
      (matematicas_ic SI)
      ?g <- (empezar_por IC)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )


   ;SI NO LE GUSTAN LAS MATEMATICAS
   (defrule si_no_le_gustan_las_mates_ic
      (matematicas_ic NO)
      ?g <- (empezar_por IC)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?p)
      (retract ?g)
   )

   ;SI NO LE GUSTAN LAS MATEMATICAS
   (defrule si_no_le_gustan_las_mates_ic_2
      (matematicas_ic NO)
      ?g <- (empezar_por IC)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?p)
      (retract ?g)
   )

   ;SI NO LE GUSTAN LAS MATEMATICAS
   (defrule si_no_le_gustan_las_mates_ic_3
      (matematicas_ic NO)
      ?g <- (empezar_por IC)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?p)
      (retract ?g)
   )

   ;SI NO LE GUSTAN LAS MATEMATICAS
   (defrule si_no_le_gustan_las_mates_ic_4
      (matematicas_ic NO)
      ?g <- (empezar_por IC)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?p)
      (retract ?g)
   )



   ;UTILIZO EL ESFUERZO PARA LLEGO CONCLUSION 2
   (defrule esfuerzo_bajo_ic
      (esfuerzo ?f)
      (microprocesador2 NO)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      ?g <- (empezar_por IC)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?g)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama_y_no_te_esfuerzas_mucho JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;UTILIZO EL ESFUERZO PARA LLEGO CONCLUSION 2 OTRO CAMINO
   (defrule esfuerzo_bajo_ic_otro_camino
      (esfuerzo ?f)
      (microprocesador1 SI)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      ?g <- (empezar_por IC)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?g)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama_y_no_te_esfuerzas_mucho JavierRamirez))
      (pop-focus) (focus Decision)
   )


   ;UTILIZO EL ESFUERZO Y LA MEDIA
   (defrule esfuerzo_alto_media_baja_ic
      (esfuerzo ALTO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      ?g <- (empezar_por IC)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?g)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama,_te_esfuerzas_pero_no_tienes_media_alta JavierRamirez))
      (pop-focus) (focus Decision)
   )


   ;UTILIZO EL ESFUERZO Y LA MEDIA
   (defrule esfuerzo_alto_media_alta_ic
      (esfuerzo ALTO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (empezar_por IC)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware_ic (read)))
   )


   ;AHORA QUE SE SI LE GUSTA HARDWARE, TOMO CONCLUSION 3
   (defrule conclusion_3_ic
      (hardware_ic SI)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IC)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?g)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama,_te_esfuerzas,_no_tienes_media_alta_pero_te_gusta_el_hardware JavierRamirez))
      (pop-focus) (focus Decision)
   )

;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------
(defmodule ModuloIS (import ModuloIC deftemplate ?ALL)
                    (export deftemplate ?ALL)) 
   
   ;PRIMERA PREGUNTA
   (defrule empezamos_por_is
      (declare (salience 9684))
      (empezar_por IS)
   =>
      (printout t "Te gusta el desarrollo de aplicaciones? (SI/NO/NO SE)" crlf)
      (assert (apps (read)))
   )  

   ;SEGUNDA PREGUNTA SI
   (defrule segunda_pregunta_si_is
      (apps SI)
      (empezar_por IS)
   =>
      (printout t "Te gusta hacer interfaces de usuario? (SI/NO/NO SE)"crlf)
      (assert (interfaz1 (read)))
   )


   ;SEGUNDA PREGUNTA NO
   (defrule segunda_pregunta_no_is
      (apps NO)
      (empezar_por IS)
   =>
      (printout t "Te gusta hacer interfaces de usuario? (SI/NO/NO SE)"crlf)
      (assert (interfaz2 (read)))
   )


   ;TERCERA PREGUNTA 1
   (defrule tercera_pregunta_1_is
      (interfaz1 SI)
      (empezar_por IS)
   =>
      (printout t "Te gusta hacer paginas web? (SI/NO/NO SE)"crlf)
      (assert (pagina_web1 (read)))
   )


   ;TERCERA PREGUNTA 2 
   (defrule tercera_pregunta_2_is
      (apps SI)
      (interfaz1 NO)
      (empezar_por IS)
   =>
      (printout t "Te gusta hacer paginas web? (SI/NO/NO SE)"crlf)
      (assert (pagina_web2 (read)))
   )

   ;TERCERA PREGUNTA 2 OTRO CAMINO
   (defrule tercera_pregunta_2_is_otro_camino
      (apps NO)
      (interfaz2 SI)
      (empezar_por IS)
   =>
      (printout t "Te gusta hacer paginas web? (SI/NO/NO SE)"crlf)
      (assert (pagina_web2 (read)))
   )


   ;LLEGO A CONCLUSION 1
   (defrule llego_a_conclusion1_is
      (pagina_web1 SI)
      ?f <- (empezar_por IS)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Ingenieria_del_Software Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )


   ;UTILIZO LA MEDIA1 ALTA
   (defrule va_para_is_media_alta_sisino
      (pagina_web1 NO)
      (nota_media ?f)
      (test (or (eq ?f NOTABLE) (eq ?f SOBRESALIENTE)))
      ?u <- (empezar_por IS)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?u)
      (assert (Consejo Ingenieria_del_Software Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;UTILIZO LA MEDIA1 ALTA ESFUERZO ALTO
   (defrule va_para_is_media_alta_esfuerzo_alto_sisino
      (pagina_web1 NO)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo ?g)
      (test (or (eq ?g MEDIO) (eq ?g ALTO)))
      ?u <- (empezar_por IS)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?u)
      (assert (Consejo Ingenieria_del_Software Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;UTILIZO LA MEDIA1 BAJA Y SIN ESFUERZO
   (defrule va_para_is_media_baja_sisino
      (pagina_web1 NO)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo BAJO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (retract ?p)
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 BAJA Y SIN ESFUERZO
   (defrule va_para_is_media_baja_sisino_2
      (pagina_web1 NO)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo BAJO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (retract ?p)
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 BAJA Y SIN ESFUERZO
   (defrule va_para_is_media_baja_sisino_3
      (pagina_web1 NO)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo BAJO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (retract ?p)
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 BAJA Y SIN ESFUERZO
   (defrule va_para_is_media_baja_sisino_4
      (pagina_web1 NO)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo BAJO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (retract ?p)
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
   )


   ;UTILIZO LA MEDIA1 ALTA
   (defrule media1_alta_is
      (pagina_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (retract ?p)
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 ALTA
   (defrule media1_alta_is_2
      (pagina_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (retract ?p)
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 ALTA
   (defrule media1_alta_is_3
      (pagina_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (retract ?p)
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 ALTA
   (defrule media1_alta_is_4
      (pagina_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (retract ?p)
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 ALTA OTRO CAMINO
   (defrule media1_alta_is_otro_camino
      (interfaz2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 ALTA OTRO CAMINO
   (defrule media1_alta_is_otro_camino_2
      (interfaz2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 ALTA OTRO CAMINO
   (defrule media1_alta_is_otro_camino_3
      (interfaz2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 ALTA OTRO CAMINO
   (defrule media1_alta_is_otro_camino_4
      (interfaz2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 MEDIA ESFUERZO ALTO
   (defrule media1_media_is_esfuerzo_alto
      (pagina_web2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 MEDIA ESFUERZO ALTO
   (defrule media1_media_is_esfuerzo_alto_2
      (pagina_web2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 MEDIA ESFUERZO ALTO
   (defrule media1_media_is_esfuerzo_alto_3
      (pagina_web2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 MEDIA ESFUERZO ALTO
   (defrule media1_media_is_esfuerzo_alto_4
      (pagina_web2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 MEDIA OTRO CAMINO ESFUERZO ALTO
   (defrule media1_media_is_otro_camino_esfuerzo_alto
      (interfaz2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?p)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 MEDIA OTRO CAMINO ESFUERZO ALTO
   (defrule media1_media_is_otro_camino_esfuerzo_alto_2
      (interfaz2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?p)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 MEDIA OTRO CAMINO ESFUERZO ALTO
   (defrule media1_media_is_otro_camino_esfuerzo_alto_3
      (interfaz2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?p)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 MEDIA OTRO CAMINO ESFUERZO ALTO
   (defrule media1_media_is_otro_camino_esfuerzo_alto_4
      (interfaz2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?p)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 MEDIA ESFUERZO BAJO
   (defrule media1_media_is_esfuerzo_bajo
      (pagina_web2 NO)
      (nota_media BIEN)
      (esfuerzo ?f)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      (empezar_por IS)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware_is (read)))
   )

   ;UTILIZO LA MEDIA1 MEDIA OTRO CAMINO ESFUERZO BAJO
   (defrule media1_media_is_otro_camino_esfuerzo_bajo
      (interfaz2 NO)
      (nota_media BIEN)
      (esfuerzo ?f)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      (empezar_por IS)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware_is (read)))
   )


   ;UTILIZO LA MEDIA1 BAJA
   (defrule media1_baja_is
      (pagina_web2 NO)
      (nota_media SUFICIENTE)
      (empezar_por IS)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware_is (read)))
   )

   ;UTILIZO LA MEDIA1 ALTA
   (defrule va_para_is_media_alta
      (pagina_web2 SI)
      (nota_media ?f)
      (test (or (eq ?f NOTABLE) (eq ?f SOBRESALIENTE)))
      ?u <- (empezar_por IS)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?u)
      (assert (Consejo Ingenieria_del_Software Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;UTILIZO LA MEDIA1 ALTA ESFUERZO ALTO
   (defrule va_para_is_media_alta_esfuerzo_alto
      (pagina_web2 SI)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo ?g)
      (test (or (eq ?g MEDIO) (eq ?g ALTO)))
      ?u <- (empezar_por IS)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?u)
      (assert (Consejo Ingenieria_del_Software Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;UTILIZO LA MEDIA1 BAJA Y SIN ESFUERZO
   (defrule va_para_is_media_baja
      (pagina_web2 SI)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo BAJO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 BAJA Y SIN ESFUERZO
   (defrule va_para_is_media_baja_2
      (pagina_web2 SI)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo BAJO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 BAJA Y SIN ESFUERZO
   (defrule va_para_is_media_baja_3
      (pagina_web2 SI)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo BAJO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 BAJA Y SIN ESFUERZO
   (defrule va_para_is_media_baja_4
      (pagina_web2 SI)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo BAJO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA1 BAJA OTRO CAMINO
   (defrule media1_baja_is_otro_camino
      (interfaz2 NO)
      (nota_media SUFICIENTE)
      (empezar_por IS)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware_is (read)))
   )

   ;SI PREFIERE HARDWARE ANTES QUE SOFTWARE ESTANDO EN IS
   (defrule gusta_hardware_is
      (hardware_is SI)
      ?g <- (empezar_por IS)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI PREFIERE HARDWARE ANTES QUE SOFTWARE ESTANDO EN IS
   (defrule gusta_hardware_is_2
      (hardware_is SI)
      ?g <- (empezar_por IS)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI PREFIERE HARDWARE ANTES QUE SOFTWARE ESTANDO EN IS
   (defrule gusta_hardware_is_3
      (hardware_is SI)
      ?g <- (empezar_por IS)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI PREFIERE HARDWARE ANTES QUE SOFTWARE ESTANDO EN IS
   (defrule gusta_hardware_is_4
      (hardware_is SI)
      ?g <- (empezar_por IS)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI PREFIERE SOFTWARE ANTES QUE HARDWARE ESTANDO EN IS
   (defrule no_gusta_hardware_is
      (hardware_is NO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI PREFIERE SOFTWARE ANTES QUE HARDWARE ESTANDO EN IS
   (defrule no_gusta_hardware_is_2
      (hardware_is NO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI PREFIERE SOFTWARE ANTES QUE HARDWARE ESTANDO EN IS
   (defrule no_gusta_hardware_is_3
      (hardware_is NO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI PREFIERE SOFTWARE ANTES QUE HARDWARE ESTANDO EN IS
   (defrule no_gusta_hardware_is_4
      (hardware_is NO)
      ?g <- (empezar_por IS)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------
(defmodule ModuloCSI (import ModuloIS deftemplate ?ALL)
                     (export deftemplate ?ALL)) 

   ;PRIMERA PREGUNTA
   (defrule empezamos_por_csi
      (declare (salience 9684))
      (empezar_por CSI)
   =>
      (printout t "Te gustan las matematicas? (SI/NO/NO SE)"crlf)
      (assert (matematicas_csi (read)))
   )




   ;SEGUNDA PREGUNTA SI
   (defrule segunda_pregunta_si_csi
      (matematicas_csi SI)
      (empezar_por CSI)
   =>
      (printout t "Te gusta la programacion? (SI/NO/NO SE)"crlf)
      (assert (programacion2 (read)))
   )


   ;SEGUNDA PREGUNTA NO
   (defrule segunda_pregunta_no_csi
      (matematicas_csi NO)
      (empezar_por CSI)
   =>
      (printout t "Te gusta la programacion? (SI/NO/NO SE)"crlf)
      (assert (programacion1 (read)))
   )


   ;TERCERA PREGUNTA 1
   (defrule tercera_pregunta_1_csi
      (matematicas_csi SI)
      (programacion2 SI)
      (empezar_por CSI)
   =>
      (printout t "Te gusta la Inteligencia Artificial? (SI/NO/NO SE)"crlf)
      (assert (ia1 (read)))
   )


   ;TERCERA PREGUNTA 2 
   (defrule tercera_pregunta_2_csi
      (matematicas_csi SI)
      (programacion2 NO)
      (empezar_por CSI)
   =>
      (printout t "Te gusta la Inteligencia Artificial? (SI/NO/NO SE)"crlf)
      (assert (ia2 (read)))
   )

   ;TERCERA PREGUNTA 2 OTRO CAMINO
   (defrule tercera_pregunta_2_csi_otro_camino
      (matematicas_csi NO)
      (programacion1 SI)
      (empezar_por CSI)
   =>
      (printout t "Te gusta la Inteligencia Artificial? (SI/NO/NO SE)"crlf)
      (assert (ia2 (read)))
   )


   ;LLEGO A CONCLUSION 1
   (defrule llego_a_conclusion1_csi
      (ia1 SI)
      ?f <- (empezar_por CSI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Computacion_y_Sistemas_Inteligentes Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )


   ;USO EL ESFUERZO 1 CSI
   (defrule uso_esfuerzo_1_csi
      (ia1 NO)
      (esfuerzo ?f)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      (empezar_por CSI)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware3_csi (read)))
   )

   ;SI LE GUSTA MAS EL HARDWARE QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_gusta_hardware3_csi
      (hardware3_csi SI)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_gusta_hardware3_csi_2
      (hardware3_csi SI)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_gusta_hardware3_csi_3
      (hardware3_csi SI)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_gusta_hardware3_csi_4
      (hardware3_csi SI)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )


   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_no_gusta_hardware3_csi
      (hardware3_csi NO)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_no_gusta_hardware3_csi_2
      (hardware3_csi NO)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_no_gusta_hardware3_csi_3
      (hardware3_csi NO)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_no_gusta_hardware3_csi_4
      (hardware3_csi NO)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )


   ;USO LA MEDIA ALTA
   (defrule uso_la_media_alta_csi
      (ia2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA ALTA
   (defrule uso_la_media_alta_csi_2
      (ia2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA ALTA
   (defrule uso_la_media_alta_csi_3
      (ia2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA ALTA
   (defrule uso_la_media_alta_csi_4
      (ia2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA ALTA OTRO CAMINO
   (defrule uso_la_media_alta_csi_otro_camino
      (programacion1 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA ALTA OTRO CAMINO
   (defrule uso_la_media_alta_csi_otro_camino_2
      (programacion1 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA ALTA OTRO CAMINO
   (defrule uso_la_media_alta_csi_otro_camino_3
      (programacion1 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA ALTA OTRO CAMINO
   (defrule uso_la_media_alta_csi_otro_camino_4
      (programacion1 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA BAJA
   (defrule uso_la_media_baja_csi
      (ia2 NO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware2_csi (read)))  
   )

   ;USO LA MEDIA ALTA OTRO CAMINO
   (defrule uso_la_media_baja_csi_otro_camino
      (programacion1 NO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware2_csi (read)))  
   )



   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule gusta_hardware2_csi
      (hardware2_csi SI)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule gusta_hardware2_csi_2
      (hardware2_csi SI)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule gusta_hardware2_csi_3
      (hardware2_csi SI)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule gusta_hardware2_csi_4
      (hardware2_csi SI)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 ESTANDO EN CSI 
   (defrule no_gusta_hardware2_csi
      (hardware2_csi NO)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 ESTANDO EN CSI 
   (defrule no_gusta_hardware2_csi_2
      (hardware2_csi NO)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 ESTANDO EN CSI 
   (defrule no_gusta_hardware2_csi_3
      (hardware2_csi NO)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 ESTANDO EN CSI 
   (defrule no_gusta_hardware2_csi_4
      (hardware2_csi NO)
      ?g <- (empezar_por CSI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )


   ;TENGO EN CUENTA EL ESFUERZO2 BAJO
   (defrule esfuerzo_2_bajo_csi
      (ia2 SI)
      (esfuerzo ?f)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      (empezar_por CSI)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware2_csi (read)))  
   )

   ;TENGO EN CUENTA EL ESFUERZO2 ALTO
   (defrule esfuerzo_2_alto_csi
      (ia2 SI)
      (esfuerzo ALTO)
      ?f <- (empezar_por CSI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Computacion_y_Sistemas_Inteligentes Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama_y_te_esfuerzas JavierRamirez))
      (pop-focus) (focus Decision)
   )

;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------
(defmodule ModuloSI (import ModuloCSI deftemplate ?ALL)
                    (export deftemplate ?ALL)) 

   ;PRIMERA PREGUNTA
   (defrule empezamos_por_si
      (declare (salience 9684))
      (empezar_por SI)
   =>
      (printout t "Te gusta la programacion web? (SI/NO/NO SE)" crlf)
      (assert (programacion_web (read)))
   )

   


   ;SEGUNDA PREGUNTA SI
   (defrule segunda_pregunta_si_si
      (programacion_web SI)
      (empezar_por SI)
   =>
      (printout t "Te gusta hacer sistemas de informacion para empresas? (SI/NO/NO SE)"crlf)
      (assert (si_empresas2 (read)))
   )


   ;SEGUNDA PREGUNTA NO
   (defrule segunda_pregunta_no_si
      (programacion_web NO)
      (empezar_por SI)
   =>
      (printout t "Te gusta hacer sistemas de informacion para empresas? (SI/NO/NO SE)"crlf)
      (assert (si_empresas1 (read)))
   )


   ;TERCERA PREGUNTA 1
   (defrule tercera_pregunta_1_si
      (si_empresas2 SI)
      (empezar_por SI)
   =>
      (printout t "Te gustan las bases de datos? (SI/NO/NO SE)"crlf)
      (assert (bases_datos1 (read)))
   )


   ;TERCERA PREGUNTA 2 
   (defrule tercera_pregunta_2_si
      (si_empresas2 NO)
      (empezar_por SI)
   =>
      (printout t "Te gustan las bases de datos? (SI/NO/NO SE)"crlf)
      (assert (bases_datos2 (read)))
   )

   ;TERCERA PREGUNTA 2 OTRO CAMINO
   (defrule tercera_pregunta_2_si_otro_camino
      (si_empresas1 SI)
      (empezar_por SI)
   =>
      (printout t "Te gustan las bases de datos? (SI/NO/NO SE)"crlf)
      (assert (bases_datos2 (read)))
   )


   ;LLEGO A CONCLUSION 1
   (defrule llego_a_conclusion1_si
      (bases_datos1 SI)
      ?f <- (empezar_por SI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Sistemas_de_Informacion Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;UTILIZO LA MEDIA1 ALTA
   (defrule media1_alta_si
      (si_empresas1 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (empezar_por SI)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware2 (read)))
   )



   ;UTILIZO LA MEDIA1 BAJA
   (defrule media1_baja_si
      (si_empresas1 NO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      (empezar_por SI)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware1 (read)))
   )


   ;SI LE GUSTA MAS EL HARDWARE1 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware1_si
      (hardware1 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE1 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware1_si_2
      (hardware1 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE1 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware1_si_3
      (hardware1 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE1 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware1_si_4
      (hardware1 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE1 EN SI
   (defrule no_le_gusta_hardware1_si
      (hardware1 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE1 EN SI
   (defrule no_le_gusta_hardware1_si_2
      (hardware1 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE1 EN SI
   (defrule no_le_gusta_hardware1_si_3
      (hardware1 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE1 EN SI
   (defrule no_le_gusta_hardware1_si_4
      (hardware1 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE1 EN SI
   (defrule no_le_gusta_hardware1_si_4_comprobacion
      (hardware1 NO)
      ?g <- (empezar_por SI)
   =>
      (pop-focus) (focus VolvemosAPrimera)
      (retract ?g)
   )

   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware2_si
      (hardware2 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware2_si_2
      (hardware2 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware2_si_3
      (hardware2 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware2_si_4
      (hardware2 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )


   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 EN SI
   (defrule no_le_gusta_hardware2_si
      (hardware2 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 EN SI
   (defrule no_le_gusta_hardware2_si_2
      (hardware2 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 EN SI
   (defrule no_le_gusta_hardware2_si_3
      (hardware2 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 EN SI
   (defrule no_le_gusta_hardware2_si_4
      (hardware2 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 EN SI
   (defrule no_le_gusta_hardware2_si_JAJA
      (hardware2 NO)
      ?g <- (empezar_por SI)
   =>
      (printout t "holi")
      (pop-focus) (focus VolvemosAPrimera)
      (retract ?g)
   )


   ;UTILIZO LA MEDIA2 ALTA PARA RECONDUCIR
   (defrule uso_media2_alta_si
      (bases_datos2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA2 ALTA PARA RECONDUCIR
   (defrule uso_media2_alta_si_2
      (bases_datos2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por SI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA2 ALTA PARA RECONDUCIR
   (defrule uso_media2_alta_si_3
      (bases_datos2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por SI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;UTILIZO LA MEDIA2 ALTA PARA RECONDUCIR
   (defrule uso_media2_alta_si_4
      (bases_datos2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )


   ;UTILIZO LA MEDIA2 BAJA PARA RECONDUCIR
   (defrule uso_media2_baja_si
      (bases_datos2 NO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      (empezar_por SI)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware1 (read)))
   )

   ;UTILIZO EL ESFUERZO BAJO/MEDIO PARA RECONDUCIR
   (defrule uso_esfuerzo_bajo_si
      (bases_datos1 NO)
      (esfuerzo ?g)
      (test (or (eq ?g BAJO) (eq ?g MEDIO)))
      ?f <- (empezar_por SI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Sistemas_de_Informacion Te_gusta_mayormente_el_contenido_de_la_rama_y_no_te_esfuerzas_mucho JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;UTILIZO EL ESFUERZO BAJO/MEDIO PARA RECONDUCIR OTRO CAMINO
   (defrule uso_esfuerzo_bajo_otro_camino_si
      (bases_datos2 SI)
      (esfuerzo ?g)
      (test (or (eq ?g BAJO) (eq ?g MEDIO)))
      ?f <- (empezar_por SI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Sistemas_de_Informacion Te_gusta_mayormente_el_contenido_de_la_rama_y_no_te_esfuerzas_mucho JavierRamirez))
      (pop-focus) (focus Decision)
   )


   ;UTILIZO EL ESFUERZO ALTO Y LA MEDIA BAJA
   (defrule uso_esfuerzo_alto_media_baja_si
      (bases_datos2 SI)
      (esfuerzo ALTO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      (empezar_por SI)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware1 (read)))
   )


   ;UTILIZO EL ESFUERZO ALTO Y LA MEDIA BAJA OTRO CAMINO
   (defrule uso_esfuerzo_alto_media_baja_otro_camino_si
      (bases_datos1 NO)
      (esfuerzo ALTO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      (empezar_por SI)
   =>
      (printout t "Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware1 (read)))
   )


   ;UTILIZO EL ESFUERZO ALTO Y LA MEDIA ALTA
   (defrule uso_esfuerzo_alto_media_alta_si
      (bases_datos2 SI)
      (esfuerzo ALTO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (empezar_por SI)
   =>
      (printout t "Prefieres las matematicas a las interfaces de usuario? (SI/NO/NO SE)"crlf)
      (assert (mates_si (read)))
   )


   ;UTILIZO EL ESFUERZO ALTO Y LA MEDIA ALTA OTRO CAMINO
   (defrule uso_esfuerzo_alto_media_alta_otro_camino_si
      (bases_datos1 NO)
      (esfuerzo ALTO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (empezar_por SI)
   =>
      (printout t "Prefieres las matematicas a las interfaces de usuario? (SI/NO/NO SE)"crlf)
      (assert (mates_si (read)))
   )

   ;SI LE GUSTAN LAS MATES MAS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_si_si
      (matematicas1 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTAN LAS MATES MAS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_si_si_2
      (matematicas1 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTAN LAS MATES MAS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_si_si_3
      (matematicas1 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTAN LAS MATES MAS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_si_si_4
      (matematicas1 SI)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )


   ;SI LE GUSTAN LAS MATES MENOS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_no_si
      (matematicas1 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTAN LAS MATES MENOS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_no_si_2
      (matematicas1 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTAN LAS MATES MENOS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_no_si_3
      (matematicas1 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Tecnologias_de_la_Informacion)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus ModuloTI)
      (retract ?g)
      (retract ?p)
   )

   ;SI LE GUSTAN LAS MATES MENOS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_no_si_4
      (matematicas1 NO)
      ?g <- (empezar_por SI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )


;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------
(defmodule ModuloTI (import ModuloSI deftemplate ?ALL)
                    (export deftemplate ?ALL)) 
   
   ;PRIMERA PREGUNTA
   (defrule empezamos_por_ti
      (declare (salience 9684))
      (empezar_por TI)
   =>
      (printout t "Te gustan los servidores? (SI/NO/NO SE)" crlf)
      (assert (servidores (read)))
   )


   ;SEGUNDA PREGUNTA SI
   (defrule segunda_pregunta_si_ti
      (servidores SI)
      (empezar_por TI)
   =>
      (printout t "Te gustan los temarios sobre redes? (SI/NO/NO SE)"crlf)
      (assert (redes1 (read)))
   )


   ;SEGUNDA PREGUNTA NO
   (defrule segunda_pregunta_no_ti
      (servidores NO)
      (empezar_por TI)
   =>
      (printout t "Te gustan los temarios sobre redes? (SI/NO/NO SE)"crlf)
      (assert (redes2 (read)))
   )


   ;TERCERA PREGUNTA 1
   (defrule tercera_pregunta_1_ti
      (redes1 SI)
      (empezar_por TI)
   =>
      (printout t "Te gusta tecnologia web? (SI/NO/NO SE)"crlf)
      (assert (tecnologia_web1 (read)))
   )


   ;TERCERA PREGUNTA 2 
   (defrule tercera_pregunta_2_ti
      (redes1 NO)
      (empezar_por TI)
   =>
      (printout t "Te gusta tecnologia web? (SI/NO/NO SE)"crlf)
      (assert (tecnologia_web2 (read)))
   )

   ;TERCERA PREGUNTA 2 OTRO CAMINO
   (defrule tercera_pregunta_2_ti_otro_camino
      (redes2 SI)
      (empezar_por TI)
   =>
      (printout t "Te gusta tecnologia web? (SI/NO/NO SE)"crlf)
      (assert (tecnologia_web2 (read)))
   )


   ;LLEGO A CONCLUSION 1
   (defrule llego_a_conclusion1_ti
      (tecnologia_web1 SI)
      ?f <- (empezar_por TI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Tecnologias_de_la_Informacion Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;USO LA MEDIA1 BAJA
   (defrule uso_media1_baja_ti
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 BAJA
   (defrule uso_media1_baja_ti_2
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 BAJA
   (defrule uso_media1_baja_ti_3
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 BAJA
   (defrule uso_media1_baja_ti_4
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 BAJA OTRO CAMINO
   (defrule uso_media1_baja_ti_otro_camino
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 BAJA OTRO CAMINO
   (defrule uso_media1_baja_ti_otro_camino_2
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 BAJA OTRO CAMINO
   (defrule uso_media1_baja_ti_otro_camino_3
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 BAJA OTRO CAMINO
   (defrule uso_media1_baja_ti_otro_camino_4
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO ALTO
   (defrule uso_media1_alta_esfuerzo_alto_ti
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO ALTO
   (defrule uso_media1_alta_esfuerzo_alto_ti_2
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO ALTO
   (defrule uso_media1_alta_esfuerzo_alto_ti_3
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO ALTO
   (defrule uso_media1_alta_esfuerzo_alto_ti_4
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )


   ;USO LA MEDIA1 ALTA ESFUERZO ALTO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_alto_ti_otro_camino
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?p)
      (retract ?g)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO ALTO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_alto_ti_otro_camino_2
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?p)
      (retract ?g)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO ALTO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_alto_ti_otro_camino_3
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?p)
      (retract ?g)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO ALTO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_alto_ti_otro_camino_4
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?p)
      (retract ?g)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO BAJO
   (defrule uso_media1_alta_esfuerzo_bajo_ti
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ?h)
      (test (or (eq ?h BAJO) (eq ?h MEDIO)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO BAJO
   (defrule uso_media1_alta_esfuerzo_bajo_ti_2
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ?h)
      (test (or (eq ?h BAJO) (eq ?h MEDIO)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO BAJO
   (defrule uso_media1_alta_esfuerzo_bajo_ti_3
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ?h)
      (test (or (eq ?h BAJO) (eq ?h MEDIO)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO BAJO
   (defrule uso_media1_alta_esfuerzo_bajo_ti_4
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ?h)
      (test (or (eq ?h BAJO) (eq ?h MEDIO)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )


   ;USO LA MEDIA1 ALTA ESFUERZO BAJO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_bajo_ti_otro_camino
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ?h)
      (test (or (eq ?h BAJO) (eq ?h MEDIO)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO BAJO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_bajo_ti_otro_camino_2
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ?h)
      (test (or (eq ?h BAJO) (eq ?h MEDIO)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO BAJO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_bajo_ti_otro_camino_3
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ?h)
      (test (or (eq ?h BAJO) (eq ?h MEDIO)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO BAJO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_bajo_ti_otro_camino_4
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ?h)
      (test (or (eq ?h BAJO) (eq ?h MEDIO)))
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;USO LA MEDIA2 BAJA
   (defrule llego_a_conclusion2_ti
      (tecnologia_web1 NO)
      (nota_media SUFICIENTE)
      ?f <- (empezar_por TI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Tecnologias_de_la_Informacion Te_gustan_sus_asignaturas_dadas,_el_contenido_de_la_rama_mayormente_y_tu_media_es_baja JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;USO LA MEDIA2 BAJA OTRO CAMINO
   (defrule media2_baja_otro_camino_ti
      (tecnologia_web2 SI)
      (nota_media SUFICIENTE)
      ?f <- (empezar_por TI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Tecnologias_de_la_Informacion Te_gustan_sus_asignaturas_dadas,_el_contenido_de_la_rama_mayormente_y_tu_media_es_baja))
      (pop-focus) (focus Decision)
   )


   ;USO LA MEDIA2 ALTA Y ESFUERZO ALTO
   (defrule media2_alta_esfuerzo_alto_ti
      (tecnologia_web1 NO)
      (nota_media ?h)
      (test (neq ?h SUFICIENTE))
      (esfuerzo ?g)
      (test (or (eq ?g MEDIO) (eq ?g ALTO)))
      (empezar_por TI)
   =>
      (printout t "Te gusta la parte multimedia de la informatica? (SI/NO/NO SE)"crlf)
      (assert (multimedia (read)))
   )

   ;USO LA MEDIA2 ALTA Y ESFUERZO ALTO OTRO CAMINO
   (defrule media2_alta_esfuerzo_alto_ti_otro_camino
      (tecnologia_web2 SI)
      (nota_media ?h)
      (test (neq ?h SUFICIENTE))
      (esfuerzo ?g)
      (test (or (eq ?g MEDIO) (eq ?g ALTO)))
      (empezar_por TI)
   =>
      (printout t "Te gusta la parte multimedia de la informatica? (SI/NO/NO SE)"crlf)
      (assert (multimedia (read)))
   )


   ;USO LA MEDIA2 ALTA Y ESFUERZO BAJO
   (defrule media2_alta_esfuerzo_bajo_ti
      (tecnologia_web1 NO)
      (nota_media ?h)
      (test (neq ?h SUFICIENTE))
      (esfuerzo BAJO)
      ?f <- (empezar_por TI)
      ?s <- (Consejo NADA)
   =>
      (retract ?s)
      (retract ?f)
      (assert (Consejo Tecnologias_de_la_Informacion Te_gusta_meyormente_el_contenido_de_la_rama_y_a_pesar_de_tener_media_alta_no_te_esfuerzas_mucho JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;USO LA MEDIA2 ALTA Y ESFUERZO BAJO OTRO CAMINO
   (defrule media2_alta_esfuerzo_bajo_otro_camino_ti
      (tecnologia_web2 SI)
      (nota_media ?h)
      (test (neq ?h SUFICIENTE))
      (esfuerzo BAJO)
      ?f <- (empezar_por TI)
      ?s <- (Consejo NADA)
   =>
      (retract ?s)
      (retract ?f)
      (assert (Consejo Tecnologias_de_la_Informacion Te_gusta_meyormente_el_contenido_de_la_rama_y_a_pesar_de_tener_media_alta_no_te_esfuerzas_mucho JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;SI LE GUSTAN LAS ASIGNATURAS DE MULTIMEDIA
   (defrule multimedia_si_ti
      (multimedia SI)
      ?f <- (empezar_por TI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (assert (Consejo Tecnologias_de_la_Informacion Te_gusta_meyormente_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;SI NO LE GUSTAN LAS ASIGNATURAS DE MULTIMEDIA
   (defrule multimedia_no_ti
      (multimedia NO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (assert (empezar_por CSI))
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
      (retract ?p)
   )

   ;SI NO LE GUSTAN LAS ASIGNATURAS DE MULTIMEDIA
   (defrule multimedia_no_ti_2
      (multimedia NO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_de_Computadores)
   =>
      (assert (empezar_por IC))
      (pop-focus) (focus ModuloIC)
      (retract ?g)
      (retract ?p)
   )

   ;SI NO LE GUSTAN LAS ASIGNATURAS DE MULTIMEDIA
   (defrule multimedia_no_ti_3
      (multimedia NO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Ingenieria_del_Software)
   =>
      (assert (empezar_por IS))
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (retract ?p)
   )

   ;SI NO LE GUSTAN LAS ASIGNATURAS DE MULTIMEDIA
   (defrule multimedia_no_ti_4
      (multimedia NO)
      ?g <- (empezar_por TI)
      ?p <- (Rama Sistemas_de_Informacion)
   =>
      (assert (empezar_por SI))
      (pop-focus) (focus ModuloSI)
      (retract ?g)
      (retract ?p)
   )

;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------;--------------------------------------------------------------------------------------------------------------------------

(defmodule VolvemosAPrimera (import ModuloTI deftemplate ?ALL)(export deftemplate ?ALL)) 

   
   (defrule si_confirma
      (primero_fue ?f)
      (Consejo NADA)
   =>
      (assert (Consejo ?f A_raiz_de_tus_asignaturas_favoritas_y_tu_media_te_aconsejo_esta_rama JavierRamirez))
      (pop-focus) (focus Decision)
   ) 

   (defrule para_empezar
      (not (exists (primero_fue ?f)))
      (not (exists (Consejo ?g ?h ?j)))
   =>
      (focus PreguntasPrevias)
   ) 


(defmodule Decision (import VolvemosAPrimera deftemplate ?ALL))


   (defrule decision_final
      (Consejo ?f ?g ?h)
      (test (neq ?f Ninguna))
   =>
      (printout t ?h " te recomienda la rama " ?f " porque " ?g crlf)
      (reset)

   ) 



;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------------------------------------


; APARTADO B: Dadas dos asignaturas entre las que se está dudando, aconsejar al alumno cuál debería elegir y por qué.

(defmodule modulo_asignaturas (export deftemplate ?ALL) )

   ;PREGUNTAS PREDEFINIDAS PARA TODAS LAS ASIGNATURAS PARA LAS QUE VAMOS A RESOLVER DUDAS
   (deffacts Preguntas
      (mayor_hipotesis a 0)
      (Pregunta1 MP (str-cat "Has utilizado alguna vez la IDE de NetBeans? (si/no)"))
      (Pregunta2 MP (str-cat "Manejas el concepto de clases en programacion? (si/no)"))
      (Pregunta3 MP (str-cat "Tienes un nivel medio por lo menos en C++? (si/no)"))
      (Pregunta4 MP (str-cat "Sabes como funcionan los punteros? (si/no)"))

      (Pregunta1 FBD (str-cat "Te gustan las bases de datos? (si/no)"))
      (Pregunta2 FBD (str-cat "Sabes lo que es el paso a tablas? (si/no)"))
      (Pregunta3 FBD (str-cat "Has hecho alguna vez esquemas Entidad/Relacion? (si/no)"))
      (Pregunta4 FBD (str-cat "Has utilizado la herramienta MySQL alguna vez o alguna similar? (si/no)"))

      (Pregunta1 IES (str-cat "Te habria gustado combinar esta carrera con ADE? (si/no)"))
      (Pregunta2 IES (str-cat "Te consideras bueno memorizando contenido teorico? (si/no)"))
      (Pregunta3 IES (str-cat "Te gusta hacer exposiciones en grupo? (si/no)"))
      (Pregunta4 IES (str-cat "Te sientes comodo con actividades que requieres participacion en clase? (si/no)"))

      (Pregunta1 IA (str-cat "Has utilizado alguna vez el entorno CodeBlocks? (si/no)"))
      (Pregunta2 IA (str-cat "Has oido hablar antes del algoritmo A*? (si/no)"))
      (Pregunta3 IA (str-cat "Conoces la diferencia entre un agente deliberativo y reactivo? (si/no)"))
      (Pregunta4 IA (str-cat "Te interesa especialmente el mundo de la inteligencia artificial? (si/no)"))

      (Pregunta1 TOC (str-cat "Has cursado fisica de circuitos antes de esta asignatura? (si/no)"))
      (Pregunta2 TOC (str-cat "Te gustan las practicas de laboratorio sobre circuitos? (si/no)"))
      (Pregunta3 TOC (str-cat "Conoces el concepto de mapa de Karnaugh? (si/no)"))
      (Pregunta4 TOC (str-cat "Eres habil con la representacion en bits de complementos y sesgos? (si/no)"))

      (Pregunta1 AC (str-cat "Te gusta la parte fisica del ordenador? (si/no)"))
      (Pregunta2 AC (str-cat "Conoces la ley de Amdahl? (si/no)"))
      (Pregunta3 AC (str-cat "Sabes la diferencia entre el protocolo MSI y MESI? (si/no)"))
      (Pregunta4 AC (str-cat "Has trabajado alguna vez con OpenMP? (si/no)"))

      (Pregunta1 CAL (str-cat "Conoces el polinomio de Taylor? (si/no)"))
      (Pregunta2 CAL (str-cat "Conoces la herramienta de Maxima? (si/no)"))
      (Pregunta3 CAL (str-cat "Eres bueno intengrando y derivando? (si/no)"))
      (Pregunta4 CAL (str-cat "Sabes lo que son los numeros imaginarios y como usarlos? (si/no)"))

      (Pregunta1 EC (str-cat "Has trabajado en ensamblador alguna vez? (si/no)"))
      (Pregunta2 EC (str-cat "Has hecho uso de Arduinos alguna vez? (si/no)"))
      (Pregunta3 EC (str-cat "Tienes buen manejo del lenguaje C? (si/no)"))
      (Pregunta4 EC (str-cat "Te genera interes el interior de un procesador y sus elementos? (si/no)"))

      (Pregunta1 PDOO (str-cat "Has programado alguna vez en Java? (si/no)"))
      (Pregunta2 PDOO (str-cat "Te gusta Ruby? (si/no)"))
      (Pregunta3 PDOO (str-cat "Sabes lo que es el patron Modelo-Vista-Controlador? (si/no)"))
      (Pregunta4 PDOO (str-cat "Has hecho alguna vez un UML? (si/no)"))

      (Pregunta1 ES (str-cat "Has impartido alguna asignatura de estadisticas o matematicas antes? (si/no)"))
      (Pregunta2 ES (str-cat "Conoces o has usado la herramienta R? (si/no)"))
      (Pregunta3 ES (str-cat "Entiendes los problemas sobre Hipotesis Nulas? (si/no)"))
      (Pregunta4 ES (str-cat "Eres bueno entendiendo y manejando formulas? (si/no)"))

   )

(defmodule primeras_preguntas (import modulo_asignaturas deftemplate ?ALL))


   (deffacts asignaturas
      (asignatura MP) 
      (asignatura IES)
      (asignatura TOC)
      (asignatura CAL)
      (asignatura ES)       
      (asignatura AC)
      (asignatura EC)
      (asignatura FBD)
      (asignatura PDOO)
      (asignatura IA)
   )

   ;;;;;;;;;;;PREGUNTAMOS POR LA PRIMERA ASIGNATURA POR LA QUE TIENE DUDAS
   (defrule primera_duda
   =>
      (printout t "Sobre cual de las siguientes asignaturas tienes dudas?:" crlf)
      (assert (pedir_primera_duda))
   )

   ;;;;;;;;;;;PARTE EN LA MUESTRA CADA ASIGNATURA 
   (defrule mostramos_primera_duda
      (pedir_primera_duda)
      (asignatura ?f)
   =>
      (printout t "        " ?f crlf)
   )

   ;;;;;;;;;;;PARTE EN LA QUE SE INTRODUCE LA PRIMERA DUDA 
   (defrule introduce_primera_duda
      ?g <- (pedir_primera_duda)
   =>
      (assert (primera_duda (read)))
      (assert (pedir_segunda_duda))
      (retract ?g)
   )


   ;;;;;;;;;;COMPRUEBA QUE LA ASIGNATURA ES CORRECTA
   (defrule comprobacion_primera_duda
      ?g <- (primera_duda ?f)
      ?w <- (pedir_segunda_duda)
      (test (and (neq ?f MP) (neq ?f IES) (neq ?f TOC) (neq ?f CAL) (neq ?f ES) (neq ?f AC) (neq ?f EC) (neq ?f FBD) (neq ?f PDOO) (neq ?f IA)))
   =>
      (printout t ?f " no es una opcion correcta, introduce otra"crlf)
      (assert (pedir_primera_duda))
      (retract ?g)
      (retract ?w)
   )

   ;;;;;;;;;;;PARTE EN LA QUE PREGUNTA CUAL ES LA SEGUNDA DUDA
   (defrule segunda_duda
      (pedir_segunda_duda)
   =>
      (printout t crlf "Cual es la otra asignatura con la que dudas?:" crlf )
      (assert (mostrar_segunda))
   )


   ;;;;;;;;;;;PARTE EN LA MUESTRA CADA ASIGNATURA QUE NO HAYA SELECCIONADO AUN
   (defrule mostramos_segunda_duda
      (asignatura ?f)
      (primera_duda ?l)
      (test (neq ?l ?f))
      (mostrar_segunda)
   =>
      (printout t "        " ?f crlf)
   )

   ;;;;;;;;;;;PARTE EN LA QUE SE INTRODUCE LA SEGUNDA ASIGNATURA CON LA QUE TIENE DUDAS
   (defrule introduce_segunda_duda
      ?g <- (pedir_segunda_duda)
      ?w <- (mostrar_segunda)
   =>
      (assert (segunda_duda (read)))
      (retract ?g)
      (retract ?w)
      (assert (comprueba_segunda))
   )

   ;;;;;;;;;;COMPRUEBA QUE LA ASIGNATURA ES CORRECTA
   (defrule comprobacion_asignatura_segundo
      ?g <- (segunda_duda ?f)
      ?w <- (comprueba_segunda)
      (test (and (neq ?f MP) (neq ?f IES) (neq ?f TOC) (neq ?f CAL) (neq ?f ES) (neq ?f AC) (neq ?f EC) (neq ?f FBD) (neq ?f PDOO) (neq ?f IA)))
   =>
      (printout t ?f " no es una opcion correcta, introduce otra"crlf)
      (assert (pedir_segunda_duda))
      (retract ?g)
      (retract ?w)
   )

   ;;;;;;;;;;COMPRUEBA QUE LA ASIGNATURA ES CORRECTA
   (defrule comprobacion_asignatura_segundo_diferente_a_primera
      ?g <- (segunda_duda ?f)
      ?w <- (comprueba_segunda)
      (test (or (eq ?f MP) (eq ?f IES) (eq ?f TOC) (eq ?f CAL) (eq ?f ES) (eq ?f AC) (eq ?f EC) (eq ?f FBD) (eq ?f PDOO) (eq ?f IA)))
      (primera_duda ?p)
      (test (eq ?p ?f))
   =>
      (printout t "No puede seleccionar la misma que eligio como primera opcion, introduce otra"crlf)
      (assert (pedir_segunda_duda))
      (retract ?g)
      (retract ?w)
   )


   ;REALIZAMOS LA PRIMERA PREGUNTA DE LA PRIMERA ASIGNATURA CON LA QUE TIENE DUDAS   
   (defrule preguntamos_primera_primera
      (primera_duda ?p)
      (Pregunta1 ?p ?g)
   =>
      (printout t ?g crlf)
      (assert (respuesta respuesta1 ?p (read)))
      (assert (comprobacion respuesta1))
   )

   ;REALIZAMOS LA SEGUNDA PREGUNTA DE LA PRIMERA ASIGNATURA CON LA QUE TIENE DUDAS   
   (defrule preguntamos_segunda_primera
      (primera_duda ?p)
      (Pregunta2 ?p ?g)
   =>
      (printout t ?g crlf)
      (assert (respuesta respuesta2 ?p (read)))
      (assert (comprobacion respuesta2))
   )

   ;REALIZAMOS LA TERCERA PREGUNTA DE LA PRIMERA ASIGNATURA CON LA QUE TIENE DUDAS   
   (defrule preguntamos_tercera_primera
      (primera_duda ?p)
      (Pregunta3 ?p ?g)
   =>
      (printout t ?g crlf)
      (assert (respuesta respuesta3 ?p (read)))
      (assert (comprobacion respuesta3))
   )

   ;REALIZAMOS LA CUARTA PREGUNTA DE LA PRIMERA ASIGNATURA CON LA QUE TIENE DUDAS   
   (defrule preguntamos_cuarta_primera
      (primera_duda ?p)
      (Pregunta4 ?p ?g)
   =>
      (printout t ?g crlf)
      (assert (respuesta respuesta4 ?p (read)))
      (assert (comprobacion respuesta4))
   )

  
   ;REALIZAMOS LA PRIMERA PREGUNTA DE LA SEGUNDA ASIGNATURA CON LA QUE TIENE DUDAS   
   (defrule preguntamos_primera_segunda
      (segunda_duda ?p)
      (Pregunta1 ?p ?g)
   =>
      (printout t ?g crlf)
      (assert (respuesta respuesta1 ?p (read)))
      (assert (comprobacion respuesta1))
   )

   ;REALIZAMOS LA SEGUNDA PREGUNTA DE LA SEGUNDA ASIGNATURA CON LA QUE TIENE DUDAS   
   (defrule preguntamos_segunda_segunda
      (segunda_duda ?p)
      (Pregunta2 ?p ?g)
   =>
      (printout t ?g crlf)
      (assert (respuesta respuesta2 ?p (read)))
      (assert (comprobacion respuesta2))
   )

   ;REALIZAMOS LA TERCERA PREGUNTA DE LA SEGUNDA ASIGNATURA CON LA QUE TIENE DUDAS   
   (defrule preguntamos_tercera_segunda
      (segunda_duda ?p)
      (Pregunta3 ?p ?g)
   =>
      (printout t ?g crlf)
      (assert (respuesta respuesta3 ?p (read)))
      (assert (comprobacion respuesta3))
   )

   ;REALIZAMOS LA CUARTA PREGUNTA DE LA SEGUNDA ASIGNATURA CON LA QUE TIENE DUDAS   
   (defrule preguntamos_cuarta_segunda
      (segunda_duda ?p)
      (Pregunta4 ?p ?g)
   =>
      (printout t ?g crlf)
      (assert (respuesta respuesta4 ?p (read)))
      (assert (comprobacion respuesta4))
   )



  ;COMPROBACION DE RESPUESTAS INCORRECTAS. DEBEN SER 'si' O 'no'
   (defrule comprobacion_respuestas_incorrectas
     (declare (salience 5))
     ?borrar1 <- (comprobacion ?l)
     ?borrar <- (respuesta ?l ?p ?x)
     (test (and 
               (neq ?x si) 
               (neq ?x no) 
            )
     )
   =>
     (retract ?borrar)
     (retract ?borrar1)
     (printout t "La respuesta no es correcta, tienes que introducir si o no " crlf)
     (assert (respuesta ?l ?p (read)))
     (assert (comprobacion ?l))
   )

   ;COMPROBACION DE RESPUESTAS CORRECTAS. SON 'si' O 'no'
   (defrule comprobacion_respuestas_correctas
     (declare (salience 5))
     ?borrar1 <- (comprobacion ?l)
     (respuesta ?l ?p ?x)
     (test (or 
               (eq ?x si) 
               (eq ?x no) 
            )
     )
   =>
     (retract ?borrar1)
   )



   ;;; convertimos cada respuesta en un  factor de certeza con su certeza propia
   (defrule certeza_evidencias
      (respuesta ?res ?asig ?sino)
      =>
      (assert (FactorCerteza ?res ?asig ?sino 1)) 
   )


   (deffunction encadenado (?fc_antecedente ?fc_regla)
   
      (if (> ?fc_antecedente 0) then
      
         (bind ?rv (* ?fc_antecedente ?fc_regla)) 
      
      else 
      
         (bind ?rv 0) 
      
      )

   ?rv)



   (defrule COMBINADA1
      (FactorCerteza respuesta1 ES si ?f1)
      (FactorCerteza respuesta3 CAL si ?f2)
      (test (and (> ?f1 0) (> ?f2 0))) 
      =>
      (assert (FactorCerteza CAL si (encadenado (* ?f1 ?f2) 0.8)))
      (bind ?expl (str-cat "   POSITIVO: Si has dado matematicas, ademas de aumentar la certeza de estadistica, aumenta la de calculo"))
      (assert (explicacion CAL ?expl))
   )

   (defrule COMBINADA2
      (FactorCerteza respuesta4 EC si ?f1)
      (FactorCerteza respuesta1 AC si ?f2)
      (test (and (> ?f1 0) (> ?f2 0))) 
      =>
      (assert (FactorCerteza EC si (encadenado (* ?f1 ?f2) 0.7)))
      (bind ?expl (str-cat "   POSITIVO: Si ademas te interesa en general el interior del pc tambien te gustara la asignatura de EC en gran medida"))
      (assert (explicacion EC ?expl))
   )

   (defrule COMBINADA3
      (FactorCerteza respuesta4 PDOO si ?f1)
      (FactorCerteza respuesta3 FBD si ?f2)
      (test (and (> ?f1 0) (> ?f2 0))) 
      =>
      (assert (FactorCerteza PDOO si (encadenado (* ?f1 ?f2) 0.8)))
      (bind ?expl (str-cat "   POSITIVO: Si ya conoces las UML y has hecho esquemas en FBD, la esquematizacion de la teoria de PDOO te va a resultar facil"))
      (assert (explicacion PDOO ?expl))
   )

   (defrule COMBINADA4
      (FactorCerteza respuesta2 IES no ?f1)
      (FactorCerteza respuesta4 ES no ?f2)
      (test (and (> ?f1 0) (> ?f2 0))) 
      =>
      (assert (FactorCerteza IES si (encadenado (* ?f1 ?f2) -0.3)))
      (bind ?expl (str-cat "   NEGATIVO: Si no se te dan bien memorizar formulas ni la teoria, no es muy recomendable que te metas en IES "))
      (assert (explicacion IES ?expl))
   )


   ; R1: SI HAS USADO CODEBLOCKS -> VAS PARA IA CON 0.7
   (defrule R1
      (FactorCerteza respuesta1 IA si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IA si 0.7))
      (bind ?expl (str-cat "   POSITIVO: En IA la practica principal es en CodeBlocks y tener manejo sobre el te va a dar facilidades"))
      (assert (explicacion IA ?expl))
   )

   ; R2: SI CONOCES EL ALGORITMO DE A* -> VAS PARA IA CON 0.4
   (defrule R2
      (FactorCerteza respuesta2 IA si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IA si 0.4))
      (bind ?expl (str-cat "   POSITIVO: El algoritmo A* es la base de la segunda practica, por lo que conocerlo hara que disfrutes la asignatura"))
      (assert (explicacion IA ?expl))
   )

   ; R3: SI NO CONOCES LA DIFERENCIA ENTRE LOS TIPOS DE AGENTES -> VAS PARA IA CON -0.2
   (defrule R3
      (FactorCerteza respuesta3 IA no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IA si -0.2))
      (bind ?expl (str-cat "   NEGATIVO: Se reduce la certeza de que escojas IA porque no tienes mucha base sobre agentes"))
      (assert (explicacion IA ?expl))
   )

   ; R3.2: SI CONOCES LA DIFERENCIA ENTRE LOS TIPOS DE AGENTES -> VAS PARA IA CON 0.2
   (defrule R3_2
      (FactorCerteza respuesta3 IA si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IA si 0.2))
      (bind ?expl (str-cat "   POSITIVO: Aumenta la certeza de que escojas IA porque tienes base sobre agentes"))
      (assert (explicacion IA ?expl))
   )

   ; R4: SI NO TE INTERESA EL MUNDO DE LA INTELIGENCIA ARTIFICIAL -> VAS PARA IA CON -0.6
   (defrule R4
      (FactorCerteza respuesta4 IA no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IA si -0.6))
      (bind ?expl (str-cat "   NEGATIVO: Si la inteligencia artificial no es un campo que te interese mucho no es buena idea cursar una asignatura previa a esa asignatura"))
      (assert (explicacion IA ?expl))
   )


   ; R5: SI HAS DADO PREVIAMENTE ESTADISTICAS O MATEMATICAS -> VAS PARA ESTADISTICAS CON 0.7
   (defrule R5
      (FactorCerteza respuesta1 ES si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza ES si 0.7))
      (bind ?expl (str-cat "   POSITIVO: Haber cursado matematicas o estadisticas va a hacer que esta asignatura te sea bastante asequible"))
      (assert (explicacion ES ?expl))
   )


   ; R6: SI NO HAS DADO NUNCA R -> VAS PARA ESTADISTICAS CON -0.4
   (defrule R6
      (FactorCerteza respuesta2 ES no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza ES si -0.4))
      (bind ?expl (str-cat "   NEGATIVO: R es la herramienta en la que se basan las practicas, por lo que si no tienes base ninguna en esta herramienta te puede costar un poco mas"))
      (assert (explicacion ES ?expl))
   )

   ; R62: SI HAS DADO NUNCA R -> VAS PARA ESTADISTICAS CON 0.7
   (defrule R62
      (FactorCerteza respuesta2 ES si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza ES si 0.7))
      (bind ?expl (str-cat "   POSITIVO: R es la herramienta en la que se basan las practicas, por lo que si tienes base en esta herramienta te puede costar menos"))
      (assert (explicacion ES ?expl))
   )

   ; R7: SI ENTIENDES CON FACILIDAD LOS PROBLEMAS DE LAS HIPOTESIS NULAS -> VAS PARA ESTADISTICAS CON 0.8
   (defrule R7
      (FactorCerteza respuesta3 ES si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza ES si 0.8))
      (bind ?expl (str-cat "   POSITIVO: Uno de los problemas mas complejos que se imparten en esa asignatura son los de hipotesis nulas y entenderla desde antes te va a ahorrar bastantes problemas"))
      (assert (explicacion ES ?expl))
   )

   ; R8: SI NO ERES MUY HABIL MEMORIZANDO FORMULAS -> VAS PARA ESTADISTICAS CON -0.5
   (defrule R8
      (FactorCerteza respuesta4 ES no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza ES si -0.5))
      (bind ?expl (str-cat "   NEGATIVO: La asignatura contiene muchas formulas y que estas no se te den bien puede ser un indicio de que no sea recomendable escogerla"))
      (assert (explicacion ES ?expl))
   )

   ; R9: SI SE TE DA BIEN JAVA -> VAS PARA PDOO CON 0.7
   (defrule R9
      (FactorCerteza respuesta1 PDOO si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza PDOO si 0.7))
      (bind ?expl (str-cat "   POSITIVO: Las practicas se basan en dos lenguajes con igual importancia, y uno de ellos es Java"))
      (assert (explicacion PDOO ?expl))
   )

   ; R10: SI SE TE DA BIEN RUBY -> VAS PARA PDOO CON 0.9
   (defrule R10
      (FactorCerteza respuesta2 PDOO si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza PDOO si 0.9))
      (bind ?expl (str-cat "   POSITIVO: Las practicas se basan en dos lenguajes con igual importancia, y uno de ellos es Ruby"))
      (assert (explicacion PDOO ?expl))
   )

   ; R11: SI NO CONOCES EL PATRON MVC -> VAS PARA PDOO CON -0.2
   (defrule R11
      (FactorCerteza respuesta3 PDOO no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza PDOO si -0.2))
      (bind ?expl (str-cat "   NEGATIVO: Porque es un patron clave en la practica que habra que saber controlar, por lo que cuanto mas conozcas sobre el mejor desempeño tendras en PDOO"))
      (assert (explicacion PDOO ?expl))
   )

   ; R112: SI CONOCES EL PATRON MVC -> VAS PARA PDOO CON 0.5
   (defrule R112
      (FactorCerteza respuesta3 PDOO si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza PDOO si 0.5))
      (bind ?expl (str-cat "   POSITIVO: Porque es un patron clave en la practica que habra que saber controlar, por lo que cuanto mas conozcas sobre el mejor desempeño tendras en PDOO"))
      (assert (explicacion PDOO ?expl))
   )


   ; R12: SI NO TIENES BASE EN LA REALIZACION DE UMLS -> VAS PARA PDOO CON -0.3
   (defrule R12
      (FactorCerteza respuesta4 PDOO no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza PDOO si -0.3))
      (bind ?expl (str-cat "   NEGATIVO: Gran parte de la teoria se basa en la comprension y realizacion de esquemas como este, por lo que no tener nada de base en este sentido hace la asignatura no recomendable"))
      (assert (explicacion PDOO ?expl))
   )

   ; R13: SI SABES TRABAJAR CON ENSAMBLADOR -> VAS PARA EC CON 0.9
   (defrule R13
      (FactorCerteza respuesta1 EC si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza EC si 0.9))
      (bind ?expl (str-cat "   POSITIVO: El trabajo en ensamblador es fundamental en esta asignatura, manejarlo antes de cogerla es una ventaja que hay que tener en cuenta"))
      (assert (explicacion EC ?expl))
   )

   ; R14: SI NO HAS TRABAJADO CON ARDUINOS NADA -> VAS PARA EC CON -0.5
   (defrule R14
      (FactorCerteza respuesta2 EC no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza EC si -0.5))
      (bind ?expl (str-cat "   NEGATIVO: Una parte no muy prescindible es relacionada con el trabajo con arduinos, no saber nada de estos es una desventaja"))
      (assert (explicacion EC ?expl))
   )

   ; R142: SI HAS TRABAJADO CON ARDUINOS -> VAS PARA EC CON 0.6
   (defrule R142
      (FactorCerteza respuesta2 EC si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza EC si 0.6))
      (bind ?expl (str-cat "   POSITIVO: Una parte imprescindible es relacionada con el trabajo con arduinos, no saber nada de estos es una desventaja"))
      (assert (explicacion EC ?expl))
   )

   ; R15: SI SABES LENGUAJE QUE SE COMBINA CON ENSAMBLADOR -> VAS PARA EC CON 0.4
   (defrule R15
      (FactorCerteza respuesta3 EC si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza EC si 0.4))
      (bind ?expl (str-cat "   POSITIVO: C es un lenguaje que se utiliza combinado con ensamblador en gran parte de las practicas"))
      (assert (explicacion EC ?expl))
   )

   ; R16: SI NO TE INTERESA CONOCER LOS COMPONENTES DE UN PROCESADOR -> VAS PARA EC CON -0.6
   (defrule R16
      (FactorCerteza respuesta4 EC no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza EC si -0.6))
      (bind ?expl (str-cat "   NEGATIVO: Si no te interesa conocer el interior de un procesador esta asignatura no es la apropiada"))
      (assert (explicacion EC ?expl))
   )
   
   ; R17: SI TE GUSTAN LAS BASES DE DATOS -> VAS PARA FBD CON 0.8
   (defrule R17
      (FactorCerteza respuesta1 FBD si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza FBD si 0.8))
      (bind ?expl (str-cat "   POSITIVO: Si te gustan las bases de datos esta es tu asignatura ideal a priori"))
      (assert (explicacion FBD ?expl))
   )

   ; R18: SI HAS PRACTICADO EL PASO A TABLAS -> VAS PARA FBD CON 0.6
   (defrule R18
      (FactorCerteza respuesta2 FBD si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza FBD si 0.6))
      (bind ?expl (str-cat "   POSITIVO: El paso a tablas es parte de la asignatura"))
      (assert (explicacion FBD ?expl))
   )

   ; R19: SI NO HAS DADO NUNCA EL ESQUEMA DE ENTIDAD/RELACION -> VAS PARA FBD CON -0.6
   (defrule R19
      (FactorCerteza respuesta3 FBD no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza FBD si -0.6))
      (bind ?expl (str-cat "   NEGATIVO: El entidad/relacion es un esquema previo a la implementacion en SQL, por lo que no saber hacerlo podria complicar los pasos siguientes en la practica"))
      (assert (explicacion FBD ?expl))
   )

   ; R192: SI HAS DADO EL ESQUEMA DE ENTIDAD/RELACION -> VAS PARA FBD CON 0.6
   (defrule R192
      (FactorCerteza respuesta3 FBD si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza FBD si 0.6))
      (bind ?expl (str-cat "   POSITIVO: El entidad/relacion es un esquema previo a la implementacion en SQL, por lo que saber hacerlo podria facilitar los pasos siguientes en la practica"))
      (assert (explicacion FBD ?expl))
   )

   ; R20: SI NO HAS DADO NUNCA MYSQL -> VAS PARA FBD CON -0.5
   (defrule R20
      (FactorCerteza respuesta4 FBD no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza FBD si -0.5))
      (bind ?expl (str-cat "   NEGATIVO: No haber dado nunca MySQL solamente va a hacer de las practicas una contra de esta asignatura"))
      (assert (explicacion FBD ?expl))
   )

   ; R21: SI TE GUSTA LA ARQUITECTURA Y PARTE FISICA DEL ORDENADOR -> VAS PARA AC CON 0.7
   (defrule R21
      (FactorCerteza respuesta1 AC si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza AC si 0.7))
      (bind ?expl (str-cat "   POSITIVO: Una parte de la teoria de la asignatura esta basada en componentes del ordenador"))
      (assert (explicacion AC ?expl))
   )

   ; R22: SI NO CONOCES LA LEY DE AMDAHL -> VAS PARA AC CON -0.4
   (defrule R22
      (FactorCerteza respuesta2 AC no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza AC si -0.4))
      (bind ?expl (str-cat "   NEGATIVO: La ley de Amdahl es uno de los principios que se estudian en teoria y no conocerla influye de forma negativa pero no excesivamente"))
      (assert (explicacion AC ?expl))
   )

   ; R222: SI CONOCES LA LEY DE AMDAHL -> VAS PARA AC CON 0.5
   (defrule R222
      (FactorCerteza respuesta2 AC si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza AC si 0.5))
      (bind ?expl (str-cat "   POSITIVO: La ley de Amdahl es uno de los principios que se estudian en teoria y conocerla influye de forma positiva"))
      (assert (explicacion AC ?expl))
   )

   ; R23: SI CONOCES MSI Y MESI -> VAS PARA AC CON 0.8
   (defrule R23
      (FactorCerteza respuesta3 AC si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza AC si 0.8))
      (bind ?expl (str-cat "   POSITIVO: Conocer los protocolos MSI y MESI te ahorra las partes mas complejas de las relaciones de ejercicios, por lo que no habria que desaprovechar la oportunidad"))
      (assert (explicacion AC ?expl))
   )  

   ; R24: SI NO HAS TRABAJADO NUNCA CON OPENMP -> VAS PARA AC CON -0.3
   (defrule R24
      (FactorCerteza respuesta4 AC no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza AC si -0.3))
      (bind ?expl (str-cat "   NEGATIVO: OpenMP es una herramienta que se utiliza para la parte de los cerrojos y haber trabajado con ella habria sido una gran ventaja"))
      (assert (explicacion AC ?expl))
   )  

   ; R25: SI NO CONOCES EL POLINOMIO DE TAYLOR -> VAS PARA CAL CON -0.3
   (defrule R25
      (FactorCerteza respuesta1 CAL no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza CAL si -0.3))
      (bind ?expl (str-cat "   NEGATIVO: Para el segundo parcial es crucial saber manejar el polinomio de Taylor, por lo que no conocerlo no ayuda precisamente"))
      (assert (explicacion CAL ?expl))
   )

   ; R252: SI CONOCES EL POLINOMIO DE TAYLOR -> VAS PARA CAL CON 0.8
   (defrule R252
      (FactorCerteza respuesta1 CAL si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza CAL si 0.8))
      (bind ?expl (str-cat "   POSITIVO: Para el segundo parcial es crucial saber manejar el polinomio de Taylor, por lo que conocerlo previamente ayuda bastante"))
      (assert (explicacion CAL ?expl))
   )

   ; R26: SI SE TE DA BIEN MAXIMA -> VAS PARA CAL CON 0.9
   (defrule R26
      (FactorCerteza respuesta2 CAL si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza CAL si 0.9))
      (bind ?expl (str-cat "   POSITIVO: Las practicas se realizan usando la herramienta de Maxima, por lo que tener conocimientos previos te va a poner la practica en bandeja"))
      (assert (explicacion CAL ?expl))
   )

   ; R27: SI SE TE DA BIEN INTEGRAR -> VAS PARA CAL CON 0.7
   (defrule R27
      (FactorCerteza respuesta3 CAL si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza CAL si 0.7))
      (bind ?expl (str-cat "   POSITIVO: Que se te den bien las integrales es buena señal para que tu asignatura sea calculo"))
      (assert (explicacion CAL ?expl))
   )  

   ; R28: SI NO CONOCES EL CONCEPTO DE NUMERO IMAGINARIO -> VAS PARA CAL CON -0.5
   (defrule R28
      (FactorCerteza respuesta4 CAL no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza CAL si -0.5))
      (bind ?expl (str-cat "   NEGATIVO: En ciertas partes se necesita conocer el uso de numero imaginarios porque se dan por aprendidos de cursos previos"))
      (assert (explicacion CAL ?expl))
   ) 

   ; R29: SI YA HAS TRABAJADO CON NETBEANS -> VAS PARA MP CON 0.7
   (defrule R29
      (FactorCerteza respuesta1 MP si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza MP si 0.7))
      (bind ?expl (str-cat "   POSITIVO: Toda la practica de esta asignatura se lleva a cabo en NetBeans por lo que es un gran punto a favor tener experiencia"))
      (assert (explicacion MP ?expl))
   ) 

   ; R30: SI NO SABES LO QUE SON LAS CLASES (OBJETOS) -> VAS PARA MP CON -0.4
   (defrule R30
      (FactorCerteza respuesta2 MP no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza MP si -0.4))
      (bind ?expl (str-cat "   NEGATIVO: En esta asignatura se introduce el concepto de clase que, si no se conoce con anterioridad puede ser dificil de digerir"))
      (assert (explicacion MP ?expl))
   ) 

   ; R302: SI SABES LO QUE SON LAS CLASES (OBJETOS) -> VAS PARA MP CON 0.6
   (defrule R302
      (FactorCerteza respuesta2 MP si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza MP si 0.6))
      (bind ?expl (str-cat "   POSITIVO: En esta asignatura se introduce el concepto de clase que, si se conoce con anterioridad puede evitar la incertidumbre del principio"))
      (assert (explicacion MP ?expl))
   ) 

   ; R31: SI NO HAS DADO NADA DE C++ -> VAS PARA MP CON -0.7
   (defrule R31
      (FactorCerteza respuesta3 MP no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza MP si -0.7))
      (bind ?expl (str-cat "   NEGATIVO: Existen asignaturas previas a esta en las que se imparte C++ por lo que no conocer el lenguaje puede dejarte fuera del ritmo de la asignatura"))
      (assert (explicacion MP ?expl))
   )

   ; R32: SI TIENES UN BUEN MANEJO DE PUNTEROS -> VAS PARA MP CON 0.8
   (defrule R32
      (FactorCerteza respuesta4 MP si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza MP si 0.8))
      (bind ?expl (str-cat "   POSITIVO: Para la mayoria de estudiantes es la parte mas abstracta y tener un buen manejo de esta te asegura un buen futuro en la asignatura"))
      (assert (explicacion MP ?expl))
   )

   ; R33: SI TE HABRIA GUSTADO HABER CURSADO ADE -> VAS PARA IES CON 0.8
   (defrule R33
      (FactorCerteza respuesta1 IES si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IES si 0.8))
      (bind ?expl (str-cat "   POSITIVO: El contenido de esta asignatura esta enfocado a las empresas por lo que te gustaria cursarla"))
      (assert (explicacion IES ?expl))
   ) 

   ; R34: SI NO ERES ESPECIALMENTE HABIL MEMORIZANDO CONTENIDO -> VAS PARA IES CON -0.7
   (defrule R34
      (FactorCerteza respuesta2 IES no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IES si -0.7))
      (bind ?expl (str-cat "   NEGATIVO: En esta asignatura los examenes son de desarrollo de teoria que en muchos casos hay que memorizar"))
      (assert (explicacion IES ?expl))
   ) 

   ; R342: SI ERES ESPECIALMENTE HABIL MEMORIZANDO CONTENIDO -> VAS PARA IES CON 0.6
   (defrule R342
      (FactorCerteza respuesta2 IES si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IES si 0.6))
      (bind ?expl (str-cat "   POSITIVO: En esta asignatura los examenes son de desarrollo de teoria que en muchos casos hay que memorizar"))
      (assert (explicacion IES ?expl))
   ) 

   ; R35: SI TE GUSTAN LAS EXPOSICIONES EN GRUPO -> VAS PARA IES CON 0.6
   (defrule R35
      (FactorCerteza respuesta3 IES si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IES si 0.6))
      (bind ?expl (str-cat "   POSITIVO: A lo largo de la asignatura, cada parte practica concluye con una exposicion en grupo"))
      (assert (explicacion IES ?expl))
   )

   ; R36: SI NO TE GUSTA PARTICIPAR EN PUBLICO -> VAS PARA IES CON -0.4
   (defrule R36
      (FactorCerteza respuesta4 IES no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza IES si -0.4))
      (bind ?expl (str-cat "   NEGATIVO: Todos los alumnos terminan siendo participativos en clase por las diferentes actividades que se hacen"))
      (assert (explicacion IES ?expl))
   )

   ; R37: SI YA HAS CURSADO FISICA DE CIRCUITOS -> VAS PARA TOC CON 0.8
   (defrule R37
      (FactorCerteza respuesta1 TOC si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza TOC si 0.8))
      (bind ?expl (str-cat "   POSITIVO: Gran parte de la asignatura es sobre circuitos y se puede considerar una continuacion de FFT"))
      (assert (explicacion TOC ?expl))
   ) 

   ; R38: SI NO TE GUSTAN LAS PRACTICAS DE LABORATORIO SOBRE CIRCUITOS -> VAS PARA TOC CON -0.6
   (defrule R38
      (FactorCerteza respuesta2 TOC no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza TOC si -0.6))
      (bind ?expl (str-cat "   NEGATIVO: La mayoria de practicas son en el laboratorio manipulando circuitos y cables"))
      (assert (explicacion TOC ?expl))
   ) 

   ; R382: SI TE GUSTAN LAS PRACTICAS DE LABORATORIO SOBRE CIRCUITOS -> VAS PARA TOC CON 0.6
   (defrule R382
      (FactorCerteza respuesta2 TOC si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza TOC si 0.6))
      (bind ?expl (str-cat "   POSITIVO: La mayoria de practicas son en el laboratorio manipulando circuitos y cables"))
      (assert (explicacion TOC ?expl))
   ) 

   ; R39: SI TE ERES BUENOS CON MAPAS DE KARNAUGH -> VAS PARA TOC CON 0.6
   (defrule R39
      (FactorCerteza respuesta3 TOC si ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza TOC si 0.6))
      (bind ?expl (str-cat "   POSITIVO: Los mapas de Karnaugh son de gran importancia al comienzo de los seminarios"))
      (assert (explicacion TOC ?expl))
   )

   ; R40: SI NO ERES BUENO CON LA REPRESENTACION EN BITS -> VAS PARA TOC CON -0.5
   (defrule R40
      (FactorCerteza respuesta4 TOC no ?f1)
      (test (> ?f1 0)) 
      =>
      (assert (FactorCerteza TOC si -0.5))
      (bind ?expl (str-cat "   NEGATIVO: El complementario y el sesgo son parte fundamental de los seminarios obligatorios"))
      (assert (explicacion TOC ?expl))
   )

 

   (deffunction combinacion (?fc1 ?fc2)
   
      (if (and (> ?fc1 0) (> ?fc2 0) ) then
      
         (bind ?rv (- (+ ?fc1 ?fc2) (* ?fc1 ?fc2) ) )

      else 
      
         (if (and (< ?fc1 0) (< ?fc2 0) ) then
      
            (bind ?rv (+ (+ ?fc1 ?fc2) (* ?fc1 ?fc2) ) )
      
         else
      
            (bind ?rv (/ (+ ?fc1 ?fc2) (- 1 (min (abs ?fc1) (abs ?fc2))) ))
      
         )
      
      )

   ?rv)

   ;;;;;; Combinar misma deduccion por distintos caminos
   (defrule combinar
      (declare (salience 1))
      ?f <- (FactorCerteza ?h ?r ?fc1)
      ?g <- (FactorCerteza ?h ?r ?fc2)
      (test (neq ?fc1 ?fc2))
      =>
      (retract ?f ?g)
      (assert (FactorCerteza ?h ?r (combinacion ?fc1 ?fc2))) 
   )

   (defrule combinar_signo
   (declare (salience 2))
      (FactorCerteza ?h si ?fc1)
      (FactorCerteza ?h no ?fc2)
      =>
      (assert (Certeza ?h (- ?fc1 ?fc2))) 
   )


   ;;;NOS QUEDAMOS SOLAMENTE CON LOS FACTORESCERTEZAS QUE SEAN DE ASIGNATURAS
  (defrule factorcerteza_problema
   (declare (salience -9996))
    (FactorCerteza ?c ?x ?f)
    (test (or (eq ?c IA) (eq ?c MP) (eq ?c CAL) (eq ?c TOC) (eq ?c FBD) (eq ?c PDOO) (eq ?c ES) (eq ?c AC) (eq ?c EC) (eq ?c IES) ))

  =>
    (assert (a_evaluar ?c ?f))
  )




  (defrule factorcerteza_problema_mayor
   (declare (salience -9997))
    ?h <- (mayor_hipotesis ?x ?y)
    (a_evaluar ?c ?f)
    (test (> ?f ?y))
  =>
   (retract ?h)
    (assert (mayor_hipotesis ?c ?f))
  )

  (defrule factorcerteza_problema_igual
   (declare (salience -9997))
    (mayor_hipotesis ?x ?y)
    (not (hay_empate))
    (a_evaluar ?c ?y)
    (test (neq ?x ?c))
  =>
    (assert (mayor_hipotesis ?c ?y))
    (assert (hay_empate))
  )

  (defrule muestra_todos
   (declare (salience -9996))
   (a_evaluar ?c ?f)
  =>
    (printout t "------ CERTEZA DE LA ASIGNATURA " ?c" " ?f crlf)
  )

  (defrule muestra_mayor
   (declare (salience -9997))
    (not (hay_empate))
    (mayor_hipotesis ?x ?y)

  =>
    (printout t crlf "La mayor hipotesis es " ?x" y los motivos son: " crlf)
    (assert (he_terminado))
    
  )

  (defrule muestra_motivos
   (declare (salience -9997))
    (not (hay_empate))
    (mayor_hipotesis ?x ?y)
    (explicacion ?x ?g)

  =>
    (printout t ?g crlf)
 )

  (defrule muestra_empate
   (declare (salience -9997))
   (hay_empate)
    ?t <- (mayor_hipotesis ?x ?y)
    ?r <- (mayor_hipotesis ?l ?m)
    (test (neq ?x ?l))
    (explicacion ?l ?w)
    (explicacion ?x ?g)

  =>
    (printout t "La mayor hipotesis " ?x" y "?g crlf)
    (printout t "Tambien esta la hipotesis " ?l" con la misma certeza y "?w crlf)
    (retract ?t ?r)
    (reset)
  )

  (defrule ningun_problema
   (declare (salience -9997))
   (mayor_hipotesis ?l 0)

  =>
    (printout t "No hemos podido decidir que rama prefieres por la combinacion de datos que ha introducido" crlf)
    (reset)
    
  )

  (defrule reinicio
   (declare (salience -9999))
   (he_terminado)

  =>
    (reset)
    
  )



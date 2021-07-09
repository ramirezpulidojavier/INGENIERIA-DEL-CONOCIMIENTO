;;;;Sistema modular que reutiliza los sistemas expertos simples desarrollados por:
;;;;      Javier Ramírez Pulido y Manuel Ángel Rodríguez Segura


;;;   El sistema aconseja sobre la elección de rama para la especializacion en el grado de Ingenieria Informatica

;;;   Este parte preguntando por las asignaturas favoritas del usuario tanto del primer como del segundo curso
;;;   teniendo disponible tambien la respuesta "Ninguna" en caso de no tener favoritas.
;;;   Ademas, se preguntara al usuario su nota media llevada hasta el momento (SUFICIENTE, BIEN, NOTABLE, SOBRESALIENTE)
;;;   y su consideracion propia del esfuerzo realizado en la carrera hasta el momento (BAJO, MEDIO, ALTO) 
;;;   Esto servira al sistema para saber por donde comenzar dependiendo de los gustos del usuario a hacer las preguntas correspondientes
;;;   Por ejemplo, escoger Calculo y Logica va a orientar al sistema de forma inicial sobre la rama de CSI
;;;   mientras que una eleccion previa de EC y AC, va a partir con preguntas relacionadas sobre la rama de IC
;;;   Cuando las asignaturas favoritas no permiten al sistema decantarse por una rama inicial directamente, utiliza los valores
;;;   de la media y el esfuerzo para conducirse por una rama que requiera más nivel que otra (a nuestro criterio)


;;;El salto de modulos lo realizamos con focus <nombre_modulo> que nos permite introducir en la pila el modulo indicado para ejecutarse


; Modulo para declarar las diferentes ramas del grado:
;  - Computacion y Sistemas Inteligentes (CSI)
;  - Ingenieria del Software (IS)
;  - Ingenieria de Computadores (IC)
;  - Sistemas de Informacion (SI)
;  - Tecnologias de la Informacion (TI) 

(defmodule DeclaracionRamas
   (export deftemplate ?ALL))
   (deffacts Ramas
      (Rama Computacion_y_Sistemas_Inteligentes)
      (Rama Ingenieria_del_Software)
      (Rama Ingenieria_de_Computadores)
      (Rama Sistemas_de_Informacion)
      (Rama Tecnologias_de_la_Informacion)
   )



; Modulo que corresponde a la pregunta inicial que realiza el sistema para orientas las preguntas posteriores

(defmodule PreguntasPrevias (export deftemplate ?ALL))
   
   ; Diferentes asignaturas de primer y segundo año del grado
   (deffacts asignaturas
      (primero MP) (primero FP) (primero IES) (primero TOC) (primero CAL) (primero ALEM) (primero ES) (primero FS) (primero FFT) (primero LMD)
      (segundo ED) (segundo AC) (segundo EC) (segundo SCD) (segundo FBD) (segundo ALG) (segundo SO) (segundo PDOO) (segundo FIS) (segundo IA)
      (Consejo NADA) 
   )


   ; Regla para indicar que este modulo es el primero que se va a ejecutar
   (defrule primer_modulo
      (declare (auto-focus TRUE)) ; Con auto-focus TRUE indicamos que este modulo tiene que ejecutarse con la maxima prioridad (el primero)
   =>
   )

   ; Regla para preguntar sobre la asignatura preferida del usuario del primer año 
   (defrule titulo_asig_primero
      (declare (salience 9999))
   =>
      (printout t "Cual de las siguientes es tu asignatura favorita de primero?:" crlf)
      (printout t "        Ninguna" crlf)
      (assert (pedir_asignatura_primero))
   )

   ; Regla para mostrar las diferentes asignaturas del primer año del grado
   (defrule asignaturas_de_primero
      (declare (salience 9998))
      (primero ?f)
   =>
      (printout t "        " ?f crlf)
   )

   ; Regla en la que el usuario introduce su asignatura favorita del primer año
   ; Y que activa la regla para pedir la asignatura favorita del segundo curso 
   (defrule favorita_de_primero
      ?g <- (pedir_asignatura_primero)
   =>
      (assert (favorita_primero (read)))
      (assert (pedir_asignatura_segundo))
      (retract ?g)
   )


   ; Regla para comprobar que lo introducido por el ususario es correcto tal y como lo deseamos
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


   ; Regla para preguntar sobre la asignatura preferida del usuario del segundo año 
   (defrule titulo_asig_segundo
      (pedir_asignatura_segundo)
   =>
      (printout t crlf "Cual de las siguientes es tu asignatura favorita de segundo?:" crlf )
      (printout t "        Ninguna" crlf)
      (assert (mostrar_segundo))
   )


   ; Regla para mostrar las diferentes asignaturas del segundo año del grado
   (defrule asignaturas_de_segundo
      (segundo ?f)
      (mostrar_segundo)
   =>
      (printout t "        " ?f crlf)
   )

   ; Regla en la que el usuario introduce su asignatura favorita del primer año
   (defrule favorita_de_segundo
      ?g <- (pedir_asignatura_segundo)
      ?w <- (mostrar_segundo)
   =>
      (assert (favorita_segundo (read)))
      (retract ?g)
      (retract ?w)
      (assert (pedir_nota))
   )

   ; Regla para comprobar que lo introducido por el ususario es correcto tal y como lo deseamos
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


   ; Regla para conocer la nota media del usuario
   (defrule nota_media_usuario
      (declare (salience 9901))
      ?g <- (pedir_nota)
   =>
      (printout t "Para comenzar necesitamos saber cual es tu nota media actual?: (SUFICIENTE (5), BIEN (6), NOTABLE (7-8), SOBRESALIENTE (9-10))" crlf)
      (assert (nota_media (read)))
      (retract ?g)
      (assert (pedir_esfuerzo))
   )

   ; Regla para comprobar que lo introducido por el usuario es correcto
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

   ; Regla para conocer el esfuerzo que considera el usuario que ha realiazado durante lo cursado hasta el momento 
   (defrule esfuerzo_usuario
      (declare (salience 9900))
      ?g <- (pedir_esfuerzo)
   =>
      (printout t "Como consideras tu esfuerzo en la carrera? (ALTO/MEDIO/BAJO)"crlf)
      (assert (esfuerzo (read)))
      (retract ?g)
      (assert (pedir_rama_comienzo))
   )

   ; Regla para comprobar que lo introducido por el usuario es correcto
   (defrule comprobacion_esfuerzo
      (declare (salience 9900))
      ?g <- (esfuerzo ?f)
      (test (and (neq ?f ALTO) (neq ?f MEDIO) (neq ?f BAJO)))
   =>
      (printout t ?f " no es una opcion correcta, introduce otra"crlf)
      (assert (pedir_esfuerzo))
      (retract ?g)
   )


   ; Una vez acabamos este modulo (cuando el usuario introduce la consideracion de su propio esfuerzo)
   ; Pasamos al siguiente modulo en el que el sistema deduce por lo introducido hasta el momento, la rama inicial sobre la que 
   ; hacer preguntas al usuario para ver si recomendarla o si estamos equivocados saltar a preguntar por otra
   (defrule esfuerzo_correcto
      (esfuerzo ?f)
      (test (or (eq ?f ALTO) (eq ?f MEDIO) (eq ?f BAJO)))
   =>
      (printout t "Estupendo, con esta informacion inicial vamos tratar de recomendarte una rama de acuerdo a tus gustos..."crlf)
      (pop-focus) (focus DecisionPrimerCamino)
   )




; Modulo para escoger la rama inicial sobre la que hacer preguntas al usuario en base a lo conocido de el hasta el momento

(defmodule DecisionPrimerCamino (import PreguntasPrevias deftemplate ?ALL)
                                (export deftemplate ?ALL))


   ; Las siguientes reglas definen la rama inicial para preguntar en caso de tener como favoritas dos asignaturas  
   ; que comparten contenidos con una rama en concreto 

   ; PARA LA RAMA CSI 
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

   ; PARA LA RAMA IS
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

   ; PARA LA RAMA SI
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

   ; PARA LA RAMA IC
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



   ; En caso de tener asignaturas favoritas de diferentes ramas, nos basamos en la media para partir de una rama u otra
   ; Siempre teniendo prioridad a las medias altas (NOTABLE O SOBRESALIENTE)

   ; Si tiene una asignatura favorita que corresponde con SI y ota con CSI y la media es alta partimos de CSI como inicial
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


   ; Si tiene una asignatura favorita que corresponde con SI y ota con IS y la media es alta partimos de IS como inicial
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


   ; Si tiene una asignatura favorita que corresponde con SI y ota con IS y la media es MAS BAJA partimos de SI como inicial
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


   ; Si tiene una asignatura favorita que corresponde con SI y ota con TI partimos de TI como inicial
   (defrule si_media_no_alta_ti
      (declare (salience 9795))
      ?g <- (pedir_rama_comienzo)
   =>
      (assert (empezar_por TI))
      (assert (primero_fue Tecnologias_de_la_Informacion))
      (retract ?g)

   )


   ; Si tiene una asignatura favorita que corresponde con SI y ota con IC y la media NO ES ALTA partimos de IC como inicial
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

   ; Una vez conocemos la rama por la que comenzar a preguntar al usuario para conocer si estamos en lo cierto 
   ; Tenemos que saltar al MODULO DE CONTROL que es el encargado de dirigirnos hacia las preguntas sobre una rama u otra
   (defrule vamos_a_modulo_control
      (empezar_por ?g)
      (primero_fue ?f)
   =>
      (pop-focus) (focus ModuloControlDeBloque)
   )




; Modulo de control que nos conduce entre las preguntas de las diferentes ramas para decidir si nuestro usuario encaja 
; con alguna de ellas y en ese csao recomendarla
(defmodule ModuloControlDeBloque (import DecisionPrimerCamino deftemplate ?ALL)
                                 (import DeclaracionRamas deftemplate ?ALL))

   ; Comenzamos a preguntar sobre la rama de CSI por lo conocido hasta el momento del usuario
   (defrule decision_csi
      (empezar_por CSI)
      ?f <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (pop-focus) (focus ModuloCSI)
      (retract ?f)
   )

   ; Comenzamos a preguntar sobre la rama de IS por lo conocido hasta el momento del usuario
   (defrule decision_is
      (empezar_por IS)
      ?f <- (Rama Ingenieria_del_Software)
   =>
      (pop-focus) (focus ModuloIS)
      (retract ?f)
   )

   ; Comenzamos a preguntar sobre la rama de SI por lo conocido hasta el momento del usuario
   (defrule decision_si
      (empezar_por SI)
      ?f <- (Rama Sistemas_de_Informacion)
   =>
      (pop-focus) (focus ModuloSI)
      (retract ?f)
   )

   ; Comenzamos a preguntar sobre la rama de IC por lo conocido hasta el momento del usuario
   (defrule decision_ic
      (empezar_por IC)
      ?f <- (Rama Ingenieria_de_Computadores)
   =>
      (pop-focus) (focus ModuloIC)
      (retract ?f)
   )

   ; Comenzamos a preguntar sobre la rama de TI por lo conocido hasta el momento del usuario
   (defrule decision_ti
      (empezar_por TI)
      ?f <- (Rama Tecnologias_de_la_Informacion)
   =>
      (pop-focus) (focus ModuloTI)
      (retract ?f)
   )





; Para cada rama, haremos un modulo correspondiente que incluye todas las preguntas relacionadas con la misma
; y posterioremente haremos un modulo final que nos va a recomendar la rama que mejor encaja con el usuario
; en caso de no encajar con ninguna, le volvemos a preguntar sobre la rama inicial que nuestro sistema escogio por las preguntas iniciales
; y si no termina de gustarle tampoco, diremos que no hemos podido aconsejarle una rama



; Modulo para la rama IC

(defmodule ModuloIC (import DeclaracionRamas deftemplate ?ALL)
                    (import DecisionPrimerCamino deftemplate ?ALL)
                    (export deftemplate ?ALL)) 


   ; Pregunta inicial, gusto por la arquitectura de sistemas
   (defrule empezamos_por_ic
      (declare (salience 9684))
      (empezar_por IC)
   =>
      (printout t "Te gustan la arquitectura del sistema? (SI/NO/NO SE)"crlf)
      (assert (arquitectura_sistema (read)))
   )


   ; Gusto por hardware digital en caso de haber contestado positivamente la pregunta anterior
   (defrule segunda_pregunta_si
      (arquitectura_sistema SI)
      (empezar_por IC)
   =>
      (printout t "Bien, vamos por buen camino, te interesa el mundo del hardware digital? (SI/NO/NO SE)"crlf)
      (assert (hardware_digital2 (read)))
   )


   ; Gusto por hardware digital en caso de haber contestado negativamente la pregunta anterior
   (defrule segunda_pregunta_no
      (arquitectura_sistema NO)
      (empezar_por IC)
   =>
      (printout t "Vale, necesito saber mas para tomar una decision. Te interesa el mundo del hardware digital? (SI/NO/NO SE)"crlf)
      (assert (hardware_digital1 (read)))
   )


   ; Gusto por el propio hardware en caso de haber contestado la primera pregunta negativa y la segunda positivamente
   (defrule tercera_pregunta_1
      (arquitectura_sistema NO)
      (hardware_digital1 SI)
      (empezar_por IC)
   =>
      (printout t "Si solo te gusta el hardware digital, necesito saber si te gustan los microprocesadores y los elementos fisicos del sistema para tomar una decision... (SI/NO/NO SE)"crlf)
      (assert (microprocesador1 (read)))
   )


   ; Gusto por el propio hardware en caso de haber contestado la primera pregunta positiva y la segunda negativamente
   (defrule tercera_pregunta_1_otro_camino
      (arquitectura_sistema SI)
      (hardware_digital2 NO)
      (empezar_por IC)
   =>
      (printout t "Si solo te gustan las arquitecturas del sistema, necesito saber si te gustan los microprocesadores y los elementos fisicos del sistema para tomar una decision... (SI/NO/NO SE)"crlf)
      (assert (microprocesador1 (read)))
   )


   ; Gusto por el propio hardware en caso de haber contestado positivamente las dos preguntas anteriores 
   (defrule tercera_pregunta_2
      (arquitectura_sistema SI)
      (hardware_digital2 SI)
      (empezar_por IC)
   =>
      (printout t "Por ahora estas encajando perfectamente con una rama, pero no es suficiente aun. Te gustan los microprocesadores y los elementos fisicos del sistema? (SI/NO/NO SE)"crlf)
      (assert (microprocesador2 (read)))
   )

   ; Si hemos contestado positivamente a todas las preguntas seleccionadas para rama, ya tenemos una conclusion directa
   (defrule llego_a_conclusion1
      (microprocesador2 SI)
      ?f <- (empezar_por IC)
      ?g <- (Consejo NADA)
   =>
      (printout t "Efectivamente, no hay dudas de que esta rama era la mas adecuada para ti."crlf)
      (retract ?g)
      (retract ?f)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )


   ; En caso de tener dudas sobre si es la rama ideal o no, optamos por considerar la nota media del usuario
   ; Ya que nos va permitir saltar a preguntar hacia una rama mas concreta a razon de sus gustos   

   (defrule llego_a_la_media_1
      (hardware_digital1 NO)
      (empezar_por IC)
   =>
      (assert (media1_ic))
   )

   (defrule llego_a_la_media_1_otro_camino
      (microprocesador1 NO)
      (empezar_por IC)
   =>
      (assert (media1_ic))
   )


   ; En caso de tener una nota media BAJA o MEDIA, vamos a saltar a preguntarle por la rama de TI
   (defrule uso_la_media_1_bajamedia
      (media1_ic)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      ?g <- (empezar_por IC)
   =>
      (assert (empezar_por TI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ; En caso de tener una nota media ALTA, vamos a saltar a preguntarle por la rama de CSI
   (defrule uso_la_media_1_alta
      (media1_ic)
      (nota_media ?f)
      (hardware_ic ?g)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE) (eq ?g NO)))
      (empezar_por IC)
   =>
      (printout t "Tu nota media es muy buena y tal vez tenga una rama que te pueda interesar. Te gustan las matematicas? (SI/NO/NO SE)" crlf)
      (assert (matematicas_ic (read)))
   )


   ; Si responde positivamente sabemos que tenemos que cambiar a la rama de CSI
   (defrule si_le_gustan_las_mates_ic
      (matematicas_ic SI)
      ?g <- (empezar_por IC)
   =>
      (printout t "Bien, si te gustan las matematicas vamos a ver si es esta la rama que mas encaja contigo..."crlf)
      (assert (empezar_por CSI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ; Si responde negativamente sabemos que tenemos que cambiar a la rama de IS
   (defrule si_no_le_gustan_las_mates_ic
      (matematicas_ic NO)
      ?g <- (empezar_por IC)
   =>
      (printout t "No te preocupes, que no te gusten las matematicas me hace pensar que tu rama ideal sea otra..."crlf)
      (assert (empezar_por IS))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ; En caso de tener dudas sobre si es la rama ideal o no, ademas de contar con la media, optamos por considerar el esfuerzo del usuario
   ; el cual nos va permitir decantarnos por saltar a preguntar hacia una rama mas concreta a razon de sus gustos
   ; O en caso de tener una idea algo mas clara sobre la rama, tomaremos una decision final dependiendo del esfuerzo considerado


   ; Miramos si el esfuerzo que ha introducido es bajo o medio
   ; Pero si contesta positivamente dos de las preguntas relacionadas con la rama
   ; Se le recomendara IC como la mejor rama a pesar de que tendra que esforzarse un poco mas 
   (defrule esfuerzo_bajo_ic
      (esfuerzo ?f)
      (microprocesador2 NO)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      ?g <- (empezar_por IC)
      ?h <- (Consejo NADA)
   =>
      (printout t "Ya tengo un veredicto sobre la rama mas adecuada para ti."crlf)
      (retract ?h)
      (retract ?g)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama_y_no_te_esfuerzas_mucho ManuelRodriguez))
      (pop-focus) (focus Decision)
   )

   ; Miramos si el esfuerzo que ha introducido es bajo o medio
   ; Pero si contesta positivamente dos de las preguntas relacionadas con la rama
   ; Se le recomendara IC como la mejor rama a pesar de que tendra que esforzarse un poco mas
   (defrule esfuerzo_bajo_ic_otro_camino
      (esfuerzo ?f)
      (microprocesador1 SI)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      ?g <- (empezar_por IC)
      ?h <- (Consejo NADA)
   =>
      (printout t "Vale, ya tengo un veredicto sobre la rama mas adecuada para ti."crlf)
      (retract ?h)
      (retract ?g)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama_y_no_te_esfuerzas_mucho ManuelRodriguez))
      (pop-focus) (focus Decision) ;SALTAMOS AL MODULO DE DECISION
   )


   ; Miramos si el esfuerzo que ha introducido es alto y si su nota media es alta o baja
   ; Pero si contesta positivamente dos de las preguntas relacionadas con la rama
   ; Se le recomendara IC como la mejor rama por su esfuerzo a pesar de que no tenga una muy buena nota para ello
   (defrule esfuerzo_alto_media_baja_ic
      (esfuerzo ALTO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      ?g <- (empezar_por IC)
      ?h <- (Consejo NADA)
   =>
      (printout t "Teniendo en cuenta tu nota media y tu esfuerzo... ya tengo un veredicto sobre la rama mas adecuada para ti."crlf)
      (retract ?h)
      (retract ?g)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama,_te_esfuerzas_pero_no_tienes_media_alta JavierRamirez))
      (pop-focus) (focus Decision) ;SALTAMOS AL MODULO DE DECISION
   )


   ; En caso de no contestar positivamente a la mayoria de las preguntas para esta rama
   ; Pero es un estudiante que se esfuerza y su nota es alta, vamos a preguntar para ver si estamos en la rama correcta o si prefiere la rama IS
   (defrule esfuerzo_alto_media_alta_ic
      (esfuerzo ALTO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (empezar_por IC)
   =>
      (printout t "Tienes muy buena nota y tu esfuerzo en la carrera es lo suficiente para poder encajarte con otra rama, pero necesito saber si te gusta mas el hardware que el software... (SI/NO/NO SE)"crlf)
      (assert (hardware_ic (read)))
   )


   ; Si nos confirma que a pesar de ello si le gusta hardaware, ya tenemos la conclusion y la recomendacion preparada para IC
   (defrule conclusion_3_ic
      (hardware_ic SI)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IC)
      ?h <- (Consejo NADA)
   =>
      (printout t "Vale, vale, diciendo que te gusta mas el hardware, ya tengo un veredicto sobre la rama mas adecuada para ti."crlf)
      (retract ?h)
      (retract ?g)
      (assert (Consejo Ingenieria_de_Computadores Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama,_te_esfuerzas,_no_tienes_media_alta_pero_te_gusta_el_hardware JavierRamirez))
      (pop-focus) (focus Decision) ;SALTAMOS AL MODULO DE DECISION
   )



;;; Una vez conocemos el funcionamiento del modulo de una asignatura, vamos a indicar que el resto de ramas llevan el mismo funcionamiento
;;; Parten de unas preguntas iniciales, y dependiendo de las respuestas del usuario y de su nota media y esfuerzo considerado
;;; Vamos a optar o bien por dar una decision directa, o bien por dar una decision dependiendo de estos valores o bien saltar a otra rama que consideremos mas indicada
;;; Es por ello por lo que el codigo no se va a comentar tan en detalle debido a su gran extension y su facil comprension 






; Modulo para la rama IC

(defmodule ModuloIS (import DeclaracionRamas deftemplate ?ALL)
                    (import DecisionPrimerCamino deftemplate ?ALL)
                    (export deftemplate ?ALL)) 
   
   ; Gusto por el desarrollo de aplicaciones
   (defrule empezamos_por_is
      (declare (salience 9684))
      (empezar_por IS)
   =>
      (printout t "Te gusta el desarrollo de aplicaciones? (SI/NO/NO SE)" crlf)
      (assert (apps (read)))
   )


   ; Gusto por las interfaces de usuario
   (defrule segunda_pregunta_si_is
      (apps SI)
      (empezar_por IS)
   =>
      (printout t "El mundo de las aplicaciones es muy interesante. Te gustaria tambien hacer interfaces de usuario? (SI/NO/NO SE)"crlf)
      (assert (interfaz1 (read)))
   )

   (defrule segunda_pregunta_no_is
      (apps NO)
      (empezar_por IS)
   =>
      (printout t "El desarollo de aplicaciones no lo es todo en esta rama. Te gustaria aprender a hacer interfaces de usuario? (SI/NO/NO SE)"crlf)
      (assert (interfaz2 (read)))
   )


   ; Gusto por hacer paginas web
   (defrule tercera_pregunta_1_is
      (interfaz1 SI)
      (empezar_por IS)
   =>
      (printout t "Aplicaciones, interfaces... apuntas mucho a una rama en concreto. Te gusta o gustaria hacer paginas web? (SI/NO/NO SE)"crlf)
      (assert (pagina_web1 (read)))
   )

   (defrule tercera_pregunta_2_is
      (apps SI)
      (interfaz1 NO)
      (empezar_por IS)
   =>
      (printout t "Ya tengo mis dudas sobre si es esta tu rama ideal o no... necesito saber si te gusta o gustaria hacer paginas web? (SI/NO/NO SE)"crlf)
      (assert (pagina_web2 (read)))
   )

   (defrule tercera_pregunta_2_is_otro_camino
      (apps NO)
      (interfaz2 SI)
      (empezar_por IS)
   =>
      (printout t "A pesar de que no te guste el desarrollo de apps, te gusta o gustaria hacer paginas web? (SI/NO/NO SE)"crlf)
      (assert (pagina_web2 (read)))
   )



   ; CONCLUSIONES EN BASE A LOS RESULTADOS DE LAS PREGUNTAS PREVIAS
   ; (mismo proceso que para el modulo anterior, VER EN CASO DE DUDA EN ALGUN PROCESO)



   (defrule llego_a_conclusion1_is
      (pagina_web1 SI)
      ?f <- (empezar_por IS)
      ?h <- (Consejo NADA)
   =>
      (printout t "Estupendo, ya la decision tomada sobre la rama mas adecuada para ti."crlf)
      (retract ?h)
      (retract ?f)
      (assert (Consejo Ingenieria_del_Software Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama ManuelRodriguez))
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
      (printout t "Vale, gracias a tu nota media ya tengo un veredicto sobre la rama mas adecuada para ti."crlf)
      (retract ?h)
      (retract ?u)
      (assert (Consejo Ingenieria_del_Software Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama ManuelRodriguez))
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
      (printout t "Tu esfuerzo ha sido el que me permita tener una decision sobre la rama mas adecuada para ti." crlf)
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
   =>
      (printout t "Si no te gustaria hacer paginas web, no tienes una media alta y consideras tener un esfuerzo bajo... creo que lo mejor sera preguntar sobre otra rama menos exigente..." crlf)
      (assert (empezar_por TI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 ALTA
   (defrule media1_alta_is
      (pagina_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
   =>
      (printout t "Si no te gustaria hacer paginas web.. y teniendo esa media tan alta... creo que lo mejor sera preguntar sobre otra rama que pueda encajar mejor contigo..." crlf)
      (assert (empezar_por CSI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 ALTA OTRO CAMINO
   (defrule media1_alta_is_otro_camino
      (interfaz2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por IS)
   =>
      (printout t "Si no te gustaria hacer interfaces de usuario.. y teniendo esa media tan alta.. creo que lo mejor sera preguntar sobre otra rama que pueda encajar mejor contigo..." crlf)
      (assert (empezar_por CSI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 MEDIA ESFUERZO ALTO
   (defrule media1_media_is_esfuerzo_alto
      (pagina_web2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
   =>
      (printout t "Si no te gustaria hacer paginas web y tu nota media es de un bien, pero tu esfuerzo en la carrera es alto... vamos a probar con otra rama" crlf)
      (assert (empezar_por CSI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;UTILIZO LA MEDIA1 MEDIA OTRO CAMINO ESFUERZO ALTO
   (defrule media1_media_is_otro_camino_esfuerzo_alto
      (interfaz2 NO)
      (nota_media BIEN)
      (esfuerzo ALTO)
      ?g <- (empezar_por IS)
   =>
      (printout t "Si no te gustaria hacer interfaces de usuario y tu nota media es de un bien, pero tu esfuerzo en la carrera es alto... vamos a probar con otra rama" crlf)
      (assert (empezar_por CSI))
      (pop-focus) (focus SaltoEntreRamas)
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
      (printout t "Si no te gustaria hacer paginas web, tu nota media es de un bien y tu esfuerzo no es alto... creo que tu rama podria estar enfocada por hardware. Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
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
      (printout t "Si no te gustaria hacer interfaces de usuario, tu nota media es de un bien y tu esfuerzo no es alto... creo que tu rama podria estar enfocada por hardware. Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware_is (read)))
   )


   ;UTILIZO LA MEDIA1 BAJA
   (defrule media1_baja_is
      (pagina_web2 NO)
      (nota_media SUFICIENTE)
      (empezar_por IS)
   =>
      (printout t "Si no te gustaria hacer pagina web y tu nota media es muy justa... tu rama deberia ser otra. Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
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
      (printout t "Vale, ya tengo un veredicto sobre la rama mas adecuada para ti." crlf)
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
      (printout t "Bien, a pesar de tu media, contando con tu esfuerzo, ya tengo un veredicto sobre la rama mas adecuada para ti." crlf)
      (retract ?h)
      (retract ?u)
      (assert (Consejo Ingenieria_del_Software Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama ManuelRodriguez))
      (pop-focus) (focus Decision)
   )


   ;UTILIZO LA MEDIA1 BAJA Y SIN ESFUERZO
   (defrule va_para_is_media_baja
      (pagina_web2 SI)
      (nota_media ?f)
      (test (and (neq ?f NOTABLE) (neq ?f SOBRESALIENTE)))
      (esfuerzo BAJO)
      ?g <- (empezar_por IS)
   =>
      (printout t "Tu esfuerzo y tu nota media no son las mas adecuadas como para decantarme por esta rama... vamos a probar con otra" crlf)
      (assert (empezar_por TI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ;UTILIZO LA MEDIA1 BAJA OTRO CAMINO
   (defrule media1_baja_is_otro_camino
      (interfaz2 NO)
      (nota_media SUFICIENTE)
      (empezar_por IS)
   =>
      (printout t "Si no te gustaria aprender a hacer interfaces de usuario y con una nota media suficiente, tu rama puede que este enfocada al mundo del hardware. Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware_is (read)))
   )


   ;SI PREFIERE HARDWARE ANTES QUE SOFTWARE ESTANDO EN IS
   (defrule gusta_hardware_is
      (hardware_is SI)
      ?g <- (empezar_por IS)
   =>
      (printout t "Sabiendo todo esto... probaremos con otra rama para ver si es la mejor para tu perfil" crlf)
      (assert (empezar_por IC))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ;SI PREFIERE SOFTWARE ANTES QUE HARDWARE ESTANDO EN IS
   (defrule no_gusta_hardware_is
      (hardware_is NO)
      ?g <- (empezar_por IS)
   =>
      (printout t "Bien, el hardware no es lo tuyo por ahora, vamos a probar con otra rama" crlf)
      (assert (empezar_por TI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )




; Modulo para rama CSI

(defmodule ModuloCSI (import DeclaracionRamas deftemplate ?ALL)
                     (import DecisionPrimerCamino deftemplate ?ALL)
                     (export deftemplate ?ALL)) 

   ; Gusto por las matematicas
   (defrule empezamos_por_csi
      (declare (salience 9684))
      (empezar_por CSI)
   =>
      (printout t "Te gustan las matematicas? (SI/NO/NO SE)"crlf)
      (assert (matematicas_csi (read)))
   )


   ; Gusto por la programacion 
   (defrule segunda_pregunta_si_csi
      (matematicas_csi SI)
      (empezar_por CSI)
   =>
      (printout t "Que te gusten las matematicas es bueno... te gusta tambien la programacion? (SI/NO/NO SE)"crlf)
      (assert (programacion2 (read)))
   )

   (defrule segunda_pregunta_no_csi
      (matematicas_csi NO)
      (empezar_por CSI)
   =>
      (printout t "Bueno, las matematicas no lo son todo en esta rama... te gusta la programacion? (SI/NO/NO SE)"crlf)
      (assert (programacion1 (read)))
   )


   ; Gusto por la inteligencia artificial
   (defrule tercera_pregunta_1_csi
      (matematicas_csi SI)
      (programacion2 SI)
      (empezar_por CSI)
   =>
      (printout t "Que bien, eres un usuario candidato a esta rama... te gusta la Inteligencia Artificial? (SI/NO/NO SE)"crlf)
      (assert (ia1 (read)))
   )

   (defrule tercera_pregunta_2_csi
      (matematicas_csi SI)
      (programacion2 NO)
      (empezar_por CSI)
   =>
      (printout t "Tu gusto por las matematicas me hace pensar que puedas ser bueno en esta rama... te gusta ademas la Inteligencia Artificial? (SI/NO/NO SE)"crlf)
      (assert (ia2 (read)))
   )

   (defrule tercera_pregunta_2_csi_otro_camino
      (matematicas_csi NO)
      (programacion1 SI)
      (empezar_por CSI)
   =>
      (printout t "A pesar de que no te gusten las mates, la programacion es muy importante en esta rama... te gusta ademas la Inteligencia Artificial? (SI/NO/NO SE)"crlf)
      (assert (ia2 (read)))
   )


   ; CONCLUSIONES EN BASE A LOS RESULTADOS DE LAS PREGUNTAS PREVIAS
   ; (mismo proceso que para el primer modulo, VER EN CASO DE DUDA EN ALGUN PROCESO)


   (defrule llego_a_conclusion1_csi
      (ia1 SI)
      ?f <- (empezar_por CSI)
      ?h <- (Consejo NADA)
   =>
      (printout t "Efectivamente, tal y como pensaba, ya tengo un veredicto sobre la rama mas adecuada para ti." crlf)
      (retract ?h)
      (retract ?f)
      (assert (Consejo Computacion_y_Sistemas_Inteligentes Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama ManuelRodriguez))
      (pop-focus) (focus Decision)
   )


   ;USO EL ESFUERZO 1 CSI
   (defrule uso_esfuerzo_1_csi
      (ia1 NO)
      (esfuerzo ?f)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      (empezar_por CSI)
   =>
      (printout t "Tu esfuerzo y tu negacion sobre el gusto de inteligencia artificial me hace pensar que esta rama no es la mas adecuada para ti... te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware3_csi (read)))
   )

   ;SI LE GUSTA MAS EL HARDWARE QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_gusta_hardware3_csi
      (hardware3_csi SI)
      ?g <- (empezar_por CSI)
   =>
      (printout t "Vale, me lo temia, tu rama no era la de CSI, vamos a probar con la rama que incluye hardware..." crlf)
      (assert (empezar_por IC))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE ESTANDO EN CSI 
   (defrule esfuerzo_1_no_gusta_hardware3_csi
      (hardware3_csi NO)
      ?g <- (empezar_por CSI)
   =>
      (printout t "Me lo temia, tu rama no es CSI, seguramente sea IS, vamos a comprobarlo..." crlf)
      (assert (empezar_por SI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ;USO LA MEDIA ALTA
   (defrule uso_la_media_alta_csi
      (ia2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
   =>
      (printout t "Tu nota media es muy buena, pero que no te guste la inteligencia artificial me hace pensar que tu rama deberia ser otra diferente..." crlf)
      (assert (empezar_por IS))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;USO LA MEDIA ALTA OTRO CAMINO
   (defrule uso_la_media_alta_csi_otro_camino
      (programacion1 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por CSI)
   =>
      (printout t "Tu buena nota media y tu pensamiento negativo sobre la programacion me hace pensar que tu rama va a ser IS... vamos a comprobarlo..." crlf)
      (assert (empezar_por IS))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ;USO LA MEDIA BAJA
   (defrule uso_la_media_baja_csi
      (ia2 NO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
   =>
      (printout t "Si no te gusta la inteligencia artificial y tu nota media deja que desear, es momento de pensar que tu rama es otra. Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware2_csi (read)))  
   )

   ;USO LA MEDIA ALTA OTRO CAMINO
   (defrule uso_la_media_baja_csi_otro_camino
      (programacion1 NO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
   =>
      (printout t "Si no te gusta la programacion y siendo tu nota media un poco baja, es momento de pensar que tu rama es otra. Te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware2_csi (read)))  
   )



   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE ESTANDO EN CSI 
   (defrule gusta_hardware2_csi
      (hardware2_csi SI)
      ?g <- (empezar_por CSI)
   =>
      (printout t "Ha ocurrido lo que pensaba, te gusta mas el hardware, por lo que vamos a probar otra rama que puede que encaje contigo mucho mas que CSI..."crlf)
      (assert (empezar_por IC))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 ESTANDO EN CSI 
   (defrule no_gusta_hardware2_csi
      (hardware2_csi NO)
      ?g <- (empezar_por CSI)
   =>
      (printout t "Ha ocurrido lo que pensaba, te gusta mas el software, por lo que vamos a probar con otra rama que puede que encaje contigo mucho mas que CSI..."crlf)
      (assert (empezar_por TI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ;TENGO EN CUENTA EL ESFUERZO2 BAJO
   (defrule esfuerzo_2_bajo_csi
      (ia2 SI)
      (esfuerzo ?f)
      (test (or (eq ?f BAJO) (eq ?f MEDIO)))
      (empezar_por CSI)
   =>
      (printout t "A pesar de que te interese el mundo de la inteligencia artificial, tu esfuerzo me hace pensar que esta no deberia ser tu rama aconsejada. Vamos a probar con otra, pero para ello necesito saber si te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware2_csi (read)))  
   )

   ;TENGO EN CUENTA EL ESFUERZO2 ALTO
   (defrule esfuerzo_2_alto_csi
      (ia2 SI)
      (esfuerzo ALTO)
      ?f <- (empezar_por CSI)
      ?h <- (Consejo NADA)
   =>
      (printout t "Vale, sabiendo que tu esfuerzo es alto y que te interesa el mundo de la IA, ya tengo un veredicto sobre la rama que mas encaja contigo"crlf)
      (retract ?h)
      (retract ?f)
      (assert (Consejo Computacion_y_Sistemas_Inteligentes Te_gustan_sus_asignaturas_dadas,_el_contenido_de_algunas_asignaturas_de_la_rama_y_te_esfuerzas ManuelRodriguez))
      (pop-focus) (focus Decision)
   )




; Modulo para la rama SI

(defmodule ModuloSI (import DeclaracionRamas deftemplate ?ALL)
                    (import DecisionPrimerCamino deftemplate ?ALL)
                    (export deftemplate ?ALL)) 

   ; Gusto por la programacion web
   (defrule empezamos_por_si
      (declare (salience 9684))
      (empezar_por SI)
   =>
      (printout t "Te gusta la programacion web? (SI/NO/NO SE)" crlf)
      (assert (programacion_web (read)))
   )


   ; Gusto por el mundo empresarial
   (defrule segunda_pregunta_si_si
      (programacion_web SI)
      (empezar_por SI)
   =>
      (printout t "Si te gusta la programacion web, te gustaria hacer sistemas de informacion para empresas? (SI/NO/NO SE)"crlf)
      (assert (si_empresas2 (read)))
   )

   (defrule segunda_pregunta_no_si
      (programacion_web NO)
      (empezar_por SI)
   =>
      (printout t "Si no te gusta la programacion web, te gustaria hacer sistemas de informacion para empresas? (SI/NO/NO SE)"crlf)
      (assert (si_empresas1 (read)))
   )


   ; Gusto por la base de datos
   (defrule tercera_pregunta_1_si
      (si_empresas2 SI)
      (empezar_por SI)
   =>
      (printout t "Parece que te gusta todo lo relacionado con esta rama, para terminar de confirmarlo... te gustan o te gustaria aprender sobre las bases de datos? (SI/NO/NO SE)"crlf)
      (assert (bases_datos1 (read)))
   )

   (defrule tercera_pregunta_2_si
      (si_empresas2 NO)
      (empezar_por SI)
   =>
      (printout t "Bueno, el mundo de las empresas no lo es todo en esta rama, te gustan las bases de datos? (SI/NO/NO SE)"crlf)
      (assert (bases_datos2 (read)))
   )

   (defrule tercera_pregunta_2_si_otro_camino
      (si_empresas1 SI)
      (empezar_por SI)
   =>
      (printout t "Si te gusta el mundo empresarial, puede que esta sea tu rama ideal, te gustaria aprender ademas sobre bases de datos? (SI/NO/NO SE)"crlf)
      (assert (bases_datos2 (read)))
   )



   ; CONCLUSIONES EN BASE A LOS RESULTADOS DE LAS PREGUNTAS PREVIAS
   ; (mismo proceso que para el primer modulo, VER EN CASO DE DUDA EN ALGUN PROCESO)


   (defrule llego_a_conclusion1_si
      (bases_datos1 SI)
      ?f <- (empezar_por SI)
      ?h <- (Consejo NADA)
   =>
      (printout t "Efectivamente, con tus respuestas tengo ya un veredicto sobre la rama ideal para ti"crlf)
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
      (printout t "Si no te gusta el mundo de empresas y teniendo una nota media tan alta, vamos probar con otra rama, para ello me gustaria saber si prefieres hardware antes que software? (SI/NO/NO SE)"crlf)
      (assert (hardware2 (read)))
   )


   ;UTILIZO LA MEDIA1 BAJA
   (defrule media1_baja_si
      (si_empresas1 NO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      (empezar_por SI)
   =>
      (printout t "Si no te gusta el mundo de empresas y teniendo en cuenta que tu nota media no es la mas indicada para esta rama, vamos a ver si encajas mas con otras. Para ello responde a la siguiente pregunta: te gusta mas el hardware que el software? (SI/NO/NO SE)"crlf)
      (assert (hardware1 (read)))
   )


   ;SI LE GUSTA MAS EL HARDWARE1 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware1_si
      (hardware1 SI)
      ?g <- (empezar_por SI)
   =>
      (printout t "Como te gusta mas el hardware, vamos a probar con otra rama, ya que IS no es la mas indicada para ti"crlf)
      (assert (empezar_por IC))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE1 EN SI
   (defrule no_le_gusta_hardware1_si
      (hardware1 NO)
      ?g <- (empezar_por SI)
   =>
      (printout t "Como te gusta mas el software que el hardware pero tu rama ideal no es SI, vamos a probar con otra diferente..."crlf)
      (assert (empezar_por TI))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;SI LE GUSTA MAS EL HARDWARE2 QUE EL SOFTWARE EN SI
   (defrule le_gusta_hardware2_si
      (hardware2 SI)
      ?g <- (empezar_por SI)
   =>
      (printout t "Te gusta mas el mundo del hardware que el del software, por lo que vamos a probar con la rama correspondiente para ello"crlf)
      (assert (empezar_por IC))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;SI LE GUSTA MAS EL SOFTWARE QUE EL HARDWARE2 EN SI
   (defrule no_le_gusta_hardware2_si
      (hardware2 NO)
      ?g <- (empezar_por SI)
   =>
      (printout t "Te gusta mas el mundo del software que el del hardware, vamos a probar con la rama correspondiente para ello"crlf)
      (assert (empezar_por IS))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ;UTILIZO LA MEDIA2 ALTA PARA RECONDUCIR
   (defrule uso_media2_alta_si
      (bases_datos2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      ?g <- (empezar_por SI)
   =>
      (printout t "Al no gustarte las bases de datos y tener unas notas tan altas... vamos a probar con otra rama"crlf)
      (assert (empezar_por IS))
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


   ;UTILIZO LA MEDIA2 BAJA PARA RECONDUCIR
   (defrule uso_media2_baja_si
      (bases_datos2 NO)
      (nota_media ?f)
      (test (and (neq ?f SOBRESALIENTE) (neq ?f NOTABLE)))
      (empezar_por SI)
   =>
      (printout t "No te gustan las bases de datos, te gustaria mas aprender cosas de hardware que de software? (SI/NO/NO SE)"crlf)
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
      (printout t "Con las respuestas que hemos recibido ya tenemos una recomendacion de rama para ti"crlf)
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
      (printout t "Con las respuestas que hemos recibido ya tenemos una recomendacion de rama para ti"crlf)
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
      (printout t "Tu esfuerzo es alto y te gustan las bases de datos. Sabiendo que tu nota media no es muy alta, te gustaria conocer mas sobre hardware que sobre software? (SI/NO/NO SE)"crlf)
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
      (printout t "Debido a tu esfuerzo alto en la carrera pero no tener gusto por las bases de datos y tener una nota media normal, necesito saber si te gustaria aprender mas sobre hardware que sobre software? (SI/NO/NO SE)"crlf)
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
      (printout t "Ya veo que te gustan las bases de datos y eres una persona competente en la carrera. Prefieres las matematicas a las interfaces de usuario? (SI/NO/NO SE)"crlf)
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
      (printout t "No importa que no te gusten las bases de datos, tu esfuerzo y tus notas son muy buenas. Prefieres las matematicas a las interfaces de usuario? (SI/NO/NO SE)"crlf)
      (assert (mates_si (read)))
   )

   ;SI LE GUSTAN LAS MATES MAS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_si_si
      (mates_si SI)
      ?g <- (empezar_por SI)
   =>
      (assert (empezar_por CSI))
      (printout t "Te gustan las matematicas que el software, es por ello por lo que vamos a probar con su rama correspondiente"crlf)
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;SI LE GUSTAN LAS MATES MENOS QUE EL SOFTWARE ESTANDO EN SI
   (defrule matematicas_no_si
      (mates_si NO)
      ?g <- (empezar_por SI)
   =>
      (assert (empezar_por IS))
      (printout t "Te gusta el software mas que las matematicas, es por ello por lo que vamos a probar con su rama correspondiente"crlf)
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )


; Modulo parla rama TI

(defmodule ModuloTI (import DeclaracionRamas deftemplate ?ALL)
                    (import DecisionPrimerCamino deftemplate ?ALL)
                    (export deftemplate ?ALL)) 
   
   ; Gusto por los servidores
   (defrule empezamos_por_ti
      (declare (salience 9684))
      (empezar_por TI)
   =>
      (printout t "Te gustaria conocer como funcionan los servidores? (SI/NO/NO SE)" crlf)
      (assert (servidores (read)))
   )

   ; Gusto por el temario de las redes
   (defrule segunda_pregunta_si_ti
      (servidores SI)
      (empezar_por TI)
   =>
      (printout t "Aparte de gustarte el mundo de los servidores, te gustan los temarios sobre redes? (SI/NO/NO SE)"crlf)
      (assert (redes1 (read)))
   )

   (defrule segunda_pregunta_no_ti
      (servidores NO)
      (empezar_por TI)
   =>
      (printout t "A pesar de no gustarte el mundo de los servidores, te gustan los temarios sobre redes? (SI/NO/NO SE)"crlf)
      (assert (redes2 (read)))
   )

   ; Gusto por la tecnologia web
   (defrule tercera_pregunta_1_ti
      (redes1 SI)
      (empezar_por TI)
   =>
      (printout t "Te gustan las redes, me interesa saber si te gusta tambien la tecnologia web? (SI/NO/NO SE)"crlf)
      (assert (tecnologia_web1 (read)))
   )

   (defrule tercera_pregunta_2_ti
      (redes1 NO)
      (empezar_por TI)
   =>
      (printout t "No te gustan las redes, pero te gustaria aprender sobre las tecnologias web? (SI/NO/NO SE)"crlf)
      (assert (tecnologia_web2 (read)))
   )

   (defrule tercera_pregunta_2_ti_otro_camino
      (redes2 SI)
      (empezar_por TI)
   =>
      (printout t "No te gustan los servidores pero si las redes, es por ello por lo que me interesa saber si ademas te gustaria conocer el temero de las tecnologias web? (SI/NO/NO SE)"crlf)
      (assert (tecnologia_web2 (read)))
   )



   ; CONCLUSIONES EN BASE A LOS RESULTADOS DE LAS PREGUNTAS PREVIAS
   ; (mismo proceso que para el primer modulo, VER EN CASO DE DUDA EN ALGUN PROCESO)


   (defrule llego_a_conclusion1_ti
      (tecnologia_web1 SI)
      ?f <- (empezar_por TI)
      ?h <- (Consejo NADA)
   =>
      (retract ?h)
      (retract ?f)
      (printout t "Con las respuestas obtenidas ya tenemos la rama que mejor concuerda con tus gustos"crlf)
      (assert (Consejo Tecnologias_de_la_Informacion Te_gustan_sus_asignaturas_dadas_y_el_contenido_de_la_rama ManuelRodriguez))
      (pop-focus) (focus Decision)
   )

   ;USO LA MEDIA1 BAJA
   (defrule uso_media1_baja_ti
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
   =>
      (assert (empezar_por IC))
      (printout t "No te preocupes, probaremos con otra rama para ver si es la mas adecuada para ti, en concreto la de IC"crlf)
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;USO LA MEDIA1 BAJA OTRO CAMINO
   (defrule uso_media1_baja_ti_otro_camino
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SUFICIENTE) (eq ?f BIEN)))
      ?g <- (empezar_por TI)
   =>
      (assert (empezar_por IC))
      (printout t "No te preocupes, probaremos con otra rama para ver si es la mas adecuada para ti"crlf)
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO ALTO
   (defrule uso_media1_alta_esfuerzo_alto_ti
      (tecnologia_web2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
   =>
      (assert (empezar_por CSI))
      (printout t "No te preocupes, al ver que no te gustan las tecnologias web y que tu nota y esfuerzo son muy buenos, probaremos con CSI para ver si es la mas adecuada para ti"crlf)
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO ALTO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_alto_ti_otro_camino
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ALTO)
      ?g <- (empezar_por TI)
   =>
      (assert (empezar_por CSI))
      (printout t "No te preocupes, al ver que no te gustan las redes y tu nota y esfuerzo son muy buenos, probaremos con CSI para ver si es la mas adecuada para ti"crlf)
      (pop-focus) (focus SaltoEntreRamas)
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
   =>
      (assert (empezar_por IC))
      (printout t "Sabiendo que entre otras cosas no te gutan las tecnologias web, probaremos con IC para ver si es la mas adecuada para ti"crlf)
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )

   ;USO LA MEDIA1 ALTA ESFUERZO BAJO OTRO CAMINO
   (defrule uso_media1_alta_esfuerzo_bajo_ti_otro_camino
      (redes2 NO)
      (nota_media ?f)
      (test (or (eq ?f SOBRESALIENTE) (eq ?f NOTABLE)))
      (esfuerzo ?h)
      (test (or (eq ?h BAJO) (eq ?h MEDIO)))
      ?g <- (empezar_por TI)
   =>
      (assert (empezar_por IC))
      (printout t "Sabiendo que entre otras cosas no te gustan las redes, probaremos con IC para ver si es la mas adecuada para ti"crlf)
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
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
      (printout t "Ya tenemos una decision sobre la rama mas adecuada para ti"crlf)
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
      (printout t "Conociendo tus respuestas, ya podemos tomar la decision sobre la rama mas recomendada para tu perfil"crlf)
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
      (printout t "Aunque no te gusten las tecnologias web, te gusta la parte multimedia de la informatica? (SI/NO/NO SE)"crlf)
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
      (printout t "Ademas de gustarte las tecnologias web, te gusta la parte multimedia de la informatica? (SI/NO/NO SE)"crlf)
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
      (printout t "A pesar de que no te gusten las tecnologias web, ya tenemos una recomendacion para ti"crlf)
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
      (printout t "Sabiendo que si te gustan las tecnologias web y a pesar de que tu esfuerzo en la carrera es bajo, ya tenemos una recomendacion de rama para ti"crlf)
      (assert (Consejo Tecnologias_de_la_Informacion Te_gusta_meyormente_el_contenido_de_la_rama_y_a_pesar_de_tener_media_alta_no_te_esfuerzas_mucho ManuelRodriguez))
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
      (printout t "Sabiendo que si te gusta el mundo de la multimedia informatica, ya tenemos una recomendacion de rama para ti"crlf)
      (assert (Consejo Tecnologias_de_la_Informacion Te_gusta_meyormente_el_contenido_de_la_rama JavierRamirez))
      (pop-focus) (focus Decision)
   )

   ;SI NO LE GUSTAN LAS ASIGNATURAS DE MULTIMEDIA
   (defrule multimedia_no_ti
      (multimedia NO)
      ?g <- (empezar_por TI)
   =>
      (assert (empezar_por CSI))
      (printout t "Sabiendo que no te gusta el mundo de la multimedia informatica, probaremos con otra rama para ver si es la mas adeucada para ti"crlf)
      (pop-focus) (focus SaltoEntreRamas)
      (retract ?g)
   )




; Modulo de control para saltar entre los modulos de las diferentes ramas
; En caso de estar en una que no esta cuadrando con el usuaria, recurrimos a este modulo para redirigirnos a otra diferente


(defmodule SaltoEntreRamas (import ModuloCSI deftemplate ?ALL)
                           (import ModuloSI deftemplate ?ALL)
                           (import ModuloIS deftemplate ?ALL)
                           (import ModuloIC deftemplate ?ALL)
                           (import ModuloTI deftemplate ?ALL)
                           (export deftemplate ?ALL))


   ;SI QUEREMOS IR A CSI Y PODEMOS
   (defrule saltamos_a_csi
      (empezar_por CSI)
      ?g <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (pop-focus) (focus ModuloCSI)
      (retract ?g)
   )    

   ;SI QUEREMOS IR A CSI Y NO PODEMOS
   (defrule no_saltamos_a_csi
      ?g <- (empezar_por CSI)
      (not (Rama Computacion_y_Sistemas_Inteligentes))
   =>
      (assert (rebota_csi))
      (retract ?g)
   )     

   ;SI QUEREMOS IR A SI Y PODEMOS
   (defrule saltamos_a_si
      (empezar_por SI)
      ?g <- (Rama Sistemas_de_Informacion)
   =>
      (pop-focus) (focus ModuloSI)
      (retract ?g)
   )    

   ;SI QUEREMOS IR A SI Y NO PODEMOS
   (defrule no_saltamos_a_si
      ?g <- (empezar_por SI)
      (not (Rama Sistemas_de_Informacion))
   =>
      (assert (rebota_si))
      (retract ?g)
   )  

   ;SI QUEREMOS IR A IS Y PODEMOS
   (defrule saltamos_a_is
      (empezar_por IS)
      ?g <- (Rama Ingenieria_del_Software)
   =>
      (pop-focus) (focus ModuloIS)
      (retract ?g)
   )    

   ;SI QUEREMOS IR A IS Y NO PODEMOS
   (defrule no_saltamos_a_is
      ?g <- (empezar_por IS)
      (not (Rama Ingenieria_del_Software))
   =>
      (assert (rebota_is))
      (retract ?g)
   )  

   ;SI QUEREMOS IR A IC Y PODEMOS
   (defrule saltamos_a_ic
      (empezar_por IC)
      ?g <- (Rama Ingenieria_de_Computadores) 
   =>
      (pop-focus) (focus ModuloIC)
      (retract ?g)
   )    

   ;SI QUEREMOS IR A IC Y NO PODEMOS
   (defrule no_saltamos_a_ic
      ?g <- (empezar_por IC)
      (not (Rama Ingenieria_de_Computadores))
   =>
      (assert (rebota_ic))
      (retract ?g)
   )  

   ;SI QUEREMOS IR A TI Y PODEMOS
   (defrule saltamos_a_ti
      (empezar_por TI)
      ?g <- (Rama Tecnologias_de_la_Informacion)
   =>
      (pop-focus) (focus ModuloTI)
      (retract ?g)
   )    

   ;SI QUEREMOS IR A TI Y NO PODEMOS
   (defrule no_saltamos_a_ti
      ?g <- (empezar_por TI)
      (not (Rama Tecnologias_de_la_Informacion))
   =>
      (assert (rebota_ti))
      (retract ?g)
   )  


   ;REBOTO DE CSI CON EXITO
   (defrule reboto_csi_exito
      (rebota_csi)
      ?g <- (Rama Ingenieria_del_Software)
   =>
      
      (pop-focus) (focus ModuloIS)
      (retract ?g)
      (assert (empezar_por IS))
   )

   ;REBOTO DE CSI Y FRACASO
   (defrule reboto_csi_fallo
      (rebota_csi)
      (not (Rama Ingenieria_del_Software))
   =>
      (assert (rebota_is))
   )    

   ;SI QUERIA IR A TI DESPUES DE NO PODER A CSI NI A IS, PRUEBO CON SI
   (defrule reboto_is_exito
      (rebota_is)
      ?h <- (Rama Sistemas_de_Informacion)
   =>
      (pop-focus) (focus ModuloSI)
      (retract ?h)
      (assert (empezar_por SI))
   ) 

   ;SI QUERIA IR A SI DESPUES DE NO PODER A CSI NI A IS NI A TI, PRUEBO CON IC
   (defrule reboto_is_fallo
      (rebota_is)
      (not (Rama Sistemas_de_Informacion))
   =>
      (assert (rebota_si))
   )  

   ;SI QUERIA IR A CSI Y AL FINAL NO PUDE REBOTAR A NINGUNA
   (defrule reboto_si_exito
      (rebota_si)
      ?h <- (Rama Tecnologias_de_la_Informacion)
   =>
      (pop-focus) (focus ModuloTI)
      (retract ?h)
      (assert (empezar_por TI))
   )   

   ;SI QUERIA IR A SI PERO NO PUDE, PRUEBO A REBOTAR CON TI
   (defrule reboto_si_fallo
      (rebota_si)
      (not (Rama Tecnologias_de_la_Informacion))
   =>
      (assert (rebota_ti))
   )

   ;SI FUI A TI REBOTANDO, PASAMOS A IC
   (defrule reboto_ti_exito
      (rebota_ti)
      ?h <- (Rama Ingenieria_de_Computadores)
   =>
      (pop-focus) (focus ModuloIC)
      (retract ?h)
      (assert (empezar_por IC))
   )   

   ; SI QUERIAMOS IR A TI PERO NO SE PUDO, PROBAMOS A REBOTAR A IC
   (defrule reboto_ti_fallo
      (rebota_ti)
      (not (Rama Ingenieria_de_Computadores))
   =>
      (assert (rebota_ic))
   )  

   ; SI REBOTAMOS CON EXITO A IC, VAMOS CSI
   (defrule reboto_ic_exito
      (rebota_ic)
      ?h <- (Rama Computacion_y_Sistemas_Inteligentes)
   =>
      (pop-focus) (focus ModuloCSI)
      (retract ?h)
      (assert (empezar_por CSI))
   )   

   ; SI FUI A IC REBOTANDO PERO NO ERA NUESTRA RAMA, PROBAMOS A REBOTAR A CSI
   (defrule reboto_ic_fallo
      (rebota_ic)
      (not (Rama Computacion_y_Sistemas_Inteligentes))
   =>
      (assert (rebota_csi))
   ) 

   ; EN CASO DE HABER REBOTADO EN TODAS SIN EXITO, VAMOS AL MODULO DE DECISION FINAL
   (defrule reboto_todas
      (rebota_ic)
      (rebota_is)
      (rebota_ti)
      (rebota_si)
      (rebota_csi)
   =>
      (pop-focus) (focus modulo_decision_final)
   ) 




; Modulo de decision final

; Es el modulo que nos va a dar la decision final de nuestro sistema
;     Si no hemos sido capaces de recomendar una de forma directa
;     preguntamos al usuario por la primera rama que se penso que podria ser (debido a sus gusto en asignatuas de otros años)
;     en caso positivo le recomendaremos esa rama, pero en otro caso le diremos que no ha sido posible recomendarle alguna
;     


(defmodule modulo_decision_final (import SaltoEntreRamas deftemplate ?ALL)
                            (export deftemplate ?ALL)) 

   ; Confirmacion de gusto por la rama de CSI si esta fue la primera que se penso que era la que mas se adaptaba pero despus ha rechazado todo
   (defrule confirmo_primera_csi
      (primero_fue Computacion_y_Sistemas_Inteligentes)
   =>
      (printout t "Necesito confirmar si la primera de tus opciones vuelve a ser la mas adecuada para ti. Te interesa el mundo de la inteligencia artificial? (SI/NO/NO SE)" crlf)
      (assert (confirmacion (read)))
   ) 

   ; Confirmacion de gusto por la rama de SI si esta fue la primera que se penso que era la que mas se adaptaba pero despus ha rechazado todo
   (defrule confirmo_primera_si
      (primero_fue Sistemas_de_Informacion)
   =>
      (printout t "Necesito confimar si la primera de tus opciones vuelve a ser la mas adecuada para ti. Te interesa el mundo del Big Data? (SI/NO/NO SE)" crlf)
      (assert (confirmacion (read)))
   ) 

   ; Confirmacion de gusto por la rama de IS si esta fue la primera que se penso que era la que mas se adaptaba pero despus ha rechazado todo
   (defrule confirmo_primera_is
      (primero_fue Ingenieria_del_Software)
   =>
      (printout t "Necesito confimar si la primera de tus opciones vuelve a ser la mas adecuada para ti. Te interesa el desarrollo de software? (SI/NO/NO SE)" crlf)
      (assert (confirmacion (read)))
   ) 

   ; Confirmacion de gusto por la rama de TI si esta fue la primera que se penso que era la que mas se adaptaba pero despus ha rechazado todo
   (defrule confirmo_primera_ti
      (primero_fue Tecnologias_de_la_Informacion)
   =>
      (printout t "Necesito confimar si la primera de tus opciones vuelve a ser la mas adecuada para ti. Te interesa el mundo de los servidores o las redes? (SI/NO/NO SE)" crlf)
      (assert (confirmacion (read)))
   ) 

   ; Confirmacion de gusto por la rama de IC si esta fue la primera que se penso que era la que mas se adaptaba pero despus ha rechazado todo
   (defrule confirmo_primera_ic
      (primero_fue Ingenieria_de_Computadores)
   =>
      (printout t "Necesito confimar si la primera de tus opciones vuelve a ser la mas adecuada para ti. Te interesa el mundo de la computacion y la arquitectura de sistemas? (SI/NO/NO SE)" crlf)
      (assert (confirmacion (read)))
   ) 

   ; En caso de que el usuario confirme la rama por la que empezamos a preguntarle en un principio, se la recomendamos como la final
   (defrule si_confirma
      (primero_fue ?f)
      (confirmacion SI)
   =>
      (assert (Consejo ?f A_raiz_de_tus_asignaturas_favoritas_y_tu_media_te_aconsejo_esta_rama ManuelRodriguez))
      (pop-focus) (focus Decision)
   ) 

   ; En el caso de que tambien rechace esta opcion, le indicamos que no ha habido una rama que le podamos aconsejar
   (defrule si_rechaza_o_duda
      (primero_fue ?f)
      (confirmacion NO)
   =>
      (assert (Consejo Ninguna No_hay_ninguna_rama_que_encaje_con_lo_que_te_gusta Dr.Manuel))
      (pop-focus) (focus Decision)
   ) 




; Modulo para mostrar por pantalla la decision y por tanto la rama aconsejada por nuestro sistema en caso de tenerla

(defmodule Decision (import modulo_decision_final deftemplate ?ALL))

   ; Si el experto no tiene una recomendacion, lo indicamos
   (defrule decision_final_ninguna
      (Consejo ?f ?g ?h)
      (test (eq ?f Ninguna))
   =>
      (printout t ?h " no te recomienda ninguna rama porque " ?g crlf)
   ) 

   ; En caso de tener una decision, la indicamos junto a su consejo
   (defrule decision_final
      (Consejo ?f ?g ?h)
      (test (neq ?f Ninguna))
   =>
      (printout t ?h " te recomienda la rama " ?f " porque " ?g crlf)
   ) 












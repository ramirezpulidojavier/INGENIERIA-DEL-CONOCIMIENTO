;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   Modulo ejecutar sistema difuso con varias variables de entrada   ;;;;;;;
;;;;;       MODULO CALCULO FUZZY (modulo calculo_fuzzy)      ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;  Copywright Juan Luis Castro  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCIONES NECESARIAS  ;;;;;;;;;;;;;;;;;;;;;;;

; Membership es una funcion que calcul el el grado de pertenencia de value al conjunto difuso 
;trapezoidal (a b c d). 
(deffunction membership (?value ?a ?b ?c ?d)
(if  (< ?value ?a) then (bind ?rv 0)
	else 
	   (if (< ?value ?b) then (bind ?rv (/ (- ?value ?a) (- ?b ?a)))
           else
             (if  (< ?value ?c) then (bind ?rv 1)
                 else
                   (if (< ?value ?d) then (bind ?rv (/ (- ?d ?value) (- ?d ?c)))
                        else (bind ?rv 0)
                   )				   
             )			  
       )
)
?rv
)

; center_of_gravity es una función que calcula el centro de gravedad del conjunto difuso
;trapezoidal (a b c d).
(deffunction center_of_gravity (?a ?b ?c ?d)
(bind ?ati (/ (- ?b ?a) 2))
(bind ?atd (/ (- ?d ?c) 2))
(bind ?rv (/ (+ (+ ?b ?c) (- ?atd ?ati)) 2))
?rv
)

(deffunction conjuncion (?x ?y)
(bind ?rv (* ?x ?y))
?rv
)

 
;;;;;;;;;;;;;;;;;;;;;;;;;; CONOCIMIENTO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Entradas: Peso y Temperatura de un enfermo  (dato ?variable ?valor)
; Salidad: dosis de medicamento a administrar (fuzzy inference ?variable ?valor)
;;;;;;;;;;;;Conocimiento ;;;;;;;;;;;;;;;;; 
;; Si la tempetura es normal no administrar medicamento
;; Si esté destemplado y el peso es medio o alto administrar dosis media
;; Si está destemplado y el peso es bajo administrar dosis baja
;; Si la temperatura es alta administrar dosis alta 

;; Definimos de las variables del sistema

(deffacts variables_difusas
(variable temperatura)
(variable peso)
(variable dosis)
)

;;  Definimos los conjuntos difusos que usará el sistema

(deffacts conjuntos_difusos
(cd temperatura normal 36 36 37.1 37.4)   ; CAMBIADO
(cd temperatura intermedia 37 37.4 38.5 38.9)  ; CAMBIADO
(cd temperatura alta 38.7 39 41.2 41.5)  ; CAMBIADO
(cd temperatura hiperpirexia 40.5 41.5 100 100)  ; CAMBIADO

(cd peso bajo 0 0 40 50)     ; CAMBIADO
(cd peso medio 47 50 80 85)  ; CAMBIADO
(cd peso alto 80 90 300 300) ; CAMBIADO

(cd edad joven 0 0 16 18)     ; CAMBIADO
(cd edad adulto 16 18 60 65)  ; CAMBIADO
(cd edad anciano 60 65 120 120) ; CAMBIADO

(cd dosis cero 0 0 0 0)
(cd dosis baja 4 5 5 6.5)    ; aproximadamente 5
(cd dosis media 6 7 7 8)     ; aproximadamente 7
(cd dosis alta 8 9 9 10)     ; aprximadamente 9
)

;; Definimos las reglas y las explicaciones asociadas

(deffacts reglas
(regla 1 antecedente temperatura normal)
(regla 1 consecuente dosis cero)
(regla 1 explicacion "Si la temperatura es normal, la dosis a aplicar es cero")

(regla 2 antecedente temperatura intermedia)
(regla 2 antecedente peso medio)
(regla 2 antecedente edad joven)
(regla 2 consecuente dosis baja)
(regla 2 explicacion "Si tiene algo de fiebre, el peso es medio y es joven, la dosis a aplicar es baja")

(regla 3 antecedente temperatura intermedia)
(regla 3 antecedente peso alto)
(regla 3 antecedente edad joven)
(regla 3 consecuente dosis media)
(regla 3 explicacion "Si tiene algo de fiebre, el peso es medio y es joven, la dosis a aplicar es media")

(regla 4 antecedente temperatura intermedia)
(regla 4 antecedente peso bajo)
(regla 4 antecedente edad adulto)
(regla 4 consecuente dosis baja)
(regla 4 explicacion "Si tiene algo de fiebre y es un adulto muy delgado, la dosis a aplicar es baja")

(regla 5 antecedente temperatura intermedia)
(regla 5 antecedente peso medio)
(regla 5 antecedente edad adulto)
(regla 5 consecuente dosis media)
(regla 5 explicacion "Si tiene algo de fiebre y es un adulto en su peso, la dosis a aplicar es media")

(regla 6 antecedente temperatura intermedia)
(regla 6 antecedente peso medio)
(regla 6 antecedente edad anciano)
(regla 6 consecuente dosis baja)
(regla 6 explicacion "Si tiene algo de fiebre pero es una persona mayor, la dosis a aplicar es baja")

(regla 7 antecedente temperatura intermedia)
(regla 7 antecedente peso alto)
(regla 7 antecedente edad anciano)
(regla 7 consecuente dosis media)
(regla 7 explicacion "Si tiene algo de fiebre pero es una persona mayor de mucho peso, la dosis a aplicar es media")

(regla 8 antecedente temperatura alta)
(regla 8 antecedente peso medio)
(regla 8 antecedente edad joven)
(regla 8 consecuente dosis media)
(regla 8 explicacion "Si tiene temperatura alta, el peso es medio y es joven, la dosis a aplicar es media")

(regla 9 antecedente temperatura alta)
(regla 9 antecedente peso alto)
(regla 9 antecedente edad joven)
(regla 9 consecuente dosis alta)
(regla 9 explicacion "Si tiene temperatura alta, tiene un peso elevado y es joven, la dosis a aplicar es alta")

(regla 10 antecedente temperatura alta)
(regla 10 antecedente peso bajo)
(regla 10 antecedente edad adulto)
(regla 10 consecuente dosis media)
(regla 10 explicacion "Si tiene fiebre y es un adulto muy delgado, la dosis a aplicar es media")

(regla 11 antecedente temperatura alta)
(regla 11 antecedente peso medio)
(regla 11 antecedente edad adulto)
(regla 11 consecuente dosis alta)
(regla 11 explicacion "Si tiene fiebre y es un adulto en su peso, la dosis a aplicar es alta")

(regla 12 antecedente temperatura alta)
(regla 12 antecedente peso medio)
(regla 12 antecedente edad anciano)
(regla 12 consecuente dosis alta)
(regla 12 explicacion "Si tiene fiebre pero es una persona mayor en el peso medio, la dosis a aplicar es alta")

(regla 13 antecedente temperatura alta)
(regla 13 antecedente peso alto)
(regla 13 antecedente edad anciano)
(regla 13 consecuente dosis alta)
(regla 13 explicacion "Si fiebre pero es una persona mayor de mucho peso, la dosis a aplicar es alta")

(regla 14 antecedente temperatura hiperpirexia)
(regla 14 consecuente dosis alta)
(regla 14 explicacion "Si la temperatura es tan elevada, independientemente del resto de factores, hace falta una dosis alta")


)


; Obtenemos el cumplimiento de cada conjunto difuso

(defrule cumplimiento_predicado_difuso
(declare (salience 3))
(modulo calculo_fuzzy)
(cd ?v ?l ?a ?b ?c ?d)
(dato ?v ?x)
=>
(bind ?g (membership ?x ?a ?b ?c ?d))
(assert (fuzzy cumplimiento ?v ?l ?g))
(if (> ?g 0) then (printout t ?v " es " ?l " en grado " ?g crlf))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Obtenemos el matching de cada antecedente de cada regla
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule matching_antecedente_simple
(declare (salience 2))
(modulo calculo_fuzzy)
(regla ?r antecedente ?v ?l)
(fuzzy cumplimiento ?v ?l ?g)
=>
(assert (fuzzy matching ?r ?g ?v))
)

(defrule matching_antecedente_1
(declare (salience 2))
(modulo calculo_fuzzy)
?f <- (fuzzy matching ?r ?g ?v)
(not (fuzzy matching_antecedente_regla ?r ?))
=>
(assert (fuzzy matching_antecedente_regla ?r ?g))
(retract ?f)
)

(defrule matching_antecedente
(declare (salience 2))
(modulo calculo_fuzzy)
?f <- (fuzzy matching ?r ?g ?v)
?h<- (fuzzy matching_antecedente_regla ?r ?g1)
=>
(retract ?f ?h)
(assert (fuzzy matching_antecedente_regla ?r (conjuncion ?g1 ?g)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inferimos con su correspondiente grado los consecuentes de las reglas que hacen algun matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule inferencia_difusa
(declare (salience 1))
(modulo calculo_fuzzy)
(fuzzy matching_antecedente_regla ?r ?g1)
(test (> ?g1 0))
(regla ?r consecuente ?v ?l)
(regla ?r explicacion ?text)
=>
(assert (fuzzy inferido ?v ?l ?g1))
(printout t "Se va a aplicar la regla " ?text  crlf)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Obtenemos el valor deducido como la media ponderada (por los grados de cada consecuente) 
;;;  de los centros de gravedad de los consecuentes inferidos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule concrecion_individual
(modulo calculo_fuzzy)
(fuzzy inferido ?v ?l ?g1)
(cd ?v ?l ?a ?b ?c ?d)
=>
(assert (fuzzy sumando_numerador ?v (* ?g1 (center_of_gravity ?a ?b ?c ?d))))
(assert (fuzzy sumando_denominador ?v ?g1))
)

(defrule concrecion_numerador
(modulo calculo_fuzzy)
?g<- (fuzzy numerador ?v ?x)
?f <- (fuzzy sumando_numerador ?v ?y)
=>
(assert (fuzzy numerador ?v (+ ?x ?y)))
(retract ?f ?g)
)

(defrule concrecion_denominador
(modulo calculo_fuzzy)
?g<- (fuzzy denominador ?v ?x)
?f <- (fuzzy sumando_denominador ?v ?y)
=>
(assert (fuzzy denominador ?v (+ ?x ?y)))
(retract ?f ?g)
)

(defrule respuesta
(declare (salience -1))
(modulo calculo_fuzzy)
(fuzzy numerador ?v ?n)
(fuzzy denominador ?v ?d)
(test (> ?d 0))
=>
(assert (fuzzy valor_inferido ?v (/ ?n ?d)))
(printout t "Aplicando esta(s) regla(s) el valor de " ?v " es " (/ ?n ?d)  crlf)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  Una vez inferido el valor salimos del modulo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule salir
(declare (salience -2))
?f <- (modulo calculo_fuzzy)
=>
(retract ?f)
)

;;;;;;;;;;;;;;;;;;;; INICIALIZACION DE LOS VALORES CUANDO SE ENTRA AL MODULO  ;;;;;;;;;;

(defrule iniciar_proceso
(declare (salience 5))
(modulo calculo_fuzzy)
=>
(assert (borrar_datos_ejecucion_anterior))
)

(defrule borrar_datos_ejecucion_anterior
(declare (salience 5))
(modulo calculo_fuzzy)
(borrar_datos_ejecucion_anterior)
?f <- (fuzzy $?)
=>
(retract ?f)
)

(defrule borrar_borrar_datos
(declare (salience 5))
(modulo calculo_fuzzy)
?f<- (borrar_datos_ejecucion_anterior)
(not (fuzzy $?))
=>
(retract ?f)
)

(defrule inicializar_fuzzy_inference
(declare (salience 4))
(modulo calculo_fuzzy)
(regla ? consecuente ?v ?)
=>
(assert (fuzzy numerador ?v 0))
(assert (fuzzy denominador ?v 0))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  PARA COMPROBARLO   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  Reglas para pedir los datos y entrar en el modulo ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule pregunta1
(declare (salience 1)) 
=>
(printout t "Temperatura: ")
(assert (dato temperatura (read)))
) 

(defrule pregunta2 
(declare (salience 1)) 
=>
(printout t "Peso: ")
(assert (dato peso (read)))
)

(defrule pregunta3 
(declare (salience 1)) 
=>
(printout t "Edad: ")
(assert (dato edad (read)))
)

(defrule pregunta4
=>
(assert (modulo calculo_fuzzy))
)

;Las aves y los mamíferos son animales
;Los gorriones, las palomas, las águilas y los pingüinos son aves
;La vaca, los perros y los caballos son mamíferos
;Los pingüinos no vuelan

(deffacts datos
	(ave gorrion) (ave paloma) (ave aguila) (ave pinguino)
	(mamifero vaca) (mamifero perro) (mamifero caballo)
	(vuela pinguino no seguro) 
)

;;; La mayor parte de los animales no vuelan --> puede interesarme asumir por defecto que un animal no va a volar
(defrule pregunta_por_animal
	(not (animal_preguntado ?))
	=>
	(printout t "Introduce el nombre de algun animal" crlf)
	(assert (animal_preguntado (read))) 

)

;;; HA INTRODUCIDO UN ANIMAL DE LOS QUE SABEMOS SI VUELAN O NO
(defrule animal_almacenado
	(animal_preguntado ?x)
	(test (or (eq ?x gorrion) (eq ?x paloma) (eq ?x aguila) (eq ?x pinguino) (eq ?x caballo) (eq ?x perro) (eq ?x vaca)))
	(vuela ?x ?f ?g) 
	(explicacion vuela ?x ?h)
	(explicacion animal ?x ?expl)
	=>
	(printout t ?expl crlf)
	(printout t ?h crlf)
)

;;; PREGUNTAMOS SI ES MAMIFERO, AVE O NO SABE
(defrule animal_no_almacenado
	(not (tipo_animal ?h))
	(animal_preguntado ?x)
	(test (and (neq ?x gorrion) (neq ?x paloma) (neq ?x aguila) (neq ?x pinguino) (neq ?x caballo) (neq ?x perro) (neq ?x vaca))) 
	=>
	(printout t "El animal " ?x " es un mamifero o un ave? (mamifero/ave/desconocido)" crlf)
	(assert (tipo_animal ?x (read)))
)


	;;;COMPRUEBA QUE EL TIPO DE ANIMAL ES CORRECTO
   (defrule comprobacion_tipo_animal
      ?g <- (tipo_animal ?x ?h)
      (test (and (neq ?h mamifero) (neq ?h ave) (neq ?h desconocido)))
   =>
      (printout t "El tipo introducido no es correcto, prueba de nuevo (mamifero/ave/desconocido) " crlf)
      (retract ?g)
      (assert (tipo_animal ?x (read)))
    )

   	;;; SI CONOCE EL TIPO DE ANIMAL, CREAMOS SU HECHO PARA QUE SEPAMOS SI VUELA O NO
   	(defrule tipo_animal_conocido_ave
      (tipo_animal ?x ave)
   	=>
      (assert (ave ?x))
    )

    ;;; SI CONOCE EL TIPO DE ANIMAL, CREAMOS SU HECHO PARA QUE SEPAMOS SI VUELA O NO
   	(defrule tipo_animal_conocido_mamifero
      (tipo_animal ?x mamifero)
   	=>
      (assert (mamifero ?x))
    )

    ;;; SI CONOCE EL TIPO DE ANIMAL, CREAMOS SU HECHO PARA QUE SEPAMOS SI VUELA O NO
   	(defrule tipo_animal_desconocido
      (tipo_animal ?x desconocido)
   	=>
      (assert (animal ?x))
    )

; Las aves son animales
(defrule aves_son_animales
	(ave ?x)
	=>
	(assert (animal ?x))
	(bind ?expl (str-cat "sabemos que un " ?x " es un animal porque las aves son  un tipo de animal"))
	(assert (explicacion animal ?x ?expl)) 
) ; añadimos un hecho que contiene la explicación de la deducción

; Los mamiferos son animales 
(defrule mamiferos_son_animales
	(mamifero ?x)
=>
	(assert (animal ?x))
	(bind ?expl (str-cat "sabemos que un " ?x " es un animal porque los mamiferos son un tipo de animal"))
	(assert (explicacion animal ?x ?expl)) 
) ;añadimos un hecho que contiene la explicación de la deducción


;;; Casi todos las aves vuela --> puedo asumir por defecto que las aves vuelan
; Asumimos por defecto
(defrule ave_vuela_por_defecto
	(declare (salience -1)) ; para disminuir probabilidad de añadir erróneamente
	(ave ?x)
	=>
	(assert (vuela ?x si por_defecto))
	(bind ?expl (str-cat "asumo que un " ?x " vuela, porque casi todas las aves vuelan"))
	(assert (explicacion vuela ?x ?expl)) 
)


;Regla por defecto: retracta
;;; COMENTARIO: esta regla también elimina los por defecto cuando ya esta seguro
; Retractamos cuando hay algo en contra
(defrule retracta_vuela_por_defecto
	(declare (salience 1)) ; para retractar antes de inferir cosas erroneamente
	?f<- (vuela ?x ?r por_defecto)
	(vuela ?x ?s seguro)
	=>
	(retract ?f)
	(bind ?expl (str-cat "retractamos que un " ?x ?r " vuela por defecto, porque sabemos seguro que " ?x ?s " vuela"))
	(assert (explicacion retracta_vuela ?x ?expl)) 
)


;;; La mayor parte de los animales no vuelan --> puede interesarme asumir por defecto que un animal no va a volar
(defrule mayor_parte_animales_no_vuelan
	(declare (salience -2)) ;;;; es mas arriesgado, mejor después de otros razonamientos
	(animal ?x)
	(not (vuela ?x ? ?))
	=>
	(assert (vuela ?x no por_defecto))
	(bind ?expl (str-cat "asumo que " ?x " no vuela, porque la mayor parte de los animales no vuelan"))
	(assert (explicacion vuela ?x ?expl)) 
)

;;; HA INTRODUCIDO UN ANIMAL QUE NO CONOCIAMOS PERO NOS HA DICHO EL TIPO
(defrule animal_no_almacenado_inferido
	(tipo_animal ?f desconocido)
	(vuela ?f ?y ?z)
	(explicacion vuela ?f ?h)
	=>
	(printout t ?h crlf)
)

;;; HA INTRODUCIDO UN ANIMAL QUE NO CONOCIAMOS PERO NOS HA DICHO EL TIPO
(defrule animal_no_almacenado_inferido_desconocido
	(tipo_animal ?f ?g)
	(vuela ?f ?y ?z)
	(explicacion vuela ?f ?h)
	(explicacion animal ?f ?expl)
	=>
	(printout t ?expl crlf)
	(printout t ?h crlf)
)




;;;;;;;;;;;EJERCICIO

;Completar esta base de conocimiento para que el sistema pregunte que de qué animal esta interesado en obtener información sobre si vuela y:
	; - si es uno de los recogidos en el conocimiento indique si vuela o no
	; - si no es uno de los recogidos pregunte si es un ave o un mamífero y según la respuesta indique si vuela o no.
	; - Si no se sabe si es un mamífero o un ave también responda según el razonamiento por defecto indicado
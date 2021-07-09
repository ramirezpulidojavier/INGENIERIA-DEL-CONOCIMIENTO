	;(load "C:/Users/xaviv/Desktop/JAVIER/UNIVERSIDAD/CURSO 20.21/CURSO/2CUATRI/INGENIERIA DEL CONOCIMIENTO/PRACTICA/PRACTICA2/APARTADO 2/FactoresDeCerteza.clp")

	;;;;;;;;;;;PARTE EN LA QUE PREGUNTA SI EL MOTOR HACE INTENTOS DE ARRANCAR
  (defrule intentos_de_arrancar

  =>
    (printout t "El motor hace intentos de arrancar? (Responde: si/no)" crlf)
    (assert (Evidencia hace_intentos_arrancar (read)))
    (assert (mayor_hipotesis a 0))
  )

	;;;COMPRUEBA QUE LA RESPUESTA SOBRE EL INTENTO DE ARRANCAR ES CORRECTA
  (defrule comprobacion_intentos_de_arrancar
    ?g <- (Evidencia hace_intentos_arrancar ?x)
    (test (and (neq ?x si) (neq ?x no) ))
  =>
    (printout t "La respuesta no es correcta, tienes que introducir si o no " crlf)
    (retract ?g)
    (assert (Evidencia hace_intentos_arrancar (read)))
  )

  ;;;;;;;;;;;PARTE EN LA QUE PREGUNTA SI HAY GASOLINA EN EL DEPOSITO 
  (defrule gasolina_deposito

  =>
    (printout t "Hay gasolina en el deposito? (Responde: si/no)" crlf)
    (assert (Evidencia hay_gasolina_en_deposito (read)))
  )

	;;;COMPRUEBA QUE LA RESPUESTA SOBRE LA GASOLINA EN EL DEPOSITO ES CIERTA
  (defrule comprobacion_gasolina_deposito
    ?g <- (Evidencia hay_gasolina_en_deposito ?x)
    (test (and (neq ?x si) (neq ?x no) ))
  =>
    (printout t "La respuesta no es correcta, tienes que introducir si o no " crlf)
    (retract ?g)
    (assert (Evidencia hay_gasolina_en_deposito (read)))
  )

  ;;;;;;;;;;;PARTE EN LA QUE PREGUNTA SI SE ENCIENDEN LAS LUCES
  (defrule luces_encienden

  =>
    (printout t "Las luces se encienden? (Responde: si/no)" crlf)
    (assert (Evidencia encienden_las_luces (read)))
  )

	;;;COMPRUEBA QUE LA RESPUESTA SOBRE SI LAS LUCES ENCIENDEN ES CORRECTA
  (defrule comprobacion_luces_encienden
    ?g <- (Evidencia encienden_las_luces ?x)
    (test (and (neq ?x si) (neq ?x no) ))
  =>
    (printout t "La respuesta no es correcta, tienes que introducir si o no " crlf)
    (retract ?g)
    (assert (Evidencia encienden_las_luces (read)))
  )

  ;;;;;;;;;;;PARTE EN LA QUE PREGUNTA SI EL MOTOR GIRA
  (defrule gira_motor

  =>
    (printout t "El motor gira? (Responde: si/no)" crlf)
    (assert (Evidencia gira_motor (read)))
  )

	;;;COMPRUEBA QUE LA RESPUESTA SOBRE SI EL MOTOR GIRA ES CORRECTA
  (defrule comprobacion_gira_motor
    ?g <- (Evidencia gira_motor ?x)
    (test (and (neq ?x si) (neq ?x no) ))
  =>
    (printout t "La respuesta no es correcta, tienes que introducir si o no " crlf)
    (retract ?g)
    (assert (Evidencia gira_motor (read)))
  )


;;; convertimos cada evidencia en una afirmación sobre su factor de certeza
(defrule certeza_evidencias
	(Evidencia ?e ?r)
	=>
	(assert (FactorCerteza ?e ?r 1)) 
)

(deffunction encadenado (?fc_antecedente ?fc_regla)
	
	(if (> ?fc_antecedente 0) then
	
		(bind ?rv (* ?fc_antecedente ?fc_regla)) 
	
	else 
	
		(bind ?rv 0) 
	
	)

?rv)

; R1: SI el motor obtiene gasolina Y el motor gira ENTONCES problemas 
; con las bujías con certeza 0,7
(defrule R1
	(FactorCerteza motor_llega_gasolina si ?f1)
	(FactorCerteza gira_motor si ?f2)
	(test (and (> ?f1 0) (> ?f2 0))) 
	=>
	(assert (FactorCerteza problema_bujias si (encadenado (* ?f1 ?f2) 0.7)))
	(bind ?expl (str-cat "se debe a que llega gasolina al motor y este no gira"))
	(assert (explicacion problemas_bujias ?expl))
)


; R2: NO GIRA EL MOTOR -> PROBLEMAS CON STARTER AL 0.8
(defrule R2
	(FactorCerteza gira_motor no ?f1)
	(test (> ?f1 0)) 
	=>
	(assert (FactorCerteza problema_starter si 0.8))
	(bind ?expl (str-cat "se debe a que el motor no gira"))
	(assert (explicacion problema_starter ?expl))
)

; R3: SI NO ENCIENDEN LAS LUCES -> PROBLEMAS CON BATERIA AL 0.9
(defrule R3
	(FactorCerteza encienden_las_luces no ?f1)
	(test (> ?f1 0))
	=>
	(assert (FactorCerteza problema_bateria si 0.9))
	(bind ?expl (str-cat "se debe a que las luces no encienden"))
	(assert (explicacion problema_bateria ?expl))
)


; R4: SI HAY GASOLINA EN EL DEPOSITO -> NO PROBLEMA CON LA OBTENCION DE GASOLINA AL 0.9
(defrule R4
	(FactorCerteza hay_gasolina_en_deposito si ?f1)
	(test (> ?f1 0) ) 
	=>
	(assert (FactorCerteza motor_llega_gasolina si 0.9))
	(bind ?expl (str-cat "se debe a que hay gasolina en el deposito"))
	(assert (explicacion motor_llega_gasolina ?expl))
)


; R5: SI HACE INTENTOS DE ARRANCAR -> PROBLEMA CON EL STARTER AL -0.6
(defrule R5
	(FactorCerteza hace_intentos_arrancar si ?f1)
	(test (> ?f1 0)) 
	=>
	(assert (FactorCerteza problema_starter si -0.6))
)


; R6: SI HACE INTENTOS DE ARRANCAR -> PROBLEMA CON BATERIA 0,5
(defrule R6
	(FactorCerteza hace_intentos_arrancar si ?f1)
	(test (> ?f1 0)) 
	=>
	(assert (FactorCerteza problema_bateria si 0.5))
	(bind ?expl (str-cat "se debe a que hace intentos de arrancar"))
	(assert (explicacion problema_bateria ?expl))
)


(deffunction combinacion (?fc1 ?fc2)
	
	(if (and (> ?fc1 0) (> ?fc2 0) ) then
	
		(bind ?rv (- (+ ?fc1 ?fc2) (* ?fc1 ?fc2) ) )

	else 
	
		(if (and (< ?fc1 0) (< ?fc2 0) )	then
	
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


;;;NOS QUEDAMOS SOLAMENTE CON LOS FACTORESCERTEZAS QUE SEAN DE PROBLEMAS
  (defrule factorcerteza_problema
  	(declare (salience -9996))
    (FactorCerteza ?c ?x ?f)
    (test (or (eq ?c problema_bateria) (eq ?c problema_bujias) (eq ?c problema_starter) ))

  =>
    (assert (a_evaluar ?c ?f))
  )

 ;;;NOS QUEDAMOS SOLAMENTE CON LOS FACTORESCERTEZAS QUE SEAN DE PROBLEMAS
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
    (printout t "A evaluar " ?c" " ?f crlf)
  )

  (defrule muestra_mayor
  	(declare (salience -9999))
    (not (hay_empate))
    (mayor_hipotesis ?x ?y)
    (explicacion ?x ?g)

  =>
    (printout t "La mayor hipotesis " ?x" y "?g crlf)
  )

  (defrule muestra_empate
  	(declare (salience -9999))
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
  )

  (defrule ningun_problema
  	(declare (salience -9999))
  	(mayor_hipotesis ?l 0)

  =>
    (printout t "No hemos podido detectar ningun error a raiz de las evidencias indicadas" crlf)
    
  )



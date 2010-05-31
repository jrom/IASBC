; Principal

(defmodule MAIN (export ?ALL))

;; TEMPLATES

(deftemplate lector
	(slot nom)
	(slot edat)
	(slot sexe)
	(slot llengua)
	(slot ocupacio)
)

;; Lector per defecte

;; Tot ha de ser desconegut pero per simplificar en development faig un cas on ja esta definit
(deffacts tipus-lector
	(lector
;		(nom desconegut)
;		(edat desconegut)
;		(sexe desconegut)
;		(llengua desconegut)
;		(ocupacio desconegut)
		(nom Jordi)
		(edat 23)
		(sexe home)
		(llengua catala)
		(ocupacio estudiant)
		)
)

;; Funcions

(deffunction pregunta-text (?pregunta)
	(printout t crlf ?pregunta)
	(bind ?resposta (read))
	?resposta
)

(deffunction pregunta-opts (?pregunta $?respostes)
	(bind ?pregunta (str-cat ?pregunta " (" (implode$ ?respostes) ") "))
	(bind ?resposta (pregunta-text ?pregunta))
	(while (not (member ?resposta ?respostes)) do
		(printout t ?pregunta)
		(bind ?resposta (read))
	)
	?resposta
)

(deffunction pregunta-sino (?pregunta)
	(bind ?resposta (pregunta-opts ?pregunta si no))
	(if (eq (lowcase ?resposta) si)
		then TRUE
		else FALSE
	)
)

(deffunction pregunta-mp (?pregunta)
	(bind ?resposta (pregunta-opts ?pregunta molt bastant indiferent poc gens ))
	?resposta
)

(deffunction pregunta-num (?pregunta)
	(bind ?pregunta (str-cat ?pregunta " (número) "))
	(printout t crlf ?pregunta)
	(bind ?resposta (read))
	(while (not (integerp ?resposta))
		(printout t ?pregunta)
		(bind ?resposta (read))
	)
	?resposta
)

(defrule banner "Banner"
	(declare (salience 10))
	=>
	(printout t crlf)
	(printout t "============ INICI =============" crlf)
	(focus preguntes-perfil-lector)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PERFIL LECTOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule preguntes-perfil-lector "Perfil lector"
	(import MAIN ?ALL)
	(export ?ALL)
)

;;; Edat
(defrule determinar-nom
	?l <- (lector (nom desconegut))
	=>
	(modify ?l (nom (pregunta-text "Nom? ")))
)

(defrule determinar-edat
	?l <- (lector (edat desconegut))
	=>
	(bind ?edat(pregunta-num "Edat? "))
	(if (< ?edat 14)
	then
		(modify ?l (edat nen))
	else
		(if (< ?edat 30)
		then
			(modify ?l (edat jove))
		else
			(if (< ?edat 60)
			then
				(modify ?l (edat adult))
			else
				(modify ?l (edat gran))
			)
		)
	)
)

(defrule determinar-sexe
	?l <- (lector (sexe desconegut))
	=>
	(modify ?l (sexe (pregunta-opts "Sexe?" home dona)))
)

(defrule determinar-llengua
	?l <- (lector (llengua desconegut))
	=>
	(modify ?l (llengua (pregunta-opts "Llengua materna?" catala castella altres)))
)

(defrule determinar-ocupacio
	?l <- (lector (ocupacio desconegut))
	=>
	(modify ?l (ocupacio (pregunta-opts "Ocupació?" estudiant academic professional desocupat altres)))
)


;;; Saltem a preguntes comunes
(defrule a-preguntes-comunes
	(declare (salience -1))
	=>
	(focus preguntes-comunes)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PREGUNTES COMUNES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule preguntes-comunes "Preguntes comunes"
	(import preguntes-perfil-lector ?ALL)
	(export ?ALL)
)


(defrule frequencia
	=>
	(assert (frequencia (pregunta-opts "En quina freqüència acostumes a llegir?" diaria setmanal mensual indiferent)))
)

(defrule tempslectura
	(frequencia diaria | setmanal | mensual)
	=>
	(assert (frequencia (pregunta-num "Quantes hores acostumes a llegir en aquesta freqüència?")))
)

(defrule genere-preferit
	=>
	(bind ?generes "")
	(printout t "Gèneres disponibles:" crlf)
	(do-for-all-instances
		((?genere Genere))
		TRUE
		(printout t " | " ?genere:nom crlf)
		(bind ?generes (str-cat ?generes " " ?genere:nom))
	)
	(bind ?resposta (pregunta-opts "Gènere: " (explode$ ?generes)))
)


(defrule experimentar
	=>
	(assert (experimentar (pregunta-mp "T'agrada experimentar amb nous gèneres?")))
)

(defrule lloc
	=>
	(assert (lloc (pregunta-opts "On llegeixes habitualment?" casa exterior transport indiferent)))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FINAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule bannerfilan
	(declare (salience -1))
	=>
	(printout t "============ FINAL =============" crlf crlf)
)

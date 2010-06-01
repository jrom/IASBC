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

(deftemplate recomanacio
	(slot isbn)
	(slot qualitat)
	(slot moltadequat)
	(slot adequat)
	(slot inadequat)
	(slot moltinadequat)
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

;;; Pregunta on li entres una llista i accepta numeros com a respostes
(deffunction pregunta-llista-index (?pregunta $?respostes)
	(bind ?linia (format nil "%s" ?pregunta))
	(printout t crlf ?linia crlf)
	(progn$ (?var ?respostes) 
			(bind ?linia (format nil "  %d. %s" ?var-index ?var))
			(printout t ?linia crlf)
	)
	(format t "%s" "Indica el numero corresponent: ")
	(bind ?resp (read))
	(bind $?llista (create$ ))
	(if (and (integerp ?resp) (and (>= ?resp 1) (<= ?resp (length$ ?respostes))))
		then 
			(bind ?elem (nth$ ?resp ?respostes))
			(if (not (member$ ?elem ?llista))
				then (bind ?llista (insert$ ?llista 1 ?elem))
			)
			else
			(bind ?llista (pregunta-llista-index ?pregunta ?respostes))
		) 
	)
	?llista
)

(deffunction getnom (?llibre)
	(bind ?genere (send ?llibre get-genere))
	(bind ?nom (send ?genere get-nomGenere))
	?nom
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
	(do-for-all-instances
		((?genere Genere))
		TRUE
		(bind ?generes (str-cat ?generes " " ?genere:nomGenere))
	)
	(bind ?generes (str-cat ?generes " indiferent"))
	(bind ?resposta (pregunta-opts "Gènere: " (explode$ ?generes)))
	(assert (genere-preferit ?resposta))
)

(defrule experimentar
	=>
	(assert (experimentar (pregunta-mp "T'agrada experimentar amb nous gèneres?")))
)

(defrule lloc
	=>
	(assert (lloc (pregunta-opts "On llegeixes habitualment?" casa exterior transport indiferent)))
)

(defrule preu
	=>
	(assert (preu (pregunta-mp "El preu és un factor prioritari?")))
)

(defrule enquadernacio
	=>
	(assert (enquadernacio (pregunta-mp "Prefereixes els llibres ben enquadernats?")))
)

(defrule contingut-explicit
	=>
	(assert (contingut-explicit (pregunta-mp "Prefereixes els llibres amb contingut explícit (violència o sexe)?")))
)

(defrule actual
	=>
	(assert (actual (pregunta-mp "Prefereixes els llibres actuals?")))
)

(defrule bestseller
	=>
	(assert (bestseller (pregunta-mp "T'agraden els best-sellers?")))
)

(defrule gruixuts
	=>
	(assert (gruixuts (pregunta-mp "Estàs disposat a llegir llibres gruixuts?")))
)

(defrule vocabulari-facil
	=>
	(assert (vocabulari-facil (pregunta-mp "Prefereixes que el vocabulari usat sigui senzill?")))
)

(defrule notes
	=>
	(assert (notes (pregunta-mp "T'agrada prendre notes al costat dels llibres?")))
)

(defrule personatges
	=>
	(assert (personatges (pregunta-mp "T'agraden els llibres amb molts personatges?")))
)

(defrule nacionalitat
	=>
	(assert (nacionalitat (pregunta-opts "Quina és la teva preferència per la nacionalitat de l'autor?" catala espanyol catala-espanyol estranger indiferent)))
)

(defrule llengua
	=>
	(assert (llengua (pregunta-opts "En quina llengua prefereixes llegir?" catala castella altres indiferent)))
)

(defrule autor-preferit
	=>
	(bind ?autors (create$))
	(do-for-all-instances
		((?autor Autor))
		TRUE
		(bind ?autors (insert$ ?autors 1 ?autor:nomAutor))
	)
	(bind ?autors (insert$ ?autors 1 "Cap dels anteriors"))
	(bind ?resposta (pregunta-llista-index "Autor preferit: " ?autors))
	(assert (autor-preferit ?resposta))
)


;;; Saltem a preguntes comunes
(defrule a-preguntes-especifiques
	(declare (salience -1))
	=>
	(focus preguntes-especifiques)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PREGUNTES ESPECIFIQUES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule preguntes-especifiques "Preguntes especifiques"
	(import preguntes-comunes ?ALL)
	(export ?ALL)
)

(defrule lletra-gran
	(lector (edat jove | adult))
	=>
	(assert (lletra-gran (pregunta-mp "Prefereixes que la lletra sigui gran?")))
)

(defrule llibres-lleugers
	(lloc exterior | transport)
	=>
	(assert (llibres-lleugers (pregunta-mp "Prefereixes els llibres lleugers?")))
)

(defrule vendes
	(bestseller molt | bastant)
	=>
	(assert (vendes (pregunta-mp "Si estàs entre dos llibres, prefereixes aquell que es ven més?")))
)

(defrule preu-detall
	(preu molt | bastant)
	=>
	(assert (preu-detall (pregunta-opts "Fins quan estàs disposat a gastar-te en un llibre?" 20 15 10 indiferent)))
)

(defrule a-esborrar
	(declare (salience -1))
	=>
	(focus esborrar)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ESBORRAR RECOMANACIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule esborrar "Heuristiques"
	(import preguntes-especifiques ?ALL)
	(export ?ALL)
)

(defrule a-heuristiques
	(declare (salience -1))
	=>
	(focus heuristiques)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASSOCIACIO HEURISTICA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule heuristiques "Heuristiques"
	(import esborrar ?ALL)
	(export ?ALL)
)

(defrule crea-recomanacions
	?llibre <- (object (is-a Llibre))
	=>
	(assert
		(recomanacio
			(isbn (send ?llibre get-isbn))
			(qualitat desconegut)
			(moltadequat 0)
			(adequat 0)
			(inadequat 0)
			(moltinadequat 0)
		)
	)
)

(defrule satisfaccio-genere-experimentador
	?llibre <- (object (is-a Llibre) (isbn ?isbn))
	(experimentar molt)
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a))
	(not (vist satisfaccio-genere-experimentar ?isbn))
	(genere-preferit ?gp)
	=>
	(bind ?genere (getnom ?llibre))
	(assert (vist satisfaccio-genere-experimentar ?isbn))
 (if (eq (str-compare ?gp ?genere) 0)
 then
 	(modify ?recomanacio (adequat (+ ?a 1)))
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FINAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defrule banner-final
;	(declare (salience -1))
;	=>
;	(printout t "============ FINAL =============" crlf crlf)
;)
;
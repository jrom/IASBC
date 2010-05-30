; Principal

(defmodule MAIN (export ?ALL))

;; TEMPLATES

(deftemplate lector
	(slot nom)
	(slot edat)
)

;; Lector per defecte

(deffacts lector-inicial
	(lector
		(nom anonim)
		(edat desconeguda)
	)
)

;; Recomanacio

(deftemplate recomanacio
	(slot id)
	(slot nivell)
	(multislot requeriments-no-satisfets)
	(multislot preferencies-satisfetes)
	(multislot sentit-comu-satisfet)
)

;; Funcions

(deffunction pregunta (?pregunta $?respostes)
	(printout t crlf ?pregunta)
	(bind ?resposta (read))
	(while (not (member ?resposta ?respostes)) do
		(printout t ?pregunta)
		(bind ?resposta (read))
	)
	?resposta
)


;; Regles

(defrule banner "Banner"
	(declare (salience 20))
	; WTF?!
	=>
	(printout t crlf)
	(printout t "======BANNER========" crlf)
	(focus preguntes-perfil-lector)
)

(defmodule preguntes-perfil-lector "Perfil lector"
	(import MAIN ?ALL)
	(export ?ALL)
)

;;; Edat
(defrule determinar-edat "Edat"
	=>
	(assert (nom (pregunta "Nom? " pepito juanito)))
)

(defrule a-preguntes-comunes
	(declare (salience -1))
	=>
	(focus preguntes-comunes)
)

(defmodule preguntes-comunes "Preguntes comunes"
	(import preguntes-perfil-lector ?ALL)
	(export ?ALL)
)


(defrule pregunta-comuna1 "Edat"
	=>
	(assert (nom (pregunta "Opcions? (a,b,c,d) " a b c d)))
)

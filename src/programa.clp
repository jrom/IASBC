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
		(edat adult)
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
	(if (< ?edat 12)
	then
		(modify ?l (edat nen))
	else
		(if (< ?edat 18)
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
	(lloc ~casa)
	=>
	(assert (llibres-lleugers (pregunta-mp "Prefereixes els llibres lleugers?")))
)

(defrule vendes
	(bestseller molt | bastant)
	=>
	(assert (vendes (pregunta-mp "Si estàs entre dos llibres, prefereixes aquell que es ven més?")))
)

(defrule preu
	(lector (ocupacio academic | professional | altres))
	=>
	(assert (preu (pregunta-opts "El preu és un factor prioritari?" si no indiferent)))
)

(defrule preu-detall
	(preu si)
	=>
	(assert (preu-detall (pregunta-num "Fins quan estàs disposat a gastar-te en un llibre?")))
)

(defrule a-incondicionals
	(declare (salience -1))
	=>
	(focus incondicionals)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASSOCIACIONS INCONDICIONALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule incondicionals "incondicionals"
	(import preguntes-especifiques ?ALL)
	(export ?ALL)
)

(defrule lletra-gran-inc
	(lector (edat nen | gran))
	=>
	(assert (lletra-gran molt))
)

(defrule llibres-lleugers-inc
	(lloc casa)
	=>
	(assert (llibres-lleugers indiferent))
)

(defrule vendes-inc
	(bestseller poc | indiferent | gens)
	=>
	(assert (vendes indiferent))
)

(defrule preu-estudiant-desocupat
	(lector (ocupacio estudiant | desocupat))
	=>
	(assert (preu si))
)

(defrule preu-detall-inc
	(preu ~si)
	=>
	(assert (preu-detall indiferent))
)

(defrule a-esborrar
	(declare (salience -1))
	=>
	(focus esborrar)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ESBORRAR RECOMANACIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule esborrar "Esborrar"
	(import incondicionals ?ALL)
	(export ?ALL)
)


;;; TODO ESBORRAR LLIBRES!

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

; Iniciem recomanacions amb comptadors a 0
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

; Si tenim genere preferit =>
;; el genere es +adequat
;; si som GENS experimentadors els altres generes son +moltinadequat
;; si som POC experimentadors els altres generes son +inadequats
(defrule satisfaccio-generes
	?llibre <- (object (is-a Llibre) (isbn ?isbn))
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a) (moltadequat ?ma) (inadequat ?ina) (moltinadequat ?mina))
	(not (vist satisfaccio-generes ?isbn))
	(genere-preferit ~indiferent)
	(genere-preferit ?gp)
	(experimentar ?experimentador)
	=>
	(bind ?genere (getnom ?llibre))
	(assert (vist satisfaccio-generes ?isbn))
	(if (eq (str-compare ?gp ?genere) 0)
	then
		(modify ?recomanacio (moltadequat (+ ?ma 1)))
	else
		(switch ?experimentador
			(case poc then (modify ?recomanacio (inadequat (+ ?ina 1))))
			(case gens then (modify ?recomanacio (moltinadequat (+ ?mina 1))))
		)
	)
)

; Si genere preferit es [fantasia, scifi], [classics, historica] =>
;; +adequats a tots els llibres de l'altra genere similar
(defrule satisfaccio-generes-similars
	?llibre <- (object (is-a Llibre) (isbn ?isbn))
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a) (moltadequat ?ma) (inadequat ?ina) (moltinadequat ?mina))
	(not (vist satisfaccio-generes-similars ?isbn))
	(genere-preferit fantasia | scifi | classics | historica)
	(genere-preferit ?gp)
	(experimentar ?experimentador)
	=>
	(bind ?genere (getnom ?llibre))
	(switch ?gp
		(case fantasia then (bind ?altre scifi))
		(case scifi then (bind ?altre fantasia))
		(case classics then (bind ?altre historica))
		(case historica then (bind ?altre classics))
	)

	(assert (vist satisfaccio-generes-similars ?isbn))
	(if (eq (str-compare ?altre ?genere) 0)
	then
		(modify ?recomanacio (adequat (+ ?a 1)))
	)
)

; Si importa el preu =>
;; llibres de menys de 10 euros +adequat
;; llibres de mes 15-25 euros +inadequat
;; llibres de mes de 25 euros +moltinadequat
;; llibres de butxaca o tapatova +adequat
(defrule satisfaccio-preu
	?llibre <- (object (is-a Llibre) (isbn ?isbn) (preu ?preullibre) (format ?format))
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a) (moltadequat ?ma) (inadequat ?ina) (moltinadequat ?mina))
	(not (vist satisfaccio-preu ?isbn))
	(preu ?preu)
	=>
	(assert (vist satisfaccio-preu ?isbn))
	(if (eq (str-compare ?preu si) 0)
	then
		(if (> ?preullibre 15)
		then
			(modify ?recomanacio (inadequat (+ ?ina 1)))
			(if (> ?preullibre 25) then (modify ?recomanacio (moltinadequat (+ ?mina 1))))
		else
			(if (< ?preullibre 10) then (modify ?recomanacio (adequat (+ ?a 1))))
		)
		(if (or (eq ?format butxaca) (eq ?format tapatova)) then (modify ?recomanacio (adequat (+ ?a 1))) )
	)
)


; Si el lector és un nen o una persona gran =>
;; No se li recomanen llibres de butxaca
(defrule satisfaccio-lletragran
	?llibre <- (object (is-a Llibre) (isbn ?isbn) (format ?format))
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a) (moltadequat ?ma) (inadequat ?ina) (moltinadequat ?mina))
	(not (vist satisfaccio-lletragran ?isbn))
	?l <- (lector (edat nen | gran))
	=>
	(assert (vist satisfaccio-lletragran ?isbn))
	(if (eq (str-compare ?format butxaca) 0)
	then
		(modify ?recomanacio (moltinadequat (+ ?mina 1)))
	)
	(if (eq (str-compare ?format tapadura) 0)
	then
		(modify ?recomanacio (adequat (+ ?a 1)))
	)
)


; Si agraden els bestsellers (molt bastant poc gens) =>
;; Es recomanen els bestsellers (molt bastant poc gens)
(defrule satisfaccio-bestsellers
	?llibre <- (object (is-a Llibre) (isbn ?isbn) (bestseller ?bs))
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a) (moltadequat ?ma) (inadequat ?ina) (moltinadequat ?mina))
	(not (vist satisfaccio-bestsellers ?isbn))
	(bestseller ?bestseller)
	=>
	(assert (vist satisfaccio-bestsellers ?isbn))
	(if ?bs	
	then
		(switch ?bestseller
			(case molt then (modify ?recomanacio (moltadequat (+ ?ma 1))))
			(case bastant then (modify ?recomanacio (adequat (+ ?a 1))))
			(case poc then (modify ?recomanacio (inadequat (+ ?ina 1))))
			(case gens then (modify ?recomanacio (moltinadequat (+ ?mina 1))))
		)
	)
)

; Si es un jove|adult|gran =>
;; Es recomanen llibres juvenils molt|poc|gens
(defrule satisfaccio-jovejuvenil
	?llibre <- (object (is-a Llibre) (isbn ?isbn) (orientacio ?orientacio))
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a) (moltadequat ?ma) (inadequat ?ina) (moltinadequat ?mina))
	(not (vist satisfaccio-jovejuvenil ?isbn))
	?l <- (lector (edat ?edat))
	=>
	(assert (vist satisfaccio-jovejuvenil ?isbn))
	(if (eq (str-compare ?orientacio juvenil) 0)
	then
		(switch ?edat
			(case jove then (modify ?recomanacio (moltadequat (+ ?ma 1))))
			(case adult then (modify ?recomanacio (inadequat (+ ?ina 1))))
			(case gran then (modify ?recomanacio (moltinadequat (+ ?mina 1))))
		)
	)
)

; Si es un academic =>
;; Recomanem llibres classics + historica + trama complexa
(defrule satisfaccio-academics
	?llibre <- (object (is-a Llibre) (isbn ?isbn) (tramasimple ?tramasimple))
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a) (moltadequat ?ma) (inadequat ?ina) (moltinadequat ?mina))
	(not (vist satisfaccio-academics ?isbn))
	?l <- (lector (ocupacio academic))
	=>
	(assert (vist satisfaccio-academics ?isbn))
	(bind ?genere (getnom ?llibre))
	(switch ?genere
		(case classics then (modify ?recomanacio (adequat (+ ?a 1))))
		(case historica then (modify ?recomanacio (adequat (+ ?a 1))))
	)
	(if (not ?tramasimple)
	then
		(modify ?recomanacio (adequat (+ ?a 1)))
	)
)


; Si es un home =>
;; Recomanem llibres historica i des-recomanem romantica
(defrule satisfaccio-home
	?llibre <- (object (is-a Llibre) (isbn ?isbn))
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a) (moltadequat ?ma) (inadequat ?ina) (moltinadequat ?mina))
	(not (vist satisfaccio-home ?isbn))
	?l <- (lector (sexe home))
	=>
	(assert (vist satisfaccio-home ?isbn))
	(bind ?genere (getnom ?llibre))
	(switch ?genere
		(case historica then (modify ?recomanacio (adequat (+ ?a 1))))
		(case romantica then (modify ?recomanacio (inadequat (+ ?ina 1))))
	)
)

; Si es una dona =>
;; Recomanem llibres thriller i novel·la negra
(defrule satisfaccio-dona
	?llibre <- (object (is-a Llibre) (isbn ?isbn))
	?recomanacio <- (recomanacio (isbn ?isbn) (adequat ?a) (moltadequat ?ma) (inadequat ?ina) (moltinadequat ?mina))
	(not (vist satisfaccio-dona ?isbn))
	?l <- (lector (sexe dona))
	=>
	(assert (vist satisfaccio-dona ?isbn))
	(bind ?genere (getnom ?llibre))
	(switch ?genere
		(case thriller then (modify ?recomanacio (adequat (+ ?a 1))))
		(case negra then (modify ?recomanacio (adequat (+ ?a 1))))
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
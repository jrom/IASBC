#TODO

- Usarem l'Script scraper Llibreria AMAZON!
- <del>Introducció</del> [Bernat]
- Formalització de l'ontologia a Protégé [Bernat] **En procés**
- Anàlisi del Problema [Bernat]

#Ontologia

##Frame Llibre
- Títol
- Preu
- Any primera edició
- Número d'edicions
- Format [gran, petit, mitjà]
- Idioma
- Número de pàgines
- Best-seller?
- Enquadernació
- ISBN
- Orientat a edat [infantil, juvenil, adults]
- Pes
- Número de ventes anuals

##Frame Gènere

- Tipus [Policíaca, Terror, Fantasia, Sci-fi, Romàntica]

##Frame Autor
- Nom
- Estil [simple, complicat, profund]
- Generació
- Nacionalitat

##Frame Lector
- Edat
- Temps disponible [freqüència + duració]
- Moment de lectura
- Lloc de lectura
- Interès en best-sellers
- Idioma preferent [català, castellà, estranger]
- Gèneres preferits

##Associacions
- **pertany_a**
  - D: Llibre
  - R: Gènere
  - C: 1
  - I: te_llibres(N)
  - Tr: No
  - Con: No
  - De: -
  - H: -
  
- **escrit_per**
  - D: Llibre
  - R: Autor
  - C: N _(més d'un autor?)_
  - I: ha_escrit(N)
  - Tr: No
  - Con: No
  - De: -
  - H: -
  

#Info recollida del professor

- No es pot bombardejar l'usuari amb preguntes



# Dictionnaire des variables Enquête Conditions de travail 2019



| TCM_FA |                                                                                      | |--|--| | VAGUE  | Vague                                                                                | | Car 1  | 1. Première interrogation<br>2. Deuxième interrogation<br>3. Troisième interrogation |

CT

| TCM_FA |                        | |--|--| | LE     | Eclatement de logement | | Car 1  |                        | | CT     |                        |

| TCM_FA |                      | |--|--| | EC     | Eclatement de ménage | | Car 1  |                      |

<!-- p. 15 -->

| TCM_FA   |                                 | |--|--| | IDENT_FA | Identifiant de la fiche adresse | | Car 10   |                                 | | CT       |                                 |

| TCM_FA    |                                                  | |--|--| | IDENT_LOG | Identifiant du logement (IDENT_LOG destinataire) | | Car 12    |                                                  | | CT        |                                                  |

TCM\_FA BS Numéro de ménage dans le logement Car. - 1

TCM\_FA IDENT\_MEN Identifiant du ménage (IDENT\_MEN destinataire) Car. - 13

TCM\_FA IDENT\_IND Identifiant de l'individu dans le logement (IDENT\_IND destinataire) Car. - 15

TCM\_FA IDENT\_IND\_2013 Identifiant de l'individu dans le logement (IDENT\_IND destinataire) lors de la collecte de CT 2013 Car. - 15 CT 2013

TCM\_FA IDENT\_IND\_2016 Identifiant de l'individu dans le logement (IDENT\_IND destinataire) lors de la collecte de CT-RPS 2016 Car. - 15 CT-RPS 2016

TCM\_FA TYPREPQAA Indicateur de réponse au QAA Car. - 1 1. Oui 2. Non

CALCULEE : vaut 1 si la dernière question du QAA (RP36) est renseignée, 2 sinon CT

CT

CT

CT 2019

| TCM_FA       |                                               | |--|--| | CHAMP_CT2005 | Champ de l'enquête conditions de travail 2005 | | Car 1        | 1. Oui                                        | |              | 2. Non                                        |

CALCULEE : vaut 1 si la personne est dans le champ de l'enquête CT2005 (en métropole, active occupée au sens de l'enquête Emploi) CT

<!-- p. 16 -->

## A - Etat-civil

| TCM_A |      | |--|--| | SEXE  | Sexe | | A2    |      | | Car 1 |      | | CT    |      |

TCM\_A MNAIS A3 Mois de naissance Num - 8 1 à 12

TCM\_A ANAIS A3 Année de naissance Num - 8 1900 à 2002 CT

TCM\_A Panel CFMATRI Lors de notre précédente enquête, PRENOM était V1ETAMATRI. Est-ce toujours le cas ? Car. - 1 1. Oui 2. Non la situation a changé 3. Non, la situation était erronée

CT-RPS

CT

| TCM_A   | Panel                                  | |--|--| | AMATRI  | Année de changement d'état matrimonial | | Num - 8 | 2012 à 2016                            | | CT2019  |                                        |

TCM\_A Panel MMATRI Mois de changement d'état matrimonial Car. - 2 1 à 12 CT2019

| TCM_A   |                            | |--|--| | AGE     | Age au moment de l'enquête | | A3      |                            | | Num - 8 | 14 à 99                    |

CALCULEE CT

<!-- p. 17 -->

| TCM_A      |                                                                                                                                                                                                                                                                     |  | |--|--|--| | AGEQ<br>A3 | Age quinquennal au moment de l'enquête                                                                                                                                                                                                                              |  | | Car 2      | 15. 15 à 19 ans<br>20. 20 à 24 ans<br>25. 25 à 29 ans<br>30. 30 à 34 ans<br>35. 35 à 39 ans<br>40. 40 à 44 ans<br>45. 45 à 49 ans<br>50. 50 à 54 ans<br>55. 55 à 59 ans<br>60.60 à 64 ans<br>65. 65 à 69 ans<br>70.70 à 74 ans<br>75. 75 à 79 ans<br>80.80 à 84 ans |  |

CALCULEE

CT

| TCM_A |                                                        | |--|--| | LNAIS | Indicateur de lieu de naissance                        | | A4    |                                                        | | Car 1 | 1. en France (métropole ou DOM-TOM)<br>2. A l'étranger |

CT

| TCM_A   |                                     | |--|--| | DEPNAIS | Département/territoire de naissance | | A5      |                                     | | Car 2   | 01 à 95 + 9A à 9D                   |

CT

| TCM_A       |                                                                                                                                                                                                      |  | |--|--|--| | NAIS7<br>A6 | Code regroupé du lieu de naissance                                                                                                                                                                   |  | | Car 1       | 1. France métropolitaine<br>2. DOM-TOM<br>3. Union européenne des 15 (sauf France)<br>4. Pays entrés depuis 2004 dans l'Union européenne<br>5. Maghreb<br>6. Afrique (sauf Maghreb)<br>7. Autre pays |  |

<!-- p. 18 -->

| TCM_A        |                                                                                                                                                                                                                                                                                                 | |--|--| | LNAISD<br>A6 | Code du lieu de naissance                                                                                                                                                                                                                                                                       | | Car 2        | 11. France<br>21. Algérie<br>22. Maroc<br>23. Tunisie<br>24. Autres pays d'Afrique<br>31. Vietnam, Laos, Cambodge<br>34. Turquie<br>35. Autres Asie<br>41. Portugal<br>42. Espagne<br>43. Italie<br>44. Autres pays de l'UE 15<br>45. Autres pays de l'UE 27<br>46. Autres Europe<br>51. Autres |

CALCULEE : Déclaration en clair (PAYSNAIS) => Codification CT

| TCM_A   |                                | |--|--| | ANARRIV | Année d'installation en France | | A8      |                                | | Num - 8 | 1900 à 2019                    |

CALCULEE : On « remplit les trous » : quand l'un est renseigné, on calcule l'autre. Si ANARRIV = blanc et AGARRIV compris entre 0 et 125 alors ANARRIV = [Année de l'enquête] - AGARRIV Si AGARRIV = blanc et ANARRIV compris entre ([Année de l'enquête] - 125) et [Année de l'enquête] alors AGARRIV = [Année de l'enquête] - ANARRIV CT

| TCM_A   |                         | |--|--| | AGARRIV | Âge d'arrivée en France | | A8      |                         | | Num - 8 |                         |

CALCULEE : On « remplit les trous » : quand l'un est renseigné, on calcule l'autre. Si ANARRIV = blanc et AGARRIV compris entre 0 et 125 alors ANARRIV = [Année de l'enquête] - AGARRIV Si AGARRIV = blanc et ANARRIV compris entre ([Année de l'enquête] - 125) et [Année de l'enquête] alors AGARRIV = [Année de l'enquête] - ANARRIV CT

<!-- p. 19 -->

### B - Situation familiale

| TCM_B  |                                                           | |--|--| | COUPLE | Vie en couple                                             | | B1     |                                                           | | Car 1  | 1. Oui, avec une personne qui vit dans le logement        | |        | 2. Oui, avec une personne qui ne vit pas dans le logement | |        | 3. non                                                    |

CT

| TCM_B   |                                                                        | |--|--| | COUPLRP | Vie en couple au sens du RP                                            | | Car 1   | 1. est en couple au sens du RP<br>2. N'est pas en couple au sens du RP |

#### CALCULEE

Pour être en couple au sens du RP, il faut remplir les conditions suivantes :

- se déclarer être en couple avec un individu habitant dans le logement
- être de sexe différent
- avoir plus de 18 ans pour l'homme, plus de 15 ans pour la femme

CT

| TCM_B    |                                             | |--|--| | CONJOINT | Numéro d'ordre du conjoint dans le logement | | B2       |                                             | | Car 2    | 1 à 20                                      |

CT

| TCM_B    |                                                           | |--|--| | ETAMATRI | Etat matrimonial légal                                    | | B3       |                                                           | | Car 1    | 1. Célibataire                                            | |          | 2. Marié(e) ou remarié(e), y compris séparé(e) légalement | |          | 3. Veuf(ve)                                               | |          | 4. Divorcé(e)                                             |

CT

| TCM_B |                     | |--|--| | PACS  | Existence d'un PACS | | B4    |                     | | Car 1 | 1. Oui              | |       | 2. Non              |

CT

| TCM_FAM |                                      | |--|--| | MER1E   | Présence de la mère dans le logement | | B5      |                                      | | Car 1   | 1. vit ici                           | |         | 2. vit ailleurs                      | |         | 3. est décédée                       | |         | 4. mère inconnue                     | |         | 9. ne sait pas                       |

<!-- p. 20 -->

| TCM_FAM |                                   | |--|--| | PER1E   | Présence du père dans le logement | | B6      |                                   | | Car 1   | 1. vit ici                        | |         | 2. vit ailleurs                   | |         | 3. est décédé                     | |         | 4. mère inconnu                   | |         | 9. ne sait pas                    |

## D - Lieux de vie

| TCM_D   |                                             | |--|--| | TYPOLOG | Type d'occupation du logement (Vit-il ici…) | | D1      |                                             | | Car 1   | (0.non (membre du ménage vivant ailleurs)   | |         | 1. toute l'année ou presque                 | |         | 2. plutôt les week-end ou les vacances      |

CT

| TCM_EGO  |                                                             | |--|--| | IPROPLOC | Indicateur d'occupant principal                             | | C7       |                                                             | | Car 1    | 1. Propriétaire ou locataire en titre du logement<br>2. Non |

CALCULEE

CT

# E - Situation principale vis-à-vis du travail et groupe de référence

| TCM_E       | Si 15 ans ou plus                                                                                                                                                                                                                                                                                                                                                 | |--|--| | SITUA<br>E1 | Situation principale vis-à-vis du travail                                                                                                                                                                                                                                                                                                                         | | Car 1       | 1. Occupe un emploi<br>2. Apprenti(e) sous contrat ou en stage rémunéré<br>3. Etudiant(e), élève, en formation ou en stage non rémunéré<br>4. Chômeur (inscrit(e) ou non au Pôle Emploi)<br>5. Retraité(e) ou retiré(e) des affaires ou en préretraite<br>6. Femme ou homme au foyer<br>7. Inactif(ve) pour cause d'invalidité<br>8. Autre situation d'inactivité |

CT

| TCM_E  | Si 15 ans ou plus                    | |--|--| | TRAREF | Travail rémunéré la semaine dernière | | E1a    |                                      | | Car 1  | 1. Oui<br>2. Non                     |

CT

| TCM_E         | Situation principale n'est pas l'emploi et n'a pas travaillé la semaine dernière (SITUA#1<br>et TRAREF=2) | |--|--| | PASTRA<br>E1b | Avoir cependant un emploi                                                                                 | | Car 1         | 1. Oui<br>2. Non                                                                                          |

<!-- p. 21 -->

| TCM_E       | A un emploi mais n'a pas travaillé (SITUA=1 ou PASTRA=1 et TAREF=2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | |--|--| | RABS<br>E1c | Raison de non travail                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | | Car 2       | 1. Congé rémunéré, y compris jours de RTT ou repos compensateur<br>2. Congé maladie, accident du travail, congés pour enfants malades<br>3. Congé de maternité, congé de paternité<br>4. Temps partiel<br>5. Congé parental<br>6. Autres types de congés non rémunérés<br>7. Formation rémunérée par l'employeur ou dans le cadre d'un contrat en alternance ou<br>en apprentissage<br>8. Chômage partiel (chômage technique)<br>9. Mis à pied ou période de fin d'emploi<br>10. Grève<br>11. Période de morte saison dans le cadre d'une activité de saisonnier ou période<br>précédant le début d'un emploi<br>12. Intempéries |

| TCM_E   | Si arrêt de longue durée (RABSP=2 ou 5 à 11) | |--|--| | RABSP   | Temps de l'interruption                      | | E1d     |                                              | | Num - 8 | 0 à 99999                                    |

CT

| TCM_E  |                                  | |--|--| | RABSPU | Unité de temps de l'interruption | | E1e    |                                  | | Car 1  | 1. Années                        | |        | 2. Mois                          | |        | 3. Semaines                      | |        | 4. Jours                         |

CT

| TCM_E         |                                  | |--|--| | DUREE_CESSATI | Durée en jours de l'interruption | | ON            |                                  | | Num - 8       | 0 à 99999                        |

CALCULEE à partir de RABSP et RABSPU

CT

| TCM_E         | Si pas d'emploi (SITUA=2 à 8 et PASTRA=2) | |--|--| | AIDFAM<br>E1f | Aide familial ou conjoint collaborateur   | | Car 1         | 1. Oui<br>2. Non                          |

CT

| TCM_E  | Si pas d'emploi (SITUA=2 à 8 et PASTRA=2) | |--|--| | INFORM | Activités informelles                     | | E1g    |                                           | | Car 1  | 1. Oui                                    | |        | 2. Non                                    |

<!-- p. 22 -->

| TCM_E                 | Individus désignés par une FA d'un SSECH 22 à 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | |--|--| | STATUTEXT<br>S7 et S8 | Statut (extension FP)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | | Car 1                 | 1. Salarié de l'État (ministères, établissements publics administratifs (EPA) nationaux,<br>établissements publics d'enseignement,)<br>2. Salarié d'une collectivité territoriale (régions, départements, communes, OPHLM,<br>établissements publics administratifs (EPA) des collectivités territoriales,)<br>3. Salarié d'un hôpital public<br>4. Salarié d'un établissement de santé privé (à but lucratif ou non lucratif)<br>5. Salarié du secteur public social et médico-social (établissement d'hébergement pour<br>personnes âgées, établissements sociaux ou médico-sociaux)<br>6. Salarié d'une entreprise, d'un artisan, d'une association (hors établissement de santé<br>privé, hors secteur public social et médico-social)<br>7. Salarié d'un ou plusieurs particuliers<br>8. Vous aidez un membre de votre famille dans son travail sans être rémunéré<br>9. Chef d'entreprise salarié, PDG, gérant minoritaire, associé<br>10. Indépendant ou à votre compte |

| Actif occupé au sens du TCM | |--| | 0. Non<br>1. Oui            | |                             |

CALCULEE : activité au sens habituel du TCM (avec SITUA). Vaut par définition toujours 1 pour les individus répondants.

<!-- p. 23 -->

### Variables calculées sur le ménage

--|--| | ENFANT | Enfant du ménage | | Car 1  | 1. Oui<br>2. Non |

CALCULEE

CT

--|--| | CJSITUA | Situation principale vis-à-vis du travail du conjoint                                                                                                                                                                                                                                                                                                             | | Car 1   | 1. Occupe un emploi<br>2. Apprenti(e) sous contrat ou en stage rémunéré<br>3. Etudiant(e), élève, en formation ou en stage non rémunéré<br>4. Chômeur (inscrit(e) ou non au Pôle Emploi)<br>5. Retraité(e) ou retiré(e) des affaires ou en préretraite<br>6. Femme ou homme au foyer<br>7. Inactif(ve) pour cause d'invalidité<br>8. Autre situation d'inactivité |

CALCULEE

CT

--|--| | CJACTOCCUP | Conjoint actif occupé au sens de l'enquête | | Num - 8    | 0. Non<br>1. Oui                           |

CALCULEE : SITUA=1 ou 2 ou TRAREF=1 ou PASTRA=1 ou AIDFAM=1 ou INFORM=1 et RASP est vide ou inférieur à 1 an / 12 mois / 52 semaines / 365 jours. CT

--|--| | CJENQUETE | Conjoint est un kish | | Car 1     | 0. Non<br>1. Oui     |

CALCULEE

CT

--|--| | NBENFM3 | Nombre d'enfants (ou beaux enfants) de moins de 3 ans de<br>l'enquêté dans le ménage (âge au moment de l'enquête) | | Num - 8 | 0 à 20                                                                                                            |

CALCULEE : compteur sur l'âge (AGE<3) et LIENQI=02,31

CT

--|--| | NBENF3A17 | Nombre d'enfants (ou beaux enfants) de 3 ans à 17 ans de<br>l'enquêté dans le ménage (âge au moment de l'enquête) | | Num - 8   | 0 à 20                                                                                                            |

CALCULEE : compteur sur l'âge (AGE>=3 et AGE<18) et LIENQI=02,31

CT

--|--| | NBENF18P | Nombre d'enfants (ou beaux enfants) de 18 ans et plus de<br>l'enquêté dans le ménage (âge au moment de l'enquête) | | Num - 8  | 0 à 20                                                                                                            |

CALCULEE : compteur sur l'âge (AGE>=18) et LIENQI=02,31

<!-- p. 24 -->

--|--| | NPERS   | Nombre de personnes du ménage | | Num - 8 | 0 à 20                        |

CALCULEE

CT

--|--| | NACTIFS | Nombre d'actifs dans le ménage | | Num - 8 | 0 à 20                         |

CALCULEE

CT

--|--| | LIEN_01 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 1                                                                                                                                                                                       | | Car 2   | 00. Lui-même<br>01. Conjoint<br>02. Enfant<br>03. Parent<br>10. Frère, sœur<br>21. Petit-enfant<br>22. Grand-parent<br>31. Beau-fils, belle-fille<br>32. Beau-parent<br>40. Autre lien familial<br>50. Lien familial indéterminé<br>60. Ami<br>90. Autre lien non familial |

CALCULEE

CT

--|--| | LIEN_02 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 2 | | Car 2   | mêmes modalités que LIEN_01                                                          | | CT      |                                                                                      |

--|--| | LIEN_03 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 3 | | Car 2   | mêmes modalités que LIEN_01                                                          | | CT      |                                                                                      |

--|--| | LIEN_04 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 4 | | Car 2   | mêmes modalités que LIEN_01                                                          | | CT      |                                                                                      |

--|--| | LIEN_05 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 5 | | Car 2   | mêmes modalités que LIEN_01                                                          |

<!-- p. 25 -->

--|--| | LIEN_06 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 6 | | Car 2   | mêmes modalités que LIEN_01                                                          | | CT      |                                                                                      |

--|--| | LIEN_07 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 7 | | Car 2   | mêmes modalités que LIEN_01                                                          | | CT      |                                                                                      |

--|--| | LIEN_08 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 8 | | Car 2   | mêmes modalités que LIEN_01                                                          | | CT      |                                                                                      |

--|--| | LIEN_09 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 9 | | Car 2   | mêmes modalités que LIEN_01                                                          | | CT      |                                                                                      |

--|--| | LIEN_10 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 10 | | Car 2   | mêmes modalités que LIEN_01                                                           | | CT      |                                                                                       |

--|--| | LIEN_11 | Lien de l'enquêté avec la personne du ménage ayant le<br>numéro d'ordre individuel 11 | | Car 2   | mêmes modalités que LIEN_01                                                           | | CT      |                                                                                       |

--|--| | TYPMEN5 | Type de ménage au sens du TCM                                                                                                                            | | Car 1   | 1. Personne seule<br>2. Famille monoparentale<br>3. Couple sans enfant<br>4. Couple avec au moins un enfant<br>5. Autre type de ménage (ménage complexe) |

CALCULEE CT

<!-- p. 26 -->

| Type de ménage détaillé au sens du TCM                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |                                 | |--|--| | 10. Personne seule active<br>11. Personne seule inactive<br>21. Famille monoparentale, parent actif<br>22. Famille monoparentale, parent inactif et au moins un enfant actif<br>23. Famille monoparentale, tous inactifs<br>31. Couple sans enfant, un actif<br>32. Couple sans enfant, deux actifs<br>33. Couple sans enfant, tous inactifs<br>41. Couple avec enfant, un membre du couple actif<br>42. Couple avec enfant, deux membres du couple actif<br>43. Couple avec enfant, couple inactif et au moins un enfant actif<br>44. Couple avec enfant, tous inactif<br>51. Autre ménage, un actif<br>52. Autre ménage, deux actifs ou plus |                                 | |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | 53. Autre ménage, tous inactifs |

CALCULEE

CT

--|--| | NOIK1   | NOI de la première personne du ménage sélectionnée pour<br>répondre au QAO (s'il y a lieu) | | Num - 8 | 1 à 20                                                                                     |

CALCULEE

CT

--|--| | NOIK2   | NOI de la seconde personne du ménage sélectionnée pour<br>répondre au QAO (s'il y a lieu) | | Num - 8 | 1 à 20                                                                                    |

CALCULEE

<!-- p. 27 -->

## 2. Variables issues du questionnaire "Individu"

## 1 - Activité professionnelle (Actifs occupés seulement)

## A - Profession

| 1A          | Panel                                                    | |--|--| | ACTOCCUP_16 | Actif occupé au sens de l'enquête en « V1MENQ » « V1AENQ | |             | »                                                        | | Num - 1     | 0. Non                                                   | |             | 1. Oui                                                   |

CALCULEE CT-RPS 2016

| 1A               | Panel                                                                                                                          | |--|--| | RPSPROFESS<br>Q1 | En "mois enquête" "année enquête" , vous étiez donc<br>RPSPROFESSC dans l'établissement RPSEMPLXC. Est-ce<br>toujours le cas ? | | Car 1            | 1. Oui<br>2. Non                                                                                                               |

Répondre "Oui" dans le cas de la même profession dans le même établissement Si l'enquêté exerce la même profession mais dans un établissement différent, répondre "non" CT2019

| 1A             | Panel                                                                                    | |--|--| | INTERRUP<br>Q2 | Avez-vous occupé cet emploi sans interruption depuis "mois<br>enquête" "année enquête" ? | | Car 1          | 1. Oui<br>2. Non                                                                         |

Avoir gardé le même emploi signifie avoir la même profession dans le même établissement Par interruption, on peut entendre de longs arrêts maladie, du chômage (plus de 6 mois) CT2019

| 1A      | Panel                                   | |--|--| | ARREMPA | A quelle date s'est arrêté cet emploi ? | | Q3      |                                         | | Num 8   | 1. Année                                | |         | 8. NSP                                  | |         | 9. REFUS                                |

De 2015 à l'année de l'enquête CT2019

| 1A           | Panel                         | |--|--| | ARREMPA_DRAP | Variable "drapeau" de ARREMPA | | Q3           |                               | | Num 8        | 1. Réponse<br>0. Sans objet   | |              | -1 . Ne sait pas              | |              | -2 . Refuse de répondre       |

Variable drapeau CT2019

<!-- p. 28 -->

| 1A      | Panel                                   | |--|--| | ARREMPM | A quelle date s'est arrêté cet emploi ? | | Q3A     |                                         | | Car 2   | 1. Mois                                 | |         | 8. NSP                                  | |         | 9. REFUS                                |

CT2019

| 1A           | Panel                         | |--|--| | ARREMPM_DRAP | Variable "drapeau" de ARREMPM | | Q3A          |                               | | Num 8        | 1. Réponse                    | |              | 0. Sans objet                 | |              | -1 . Ne sait pas              | |              | -2 . Refuse de répondre       |

Variable drapeau CT2019

| 1A                  |                                                                                                                                                                                                                | |--|--| | PE<br>PRO1 et PRO12 | Pour ceux qui aident un membre de leur famille : Quelle est la<br>profession principale de la personne que vous aidez ?<br>Pour les autres : Quelle est votre profession principale ?<br>Profession (PCS 2003) | | Car 4               |                                                                                                                                                                                                                |

CALCULEE : Déclaration en clair (PROFESS et PROFESSA) => traitement SICORE (embarqué puis en bureau) et reprise manuelle

Pour les individus panel ayant occupé sans interruption le même emploi depuis la dernière enquête (si RPSPROFESS = 1 et INTERRUP= 1), profession déclarée en 2015. Sinon et pour tous les entrants, profession déclarée en 2018. CT

| 1A    |                                                                                                                                                                                                                                                      | |--|--| | CSE   | Pour ceux qui aident un membre de leur famille : Quelle est la<br>profession principale de la personne que vous aidez ?<br>Pour les autres : Quelle est votre profession principale ?<br>Catégorie socio-professionnelle niveau détaillée (PCS 2003) | | Car 2 |                                                                                                                                                                                                                                                      |

CALCULEE : Déclaration en clair (PROFESS et PROFESSA) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 1A    |                                                                                                                                                                                                                                                             | |--|--| | CSEI  | Pour ceux qui aident un membre de leur famille : Quelle est la<br>profession principale de la personne que vous aidez ?<br>Pour les autres : Quelle est votre profession principale ?<br>Catégorie socio-professionnelle niveau intermédiaire (PCS<br>2003) | | Car 2 |                                                                                                                                                                                                                                                             |

CALCULEE : Déclaration en clair (PROFESS et PROFESSA) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

<!-- p. 29 -->

| 1A    |                                                                                                                                                                                                                                                   | |--|--| | CSER  | Pour ceux qui aident un membre de leur famille : Quelle est la<br>profession principale de la personne que vous aidez ?<br>Pour les autres : Quelle est votre profession principale ?<br>Catégorie socio-professionnelle niveau agrégé (PCS 2003) | | Car 4 |                                                                                                                                                                                                                                                   |

CALCULEE : Déclaration en clair (PROFESS et PROFESSA) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 1A    |                                                                                                                                                                                                                                                | |--|--| | PEUN  | Pour ceux qui aident un membre de leur famille : Quelle est la<br>profession principale de la personne que vous aidez ?<br>Pour les autres : Quelle est votre profession principale ?<br>Profession, nomenclature européenne CITP08 (COM)-ISCO | | Car 8 |                                                                                                                                                                                                                                                |

CALCULEE : Déclaration en clair (PROFESS et PROFESSA) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 1A     |                                                                                                                                                                                                                                                                 | |--|--| | PEUN10 | Pour ceux qui aident un membre de leur famille : Quelle est la<br>profession principale de la personne que vous aidez ?<br>Pour les autres : Quelle est votre profession principale ?<br>Profession niveau agrégé, nomenclature européenne CITP08<br>(COM)-ISCO | | Car 8  |                                                                                                                                                                                                                                                                 |

CALCULEE : Déclaration en clair (PROFESS et PROFESSA) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 1A     |                                                                                                                                                                                                                                      | |--|--| | FAP225 | Pour ceux qui aident un membre de leur famille : Quelle est la<br>profession principale de la personne que vous aidez ?<br>Pour les autres : Quelle est votre profession principale ?<br>Famille professionnelle détaillée (FAP2009) | | Car 5  |                                                                                                                                                                                                                                      |

CALCULEE : Déclaration en clair (PROFESS et PROFESSA) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 1A    |                                                                                                                                                                                                                                  | |--|--| | FAP87 | Pour ceux qui aident un membre de leur famille : Quelle est la<br>profession principale de la personne que vous aidez ?<br>Pour les autres : Quelle est votre profession principale ?<br>Famille professionnelle agrégée (FAP87) | | Car 3 |                                                                                                                                                                                                                                  |

CALCULEE : Déclaration en clair (PROFESS et PROFESSA) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

<!-- p. 30 -->

| 1A             |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | |--|--| | STATUT<br>PRO2 | Dans votre emploi principal actuel, êtes-vous ?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | | Car 2          | 1. Salarié de l'État (ministères, établissements publics administratifs (EPA) nationaux,<br>établissements publics d'enseignement,)<br>2. Salarié d'une collectivité territoriale (régions, départements, communes, OPHLM,<br>établissements publics administratifs (EPA) des collectivités territoriales,)<br>3. Salarié d'un hôpital public<br>4. Salarié d'un établissement de santé privé (à but lucratif ou non lucratif)<br>5. Salarié du secteur public social et médico-social (établissement d'hébergement pour<br>personnes âgées, établissements sociaux ou médico-sociaux)<br>6. Salarié d'une entreprise, d'un artisan, d'une association (hors établissement de santé<br>privé, hors secteur public social et médico-social)<br>7. Salarié d'un ou plusieurs particuliers<br>8. Vous aidez un membre de votre famille dans son travail sans être rémunéré<br>9. Chef d'entreprise salarié, PDG, gérant minoritaire, associé<br>10. Indépendant ou à votre compte |

CALCULEE : Pour les SSECH 2 à 6, réponse à STATUTEXTx p41 sur le questionnaire

Présenter la carte 2. Si la personne effectue un travail informel non déclaré :

- Coder "7 Salarié de particuliers" si la personne travaille régulièrement pour le ou les mêmes particuliers (des heures de ménage chaque semaine par exemple) - Coder "10 - Indépendant ou à votre compte" s'il s'agit de prestations de services occasionnelles à des clients divers (réparation, bricolage).
- Dans les autres cas, coder "6 Salarié d'une entreprise, d'une association ou d'un artisan". CT

| 1A       |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | |--|--| | TYPEXTFP | A quel SSECH d'extension l'enquêté aurait-il pu être éligible ?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | | Car 3    | 01. Salarié non éligible aux extensions ou non salarié<br>02. Salarié de l'État (ministères, établissements publics administratifs (EPA) nationaux,<br>établissements publics d'enseignement,)<br>03. Salarié d'une collectivité territoriale (régions, départements, communes, OPHLM,<br>établissements publics administratifs (EPA) des collectivités territoriales,)<br>04. Salarié d'un hôpital public<br>05. Salarié d'un établissement de santé privé (à but lucratif ou non lucratif)<br>06. Salarié du secteur public social et médico-social (établissement d'hébergement pour<br>personnes âgées, établissements sociaux ou médico-sociaux) |

CALCULEE : recodification de STATUT avec les valeurs des SSECH Si STATUT= « 1 », TYPEXTFP= « 02 »

Si STATUT= « 2 », TYPEXTFP= « 03 »

Si STATUT= « 3 », TYPEXTFP= « 04 » Si STATUT= « 4 », TYPEXTFP= « 05 »

Si STATUT= « 5 », TYPEXTFP= « 06 »

Sinon TYPEXTFP= « 01 »

CT

| 1A      |                                         | |--|--| | ELIGEXT | Indicateur d'éligibilité aux extensions | | Car 4   | 0. Non                                  | |         | 1. Oui                                  |

CALCULEE : vaut 1 si TYPEXTFP compris entre "02" et "06", 0 sinon CT

| 1A                | Si l'individu est salarié d'une entreprise privé ou publique, d'un artisan, d'une association,<br>etc. (STATUT(PRO2) )= 6) | |--|--| | TYPEMPLOY<br>PRO3 | Votre employeur est-il                                                                                                     | | Car 1             | 1. Une entreprise publique ou privée<br>2. Une structure à but non lucratif (association, mutuelle, coopérative…)          |



<!-- p. 31 -->

| 1A               | Si l'individu est personnel d'enseignement et de direction des établissements scolaires                                                                                                             | |--|--| | TYPETSCO<br>PRO4 | Dans quel type d'établissement travaillez-vous ?                                                                                                                                                    | | Car 1            | 1. Ecole maternelle oou primaire<br>2. Collège<br>3. Lycée professionnel<br>4. Lycée général, classe préparatoire<br>5. Université, école d'ingénieur, autre établissement d'enseignement supérieur |

Choisir le plus haut niveau d'enseignement exercé : par exemple, si un professeur enseigne à des collégiens et des lycéens CT-RPS 2016

| 1A            | Si l'individu est salarié d'un établissement de santé privé à but lucratif (clinique) ou non<br>lucratif ( STATUT (PRO2) = 4) | |--|--| | ESPIC<br>PRO5 | Votre établissement est-il un établissement de santé privé<br>d'intérêt collectif à but non lucratif ?                        | | Car 1         | 1. Oui<br>2. Non<br>8. NSP<br>9. REFUS                                                                                        |

Principalement établissements de santé privé d'intérêt collectif (ESPIC) gérés par exemple par des associations (type Croix-Rouge), des mutuelles (type MGEN) ou des congrécations religieuses. CT-RPS 2016

| 1A               | Si l'individu est salarié (STATUT(PRO2) = 1 à 7)                                                                                                        | |--|--| | PLURIEMP<br>PRO6 | Lors de la dernière semaine travaillée, avez-vous eu :                                                                                                  | | Car 1            | 1. Un seul employeur<br>2. Plusieurs employeurs, mais il en existe plusieurs<br>3. Plusieurs employeurs, sans employeur principal<br>8. NSP<br>9. REFUS |



| 1A              | Si l'individu est non salarié (STATUT(PRO2) = 8 à 10)                                                                                                  | |--|--| | INDESAL<br>PRO7 | Au-delà de votre activité principale avez-vous eu durant la<br>dernière<br>semaine<br>travaillée<br>une<br>ou<br>plusieurs<br>activités<br>salariées ? | | Car 1           | 1. Oui<br>2. Non<br>8. NSP<br>9. REFUS                                                                                                                 |



| 1A                |                                                                                                               | |--|--| | PLURIPROF<br>PRO8 | Durant la dernière semaine travaillée, avez-vous exercé<br>plusieurs activités professionnelles différentes ? | | Car 1             | 1. Oui<br>2. Non<br>8. NSP<br>9. REFUS                                                                        |



<!-- p. 32 -->

| 1A           | Si l'individu exerce plusieurs activités professionnelles ou si l'individu est non salarié et<br>exerce un ou plusieurs activités salariés (PLURIPROF(PRO8)=1 ou INDESAL(PRO7) =<br>1) | |--|--| | AREG<br>PRO9 | Est-ce que cette ou ces activités sont régulières ?                                                                                                                                    | | Car 1        | 1. Oui<br>2. Non<br>8. NSP<br>9. REFUS                                                                                                                                                 |



| 1A                | Si l'individu exerce plusieurs activités de façon régulière (AREG (PRO9) = 1)                                       | |--|--| | STATPLUR<br>PRO10 | Exercez-vous cette ou ces activités de façon :                                                                      | | Car 1             | 1. Exclusivement salariée<br>2. Exclusivement à votre compte<br>3. Pour partie à votre compte<br>8. NSP<br>9. REFUS |



| 1A              | Si l'individu est chef d'entreprise ou indépendant ou s'il exerce une autre activité<br>professionnelle à son compte, même partiellement ( STATUT (PRO2) = 9 ou 10 ou<br>STATPLUR (PRO10) = 2 ou 3) | |--|--| | AUTENT<br>PRO11 | Exercez-vous à titre d'auto-entrepreneur ?                                                                                                                                                          | | Car 1           | 1. Oui<br>2. Non<br>8. NSP                                                                                                                                                                          |

CT-RPS 2016

| 1A               | Si l'individu est chef d'entreprise ou indépendant ou s'il exerce une autre activité<br>professionnelle à son compte, même partiellement ( STATUT (PRO2) = 9 ou 10 ou<br>STATPLUR (PRO10) = 2 ou 3) | |--|--| | LIBPRIX<br>PRO12 | Décidez-vous vous-même de vos tarifs ou prix de vente ?                                                                                                                                             | | Car 1            | 1. Oui, dans une large mesure<br>2. Oui, mais dans une faible mesure<br>3. Non<br>8. NSP                                                                                                            |



| 1A               | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | |--|--| | CLASSIF<br>PRO13 | Dans votre emploi, êtes-vous classé comme                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | | Car 2            | 1. Manœuvre ou ouvrier spécialisé<br>2. Ouvrier qualifié ou hautement qualifié<br>3. Technicien<br>4. Personnel de catégorie B ou assimilé<br>5. Agent de maîtrise, maîtrise administrative ou commerciale, vrp (non cadre)<br>6. Personnel de catégorie A ou assimilé<br>7. Ingénieur, cadre (à l'exception des directeurs généraux ou de ses adjoints directs)<br>8. Personnel de catégorie C ou D ou assimilé<br>9. Employé de bureau, employé de commerce, personnel de services<br>10. Directeur général, adjoint direct |

Les modalités proposées sont 1,2,3,4,6,8 pour les salariés du public / 1,2,3,5,7,9,10 pour les autres

Présenter la carte 3

<!-- p. 33 -->

| 1A              | Si l'individu est salariés du public et classé comme manœuvre, ouvrier qualifié ou<br>technicien (STATUT (PRO2) =1, 2, 3 ou 5 et CLASSIF (PRO8) = 1, 2, ou 3) | |--|--| | CLASS<br>PRO14A | Diriez-vous que vous êtes :                                                                                                                                   | | Car 1           | 1. de catégorie B ou assimilé<br>2. de catégorie A ou assimilé<br>3. de catégorie C ou assimilé                                                               |

CT-RPS 2016

| 1A                 | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | |--|--| | TYPEMPLOI<br>PRO15 | Quel est le type de votre emploi ?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | | Car 1              | 1. Contrat d'apprentissage ou de professionnalisation<br>2. Placement par une agence d'intérim hors CDI<br>3. Placement par une agence d'intérim en CDI (en CDI avec l'agence d'intérim)<br>4. Stage rémunéré en entreprise<br>5. Emploi aidé (contrat unique d'insertion (CUI), contrat d'accompagenemnt dans l'emploi<br>(CAE), contrat d'initiative emploi (CIE), emploi d'avenir…)<br>6. Autre emploi à durée limitée, CDD, contrat court, saisonnier, vacataire, etc.<br>7. Emploi sans limite de durée, CDI, titulaire de la fonction publique<br>8. Travail sans contrat |

Présenter la carte 3 CT 2013

| 1A                | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                                                                                                                                                                                                                                                                                    | |--|--| | FONCTION<br>PRO16 | Dans votre emploi, quelle est votre fonction principale ?                                                                                                                                                                                                                                                                                                                                              | | Car 2             | 1. Production, chantier, exploitation<br>2. Installation, réparation, maintenance<br>3. Gardiennage, nettoyage, entretien ménager<br>4. Manutention, magasinage, logistique<br>5. Secrétariat, saisie, accueil<br>6. Gestion, comptabilité<br>7. Commercial, technico-commercial<br>8. Études, recherche et développement, méthodes<br>9. Enseignement<br>10. Soin des personnes<br>11. Autre fonction |

Présenter la carte 4 CT 2013

| 1A    | Si l'individu est chef d'entreprise ou indépendant ( STATUT (PRO2) = 9 ou 10) | |--|--| | NBSAL | Combien de salariés employez-vous ?                                           | | PRO17 |                                                                               | | Car 1 | 0. Aucun<br>1. 1 ou 2 salariés                                                | |       | 2. Entre 3 et 9 salariés                                                      | |       | 3. Entre 10 et 20 salariés                                                    | |       | 4. Plus de 20 salariés                                                        |



| 1A     | Si l'individu aide un membre de sa famille ( STATUT (PRO2) = 8) | |--|--| | AIDE1E | La personne que vous aidez appartient-elle au ménage ?          | | PRO18  |                                                                 | | Car 1  | 1. Oui                                                          | |        | 2. Non                                                          |

<!-- p. 34 -->

| 1A      | Si l'individu aide un membre de sa famille ( STATUT (PRO2) = 8) | |--|--| | AIDE2E  | De qui s'agit-il ?                                              | | PRO19   |                                                                 | | Car 2   | 01 à 20                                                         | | CT 2013 |                                                                 |

| 1A                 | Si l'individu aide un membre de sa famille ( STATUT (PRO2) = 8)                                 | |--|--| | AFTYPTRAV<br>PRO20 | Faites-vous<br>un<br>travail<br>de<br>secrétariat,<br>de<br>vente<br>ou<br>de<br>comptabilité ? | | Car 1              | 1. Oui<br>2. Non                                                                                |



| 1A                | Si l'individu aide un membre de sa famille ( STATUT (PRO2) = 8)                                                    | |--|--| | NBSALAID<br>PRO22 | Combien de salariés la personne que vous aidez emploie-t<br>elle ?                                                 | | Car 1             | 0. Aucun<br>1. 1 ou 2 salariés<br>2. Entre 3 et 9 salariés<br>3. Entre 10 et 20 salariés<br>4. Plus de 20 salariés |



| 1A     |                                                                                                 | |--|--| | ENCADR | Dans votre emploi principal, vous arrive-t-il de superviser le                                  | | PRO23  | travail d'autres salariés (hors apprentis ou stagiaires) ?                                      | | Car 1  | 1. Oui, et c'est ma tâche principale<br>2. Oui, mais ce n'est pas ma tâche principale<br>3. Non | |        | 9. NSP                                                                                          |

Si hésitation ou demande des précisions :

Superviser le travail d'autres salariés regroupe par exemple les tâches suivantes :

- coordonner l'activité d'autres salariés ;
- être formellement responsable de leur activité ;
- organiser leur programme de travail au jour le jour ;
- être chargé de leur montrer comment le travail doit être fait ;
- surveiller la qualité de leur travail et/ou les délais ;etc.



| 1A               | Si salarié du public ( STATUT (PRO2) = 1,2,3,5)                                                                                                                                                                                                                                | |--|--| | TITPUBR<br>PRO24 | Êtes-vous ?                                                                                                                                                                                                                                                                    | | Car 1            | 1. Élève fonctionnaire ou fonctionnaire stagiaire<br>2. Titulaire civil<br>3. Militaire<br>4. Agent contractuel<br>5. Ouvrier d'État<br>6. Assistant(e) maternel(le)<br>7. Personnel médical hospitalier<br>8. Enseignant de l'enseignement privé sous contrat<br>9. Stagiaire |

<!-- p. 35 -->

## B - Ancienneté dans l'emploi et contrat

| 1B            | Si apprenti, stagiaire, en emploi aidé, intérimaire ou en CDD ( TYPEMPLOI (PRO15) =<br>1 à 6)                                                  | |--|--| | DUDET<br>ANC1 | Intérimaires : Quelle est la durée de votre mission d'intérim ?<br>Autres contrats à durée limitée : Quelle est la durée de votre<br>contrat ? | | Num - 8       | 01 à 99997                                                                                                                                     |

Cette durée peut être exprimée en année, en mois, en semaine ou en jour. CT 2013

| 1B                 |                                                                            | |--|--| | DUDET_DRAP<br>ANC1 | Variable "drapeau" de DUDET                                                | | Car 2              | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau CT 2013

| 1B             | Si apprenti, stagiaire, en emploi aidé, intérimaire ou en CDD ( TYPEMPLOI (PRO15) =<br>1 à 6) | |--|--| | DUDETU<br>ANC2 | Durée exprimée en …                                                                           | | Car 1          | 1. année<br>2. mois<br>3. semaine<br>4. jour                                                  |



| 1B          | Si apprenti, stagiaire, en emploi aidé, intérimaire ou en CDD ( TYPEMPLOI (PRO15) =<br>1 à 6) | |--|--| | TPS_CONTRAT | Durée en mois du contrat ou de la mission                                                     | | Num 8       |                                                                                               |

CALCULEE à partir des réponses à DUDET et DUDETU CT 2013

| 1B              | Si apprenti, stagiaire, en emploi aidé, intérimaire ou en CDD ( TYPEMPLOI (PRO15) =<br>1 à 6) | |--|--| | DEBDETA<br>ANC3 | Quand a débuté ce contrat ?<br>Année…                                                         | | Num - 8         | 1950 à année de l'enquête                                                                     | | CT 2013         |                                                                                               |

| 1B           |                               | |--|--| | DEBDETA_DRAP | Variable "drapeau" de DEBDETA | | ANC3         |                               | | Car 2        | 1. Réponse                    | |              | 0. Sans objet                 | |              | -1 . Ne sait pas              | |              | -2 . Refuse de répondre       |

Variable drapeau

<!-- p. 36 -->

| 1B              | Si apprenti, stagiaire, en emploi aidé, intérimaire ou en CDD ( TYPEMPLOI (PRO15) =<br>1 à 6) | |--|--| | DEBDETM<br>ANC3 | Quand a débuté ce contrat ?<br>Mois…                                                          | | Num - 8         | 01 à 12                                                                                       |



| Variable "drapeau" de DEBDETM                                              | |--| | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre | |                                                                            |

Variable drapeau



| 1B            | Si apprenti, stagiaire, en emploi aidé, intérimaire ou en CDD ( TYPEMPLOI (PRO15) =<br>1 à 6) | |--|--| | DEBUT_CONTRAT | Durée écoulée depuis le début du contrat ou de la mission ?                                   | | Num - 8       |                                                                                               |

CALCULEE à partir des réponses à DEBDETA et DEBDETM CT 2013

| 1B             | Si intérimaire ( TYPEMPLOI (PRO15) = 2 ou 3)                | |--|--| | TPSINT<br>ANC4 | Depuis combien de temps êtes-vous travailleur intérimaire ? | | Num - 8        | 01 à 99997                                                  |



| 1B          |                              | |--|--| | TPSINT_DRAP | Variable "drapeau" de TPSINT | | ANC4        |                              | | Car 2       | 1. Réponse                   | |             | 0. Sans objet                | |             | -1 . Ne sait pas             | |             | -2 . Refuse de répondre      |

Variable drapeau



| 1B      | Si intérimaire ( TYPEMPLOI (PRO15) = 2 ou 3) | |--|--| | TPSINTU | Durée exprimée en …                          | | ANC5    |                                              | | Car 1   | 1. année                                     | |         | 2. mois                                      | |         | 3. semaine                                   | |         | 4. jour                                      |



| 1B           | Si stagiaire, en emploi aidé, intérimaire ou en CDD (TYPEMPLOI (PRO15) = 2 à 6)                                       | |--|--| | RDET<br>ANC6 | Est-ce votre choix d'être en contrat d'intérim / en stage<br>rémunéré / en emploi aidé / en contrat à durée limitée ? | | Car 1        | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                |

<!-- p. 37 -->

| 1B          | Si intérimaire ( TYPEMPLOI (PRO15) = 2 ou 3)    | |--|--| | TPS_INTERIM | Durée en mois de travail en tant qu'intérimaire | | Num 8       |                                                 |

CALCULEE à partir des réponses à TPSINT et TPSINTU CT 2013

| 1B             |                                                                                                                                                                                                                                                                                      | |--|--| | DATANT<br>ANC7 | Si STATUT (PRO2) = 1, 2, 3, 5 : A quelle date avez-vous commencé à<br>travailler dans la fonction publique ?<br>Si STATUT (PRO2) = 4, 6 : A quelle date avez-vous commencé à<br>travailler pour cette entreprise, cet artisan ou cette association ?<br>Si STATUT (PRO2) =<br>Année… | | Num - 8        | 1950 à année de l'enquête                                                                                                                                                                                                                                                            |

Si STATUT (PRO2) = 4, 6 : Certaines entreprises ou administrations comptent plusieurs établissements c'est pourquoi nous parlons ici de l'entreprise et non pas de l'établissement. Par exemple, une entreprise commerciale peut avoir un siège et plusieurs points de vente, une entreprise industrielle plusieurs unités de production.Si STATUT (PRO2) = 9 ou 10 : Affaire peut désigner votre cabinet, commerce, atelier, société, exploitation CT 2013

| 1B          |                              | |--|--| | DATANT_DRAP | Variable "drapeau" de DATANT | | ANC7        |                              | | Car 2       | 1. Réponse                   | |             | 0. Sans objet                | |             | -1 . Ne sait pas             | |             | -2 . Refuse de répondre      |

Variable drapeau CT 2013

| 1B             | Si année 2005 ou postérieure (DATANT>=2005)                                                                                                                                                                                                                                | |--|--| | DAMOIS<br>ANC8 | Si STATUT (PRO2) = 1, 2, 3, 5 : A quelle date avez-vous commencé à<br>travailler dans la fonction publique ?<br>Si STATUT (PRO2) = 4, 6 : A quelle date avez-vous commencé à<br>travailler pour cette entreprise, cet artisan ou cette association ?<br>Si STATUT (PRO2) = | |                | Mois…                                                                                                                                                                                                                                                                      | | Num - 8        | 01 à 12                                                                                                                                                                                                                                                                    |

Si STATUT (PRO2) = 4, 6 : Certaines entreprises ou administrations comptent plusieurs établissements c'est pourquoi nous parlons ici de l'entreprise et non pas de l'établissement. Par exemple, une entreprise commerciale peut avoir un siège et plusieurs points de vente, une entreprise industrielle plusieurs unités de production.Si STATUT (PRO2) = 9 ou 10 : Affaire peut désigner votre cabinet, commerce, atelier, société, exploitation CT 2013

| 1B                  |                                                                            | |--|--| | DAMOIS_DRAP<br>ANC8 | Variable "drapeau" de DAMOIS                                               | | Car 2               | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau CT 2013

<!-- p. 38 -->

| 1B         |                                                                        | |--|--| | ANCIENNETE | Ancienneté dans l'entreprise / la fonction publique / la<br>profession | | Num - 8    |                                                                        |

CALCULEE : à partir des réponses à DAMOIS et DATANT CT 2013

# C - Établissement employeur

| 1C            | Si salarié (STATUT(PRO2) = 1 à 7)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | |--|--| | LIEUW<br>ETA1 | Où passez-vous la plus grande partie de votre temps de travail<br>?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | | Car 1         | 1. Dans l'établissement qui vous emploie (y compris salariés payés par le siège de<br>l'entreprise ou une direction centrale…)<br>2. Dans différents établissements de l'entreprise / l'administration qui vous emploie (ex :<br>formateur qui travaille dans les différents établissements d'une entreprise…)<br>3. Dans une autre entreprise où vous êtes envoyé par votre employeur (ex : intérimaire,<br>prestataire de services…)<br>4. Dans différentes entreprises clientes de votre employeur (ex : réparateur<br>d'ascenseurs…)<br>5. En déplacement (ex : hôtesse de l'air, VRP, chauffeur routier, conducteur…)<br>6. Chez un ou plusieurs particuliers<br>7. A votre domicile (ex : assistante maternelle, traducteur)<br>8. Sur un ou plusieurs chantiers<br>9. Autre situation |

Si cela varie souvent, prendre la dernière semaine travaillée. Présenter la carte 5 CT 2013

| 1C              | Si salarié (STATUT(PRO2) = 1 à 7)                                                                                                                                           | |--|--| | TELTRAV<br>ETA3 | Pratiquez-vous le télétravail ?                                                                                                                                             | | Car 1           | 1. Quelques jours ou demi-journées par mois<br>2. Un jour par semaine<br>3. Deux jours par semaine<br>4. Trois jours ou plus par semaine<br>5. Jamais<br>8. REFUS<br>9. NSP |

Désigne toute forme d'organisation du travail dans laquelle un travail qui aurait pu être exécuté dans les locaux de l'employeur est effectué hors de ces locaux de façon volontaire, en utilisant les technologies de l'information et de la communication, à distinguer du fait de ramener du travail à la maison. 

| 1C             | Si le salarié pratique le télétravail (TELTRAV(ETA3) = 1 à 4)                                                                              | |--|--| | TELEOU<br>ETA4 | Où pratiquez-vous ce télétravail ?                                                                                                         | | Car 1          | 1. Chez vous dans une pièce dédiée au travail<br>2. Chez vous dans une pièce de vie<br>3. A l'extérieur de chez vous<br>8. REFUS<br>9. NSP |



<!-- p. 39 -->

| 1C      |                                                      | |--|--| | TRAJET  | En combien de temps en moyenne faites-vous le trajet | | ETA5    | domicile-travail ?                                   | | Num - 8 |                                                      |

Temps en minutes, voyage aller, lieu de travail le plus courant, hors détours éventuels. Si pas de trajet habituel mettre 997, si domicile est le lieu de travail mettre 0 CT 2013

| 1C          |                              | |--|--| | TRAJET_DRAP | Variable "drapeau" de TRAJET | | ETA5        |                              | | Car 2       | 1. Réponse                   | |             | 0. Sans objet                | |             | -1 . Ne sait pas             | |             | -2 . Refuse de répondre      |

Variable drapeau CT 2013

| 1C               | Si le temps de trajet n'est pas nul (TRAJET(ETA5) # 0)                                                            | |--|--| | PREVTRAJ<br>ETA6 | Cette durée est-elle variable d'un jour à l'autre ?                                                               | | Car 1            | 1. Oui, dans une forte mesure<br>2. Oui mais dans une faible mesure<br>3. Non (ou très peu)<br>8. REFUS<br>9. NSP |



| 1C       |                                            | |--|--| | CONFTRAJ | Ce trajet est-il inconfortable, fatigant ? | | ETA7     |                                            | | Car 1    | 1. Oui                                     | |          | 2. Non                                     | |          | 8. REFUS                                   | |          | 9. NSP                                     |



| 1C                    | Pour tous les salariés sauf de particuliers.<br>Si STATUT (PRO2) = 1 à 6  | |--|--| | ACTIVFIN<br>ETA8 à 10 | Quelle est l'activité de cet établissement ?<br>NAF 2008 sur 88 positions | | Car 2                 | Voir nomenclature                                                         |

CALCULEE : Réponse en clair (ACTIV) et précisions si non reconnu => Codage en NAF 88 positions CT

| 1C    | Pour tous les salariés sauf de particuliers.<br>Si STATUT (PRO2) = 1 à 6  | |--|--| | NAF38 | Quelle est l'activité de cet établissement ?<br>NAF 2008 sur 38 positions | | Car 2 | Voir nomenclature                                                         |

CALCULEE : Réponse en clair (ACTIV) et précisions si non reconnu => Codage en NAF rev2 17 positions CT

<!-- p. 40 -->

| 1C    | Pour tous les salariés sauf de particuliers.<br>Si STATUT (PRO2) = 1 à 7  | |--|--| | NAF17 | Quelle est l'activité de cet établissement ?<br>NAF 2008 sur 17 positions | | Car 2 | Voir nomenclature                                                         |

CALCULEE : Réponse en clair (ACTIV) et précisions si non reconnu => Codage en NAF rev2 17 positions CT

| 1C    | Pour tous les salariés sauf de particuliers.<br>Si STATUT (PRO2) = 1 à 8                   |  | |--|--|--| | NAF4  | Quelle est l'activité de cet établissement ?                                               |  | | Car 2 | ES. Agriculture<br>ET. Industrie<br>EU. Construction<br>EV. Tertiaire<br>00. Non renseigné |  |

CALCULEE : Réponse en clair (ACTIV) et précisions si non reconnu => Codage en NAF rev2 17 positions CT

| 1C      | Si activité agricole (ACTIVFIN="01")                        | |--|--| | SUPH    | Quelle est la superficie de cette exploitation (en hectares | | ETA11   | s.a.u) ?                                                    | | Num - 8 | 0 à 999                                                     |

Pour les GAEC (Groupements d'exploitation agricole en commun), c'est la superficie totale du GAEC qu'il faut saisir. CT

| 1C      | Si superficie inférieure à 5 Ha (SUPH (ETA7) <5) | |--|--| | SUPA    | Quelle est précisément la superficie en ares ?   | | ETA12   |                                                  | | Num - 8 | 0 à 499                                          | | CT      |                                                  |

| 1C           | Si activité agricole (ACTIVFIN="01")                                                                                                                                                                                                                                                                 | |--|--| | OPA<br>ETA13 | Quelle est l'orientation des productions agricoles ?                                                                                                                                                                                                                                                 | | Car 1        | 1. Polyculture (culture des terres labourables)<br>2. Maraîchage ou horticulture<br>3. Vigne ou arbres fruitiers<br>4. Élevage d'herbivores (bovins, ovins,…)<br>5. Élevage de granivores (volailles, porcins,…)<br>6. Polyculture – élevage<br>7. Élevage d'herbivores et de granivores<br>8. Autre | | CT           |                                                                                                                                                                                                                                                                                                      |

1C Si non salarié (STATUT(PRO2) = 8 à 10)

| CLIENT<br>ETA14 | Vos clients, c'est-à-dire les personnes qui vous payent, sont<br>ils …                                                                                     | |--|--| | Car 1           | 1. uniquement des particuliers<br>2. une seule entreprise<br>3. uniquement des entreprises<br>4. des entreprises et des particuliers<br>8. REFUS<br>9. NSP |

<!-- p. 41 -->

| 1C        | Si a pour clients des entreprises (CLIENT(ETA14) = 3 ou 4)         | |--|--| | POURCLIEN | Quelle proportion de votre chiffre d'affaires votre client le plus | | ETA15     | important représente-t-il ?                                        | | Num - 8   | 2 à 99                                                             |

Sur la dernière année et en pourcentagea CT 2013

| 1C            |                                                                            | |--|--| | POURCLIEN_DRA | Variable "drapeau" de POURCLIEN                                            | | P             |                                                                            | | ETA15         |                                                                            | | Car 2         | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau



| 1C      | Si salarié (STATUT(PRO2) = 1 à 7)                      | |--|--| | ANETA   | Depuis quelle année êtes-vous dans cet établissement ? | | ETA16   |                                                        | | Num - 8 | 1950 à l'année de l'enquête                            | |         |                                                        |



| 1C         |                             | |--|--| | ANETA_DRAP | Variable "drapeau" de ANETA | | ETA16      |                             | | Car 2      | 1. Réponse                  | |            | 0. Sans objet               | |            | -1 . Ne sait pas            | |            | -2 . Refuse de répondre     |

Variable drapeau



| 1C    | Si l'individu est arrivé dans son établissement après 2011 | |--|--| | ANETM | Depuis quel mois ?                                         | | ETA17 |                                                            | | Car 2 |                                                            |

CT-RPS 2016

| 1C                  |                                         | |--|--| | ANETM_DRAP<br>ETA17 | Variable "drapeau de ANETM              | | Car 2               | 1. Réponse<br>0. Sans objet             | |                     | 1. Ne sait pas<br>2. Refuse de répondre |

Variable drapeau

CT-RPS 2016

<!-- p. 42 -->

| 1C      | Panel. Si l'individu a exercé sans interruption la même profession depuis la dernière<br>enquête, s'il a confirmé le nom de l'établissement et si le code postal et la commune<br>déclarés à RPS 2016 sont renseignés                                                                              | |--|--| | RPSETAB | L'adresse de l'établissement où vous travaillez est "". Est                                                                                                                                                                                                                                        | | ETA18   | ce exact ?                                                                                                                                                                                                                                                                                         | | Car 1   | 1. Oui, l'adresse est exacte<br>2. Oui, vous y travaillez toujours mais vous souhaitez apporter des modifications à<br>l'adresse (changement de raison de sociale, orthographe par exemple)<br>3. Non, vous travaillez dans un autre établissement<br>4. Non, l'établissement a déménagé<br>9. NSP |

Merci de vérifier avec précision CT-RPS 2016

| 1C     |                                                             | |--|--| | FRANCE | L'établissement dans lequel vous travaillez actuellement se | | ETA19  | situe-t-il en France ?                                      | | Car 1  | 1. Oui                                                      | |        | 2. Non                                                      | |        | 8. REFUS                                                    | |        | 9. NSP                                                      |



| 1C              | Si salarié (STATUT(PRO2) = 1 à 7)                                                                                                          | |--|--| | NBSALA<br>ETA23 | Combien y-a-t-il environ de salariés dans cet établissement ?                                                                              | | Car 3           | 1. 1 à 4<br>2. 5 à 9<br>3. 10 à 19<br>4. 20 à 49<br>5. 50 à 199<br>6. 200 à 499<br>7. 500 à 999<br>8. 1000 et plus<br>98. REFUS<br>99. NSP |



| 1C               | Si salarié du privé (STATUT(PRO2) = 4,6)             | |--|--| | AUTRETA<br>ETA24 | Y-a-t-il d'autres établissements dans l'entreprise ? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP               |



| 1C              | Si autres établissements (AUTRETA (ETA20) = 1)                                                | |--|--| | NBSALB<br>ETA25 | Au<br>total,<br>environ<br>combien<br>de<br>salariés<br>travaillent<br>dans<br>l'entreprise ? | | Car 2           | 1. 1 à 49 salariés<br>2. 50 à 499<br>3. 500 et plus<br>8. REFUS<br>9. NSP                     |

<!-- p. 43 -->

| 1C        |                                                                                    | |--|--| | NBSALENTC | Nombre de salariés dans l'entreprise (y compris entreprises<br>mono-établissement) | | Car 1     | 1. 1 à 49 salariés<br>2. 50 à 499<br>3. 500 et plus<br>8. REFUS<br>9. NSP          |

## CALCULEE :

Si AUTRETA=2 et NBSALA=1,2,3,4 alors NBSALENTC=1 (variable caractère)

Sinon Si AUTRETA=2 et NBSALA=5,6 alors NBSALENTC=2

Sinon Si AUTRETA=2 et NBSALA=7,8 alors NBSALENTC=3

Sinon Si AUTRETA=2 et NBSALA=99 alors NBSALENTC=9

Sinon Si AUTRETA=2 et NBSALA=98 alors NBSALENTC=8

Sinon, NBSALENTC=NBSALB



| 1C               |                                                                                                 | |--|--| | EVOLEFF<br>ETA26 | Comment ont évolué les effectifs dans votre établissement au<br>cours des douze derniers mois ? | | Car 1            | 1. A la baisse<br>2. A la hausse<br>3. Stable<br>8. REFUS<br>9. NSP                             |

#### CT-RPS 2016

| 1C         |                                                            | |--|--| | CHAMP_EMPL | Indicateur d'un employeur dans le champ du volet Employeur | | Car 1      | 1. Oui<br>2. Non                                           |

CALCULEE : IF STATUT in ('7' '8' '9' '10') OR NBSALA in ('1' '2') OR FRANCE='2' THEN CHAMP\_EMPL='2'; ELSE CHAMP\_EMPL='1';



| 1C         |                                                       | |--|--| | QUAL_EMPLX | Indicateur de renseignement du nom de l'établissement | | Car 1      | A. Renseigné<br>B. Non renseigné                      |



| 1C       |                                                                                                                                                                                                                                                                                                                                                   | |--|--| | QUAL_ADR | Indicateur de renseignement de l'adresse de l'établissement                                                                                                                                                                                                                                                                                       | | Car 1    | A. Adresse complète : Les champs numc, tvoic, voic, comc et codposc sont renseignés<br>B. Adresse partielle : sont renseignés au moins le libellé de voie (VOIC) et soit la<br>commune (COMC) soit le code postal (codposc)<br>C. Adresse incomplète : Seule la commune (COMC) ou le code postal (CODPOSC) sont<br>renseignés<br>D. Pas d'adresse |



| 1C         |                                                         | |--|--| | QUAL_SIRET | Indicateur de renseignement du SIRET de l'établissement | | Car 1      | A. Renseigné                                            | |            | B. Non renseigné                                        |

CALCULEE

<!-- p. 44 -->

| 1C            |                                                                                    |     | |--|--|--| | QUALITE_VOLET | Indicateur<br>de<br>qualité<br>des<br>renseignements<br>fournis<br>l'établissement | sur | | Car 3         | De AAA (Nom, SIRET et adresses complètes) à BBD (Aucune information)               |     |

CALCULEE : Concaténation des 3 variables précédentes : QUALITE\_VOLET = QUAL\_EMPLX !! QUAL\_SIRET !! QUAL\_ADR AAA = info "parfaite" / BBD = "aucune info" CT 2013

## D - Rémunération

| 1D             | Si salarié (STATUT(PRO2) = 1 à 7)                                                                                                                                                                         | |--|--| | REVSAL<br>REM1 | Au cours du dernier mois, quel montant net en EUROS avez<br>vous perçu pour votre activité professionnelle (salaires,<br>primes, traitements et revenus d'une activité professionnelle<br>non salariée) ? | | Num - 8        | 0 à 99.999.997                                                                                                                                                                                            |

Revenus nets de cotisations sociales. Ne pas compter les avantages en nature. Prend la valeur de REVSAL (REM1) pour les salariés CT

| 1D                  |                                                                            | |--|--| | REVSAL_DRAP<br>REM1 | Variable "drapeau" de REVSAL                                               | | Car 2               | 1. Reponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau de REVSAL CT

| 1D              | Si non salarié (STATUT(PRO2) = 8 à 10)                                                                                                                                                                       | |--|--| | REVANNU<br>REM5 | Au cours de la dernière année, quel montant net en euros<br>avez-vous perçu pour votre activité professionnelle (revenus<br>d'une activité professionnelle non salariée, salaires, primes,<br>traitements) ? | | Num - 8         | 0 à 99.999.997                                                                                                                                                                                               |

Revenus nets de cotisations sociales. Ne pas compter les avantages en nature. Prend la valeur de REVNSAL (REM5) pour les non salariés CT

| 1D                   |                                                                            | |--|--| | REVANNU_DRAP<br>REM5 | Variable "drapeau" de REVANNU                                              | | Car 2                | 1. Reponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau de REVANNU

<!-- p. 45 -->

| 1D               | Si le montant est nsp ou refus (REVSAL (REM1) = NSP ou REFUS)                                                                                                                                                                                                                                                                                                                                                                                                                                                            | |--|--| | REVTRSAL<br>REM2 | Si vous ne pouvez pas donner un montant précis de ces<br>ressources, à combien les estimez-vous pour un mois<br>ordinaire ?                                                                                                                                                                                                                                                                                                                                                                                              | | Car 2            | 1. à moins de 400 €<br>2. de 400 € à moins de 600 €<br>3. de 600 € à moins de 800 €<br>4. de 800 € à moins de 1 000 €<br>5. de 1 000 € à moins de 1 200 €<br>6. de 1 200 € à moins de 1 500 €<br>7. de 1 500 € à moins de 1 800 €<br>8. de 1 800 € à moins de 2 000 €<br>9. de 2 000 € à moins de 2 500 €<br>10. de 2 500 € à moins de 3 000 €<br>11. de 3 000 € à moins de 4 000 €<br>12. de 4 000 € à moins de 6 000 €<br>13. de 6 000 € à moins de 10 000 €<br>14. à 10 000 € ou plus<br>98. Refus<br>99. Ne sait pas |

Revenus nets de cotisations sociales. Ne pas compter les avantages en nature. Présenter la carte 7 CT

| 1D              | Si non salarié (STATUT (PRO2)= 9, 10)                                                                                                                                                                                  | |--|--| | REVNSAL<br>REM5 | Au cours du dernier mois, quel montant net en EUROS avez<br>vous perçu pour votre activité professionnelle (revenus d'une<br>activité<br>professionnelle<br>non<br>salariée,<br>salaires,<br>primes,<br>traitements) ? | | Num 8           | 0 à 99.999.997                                                                                                                                                                                                         |

Revenus nets de cotisations sociales. Ne pas compter les avantages en nature. Prend la valeur de REVNSAL (REM5) pour les non salariés CT

| 1D                   |                                                                            | |--|--| | REVNSAL_DRAP<br>REM5 | Variable "drapeau" de REVNSAL                                              | | Car 2                | 1. Reponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau de REVNSAL

CT

| 1D              |                                                                                                                                                                                                                                                                                                                        | |--|--| | REVMENS<br>REM1 | Si non salarié (STATUT (PRO2) = 8 à 10), formulation entre () :<br>Au cours du dernier mois (de la dernière année), quel montant<br>net<br>EN<br>EUROS<br>avez-vous<br>perçu<br>pour<br>votre<br>activité<br>professionnelle (revenus d'une activité professionnelle non<br>salariée, salaires, primes, traitements) ? | | Num - 8         | 0 à 99.999.997                                                                                                                                                                                                                                                                                                         |

Revenus nets de cotisations sociales. Ne pas compter les avantages en nature. CT

<!-- p. 46 -->

| 1D                   |                                                                            | |--|--| | REVMENS_DRAP<br>REM1 | Variable "drapeau" de REVMENS                                              | | Car 2                | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau

CT

| 1D       |                                         | |--|--| | REVMENSC | Revenu MENSUEL (y compris non salariés) | | REM1     |                                         | | Num - 8  | 0 à 99.999.997                          |

CALCULEE : Prend la valeur de REVSAL pour les salariés et REVANNU/12 pour les non salariés CT

| 1D           |                                                                            | |--|--| | REVMENSC_DRA | Variable "drapeau" de REVMENSC                                             | | P            |                                                                            | | REM1         |                                                                            | | Car 2        | 1. Reponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau de REVMENSC\_DRAP

CT

| 1D               | Si le montant est nsp ou refus (REVNSAL (REM5) = NSP ou REFUS)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | |--|--| | REVTRNSA<br>REM6 | Si vous ne pouvez pas donner un montant précis de ces<br>ressources, à combien les estimez-vous ?                                                                                                                                                                                                                                                                                                                                                                                                                                                        | | Car 2            | 1. à moins de 4 800 €<br>2. de 4 800 € à moins de 7 200 €<br>3. de 7 200 € à moins de 9 600 €<br>4. de 9 600 € à moins de 12 000 €<br>5. de 12 000 € à moins de 14 400 €<br>6. de 14 400 € à moins de 18 000 €<br>7. de 18 000 € à moins de 21 600 €<br>8. de 21 600 € à moins de 24 000 €<br>9. de 24 000 € à moins de 30 000 €<br>10. de 30 000 € à moins de 36 000 €<br>11. de 36 000 € à moins de 48 000 €<br>12. de 48 000 € à moins de 72 000 €<br>13. de 72 000 € à moins de 120 000 €<br>14. à 120 000 € ou plus<br>98. Refus<br>99. Ne sait pas |

Revenus nets de cotisations sociales. Ne pas compter les avantages en nature. Présenter la carte 8 CT

| 1D              | Si le revenu n'est pas nul (REVSAL (REM1) # 0 ou REVNSAL # 0)                              | |--|--| | PREVSAL<br>REM7 | Pouvez-vous prévoir vos revenus professionnels pour les 3<br>prochains mois ?              | | Car 1           | 1. Oui, assez précisément<br>2. Oui mais approximativement<br>3. Non<br>8. REFUS<br>9. NSP |



<!-- p. 47 -->

| 1D              | Si le revenu n'est pas nul (REVMENS (REM1) # 0)                                                                          | |--|--| | PAYECOM<br>REM8 | Compte tenu du travail que vous réalisez, diriez-vous que<br>vous êtes ?                                                 | | Car 1           | 1. Très bien payé<br>2. Bien payé<br>3. Normalement payé<br>4. Plutôt mal payé<br>5. Très mal payé<br>8. REFUS<br>9. NSP |

<!-- p. 48 -->

## 2. Organisation du temps de travail

### A - Durée du travail, temps partiel

| 2A          | Si l'individu a un contrat de travail ( TYPEMPLOI (PRO15) # 8 et STATUT (PRO2) # 4,6<br>et 7) | |--|--| | TPP<br>DUR1 | Dans votre emploi principal, travaillez-vous ?                                                | | Car 1       | 1. A temps complet<br>2. A temps partiel<br>8. REFUS<br>9. NSP                                |



| 2A             | Si temps partiel (TPP (DUR1) = 2)                                                                                            | |--|--| | TXTPPB<br>DUR2 | Quel est le type de ce temps partiel ?                                                                                       | | Car 1          | 1. Moins d'un mi-temps (50%)<br>2. Mi-temps (50%)<br>3. Entre 50 et 80 %<br>4. 80 %<br>5. Plus de 80 %<br>8. REFUS<br>9. NSP |



| 2A             | Si temps partiel (TPP (DUR1) = 2)                                                                                                                                                                                                                                                                                           | |--|--| | RAISTP<br>DUR3 | Pour quelle raison principale travaillez-vous à temps partiel ?                                                                                                                                                                                                                                                             | | Car 1          | 1. Vous n'avez pas la possibilité de travailler davantage avec votre emploi actuel<br>2. Pour exercer une autre activité professionnelle<br>3. Pour suivre des études ou une formation<br>4. Pour compléter d'autres revenus (retraite par exemple)<br>5. Pour des raisons personnelles ou familiales<br>8. REFUS<br>9. NSP |

Assistant(e) maternel(le) : si la raison est que la personne n'a pas trouvé d'autres enfants à garder, répondre "1. Vous n'avez pas la possibilité de travailler d'avantage avec votre emploi actuel" 

| 2A             | Si la personne travaille à temps partiel pour des raisons personnelles ou familiales<br>(RAISTP(DUR3) = 5)                                                                                                            | |--|--| | RAISTF<br>DUR4 | Si la personne travaille à temps partiel pour des raisons personnelles ou<br>familiales<br>Précisez, est-ce…                                                                                                          | | Car 1          | 1. Pour des raisons de santé<br>2. Pour vous occuper de vos enfants ou d'une personne dépendante<br>3. Pour disposer de temps libre<br>4. Pour faire des travaux domestiques<br>5. Autre raison<br>8. REFUS<br>9. NSP |



| 2A      |                                                         | |--|--| | HH      | Dans le cadre de votre emploi principal, habituellement | | DUR5    | combien d'heures travaillez-vous par semaine ?          | | Num - 8 | De 1 à 168                                              |

Il s'agit des heures habituellement effectuées, et non de celles figurant sur le contrat de travail quand il y en a un. CT 2013

<!-- p. 49 -->

| 2A              |                                                                            | |--|--| | HH_DRAP<br>DUR5 | Variable "drapeau" de HH                                                   | | Car 2           | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau CT 2013

| 2A            | Si l'individu a plusieurs employeurs (PLURIEMP(PRO6) = 2 ou 3 ou INDESAL (PRO7) =<br>1 ou PLURIPROF (PRO8) = 1)                                    | |--|--| | HHTOT<br>DUR6 | Et<br>en<br>tout,<br>en<br>tenant<br>compte<br>de<br>vos<br>autres<br>emplois,<br>habituellement combien d'heures travaillez-vous par semaine<br>? | | Num - 8       | De 1 à 168                                                                                                                                         |

Il s'agit des heures habituellement effectuées, et non de celles figurant sur les contrats de travail. CT 2013

| 2A                 |                                                                            | |--|--| | HHTOT_DRAP<br>DUR6 | Variable "drapeau" de HHTOT                                                | | Car 2              | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau CT 2013

# B - Calendrier hebdomadaire et mensuel de travail

| 2B      |                                                               | |--|--| | JOURTR  | Dans votre emploi principal, combien de jours travaillez-vous | | CAL1    | habituellement par semaine ?                                  | | Num - 8 | Valeurs de 0 à 7, entiers ou demi-entiers                     | |         |                                                               |

Indiquer le NOMBRE MOYEN DE JOURS DE PRESENCE au travail. Saisir un nombre entier ou demi-entier (par exemple : « 3 » pour trois jours ou « 3.5 » pour trois jours et demi). Si la personne a des horaires très variables ou alternants, elle doit essayer de décrire les horaires habituels, ceux qui reviennent le plus fréquemment. Si elle n'y parvient pas, décrire la dernière journée ou semaine « normalement » travaillée. CT 2013

| 2B          |                              | |--|--| | JOURTR_DRAP | Variable "drapeau" de JOURTR | | CAL1        |                              | | Car 2       | 1. Réponse                   | |             | 0. Sans objet                | |             | -1 . Ne sait pas             | |             | -2 . Refuse de répondre      |

Variable drapeau

<!-- p. 50 -->

| 2B    |                                                           | |--|--| | REPOS | Disposez-vous d'au moins 48 heures consécutives de repos, | | CAL2  | au cours d'une semaine ?                                  | | Car 1 | 1. Oui                                                    | |       | 2. Non                                                    | |       | 8. REFUS                                                  | |       | 9. NSP                                                    |



| 2B     |                             | |--|--| | SAMEDI | Travaillez-vous le samedi ? | | CAL3   |                             | | Car 1  | 1. Habituellement           | |        | 2. Occasionnellement        | |        | 3. Jamais                   |



| 2B             | Si travail le samedi (SAMEDI (CAL3) =1 ou 2)                                                                                   | |--|--| | NBSAME<br>CAL4 | Combien de samedis par an travaillez-vous sur votre lieu de<br>travail ?                                                       | | Num - 8        | 0. Aucun<br>1. Un seul<br>2. De 2 à 5<br>3. De 6 à 10<br>4. De 11 à 20<br>5. De 21 à 40<br>6. Plus de 40<br>8. REFUS<br>9. NSP |

NBSAME\_DRAP Pour qualifier la non réponse voir NBSAME\_DRAP

Cela exclut le travail emporté à la maison et effectué le samedi, si c'est uniquement cela, mettre "aucun". On peut estimer qu'une personne qui travaille tous les samedis travaille 47 samedis par an. CT 2013

| 2B          |                              | |--|--| | NBSAME_DRAP | Variable "drapeau" de NBSAME | | CAL4        |                              | | Car 2       | 1. Réponse<br>0. Sans objet  | |             | -1 . Ne sait pas             | |             | -2 . Refuse de répondre      |

Variable drapeau



| 2B       |                               | |--|--| | DIMANCHE | Travaillez-vous le dimanche ? | | CAL5     |                               | | Car 1    | 1. Habituellement             | |          | 2. Occasionnellement          | |          | 3. Jamais                     |

<!-- p. 51 -->

| 2B             | Si travail le dimanche (DIMANCHE (CAL5) =1 ou 2)                                                                               | |--|--| | NBDIMA<br>CAL6 | Combien de dimanches par an travaillez-vous sur votre lieu<br>de travail ?                                                     | | Num - 8        | 0. Aucun<br>1. Un seul<br>2. De 2 à 5<br>3. De 6 à 10<br>4. De 11 à 20<br>5. De 21 à 40<br>6. Plus de 40<br>8. REFUS<br>9. NSP |

NBDIMA\_DRAP Pour qualifier la non réponse voir NBDIMA\_DRAP

Cela exclut le travail emporté à la maison et effectué le dimanche, si c'est uniquement cela, mettre "Aucun". On peut estimer qu'une personne qui travaille tous les dimanches travaille 47 dimanches par an. CT 2013

| 2B                  |                                                                            | |--|--| | NBDIMA_DRAP<br>CAL6 | Variable "drapeau" de NBDIMA                                               | | Car 2               | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau



# C - Prévision des horaires

| 2C             |                                                          | |--|--| | HORDET<br>PRH1 | Pouvez-vous modifier vous-même vos horaires de travail ? | | Car 1          | 1. Oui<br>2. Non<br>3. Sans objet<br>8. REFUS<br>9. NSP  |



| 2C      | Si l'individu est salarié (hors salariés de particuliers) (STATUT = 1 à 6) | |--|--| | FORFAIT | Votre durée de travail est-elle déterminée sous la forme d'un              | | PRH2    | forfait en jours ?                                                         | | Car 1   | 1. Oui                                                                     | |         | 2. Non                                                                     | |         | 8. REFUS                                                                   | |         | 9. NSP                                                                     |

Le principe du forfait jour repose sur le décompte du temps de travail en nombre de jours par an plutôt qu'en heures. 

| 2C      |                                                        | |--|--| | PREMOIS | Connaissez-vous les horaires que vous devrez effectuer | | PRH3    | a) … dans le mois à venir ?                            | | Car 1   | 1. Oui                                                 | |         | 2. Non                                                 | |         | 8. REFUS                                               | |         | 9. NSP                                                 |

<!-- p. 52 -->

| 2C     | Si ne connaît pas ses horaires du mois à venir (PREMOIS (PRH3) =2) | |--|--| | PRESEM | Connaissez-vous les horaires que vous devrez effectuer             | | PRH4   | a) … la semaine prochaine ?                                        | | Car 1  | 1. Oui<br>2. Non<br>8. REFUS                                       | |        | 9. NSP                                                             |



| 2C      | Si ne connaît pas ses horaires du mois à venir (PRESEM (PRH4) =2) | |--|--| | PREJOUR | Connaissez-vous les horaires que vous devrez effectuer            | | PRH5    | a) … demain ?                                                     | | Car 1   | 1. Oui                                                            | |         | 2. Non                                                            | |         | 8. REFUS                                                          | |         | 9. NSP                                                            |



| 2C     |                                                                                                 | |--|--| | PREVIS | Connaissance des horaires à effectuer…                                                          | | Car 1  | 1. Dans le mois à venir<br>2. La semaine prochaine<br>3. Demain<br>4. Non<br>8. REFUS<br>9. NSP |

CALCULEE : Vaut 1 si PREMOIS=1, sinon vaut 2 si PRESEM=1, sinon vaut 3 si PREJOUR=1, sinon vaut 4 si PREJOUR=2,

sinon si PREMOIS=8 ou PRESEM=8 ou PREJOUR=8, alors PREVIS=8 sinon si PREMOIS=9 ou PRESEM=9 ou PREJOUR=9, alors PREVIS=9 sinon à vide CT 2013

# D - Horaires quotidiens

| 2D             |                                                                                                                                                                                 |  | |--|--|--| | HORVAR<br>HOR1 | Vos horaires de travail quotidiens sont-ils ?                                                                                                                                   |  | | Car 1          | 1. Les mêmes tous les jours<br>2. Alternants 2x8 (équipes, brigades)<br>3. Alternants 3x8 (équipes, brigades) ou plus<br>4. Variables d'un jour à l'autre<br>8. REFUS<br>9. NSP |  |



| 2D      |                                                                 | |--|--| | PERIODE | Habituellement, votre journée de travail est-elle morcelée en 2 | | HOR2    | périodes séparées par 3 heures ou plus ?                        | | Car 1   | 1. Oui                                                          | |         | 2. Non                                                          | |         | 8. REFUS                                                        | |         | 9. NSP                                                          |

<!-- p. 53 -->

| 2D             | Si les horaires de travail ne sont pas alternants (HORVAR (HOR1) # 2 ou 3) | |--|--| | DEBUTH<br>HOR3 | Habituellement, à quelle heure commencez-vous votre travail ?<br>Heure…    | | Num - 8        | De 0 à 23                                                                  |

## DEBUTH\_DRAP Pour qualifier la non réponse voir DEBUTH\_DRAP

Il s'agit des horaires réels, y compris les heures supplémentaires, les déplacements professionnels, le travail en dehors du lieu de travail (déplacements domicile-travail exclus). Si la personne a des horaires très variables ou alternants, elle doit essayer de décrire les horaires habituels, ceux qui reviennent le plus fréquemment. Si elle n'y parvient pas, décrire la dernière journée ou semaine « normalement » travaillée. CT 2013

| 2D                  |                                                                            | |--|--| | DEBUTH_DRAP<br>HOR3 | Variable "drapeau" de DEBUTH                                               | | Car 2               | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

## Variable drapeau



| 2D      | Si les horaires de travail ne sont pas alternants (HORVAR (HOR1) # 2 ou 3) | |--|--| | DEBUTM  | Habituellement, à quelle heure commencez-vous votre travail ?              | | HOR3    | Minute…                                                                    | | Num - 8 | De 0 à 59                                                                  |

### DEBUTM\_DRAP Pour qualifier la non réponse voir DEBUTM\_DRAP

Il s'agit des horaires réels, y compris les heures supplémentaires, les déplacements professionnels, le travail en dehors du lieu de travail (déplacements domicile-travail exclus). Si la personne a des horaires très variables ou alternants, elle doit essayer de décrire les horaires habituels, ceux qui reviennent le plus fréquemment. Si elle n'y parvient pas, décrire la dernière journée ou semaine « normalement » travaillée. CT 2013

| 2D          |                              | |--|--| | DEBUTM_DRAP | Variable "drapeau" de DEBUTM | | HOR3        |                              | | Car 2       | 1. Réponse                   | |             | 0. Sans objet                | |             | -1 . Ne sait pas             | |             | -2 . Refuse de répondre      |



| 2D           | Si les horaires de travail ne sont pas alternants (HORVAR (HOR1) # 2 ou 3)                             | |--|--| | FINH<br>HOR4 | A quelle heure terminez-vous votre travail / s'achève votre première<br>période de travail ?<br>Heure… | | Num - 8      | De 0 à 23                                                                                              |

### FINH\_DRAP Pour qualifier la non réponse voir FINH\_DRAP

Il s'agit des horaires réels, y compris les heures supplémentaires, les déplacements professionnels, le travail en dehors du lieu de travail (déplacements domicile-travail exclus). Si la personne a des horaires très variables ou alternants, elle doit essayer de décrire les horaires habituels, ceux qui reviennent le plus fréquemment. Si elle n'y parvient pas, décrire la dernière journée ou semaine « normalement » travaillée. CT 2013

<!-- p. 54 -->

| 2D        |                             | |--|--| | FINH_DRAP | Variable "drapeau" de FINHH | | HOR4      |                             | | Car 2     | 1. Réponse                  | |           | 0. Sans objet               | |           | -1 . Ne sait pas            | |           | -2 . Refuse de répondre     |

Variable drapeau



| 2D           | Si les horaires de travail ne sont pas alternants (HORVAR (HOR1) # 2 ou 3)                              | |--|--| | FINM<br>HOR4 | A quelle heure terminez-vous votre travail / s'achève votre première<br>période de travail ?<br>Minute… | | Num - 8      | De 0 à 59                                                                                               |

## FINM\_DRAP Pour qualifier la non réponse voir FINM\_DRAP

Il s'agit des horaires réels, y compris les heures supplémentaires, les déplacements professionnels, le travail en dehors du lieu de travail (déplacements domicile-travail exclus). Si la personne a des horaires très variables ou alternants, elle doit essayer de décrire les horaires habituels, ceux qui reviennent le plus fréquemment. Si elle n'y parvient pas, décrire la dernière journée ou semaine « normalement » travaillée. CT 2013

| 2D                |                                                                            | |--|--| | FINM_DRAP<br>HOR4 | Variable "drapeau" de FINM                                                 | | Car 2             | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau



| 2D      | Si les horaires de travail ne sont pas alternants (HORVAR (HOR1) # 2 ou 3) et deux<br>périodes de travail (PERIODE (HOR2) = 1) | |--|--| | DEBUT2H | A quelle heure commence la deuxième période de votre travail ?                                                                 | | HOR5    | Heure…                                                                                                                         | | Num - 8 | De 0 à 23                                                                                                                      |

## DEBUT2H\_DRAP Pour qualifier la non réponse voir DEBUT2H\_DRAP

Il s'agit des horaires réels, y compris les heures supplémentaires, les déplacements professionnels, le travail en dehors du lieu de travail (déplacements domicile-travail exclus). Si la personne a des horaires très variables ou alternants, elle doit essayer de décrire les horaires habituels, ceux qui reviennent le plus fréquemment. Si elle n'y parvient pas, décrire la dernière journée ou semaine « normalement » travaillée. CT 2013

| 2D                   |                                                                            | |--|--| | DEBUT2H_DRAP<br>HOR5 | Variable "drapeau" de DEBUT2H                                              | | Car 2                | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau

<!-- p. 55 -->

| 2D      | Si les horaires de travail ne sont pas alternants (HORVAR (HOR1) # 2 ou 3) et deux<br>périodes de travail (PERIODE (HOR2) = 1) | |--|--| | DEBUT2M | A quelle heure commence la deuxième période de votre travail ?                                                                 | | HOR5    | Minute…                                                                                                                        | | Num - 8 | De 0 à 59                                                                                                                      |

#### DEBUT2M\_DRAP Pour qualifier la non réponse voir DEBUT2M\_DRAP

Il s'agit des horaires réels, y compris les heures supplémentaires, les déplacements professionnels, le travail en dehors du lieu de travail (déplacements domicile-travail exclus). Si la personne a des horaires très variables ou alternants, elle doit essayer de décrire les horaires habituels, ceux qui reviennent le plus fréquemment. Si elle n'y parvient pas, décrire la dernière journée ou semaine « normalement » travaillée. CT 2013

| 2D           |                               | |--|--| | DEBUT2M_DRAP | Variable "drapeau" de DEBUT2M | | HOR5         |                               | | Car 2        | 1. Réponse                    | |              | 0. Sans objet                 | |              | -1 . Ne sait pas              | |              | -2 . Refuse de répondre       |



| 2D      | Si les horaires de travail ne sont pas alternants (HORVAR (HOR1) # 2 ou 3) et deux<br>périodes de travail (PERIODE (HOR2) = 1) | |--|--| | FIN2H   | A quelle heure s'achève la deuxième période de travail ?<br>Heure…                                                             | | HOR6    |                                                                                                                                | | Num - 8 | De 0 à 23                                                                                                                      |

#### FIN2H\_DRAP Pour qualifier la non réponse voir FIN2H\_DRAP

Il s'agit des horaires réels, y compris les heures supplémentaires, les déplacements professionnels, le travail en dehors du lieu de travail (déplacements domicile-travail exclus). Si la personne a des horaires très variables ou alternants, elle doit essayer de décrire les horaires habituels, ceux qui reviennent le plus fréquemment. Si elle n'y parvient pas, décrire la dernière journée ou semaine « normalement » travaillée. CT 2013

| 2D         |                             | |--|--| | FIN2H_DRAP | Variable "drapeau" de FIN2H | | HOR6       |                             | | Car 2      | 1. Réponse                  | |            | 0. Sans objet               | |            | -1 . Ne sait pas            | |            | -2 . Refuse de répondre     |



| 2D      | Si les horaires de travail ne sont pas alternants (HORVAR (HOR1) # 2 ou 3) et deux<br>périodes de travail (PERIODE (HOR2) = 1) | |--|--| | FIN2M   | A quelle heure s'achève la deuxième période de travail ?                                                                       | | HOR6    | Minute…                                                                                                                        | | Num - 8 | De 0 à 59                                                                                                                      |

### FIN2M\_DRAP Pour qualifier la non réponse voir FIN2M\_DRAP

Il s'agit des horaires réels, y compris les heures supplémentaires, les déplacements professionnels, le travail en dehors du lieu de travail (déplacements domicile-travail exclus). Si la personne a des horaires très variables ou alternants, elle doit essayer de décrire les horaires habituels, ceux qui reviennent le plus fréquemment. Si elle n'y parvient pas, décrire la dernière journée ou semaine « normalement » travaillée. CT 2013

<!-- p. 56 -->

| 2D                 |                                                                            | |--|--| | FIN2M_DRAP<br>HOR6 | Variable "drapeau" de FIN2M                                                | | Car 2              | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau



| 2D               | Si l'individu est salarié (STATUT = 1 à 7)                                                                                                                                                                                      | |--|--| | CONTROLE<br>HOR7 | A quel type de contrôle d'horaires êtes-vous soumis ?                                                                                                                                                                           | | Car 1            | 1. Aucun contrôle<br>2. Horloge pointeuse, badge<br>3. Signature, fiche horaire et assimilé<br>4. Contrôle par l'encadrement<br>5. Contrôle par d'autres personnes, par exemple vos collègues<br>6. Autre<br>8. REFUS<br>9. NSP |



| 2D              |                                                                                             | |--|--| | PTMATIN<br>HOR8 | Dans votre emploi principal, travaillez-vous entre cinq heures<br>et sept heures le matin ? | | Car 1           | 1. Habituellement<br>2. Occasionnellement<br>3. Jamais<br>8. REFUS<br>9. NSP                |



| 2D           |                                                                              | |--|--| | SOIR<br>HOR9 | Travaillez-vous entre vingt heures et minuit ?                               | | Car 1        | 1. Habituellement<br>2. Occasionnellement<br>3. Jamais<br>8. REFUS<br>9. NSP |



| 2D            |                                                                              | |--|--| | NUIT<br>HOR10 | Travaillez-vous la nuit, entre minuit et cinq heures du matin ?              | | Car 1         | 1. Habituellement<br>2. Occasionnellement<br>3. Jamais<br>8. REFUS<br>9. NSP |

<!-- p. 57 -->

| 2D              | Si travail la nuit (NUIT (HOR10 =1 ou 2)                                                                                             | |--|--| | NBNUIT<br>HOR11 | Combien de nuits par an travaillez-vous sur votre lieu de<br>travail ?                                                               | | Num - 8         | 0. Aucune<br>1. 1 à 3<br>2. 4 à 11<br>3. 12 à 23<br>4. 24 à 49<br>5. 50 à 99<br>6. 100 à 199<br>7. 200 ou plus<br>8. REFUS<br>9. NSP |

Cela exclut le travail emporté à la maison et effectué la nuit, si c'est uniquement cela coder 0. La réponse peut être approximative. On peut estimer qu'une personne qui travaille toutes les nuits travaille 235 nuits par an. CT 2013

| 2D             |                              | |--|--| | NBNUIT_DRAP    | Variable "drapeau" de NBNUIT | | HOR12<br>Car 2 | 1. Réponse                   | |                | 0. Sans objet                | |                | -1 . Ne sait pas             | |                | -2 . Refuse de répondre      |

Variable drapeau



| 2D    |                                                             | |--|--| | HSUP  | Vous arrive-t-il de travailler au-delà de l'horaire prévu ? | | DEP1  |                                                             | | Car 1 | 1. Tous les jours                                           | |       | 2. Souvent                                                  | |       | 3. Parfois                                                  | |       | 4. Jamais                                                   |



| 2D               | Si l'individu fait des heures supplémentaire (HSUP (DEP1 =1, 2 ou 3) et salarié (STATUT<br>(PRO2) # 8 à 10)           | |--|--| | HSUPCOMP<br>DEP2 | Ces heures au-delà de l'horaire prévu font-elles l'objet d'une<br>compensation particulière, en salaire ou en repos ? | | Car 1            | 1. Oui, toutes<br>2. Oui, une partie<br>3. Non<br>8. REFUS<br>9. NSP                                                  |



| 2D              |                                                                                                                                                                                       | |--|--| | JOINDRE<br>DEP3 | Au cours des 12 derniers mois, avez-vous été joint par votre<br>établissement, vos collègues ou vos supérieurs, en dehors de<br>vos horaires de travail pour les besoins du travail ? | | Car 1           | 1. Oui<br>2. Non<br>3. Sans objet : travaille seul<br>8. REFUS<br>9. NSP                                                                                                              |

Y compris lorsque vous étiez d'astreinte

<!-- p. 58 -->

| 2D              | Si joint par son établissement (JOINDRE (DEP6) = 1)                                                  | |--|--| | COMBIEN<br>DEP4 | Approximativement combien de fois avez-vous ainsi été<br>contacté au cours des douze derniers mois ? | | Num - 8         | 1. 1 à 5 fois<br>2. 6 à 20 fois<br>3. Plus de 20 fois<br>8. REFUS<br>9. NSP                          |

COMBIEN\_DRAP Pour qualifier la non réponse voir COMBIEN\_DRAP

Y compris lorsque vous étiez d'astreinte. Réponse approximative.Si cela arrive environ une fois par semaine, on peut l'estimer à 47 fois par an. CT 2013

| 2D                   |                                                                            | |--|--| | COMBIEN_DRAP<br>DEP4 | Variable "drapeau" de COMBIEN                                              | | Car 2                | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau



| 2D             |                                                                                                                                                               | |--|--| | MAISON<br>DEP7 | Vous arrive-t-il d'emporter du travail chez vous ?                                                                                                            | | Car 1          | 1. Tous les jours ou presque<br>2. Souvent<br>3. Parfois<br>4. Jamais<br>5. Sans objet (travail à domicile, impossibilité technique, …)<br>8. REFUS<br>9. NSP |



| 2D               | Si emporte du travail à la maison (MAISON (DEP7) = 1, 2 ou 3)   | |--|--| | TPMAISON<br>DEP8 | Combien d'heures y passez-vous en moyenne dans une<br>semaine ? | | Num - 8          | Réponse                                                         |

TPMAISON\_DRAP Pour qualifier la non réponse voir TPMAISON\_DRAP CT 2013

| 2D                    |                                                                            | |--|--| | TPMAISON_DRAP<br>DEP8 | Variable "drapeau" de TPMAISON                                             | | Car 2                 | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau

<!-- p. 59 -->

| 2D            | Si l'individu est salarié (STATUT(PRO2) = 1 à 7) et a un contrat de travail (TYPEMPLOI<br>(PRO15) # 8 et STATUT (PRO2) # 4,6 et 7) | |--|--| | CONGE<br>CON1 | A combien de congés annuels avez-vous droit de manière<br>régulière, y compris RTT et congés d'ancienneté ?                        | | Num - 8       | Réponse                                                                                                                            |

CONGE\_DRAP Pour qualifier la non réponse voir CONGE\_DRAP

Ceux qui n'ont pas de congés (enquêteurs,…), mettre 0 Ceux qui ont un contrat de moins d'un an, indiquez le nombre de jours de congés sur la durée totale du contrat. CT 2013

| 2D                 |                                                                            | |--|--| | CONGE_DRAP<br>CON1 | Variable "drapeau" de CONGE                                                | | Car 2              | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau



| 2D             | Si l'individu est salarié et a des congés (Si STATUT(PRO2)= 1 à 7 et CONGE (CON1) #<br>0, 9. NSP ou 8. REFUS) | |--|--| | CONGEU<br>CON1 | Durée exprimée en …                                                                                           | | Car 1          | 1. jours<br>2. semaines                                                                                       |



| 2D              |                                                                                                                                                                                                      | |--|--| | CONGETR<br>CON1 | Durée des congés des salariés, en tranches                                                                                                                                                           | | Car 1           | 1. Non réponse<br>2. Aucun congé<br>3. Moins de 5 semaines, 25 jours<br>4. 5 semaines, 25 jours<br>5. 6 ou 7 semaines (26 à 35 jours)<br>6. 8 ou 9 semaines (36 à 45 jours)<br>7. Plus de 9 semaines |

CALCULEE : à partir des réponses à CONGE et CONGEU



| 2D      | Si l'individu est non salarié (STATUT(PRO2) = 8 à 10)    | |--|--| | CONGENS | Habituellement, dans une année, prenez-vous des congés ? | | CON1B   |                                                          | | Car 1   | 1. Oui, régulièrement<br>2. Oui, de temps en temps       | |         | 3. Non, aucun                                            | |         | 8. REFUS                                                 | |         | 9. NSP                                                   |



| 2D               | Si l'individu est non salarié et prend des congés (STATUT(PRO2) = 8 à 10 et<br>CONGENS(CON1B) # 3, NSP, refus) | |--|--| | CONGNSN<br>CON1C | Combiens avez-vous pris de jours de congés ces douze<br>derniers mois ?                                        | | Num - 8          | Réponse                                                                                                        |

CONGNSN\_DRAP Pour qualifier la non réponse voir CONGNSN\_DRAP



<!-- p. 60 -->

| 2D           |                               | |--|--| | CONGNSN_DRAP | Variable "drapeau" de CONGNSN | | CON1C        |                               | | Car 2        | 1. Réponse                    | |              | 0. Sans objet                 | |              | -1 . Ne sait pas              | |              | -2 . Refuse de répondre       |

Variable drapeau



| 2D               | Si l'individu est non salarié et prend des congés (Si STATUT(PRO2)=8 à 10 et<br>CONGNSN (CON1C) # 0, 9. NSP ou 8. REFUS) | |--|--| | CONGNSU<br>CON1C | Durée exprimée en …                                                                                                      | | Car 1            | 1. jours<br>2. semaines                                                                                                  |



| 2D                 |                                                                                                                                                                                                      | |--|--| | CONGETRNS<br>CON1C | Durée des congés des non salariés, en tranches                                                                                                                                                       | | Car 1              | 1. Non réponse<br>2. Aucun congé<br>3. Moins de 5 semaines, 25 jours<br>4. 5 semaines, 25 jours<br>5. 6 ou 7 semaines (26 à 35 jours)<br>6. 8 ou 9 semaines (36 à 45 jours)<br>7. Plus de 9 semaines |

CALCULEE : à partir des réponses à CONGE et CONGEU



| 2D              | Si l'individu a un contrat de travail ( TYPEMPLOI (PRO15) # 8 et STATUT (PRO2) # 4,6<br>et 7)    | |--|--| | ARETMAL<br>CON2 | Au cours des douze derniers mois, combien avez-vous eu<br>d'arrêts maladie (hors maternité) ?    | | Car 1           | 0. Pas d'arrêt<br>1. Un arrêt<br>2. Deux arrêts<br>3. Trois arrêts ou plus<br>8. REFUS<br>9. NSP |



| 2D            | Si arrêts pour maladie (ARETMAL = 1, 2 ou 3)                       | |--|--| | ATMAL<br>CON3 | A combien de jours d'absence correspondent ces arrêts<br>maladie ? | | Num - 8       | Réponse                                                            |

ATMAL\_DRAP Pour qualifier la non réponse voir ATMAL\_DRAP CT 2013

| 2D         |                             | |--|--| | ATMAL_DRAP | Variable "drapeau" de ATMAL | | CON3       |                             | | Car 2      | 1. Réponse                  | |            | 0. Sans objet               | |            | -1 . Ne sait pas            | |            | -2 . Refuse de répondre     |

Variable drapeau

<!-- p. 61 -->

| 2D               | Si l'individu a des congés ( Si CONGE (CON1) # 0, NSP ou REFUS)                                                                                                                                             | |--|--| | PRISCONG<br>CON4 | Au cours de l'année écoulée, avez-vous pris tous les congés<br>auxquels vous aviez droit ?                                                                                                                  | | Car 1            | 1. Oui<br>2. Non, mais j'ai pu reporter tous les jours non pris (report sur l'année suivante, compte<br>épargne temps)<br>3. Non, et j'ai perdu les jours non pris (certains ou tous)<br>8. REFUS<br>9. NSP |

| 2D             |                                                                                                               | |--|--| | URGFAM<br>CON5 | En cas d'imprévu personnel ou familial, pouvez-vous vous<br>absenter de votre travail, même quelques heures ? | | Car 1          | 1. Oui, c'est facile<br>2. Oui, mais ce n'est pas facile<br>3. Non, c'est impossible<br>8. REFUS<br>9. NSP    |



| 2D            |                                                                                                                                  | |--|--| | CVFVP<br>CON6 | En général, vos horaires de travail s'accordent-ils avec vos<br>engagements sociaux et familiaux en dehors de votre travail<br>? | | Car 1         | 1. Très bien<br>2. Bien<br>3. Pas très bien<br>4. Pas bien du tout<br>8. REFUS<br>9. NSP                                         |



| 2D             |                                                                                                                    | |--|--| | AIDHID<br>CON7 | Vous occupez-vous régulièrement d'une personne malade,<br>âgée ou handicapée, qu'elle soit ou non de votre ménage, | |                | sans être payé pour cela ?                                                                                         | | Car 1          | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                             |

Les activités professionnelles ou dans le cadre d'une association ne doivent pas être prises en compte. CT 2013

<!-- p. 62 -->

## 3. Contraintes physiques, prévention et accidents

## A - Nuisances, contraintes physiques et risques

| 4A      |                                                   |  | |--|--|--| | CWDEBOU | L'exécution de votre travail vous impose-t-elle … |  | | PPA1    | a) … de rester longtemps debout ?                 |  | | Car 1   | 1. Oui<br>2. Non                                  |  | |         | 8. REFUS<br>9. NSP                                |  |



| 4A      |                                                            | |--|--| | CWPOSTU | L'exécution de votre travail vous impose-t-elle …          | | PPA2    | b) … de rester longtemps dans une autre posture pénible ou | |         | fatigante à la longue ?                                    | | Car 1   | 1. Oui                                                     | |         | 2. Non                                                     | |         | 8. REFUS                                                   | |         | 9. NSP                                                     |



| 4A              |                                                                                                                       | |--|--| | CWDEPLA<br>PPA3 | L'exécution de votre travail vous impose-t-elle …<br>c) … d'effectuer des déplacements à pied longs ou fréquents<br>? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                |



| 4A      |                                                   | |--|--| | CWLOURD | L'exécution de votre travail vous impose-t-elle … | | PPA4    | d) … de porter ou déplacer des charges lourdes ?  | | Car 1   | 1. Oui                                            | |         | 2. Non                                            | |         | 8. REFUS                                          | |         | 9. NSP                                            | |         |                                                   |



| 4A    |                                                           | |--|--| | CWMVT | L'exécution de votre travail vous impose-t-elle …         | | PPA5  | e) … d'effectuer des mouvements douloureux ou fatigants ? | | Car 1 | 1. Oui                                                    | |       | 2. Non                                                    | |       | 8. REFUS                                                  | |       | 9. NSP                                                    |

<!-- p. 63 -->

| 4A    |                                                   | |--|--| | CWVIB | L'exécution de votre travail vous impose-t-elle … | | PPA6  | f) … de subir des secousses ou vibrations ?       | | Car 1 | 1. Oui                                            | |       | 2. Non                                            | |       | 8. REFUS                                          | |       | 9. NSP                                            |

| L'exécution de votre travail vous impose-t-elle … | |--| | g) … de ne pas quitter votre travail des yeux ?   | | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP            | |                                                   |



| 4A             |                                                                                                                                                 | |--|--| | CWLETR<br>PPA8 | L'exécution de votre travail vous impose-t-elle …<br>h) … de lire des lettres ou des chiffres de petites tailles, mal<br>imprimés, mal écrits ? | | Car 1          | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                          |



| 4A      |                                                            | |--|--| | CWMINUS | L'exécution de votre travail vous impose-t-elle …          | | PPA9    | i) … d'examiner des objets très petits, des détails fins ? | | Car 1   | 1. Oui                                                     | |         | 2. Non                                                     | |         | 8. REFUS                                                   | |         | 9. NSP                                                     |



| 4A     |                                                                                                             | |--|--| | CWVISO | L'exécution de votre travail vous impose-t-elle …                                                           | | PPA10  | j) … de faire attention à des signaux visuels ou sonores brefs,<br>imprévisibles ou difficiles à détecter ? | | Car 1  | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                      |



| 4A              |                                                                                                        | |--|--| | HYGSAL<br>PPA11 | Votre travail ou votre lieu de travail, présente-t-il les inconvénients<br>suivants …<br>a) … saleté ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                 |

<!-- p. 64 -->

| 4A               |                                                                                                          | |--|--| | HYGCHUM<br>PPA12 | Votre travail ou votre lieu de travail, présente-t-il les inconvénients<br>suivants …<br>b) … humidité ? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                   |

| 4A               |                                                                                                                | |--|--| | HYGCOUR<br>PPA13 | Votre travail ou votre lieu de travail, présente-t-il les inconvénients<br>suivants …<br>c) … courants d'air ? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                         |



| 4A                |                                                                                                                  | |--|--| | HYGODEUR<br>PPA14 | Votre travail ou votre lieu de travail, présente-t-il les inconvénients<br>suivants …<br>d) … mauvaises odeurs ? | | Car 1             | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                           |



| 4A                |                                                                                                                    | |--|--| | HYGCHAUD<br>PPA15 | Votre travail ou votre lieu de travail, présente-t-il les inconvénients<br>suivants …<br>e) … température élevée ? | | Car 1             | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                             |



| 4A                |                                                                                       | |--|--| | HYGFROID<br>PPA16 | Votre travail ou votre lieu de travail, présente-t-il les inconvénients<br>suivants … | |                   | f) … température basse ?                                                              | | Car 1             | 1. Oui                                                                                | |                   | 2. Non                                                                                | |                   | 8. REFUS                                                                              | |                   | 9. NSP                                                                                |



| 4A               |                                                                                                                                               | |--|--| | HYGSANI<br>PPA17 | Votre travail ou votre lieu de travail, présente-t-il les inconvénients<br>suivants …<br>g) … absence ou mauvais état des locaux sanitaires ? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                        |

<!-- p. 65 -->

| 4A              |                                                                                                                                | |--|--| | HYGVUE<br>PPA18 | Votre travail ou votre lieu de travail, présente-t-il les inconvénients<br>suivants …<br>h) … absence de vue sur l'extérieur ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                         |

| 4A              |                                                                                    | |--|--| | OPENSP<br>PPA19 | Travaillez-vous dans un bureau sans cloison ou un plateau<br>ouvert (open space) ? | | Car 1           | 1. Oui<br>2. Non<br>3. Sans objet<br>8. REFUS<br>9. NSP                            |



| 4A                |                                                                                                                                             | |--|--| | CONDUITE<br>PPA20 | Utilisez-vous un véhicule dans le cadre de votre travail ou<br>pour vos besoins professionnels, en dehors des trajets<br>domicile-travail ? | | Car 1             | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                      |



| 4A      | Si utilisation d'un véhicule (CONDUITE (PPA20) = 1) | |--|--| | NBRKM   | Combien de km en moyenne parcourez-vous … ?         | | PPA21   |                                                     | | Num - 8 | De 1 à 99997                                        |

NBRKM\_DRAP Pour qualifier la non réponse voir NBRKM\_DRAP CT 2013

| 4A                  |                                                                            | |--|--| | NBRKM_DRAP<br>PPA21 | Variable "drapeau" de NBRKM                                                | | Car 2               | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau CT 2013

| 4A     |                                             | |--|--| | NBRKMU | Combien de km en moyenne parcourez-vous … ? | | PPA21  | par …                                       | | Car 1  | 1. semaine                                  | |        | 2. mois                                     | |        | 3. année                                    |

<!-- p. 66 -->

| 4A      |                                             | |--|--| | KMANNU  | Nombre de kilomètres parcourus annuellement | | PPA21   |                                             | | Num - 8 | De 1 à 99997                                |

CALCULEE : vaut NBRKM si NBRKMU=3, vaut NBRKMx11 si NBRKMU=2, vaut NBRKMx47 si NBRKMU=1 CT 2013

| 4A                   |                                                                            | |--|--| | KMANNU_DRAP<br>PPA21 | Variable "drapeau" de KMANNU                                               | | Num - 8              | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau



| 4A       |                                                     | |--|--| | SECFUPOU | A votre emplacement de travail, êtes-vous amené à … | | PPA22    | a) … respirer des fumées ou des poussières ?        | | Car 1    | 1. Oui                                              | |          | 2. Non                                              | |          | 8. REFUS                                            | |          | 9. NSP                                              |



| 4A       |                                                     | |--|--| | SECTOXNO | A votre emplacement de travail, êtes-vous amené à … | | PPA23    | b) … être en contact avec des produits dangereux ?  | | Car 1    | 1. Oui<br>2. Non                                    | |          | 8. REFUS                                            | |          | 9. NSP                                              |



| A votre emplacement de travail, êtes-vous amené à … | |--| | c) … être exposé à des risques infectieux ?         | | 1. Oui                                              | | 2. Non                                              | | 8. REFUS                                            | | 9. NSP                                              | |                                                     |



| 4A       |                                                     | |--|--| | SECACCID | A votre emplacement de travail, êtes-vous amené à … | | PPA25    | d) … risquer d'être blessé ou accidenté ?           | | Car 1    | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP              |

<!-- p. 67 -->

| 4A               |                                                                                                                              | |--|--| | SECROUT<br>PPA26 | A votre emplacement de travail, êtes-vous amené à …<br>e) … risquer des accidents de la circulation au cours du travail<br>? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                       |

| 4A               |                                                                                                                                                                  | |--|--| | ENTENDR<br>PPA27 | Quand vous travaillez, si une personne, placée à 2 ou 3 mètres<br>de vous, vous adresse la parole …                                                              | | Car 1            | 1. vous l'entendez, si elle parle normalement<br>2. vous l'entendez, à condition qu'elle élève la voix<br>3. vous ne pouvez pas l'entendre<br>8. REFUS<br>9. NSP |



| 4A           | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                                                   | |--|--| | EPI<br>PPA28 | Votre employeur met-il à votre disposition des équipements<br>individuels de protection comme des gants, des lunettes, des<br>chaussures de sécurité, un harnais, … ? | | Car 1        | 1. oui, de façon suffisante<br>2. oui, mais insuffisamment<br>3. non, mais vous en auriez besoin<br>4. non et vous n'en avez pas besoin<br>8. REFUS<br>9. NSP         |



| 4A               | Si équipements mis a disposition (EPI (PPA28) =1 ou 2)                               | |--|--| | EPIUTIL<br>PPA29 | Les utilisez-vous ?                                                                  | | Car 1            | 1. Oui, tous<br>2. Oui mais pas tous ou pas toujours<br>3. Non<br>8. REFUS<br>9. NSP |



## B - Prévention

| 4B           | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                                                         | |--|--| | RISK<br>PRE1 | Au<br>cours<br>des<br>12<br>derniers<br>mois,<br>avez-vous<br>reçu<br>une<br>information sur les risques que votre travail fait courir à votre<br>santé ou votre sécurité ? | | Car 1        | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                                                      |

<!-- p. 68 -->

| 3B               | Si information reçue (RISK (PRE1) = 1)                                                                                     | |--|--| | RISKINF1<br>PRE2 | L'information provenait …<br>a) … de votre entreprise / administration (supérieurs, service<br>spécialisé ou consultant) ? | | Car 1            | 1. Oui<br>2. Non<br>3. Sans objet (pas de supérieur ou de service spécialisé)<br>8. REFUS<br>9. NSP                        |

| 3B       | Si information reçue (RISK (PRE1) = 1)                                                                        | |--|--| | RISKINF2 | L'information provenait …                                                                                     | | PRE3     | b) … des représentants du personnel (délégué du personnel,<br>délégué syndical, membre du CE ou du CHSCT …) ? | | Car 1    | 1. Oui<br>2. Non<br>3. Sans objet (pas de représentants du personnel)<br>8. REFUS<br>9. NSP                   |



| 3B       | Si information reçue (RISK (PRE1) = 1)        | |--|--| | RISKINF3 | L'information provenait …                     | | PRE4     | c) … du médecin du travail ou de prévention ? | | Car 1    | 1. Oui                                        | |          | 2. Non                                        | |          | 8. REFUS                                      | |          | 9. NSP                                        |



| 3B       | Si information reçue (RISK (PRE1) = 1) | |--|--| | RISKINF4 | L'information provenait …              | | PRE5     | d) … de vos collègues ?                | | Car 1    | 1. Oui<br>2. Non                       | |          | 3. Sans objet (pas de collègues)       | |          | 8. REFUS                               | |          | 9. NSP                                 |



| 3B       | Si information reçue (RISK (PRE1) = 1) | |--|--| | RISKINF5 | L'information provenait …              | | PRE6     | d) … autre ?                           | | Car 1    | 1. Oui                                 | |          | 2. Non                                 |



| 4B              | Si l'individu est salarié, mais pas de particuliers ( STATUT (PRO2) # 7 à 10)                                                            | |--|--| | FORMSEC<br>PRE7 | Au cours des 12 derniers mois, avez-vous bénéficié d'une<br>formation à la sécurité dispensée par votre entreprise /<br>administration ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                   |

<!-- p. 69 -->

| 4B              | Si l'individu est salarié, mais pas de particuliers ( STATUT (PRO2) # 7 à 10)                                                                                                                              | |--|--| | DOCEVAL<br>PRE8 | Au<br>cours<br>des<br>douze<br>derniers<br>mois,<br>avez-vous<br>eu<br>connaissance d'un document rédigé par la direction et<br>décrivant les risques liés au travail dans votre établissement<br>(DUER) ? | | Car 2           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                                                                                     |

DUER : document unique d'évaluation des risques CT 2013

| 4B              | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                         | |--|--| | MEDECIN<br>PRE9 | A quand remonte votre dernière visite médicale avec un<br>médecin du travail ou de prévention ?                                             | | Car 1           | 1. Moins d'un an<br>2. De 1 à 2 ans<br>3. De plus de 2 ans à 5 ans<br>4. Plus de 5 ans<br>5. Vous n'en avez jamais eu<br>8. REFUS<br>9. NSP |



| 3B                | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                             | |--|--| | VISITUNI<br>PRE10 | Au cours des 12 derniers mois, le médecin du travail ou de<br>prévention a t-il visité votre unité de travail ? | | Car 2             | 1. Oui<br>2. Non<br>3. Sans objet (pas d'unité de travail)<br>8. REFUS<br>9. NSP                                |



| 3B                | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                                                                   | |--|--| | SECURITE<br>PRE11 | Pour préserver votre sécurité ou votre santé dans votre<br>travail, disposez-vous de consignes ou d'instructions écrites,<br>en dehors des consignes d'évacuation en cas d'incendie ? | | Car 1             | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                                                                |



| 3B              | Si consignes de sécurité (SECURITE (PRE13) = 1)                         | |--|--| | APPLIQ<br>PRE12 | Pouvez-vous les appliquer ?                                             | | Car 1           | 1. Oui, totalement<br>2. Oui, en partie<br>3. Non<br>8. REFUS<br>9. NSP |

<!-- p. 70 -->

## C - Accidents du travail

| 4C             |                                                                                                                                                                | |--|--| | ACCIDT<br>ACC1 | Dans les douze derniers mois, au cours de votre travail, avez<br>vous eu un ou plusieurs accidents, même bénins, qui vous<br>ont obligé à vous faire soigner ? | | Car 1          | 0. Pas d'accident<br>1. Un accident<br>2. Deux accidents<br>3. Trois accidents et plus<br>8. REFUS<br>9. NSP                                                   |

En dehors des accidents survenus lors des trajets domicile/travail, et même s'il n'y a pas eu de blessure grave CT 2013

| 4C               | Si un accident (ACCIDT (ACC1) = 1)                     | |--|--| | ACTUACC1<br>ACC2 | Cet accident est-il survenu dans votre emploi actuel ? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                 |



| 4C               | Si plusieurs accidents (ACCIDT (ACC1) = 2 ou 3)            | |--|--| | ACTUACC2<br>ACC3 | Ces accidents sont-ils survenus dans votre emploi actuel ? | | Car 1            | 1. Oui, tous<br>2. Oui, certains                           | |                  | 3. Non<br>8. REFUS<br>9. NSP                               |



| 4C            | Si au moins un accident (ACCIDT (ACC1) = 1, 2 ou 3)                                                                                        | |--|--| | ARRET<br>ACC4 | Nous allons maintenant nous intéresser au dernier accident<br>survenu. Avez-vous dû interrompre votre travail à cause de<br>cet accident ? | | Car 1         | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                     |

Au moins une journée. CT 2013

| 4C             | Si interruption à cause de l'accident (ARRET (ACC5) = 1)   | |--|--| | NBJARR<br>ACC5 | Combien de jours, avez-vous dû interrompre votre travail ? | | Num - 8        | Réponse                                                    |

NBJARR\_DRAP Pour qualifier la non réponse voir NBJARR\_DRAP CT 2013

<!-- p. 71 -->

| 4C                  |                                                                            | |--|--| | NBJARR_DRAP<br>ACC5 | Variable "drapeau" de NBJARR                                               | | Car 2               | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau



| 3C              | Si au moins un accident (ACCIDT (ACC1) = 1, 2 ou 3) et l'individu est salarié (STATUT<br>(PRO2) = 1 à 7)    | |--|--| | INDEMNI<br>ACC6 | Cet accident a t-il été reconnu et indemnisé par la Sécurité<br>Sociale au titre des accidents du travail ? | | Car 2           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                      |

Remboursement intégral des soins, prise en charge dès le 1er jour d'arrêt CT 2013

<!-- p. 72 -->

# 4. Organisation du travail

## A - Rythme de travail

| 5A    |                                                                  | |--|--| | RWDEP | Votre rythme de travail vous est-il imposé par …                 | | EXI1  | a) … le déplacement automatique d'un produit ou d'une pièce<br>? | | Car 1 | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                           |



| 5A    |                                                  | |--|--| | RWCAD | Votre rythme de travail vous est-il imposé par … | | EXI2  | b) … la cadence automatique d'une machine ?      | | Car 1 | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP           |



| 5A     |                                                  | |--|--| | RWTECH | Votre rythme de travail vous est-il imposé par … | | EXI3   | c) … d'autres contraintes techniques ?           | | Car 1  | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP           |



| 5A              |                                                                                                                                        | |--|--| | RWCOLEG<br>EXI4 | Votre rythme de travail vous est-il imposé par …<br>d) … la dépendance immédiate vis-à-vis du travail d'un ou<br>plusieurs collègues ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                 |



| 5A              |                                                                                                                                       | |--|--| | RWNORMH<br>EXI5 | Votre rythme de travail vous est-il imposé par …<br>e) … des normes de production ou des délais à respecter en<br>une heure au plus ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                |

<!-- p. 73 -->

| 5A              | Si pas de délais ou normes à respecter en une heure au plus (RWNORMH (EXI5) # 1)                                                        | |--|--| | RWNORMJ<br>EXI6 | Votre rythme de travail vous est-il imposé par …<br>f) … des normes de production ou des délais à respecter en<br>une journée au plus ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                  |

| 5A            |                                                                                                                                          | |--|--| | RWDEM<br>EXI7 | Votre rythme de travail vous est-il imposé par …<br>g) … une demande extérieure (clients, public) obligeant à une<br>réponse immédiate ? | | Car 1         | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                   |



| 5A               | Si pas de demande obligeant à une réponse immédiate (RWDEM (EXI7) #1)                                                                          | |--|--| | RWDEMAND<br>EXI8 | Votre rythme de travail vous est-il imposé par …<br>h) … une demande extérieure (clients, public) n'obligeant pas<br>à une réponse immédiate ? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                         |



| 5A     | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                    | |--|--| | RWSURV | Votre rythme de travail vous est-il imposé par …                                                       | | EXI9   | i) … les contrôles ou surveillances permanents (ou au moins<br>quotidiens) exercés par la hiérarchie ? | | Car 1  | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                 |



| 5A     |                                                  |  | |--|--|--| | RWINFO | Votre rythme de travail vous est-il imposé par … |  | | EXI10  | j) … un contrôle ou un suivi informatisé ?       |  | | Car 1  | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP           |  |



| 5A              |                                                                            | |--|--| | DEPECH<br>EXI12 | Êtes-vous obligé de vous dépêcher ?                                        | | Car 1           | 1. Toujours<br>2. Souvent<br>3. Parfois<br>4. Jamais<br>8. REFUS<br>9. NSP |

<!-- p. 74 -->

| 5A              |                                                                                                                             | |--|--| | DEBORD<br>EXI13 | Devez-vous fréquemment interrompre une tâche que vous<br>êtes en train de faire pour en effectuer une autre non prévue<br>? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                      |

| 5A       |                                                      | |--|--| | OBJECTIF | Devez-vous atteindre des objectifs chiffrés précis ? | | EXI15    |                                                      | | Car 1    | 1. Oui                                               | |          | 2. Non                                               | |          | 8. REFUS                                             | |          | 9. NSP                                               |



| 5A              |                                                                                          | |--|--| | DELAIS<br>EXI18 | Pour faire votre travail, avez-vous la possibilité de faire varier<br>les délais fixés ? | | Car 1           | 1. Oui<br>2. Non<br>3. Sans objet (pas de délais)<br>8. REFUS<br>9. NSP                  |



| 5A     |                                              | |--|--| | PUBLIC | Êtes-vous en contact direct avec le public ? | | EXI19  |                                              | | Car 1  | 1. Oui<br>2. Non                             | |        | 8. REFUS<br>9. NSP                           |

Usagers, patients, élèves, voyageurs, clients, fournisseurs, ... CT 2013

| 5A      | Si contact avec le public (PUBLIC (EXI19) = 1)                             | |--|--| | PUBLIC1 | Êtes-vous en contact direct avec le public ?                               | | EXI20   | 1 - de vive voix en face à face ?                                          | | Car 1   | 1. Toujours<br>2. Souvent<br>3. Parfois<br>4. Jamais<br>8. REFUS<br>9. NSP |

<!-- p. 75 -->

| 5A               | Si contact avec le public (PUBLIC (EXI19) = 1)                             | |--|--| | PUBLIC2<br>EXI21 | Êtes-vous en contact direct avec le public ?<br>2 - par téléphone ?        | | Car 1            | 1. Toujours<br>2. Souvent<br>3. Parfois<br>4. Jamais<br>8. REFUS<br>9. NSP |

| 5A               | Si contact avec le public (PUBLIC (EXI19) = 1)                                                                                                | |--|--| | PUBLIC3<br>EXI22 | Êtes-vous en contact direct avec le public ?<br>3<br>-<br>par<br>voie<br>électronique<br>(mail,<br>forum,<br>chats,<br>réseaux<br>sociaux…) ? | | Car 1            | 1. Toujours<br>2. Souvent<br>3. Parfois<br>4. Jamais<br>8. REFUS<br>9. NSP                                                                    |



# B - Autonomie, marge de manœuvre

| 5B           | Si l'individu est salarié ( STATUT (PRO2) =1 à 7)                                                                                                                                                | |--|--| | POLY<br>AUT1 | Occupez-vous différents postes ?                                                                                                                                                                 | | Car 1        | 1. OUI, vous faites une ROTATION REGULIERE entre plusieurs postes<br>2. OUI, vous changez de poste EN FONCTION DES BESOINS de l'entreprise /<br>l'administration<br>3. Non<br>8. REFUS<br>9. NSP |

On ne parle pas ici des postes complexes "par nature", ni des "postes" au sens de travail en "2x8", "3x8", … CT 2013

| 5B               | Si l'individu occupe plusieurs postes (POLY (AUT1 = 1 ))                                                                               | |--|--| | POLYQUAL<br>AUT2 | Du<br>point<br>de<br>vue<br>de<br>l'intérêt<br>de<br>votre<br>travail,<br>de<br>vos<br>compétences ou de votre qualification, est-ce … | | Car 1            | 1. positif<br>2. négatif<br>3. sans effet<br>8. REFUS<br>9. NSP                                                                        |



| 5B              |                                                                                              | |--|--| | INTERUP<br>AUT3 | Pouvez-vous<br>interrompre<br>momentanément<br>votre<br>travail<br>quand vous le souhaitez ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                       |

<!-- p. 76 -->

| 5B              | Si interruptions possibles (INTERUP (AUT4) = 1)                          | |--|--| | REMPLAS<br>AUT4 | Cette interruption nécessite-t-elle que vous vous fassiez<br>remplacer ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                   |

| 5B              | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                                                     | |--|--| | COMMENT<br>AUT5 | Les indications données par vos supérieurs hiérarchiques<br>vous disent ce qu'il faut faire. En général, est-ce que                                                     | | Car 1           | 1. ils vous disent aussi comment il faut faire<br>2. ils indiquent plutôt l'objectif du travail et vous choisissez vous-même la façon d'y arriver<br>8. REFUS<br>9. NSP |

CT

| 5B            | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                                                                                                                 | |--|--| | STARK<br>AUT6 | Vous<br>recevez<br>des<br>ordres,<br>des<br>consignes,<br>des<br>modes<br>d'emploi. Pour faire votre travail correctement, est-ce que …                                                                                             | | Car 1         | 1. vous appliquez strictement les consignes<br>2. dans certains cas, vous faites autrement<br>3. la plupart du temps vous faites autrement<br>4. sans objet (pas d'ordres, de consignes ou de modes d'emploi)<br>8. REFUS<br>9. NSP |

CT

| 5B       |                                                                                                                                                                                                                                                                                        | |--|--| | INCIDENT | Quand au cours de votre travail, il se produit quelque chose                                                                                                                                                                                                                           | | AUT7     | d'anormal, est-ce que                                                                                                                                                                                                                                                                  | | Car 1    | 1. la plupart du temps, vous réglez personnellement l'incident<br>2. vous réglez personnellement l'incident mais dans des cas bien précis, prévus d'avance<br>3. vous faites généralement appel à d'autres (un supérieur, un collègue, un service<br>spécialisé)<br>8. REFUS<br>9. NSP |

CT

| 5B               |                                                                                                                         | |--|--| | PROCEDUR<br>AUT8 | Devez-vous<br>suivre<br>des<br>procédures<br>de<br>qualité<br>strictes<br>(certification ISO, accréditation, EAQF, …) ? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                  |

CT

| 5B     |                                                                | |--|--| | REPETE | Votre travail consiste-t-il à répéter continuellement une même | | AUT9   | série de gestes ou d'opérations ?                              | | Car 1  | 1. Oui                                                         | |        | 2. Non                                                         | |        | 8. REFUS                                                       | |        | 9. NSP                                                         |

<!-- p. 77 -->

| 5B    | Si répétition d'une série (REPETE (AUT10) = 1) | |--|--| | CYCLE | Chaque série dure-t-elle moins d'une minute ?  | | AUT10 |                                                | | Car 1 | 1. Oui                                         | |       | 2. Non                                         | |       | 8. REFUS                                       | |       | 9. NSP                                         |

| 5B              |                                                                               | |--|--| | QUANTI<br>AUT11 | Pouvez-vous intervenir sur la quantité de travail qui vous est<br>attribuée ? | | Car 1           | 1. Toujours<br>2. Souvent<br>3. Parfois<br>4. Jamais<br>8. REFUS<br>9. NSP    |

CT-RPS 2016

| 5B       |                                                               | |--|--| | NOUVELLE | Votre travail vous permet-il d'apprendre des choses nouvelles | | AUT12    | ?                                                             | | Car 1    | 1. Oui                                                        | |          | 2. Non                                                        | |          | 8. REFUS                                                      | |          | 9. NSP                                                        |

CT

| 5B     |                                                                | |--|--| | SIEG34 | Votre position professionnelle actuelle correspond-elle bien à | | AUT15  | votre formation ?                                              | | Car 1  | 1. Oui                                                         | |        | 2. Non                                                         | |        | 8. REFUS                                                       | |        | 9. NSP                                                         |

CT

## C - Ambiance de travail

| 5C              | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                         | |--|--| | AIDCHEF<br>SOC1 | Si vous avez du mal à faire un travail délicat, compliqué, est-ce que vous<br>êtes aidé par | |                 | a) … vos supérieurs hiérarchiques ?                                                         | | Car 1           | 1. Oui<br>2. Non<br>3. Sans objet (pas de supérieur hiérarchique)<br>8. REFUS<br>9. NSP     |

<!-- p. 78 -->

| 5C              |                                                                                                                                                                                               | |--|--| | AIDCOLL<br>SOC2 | Si vous avez du mal à faire un travail délicat, compliqué, est-ce que vous<br>êtes aidé par<br>b)<br>…<br>les<br>autres<br>personnes<br>avec<br>qui<br>vous<br>travaillez<br>habituellement ? | | Car 1           | 1. Oui<br>2. Non<br>3. Sans objet (pas de collègue)<br>8. REFUS<br>9. NSP                                                                                                                     |

| 5C                | Si non salarié (STATUT(PRO2) = 8 à 10)                                                                                                                      | |--|--| | AIDCOLLNS<br>SOC3 | Si vous avez du mal à faire un travail délicat, compliqué, est-ce que vous<br>êtes aidé par<br>c) … les personnes avec qui vous travaillez habituellement ? | | Car 1             | 1. Oui<br>2. Non<br>3. Sans objet (pas de collègue)<br>8. REFUS<br>9. NSP                                                                                   |

CT-RPS 2016

| 5C               |                                                                            | |--|--| | TRAVSEUL<br>SOC4 | Travaillez-vous seul ?                                                     | | Car 1            | 1. Toujours<br>2. Souvent<br>3. Parfois<br>4. Jamais<br>8. REFUS<br>9. NSP |

« Seul » signifie ici en autonomie, sans collaborer et non pas seul dans une pièce. CT

| 5C              | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                                                                                                                 | |--|--| | COLLECT<br>SOC6 | Avez-vous l'occasion d'aborder collectivement, avec d'autres<br>personnes de votre atelier ou de votre service, des questions<br>d'organisation ou de fonctionnement de votre unité de travail<br>? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                                                                              |

Exemples : répartition du travail, nature du matériel et des équipements, pannes, défauts et incidents, pauses, remplacements, horaires, conditions de travail et de sécurité. CT

| 5C              | Si discussions collectives (COLLECT (SOC6) = )                          | |--|--| | REUNION<br>SOC7 | Ces échanges se déroulent-ils dans le cadre de réunions<br>organisées ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                  |

<!-- p. 79 -->

| 5C                | Si l'individu travaille en relation avec du public (PUBLIC (EXI19) = 1)                                                                                  | |--|--| | TENSION1<br>SOC17 | Vivez-vous des situations de tension …<br>a) … dans vos rapports avec le public (usagers, patients,<br>élèves, voyageurs, clients, fournisseurs, etc…) ? | | Car 1             | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                                   |

Souvent ou suffisamment pour perturber votre travail.

CT

| 5C       | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                 | |--|--| | TENSION2 | Vivez-vous des situations de tension …                              | | SOC18    | b) … dans vos rapports avec vos supérieurs hiérarchiques ?          | | Car 1    | 1. Oui<br>2. Non                                                    | |          | 3. Sans objet (pas de supérieur hiérarchique)<br>8. REFUS<br>9. NSP |

Souvent ou suffisamment pour perturber votre travail.

CT

| 5C       |                                             |  | |--|--|--| | TENSION3 | Vivez-vous des situations de tension …      |  | | SOC19    | c) … dans vos rapports avec vos collègues ? |  | | Car 1    | 1. Oui                                      |  | |          | 2. Non                                      |  | |          | 3. Sans objet (pas de collègue)             |  | |          | 8. REFUS                                    |  | |          | 9. NSP                                      |  |

Souvent ou suffisamment pour perturber votre travail.

CT

| 5C                | Si supervise (ENCADR (PRO17) = 1 ou 2)                                                                     | |--|--| | TENSION4<br>SOC20 | Vivez-vous des situations de tension …<br>d) … dans vos rapports avec les personnes que vous<br>encadrez ? | | Car 1             | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                     |

Souvent ou suffisamment pour perturber votre travail.

CT

| 5C<br>EMOTION<br>SOC24 | Dans<br>votre<br>travail,<br>vous<br>arrive-t-il<br>d'être<br>bouleversé(e),<br>secoué(e), ému(e) ? | |--|--| | Car 1                  | 1. Tous les jours ou presque<br>2. Souvent<br>3. Parfois<br>4. Jamais<br>8. Refus<br>9. NSP         |

CT-RPS 2016

<!-- p. 80 -->

| 5C      |                                                              | |--|--| | ATTENTE | Savez-vous précisément ce que l'on attend de vous au travail | | SOC25   | ?                                                            | | Car 1   | 1. Toujours                                                  | |         | 2. Souvent                                                   | |         | 3. Parfois                                                   | |         | 4. Jamais                                                    | |         | 5. Sans objet                                                | |         | 8. REFUS                                                     | |         | 9. NSP                                                       |

CT-RPS 2016

| 5C               |                                                                                                           | |--|--| | CRITERE<br>SOC26 | Les<br>critères<br>utilisés<br>pour<br>évaluer<br>votre<br>travail<br>vous<br>paraissent-ils pertinents ? | | Car 1            | 1. Oui<br>2. Non<br>3. Non concerné (non salarié)<br>8. REFUS<br>9. NSP                                   |

CT-RPS 2016

| 5C      |                                                            | |--|--| | CONTRAD | Recevez-vous des ordres ou des indications contradictoires | | SOC23   | ?                                                          | | Car 1   | 1. Oui                                                     | |         | 2. Non                                                     | |         | 8. REFUS                                                   | |         | 9. NSP                                                     |

CT

| 5C    | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)   | |--|--| | EVA   | Avez-vous au moins un entretien d'évaluation par an ? | | SOC24 |                                                       | | Car 1 | 1. Oui                                                | |       | 2. Non                                                | |       | 8. REFUS                                              | |       | 9. NSP                                                |

CT

| 5C               | Si entretien annuel (EVA (SOC20) = 1)                                                                                | |--|--| | EVACRIT<br>SOC25 | L'entretien porte-t-il sur des critères précis et mesurables<br>(objectifs, résultats, acquisition de compétences) ? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                               |

<!-- p. 81 -->

## D - Représentation du personnel

| 5D             | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                                     | |--|--| | SYNDIC<br>PER1 | Êtes-vous adhérent ou sympathisant d'une organisation<br>syndicale de salariés ?                        | | Car 1          | 1. Oui, adhérent(e)<br>2. Oui, sympathisant(e)<br>3. Non<br>4. Vous ne souhaitez pas répondre<br>9. NSP |

CT

| 5D               | Si l'individu n'est pas salarié ( STATUT (PRO2) = 8 à 10)                                               | |--|--| | SYNDICNS<br>PER2 | Êtes-vous adhérent ou sympathisant d'une organisation<br>professionnelle ?                              | | Car 1            | 1. Oui, adhérent(e)<br>2. Oui, sympathisant(e)<br>3. Non<br>4. Vous ne souhaitez pas répondre<br>9. NSP |

CT

| 5D              | Si l'individu est adhérent ou sympathisant d'une organisation syndicale de salariés<br>(SYNDIC = 1,2) ou d'une organisation professionnelle (SYNDICNS = 1,2) | |--|--| | SYNDPAR<br>PER3 | Au cours des 12 derniers mois, avez-vous participé aux<br>activités de cette organisation ?                                                                  | | Car 1           | 1. Régulièrement<br>2. De temps en temps tout au long de l'année<br>3. Rarement ou jamais<br>8. REFUS<br>9. NSP                                              |



| 5D                | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10)                                   | |--|--| | IREP_FLAG<br>PER4 | Votre<br>établissement<br>est-il<br>couvert<br>par<br>des<br>instances<br>représentatives du personnel telles que : | | Car 2             | 1. Oui<br>0. Non<br>-1 . Ne sait pas<br>-2 . Refuse de répondre                                                     |

Plusieurs réponses possibles 

| 5D      | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10) | |--|--| | IREP_a  | Un Comité Hygiène Sécurité et Conditions de Travail (CHSCT,<br>CSSCT)             | | Num - 8 | 1. Oui<br>0. Non                                                                  |

Calculée 

| 5D      | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10) | |--|--| | IREP_b  | Une ou plusieurs instances élues du personnel (DP, CE, DUP,<br>CSE…)              | | Num - 8 | 1. Oui<br>0. Non                                                                  |

Calculée 

<!-- p. 82 -->

| 5D      | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10) | |--|--| | IREP_c  | Un ou plusieurs délégués syndicaux (ou représentants de<br>sections syndicales)   | | Num - 8 | 1. Oui<br>0. Non                                                                  |

Calculée



| 5D      | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10) | |--|--| | IREP_d  | Non, aucune représentation collective du personnel                                | | Num - 8 | 1. Oui                                                                            | |         | 0. Non                                                                            |

Calculée



| 5D              | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10), et que<br>l'établissement est couvert par une instance représentative du personnel (IREP = 1 à 3)                                              | |--|--| | DISCURP<br>PER5 | Au cours des douze derniers mois, avez-vous participé à une<br>discussion autour de problèmes liés à votre travail avec un<br>représentant du personnel (délégué du personnel, délégué<br>syndical, membre du CE ou du CHSCT …) ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP<br>9. NSP                                                                                                                                                                                  |

Il peut s'agir d'une discussion en tête à tête ou collective, informelle ou formelle, une assemblée générale … CT

| 5D                  | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10), et que<br>l'établissement est couvert par une instance représentative du personnel (IREP = 1 à 3) | |--|--| | MANDAT_FLAG<br>PER6 | Vous-mêmes, êtes-vous                                                                                                                                                                | | Car 2               | 1. Oui<br>0. Non<br>-1 . Ne sait pas<br>-2 . Refuse de répondre                                                                                                                      |

Plusieurs réponses possibles 

|          | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10), et que<br>l'établissement est couvert par une instance représentative du personnel (IREP = 1 à 3) | |--|--| | MANDAT_a | Délégué syndical (ou RSS)                                                                                                                                                            | | Num - 8  | 1. Oui<br>0. Non                                                                                                                                                                     |

Calculée 

|          | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10), et que<br>l'établissement est couvert par une instance représentative du personnel (IREP = 1 à 3) | |--|--| | MANDAT_b | Elu du personnel                                                                                                                                                                     | | Num - 8  | 1. Oui<br>0. Non                                                                                                                                                                     |

Calculée



<!-- p. 83 -->

|          | Si l'individu est salarié, hors salariés de particulier ( STATUT (PRO2) # 7 à 10), et que<br>l'établissement est couvert par une instance représentative du personnel (IREP = 1 à 3) | |--|--| | MANDAT_c | Aucun mandat ou responsabilité                                                                                                                                                       | | Num - 8  | 1. Oui<br>0. Non                                                                                                                                                                     |

Calculée 

## E - Moyens de travail

| 5E              |                                                                                                | |--|--| | CORRTAN<br>OUT1 | Pour effectuer correctement votre travail, avez-vous en général …<br>a) … un temps suffisant ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                         |



| 5E      |                                                                   | |--|--| | CORRINF | Pour effectuer correctement votre travail, avez-vous en général … | | OUT2    | b) … des informations claires et suffisantes ?                    | | Car 1   | 1. Oui                                                            | |         | 2. Non                                                            | |         | 8. REFUS                                                          | |         | 9. NSP                                                            |



| 5E              |                                                                                                                                                   | |--|--| | CORRCOP<br>OUT3 | Pour effectuer correctement votre travail, avez-vous en général …<br>c) … la possibilité de coopérer<br>(échanges d'informations,<br>entraide,) ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                            |



| 5E              |                                                                                                                                          | |--|--| | CORRCOL<br>OUT4 | Pour effectuer correctement votre travail, avez-vous en général …<br>d) … des collaborateurs (ou des collègues) en nombre<br>suffisant ? | | Car 1           | 1. Oui<br>2. Non<br>3. Sans objet (pas de collaborateurs ou de collègues)<br>8. REFUS<br>9. NSP                                          | |                 |                                                                                                                                          |



| 5E      | Pour effectuer correctement votre travail, avez-vous en général … | |--|--| | CORRLOG | e) … des logiciels et des programmes informatiques bien           | | OUT5    | adaptés ?                                                         | | Car 1   | 1. Oui<br>2. Non<br>3. Sans objet<br>8. REFUS<br>9. NSP           |

<!-- p. 84 -->

| 5E              |                                                                                                             | |--|--| | CORRMAT<br>OUT6 | Pour effectuer correctement votre travail, avez-vous en général …<br>f) … un matériel suffisant et adapté ? | | Car 1           | 1. Oui<br>2. Non<br>3. Sans objet<br>8. REFUS<br>9. NSP                                                     |

| 5E               |                                                                                                                          | |--|--| | CORRFORM<br>OUT7 | Pour effectuer correctement votre travail, avez-vous en général …<br>g) … une formation continue suffisante et adaptée ? | | Car 1            | 1. Oui<br>2. Non<br>3. Sans objet<br>8. REFUS<br>9. NSP                                                                  |



| 5E               |                                                                                                                                                               | |--|--| | TELEPORT<br>OUT8 | Utilisez-vous<br>un<br>téléphone<br>portable<br>ou<br>un<br>appareil<br>de<br>téléphonie mobile pour des besoins professionnels (même<br>occasionnellement) ? | | Car 1            | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                                                        |

Y compris Pocket PC, Blackberry, Smartphone, I phone CT 2013

| 5E     |                                                                   | |--|--| | MICRO1 | Au cours de votre travail, utilisez-vous (même occasionnellement) | | OUT9   | a) … un ordinateur fixe ?                                         | | Car 1  | 1. Oui                                                            | |        | 2. Non                                                            | |        | 8. REFUS                                                          | |        | 9. NSP                                                            |

Y compris terminal ou console. CT 2013

| 5E     |                                                                   | |--|--| | MICRO2 | Au cours de votre travail, utilisez-vous (même occasionnellement) | | OUT10  | b) … un ordinateur portable ?                                     | | Car 1  | 1. Oui                                                            | |        | 2. Non                                                            | |        | 8. REFUS                                                          | |        | 9. NSP                                                            |



| 5E     |                                                                   | |--|--| | MICRO3 | Au cours de votre travail, utilisez-vous (même occasionnellement) | | OUT11  | c) … une tablette, un PDA, un terminal mobile ou embarqué ?       | | Car 1  | 1. Oui                                                            | |        | 2. Non                                                            | |        | 8. REFUS                                                          | |        | 9. NSP                                                            |



<!-- p. 85 -->

| 5E    |                                                                 | |--|--| | MEL   | Disposez-vous<br>d'une<br>boîte<br>à<br>lettres<br>électronique | | OUT12 | professionnelle ?                                               | | Car 1 | 1. Oui<br>2. Non                                                | |       | 8. REFUS<br>9. NSP                                              |

| 5E       |                                                               | |--|--| | INTERNET | En dehors de la messagerie, utilisez-vous Internet à des fins | | OUT13    | professionnelles ?                                            | | Car 1    | 1. Oui                                                        | |          | 2. Non                                                        | |          | 8. REFUS                                                      | |          | 9. NSP                                                        |



| 5E       |                                                         | |--|--| | INTRANET | Utilisez-vous un Intranet ou un réseau de communication | | OUT14    | interne ?                                               | | Car 1    | 1. Oui                                                  | |          | 2. Non                                                  | |          | 8. REFUS                                                | |          | 9. NSP                                                  |



| 5E              | Si l'individu possède une boite à lettres électronique (MEL (OUT12) = 1)                                                             | |--|--| | MELDOM<br>OUT15 | Quand vous n'êtes pas sur votre lieu de travail, pouvez-vous accéder…<br>a) … à votre boîte à lettres électronique professionnelle ? | | Car 1           | 1. Oui<br>2. Non<br>8. REFUS<br>9. NSP                                                                                               |



| 5E              | Si l'individu n'est pas salarié de particuliers (STATUT (PRO2) # 7)                                                                               | |--|--| | RESDOM<br>OUT16 | Quand vous n'êtes pas sur votre lieu de travail, pouvez-vous accéder…<br>b) … au système informatique de votre établissement ou<br>organisation ? | | Car 1           | 1. Oui<br>2. Non<br>3. Sans objet (pas de système informatique dans l'établissement)<br>8. REFUS<br>9. NSP                                        |



| 5E             | Si l'individu peut accéder à distance à sa boîte à lettres électroniques ou au système<br>d'information (MELDOM (OUT15) = 1 ou RESDOM (OUT16) = 1) | |--|--| | UTDOM<br>OUT17 | En pratique, à quelle fréquence vous connectez-vous à<br>distance à ces outils ?                                                                   | | Car 1          | 1. Tous les jours<br>2. Souvent<br>3. Parfois<br>4. Jamais<br>8. Refus<br>9. NSP                                                                   |



<!-- p. 86 -->

| 5E                | Si l'individu peut accéder à distance à sa boîte à lettres électroniques ou au système<br>d'information (MELDOM (OUT15) = 1 ou RESDOM (OUT16) = 1) | |--|--| | DECONNEC<br>OUT18 | Cette boîte aux lettres ou ce réseau est-il accessible à toute<br>heure ?                                                                          | | Car 1             | 1. Oui<br>2. Non, uniquement durant une place fixe<br>8. REFUS<br>9. NSP                                                                           |



| 5E             | Si l'individu utilise au moins un des matériels informatiques précités. (Si MICRO1 (OUT9)<br>= 1 ou MICRO2 (OUT10) = 1 ou MICRO3 (OUT11) = 1 ou MEL (OUT12) = 1 ou<br>INTERNET (OUT13) = 1 ou INTRANET (OUT14) = 1 ou RESDOM (OUT16) = 1) | |--|--| | INFOH<br>OUT19 | Au total, pendant combien de temps utilisez-vous à titre<br>professionnel les matériels informatiques dont nous venons<br>de parler ?                                                                                                     | | Num - 8        | De 1 à 44640                                                                                                                                                                                                                              |

INFOH\_DRAP Pour qualifier la non réponse voir INFOH\_DRAP

Une estimation même grossière est suffisante, il faut ajouter le temps passé sur le lieu de travail et ailleurs. CT 2013

| 5E         |                             | |--|--| | INFOH_DRAP | Variable "drapeau" de INFOH | | OUT19      |                             | | Car 2      | 1. Réponse                  | |            | 0. Sans objet               | |            | -1 . Ne sait pas            | |            | -2 . Refuse de répondre     |

Variable drapeau



| 5E      |                                                 | |--|--| | INFOHU1 | Durée d'utilisation des matériels informatiques | | OUT19   | Durée exprimée en …                             | | Car 1   | 1. minutes                                      | |         | 2. heures                                       |



| 5E      |                                                 | |--|--| | INFOHU2 | Durée d'utilisation des matériels informatiques | | OUT19   | par …                                           | | Car 1   | 1. jour                                         | |         | 2. semaine                                      | |         | 3. mois                                         |



| 5E      |                                                              | |--|--| | TPSINFO | Durée en minutes d'utilisation de l'informatique par semaine | | OUT19   |                                                              | | Num - 8 | De 1 à 10080                                                 |

CALCULEE : vaut INFOH x 60 si INFOHU1=2 x 5 si INFOHU2=1 / 4 si INFOHU2=3

Ex : TPSINFO=INFOHx60 /4 si INFOHU1=2 et INFOHU2=3

<!-- p. 87 -->

| 5E                | Si l'individu est salarié et s'il utilise au moins un des matériels informatiques précités. (Si<br>STATUT (PRO2) = 1 à 7 ET Si MICRO1 (OUT9) = 1 ou MICRO2 (OUT10) = 1 ou<br>MICRO3 (OUT11) = 1 ou MEL (OUT12) = 1 ou INTERNET (OUT13) = 1 ou INTRANET<br>(OUT14) = 1 ou | |--|--| | CONTRNUM<br>OUT20 | A votre connaissance ces outils informatiques permettent-ils<br>à votre employeur de contrôler votre activité ?                                                                                                                                                          | | Car 1             | 1. Oui, dans une forte mesure<br>2. Oui, dans une faible mesure<br>3. Non<br>8. REFUS<br>9. NSP                                                                                                                                                                          |



## F - Evolution

| 5F              |                                                                       | |--|--| | CRAINTE<br>INS1 | Pour l'année qui vient, avez-vous des craintes pour votre<br>emploi ? | | Car 2           | 1. Oui<br>2. Non<br>9. Ne sait pas<br>8. REFUS                        |



| 5F             |                                                                                                                   | |--|--| | METIER<br>INS2 | Dans<br>les<br>trois<br>prochaines<br>années,<br>pensez-vous<br>devoir<br>changer de qualification ou de métier ? | | Car 2          | 1. Oui<br>2. Non<br>9. Ne sait pas<br>8. REFUS                                                                    |



| 5F     | Si vous deviez perdre ou quitter votre emploi actuel, vous | |--|--| | NOCHOM | serait-il facile de trouver un emploi avec un salaire, une | | INS3   | rémunération similaire ?                                   | | Car 2  | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                     |



| 5F    |                                                                        | |--|--| | TENIR | Vous<br>sentez-vous<br>capable<br>de<br>faire<br>le<br>même<br>travail | | INS4  | qu'actuellement jusqu'à votre retraite ?                               | | Car 1 | 1. Oui                                                                 | |       | 2. Non                                                                 | |       | 9. NSP                                                                 | |       | 8. REFUS                                                               |

<!-- p. 88 -->

| 5F      |                     | |--|--| | SOUHAIT | Le souhaitez-vous ? | | INS5    |                     | | Car 1   | 1. Oui              | |         | 2. Non              | |         | 9. NSP              | |         | 8. REFUS            |

| 5F           | Si l'individu est salarié ( STATUT (PRO2) # 8 à 10)                                | |--|--| | MUTE<br>INS6 | Craignez-vous d'être muté(e) à un autre poste de travail<br>contre votre volonté ? | | Car 1        | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                             |

CT-RPS 2016

#### G - Changements

| 5G               | Si l'individu est salarié (STATUT (PRO2) # 8 à 10) mais n'est pas intérimaire<br>(TYPEMPLOI (PRO15) # 2, 3)                                                          | |--|--| | FORTMOD1<br>CHA1 | Au cours des douze derniers mois, votre environnement de travail a-t-il<br>été fortement modifié…<br>a) … par un changement de votre poste ou de votre fonction<br>? | | Car 1            | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                               |



| 5G               | Si l'individu n'est pas intérimaire (TYPEMPLOI (PRO15) # 2, 3)                                    | |--|--| | FORTMOD2<br>CHA2 | Au cours des douze derniers mois, votre environnement de travail a-t-il<br>été fortement modifié… | |                  | b) … par un changement dans les techniques utilisées ?                                            | | Car 1            | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                            |



| 5G               | Si l'individu n'est pas intérimaire (TYPEMPLOI (PRO15) # 2, 3)                                                                                                                                                                                      | |--|--| | FORTMOD3<br>CHA3 | Au cours des douze derniers mois, votre environnement de travail a-t-il<br>été fortement modifié…<br>c) … par une restructuration ou un déménagement de<br>l'établissement,<br>l'entreprise<br>ou<br>l'administration<br>où<br>vous<br>travaillez ? | | Car 1            | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                                                                              |

<!-- p. 89 -->

| 5G               | Si l'individu n'est pas intérimaire (TYPEMPLOI (PRO15) # 2, 3)                                                                                                                           | |--|--| | FORTMOD4<br>CHA4 | Au cours des douze derniers mois, votre environnement de travail a-t-il<br>été fortement modifié…<br>d) … par un changement de l'organisation du travail au sein<br>de l'établissement ? | | Car 1            | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                   |

| 5G               | Si l'individu n'est pas salarié de particuliers (STATUT (PRO2) # 7) et n'est pas intérimaire<br>(TYPEMPLOI (PRO15) # 2, 3)                                                          | |--|--| | FORTMOD5<br>CHA5 | Au cours des douze derniers mois, votre environnement de travail a-t-il<br>été fortement modifié…<br>e) … par un plan de licenciements dans l'établissement où<br>vous travaillez ? | | Car 1            | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                              |



| 5G               | Si l'individu n'est pas salarié de particuliers (STATUT (PRO2) # 7) et n'est pas intérimaire<br>(TYPEMPLOI (PRO15) # 2, 3)                                               | |--|--| | FORTMOD6<br>CHA6 | Au cours des douze derniers mois, votre environnement de travail a-t-il<br>été fortement modifié…<br>f) … par un rachat ou un changement dans l'équipe de<br>direction ? | | Car 1            | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                   |



| 5G               | Si l'individu n'est pas intérimaire (TYPEMPLOI (PRO15) # 2 ou 3)                                  | |--|--| | FORTMOD7<br>CHA7 | Au cours des douze derniers mois, votre environnement de travail a-t-il<br>été fortement modifié… | |                  | g) … par une autre raison ?                                                                       | | Car 1            | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                            |



| 5G              | Si au moins un changement en dehors d'un plan de licenciements (Si FORTMOD1<br>(CHA1) = 1 ou FORTMOD2 (CHA2) = 1 ou FORTMOD3 (CHA3) = 1 ou FORTMOD4<br>(CHA4) = 1 ou FORTMOD6 (CHA6) = 1 ou FORTMOD7 (CHA7) = 1) | |--|--| | CHANGOP<br>CHA8 | Pensez-vous que ces changements sont pour votre travail, …                                                                                                                                                       | | Car 1           | 1. plutôt positifs<br>2. plutôt négatifs<br>3. les aspects positifs et les aspects négatifs se compensent<br>9. NSP<br>8. REFUS                                                                                  |

<!-- p. 90 -->

| 5G               | Si au moins un changement (Si FORTMOD1 (CHA1) = 1 ou FORTMOD2 (CHA2) = 1 ou<br>FORTMOD3 (CHA3) = 1 ou FORTMOD4 (CHA4) = 1 ou FORTMOD5 (CHA5) = 1 ou<br>FORTMOD6 (CHA6) = 1 ou FORTMOD7 (CHA7) = 1) | |--|--| | CHGTINFO<br>CHA9 | Avez-vous reçu une information suffisante et adaptée au<br>moment de ces changements ?                                                                                                             | | Car 1            | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                             |

| 5G                | Si au moins un changement (Si FORTMOD1 (CHA1) = 1 ou FORTMOD2 (CHA2) = 1 ou<br>FORTMOD3 (CHA3) = 1 ou FORTMOD4 (CHA4) = 1 ou FORTMOD5 (CHA5) = 1 ou<br>FORTMOD6 (CHA6) = 1 ou FORTMOD7 (CHA7) = 1) | |--|--| | CHGTCONS<br>CHA10 | Avez-vous été consulté au moment de ces changements ?                                                                                                                                              | | Car 1             | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                             |



| 5G       | Si l'individu a été consulté au moment des changements (CHGTCONS (CHA10) =1) | |--|--| | CHGTINFL | Avez-vous eu l'impression d'avoir une influence sur la mise                  | | CHA11    | en œuvre de ces changements ?                                                | | Car 1    | 1. Oui                                                                       | |          | 2. Non                                                                       | |          | 9. NSP                                                                       | |          | 8. REFUS                                                                     |

<!-- p. 91 -->

# 5. Santé

# A - Santé déclarée

| 6A             |                                                                                                      | |--|--| | BSANTE<br>SAN1 | Comment jugez-vous votre état de santé général ?                                                     | | Car 1          | 1. Très bon<br>2. Bon<br>3. Assez bon<br>4. Mauvais<br>5. Très mauvais<br>9. Ne sait pas<br>8. REFUS |



| 6A    |                                                            | |--|--| | BCHRO | Avez-vous actuellement une maladie ou un problème de santé | | SAN2  | qui soit chronique ou de caractère durable ?               | | Car 1 | 1. Oui                                                     | |       | 2. Non                                                     | |       | 9. Ne sait pas                                             | |       | 8. REFUS                                                   |

Une maladie chronique est une maladie qui a duré ou peut durer pendant 6 mois au moins ; ou qui revient ou peut revenir régulièrement. Les allergies telles que le rhume des foins ne sont pas des maladies chroniques, ni les grippes ou les angines à répétition. CT 2013

| 6A            |                                                                                                                                        | |--|--| | BLIMI<br>SAN3 | Êtes-vous limité, depuis au moins six mois, à cause d'un<br>problème de santé dans les activités que les gens font<br>habituellement ? | | Car 1         | 1. Oui, fortement limité<br>2. Oui, limité mais pas fortement<br>3. Non, pas limité du tout<br>9. Ne sait pas<br>8. REFUS              |



| 6A            |                                                                                           | |--|--| | BRECO<br>SAN4 | Avez-vous une reconnaissance administrative d'un handicap<br>ou d'une perte d'autonomie ? | | Car 1         | 1. Oui<br>2. En cours<br>3. Non<br>9. Ne sait pas<br>8. REFUS                             |

Une aide sur les reconnaissances possibles est à votre disposition dans l'instruction aux enquêteurs. CT 2013

| 6A    |                                                        | |--|--| | SDOUL | Au cours des douze derniers mois, avez-vous souvent    | | SAN5  | ressenti des douleurs dans une partie de votre corps ? | | Car 1 | 1. Oui                                                 | |       | 2. Non                                                 | |       | 9. Ne sait pas                                         | |       | 8. REFUS                                               |

CT-RPS 2016

<!-- p. 92 -->

| 6A                   | Si a ressenti des douleurs dans une partie du corps (SDOUL=1)                                                | |--|--| | SDOULOC_FLAG<br>SAN6 | Pouvez-vous m'indiquer la localisation de cette ou ces<br>douleurs en me précisant le numéro correspondant ? | | Car 2                | 1. Oui<br>0. Non<br>-1 . Ne sait pas<br>-2 . Refuse de répondre                                              |

CT-RPS 2016

| 6A         | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_nb | Nombre de douleurs localisées                                 | | SAN6       |                                                               | | Num - 8    | 1. Réponse                                                    |

Calculée

Plusieurs réponses possibles CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_a | Douleurs à la tête                                            | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_b | Douleurs aux yeux                                             | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A                | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_c<br>SAN6 | Douleurs aux sinus                                            | | Num - 8           | 1. Oui<br>0. Non                                              |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_d | Douleurs aux oreilles                                         | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_e | Douleurs aux dents                                            | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

<!-- p. 93 -->

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_f | Douleurs au cou                                               | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_g | Douleurs aux épaules                                          | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_h | Douleurs au(x) coude(s)                                       | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_i | Douleurs au(x) poignet(s)                                     | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_j | Douleurs aux mains                                            | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_k | Douleurs à la poitrine, au torse                              | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_l | Douleurs au ventre                                            | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

93

<!-- p. 94 -->

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_m | Douleurs aux jambes                                           | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_n | Douleurs aux hanches                                          | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_o | Douleurs au(x) genoux                                         | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_p | Douleurs au(x) cheville(s)                                    | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_q | Douleurs au(x) pieds                                          | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_r | Douleurs au dos                                               | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_s | Douleurs au(x) bras                                           | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

Calculée

CT-RPS 2016

94

<!-- p. 95 -->

| 6A        | Si a ressenti des douleurs dans une partie du corps (SDOUL=1) | |--|--| | SDOULOC_t | Douleurs autres                                               | | SAN6      |                                                               | | Num - 8   | 1. Oui                                                        | |           | 0. Non                                                        |

## Calculée

CT-RPS 2016

| 6A    |                                                          | |--|--| | MEDEC | Avez-vous vu un médecin au cours des douze derniers mois | | SAN9  | ?                                                        | | Car 1 | 1. Oui<br>2. Non                                         | |       | 9. Ne sait pas                                           | |       | 8. REFUS                                                 |

CT-RPS 2016

<!-- p. 96 -->

## 6. Parcours familial et professionnel

## A - Vos parents

| 7A    |                                                        | |--|--| | NAIP  | Votre père est-il né …                                 | | TRA1  |                                                        | | Car 1 | 1. en France (métropole ou DOM-TOM)<br>2. à l'étranger | |       | 9. NSP                                                 | |       | 8. REFUS                                               |

CT

| 7A               |                                                                                                                                                                                                                                                                                                                                                          | |--|--| | LNAISPER<br>TRA2 | Lieu de naissance du père                                                                                                                                                                                                                                                                                                                                | | Car 2            | 11. France<br>21. Algérie<br>22. Maroc<br>23. Tunisie<br>24. Autres pays d'Afrique<br>31. Vietnam, Laos, Cambodge<br>34. Turquie<br>35. Autres Asie<br>41. Portugal<br>42. Espagne<br>43. Italie<br>44. Autres pays de l'UE 15<br>45. Autres pays de l'UE 27<br>46. Autres Europe<br>51. Autres<br>99. Non réponse<br>99. Non réponse<br>99. Non réponse |

Calculée : déclaration en clair (PAYSP) => Codification

CT

| 7A                 |                                                                                                                                                                                                                                                                                                                                                          | |--|--| | NATNAISPER<br>TRA3 | Quelle était la nationalité de votre père à sa naissance ?                                                                                                                                                                                                                                                                                               | | Car 2              | 11. France<br>21. Algérie<br>22. Maroc<br>23. Tunisie<br>24. Autres pays d'Afrique<br>31. Vietnam, Laos, Cambodge<br>34. Turquie<br>35. Autres Asie<br>41. Portugal<br>42. Espagne<br>43. Italie<br>44. Autres pays de l'UE 15<br>45. Autres pays de l'UE 27<br>46. Autres Europe<br>51. Autres<br>99. Non réponse<br>99. Non réponse<br>99. Non réponse |

Calculée : Déclaration en clair (NATIOP) => Codification

<!-- p. 97 -->

| 7A    |                                                        | |--|--| | NAIM  | Et votre mère est-elle née …                           | | TRA4  |                                                        | | Car 1 | 1. en France (métropole ou DOM-TOM)<br>2. à l'étranger | |       | 9. NSP<br>8. REFUS                                     |

CT

| 7A               |                                                                                                                                                                                                                                                                                                                                                          | |--|--| | LNAISMER<br>TRA5 | Lieu de naissance de la mère                                                                                                                                                                                                                                                                                                                             | | Car 2            | 11. France<br>21. Algérie<br>22. Maroc<br>23. Tunisie<br>24. Autres pays d'Afrique<br>31. Vietnam, Laos, Cambodge<br>34. Turquie<br>35. Autres Asie<br>41. Portugal<br>42. Espagne<br>43. Italie<br>44. Autres pays de l'UE 15<br>45. Autres pays de l'UE 27<br>46. Autres Europe<br>51. Autres<br>99. Non réponse<br>99. Non réponse<br>99. Non réponse |

Calculée : Déclaration en clair (PAYSM) => codification

CT

| 7A                 |                                                                                                                                                                                                                                                                                                                                                          | |--|--| | NATNAISMER<br>TRA6 | Quelle était la nationalité de votre mère à sa naissance ?                                                                                                                                                                                                                                                                                               | | Car 2              | 11. France<br>21. Algérie<br>22. Maroc<br>23. Tunisie<br>24. Autres pays d'Afrique<br>31. Vietnam, Laos, Cambodge<br>34. Turquie<br>35. Autres Asie<br>41. Portugal<br>42. Espagne<br>43. Italie<br>44. Autres pays de l'UE 15<br>45. Autres pays de l'UE 27<br>46. Autres Europe<br>51. Autres<br>99. Non réponse<br>99. Non réponse<br>99. Non réponse |

Calculée : Déclaration en clair (NATIOM) => Codification

<!-- p. 98 -->

| 7A               |                                                                                                                                                                                           | |--|--| | NATIO1N1<br>TRA7 | Et vous-même, êtes-vous ?                                                                                                                                                                 | | Car 1            | 1. Français de naissance, y compris par réintégration<br>2. Français par naturalisation, mariage, déclaration ou option à sa majorité<br>3. Étranger<br>4. Apatride<br>9. NSP<br>8. REFUS |

Si l'enquêté a la nationalité française (item 1 ou 2 ) et une nationalité étrangère (item 3), renseigner les deux cas. CT

| 7A               |                                                                                                                                                                                           | |--|--| | NATIO1N2<br>TRA7 | Et vous-même, êtes-vous ?                                                                                                                                                                 | | Car 1            | 1. Français de naissance, y compris par réintégration<br>2. Français par naturalisation, mariage, déclaration ou option à sa majorité<br>3. Étranger<br>4. Apatride<br>9. NSP<br>8. REFUS |

Si l'enquêté a la nationalité française (item 1 ou 2 ) et une nationalité étrangère (item 3), renseigner les deux cas. CT

| 7A           | Si l'enquêté a une nationalité étrangère (NATIO1NA (TRA8)=3 ou NATIO1NB(TRA8)=3)                                                                                                                                                                                                                                                                                                                                                            | |--|--| | NATI<br>TRA8 | Quelle est votre nationalité ?                                                                                                                                                                                                                                                                                                                                                                                                              | | Car 2        | 11. Française<br>21. Algérienne<br>22. Marocaine<br>23. Tunisienne<br>24. Nationalité d'un autre pays d'Afrique<br>31. Vietnamienne, laotienne, cambodgienne<br>34. Turque<br>35. Nationalité d'un autre pays d'Asie<br>41. Portugaise<br>42. Espagnole<br>43. Italienne<br>44. Nationalité d'un autre pays de l'UE 15<br>45. Nationalité d'un autre pays de l'UE 27<br>46. Nationalité d'un autre pays d'Europe<br>51. Autres nationalités |

Calculée : Déclaration en clair (NATIO2N) => Codification CT

| 7A            |                                                                                                                                                                                                                                               | |--|--| | ACTIP<br>TRA9 | Quand vous aviez 15 ans, votre père ?                                                                                                                                                                                                         | | Car 2         | 1. Travaillait<br>2. Était au chômage<br>3. Était retraité, retiré des affaires, préretraité<br>4. Était inactif, mais avait déjà travaillé<br>5. Était inactif, et n'avait jamais travaillé<br>6. Était décédé<br>9. Ne sait pas<br>8. REFUS |

<!-- p. 99 -->

| 7A      | Si le père travaillait ou avait travaillé (ACTIP (TRA9) = 1 à 4) | |--|--| | PROFPER | Quelle était sa (dernière) profession principale ?               | | TRA10   |                                                                  | | Car 4   |                                                                  |

CALCULEE : Déclaration en clair (PROFP) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 7A     | Si le père travaillait ou avait travaillé (ACTIP (TRA9) = 1 à 4) | |--|--| | CSEPER | Quelle était sa (dernière) profession principale ?               | | TRA10  |                                                                  | | Car 2  |                                                                  |

CALCULEE : Déclaration en clair (PROFP) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 7A      | Si le père travaillait ou avait travaillé (ACTIP (TRA9) = 1 à 4) | |--|--| | CSERPER | Quelle était sa (dernière) profession principale ?               | | TRA10   | Catégorie socioprofessionnelle, niveau regroupé (PCS 2003)       | | Car 4   |                                                                  |

CALCULEE : Déclaration en clair (PROFP) => traitement SICORE (embarqué puis en bureau) et reprise manuelle

| 7A               | Si le père travaillait ou avait travaillé (ACTIP (TRA9) = 1 à 4)                                                                                                                                                                                                                          | |--|--| | STATUTP<br>TRA11 | Travaillait-il ?                                                                                                                                                                                                                                                                          | | Car 2            | 1. A son compte (artisan, commerçant, agriculteur, profession libérale)<br>2. En aidant un membre de sa famille dans son travail sans être salarié<br>3. Comme salarié de l'État ou des collectivités locales ou hôpitaux publics<br>4. Comme autre salarié<br>9. Ne sait pas<br>8. REFUS |

CT

| 7A             |                                                                                                                                                                                                                                                     | |--|--| | ACTIM<br>TRA12 | Quand vous aviez 15 ans, votre mère ?                                                                                                                                                                                                               | | Car 2          | 1. Travaillait<br>2. Était au chômage<br>3. Était retraitée, retirée des affaires, préretraitée<br>4. Était inactive, mais avait déjà travaillé<br>5. Était inactive, et n'avait jamais travaillé<br>6. Était décédée<br>9. Ne sait pas<br>8. REFUS |

CT

| 7A      | Si la mère travaillait ou avait travaillé (ACTIM (TRA12) = 1 à 4) | |--|--| | PROFMER | Quelle était sa (dernière) profession principale ?                | | TRA13   |                                                                   | | Car 4   |                                                                   |

CALCULEE : Déclaration en clair (PROFM) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 7A     | Si la mère travaillait ou avait travaillé (ACTIM (TRA12) = 1 à 4) | |--|--| | CSEMER | Quelle était sa (dernière) profession principale ?                | | TRA13  |                                                                   | | Car 2  |                                                                   |

CALCULEE : Déclaration en clair (PROFM) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

<!-- p. 100 -->

| 7A      | Si la mère travaillait ou avait travaillé (ACTIM (TRA12) = 1 à 4) | |--|--| | CSERMER | Quelle était sa (dernière) profession principale ?                | | TRA13   | Catégorie socioprofessionnelle, niveau regroupé (PCS 2003)        | | Car 4   |                                                                   |

CALCULEE : Déclaration en clair (PROFM) => traitement SICORE (embarqué puis en bureau) et reprise manuelle

| 7A               | Si la mère travaillait ou avait travaillé (ACTIM (TRA12) = 1 à 4)                                                                                                                                                                                                                              | |--|--| | STATUTM<br>TRA14 | Travaillait-elle ?                                                                                                                                                                                                                                                                             | | Car 2            | 1. A son compte (artisan, commerçante, agricultrice, profession libérale)<br>2. En aidant un membre de sa famille dans son travail sans être salariée<br>3. Comme salariée de l'État ou des collectivités locales ou hôpitaux publics<br>4. Comme autre salariée<br>9. Ne sait pas<br>8. REFUS |

CT

| 7A    |                                                                                                                                                                                                                                                        | |--|--| | NATIO | Nationalité                                                                                                                                                                                                                                            | | Car 1 | 1. Français de naissance, y compris par réintégration<br>2. Français par naturalisation, mariage, déclaration ou option à sa majorité<br>3. Etranger<br>4. Apatride<br>5. Français de naissance et étranger<br>6. Français par acquisition et étranger |

CALCULEE : Recodification des modalités de NATIO1NA et NATIO1NB

Si NATIO1NA=1 et NATIO1NB=vide alors NATIO=1

Si NATIO1NA=2 et NATIO1NB=vide alors NATIO=2

Si NATIO1NA=3 et NATIO1NB=vide alors NATIO=3

Si NATIO1NA=4 et NATIO1NB=vide alors NATIO=4

Si NATIO1NA=1 et NATIO1NB=3 alors NATIO=5

Si NATIO1NA=2 et NATIO1NB=3 alors NATIO=6

CT

| 7A    |                       | |--|--| | IMMI  | Immigré (indicatrice) | | Car 1 | 0. Non immigré        | |       | 1. Immigré            |

CALCULEE : vaut 1 si LNAIS=2 et NATIO<>1 ou 5, 0 sinon

CT

| 7A      |                              | |--|--| | PERIMMI | Père immigré (indicatrice)   | | Car 1   | 0. Non immigré<br>1. Immigré |

CALCULEE : vaut 1 si NAIP=2 et NATIOP <> "FRANCAISE" , 0 sinon

<!-- p. 101 -->

| 7A      |                            | |--|--| | MERIMMI | Mère immigée (indicatrice) | | Car 1   | 0. Non immigrée            | |         | 1. Immigrée                |

CALCULEE : vaut 1 si NAIM=2 et NATIOM <> "FRANCAISE" , 0 sinon

CT

| 7A      |                                                   | |--|--| | LIENMIG | Lien à la migration                               | | Car 1   | 1. Immigré<br>2. Deux parents immigrés            | |         | 3. Un seul parent immigré<br>4. Autres situations |

CALCULEE : prend la valeur 1 si IMMI=1; sinon prend la valeur 2 si PERIMMI=1 et MERIMMI=1; sinonprend la valeur 3 si PERIMMI=1 ou MERIMMI=1; sinon prend la valeur 4 CT

## B - Formation

| 7B     |                                                           | |--|--| | ETUDES | Êtes-vous inscrit dans un établissement d'enseignement (y | | TRA15  | compris cours par correspondance ou apprentissage) ?      | | Car 1  | 1. Oui                                                    | |        | 2. Non                                                    | |        | 9. NSP                                                    | |        | 8. REFUS                                                  |

CT

| 6B      | Si inscrit dans un établissement d'enseignement (ETUDES (TRA15) = 1) | |--|--| | ETUDIPL | Cette formation conduit-elle à un diplôme ou à un titre              | | TRA16   | reconnu ?                                                            | | Car 1   | 1. Oui                                                               | |         | 2. Non                                                               | |         | 9. NSP                                                               | |         | 8. REFUS                                                             |

CT

| 6B                | Si inscrit dans un établissement d'enseignement (ETUDES (TRA15) = 1) | |--|--| | FORMINIT<br>TRA17 | Est-ce dans le cadre de votre formation initiale ?                   | | Car 1             | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                               |

CT

| 6B               | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1)        | |--|--| | FINETUD<br>TRA18 | En quelle année ou à quel âge avez-vous terminé vos études<br>initiales ? | | Num - 8          | 0, 6 à année de l'enquête                                                 |

<!-- p. 102 -->

| 6B       | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1) | |--|--| | FINETUDU | En quelle année ou à quel âge avez-vous terminé vos études         | | TRA19    | initiales ? Réponse en âge ou en année ?                           | | Car 1    | 1. âge                                                             | |          | 2. année                                                           |

CT

| 6B       |                     | |--|--| | AGFINETU | Age de fin d'études | | -        | 6 à 99              |

CALCULEE

CT

| 6B       |                           | |--|--| | ANFINETU | Année de fin d'études     | | -        | 1900 à année de l'enquête |

CALCULEE

CT

| 6B                | Si l'enquêté est panel (*)                                            | |--|--| | DIPLOPAN<br>TRA20 | Au cours des trois dernières années, avez-vous obtenu un<br>diplôme ? | | Car 1             | 1. Oui<br>2. Non<br>9. Ne sait pas<br>8. REFUS                        |

CT-RPS 2016

| 6B                   | Si l'enquêté est panel et répondant en 2016                           | |--|--| | DIPLOPAN_16<br>TRA20 | Au cours des trois dernières années, avez-vous obtenu un<br>diplôme ? | | Car 1                | 1. Oui<br>2. Non<br>9. Ne sait pas<br>8. REFUS                        |

Données 2016 CT-RPS 2016

| 6B               |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | |--|--| | DIPLOME<br>TRA21 | Quel est votre diplôme le plus élevé ?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | | Car 2            | 0. Aucun diplôme<br>1. CEP (certificat d'études primaires) ou diplôme étranger de même niveau<br>2. Brevet des collèges, BEPC, brevet élémentaire ou diplôme étranger de même niveau<br>3. CAP, BEP ou diplôme de ce niveau<br>4. Baccalauréat technologique ou professionnel ou diplôme de ce niveau<br>5. Baccalauréat général (séries A, B, C, D, E, ES, L, S), brevet supérieur, capacité en<br>droit, DAEU, ESEU ou diplôme étranger de même niveau<br>6. Diplôme de niveau Bac+2<br>7. Diplôme de niveau bac +3 ou bac +4 (licence, licence professionnelle, maîtrise, master<br>,…)<br>8. Diplôme de niveau supérieur à bac+4 (master 2, DES, DEA, DESS, doctorat, diplôme<br>d'une grande école)<br>99. NSP<br>98. REFUS |

<!-- p. 103 -->

### C - Trajectoire professionnelle depuis la fin des études - Individus ENTRANTS 2019

| 6C     | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1)   | |--|--| | TCHOLE | Depuis la fin de vos études initiales, avez-vous vécu les situations | | TRA22  | suivantes …                                                          | |        | a) … une période de chômage d'un an ou plus ?                        | | Car 1  | 1. Une fois                                                          | |        | 2. Plusieurs fois                                                    | |        | 3. Jamais                                                            | |        | 9. NSP                                                               | |        | 8. REFUS                                                             |

CT

| 6C              | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1)                                                                                               | |--|--| | TCHOCE<br>TRA23 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>b) … une période de chômage de plus de trois mois mais<br>moins d'un an ? | | Car 1           | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                              |

CT

| 6C             | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1)                                                                                                                  | |--|--| | TSANE<br>TRA24 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>c) … vous avez arrêté de travailler pendant un an ou plus à<br>cause de problèmes de santé ? | | Car 1          | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                 |

CT

| 6C             | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1)                                                                                                                                                                                         | |--|--| | TINAE<br>TRA25 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>d) … vous avez arrêté de travailler pendant un an ou plus pour<br>une autre raison, par exemple s'occuper de votre foyer, de vos<br>enfants, reprendre des études ? | | Car 1          | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                        |

<!-- p. 104 -->

| 6C               | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1)                                                                                                 | |--|--| | TINDEPE<br>TRA26 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>e) … vous avez travaillé à votre compte, employant ou non<br>des salariés ? | | Car 1            | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                            |

CT

| 6C        | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1) et n'a pas toujours<br>ou presque été indépendant (TINDEP (TRA25) # 1) | |--|--| | TINTERIME | Depuis la fin de vos études, vous est-il arrivé …                                                                                         | | TRA27     | a) … de travailler comme intérimaire ?                                                                                                    | | Car 1     | 1. Toujours ou presque<br>2. Parfois                                                                                                      | |           | 3. Jamais                                                                                                                                 | |           | 9. NSP                                                                                                                                    | |           | 8. REFUS                                                                                                                                  |

CT

| 6C             | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1) et n'a pas toujours<br>ou presque été indépendant (TINDEP (TRA25) # 1) ou intérimaire (TINTERIM (TRA26)<br>#1) | |--|--| | TCDDE<br>TRA28 | Depuis la fin de vos études, vous est-il arrivé …<br>b) … de travailler avec un contrat à durée déterminée ou un<br>contrat aidé, ?                                               | | Car 1          | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                           |

CT

| 6C              | Si l'enquêté a terminé ses études initiales (FORMINIT (TRA17) # 1)                                                                                                                                        | |--|--| | NBEMPE<br>TRA29 | En dehors de votre emploi actuel et depuis que vous avez<br>terminé vos études, combien d'emplois de PLUS D'UN AN<br>dans des entreprises différentes ou pour des employeurs<br>différents avez-vous eu ? | | Car 2           | 00. Aucun<br>01. Un<br>02. Deux<br>03. Trois<br>04. Quatre<br>05. Cinq<br>06. Six<br>07. Sept<br>08. Huit<br>09. Neuf<br>10. Dix ou plus                                                                  |

<!-- p. 105 -->

| 6C               | Si au moins un autre emploi (NBEMP (TRA29) # 00)                                                                                             | |--|--| | PROFDEB<br>TRA30 | Parlons alors de cet emploi / du premier de ces emplois que vous avez<br>occupé un an ou plus.<br>Quelle était votre profession principale ? | | Car 4            |                                                                                                                                              |

CALCULEE : Déclaration en clair (PROFESSAN) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 6C              | Si au moins un autre emploi (NBEMP (TRA29) # 00)                                                                                             | |--|--| | CSEDEB<br>TRA30 | Parlons alors de cet emploi / du premier de ces emplois que vous avez<br>occupé un an ou plus.<br>Quelle était votre profession principale ? | | Car 2           |                                                                                                                                              |

CALCULEE : Déclaration en clair (PROFESSAN) => traitement SICORE (embarqué puis en bureau) et reprise manuelle CT

| 6C               | Si au moins un autre emploi (NBEMP (TRA29) # 00)                                                                                                                                                           | |--|--| | CSERDEB<br>TRA30 | Parlons alors de cet emploi / du premier de ces emplois que vous avez<br>occupé un an ou plus.<br>Quelle était votre profession principale ?<br>Catégorie socioprofessionnelle, niveau regroupé (PCS 2003) | | Car 4            |                                                                                                                                                                                                            |

CALCULEE : Déclaration en clair (PROFESSAN) => traitement SICORE (embarqué puis en bureau) et reprise manuelle

| 6C               | Si au moins un autre emploi (NBEMP (TRA29) # 00)                                                                                                                                                                                                                 | |--|--| | STATUTA<br>TRA31 | Dans cet emploi, étiez-vous ?                                                                                                                                                                                                                                    | | Car 2            | 1. A votre compte (artisan, commerçant, agriculteur, profession libérale)<br>2. Vous aidiez un membre de votre famille dans son travail sans être salarié<br>3. Salarié de l'État ou des collectivités locales<br>4. Autre salarié<br>9. Ne sait pas<br>8. REFUS |

CT

| 6C              | Si l'enquêté est entrant et a terminé ses études initiales                                                                                                                                                                      | |--|--| | DEMPRO<br>TRA32 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>a) … un déménagement pour raison professionnelle (par<br>exemple pour poursuivre une activité professionnelle ou<br>trouver un emploi) ? | | Car 1           | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                                                          |

<!-- p. 106 -->

| 6C              | Si l'enquêté est entrant et a terminé ses études initiales                                                                                                                                                                              | |--|--| | DEMENA<br>TRA33 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>b) … un déménagement ayant eu des conséquences sur votre<br>activité professionnelle (par exemple, en cas de mutation de<br>votre conjoint(e)) ? | | Car 1           | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                                                                  |

CT

| 6C                | Si l'individu est ENTRANT                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | |--|--| | STATUANT<br>TRA34 | Au 1er décembre 2012, étiez-vous ?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | | Car 2             | 1. Salarié de l'État (ministères, établissements publics administratifs (EPA) nationaux,<br>établissements publics d'enseignement,)<br>2. Salarié d'une collectivité territoriale (régions, départements, communes, OPHLM,<br>établissements publics administratifs (EPA) des collectivités territoriales,)<br>3. Salarié d'un hôpital public<br>4. Salarié d'un établissement de santé privé (à but lucratif ou non lucratif)<br>5. Salarié du secteur public social et médico-social (établissement d'hébergement pour<br>personnes âgées, établissements sociaux ou médico-sociaux)<br>6. Salarié d'une entreprise, d'un artisan, d'une association (hors établissement de santé<br>privé, hors secteur public social et médico-social)<br>7. Salarié d'un ou plusieurs particuliers<br>8. Vous aidez un membre de votre famille dans son travail sans être rémunéré<br>9. Chef d'entreprise salarié, PDG, gérant minoritaire, associé<br>10. Indépendant ou à votre compte<br>11. Etudiant<br>12. Chîomeur<br>13. Autre situation |

RPS

| 6C                 | Si l'individu est ENTRANT et s'il est né avant 2000 ou est arrivé avant 2015                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | |--|--| | STATUANTB<br>TRA34 | Au 1er décembre 2015, étiez-vous ?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | | Car 2              | 1. Salarié de l'État (ministères, établissements publics administratifs (EPA) nationaux,<br>établissements publics d'enseignement,)<br>2. Salarié d'une collectivité territoriale (régions, départements, communes, OPHLM,<br>établissements publics administratifs (EPA) des collectivités territoriales,)<br>3. Salarié d'un hôpital public<br>4. Salarié d'un établissement de santé privé (à but lucratif ou non lucratif)<br>5. Salarié du secteur public social et médico-social (établissement d'hébergement pour<br>personnes âgées, établissements sociaux ou médico-sociaux)<br>6. Salarié d'une entreprise, d'un artisan, d'une association (hors établissement de santé<br>privé, hors secteur public social et médico-social)<br>7. Salarié d'un ou plusieurs particuliers<br>8. Vous aidez un membre de votre famille dans son travail sans être rémunéré<br>9. Chef d'entreprise salarié, PDG, gérant minoritaire, associé<br>10. Indépendant ou à votre compte<br>11. Etudiant<br>12. Chîomeur<br>13. Autre situation |

<!-- p. 107 -->

### D1 - Données antérieures sur les trajectoires professionnelles entre 2013 et 2016 - Actifs occupés du PANEL

| 6C       | Si l'individu est panel et répondant en 2013 et 2016                                                                                 | |--|--| | TCHOL_13 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>a) … une période de chômage d'un an ou plus ? | | Car 1    | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                  |

Données 2013 CT

| 6C        | Si l'individu est panel et répondant entrant en 2016                                                                                 | |--|--| | TCHOLE_16 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>a) … une période de chômage d'un an ou plus ? | | Car 1     | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                  |

Données 2016 CT

| 6D1       | Si l'individu est panel et répondant en 2013 et 2016                                                                              | |--|--| | TCHOLP_16 | Depuis la précédente enquête [2013] avez-vous vécu les situations<br>suivantes ?<br>a) … une période de chômage d'un an ou plus ? | | Car 1     | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                               |

Données 2016 CT 2013

| 6D1      | Si l'individu est panel, répondant en 2013 et 2016 et si ACTOCCUP_16=0                                                    | |--|--| | TCHOL_16 | Depuis la fin de votre emploi, avez-vous vécu les situations suivantes …<br>a) … une période de chômage d'un an ou plus ? | | Car 1    | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                       |

Données 2016 CT 2013

<!-- p. 108 -->

| 6C       | Si l'individu est panel et répondant en 2013                                                                                                                     | |--|--| | TCHOC_13 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>b) … une période de chômage de plus de trois mois mais<br>moins d'un an ? | | Car 1    | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                              |

| 6C        | Si l'individu est panel et répondant en 2016                                                                                                                     | |--|--| | TCHOCE_16 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>b) … une période de chômage de plus de trois mois mais<br>moins d'un an ? | | Car 1     | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                              |

Données 2016 CT

| 6D1       | Si l'individu est panel et répondant en 2013 et 2016                                                                               | |--|--| | TCHOCP_16 | Depuis 2013 avez-vous vécu les situations suivantes ?<br>b) … une période de chômage de plus de trois mois mais<br>moins d'un an ? | | Car 1     | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                |

Données 2016 CT 2013

| 6D1      | Si l'individu est panel, répondant en 2013 et 2016 et si ACTOCCUP_16=0                                                                                | |--|--| | TCHOC_16 | Depuis la fin de votre emploi, avez-vous vécu les situations suivantes …<br>b) … une période de chômage de plus de trois mois mais<br>moins d'un an ? | | Car 1    | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                   |

Données 2016 CT 2013

<!-- p. 109 -->

| 6D1     | Si l'individu est panel et répondant en 2013                                                                                                                                        | |--|--| | TSAN_13 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>c) … vous avez arrêté de travailler pendant un an ou plus à<br>cause de problèmes de santé ? | | Car 1   | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                 |

| 6D1     | Si l'individu est panel et répondant en 2016                                                                                                                                        | |--|--| | TSAN_16 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>c) … vous avez arrêté de travailler pendant un an ou plus à<br>cause de problèmes de santé ? | | Car 1   | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                 |

Données 2016 CT

| 6D1      | Si l'individu est panel et répondant en 2013 et 2016                                                                                                  | |--|--| | TSANP_16 | Depuis 2013 avez-vous vécu les situations suivantes ?<br>c) … vous avez arrêté de travailler pendant un an ou plus à<br>cause de problèmes de santé ? | | Car 1    | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                   |

Données 2016 CT 2013

| 6D1     | Si l'individu est panel et répondant en 2013                                                                                                                                                                                                               | |--|--| | TINA_13 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>d) … vous avez arrêté de travailler pendant un an ou plus pour<br>une autre raison, par exemple s'occuper de votre foyer, de vos<br>enfants, reprendre des études ? | | Car 1   | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                        |

Données 2013

<!-- p. 110 -->

| 6D1      | Si l'individu est panel, répondant entrant en 2016                                                                                                                                                                                                         | |--|--| | TINAE_16 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>d) … vous avez arrêté de travailler pendant un an ou plus pour<br>une autre raison, par exemple s'occuper de votre foyer, de vos<br>enfants, reprendre des études ? | | Car 1    | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                        |

| 6D1      | Si l'individu est panel et répondant en 2013 et 2016                                                                                                                                                                                                    | |--|--| | TINAP_16 | Depuis la précédente enquête [2013] avez-vous vécu les situations<br>suivantes ?<br>d) … vous avez arrêté de travailler pendant un an ou plus pour<br>une autre raison, par exemple s'occuper de votre foyer, de vos<br>enfants, reprendre des études ? | | Car 1    | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                     |

Données 2016 CT 2013

| 6D1     | Si l'individu est panel et répondant en 2016 et si ACTOCCUP_16=0                                                                                                                                                                                | |--|--| | TINA_16 | Depuis la fin de votre emploi, avez-vous vécu les situations suivantes …<br>d) … Avez-vous arrêté de travailler pendant un an ou plus<br>pour une autre raison, par exemple s'occuper de votre foyer,<br>de vos enfants, reprendre des études ? | | Car 1   | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                             |

Données 2016 CT

| 6D1       | Si l'individu est panel et entrant en 2013                                                                                                                         | |--|--| | TINDEP_13 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>e) … vous avez travaillé à votre compte, employant ou non<br>des salariés ? | | Car 1     | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                            |

Données 2013 CT

<!-- p. 111 -->

| 6D1        | Si l'individu est panel entrant en 2016                                                                                                                            | |--|--| | TINDEPE_16 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>e) … vous avez travaillé à votre compte, employant ou non<br>des salariés ? | | Car 1      | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                            |

| 6D1       | Si l'individu est panel entrant en 2013 et si ACTOCCUP_16=0                                                                                             | |--|--| | TINDEP_16 | Depuis la fin de votre emploi, avez-vous vécu les situations suivantes ?<br>e) … vous avez travaillé à votre compte, employant ou non<br>des salariés ? | | Car 1     | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                 |

Données 2016 CT

| 6D1        | Si l'individu est panel et répondant en 2013 et 2016                                                                                 | |--|--| | TINDEPP_16 | Depuis 2013 avez-vous vécu les situations suivantes ?<br>e) … vous avez travaillé à votre compte, employant ou non<br>des salariés ? | | Car 1      | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                              |

Données 2016 CT 2013

| 6D1         | Si l'individu est panel et entrant en 2013                                                  | |--|--| | TINTERIM_13 | Depuis la fin de vos études, vous est-il arrivé …<br>a) … de travailler comme intérimaire ? | | Car 1       | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                     |

Données 2013 CT

| 6D1          | Si l'individu est panel et répondant en 2013      | |--|--| | TINTERIME_16 | Depuis la fin de vos études, vous est-il arrivé … | |              | a) … de travailler comme intérimaire ?            | | Car 1        | 1. Toujours ou presque                            | |              | 2. Parfois                                        | |              | 3. Jamais                                         | |              | 9. NSP                                            | |              | 8. REFUS                                          |

Données 2016

<!-- p. 112 -->

| 6D1          | Si l'individu est panel et répondant en 2013 et 2016                                            | |--|--| | TINTERIMP_16 | Depuis 2013 avez-vous vécu les situations suivantes ?<br>a) … de travailler comme intérimaire ? | | Car 1        | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                         |

| 6D1         | Si l'individu est panel répondant en 2013 et 2016 et si ACTOCCUP_16=0   | |--|--| | TINTERIM_16 | Depuis la fin de votre emploi avez-vous vécu les situations suivantes ? | |             | a) … de travailler comme intérimaire ?                                  | | Car 1       | 1. Toujours ou presque                                                  | |             | 2. Parfois                                                              | |             | 3. Jamais                                                               | |             | 9. NSP                                                                  | |             | 8. REFUS                                                                |

Données 2016 CT 2013

| 6D1     | Si l'individu est panel et répondant en 2013                                                                                        | |--|--| | TCDD_13 | Depuis la fin de vos études, vous est-il arrivé …<br>b) … de travailler avec un contrat à durée déterminée ou un<br>contrat aidé, ? | | Car 1   | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                             |

Données 2013 CT

| 6D1      | Si l'individu est panel entrant en 2016                                                                                             | |--|--| | TCDDE_16 | Depuis la fin de vos études, vous est-il arrivé …<br>b) … de travailler avec un contrat à durée déterminée ou un<br>contrat aidé, ? | | Car 1    | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                             |

Données 2016 CT

| 6D1      | Si l'individu est panel et répondant en 2013 et 2016                                                                                    | |--|--| | TCDDP_16 | Depuis 2013 avez-vous vécu les situations suivantes ?<br>b) … de travailler avec un contrat à durée déterminée ou un<br>contrat aidé, ? | | Car 1    | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                 |

Données 2016 CT 2013

<!-- p. 113 -->

| 6D1     | Si l'individu est panel répondant en 2013 et 2016 et si ACTOCCUP_16=0                                                                                      | |--|--| | TCDD_16 | Depuis la fin de votre emploi, avez-vous vécu les situations suivantes ?<br>b) … de travailler avec un contrat à durée déterminée ou un<br>contrat aidé, ? | | Car 1   | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                    |

| 6D1      | Si l'individu est panel et répondant en 2013 et 2016                                                                                                                                                      | |--|--| | NBEMP_13 | En dehors de votre emploi actuel et depuis que vous avez<br>terminé vos études, combien d'emplois de PLUS D'UN AN<br>dans des entreprises différentes ou pour des employeurs<br>différents avez-vous eu ? | | Car 2    | 00. Aucun<br>01. Un<br>02. Deux<br>03. Trois<br>04. Quatre<br>05. Cinq<br>06. Six<br>07. Sept<br>08. Huit<br>09. Neuf<br>10. Dix ou plus                                                                  |

Données 2013 CT

| 6D1       | Si l'individu est panel et répondant entrant en 2016                                                                                                                                                      | |--|--| | NBEMPE_16 | En dehors de votre emploi actuel et depuis que vous avez<br>terminé vos études, combien d'emplois de PLUS D'UN AN<br>dans des entreprises différentes ou pour des employeurs<br>différents avez-vous eu ? | | Car 2     | 00. Aucun<br>01. Un<br>02. Deux<br>03. Trois<br>04. Quatre<br>05. Cinq<br>06. Six<br>07. Sept<br>08. Huit<br>09. Neuf<br>10. Dix ou plus                                                                  |

Données 2016

<!-- p. 114 -->

| 6D1       | Si l'individu est panel et répondant en 2013 et 2016 et ACTOCCUP_16=1                                                                                                                      | |--|--| | NBEMPP_16 | En dehors de votre emploi actuel et depuis "V1MENQ"<br>V1ANENQ", combien d'emplois de PLUS D'UN AN dans des<br>entreprises différentes ou pour des employeurs différents<br>avez-vous eu ? | | Car 1     | 00. Aucun<br>01. Un<br>02. Deux<br>03. Trois<br>04. Quatre<br>05. Cinq<br>06. Six<br>07. Sept<br>08. Huit<br>09. Neuf<br>10. Dix ou plus                                                   |

| 6D1      | Si l'individu est panel, répondant en 2013 et 2016 et si ACTOCCUP_16=0                                                                                                 | |--|--| | NBEMP_16 | Depuis "DATARMOI, DATARAN", / depuis la fin de votre<br>emploi, combien d'emplois dans des entreprises différentes<br>ou pour des employeurs différents avez-vous eu ? | | Car 1    | 00. Aucun<br>01. Un<br>02. Deux<br>03. Trois<br>04. Quatre<br>05. Cinq<br>06. Six<br>07. Sept<br>08. Huit<br>09. Neuf<br>10. Dix ou plus                               |

Données 2016 CT-RPS 2016

| 6D1       | Si l'individu est panel entrant en 2013                                                                                                                                                                                         | |--|--| | DEMPRO_13 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>a) … un déménagement pour raison professionnelle (par<br>exemple pour poursuivre une activité professionnelle ou<br>trouver un emploi) ? | | Car 1     | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                                                          |

Données 2013 CT

<!-- p. 115 -->

| 6D1       | Si l'individu est panel enrant en 2016                                                                                                                                                                                          | |--|--| | DEMPRO_16 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>a) … un déménagement pour raison professionnelle (par<br>exemple pour poursuivre une activité professionnelle ou<br>trouver un emploi) ? | | Car 1     | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                                                          |

| 6D1       | Si l'individu est panel et répondant en 2013                                                                                                                                                                                            | |--|--| | DEMENA_13 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>b) … un déménagement ayant eu des conséquences sur votre<br>activité professionnelle (par exemple, en cas de mutation de<br>votre conjoint(e)) ? | | Car 1     | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                                                                  |

Données 2013 CT

| 6D1       | Si l'individu est panel et répondant en 2016                                                                                                                                                                                            | |--|--| | DEMENA_16 | Depuis la fin de vos études initiales, avez-vous vécu les situations<br>suivantes …<br>b) … un déménagement ayant eu des conséquences sur votre<br>activité professionnelle (par exemple, en cas de mutation de<br>votre conjoint(e)) ? | | Car 1     | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                                                                                                                                                  |

Données 2016 CT

| 6D1        | Si l'individu est ACTIF-OCCUPE PANEL et SI ACTOCCUP_16 =0                                                                               | |--|--| | DATARAN_16 | En « V1MENQ » « V1AENQ », vous étiez « RPSPROFESSCI » dans<br>l'établissement « RPSEMPLXCI ». Quand s'est arrêté cet emploi ?<br>Année… | | Car 1      |                                                                                                                                         |

Données 2016

| 6D1          | Si l'individu est ACTIF-OCCUPE PANEL et SI ACTOCCUP_16 =1 | |--|--| | DATARAN_DRAP | Variable "drapeau" de DATARAN_DRAP                        | | TRAI1        |                                                           | | Num - 8      | 1. Réponse                                                | |              | 0. Sans objet                                             | |              | -1 . Ne sait pas                                          | |              | -2 . Refuse de répondre                                   |

Variable drapeau Données 2016

<!-- p. 116 -->

| 6D1         | Si l'individu est ACTIF-OCCUPE PANEL et SI ACTOCCUP_16 =2 | |--|--| | DATARMOI_16 | Mois…                                                     | | TRAI1A      |                                                           | | Car 2       | De 01 à 12                                                | |             | 9. NSP                                                    | |             | 8. REFUS                                                  |

Données 2016

| 6D1                     | Si l'individu est ACTIF-OCCUPE PANEL et SI ACTOCCUP_16 =3                  | |--|--| | DATARMOI_DRAP<br>TRAI1A | Variable "drapeau" de DATARMOI_DRAP                                        | | Num - 8                 | 1. Réponse<br>0. Sans objet<br>-1 . Ne sait pas<br>-2 . Refuse de répondre |

Variable drapeau Données 2016

# D - Trajectoire professionnelle depuis 2015 - Actifs occupés du PANEL

| 6D              | Si l'individu est ACTIF-OCCUPE PANEL                                                                                                                                                                                         | |--|--| | TCHOLP<br>TRA35 | SI ACTOCCUP_16 = 1 : Depuis la précédente enquête [2016] avez-vous<br>vécu les situations suivantes ?<br>Si ACTOCCUP_16=0 : Depuis la fin de votre emploi déclaré dans CT<br>2013, avez-vous vécu les situations suivantes ? | | Car 1           | a) … une période de chômage d'un an ou plus ?<br>1. Une fois                                                                                                                                                                 | |                 | 2. Plusieurs fois<br>3. Jamais<br>9. NSP                                                                                                                                                                                     | |                 | 8. REFUS                                                                                                                                                                                                                     |



| 6D              | Si l'individu est ACTIF-OCCUPE PANEL                                                                                                                                                                                                                                                                      | |--|--| | TCHOCP<br>TRA36 | SI ACTOCCUP_16 = 1 : Depuis la précédente enquête [2016] avez-vous<br>vécu les situations suivantes ?<br>Si ACTOCCUP_16=0 : Depuis la fin de votre emploi déclaré dans CT<br>2013, avez-vous vécu les situations suivantes ?<br>b) … une période de chômage de plus de trois mois mais<br>moins d'un an ? | | Car 1           | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                                                                       |



<!-- p. 117 -->

| 6D             | Si l'individu est ACTIF-OCCUPE PANEL                                                                                                                                                                                                                                                                                         | |--|--| | TSANP<br>TRA37 | SI ACTOCCUP_16 = 1 : Depuis la précédente enquête [2016] avez-vous<br>vécu les situations suivantes ?<br>Si ACTOCCUP_16=0 : Depuis la fin de votre emploi déclaré dans CT<br>2013, avez-vous vécu les situations suivantes ?<br>c) … vous avez arrêté de travailler pendant un an ou plus à<br>cause de problèmes de santé ? | | Car 1          | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                                                                                          |



| 6D             | Si l'individu est ACTIF-OCCUPE PANEL                                                                                                                                                                                                                                                                                                                                                                | |--|--| | TINAP<br>TRA38 | SI ACTOCCUP_16 = 1 : Depuis la précédente enquête [2016] avez-vous<br>vécu les situations suivantes ?<br>Si ACTOCCUP_16=0 : Depuis la fin de votre emploi déclaré dans CT<br>2013, avez-vous vécu les situations suivantes ?<br>d) … vous avez arrêté de travailler pendant un an ou plus pour<br>une autre raison, par exemple s'occuper de votre foyer, de vos<br>enfants, reprendre des études ? | | Car 1          | 1. Une fois<br>2. Plusieurs fois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                                                                                                                                                                 |



| 6D               | Si l'individu est ACTIF-OCCUPE PANEL                                                                                                                                                                                                                                                                        | |--|--| | TINDEPP<br>TRA39 | SI ACTOCCUP_16 = 1 : Depuis la précédente enquête [2016] avez-vous<br>vécu les situations suivantes ?<br>Si ACTOCCUP_16=0 : Depuis la fin de votre emploi déclaré dans CT<br>2013, avez-vous vécu les situations suivantes ?<br>e) … vous avez travaillé à votre compte, employant ou non<br>des salariés ? | | Car 1            | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                                                                     |



| 6D                 | Si l'individu est ACTIF-OCCUPE PANEL                                                                                                                                                                                                                                   | |--|--| | TINTERIMP<br>TRA40 | SI ACTOCCUP_16 = 1 : Depuis la précédente enquête [2016] avez-vous<br>vécu les situations suivantes ?<br>Si ACTOCCUP_16=0 : Depuis la fin de votre emploi déclaré dans CT<br>2013, avez-vous vécu les situations suivantes ?<br>a) … de travailler comme intérimaire ? | | Car 1              | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                                |



<!-- p. 118 -->

| 6D             | Si l'individu est ACTIF-OCCUPE PANEL                                                                                                                                                                                                                                                                           | |--|--| | TCDDP<br>TRA41 | SI ACTOCCUP_16 = 1 : Depuis la précédente enquête [2016] avez-vous<br>vécu les situations suivantes ?<br>Si ACTOCCUP_16=0 : Depuis la fin de votre emploi déclaré dans CT<br>2013, avez-vous vécu les situations suivantes ?<br>b) … de travailler avec un contrat à durée déterminée ou un<br>contrat aidé, ? | | Car 1          | 1. Toujours ou presque<br>2. Parfois<br>3. Jamais<br>9. NSP<br>8. REFUS                                                                                                                                                                                                                                        |



| 6D              | Si l'individu est ACTIF-OCCUPE PANEL                                                                                                                                                       | |--|--| | NBEMPP<br>TRA42 | En dehors de votre emploi actuel et depuis "V1MENQ"<br>V1ANENQ", combien d'emplois de PLUS D'UN AN dans des<br>entreprises différentes ou pour des employeurs différents<br>avez-vous eu ? | | Car 1           | 00. Aucun<br>01. Un<br>02. Deux<br>03. Trois<br>04. Quatre<br>05. Cinq<br>06. Six<br>07. Sept<br>08. Huit<br>09. Neuf<br>10. Dix ou plus                                                   |



#### E - NIR

| 6E             | Si l'individu a plus de 18 ans                                                                  | |--|--| | ACCNIR<br>NIR1 | Pouvez-vous m'indiquer le N° de sécurité sociale que vous<br>utilisez pour vos remboursements ? | | Car 1          | 1. Oui<br>2. Non, je ne le souhaite pas                                                         | |                | 3. Non, je ne le connais pas                                                                    |

CT-RPS 2016

<!-- p. 119 -->

## 7. Auto-questionnaire

#### 0. Introduction

| 7I    |                                                                 | |--|--| | LANG1 | La personne enquêtée est-elle en mesure de comprendre et        | | QAA1  | de répondre seule au questionnaire sous casque en Français<br>? | | Car 1 | 1. Oui<br>2. Non<br>Non réponse                                 |

CT

| 7I            |                                                                                              | |--|--| | LANG2<br>QAA2 | La personne enquêtée comprend-elle une de ces langues ?                                      | | Car 1         | 1. L'arabe<br>2. Le turc<br>3. L'anglais<br>4. Le portugais<br>5. Une autre langue<br>9. NSP | |               | Non réponse                                                                                  |

CT

| 7I    | Si enquête en Guyane et aucune des langues n'est comprise (RGES="03" et LANG2<br>(QAA2) =5) | |--|--| | LANG4 | Le remplissage du questionnaire avec l'aide de l'enquêteur                                  | | QAA3b | est…                                                                                        | | Car 1 | 1 accepté<br>2 refusé<br>3 impossible à réaliser<br>4 abandonné                             |

CT

| 7I     |                                                                 | |--|--| | REPQAA | Le questionnaire est …                                          | | QAA4   |                                                                 | | Car 1  | 1 accepté<br>2 refusé<br>3 impossible à réaliser<br>4 abandonné |

<!-- p. 120 -->

#### A - Vie personnelle

| 7A         |                                                                                                                                    | |--|--| | RP1<br>RP1 | Y a-t-il quelqu'un sur qui vous pouvez compter pour discuter<br>de choses personnelles ou pour prendre une décision difficile<br>? | | Car 1      | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS<br>Non réponse                                                                              |

CT-RPS 2016

| 7A           |                                                      | |--|--| | RP2          | Auriez-vous besoin d'être davantage aidé pour cela ? | | RP2<br>Car 1 | 1. Oui<br>2. Non                                     | |              | 9. NSP<br>8. REFUS                                   | |              | Non réponse                                          |

CT-RPS 2016

| 7A           | Si l'individu est entrant                                                                                                                                                                         | |--|--| | RP3a<br>RP3a | De votre naissance à vos 18 ans, avez-vous été marqué par l'un des<br>évènements suivants ?<br>Un handicap (de naissance ou suite à un accident ou un<br>problème de santé), ou une maladie grave | | Car 1        | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS<br>Non réponse                                                                                                                                             |

CT-RPS 2016

| 7A           |                                                                                                                                                                                                   | |--|--| | RP3b<br>RP3b | De votre naissance à vos 18 ans, avez-vous été marqué par l'un des<br>évènements suivants ?<br>De graves problèmes de santé de l'un de vos proches ou le<br>décès d'un proche (père, mère, autre) | | Car 1        | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS<br>Non réponse                                                                                                                                             |

CT-RPS 2016

| 7A           |                                                                                                            | |--|--| | RP3c<br>RP3c | De votre naissance à vos 18 ans, avez-vous été marqué par l'un des<br>évènements suivants ?                | |              | Vous avez été fortement affecté(e) par des conflits familiaux<br>vous concernant ou concernant vos proches | | Car 1        | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS                                                                     | |              | Non réponse                                                                                                |

CT-RPS 2016

<!-- p. 121 -->

| 7A           |                                                                                                                                                                                     | |--|--| | RP3d<br>RP3d | De votre naissance à vos 18 ans, avez-vous été marqué par l'un des<br>évènements suivants ?<br>Vous<br>avez<br>subi<br>de<br>mauvais<br>traitements<br>physiques,<br>psychologiques | | Car 1        | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS<br>Non réponse                                                                                                                               |

CT-RPS 2016

| 7A    |                                                                    | |--|--| | RP3e  | De votre naissance à vos 18 ans, avez-vous été marqué par l'un des | | RP3e  | évènements suivants ?                                              | |       | Vous avez vécu des conflits ou subi des violences répétées,        | |       | à l'école ou dans votre voisinage (racket, etc.)                   | | Car 1 | 1. Oui                                                             | |       | 2. Non                                                             | |       | 9. NSP                                                             | |       | 8. REFUS                                                           | |       | Non réponse                                                        |

CT-RPS 2016

| 7A           |                                                                                        | |--|--| | RP4a<br>RP4a | Et au cours des trois dernières années, l'un de ces évènements vous a<br>t-il marqué ? | |              | Un handicap (suite à un accident ou un problème de santé) ou<br>une maladie grave      | | Car 1        | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS<br>Non réponse                                  |

Si panel, données 2016 CT-RPS 2016

| 7A           |                                                                                                    | |--|--| | RP4b<br>RP4b | Et au cours des trois dernières années, l'un de ces évènements vous a<br>t-il marqué ?             | |              | De graves problèmes de santé de l'un de vos proches ou le<br>décès d'un proche (père, mère, autre) | | Car 1        | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS<br>Non réponse                                              |

Si panel, données 2016 CT-RPS 2016

<!-- p. 122 -->

| 7A           |                                                                                                                                                         | |--|--| | RP4c<br>RP4c | Et au cours des trois dernières années, l'un de ces évènements vous a<br>t-il marqué ?<br>Vous avez été fortement affecté(e) par des conflits familiaux | |              | vous concernant ou concernant vos proches                                                                                                               | | Car 1        | 1. Oui<br>2. Non<br>9. NSP                                                                                                                              | |              | 8. REFUS<br>Non réponse                                                                                                                                 |

Si panel, données 2016 CT-RPS 2016

| 7A           |                                                                                                                                                                                | |--|--| | RP4d<br>RP4d | Et au cours des trois dernières années, l'un de ces évènements vous a<br>t-il marqué ?<br>Vous<br>avez<br>subi<br>de<br>mauvais<br>traitements<br>physiques,<br>psychologiques | | Car 1        | 1. Oui<br>2. Non<br>9. NSP<br>8. REFUS<br>Non réponse                                                                                                                          |

Si panel, données 2016 CT-RPS 2016

| 7A             |                                                                                                                                                                          | |--|--| | RPC1A<br>RPC1A | A quelle fréquence avez-vous ressenti les affirmations suivantes, au<br>cours des deux dernières semaines dans votre vie de tous les jours, au<br>travail et en dehors ? | |                | a) Je me suis senti(e) bien et de bonne humeur                                                                                                                           | | Car 1          | 1. Tout le temps,<br>2. La plupart du temps,<br>3. Plus de la moitié du temps,<br>4. Moins de la moitié du temps,<br>5. De temps en temps,                               | |                | 6. Jamais,<br>9. NSP<br>8. REFUS                                                                                                                                         | |                | Non réponse                                                                                                                                                              |

CT

| 7A             |                                                                                                                                                                                               | |--|--| | RPC1B<br>RPC1B | A quelle fréquence avez-vous ressenti les affirmations suivantes, au<br>cours des deux dernières semaines dans votre vie de tous les jours, au<br>travail et en dehors ?                      | |                | b) Je me suis senti(e) calme et tranquille                                                                                                                                                    | | Car 1          | 1. Tout le temps,<br>2. La plupart du temps,<br>3. Plus de la moitié du temps,<br>4. Moins de la moitié du temps,<br>5. De temps en temps,<br>6. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse |

<!-- p. 123 -->

| 7A             |                                                                                                                                                                                               | |--|--| | RPC1C<br>RPC1C | A quelle fréquence avez-vous ressenti les affirmations suivantes, au<br>cours des deux dernières semaines dans votre vie de tous les jours, au<br>travail et en dehors ?                      | |                | c) Je me suis senti(e) plein(e) d'énergie et vigoureux(se)                                                                                                                                    | | Car 1          | 1. Tout le temps,<br>2. La plupart du temps,<br>3. Plus de la moitié du temps,<br>4. Moins de la moitié du temps,<br>5. De temps en temps,<br>6. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse |

| 7A             |                                                                                                                                                                                                                                             | |--|--| | RPC1D<br>RPC1D | A quelle fréquence avez-vous ressenti les affirmations suivantes, au<br>cours des deux dernières semaines dans votre vie de tous les jours, au<br>travail et en dehors ?<br>d) Je me suis réveillé(e) en me sentant frais(che) et dispos(e) | | Car 1          | 1. Tout le temps,<br>2. La plupart du temps,<br>3. Plus de la moitié du temps,<br>4. Moins de la moitié du temps,<br>5. De temps en temps,<br>6. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse                                               |

CT

| 7A             |                                                                                                                                                                                                                                         | |--|--| | RPC1E<br>RPC1E | A quelle fréquence avez-vous ressenti les affirmations suivantes, au<br>cours des deux dernières semaines dans votre vie de tous les jours, au<br>travail et en dehors ?<br>e) Ma vie quotidienne a été remplie de choses intéressantes | | Car 1          | 1. Tout le temps,<br>2. La plupart du temps,<br>3. Plus de la moitié du temps,<br>4. Moins de la moitié du temps,<br>5. De temps en temps,<br>6. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse                                           |

<!-- p. 124 -->

## C - Relations avec les autres au travail

| 7C             |                                                                                                                                                                                 | |--|--| | RPA1A<br>RPA1A | Pour chacune des affirmations suivantes, êtes-vous pas du tout<br>d'accord,pas d'accord, d'accord ou tout à fait d'accord ?<br>a) Mon supérieur prête attention à ce que je dis | | Car 1          | 1. Pas du tout d'accord,<br>2. Pas d'accord,<br>3. D'accord,<br>4. Tout à fait d'accord,<br>5. Non concerné, pas de supérieur,<br>9. NSP<br>8. REFUS<br>Non réponse             |

CT

| 7C    |                                                                                                              | |--|--| | RPA1C | Pour chacune des affirmations suivantes, êtes-vous pas du tout                                               | | RPA1C | d'accord,pas d'accord, d'accord ou tout à fait d'accord ?                                                    | |       | c) Les personnes qui évaluent mon travail le connaissent bien                                                | | Car 1 | 1. Pas du tout d'accord,<br>2. Pas d'accord,<br>3. D'accord,<br>4. Tout à fait d'accord,<br>5. Non concerné, | |       | 9. NSP<br>8. REFUS                                                                                           | |       | Non réponse                                                                                                  |

CT

| 7C             |                                                                                                                                                                                                                   | |--|--| | RPA1G<br>RPA1G | Pour chacune des affirmations suivantes, êtes-vous pas du tout<br>d'accord,pas d'accord, d'accord ou tout à fait d'accord ?<br>g) Vu tous mes efforts, je reçois le respect et l'estime que<br>mérite mon travail | | Car 1          | 1. Pas du tout d'accord,<br>2. Pas d'accord,<br>3. D'accord,<br>4. Tout à fait d'accord,<br>5. Non concerné,<br>9. NSP<br>8. REFUS<br>Non réponse                                                                 |

<!-- p. 125 -->

| 7C             |                                                                                                                                                                                                             | |--|--| | RPA1H<br>RPA1H | Pour chacune des affirmations suivantes, êtes-vous pas du tout<br>d'accord,pas d'accord, d'accord ou tout à fait d'accord ?<br>h) Vu tous mes efforts, mes perspectives de promotion sont<br>satisfaisantes | | Car 1          | 1. Pas du tout d'accord,<br>2. Pas d'accord,<br>3. D'accord,<br>4. Tout à fait d'accord,<br>5. Non concerné,<br>9. NSP<br>8. REFUS<br>Non réponse                                                           |

| 7C             |                                                                                                                                                                                                                          | |--|--| | RPA1I<br>RPA1I | Pour chacune des affirmations suivantes, êtes-vous pas du tout<br>d'accord,pas d'accord, d'accord ou tout à fait d'accord ?<br>i)<br>On<br>me<br>demande<br>d'effectuer<br>une<br>quantité<br>de<br>travail<br>excessive | | Car 1          | 1. Pas du tout d'accord,<br>2. Pas d'accord,<br>3. D'accord,<br>4. Tout à fait d'accord,<br>5. Non concerné,<br>9. NSP<br>8. REFUS<br>Non réponse                                                                        |

CT

| 7C             |                                                                                                                                                                                                           | |--|--| | RPA1J<br>RPA1J | Pour chacune des affirmations suivantes, êtes-vous pas du tout<br>d'accord,pas d'accord, d'accord ou tout à fait d'accord ?<br>j) Je peux organiser mon travail de la manière qui me convient<br>le mieux | | Car 1          | 1. Pas du tout d'accord,<br>2. Pas d'accord,<br>3. D'accord,<br>4. Tout à fait d'accord,<br>5. Non concerné,<br>9. NSP<br>8. REFUS<br>Non réponse                                                         |

CT

| 7C             |                                                                                                                                                                                                                       | |--|--| | RPA1L<br>RPA1L | Pour chacune des affirmations suivantes, êtes-vous pas du tout<br>d'accord,pas d'accord, d'accord ou tout à fait d'accord ?<br>l)<br>J'ai<br>l'occasion<br>de<br>développer<br>mes<br>compétences<br>professionnelles | | Car 1          | 1. Pas du tout d'accord,<br>2. Pas d'accord,<br>3. D'accord,<br>4. Tout à fait d'accord,<br>5. Non concerné,<br>9. NSP<br>8. REFUS<br>Non réponse                                                                     |

<!-- p. 126 -->

| 7C             |                                                                                                                                                                                   | |--|--| | RPA1m<br>RPA1m | Pour chacune des affirmations suivantes, êtes-vous pas du tout<br>d'accord,pas d'accord, d'accord ou tout à fait d'accord ?<br>m) Je suis traité(e) de façon équitable au travail | | Car 1          | 1. Pas du tout d'accord,<br>2. Pas d'accord,<br>3. D'accord,<br>4. Tout à fait d'accord,<br>5. Non concerné,<br>9. NSP<br>8. REFUS<br>Non réponse                                 |

CT-RPS 2016

| 7C             |                                                                                               | |--|--| | RPA2A<br>RPA2A | A quelle fréquence vivez-vous chacune des situations suivantes dans<br>votre travail ?        | |                | a) Je travaille sous pression                                                                 | | Car 1          | 1. Toujours,<br>2. Souvent,<br>3. Parfois,<br>4. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse |

CT

| 7C             |                                                                                                                                                        | |--|--| | RPA2C<br>RPA2C | A quelle fréquence vivez-vous chacune des situations suivantes dans<br>votre travail ?<br>c) Je continue à penser à mon travail même quand je n'y suis | |                | pas                                                                                                                                                    | | Car 1          | 1. Toujours,                                                                                                                                           | |                | 2. Souvent,<br>3. Parfois,                                                                                                                             | |                | 4. Jamais,                                                                                                                                             | |                | 9. NSP                                                                                                                                                 | |                | 8. REFUS                                                                                                                                               | |                | Non réponse                                                                                                                                            |

CT

| 7C             |                                                                                                                                                   | |--|--| | RPA2F<br>RPA2F | A quelle fréquence vivez-vous chacune des situations suivantes dans<br>votre travail ?<br>f) Je vis des changements imprévisibles ou mal préparés | | Car 1          | 1. Toujours,<br>2. Souvent,<br>3. Parfois,<br>4. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse                                                     |

<!-- p. 127 -->

| 7C             |                                                                                                                                          | |--|--| | RPA2H<br>RPA2H | A quelle fréquence vivez-vous chacune des situations suivantes dans<br>votre travail ?<br>h) Je dois faire des choses que je désapprouve | | Car 1          | 1. Toujours,<br>2. Souvent,<br>3. Parfois,<br>4. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse                                            |

| 7C             |                                                                                               | |--|--| | RP22i<br>RP22i | A quelle fréquence vivez-vous chacune des situations suivantes dans<br>votre travail ?        | |                | i) Je ne peux pas faire du bon travail, je dois sacrifier la qualité                          | | Car 1          | 1. Toujours,<br>2. Souvent,<br>3. Parfois,<br>4. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse |

CT-RPS 2016

| 7C             |                                                                                                                                                                      | |--|--| | RPA2i<br>RPA2i | A quelle fréquence vivez-vous chacune des situations suivantes dans<br>votre travail ?<br>q) Je dois cacher mes émotions ou faire semblant d'être de<br>bonne humeur | | Car 1          | 1. Toujours,<br>2. Souvent,<br>3. Parfois,<br>4. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse                                                                        |



| 7C             |                                                                                                                                                                                    | |--|--| | RPA2K<br>RPA2K | A quelle fréquence vivez-vous chacune des situations suivantes dans<br>votre travail ?<br>s) Il m'arrive d'avoir peur pendant mon travail, pour ma<br>sécurité ou celle des autres | | Car 1          | 1. Toujours,<br>2. Souvent,<br>3. Parfois,<br>4. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse                                                                                      |

<!-- p. 128 -->

| 7C             |                                                                                                                                      | |--|--| | RPB5A<br>RPB5A | Dans votre travail, à quelle fréquence vous arrive-t-il d'éprouver les<br>sentiments suivants ?<br>a) La fierté du travail bien fait | | Car 1          | 1. Toujours,<br>2. Souvent,<br>3. Parfois,<br>4. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse                                        |

| 7C             |                                                                                                                                                              | |--|--| | RPB5B<br>RPB5B | Dans votre travail, à quelle fréquence vous arrive-t-il d'éprouver les<br>sentiments suivants ?<br>b) L'impression de faire quelque chose d'utile aux autres | |                |                                                                                                                                                              | | Car 1          | 1. Toujours,<br>2. Souvent,                                                                                                                                  | |                | 3. Parfois,                                                                                                                                                  | |                | 4. Jamais,                                                                                                                                                   | |                | 9. NSP                                                                                                                                                       | |                | 8. REFUS                                                                                                                                                     | |                | Non réponse                                                                                                                                                  |

CT

| 7C             |                                                                                                                                                                                                             | |--|--| | RPB5C<br>RPB5C | Dans votre travail, à quelle fréquence vous arrive-t-il d'éprouver les<br>sentiments suivants ?<br>c)<br>L'impression<br>que<br>mon<br>travail<br>a<br>des<br>conséquences<br>négatives sur l'environnement | | Car 1          | 1. Toujours,<br>2. Souvent,<br>3. Parfois,<br>4. Jamais,<br>9. NSP<br>8. REFUS<br>Non réponse                                                                                                               |



| 7C           |                                                                                                                                                                                                  | |--|--| | RPB6<br>RPB6 | Pensez-vous que votre travail influence votre santé ?                                                                                                                                            | | Car 1        | 1. Non, mon travail n'influence pas ma santé,<br>2. Oui, mon travail est plutôt bon pour ma santé,<br>3. Oui, mon travail est plutôt mauvais pour ma santé,<br>9. NSP<br>8. REFUS<br>Non réponse |

CT-RPS 2016

### G - questions finales

<!-- p. 129 -->

| 8G           |                                                                                                                                                                                                     | |--|--| | RP35<br>RP35 | Sur l'ensemble de votre vie professionnelle, pouvez-vous noter de 0 à<br>10 votre niveau d'accord avec l'affirmation suivante ?<br>Pour l'essentiel, je suis satisfait(e) de ma vie professionnelle | | Num - 8      | Réponse                                                                                                                                                                                             |

Donner une note de 0 à 10 où 0 signifie "pas du tout satisfait", 10 "totalement satisfait" CT-RPS 2016

| 8G           |                                                                                                                     | |--|--| | RP36<br>RP36 | Maintenant, concernant votre vie personnelle.<br>Dans quelle mesure êtes-vous satisfait(e) de votre vie privée<br>? | | Num - 8      | Réponse                                                                                                             |

Donner une note de 0 à 10 où 0 signifie "pas du tout satisfait", 10 "totalement satisfait" CT-RPS 2016

| 8G      |                                    | |--|--| | WHO5    | Score de bien-être de l'OMS (WHO5) | | Num - 8 | De 1 à 25                          |

A partir des réponses à RPc1a RPc1b RPc1c RPc1d RPc1e : 5 pour une réponse "1", 4 pour une réponse "2", 3 pour une réponse "3", 2

pour une réponse "4", 1 pour une réponse "5". Le score n'est pas calculé si une réponse est manquante. Plus le score est élevé, plus le "bien-être" est élevé.

CT-RPS 2016

| 8G        |                                                       |      | |--|--|--| | CHERCHEUR | Accepteriez-vous<br>un<br>entretien<br>complémentaire | pour | | CLC1      | approfondir certains sujets ?                         |      | | Car 1     | 1. Oui                                                |      | |           | 2. Non                                                |      |

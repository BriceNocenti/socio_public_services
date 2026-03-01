# Guide des étiquettes de valeurs pour variables factorielles

Ce document présente des exemples de renommage de valeurs d'étiquettes (value labels) et les règles générales appliquées de manière cohérente.

---

## 1. Exemples de bonne pratique

### 1.1 Variables ordinales (échelles de fréquence, intensité, etc.)

#### Exemple 1 : Fréquence d'écoute de musique avec les proches

**Variable description** : `MUSSOCCONJ` - "Mon conjoint.e - Au cours des 12 derniers mois, avez-vous partagé des moments d'écoute avec certains de vos proches ?"

**Input (base labels)** :
1-Oui, souvent
2-Oui, de temps en temps
3-Oui, rarement
4-Non, jamais

**Output (modified labels)** :
```r
pc18$MUSSOCCONJ <- fct_recode(pc18$MUSSOCCONJ, # "new" = "old" 
  "1-Moments conjoint·e: souvent" = "1-Oui, souvent",
  "2-Parfois"                     = "2-Oui, de temps en temps",
  "3-Rarement"                    = "3-Oui, rarement",
  "4-Jamais"                      = "4-Non, jamais"
)
```

**Observations** :
- La description de la variable précise et concise "Moments conjoint·e", ajoutée à la première modalité
- Les modalités suivantes sont épurées : "Parfois", "Rarement", "Jamais" (sans répétition du contexte)
- Condensation : "Oui, de temps en temps" → "Parfois"


#### Exemple 2 : Pratique de lecture durant l'enfance

**Variable description** : `LECTENFENFANCE` - "Enfance - Aux périodes suivantes de votre vie, avec quelle régularité lisiez-vous des livres ?"

**Input (base labels)** :
1-Jamais
2-1 ou 2 livres/an
3-3 à 5
4-Plus de 5


**Output (modified labels)** :
```r
L325$LECTENFENFANCE <- fct_recode(L325$LECTENFENFANCE, # "new" = "old" 
  "1-Enfance: plus de 5 livres/an" = "4-Plus de 5"
  "2-3 à 5 livres"                 = "3-3 à 5",
  "3-1 ou 2"                       = "2-1 ou 2 livres/an",
  "4-Aucun"                        = "1-Jamais",
) |> 
  fct_relevel(sort)
```

**Observations** :
- Ajout de "Enfance:", tiré de la description de la variable dans la première modalité positive
- Inversion de l’ordre des modalités, pour avoir des modalités décroissantes ; mais seulement si demandé par l’utilisateur. 


#### Exemple 3 : Fréquence de repas

**Variable description** : `REPASFREQRESCAN` - "Restaurant ou cantine - Au cours des 7 derniers jours, à quelle fréquence avez-vous fait chacun des repas suivants ?"

**Input (base labels)** :
1-4 fois/sem ou plus
2-2 ou 3 fois
3-1 fois
4-Jamais

**Output (modified labels)** :
```r
L325$REPASFREQRESCAN <- fct_recode(L325$REPASFREQRESCAN, # "new" = "old" 
  "1-Restaurant, cantine: 4 fois/sem ou +" = "1-4 fois/sem ou plus",
  "2-2 ou 3 fois"                          = "2-2 ou 3 fois",
  "3-1 fois"                               = "3-1 fois",
  "4-Jamais"                               = "4-Jamais"
)
```

**Observations** :
- Ajout de "Restaurant, cantine:" tiré de la description dans la première modalité

#### Exemple 4 : Temps de préparation pour événements

**Variable description** : `APPTEMPSREUFAM` - "Réunion familiale - Pour chacun des événements suivants, combien de temps passez-vous généralement à vous préparer ?"

**Input (base labels)** :
1-Moins de 15 minutes
2-15 à 30 minutes
3-30 minutes à 1h
4-Plus d'1h

**Output (modified labels)** :
```r
L325$APPTEMPSREUFAM2 <- fct_recode(L325$APPTEMPSREUFAM, # "new" = "old" 
  "1-Réunion familiale: >1h" = "4-Plus d'1h"
  "2-30 min à 1h"            = "3-30 minutes à 1h",
  "3-15 à 30 min"            = "2-15 à 30 minutes",
  "4-<15 min"                = "1-Moins de 15 minutes",
)
```

**Observations** :
- Ajout de "Réunion familiale:" dans la première modalité
- Simplification avec symboles mathématiques (≤, >)

---

### 1.2 Variables ordinales (d'accord/appréciation)

#### Exemple 5 : Appréciation de pratiques corporelles

**Variable description** : `APPPRATIQUESTATOUAGE` - "Tatouage - Que pensez-vous des pratiques suivantes ?"

**Input (base labels)** :
1-J'en porte ou j'envisage de le faire
2-J'apprécie sur les autres mais je n'oserais pas en porter
3-Je n'apprécie pas
4-Je n'ai pas d'avis

**Output (modified labels)** :
```r
L325$APPPRATIQUESTATOUAGE <- fct_recode(L325$APPPRATIQUESTATOUAGE, # "new" = "old" 
  "1-Tatouage : en porte ou envisage" = "1-Tatouage : J'en porte ou j'envisage de le faire",
  "2-Apprécie mais n'oserait pas" = "2-J'apprécie sur les autres mais je n'oserais pas en porter",
  "3-N'apprécie pas" = "3-Je n'apprécie pas",
  "4-Pas d'avis" = "4-Je n'ai pas d'avis"
)
```

**Observations** :
- Ajout de "Tatouage" dans la première modalité uniquement
- Passage de la 1ère personne ("J'en porte") à la 3ème personne neutre ("en porte")
- Condensation de phrases longues : "J'apprécie sur les autres mais je n'oserais pas en porter" → "Apprécie mais n'oserait pas"


---

### 1.3 Variables nominales (catégories non ordonnées)

#### Exemple 6 : Diplôme

**Variable description** : `DIPLOM` - "Quel est le diplôme le plus élevé que vous avez obtenu ?"

**Input (base labels)** :
1-Vous n'avez jamais été à l'école ou vous l'avez quittée avant la fin du primaire
2-Aucun diplôme et scolarité interrompue à la fin du primaire ou avant la fin du collège
3-Aucun diplôme et scolarité jusqu'à la fin du collège et au-delà
4-CEP
5-BEPC, brevet élémentaire, brevet des collèges, DNB
6-CAP, BEP ou diplôme équivalent
7-Baccalauréat général ou technologique, brevet supérieur
8-Capacité en droit, DAEU, ESEU
9-Baccalauréat professionnel, brevet professionnel, de technicien ou d'enseignement, diplôme équivalent
10-BTS, DUT, DEUST, diplôme de la santé ou social de niveau Bac+2 ou diplôme équivalent
11-Licence, licence pro, maîtrise ou autre diplôme de niveau Bac+3 ou 4 ou diplôme équivalent
12-Master, DEA, DESS, diplôme grande école de niveau Bac+5, doctorat de santé
13-Doctorat de recherche (hors santé)

**Output (modified labels)** :
```r
pc18$DIPLOM <- fct_recode(pc18$DIPLOM, # "new" = "old" 
  "1-Aucun diplôme" = "1-Vous n'avez jamais été à l'école ou vous l'avez quittée avant la fin du primaire",
  "1-Aucun diplôme" = "2-Aucun diplôme et scolarité interrompue à la fin du primaire ou avant la fin du collège",
  "1-Aucun diplôme" = "3-Aucun diplôme et scolarité jusqu'à la fin du collège et au-delà",
  "2-CAP BEP"       = "4-CEP",
  "2-CAP BEP"       = "5-BEPC, brevet élémentaire, brevet des collèges, DNB",
  "2-CAP BEP"       = "6-CAP, BEP ou diplôme équivalent",
  "4-Bac"           = "7-Baccalauréat général ou technologique, brevet supérieur",
  "4-Bac"           = "8-Capacité en droit, DAEU, ESEU",
  "3-Bac pro"       = "9-Baccalauréat professionnel, brevet professionnel, de technicien ou d'enseignement, diplôme équivalent",
  "5-Bac+2"         = "10-BTS, DUT, DEUST, diplôme de la santé ou social de niveau Bac+2 ou diplôme équivalent",
  "6-Licence"       = "11-Licence, licence pro, maîtrise ou autre diplôme de niveau Bac+3 ou 4 ou diplôme équivalent",
  "7-Bac+5"         = "12-Master, DEA, DESS, diplôme grande école de niveau Bac+5, doctorat de santé",
  "7-Bac+5"         = "13-Doctorat de recherche (hors santé)"
)
```

**Observations** :
- Regroupement massif des longues descriptions en catégories simples et courtes
- Quand c’est possible, hiérarchie claire par niveau : "Aucun diplôme", "CAP BEP", "Bac pro", "Bac", "Bac+2", "Licence", "Bac+5"
- Suppression des détails inutiles ("ou diplôme équivalent", etc.)


#### Exemple 7 : Situation d'activité

**Variable description** : `SITUA` - "Quelle est votre situation principale par rapport au travail ?"

**Input (base labels)** :
1-Occupe un emploi
2-Apprenti sous contrat ou stagiaire rémunéré
3-Etudiant, élève, en formation ou stagiaire non rémunéré
4-Chômeur inscrit ou non à Pôle Emploi
5-Retraité ou retiré des affaires ou en préretraite
6-Femme ou homme au foyer
7-Inactif ou inactive pour cause d'invalidité
8-Autre situation d'inactivité

**Output (modified labels)** :
```r
pc18$SITUA <- fct_recode(pc18$SITUA, # "new" = "old" 
  "1-En emploi"             = "1-Occupe un emploi",
  "2-Apprenti, stagiaire"   = "2-Apprenti sous contrat ou stagiaire rémunéré",
  "3-Étudiant, élève"       = "3-Etudiant, élève, en formation ou stagiaire non rémunéré",
  "4-Chômeur"               = "4-Chômeur inscrit ou non à Pôle Emploi",
  "5-Retraité"              = "5-Retraité ou retiré des affaires ou en préretraite",
  "6-Au foyer"              = "6-Femme ou homme au foyer",
  "7-Invalide"              = "7-Inactif ou inactive pour cause d'invalidité",
  "8-Autre inactif"         = "8-Autre situation d'inactivité",
)
```

**Observations** :
- Condensation massive : "Occupe un emploi" → "En emploi"
- Suppression des détails ("inscrit ou non à Pôle Emploi", "Femme ou homme")
- Simplification : "Inactif ou inactive pour cause d'invalidité" → "Invalide"




#### Exemple 10 : Statut d'emploi

**Variable description** : `STATUT` - "Statut d'emploi"

**Input (base labels)** :
1-Salarié.e de l'État
2-Salarié.e d'une collectivité territoriale
3-Salarié.e d'une entreprise, d'un artisan, d'une association
4-Salarié.e d'un ou plusieurs particuliers
5-Vous aidez un membre de votre famille sans être rémunéré
6-Chef d'entreprise salarié, PDG, gérant.e minoritaire, associé.e
7-Indépendant.e ou à son compte

**Output (modified labels)** :
```r
pc18$STATUT <- fct_recode(pc18$STATUT, # "new" = "old" 
  "1-Salarié·e État"         = "1-Salarié.e de l'État",
  "2-Salarié·e collectivité" = "2-Salarié.e d'une collectivité territoriale",
  "3-Salarié·e entreprise"   = "3-Salarié.e d'une entreprise, d'un artisan, d'une association",
  "4-Salarié·e particulier"  = "4-Salarié.e d'un ou plusieurs particuliers",
  "7-À son compte"           = "5-Vous aidez un membre de votre famille sans être rémunéré",
  "7-À son compte"           = "6-Chef d'entreprise salarié, PDG, gérant.e minoritaire, associé.e",
  "7-À son compte"           = "7-Indépendant.e ou à son compte"
)
```

**Observations** :
- Condensation maximale : "de l'État" → "État", "collectivité territoriale" → "collectivité"
- Regroupement de toutes les formes d'indépendants sous "À son compte"
- Maintien de l'écriture inclusive, mais avec point médian `·`


---

### 1.4 Variables binaires

#### Exemple 11 : Pratique en amateur (variables binaires Oui/Non → Nom de la pratique/Non)

**Variable description** : `A1001` - "Faire du tricot, de la broderie ou de la couture, créer ou personnaliser des vêtements"

**Input (base labels)** :
1-Oui
2-Non

**Output (modified labels)** :
```r
pc18$A1001 <- fct_recode(pc18$A1001, # "new" = "old" 
  "1-Tricot, couture" = "1-Oui",
  "2-Non"             = "2-Non"
)
```

**Observations** :
- Remplacement de "Oui" par un label descriptif court tiré de la question : "Tricot, couture"
- "Non" reste inchangé
- **Règle fondamentale** : sur les binaires, la première modalité prend le nom de la variable/pratique


#### Exemple 13 : Variables QCM avec répétition du sujet

**Variable description** : `APPREMARQUESPARENT` - "Un parent - Parmi ces personnes, y en a-t-il qui ont déjà fait des remarques péjoratives, blessantes ou déplacées à propos de votre apparence ?"

**Input (base labels)** :
1-Un parent
2-Non

**Output (modified labels)** :
```r
L325$APPREMARQUESPARENT <- fct_recode(L325$APPREMARQUESPARENT, # "new" = "old" 
  "1-Parent" = "1-Un parent",
  "2-Non"    = "2-Non"
)
```

**Observations** :
- Simplification : "Un parent" → "Parent" (retrait de l'article)
- Structure identique pour toutes les modalités QCM : nom de la catégorie / Non




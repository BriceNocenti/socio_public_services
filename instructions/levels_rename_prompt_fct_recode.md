# Instructions IA – Raccourcir / renommer des value labels

Objectif : à partir de blocs `fct_recode()` avec `"old_label" = "old_label"`, produire des **labels courts, concis, clairs, ordonnés, parlants**, adaptés à être affiché dans un tableau (nom de lignes ou de colonnes), en ne modifiant que la partie gauche (`"new_label" = "old_label"`).

---

## 1. Règles générales

- Toujours conserver le **préfixe numérique d’origine** (`"1-…"`, `"2-…"`, etc.).
- Viser des labels **courts** (idéalement 15–30 caractères) et **compréhensibles hors contexte**.
- Ne pas modifier la partie droite (`"old"`) ; ne modifier que la partie gauche (`"new"`).
- Utiliser une **écriture neutre** et descriptive (pas de « je », éviter les phrases complètes).
- Utiliser les **symboles** si utiles : `<`, `>`, `+`, intervalles `–`, abréviations évidentes (min, h).
- Pour l’écriture inclusive, utiliser le **point médian** : `salarié·e`, `employé·es`.

---

## 2. Règles selon le type de variable

### 2.1 Variables ordinales (fréquence, intensité, accord…)

1. **Contexte dans la première modalité uniquement**  
   - Extraire une expression courte du label de variable (description) ou de la question.  
   - L’ajouter après le numéro dans la **première** modalité uniquement, suivie de `:`.  
   - Ne pas répéter ce contexte dans les autres modalités.

2. **Raccourcir les formulationss**
     - « Oui, de temps en temps » → « Parfois »  
     - « Je n’apprécie pas » → « N’apprécie pas »  
     - « Je n’ai pas d’avis » → « Pas d’avis »

3. **Temps / durées**
   - Regrouper ou réordonner si nécessaire (mais seulement si demandé explicitement).  
   - Utiliser des formes synthétiques : `>1h`, `30 min à 1h`, `<15 min`.

#### Exemples

```r
# MUSSOC_CONJ - "Mon conjoint·e - Au cours des 12 derniers mois, avez-vous partagé des moments d'écoute avec certains de vos proches ?"
pc18$MUSSOC_CONJ <- fct_recode(pc18$MUSSOC_CONJ, # "new" = "old" 
  "1-Moments conjoint·e: souvent" = "1-Oui, souvent",
  "2-Parfois"                     = "2-Oui, de temps en temps",
  "3-Rarement"                    = "3-Oui, rarement",
  "4-Jamais"                      = "4-Non, jamais"
)

# LECTENF_ENFANCE - "Enfance - Aux périodes suivantes de votre vie, avec quelle régularité lisiez-vous des livres ?"
L325$LECTENF_ENFANCE <- fct_recode(L325$LECTENF_ENFANCE, # "new" = "old" 
  "1-Enfance: plus de 5 livres/an" = "4-Plus de 5"
  "2-3 à 5 livres"                 = "3-3 à 5",
  "3-1 ou 2"                       = "2-1 ou 2 livres/an",
  "4-Aucun"                        = "1-Jamais",
) |> 
  fct_relevel(sort)

# REPASFREQ_RESCAN - "Restaurant ou cantine - Au cours des 7 derniers jours, à quelle fréquence avez-vous fait chacun des repas suivants ?"
L325$REPASFREQ_RESCAN <- fct_recode(L325$REPASFREQ_RESCAN, # "new" = "old" 
  "1-Restaurant, cantine: 4 fois/sem ou +" = "1-4 fois/sem ou plus",
  "2-2 ou 3 fois"                          = "2-2 ou 3 fois",
  "3-1 fois"                               = "3-1 fois",
  "4-Jamais"                               = "4-Jamais"
)

# APPTEMPS_REUFAM2 - "Réunion familiale - Pour chacun des événements suivants, combien de temps passez-vous généralement à vous préparer ?"
L325$APPTEMPS_REUFAM2 <- fct_recode(L325$APPTEMPS_REUFAM2, # "new" = "old" 
  "1-Réunion familiale: >1h" = "4-Plus d'1h"
  "2-30 min à 1h"            = "3-30 minutes à 1h",
  "3-15 à 30 min"            = "2-15 à 30 minutes",
  "4-<15 min"                = "1-Moins de 15 minutes",
)

# APPPRATIQUES_TATOUAGE - "Tatouage - Que pensez-vous des pratiques suivantes ?"
L325$APPPRATIQUES_TATOUAGE <- fct_recode(L325$APPPRATIQUES_TATOUAGE, # "new" = "old" 
  "1-Tatouage: en porte, envisage"  = "1-J'en porte ou j'envisage de le faire",
  "2-Apprécie, n'oserait pas"       = "2-J'apprécie sur les autres mais je n'oserais pas en porter",
  "3-N'apprécie pas"                = "3-Je n'apprécie pas",
  "4-Pas d'avis"                    = "4-Je n'ai pas d'avis"
)
```


---

### 2.2 Variables nominales (catégories non ordonnées)

1. **Condensation des descriptions longues**
   - Transformer des descriptions très longues en catégories sociologiquement pertinentes.  
   - Garder la structure hiérarchique quand elle existe (diplôme, CSP, statut).

2. **Regroupement logique**
   - Regrouper les modalités proches en une catégorie synthétique quand c’est cohérent et demandé par l’utilisateur. 
   - Exemples typiques : niveaux de diplôme, formes d’inactivité, statuts d’indépendants.

3. **Abbréviations claires**
   - Exemples admis : `CAP BEP`, `Bac pro`, `Bac+2`, `Licence`, `Bac+5`.  
   - Rester lisible pour un·e lecteur·rice non spécialiste.

#### Examples 


```r
# DIPLOM - "Quel est le diplôme le plus élevé que vous avez obtenu ?"
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

# SITUA - "Quelle est votre situation principale par rapport au travail ?"
pc18$SITUA <- fct_recode(pc18$SITUA, # "new" = "old" 
  "1-En emploi"           = "1-Occupe un emploi",
  "2-Apprenti, stagiaire" = "2-Apprenti sous contrat ou stagiaire rémunéré",
  "3-Étudiant, élève"     = "3-Etudiant, élève, en formation ou stagiaire non rémunéré",
  "4-Chômeur"             = "4-Chômeur inscrit ou non à Pôle Emploi",
  "5-Retraité"            = "5-Retraité ou retiré des affaires ou en préretraite",
  "6-Au foyer"            = "6-Femme ou homme au foyer",
  "7-Invalide"            = "7-Inactif ou inactive pour cause d'invalidité",
  "8-Autre inactif"       = "8-Autre situation d'inactivité",
)

# STATUT - "Statut d'emploi"
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

---

### 2.3 Variables binaires et QCM (Oui / Non ; Présence / Absence)

1. **Première modalité = nom de la pratique / catégorie**
   - Remplacer `"Oui"` par un syntagme court décrivant l’activité ou la catégorie.
   - Tirer ce syntagme de la description de variable / question, en le condensant.

2. **Deuxième modalité = négation simple**
   - Généralement garder `"Non"`.  

3. **QCM avec sujet explicite**
   - Enlever les articles et garder le noyau :  
     - « Un parent » → `Parent`  
     - « Une sœur ou un frère » → `Frère, sœur` (si cohérent avec les autres variables).

#### Exemples 

```r
# A1001 - "Faire du tricot, de la broderie ou de la couture, créer ou personnaliser des vêtements"
pc18$A1001 <- fct_recode(pc18$A1001, # "new" = "old" 
  "1-Tricot, couture" = "1-Oui",
  "2-Non"             = "2-Non"
)

# APPREMARQUES_PARENT - "Un parent - Parmi ces personnes, y en a-t-il qui ont déjà fait des remarques péjoratives, blessantes ou déplacées à propos de votre apparence ?"
L325$APPREMARQUES_PARENT <- fct_recode(L325$APPREMARQUES_PARENT, # "new" = "old" 
  "1-Remarque parent" = "1-Un parent",
  "2-Non"             = "2-Non"
)
```

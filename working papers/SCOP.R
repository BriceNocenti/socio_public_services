# Démarrage ----
source("~\\github/socio_public_services/Demarrage.R", encoding = "UTF-8")
library(tidyverse)
library(data.table)

# load("ee13_20_front.RData")
# load("ee13_20_front_SAVE.RData")
# #load("D:\\Statistiques\\Formations\\FormationR_UHA\\ee13_20_front.RData")
# 
# ee2013_20 <- ee2013_20 %>%
#   mutate(CSTOTR = fct_recode(CSTOTR, 
#                              "8-Chômeurs n'ayant jamais travaillé" = "8-Autres personnes sans activité professionnelle (chômeurs et inactifs n'ayant jamais travaillé, inactifs dont l'ancienne profession est non renseignée)", 
#                              "3-Cadres et professions supérieures" = "3-Cadres et professions intellectuelles supérieures"
#   ))
# 
# ee2013_20 <- ee2013_20 %>% 
#   mutate(EXTRIDF8 = EXTRIDF/8, 
#          EXTRID8  = EXTRID/8  )
# 
# save("ee2013_20", file = "ee13_20_front.RData")

ee <- readRDS("~/Formations/M1_Demo_AGD/ee2013_20.rds") 
ee <-  ee |> 
  mutate(AGE = as.integer(ANNEE) - NAIA) |>

  filter(as.integer(ANNEE) <= 2019 & SALARIAT == "1-Salarié" & 
           RGA == "1-Grappe entrante (1ère interrogation)" ) 


# Analyses ----

CJUR_SCOP <- 
c(
# "5191-Société de caution mutuelle"                                                                              ,
# "5192-Société coopérative de banque populaire"                                                                  ,
# "5193-Caisse de crédit maritime mutuel"                                                                         ,
  # "5194-Caisse (fédérale) de crédit mutuel"                                                                       ,
  # "5195-Association coopérative inscrite (droit local Alsace Moselle)"                                            ,
  # "5196-Caisse d'épargne et de prévoyance à forme coopérative"                                                    ,
  # "5202-Société en nom collectif"                                                                                 ,
  # "5306-Société en commandite simple"                                                                             ,
  # "5308-Société en commandite par actions"                                                                        ,
  # "5385-Société d'exercice libéral en commandite par actions"                                                     ,
  # "5432-SARL d'intérêt collectif agricole (SICA)"                                                                 ,
  # "5442-SARL d'attribution"                                                                                       ,
  # "5451-SARL coopérative de consommation"                                                                         ,
  # "5453-SARL coopérative artisanale"                                                                              ,
  # "5455-SARL coopérative de transport"                                                                            ,
  "5458-SARL coopérative ouvrière de production (SCOP)"                                                           ,
  # "5459-SARL union de sociétés coopératives"                                                                      ,
  # "5460-Autre SARL coopérative"                                                                                   ,
  
  # "5470-Société de Participations Financières de Profession Libérale Société à responsabilité limitée (SPFPL SARL)",
  # "5485-Société d'exercice libéral à responsabilité limitée"                                                      ,
  # "5543-SA coopérative de construction à conseil d'administration"                                                ,
  # "5547-SA coopérative de production de HLM à conseil d'administration"                                           ,
  # "5551-SA coopérative de consommation à conseil d'administration"                                                ,
  # "5552-SA coopérative de commerçants-détaillants à conseil d'administration"                                     ,
  # "5553-SA coopérative artisanale à conseil d'administration"                                                     ,
  # "5554-SA coopérative (d'intérêt) maritime à conseil d'administration"                                           ,
  # "5555-SA coopérative de transport à conseil d'administration"                                                   ,
  "5558-SA coopérative ouvrière de production (SCOP) à conseil d'administration"                                  ,
  # "5559-SA union de sociétés coopératives à conseil d'administration"                                             ,
  # "5560-Autre SA coopérative à conseil d'administration"                                                          ,
  # "5585-Société d'exercice libéral à forme anonyme à conseil d'administration"                                    ,
  # "5651-SA coopérative de consommation à directoire"                                                              ,
  # "5652-SA coopérative de commerçants-détaillants à directoire"                                                   ,
  # "5654-SA coopérative d'intérêt maritime à directoire"                                                           ,
  "5658-SA coopérative ouvrière de production (SCOP) à directoire"                                                #,
  #"5659-SA union de sociétés coopératives à directoire"                                                           ,
  #"5660-Autre SA coopérative à directoire"                                                                        ,
  
  # "6100-Caisse d'Épargne et de Prévoyance"                                                                        ,
  # "6210-Groupement européen d'intérêt économique (GEIE)"                                                          ,
  # "6220-Groupement d'intérêt économique (GIE)"                                                                    ,
  # "6316-Coopérative d'utilisation de matériel agricole en commun (CUMA)"                                          ,
# "6317-Société coopérative agricole"                                                                             ,
# "6318-Union de sociétés coopératives agricoles"                                                                 ,
# "6411-Société d'assurance à forme mutuelle"                                                                     ,
# "6511-Sociétés Interprofessionnelles de Soins Ambulatoires "                                                    ,
# "6521-Société civile de placement collectif immobilier (SCPI)"                                                  ,
# "6532-Société civile d'intérêt collectif agricole (SICA)"                                                       ,
# "6533-Groupement agricole d'exploitation en commun (GAEC)"                                                      ,
# "6534-Groupement foncier agricole"                                                                              ,
# "6536-Groupement forestier"                                                                                     ,
# "6537-Groupement pastoral"                                                                                      ,
# "6538-Groupement foncier et rural"                                                                              ,
# "6540-Société civile immobilière"                                                                               ,
# "6541-Société civile immobilière de construction-vente"                                                         ,
# "6542-Société civile d'attribution"                                                                             ,
# "6543-Société civile coopérative de construction"                                                               ,
# "6551-Société civile coopérative de consommation"                                                               ,
# "6558-Société civile coopérative entre médecins"                                                                ,
# "6560-Autre société civile coopérative"                                                                         ,
# "6561-SCP d'avocats"                                                                                            ,
# "6562-SCP d'avocats aux conseils"                                                                               ,
# "6563-SCP d'avoués d'appel"                                                                                     ,
# "6564-SCP d'huissiers"                                                                                          ,
# "6565-SCP de notaires"                                                                                          ,
# "6566-SCP de commissaires-priseurs"                                                                             ,
# "6567-SCP de greffiers de tribunal de commerce"                                                                 ,
# "6568-SCP de conseils juridiques"                                                                               ,
# "6569-SCP de commissaires aux comptes"                                                                          ,
# "6571-SCP de médecins"                                                                                          ,
# "6572-SCP de dentistes"                                                                                         ,
# "6573-SCP d'infirmiers"                                                                                         ,
# "6574-SCP de masseurs-kinésithérapeutes"                                                                        ,
# "6575-SCP de directeurs de laboratoire d'analyse médicale"                                                      ,
# "6576-SCP de vétérinaires"                                                                                      ,
# "6577-SCP de géomètres experts"                                                                                 ,
# "6578-SCP d'architectes"                                                                                        ,
# "6585-Autre société civile professionnelle"                                                                     ,
# "6589-Société civile de moyens"                                                                                 ,
# "6595-Caisse locale de crédit mutuel"                                                                           ,
# "6596-Caisse de crédit agricole mutuel"                                                                         ,
# "6597-Société civile d'exploitation agricole"                                                                   ,
# "6599-Autre société civile"                                                                                     ,
                            
# "9110-Syndicat de copropriété"                                                                                  ,
# "9150-Association syndicale libre"                                                                              ,
# "9210-Association non déclarée"                                                                                 ,
# "9220-Association déclarée"                                                                                     ,
# "9221-Association déclarée d'insertion par l'économique"                                                        ,
# "9222-Association intermédiaire"                                                                                ,
# "9223-Groupement d'employeurs"                                                                                  ,
# "9224-Association d'avocats à responsabilité professionnelle individuelle"                                      ,
# "9230-Association déclarée, reconnue d'utilité publique"                                                        ,
# "9240-Congrégation"                                                                                             ,
# "9260-Association de droit local (Bas-Rhin, Haut-Rhin et Moselle)"                                              ,
# "9300-Fondation"                                                                                                ,
# "9900-Autre personne morale de droit privé"                                                                     ,
# "9970-Groupement de coopération sanitaire à gestion privée"  # ,
)

# # Pas trouvé les SCIC : société coopératives d'intérêt collectif
# levels(ee$CJUR)[str_detect(levels(ee$CJUR), "intéret collectif|interet collectif|intérêt collectif")]

# # toutes les CJUR avec "coop"
# ee |>
#   filter(!is.na(CJUR) & str_detect(CJUR, "coop") & !is.na(CSTOTR)) |> 
#   tab(CJUR, CSTOTR, wt = EXTRID, pct = "row" ) |> # ANNEE
#   mutate(n = mutate(Total, display = "n")) |>
#   arrange(desc(n)) |>
#   tab_kable()

# Chiffres CGSCOP, rapport d'activité 2023 : 
# Scop : 60 056 salariés à fin 2023.
# Scic : 15 367 salariés à fin 2023.
# Filiales : 8 871
# 
# Les femmes représentent 31 % des effectifs du Mouvement (soit
#  un point de moins que l’année passée).
# Parmi les 3 847 dirigeants recensés des coopératives actives
#  en 2023, 27 % sont des femmes. 


# 237 personnes en SCOP dans EE empilées 2013-2019 
# (en représentant 25 000 à 30 000, donc il en manque plus de la moitié)
ee |>
  filter(!is.na(CJUR) & CJUR %in% CJUR_SCOP) |> 
  tab(ANNEE, wt = EXTRID) |>
  mutate(wn = n, n = mutate(n, display = "n")) |>
  select(-Total)


# Selon le genre : 163 hommes et 74 femmes (en représentant autour de 8200)
ee |>
  filter(!is.na(CJUR) & CJUR %in% CJUR_SCOP) |> 
  tab(SEXE, wt = EXTRID) |>
  mutate(wn = n, n = mutate(n, display = "n")) |>
  select(-Total)




# - Questionnaire passé à travers la CGSCOP ? Papier ou numérique ou les deux ?
# - Hommes et femmes des SCOP : suréchantillon féminin 2/3 1/3 (= inversé ; ou 50/50 ?) ? 
# - Phase de test
# - Essentiel de pouvoir comparer avec les salariés hors SCOP : reprendre les questions 
# sur lesquelles on veut comparer de l'enquête Conditions de travail 2024 ?
# 
# 


bts21 <- readr::read_csv2("~\\Data\\Base tous salariés\\FD_SALAAN_2021.csv")
bts21 <- bts21 |> mutate(across(where(is.character), as.factor))


bts21


# Agréger catégories
"CONT_TRAV"

bts21_dt <- as.data.table(bts21)

tab_test <- bts21_dt |>
  filter(FILT == "1") |> # seulement postes "non-annexes"
  
  tab_dt(groups = c("SEXE", "PCS", "CONT_TRAV", "REGT", "CPFD", "DOMEMPL_EM", "A38"),  # "TYP_EMPLOI", "TRNNETO", "TREFF"
         # wt = "POND", 
         n_min = 5,
         num_vars = c("NBHEUR", "NBHEUR_TOT", "AGE") # , 
  ) 
# Tous les NA sont bien des n < 5.

# tab_test2 <- tab_test

tab_test <- bts21_dt |>
  filter(FILT == "1") |> # seulement postes "non-annexes"
  tab_dt(groups = c("SEXE", "PCS", "CONT_TRAV", "REGT", "CPFD", "DOMEMPL_EM", "A38"),  # "TYP_EMPLOI", "TRNNETO", "TREFF"
         n_min = 0,
         wt = "POND", 
         num_vars = c("AGE") # , 
  ) 
setDF(tab_test)
tab_test <- tab_test |> as_tibble()
tab_test



tab_test |> filter(n >= 5)


















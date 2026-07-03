

ct05$ANNEE <- "2005"
ct13$ANNEE <- "2013"
ct16$ANNEE <- "2016"
ct19$ANNEE <- "2019"

ct05$base <- "AO"
ct13$base <- "AO"
ct16$base <- "AO"
ct19$base <- "AO"

ct05$CHAMP_CT2005 <- "1"



# Selectionner les variables à partir de celles de CT2016 --------------------------
ctsel <-
  c(#A-Etat-civil
    "SEXE", "ANAIS", "AGE", "AGEQ", "LNAIS", "DEPNAIS", "region", #"region_anc" # avant réforme
    # "MNAIS",   "lnaisd", "ANARRIV",   "AGARRIV",
    
    #B-Situation familiale
    "COUPLE", "ETAMATRI", "PACS", "typmen5", "typmen15",
    "NBENFM3", "NBENF3A17", "NBENF18P", "NPERS", #Nombre de personnes du ménage
    
    #1-Activité professionnelle
    
    #CSP
    "PE", "CSE", "cser", #"peun", "peun10",
    "FAP225", "FAP87",
    
    #Employeur
    "STATUT",  #"TYPETSCO" pour les enseignants "ESPIC" (privé non-lucratif) "NBSAL" patrons
    "AUTENT", # Auto-entrepreneur
    "ACTIVFIN",  "naf38", "naf17", "NAF4",
    "FRANCE",
    
    #Effectifs
    "NBSALA", "NBSALB", "EVOLEFF", #établissement, et évolution des effectifs
    "NBSALENTC", #entreprise
    
    #Par rapport à l'emploi en 2013
    #"INTERRUP", "ARREMPA", "ARREMPM", "CTPROFESS",
    # "DATANT", "DAMOIS"  # ancienneté
    
    #Fonction
    "CLASSIF", "FONCTION", "ENCADR", "LIEUW", "CLIENT", "POURCLIEN", #lieu de travail principal, client le plus important en % du CA # "TRAJET"
    
    #Contrat et statut
    "TYPEMPLOI", "TITPUBR", "TITPUB", "RDET", #choix CDD-intérim
    #"tps_contrat",   "TPS_INTERIM", "duree_cessation",
    
    #Ancienneté
    "anciennete", #entreprise/FP/profession  # "ANETA", "ANETM"  # année et mois dans l'établissement
    
    #Rémunération
    "revmensc",
    "REMPERF", #"rémunération en fonction des performances
    "PAYECOM", #bien/mal payé
    
    #Temps de travail
    "TPP", "TXTPPB", "RAISTP", #temps partiel, quotité, raison
    "HH", #temps de travail habituel hebdomadaire  #"HHTOT" #plusieurs employeurs
    "JOURTR", "REPOS", "SAMEDI", "NBSAME", "DIMANCHE", "NBDIMA",
    
    #Horaires
    "HORANGT", #modifier ses horaires en cas d'imprévu
    "PREVIS", #connaissance des horaires, mois, semaine, jour
    "HORVAR", "PERIODE", #variables d'un jour à l'autre, interruption de 3h+
    "CONTROLE",
    "PTMATIN", "SOIR", "NUIT", "NBNUIT",
    "HSUP", "HSUPCOMP", #Au-delà de l'horaire prévu, compensé ou payé
    
    #Conciliation travail/famille
    "JOINDRE", "COMBIEN", "JOINEXT", "COMBEXT", #Joint en dehors du travail
    "MAISON", "TPMAISON", #Emporter du travail à la maison
    "MISSION", #dormir hors de chez soi
    "ARETMAL", "ATMAL",  #Nb arrêts maladie, nb heures
    "URGFAM", # Quitter son travail en cas d'urgence familiale,
    "CVFVP", # horaires accordées à engagements sociaux et familiaux
    "FATIGUE", #fatigue au travail à cause de la vie domestique
    "INDISPO", #proche se plaignent des horaires
    "DOMEST", #tâches domestiques heures par semaine
    
    #Contraintes physiques
    "CWDEBOU", "CWPOSTU", "CWDEPLA", "CWLOURD", "CWMVT", "CWVIB", "CWVUE",
    "CONDUITE", "KMANNU", # conduire au travail, km/an
    "SECFUPOU", "SECTOXNO", "ENTENDR", #fumées, produits dangereux, bruit
    
    #Prévention et accidents
    "RISK", "FORMSEC", "DOCEVAL", "MEDECIN", #information, formation, documents sur les risques
    "ACCIDT", "ARRET", "NBJARR", #accidents, arrêts, nombre de jours
    
    #Organisation du travail
    "RWDEP", "RWCAD", "RWTECH", "RWCOLEG", "RWNORMH", "RWNORMJ", "RWDEM", "RWDEMAND",
    "RWSURV", "RWINFO",  #Contraintes de rythme
    "CHAINE", "DEPECH", #à la chaîne, obligé de se dépécher
    "DEBORD", "INTERACT", #interruptions, positif ou négatif
    "OBJECTIF", "OBJMODIF", "OBJATTEIN", #objectifs
    "DELAIS", #faire varier les délais
    "PUBLIC", "PUBLIC1", "PUBLIC2", "PUBLIC3", #contact avec public, f-à-f, tél, mail
    "POLY", "POLYQUAL", "MONOTON", #Rotation des postes (pas 2016)
    "INTERUP", "REMPLAS", #possibilité d'interrompre son travail, devoir ou non se faire remplacer
    "COMMENT", "STARK", "INCIDENT", #autonomie, respect des consignes, face aux incidents
    "PROCEDUR", #procédures qualité
    "REPETE", "CYCLE", #répétition, durée du cycle
    "QUANTI", #Intervenir sur la quantité de travail attribuée
    "INITIAT", "IDEE", #initiative, mettre ses propres idées en pratique
    "NOUVELLE", #apprendre des choses nouvelles
    "RESTMAI", "RESTMAIN", #travailler en étant malade
    "SIEG34", #correspondance formation/travail
    "AIDCHEF", "AIDCOLL", "AIDCOLLNS", #aide
    "AIDAUTR", "AIDEXT", "AIDOM",  #aide + seulement 2013
    "TRAVSEUL", "CHGTCOLL", #changements des collègues depuis un an ?
    "COLLECT", "REUNION", #parler de l'orga du travail, dans réunions formalisées
    "ACCHEF", "DICHEF", "ACCOL", "DICOL", #désaccord supérieurs, collègues, possible de discuter
    "CONFSAL", #les supérieurs font confiance aux salariés
    "INFOCONF", #on peut faire confiance aux infos des supérieurs
    "DIFFIC", #que fait la direction en cas de difficultés
    "BIENETR1",  "BIENETR2", #supérieurs attention bien-être, répartissent tâches équitablement
    "TENSION1", "TENSION2", "TENSION3", "TENSION4", #public, chef, collègues, subordonnés
    "DETRESSE", "CALMER", "CONFLIT", #contact personnes en détresse, en colère, gérer conflits
    "EMOTION", #secoué/ému au travail ?
    "ATTENTE", # Savez-vous précisément ce que l’on attend de vous au travail ?
    "CRITERE", # critères utilisés pour évaluer votre travail paraissent-ils pertinents ?
    "CONTRAD", # ordres ou indications contradictoires
    "EVA", "EVACRIT", #entretien d'évaluation
    "SYNDIC", "DISCURP", "SYNDICNS", #syndicat, parlé avec, organisation professionnelle
    "CORRTAN", "CORRINF", "CORRCOP", "CORRCOL", "CORRLOG", "CORRMAT", "CORRFORM", #moyens de travail
    "CRAINTE", "METIER", "NOCHOM", "TENIR", "SOUHAIT", "MUTE", #changement de travail ?
    "FORTMOD1", "FORTMOD2", "FORTMOD3", "FORTMOD4", "FORTMOD5", "FORTMOD6",
    "FORTMOD7", #envir trav fortement modif : cg poste, tech, restruc, orga, plan social, direction, autre
    "CHANGOP", "CHGTINFO", "CHGTCONS", "CHGTINFL", #ch positif/négatif, info, consulté, influence ?
    
    #Santé
    #Santé déclarée
    "BSANTE", "BCHRO", "BLIMI", "BRECO", #état de santé, maladies chronique, limitant, handicap
    "SDOUL", "SDOULOC_NB", #douleurs dans l'année écoulée
    # "SDOULOC_a", "SDOULOC_b", "SDOULOC_c", "SDOULOC_d", "SDOULOC_e", "SDOULOC_f",
    # "SDOULOC_g", "SDOULOC_h", "SDOULOC_i", "SDOULOC_j", "SDOULOC_k", "SDOULOC_l",
    # "SDOULOC_m", "SDOULOC_n", "SDOULOC_o", "SDOULOC_p", "SDOULOC_q", "SDOULOC_r",
    # "SDOULOC_s", "SDOULOC_t", "SDOULOC_FLAG",
    "SOMTBL", "DORMED", #troubles du sommeil, somnifères
    "MEDEC", #vu médecin dans l'année
    #Santé mentale
    #"MIN1EDM", "MIN2EDM", #déprimé, gout à rien
    #"MIN3EDM_a", "MIN3EDM_b", "MIN3EDM_c", "MIN3EDM_d", "MIN3EDM_e", "MIN3EDM_f", "MIN3EDM_g",
    #"MIN3EDM_h", "NBEDM", #appetit, sommeil, lent/agité, fatigué, sans valeur/coupable, concentré, idées noires, rien
    "EDM", #"MIN4EDM", "MIN5EDM", #  épisode dépressif majeur, 1 + 3 + au moins trois 3, ou plus de quatre 3
    # "MIN1TAG", "MIN2TAG", "MIN3TAG", "MIN4TAG_FLAG",
    # "MIN4TAG_a", "MIN4TAG_b", "MIN4TAG_c", "MIN4TAG_d", "MIN4TAG_e", "MIN4TAG_f",
    # "MIN4TAG_g", "NBTAG",
    "TAG", #trouble de l'anxiété
    
    #Parcours familial et professionnel
    "NATIO1N1", "NATIO1N2", "NATIO", "IMMI", "nati", "lienmig", #nationalité, immigration
    "DIPLOME", "AGFINETU", "ANFINETU", #diplôme, age et année de fin d'études
    #"ETUDES", "ETUDIPL", "FORMINIT", # actuellement en formation
    #Trajectoire professionnelle : #depuis études initiale (_E), depuis 2012 (_P)
    "TCHOLE", "TCHOLP", "TCHOCE", "TCHOCP", #chômage > 1an, entre 3 mois et 1 an
    "TSANE", "TSANP", "TINAE", "TINAP", #arrêté de travailler >1an, problèmes de santé, autres
    "TINDEPE", "TINDEPP", "TINTERIME", "TINTERIMP", "TCDDE", "TCDDP", #à son compte, intérim, CDD
    
    "NBEMP", "v1nbemp", "NBEMPE", "nbemp_13", "nbempe_16", #nb d'emploi >1 an pour des employeurs différents
    "nbempp_16",   "NBEMPP",  #nb d'emploi >1 an depuis la dernière enquête
    #"NBEMP_16"
    "PROFDEB", "CSEDEB", "cserdeb", "STATUTA", # Profession du premier emploi
    "DEMPRO", "DEMENA", #déménagement professionnel, avec effets professionnels (ex : conjoint)
    #"v1tchol", "v1tchoc", "v1tsan", "v1tina", "v1tindep", "v1tinterim", "v1tcdd", "v1nbemp", "v1dempro", "v1demena",
    #Origines sociales
    "perimmi", "merimmi", "NAIP", "NAIM", "lnaisper", "lnaismer",
    "natnaisper", "natnaismer", #lieu de naissance / natio de naissance
    "ACTIP", "STATUTP", "PROFPER", "CSEPER", "cserper", #profession du père
    "ACTIM", "STATUTM", "PROFMER", "CSEMER", "csermer",  #profession de la mère
    
    #Auto-questionnaire :
    "typrepqaa", #Indicateur de réponse au QAA (jusqu'à dernière question)
    "REPQAA",  #questionnaire accepté, refusé, impossible pour raisons de langue
    #les variables sont renommées plus bas, plus facile de s'y retrouver
    
    # "QUAL_ADR", "QUAL_SIRET", "QUAL_EMPLX", "QUALITE_VOLET",
    #  "debdetm_drap",
    #  "damois_drap", "anetm_drap", "arrempm_drap", "trajet_drap", "pourclien_drap", "hh_drap",
    #  "hhtot_drap", "jourtr_drap", "tpmaison_drap", "atmal_drap", "nbjarr_drap", "restmain_drap",
    #  "arrempa_drap", "datant_drap", "debdeta_drap", "aneta_drap", "tpsint_drap", "dudet_drap",
    #  "nbrkm_drap", "kmannu_drap", "revmens_drap", "revmensc_drap", "revannu_drap",
    
    # Variables spécifiques à 2005 : 
    "NAF_05"   , "NAF36_05" , "NAF16_05" , "NAF4_05"   , #"GOLDEN_05", 
    "NEWPUB_05", "STATUT_05", "STATOEP_05", "STAT2_05", "CJUR_05", "CHPUB_05",
    "CONTRA_05", "TITC_05"   ,
    "CHEF_05"  , "CHEFNBR_05", "CHEFPROM_05", "DIP_05", #"DDIPL_05", 
    "NEWTEFEN_05", "NEWTEFET_05", 
    
    #Identifiants, pondérations, variables techniques
    "IDENT_IND", "CHAMP_CT2005", "CHAMP_EMPL", "pondcal", "pondqaa", "ACCNIR",
    "ANNEE", "base"
  )




# Création et recodages de variables (base non couplée) ------------------------
ct <- ct |> 
  mutate(ID = row_number(), `1319` = ANNEE %in% c("2013", "2016", "2019")) |>
  group_by(`1319`) |> 
  mutate(ID1319 = row_number()) |> 
  ungroup() |>
  mutate(ID1319 = if_else(`1319`, ID1319, NA_integer_)) |>
  select(-any_of(c("1319")))

# ct |> 
#   mutate(across(c(NBEMPE, nbempe_16, nbemp_13, NBEMP, v1nbemp), as.factor)) |> 
#   #tab_many(NBEMPE, nbempe_16, nbemp_13)
#   #tab_many(NBEMPE, v1nbemp)

ct <- ct |> mutate(NBEMPall = case_when(
  !is.na(NBEMP)     ~ NBEMP, 
  !is.na(NBEMPE)    ~ NBEMPE, 
  !is.na(nbempe_16) ~ nbempe_16, 
  !is.na(nbemp_13)  ~ nbemp_13, 
  !is.na(v1nbemp)   ~ v1nbemp
)) 
# tab_many(ct, NBEMPall, ANNEE)



ct$INCIDENTb <- ctraw$INCIDENT %>% fct_recode(
  "1-Autonomie cas imprévus" = "1",
  "2-Pas autonomie imprévus" = "2", 
  "2-Pas autonomie imprévus" = "3",
  "NULL"  = "8",
  "NULL"  = "9",
)

ct$INCIDENTc <- ctraw$INCIDENT %>% fct_recode(
  "1-Autonomie incidents" = "1",
  "1-Autonomie incidents" = "2", 
  "2-Pas autonomie incid" = "3",
  "NULL"  = "8",
  "NULL"  = "9",
)

ct$AGE4  <- ctraw$AGEQ %>% fct_recode(
  "15-15 à 29ans"  = "15",
  "15-15 à 29ans"  = "20",
  "15-15 à 29ans"  = "25",
  "30-30 à 39ans"  = "30",
  "30-30 à 39ans"  = "35",
  "40-40 à 49ans"  = "40",
  "40-40 à 49ans"  = "45",
  "50-+50ans"      = "50",
  "50-+50ans"      = "55",
  "50-+50ans"      = "60",
  "50-+50ans"      = "65",
  "50-+50ans"      = "70",
  "50-+50ans"      = "75"
)



## Nouvelle variable employeur ----

#Emp à partir en premier lieu des déclarations des salariés (2005 comme ensuite)
# ct$EMP <- ctraw %>%
#   bind_cols(select(ct, NBSALENTC2)) %>% 
#   transmute(new_var = as.factor(
#    # ... {TOO LONG}... 
#     )
#   )) %>% deframe()



   
## Employeur, plus strict entreprises taille inconnue ----
ct$NBSALENTCstrict <- as.factor(case_when(
  ct$ANNEE != "2005"                            ~ as.character(ct$NBSALENTC),
  ct$NBSALA2 == ct$NBSALB                       ~ as.character(ct$NBSALA2), 
  is.na(ct$NBSALB) & ct$NBSALA2 == "3-500 et +" ~ as.character(ct$NBSALA2),
  is.na(ct$NBSALB)                              ~ NA_character_,
  TRUE                                          ~ as.character(ct$NBSALB), 
))
## tab(ct, NBSALENTCstrict, NBSALENTC, ANNEE) #Idem NBSALENTC sur 2013:2019, sans NA administrations
## tab(ct, NBSALENTCstrict, ANNEE, pct = "col", wt = pondcal) 
## tab(ct, CHPUB_05, NBSALENTCstrict, ANNEE, pct = "col", wt = pondcal)

   
   
## Nouvelles variables orga ----

# Discussions sur l'organisation du travail 
# ct <- ct |> mutate(ct, ORGA = fct_drop(fct_cross(COLLECT, fct_explicit_na(REUNION, "NA"))))
# tabxplor:::fct_recode_helper(ct, all_of("ORGA"))
ct$ORGA <- fct_recode(
  fct_drop(fct_cross(ct$COLLECT, fct_explicit_na(ct$REUNION, "NA"))), 
  "1-Réunion orga"                = "1-Discussion orga:1-Réunion orga",
  "2-Discussion orga"             = "1-Discussion orga:2-Pas réu orga",
  "2-Discussion orga"             = "1-Discussion orga:NA"            ,
  "3-Pas disc orga"               = "2-Pas disc orga:NA"              
)





ct <- ct |> mutate(
  RP_ORGA2 = fct_recode(RP_ORGA, 
                        "1-Pas pouvoir s'organiser" = "1-Peut s'organiser: PDT d’accord", 
                        "1-Pas pouvoir s'organiser" = "2-Peut s'organiser: Pas d’accord", 
                        "2-Pouvoir s'organiser"     = "3-Peut s'organiser: D’accord", 
                        "2-Pouvoir s'organiser"     = "4-Peut s'organiser: TAF d’accord")
)



ct$RWTECH2 <- as.factor(case_when(
  ct$RWDEP == "1-CR chaîne" | ct$RWCAD == "1-CR cadence" |
    ct$RWTECH == "1-CR tech"                              ~ "1-CR automatiques", 
  is.na(ct$RWDEP) & is.na(ct$RWCAD ) & is.na(ct$RWTECH)   ~ NA_character_ ,
  TRUE                                                    ~ "2-Pas CR auto"
))



ct$ACCHEF2 <- fct_recode(
  ct$ACCHEF,
  "1-Sv désaccord chef" = "1-Tj désaccord chef",
  "1-Sv désaccord chef" = "2-Sv désaccord chef",
  "2-Parfois désaccord chef" = "3-Pf désaccord chef",
  "3-Jm désaccord chef" = "4-Jm désaccord chef"
)


ct$RP_INUTILE2 <- fct_recode(
  ct$RP_INUTILE,
  "1-Parfois choses inutiles" = "1-Choses inutiles: Toujours",
  "1-Parfois choses inutiles" = "2-Choses inutiles: Souvent" ,
  "1-Parfois choses inutiles" = "3-Choses inutiles: Parfois" ,
  "2-Jamais choses inutiles"  = "4-Choses inutiles: Jamais"  
)

ct$RP_MAUVAIS_TRAVAIL2 <- fct_recode(
  ct$RP_MAUVAIS_TRAVAIL,
  "1-Souvent sacrifier qualité"  = "1-Sacrifier la qualité: Toujours",
  "1-Souvent sacrifier qualité"  = "2-Sacrifier la qualité: Souvent" ,
  "2-Rarement sacrifier qualité" = "3-Sacrifier la qualité: Parfois" ,
  "2-Rarement sacrifier qualité" = "4-Sacrifier la qualité: Jamais"  
)


ct$RP_SUP_EVAL2 <- fct_recode(
  ct$RP_SUP_EVAL,
  "1-Éval connaissent pas travail" = "1-Éval connaissent: PDT d’accord",
  "1-Éval connaissent pas travail" = "2-Éval connaissent: Pas d’accord",
  "2-Éval connaissent travail"     = "3-Éval connaissent: D’accord"    ,
  "2-Éval connaissent travail"     = "4-Éval connaissent: TAF d’accord",
  "5-Non concerné"                 = "5-Non concerné"                  
)

ct$CONFSAL2 <- fct_recode(
  ct$CONFSAL,
  "1-Chef fait tj confiance" = "1-Tj chef fait confiance",
  "2-Chef fait sv confiance"  = "2-Sv chef fait confiance",
  "3-Chef fait rarement confiance" = "3-Pf chef fait confiance",
  "3-Chef fait rarement confiance" = "4-Jm chef fait confiance"
)

ct$INFOCONF2 <- fct_recode(
  ct$INFOCONF,
  "1-Tj confiance infos chef"       = "1-Tj confiance infos chef",
  "2-Sv confiance infos chef"       = "2-Sv confiance infos chef",
  "3-Rarement confiance infos chef" = "3-Pf confiance infos chef",
  "3-Rarement confiance infos chef" = "4-Jm confiance infos chef"
)





ct$COMMENT2 <- case_when(
  ct$COMMENT == "2-Pas autonomie procédurale"    ~ factor("3-Pas autonomie procédurale", c("1-Pas de consignes",
                                                                             "2-Autonomie procédurale",
                                                                             "3-Pas autonomie procédurale")), 
  ct$STARK   == "1-Pas de consignes"      ~ factor("1-Pas de consignes"  ), 
  ct$COMMENT == "1-Autonomie procédurale" ~ factor("2-Autonomie procédurale"), 
) # On perd 1% de pas de consignes qui ont dit pas d'autonomie d'exécution.


ct$MONOTON2 <- fct_recode(
  ct$MONOTON,
  "1-Souvent monotone" = "1-Tj monotone",
  "1-Souvent monotone" = "2-Sv monotone",
  "2-Rarement monotone" = "3-Pf monotone",
  "2-Rarement monotone" = "4-Jm monotone"
)

ct$RPE_EQUIPE2 <- fct_recode(
  ct$RPE_EQUIPE,
  "1-Souvent partie équipe"  = "1-Faire partie équipe: Toujours",
  "1-Souvent partie équipe"  = "2-Faire partie équipe: Souvent" ,
  "2-Rarement partie équipe" = "3-Faire partie équipe: Parfois" ,
  "2-Rarement partie équipe" = "4-Faire partie équipe: Jamais"  
)

ct$TRAVSEUL2 <- fct_recode(
  ct$TRAVSEUL,
  "1-Tj seul"     = "1-Tj seul",
  "2-Pas tj seul" = "2-Sv seul",
  "2-Pas tj seul" = "3-Pf seul",
  "2-Pas tj seul" = "4-Jm seul"
)

ct$TRAVSEUL3 <- fct_recode(
  ct$TRAVSEUL,
  "1-Souvent seul"   = "1-Tj seul",
  "1-Souvent seul"   = "2-Sv seul",
  "2-Rarement seul"  = "3-Pf seul",
  "2-Rarement seul"  = "4-Jm seul"
)

ct$OBJMODIF2 <- fct_recode(
  ct$OBJMODIF,
  "NULL"                    = "1-Obj modif seul"     ,
  "1-Objectifs négociables" = "2-Obj modif chef"     ,
  "1-Objectifs négociables" = "3-Obj modif collectif",
  "2-Obj non négociables"   = "4-Obj pas modif"      
)

ct$OBJVRAI2 <- fct_recode(
  fct_explicit_na(ct$OBJMODIF, "NA"),
  "2-Pas obj chiffrés"   = "1-Obj modif seul"     ,
  "1-Objectifs chiffrés" = "2-Obj modif chef"     ,
  "1-Objectifs chiffrés" = "3-Obj modif collectif",
  "1-Objectifs chiffrés" = "4-Obj pas modif"      , 
  "2-Pas obj chiffrés"   = "NA"
)


ct$POLY2 <- fct_recode(
  ct$POLY,
  "1-Rotation des postes" = "1-Rotation postes"    ,
  "1-Rotation des postes" = "2-Chg poste au besoin",
  "2-Poste unique"        = "3-Poste unique"       
)

ct$STARK3 <- forcats::fct_recode(ctraw$STARK,
                                 "3-Consignes: respect strict"="1",
                                 "2-Consignes: latitude"="2",
                                 "2-Consignes: latitude"="3",
                                 "1-Pas de consignes"="4",
                                 "NULL"="8",
                                 "NULL"="9",
                                 
) |> 
  fct_relevel(sort) 

ct$RP_PLAISE3 <- fct_recode(
  ct$RP_PLAISE,
  "1-Souvent choses plaisantes"  = "1-Choses qui me plaisent: Toujours",
  "1-Souvent choses plaisantes"  = "2-Choses qui me plaisent: Souvent" ,
  "2-Parfois choses plaisantes"  = "3-Choses qui me plaisent: Parfois" ,
  "3-Jamais choses plaisantes"   = "4-Choses qui me plaisent: Jamais"  
)

ct$RP_PLAISE2 <- fct_recode(
  ct$RP_PLAISE,
  "1-Souvent choses plaisantes"  = "1-Choses qui me plaisent: Toujours",
  "1-Souvent choses plaisantes"  = "2-Choses qui me plaisent: Souvent" ,
  "2-Rarement choses plaisantes" = "3-Choses qui me plaisent: Parfois" ,
  "2-Rarement choses plaisantes" = "4-Choses qui me plaisent: Jamais"  
)




# ct$RP_SUB_PAR_SUPERIEUR <- fct_recode(
#   ct$RP_SUB_PAR_SUPERIEUR,
#   "1-Par supérieurs" = "1-Par supérieurs",
#   "2-Pas sup"        = "2-Pas sup"       
# )



ct$CHEF_CRITIQUE <- 
  if_else(ct$RP_SUB_CRITIQUE == "1-Critique injustement travail" & ct$RP_SUB_PAR_SUPERIEUR == "1-Par supérieurs", 
          true  = "1-Chef critique injustement travail", 
          false = "2-Chef critique pas")

ct$CHEF_IGNORE <- 
  if_else(ct$RP_SUB_IGNORE == "1-Comme si pas là" & ct$RP_SUB_PAR_SUPERIEUR == "1-Par supérieurs", 
          true  = "1-Chef m'ignore", 
          false = "2-Chef m'ignore pas")

ct$CHEF_TAIRE <- 
  if_else(ct$RP_SUB_TAIRE == "1-Empêchent exprimer" & ct$RP_SUB_PAR_SUPERIEUR == "1-Par supérieurs", 
          true  = "1-Chef empêche exprimer", 
          false = "2-Chef m'ignore pas")








ct$DIPLOME4 <- fct_recode(
  ct$DIPLOME,
  "1-Brevet ou -"  = "0-Aucun diplôme"      ,
  "1-Brevet ou -"  = "1-Certificat d'études",
  "1-Brevet ou -"  = "2-Brevet"             ,
  "2-CAP-BEP"      = "3-CAP BEP"            ,
  "3-Bac"          = "4-Bac pro"            ,
  "3-Bac"          = "5-Bac général"        ,
  "4->=Bac+2"      = "6-Bac+2"              ,
  "4->=Bac+2"      = "7-Bac+4"              ,
  "4->=Bac+2"      = "8-Bac+5"              
)


ct$PUBLICtel <- fct_recode(
  ct$PUBLICtel,
  "1-Toujours contact téléphone + face-à-face" = "1-Toujours téléphone avec public",
  "2-Contact avec le public"                   = "2-Contact avec public"           ,
  "5-Pas contact avec le public"               = "5-Pas contact avec public"       
)

ct$OBJATTEIN3 <- fct_recode(
  ct$OBJATTEIN,
  "2-Obj souvent difficile à atteindre"    = "1-Tj du mal objectifs",
  "2-Obj souvent difficile à atteindre"    = "2-Sv du mal"          ,
  "3-Obj parfois difficile à atteindre"= "3-Pf du mal"          ,
  "4-Obj facile à atteindre"        = "4-Jm du mal"          
)

ct$INITIAT3 <- fct_recode(
  ct$INITIAT,
  "1-Toujours initiatives" = "1-Tj initiatives",
  "2-Souvent initiatives"  = "2-Sv initiatives",
  "3-Rarement initiatives" = "3-Pf initiatives",
  "3-Rarement initiatives" = "4-Jm initiatives"
)

ct$IDEE3 <- fct_recode(
  ct$IDEE,
  "1-Tj propres idées"       = "1-Tj propres idées",
  "2-Sv propres idées"       = "2-Sv propres idées",
  "3-Rarement propres idées" = "3-Pf propres idées",
  "3-Rarement propres idées" = "4-Jm propres idées"
)



## Variables santé RPS ---------------------

# Sources : 
# - Rapport Gollac 2010
# - Davie DGAFP 2014
# - Corinne PERRAUDIN, Nadine THEVENOT et Sophie DESSEIN, Conditions de travail et préventions
#  des risques professionnels dans le travail en sous-traitance, Paris, DARES, 2022



# Risques physiques :
ct$ENTENDR2 <- fct_recode(
  ct$ENTENDR,
  "2-Pas bruyant"  = "1-Pas bruyant" ,
  "1-Bruyant"      = "2-Bruyant"     ,
  "1-Bruyant"      = "3-Très bruyant"
) |> fct_relevel(sort)

vars_risques_phy <- 
  c("CWDEBOU" , #Rester longtemps debout
    "CWPOSTU" , #Rester longtemps dans une posture pénible
    "CWDEPLA" , #Effectuer des déplacements à pied longs ou fréquents
    "CWLOURD" , #Porter ou déplacer des charges lourdes
    "CWMVT"   , #Effectuer des mouvements douloureux ou fatigants
    "CWVIB"   , #Subir des secousses ou vibrations
    "ENTENDR2" , #Etre exposé à un bruit intense
    "SECFUPOU", #Respirer des fumées ou des poussières
    "SECTOXNO" #Etre en contact avec des produits dangereux
  )
# vars_risques_phy %in% names(ct)

# vars_risques_phy |> map(~pull(ct, .) |> levels())
ct <- ct |> score_from_lv1("score_risques_phy", vars_risques_phy)
# tab(ct, score_risques_phy)




# Intensité du travail (dont charge mentale et contradictions) 
#   déjà présent dans formes d'organisation du travail

# Manque d’autonomie et de marges de manœuvre :
ct$CONSIGN <- fct_recode(
  ct$STARK,
  "1-Consignes respect strict" = "4-Consignes respect strict",
  "2-Consignes pas strict"     = "1-Pas de consignes"        ,
  "2-Consignes pas strict"     = "2-Consignes sv autrement"  ,
  "2-Consignes pas strict"     = "3-Consignes pf autrement"  ,
) |> 
  fct_relevel(sort)

ct$ENNUI <- fct_recode(
  ct$RPE_ENNUI,
  "1-Parfois ennui"   = "1-Ennui: Toujours",
  "1-Parfois ennui"   = "2-Ennui: Souvent" ,
  "1-Parfois ennui"   = "3-Ennui: Parfois" ,
  "2-Jamais d'ennui"  = "4-Ennui: Jamais"  
) |> 
  fct_relevel(sort)

ct$RP_COMPETENCES2 <- fct_recode(
  ct$RP_COMPETENCES,
  "1-Absence dvlp compétences" = "1-Développe compétences: PDT d’accord",
  "1-Absence dvlp compétences" = "2-Développe compétences: Pas d’accord",
  "2-Dvlp compétences"         = "3-Développe compétences: D’accord"    ,
  "2-Dvlp compétences"         = "4-Développe compétences: TAF d’accord",
  "NULL"                       = "5-Non concerné"                       
) |> 
  fct_relevel(sort)

# vars_autonomie <- 
#   c(
#     "COMMENT",         #Devoir suivre les indications données par les supérieurs
#     "CONSIGN",         #Appliquer strictement les consignes
#     "INCIDENT",        #Faire appel généralement à d'autres en cas d'incident
#     "DELAIS",          #Ne pas pouvoir faire varier les délais fixés
#     "INTERUP",         #Ne pas pouvoir interrompre momentanément son travail quand on le souhaite
#     "RP_ORGA2",        #Ne pas pouvoir organiser son travail de la manière qui convient le mieux
#     "ENNUI",           #Eprouver toujours, souvent ou parfois de l'ennui dans son travail
#     "NOUVELLE",        #Le travail ne permet pas d'apprendre des choses nouvelles
#     "RP_COMPETENCES2", #Ne pas avoir l'occasion de développer des compétences professionnelles
#     "CHAINE",          #Travailler à la chaine
#     "REPETE",          #Répéter continuellement une même série de gestes/opérations
#     "CYCLE",           #Chaque série de gestes/opérations répétés dure moins d'une minute
#     "QUANTI",          #Ne jamais pouvoir intervenir sur la quantité de travail attribuée
#     "INITIAT",         #Travail ne nécessitant jamais ou parfois prendre d'initiatives
#     "IDEE"             #Jamais ou parfois pouvoir mettre ses propres idées en pratique dans travail
#   )
# # vars_autonomie %in% names(ct)


#Insécurité
ct$RP_ANTICIPER2 <- fct_recode(
  ct$RP_ANTICIPER,
  "1-Jamais anticiper mois sv" = "4-Peut anticiper mois sv : Jamais"  ,
  "2-Anticiper mois sv"        = "1-Peut anticiper mois sv : Toujours",
  "2-Anticiper mois sv"        = "2-Peut anticiper mois sv : Souvent" ,
  "2-Anticiper mois sv"        = "3-Peut anticiper mois sv : Parfois" ,
) |> 
  fct_relevel(sort)


ct$RP_CHANGEMENTS2 <- fct_recode(
  ct$RP_CHANGEMENTS,
  "1-Souvent chg mal préparés" = "1-Chg mal préparés: Toujours",
  "1-Souvent chg mal préparés" = "2-Chg mal préparés: Souvent" ,
  "2-Rarement chr mp"          = "3-Chg mal préparés: Parfois" ,
  "2-Rarement chr mp"          = "4-Chg mal préparés: Jamais"  
)

ct$RPE_DEPASSE2 <- fct_recode(
  ct$RPE_DEPASSE,
  "1-Souvent dépassé chg"  = "1-Dépassé chg: Toujours",
  "1-Souvent dépassé chg"  = "2-Dépassé chg: Souvent" ,
  "2-Rarement dépassé chg" = "3-Dépassé chg: Parfois" ,
  "2-Rarement dépassé chg" = "4-Dépassé chg: Jamais"  
)

ct$CHANG_INFOCONS <- 
  if_else(!is.na(ct$CHGTINFO) & ct$CHGTINFO == "2-ENVMOD pas infos" & 
            !is.na(ct$CHGTCONS) & ct$CHGTCONS == "2-ENVMOD pas consulté", 
          factor("1-Chg sans infos ni consultation", c("1-Chg sans infos ni consultation", 
                                                       "2-Pas chg sans info conf")), 
          factor("2-Pas chg sans info conf")
  )

ct$METIER2 <- fct_recode(
  ct$METIER,
  "1-Changer de métier"     = "2-Changer de métier"    ,
  "2-Pas changer de métier" = "1-Pas changer de métier",
  "NULL"                    = "98"                     
) |> fct_relevel(sort)

ct$TENIR2 <- fct_recode(
  ct$TENIR,
  "1-Pas tenir jsq retraite"  = "2-Pas tenir" ,      
  "2-Tenir jusqu'à retraite" = "1-Tenir jusqu'à retraite",
) |> fct_relevel(sort)

ct$CHANGOP2 <- fct_recode(
  fct_explicit_na(ct$CHANGOP, "2-Pas changement nég"),
  "1-Changement négatif" = "2-ENVMOD négatif"      ,
  "2-Pas changement nég" = "1-ENVMOD positif"      ,
  "2-Pas changement nég" = "3-ENVMOD se compensent"
) |> fct_relevel(sort)




vars_insecurite16 <- 
  c(
    #	Insécurité économique :
    "CRAINTE", #Crainte pour son emploi dans l'année
    "METIER2",  #Devoir changer de qualification ou de métier dans les 3 prochaines années
    "TENIR2",   #Ne pas se sentir capable de faire le même travail jusqu'à la retraite
    "MUTE",    #Craindre d'être muté à un autre poste de travail contre sa volonté
    "RP_ANTICIPER2",   #Ne jamais savoir à l'avance les tâches pour le mois suivant
    "RP_CHANGEMENTS2",   #Vivre toujours ou souvent des changements imprévisibles ou mal préparés
    "RPE_DEPASSE2",   #Avoir toujours ou souvent le sentiment d'être dépassé par les chgments trop ra-pides
    # Changements de l'environnement de travail :
    "FORTMOD3", #L'environnement de travail a été modifié par un changement de structure
    "FORTMOD4",   #L'environnement de travail a été modifié par un changement organisationnel
    "CHANGOP2",   #Changements plutôt négatifs pour son travail
    "CHANG_INFOCONS"  #Ne pas avoir été consulté ni reçu d'information suffisante au moment chgements
  )
# vars_insecurite %in% names(ct)

vars_insecurite <- 
  c(#	Insécurité économique :
    "CRAINTE", "METIER2", "TENIR2", "RP_CHANGEMENTS2",      
    #"RP_ANTICIPER2", "RPE_DEPASSE2", #Pas 2019
    #"MUTE",                          #Pas 2013
    
    # Changements de l'environnement de travail :
    "FORTMOD3", "FORTMOD4", "CHANGOP2", "CHANG_INFOCONS"  
  )

vars_insecurite13 <- 
  c(#	Insécurité économique :
    "CRAINTE", "METIER2", "TENIR2", "RP_CHANGEMENTS2",      
    "RP_ANTICIPER2", "RPE_DEPASSE2", #Pas 2019
    #"MUTE",                         #Pas 2013
    
    # Changements de l'environnement de travail :
    "FORTMOD3", "FORTMOD4", "CHANGOP2", "CHANG_INFOCONS"  
  )

# vars_insecurite16 |> map(~pull(ct, .) |> levels())
ct <- ct |> score_from_lv1("score_insecurite16", vars_insecurite16)
ct <- ct |> score_from_lv1("score_insecurite"  , vars_insecurite  )
ct <- ct |> score_from_lv1("score_insecurite13", vars_insecurite13)
# tab(ct[`2016`,], score_insecurite16, pct = "col")
# tab(ct[`2013` | `2016` | `2019`,], score_insecurite, pct = "col")
# tab(ct[`2013`,], score_insecurite13, pct = "col")

#tab_many(ct[`2016`,], vars_insecurite16, pct = "col", wt = pondqaa)
#tabxplor:::fct_recode_helper(ct16, vars_insecurite16) # ct16 <- ct[`2016`,]




# Rapports sociaux au travail
ct$TRAVSEUL4 <- fct_recode(
  ct$TRAVSEUL,
  "1-Toujours seul" = "1-Tj seul",
  "2-Rarement seul" = "2-Sv seul",
  "2-Rarement seul" = "3-Pf seul",
  "2-Rarement seul" = "4-Jm seul"
)


ct$CHGTCOLL2 <- fct_recode(
  fct_explicit_na(ct$CHGTCOLL, "2-Pas pb chg collègues"),
  "1-Certains collègues changé" = "2-Certains changé",
  "1-Certains collègues changé" = "3-Plupart changé" ,
  "2-Pas pb chg collègues"      = "1-Mêmes collègues",
) |> fct_relevel(sort)


ct$AIDCHEF2 <- fct_recode(
  ct$AIDCHEF,
  "1-Absence aide chef" = "2-Pas aide chef",
  "2-Aide chef ok"   = "1-Aide du chef" ,
  "2-Aide chef ok"   = "3-Pas de chef"  
) |> fct_relevel(sort)


ct$AIDCOLL2 <- fct_recode(
  ct$AIDCOLL,
  "1-Absence aide collègues" = "2-Pas aide coll"  ,
  "2-Aide collègues ok" = "1-Aide collègues" ,
  "2-Aide collègues ok" = "3-Pas de collègue"
) |> fct_relevel(sort)


ct$COLLECT2 <- fct_recode(
  ct$COLLECT,
  "1-Absence discussion orga" = "2-Pas disc orga"  ,
  "2-Discussion orga"         = "1-Discussion orga",
) |> fct_relevel(sort)


ct$ACCHEF3 <- fct_recode(
  ct$ACCHEF,
  "1-Souvent désaccord chef"  = "1-Tj désaccord chef",
  "1-Souvent désaccord chef"  = "2-Sv désaccord chef",
  "2-Rarement désaccord chef" = "3-Pf désaccord chef",
  "2-Rarement désaccord chef" = "4-Jm désaccord chef"
) |> fct_relevel(sort)


ct$DICHEF2 <- fct_recode(
  fct_explicit_na(ct$DICHEF, "2-Pas pb pas discuter chef"),
  "1-Désaccord chef: pas discuter" = "2-Désaccord chef: pas discuter",
  "2-Pas pb pas discuter chef"     = "1-Désaccord chef: discuter"    ,
) |> fct_relevel(sort)


ct$ACCOL2 <- fct_recode(
  ct$ACCOL,
  "1-Souvent désaccord collègues" = "1-Tj désaccord collègues",
  "1-Souvent désaccord collègues" = "2-Sv désaccord collègues",
  "2-Peu désaccord collègues" = "3-Pf désaccord collègues",
  "2-Peu désaccord collègues" = "4-Jm désaccord collègues",
  "2-Peu désaccord collègues" = "5-Pas de collègues"      
) |> fct_relevel(sort)


ct$CONFSAL2 <- fct_recode(
  ct$CONFSAL,
  "1-Chef fait rarement confiance" = "3-Pf chef fait confiance",
  "1-Chef fait rarement confiance" = "4-Jm chef fait confiance",
  "2-Chef fait souvent confiance"  = "1-Tj chef fait confiance",
  "2-Chef fait souvent confiance"  = "2-Sv chef fait confiance",
) |> fct_relevel(sort)


ct$INFOCONF2 <- fct_recode(
  ct$INFOCONF,
  "1-Rarement confiance infos chef" = "3-Pf confiance infos chef",
  "1-Rarement confiance infos chef" = "4-Jm confiance infos chef",
  "2-Souvent confiance infos chef"  = "1-Tj confiance infos chef",
  "2-Souvent confiance infos chef"  = "2-Sv confiance infos chef",
) |> fct_relevel(sort)


ct$TENSION22 <- fct_recode(
  ct$TENSION2,
  "1-Tension chef"  = "1-Tension chef" ,
  "2-Pas tens chef" = "2-Pas tens chef",
  "2-Pas tens chef" = "3-Pas de chef"  
) |> fct_relevel(sort)


ct$TENSION32 <- fct_recode(
  ct$TENSION3,
  "1-Tension collègues" = "1-Tension collègues",
  "2-Pas tension coll"  = "2-Pas tension coll" ,
  "2-Pas tension coll"  = "3-Pas de collègues" 
) |> fct_relevel(sort)


ct$TENSION42 <- fct_recode(
  fct_explicit_na(ct$TENSION4, "2-Pas pb tension sub"),
  "1-Tension subordonnés" = "1-Tension subordonnés",
  "2-Pas pb tension sub"  = "2-Pas tension sub"
) |> fct_relevel(sort)


ct$RP_SUP_ATTE2 <- fct_recode(
  ct$RP_SUP_ATTE,
  "1-Absence d'attention chef" = "1-Sup attention: PDT d’accord",
  "1-Absence d'attention chef" = "2-Sup attention: Pas d’accord",
  "2-Pas pb attention chef"    = "3-Sup attention: D’accord"    ,
  "2-Pas pb attention chef"    = "4-Sup attention: TAF d’accord",
  "2-Pas pb attention chef"    = "5-Non concerné"               
)


ct$RP_SUP_AIDE2 <- fct_recode(
  ct$RP_SUP_AIDE,
  "1-Absence d'aide chef" = "1-Supérieur aide: PDT d’accord",
  "1-Absence d'aide chef" = "2-Supérieur aide: Pas d’accord",
  "2-Pas pb aide chef"           = "3-Supérieur aide: D’accord"    ,
  "2-Pas pb aide chef"           = "4-Supérieur aide: TAF d’accord",
  "2-Pas pb aide chef"         = "5-Non concerné"                
)


ct$RP_COL_AIDE2 <- fct_recode(
  ct$RP_COL_AIDE,
  "1-Absence d'entraide"     = "1-Collègues aident: PDT d’accord",
  "1-Absence d'entraide"     = "2-Collègues aident: Pas d’accord",
  "2-Pas pb aide collègues"  = "3-Collègues aident: D’accord"    ,
  "2-Pas pb aide collègues"  = "4-Collègues aident: TAF d’accord",
  "2-Pas pb aide collègues"  = "5-Non concerné"                  
)


ct$RPE_EQUIPE3 <- fct_recode(
  ct$RPE_EQUIPE,
  "1-Rarement partie équipe" = "3-Faire partie équipe: Parfois" ,
  "1-Rarement partie équipe" = "4-Faire partie équipe: Jamais"  ,
  "2-Souvent partie équipe"  = "1-Faire partie équipe: Toujours",
  "2-Souvent partie équipe"  = "2-Faire partie équipe: Souvent" ,
) |> 
  fct_relevel(sort)


ct$COLL_IGNORE <- as.factor(if_else(ct$RP_SUB_IGNORE == "1-Comme si pas là" & 
                                      ct$RP_SUB_PAR_COLLEGUE == "1-Par collègues", 
                                    "1-Collègues ignorent", 
                                    "2-Pas coll ignorent"
))


ct$COLL_TAIRE <- as.factor(if_else(ct$RP_SUB_TAIRE == "1-Empêchent exprimer"  & 
                                     ct$RP_SUB_PAR_COLLEGUE == "1-Par collègues",
                                   "1-Collègues empêchent exprimer", 
                                   "2-Pas coll emp exprimer"
))


# ct$COLL_AGR_VERB <- as.factor(if_else(ct$RP_AGR_VERB_COL  == "1-Agression verbale collègues ou sup" & 
#                                         ct$RP_AGR_PAR_COL == "1-Par un collègue",
#                                    "1-Collègues agression verbale", 
#                                    "2-Pas coll agr verbale"
# ))
# 
# ct$CHEF_AGR_VERB <- as.factor(if_else(ct$RP_AGR_VERB_COL  == "1-Agression verbale collègues ou sup" & 
#                                         ct$RP_AGR_PAR_SUP == "1-Par un supérieur",
#                                       "1-Chef agression verbale", 
#                                       "2-Pas chef agr verbale"
# ))





#Comportements hostiles
#Attention : on ne peut pas savoir s'ils viennent du chef ou des collègues en 2013
# => il faut distinguer entre intérieur et extérieur à l'organisation

## Les deux tiers subissent plusieurs comportements hostiles en même temps. 
##   parmi eux, seulement 6% ont été agressés à la fois par le chef et le public
##              seulement 5% par les collègues et le public
##              36% par le chef mais pas le public (dont 11% avec les collègues aussi)
##              40% par les collègues mais pas le public (dont 11% avec le chef aussi)
##              16 % par le public, dont 7% seul et 9% avec chef ou collègues
##              8% restant ont subi un comportement hostile sans dire qui c'était
##  dit autrement : 62% ont indiqué une seule source ; 18 % deux sources ; 2% les trois
## => On ne dit pas n'importe quoi si on isole les comportements hostiles des membres de 
##  la même organisation ; mais on dit n'importe quoi si on isole ceux du public ;
##  Pour ce qui est d'isoler le chef ou les collègues, il y a un quart/un cinquième de cas
##   où l'on est pas certain de quel comportement renvoie à quel acteur.
# ggplot(data = ct[salariat & `2013`, ], aes(x = score_hostile)) + geom_histogram()
# tab_many(ct[salariat & ct$score_hostile >= 1, ], score_hostile, ANNEE, pct = "col")
# tab_many(ct[salariat & ct$score_hostile >= 1 & `2016`, ],  RP_SUB_PAR_COLLEGUE,
#          RP_SUB_PAR_PUBLIC, RP_SUB_PAR_SUPERIEUR, wt = pondqaa, pct = "all_tabs")
# ct[salariat & ct$score_hostile >= 1 & `2016` & !is.na(ct$pondqaa), ] |>
#   score_from_lv1("hostile_nb_source", c("RP_SUB_PAR_SUPERIEUR", "RP_SUB_PAR_COLLEGUE",
#   "RP_SUB_PAR_PUBLIC")) |>
#   tab_many(hostile_nb_source, wt = pondqaa, pct = "col")

ct <- ct |> 
  mutate(across(c(RP_SUB_IGNORE, RP_SUB_TAIRE, RP_SUB_RIDICULISE, RP_SUB_CRITIQUE, RP_SUB_INUTILE, 
                  RP_SUB_SABOTE, RP_SUB_FOU, RP_SUB_DEGRADANT, RP_SUB_AVANCES, RP_SUB_MOQUE), 
                ~ if_else(RP_SUB_PAR_ORGA == "1-Par qq orga", ., factor(levels(.)[2])), 
                .names = "RP_ORGA_{.col}"
  )) |> 
  rename_with(.cols = starts_with("RP_ORGA_RP_SUB_"), .fn = ~ str_remove(., "RP_SUB_")) |> 
  
  mutate(across(c(RP_SUB_IGNORE, RP_SUB_TAIRE, RP_SUB_RIDICULISE, RP_SUB_CRITIQUE, RP_SUB_INUTILE, 
                  RP_SUB_SABOTE, RP_SUB_FOU, RP_SUB_DEGRADANT, RP_SUB_AVANCES, RP_SUB_MOQUE), 
                ~ if_else(RP_SUB_PAR_PUBLIC  == "1-Par qq public", ., factor(levels(.)[2])), 
                .names = "RP_PUBLIC_{.col}"
  )) |> 
  rename_with(.cols = starts_with("RP_PUBLIC_RP_SUB_"), .fn = ~ str_remove(., "RP_SUB_"))



#Comportements méprisants
ct$MEPRISANT <- as.factor(if_else(ct$RP_SUB_IGNORE       == "1-Comme si pas là"
                                  | ct$RP_SUB_TAIRE      == "1-Empêchent exprimer"  
                                  | ct$RP_SUB_RIDICULISE == "1-Ridiculisent en public" ,
                                  #| ct$RP_SUB_MOQUE      == "1-Se moque de moi" , 
                                  "1-Comportements méprisants", 
                                  "2-Pas comp méprisants"
))


ct$ORGA_MEPRISANT <- as.factor(if_else(ct$MEPRISANT == "1-Comportements méprisants"
                                       & ct$RP_SUB_PAR_ORGA == "1-Par qq orga",
                                       "1-Chef colls méprisants", 
                                       "2-Pas méprisant"
))

ct$PUBLIC_MEPRISANT <- as.factor(if_else(ct$MEPRISANT == "1-Comportements méprisants"
                                         & ct$RP_SUB_PAR_PUBLIC == "1-Par qq public",
                                         "1-Public méprisant", 
                                         "2-Pas méprisant"
))


ct$CHEF_MEPRISANT <- as.factor(if_else(ct$MEPRISANT == "1-Comportements méprisants"
                                       & ct$RP_SUB_PAR_SUPERIEUR == "1-Par supérieurs",
                                       "1-Chef méprisant", 
                                       "2-Pas chef méprisant"
))

ct$COLL_MEPRISANT <- as.factor(if_else(ct$MEPRISANT == "1-Comportements méprisants"
                                       & ct$RP_SUB_PAR_COLLEGUE == "1-Par collègues", 
                                       "1-Collègues méprisants", 
                                       "2-Pas collègues méprisants"
))

#Déni de reconnaissance
ct$DENI_RECO <- as.factor(if_else(ct$RP_SUB_CRITIQUE  == "1-Critique injustement travail" 
                                  | ct$RP_SUB_INUTILE == "1-Tâches inutiles/dégradantes"  
                                  | ct$RP_SUB_SABOTE  == "1-Sabote mon travail", 
                                  "1-Déni de reconnaissance", 
                                  "2-Pas déni reco"
))

ct$ORGA_DENI_RECO <- as.factor(if_else(ct$DENI_RECO == "1-Déni de reconnaissance"
                                       & ct$RP_SUB_PAR_ORGA == "1-Par qq orga", 
                                       "1-Chef colls reconnaissent pas", 
                                       "2-Pas déni reco"
))

ct$PUBLIC_DENI_RECO <- as.factor(if_else(ct$DENI_RECO == "1-Déni de reconnaissance"
                                         & ct$RP_SUB_PAR_PUBLIC == "1-Par qq public",
                                         "1-Public reconnaît pas", 
                                         "2-Pas déni reco"
))


ct$CHEF_DENI_RECO <- as.factor(if_else(ct$DENI_RECO == "1-Déni de reconnaissance"
                                       & ct$RP_SUB_PAR_SUPERIEUR == "1-Par supérieurs", 
                                       "1-Chef ne reconnait pas", 
                                       "2-Pas chef déni reco"
))

ct$COLL_DENI_RECO <- as.factor(if_else(ct$DENI_RECO == "1-Déni de reconnaissance"
                                       & ct$RP_SUB_PAR_COLLEGUE == "1-Par collègues", 
                                       "1-Collègues reconnaissent pas", 
                                       "2-Pas collègues déni reco"
))



#Atteintes degradantes 
ct$DEGRADANT <- as.factor(if_else(ct$RP_SUB_FOU         == "1-Traite de fou" 
                                  | ct$RP_SUB_DEGRADANT == "1-Paroles dégradantes" 
                                  | ct$RP_SUB_AVANCES   == "1-Propositions sexuelles", 
                                  "1-Atteinte dégradante", 
                                  "2-Pas atteinte dégradante"
))

ct$ORGA_DEGRADANT <- as.factor(if_else(ct$DEGRADANT == "1-Atteinte dégradante"
                                       & ct$RP_SUB_PAR_ORGA == "1-Par qq orga", 
                                       "1-Chef colls dégradants", 
                                       "2-Pas dégradants"
))

ct$PUBLIC_DEGRADANT <- as.factor(if_else(ct$DEGRADANT == "1-Atteinte dégradante"
                                         & ct$RP_SUB_PAR_PUBLIC == "1-Par qq public",
                                         "1-Public dégradant", 
                                         "2-Pas dégradants"
))

ct$CHEF_DEGRADANT <- as.factor(if_else(ct$DEGRADANT == "1-Atteinte dégradante"
                                       & ct$RP_SUB_PAR_SUPERIEUR == "1-Par supérieurs", 
                                       "1-Chef dégradant", 
                                       "2-Pas chef dégradant"
))

ct$COLL_DEGRADANT <- as.factor(if_else(ct$DEGRADANT == "1-Atteinte dégradante"
                                       & ct$RP_SUB_PAR_COLLEGUE == "1-Par collègues", 
                                       "1-Collégues degradants", 
                                       "2-Pas collègues dégradants"
))



# Au moins un comportement hostile
ct$HOSTILE <- as.factor(if_else(ct$MEPRISANT   == "1-Comportements méprisants"
                                | ct$DEGRADANT == "1-Atteinte degradante"  
                                | ct$DENI_RECO == "1-Déni de reconnaissance" ,
                                "1-Au moins un comportement hostile", 
                                "2-Aucun comp hostile"
))

ct$ORGA_HOSTILE <- as.factor(if_else(ct$ORGA_MEPRISANT  == "1-Chef colls méprisants"
                                     | ct$ORGA_DEGRADANT == "1-Chef colls dégradants"
                                     | ct$ORGA_DENI_RECO == "1-Chef colls reconnaissent pas",
                                     "1-Orga comportement hostile >=1", 
                                     "2-Orga aucun comp hostile"
))

ct$PUBLIC_HOSTILE <- as.factor(if_else(ct$PUBLIC_MEPRISANT  == "1-Public méprisant"
                                       | ct$PUBLIC_DEGRADANT == "1-Public dégradant"
                                       | ct$PUBLIC_DENI_RECO == "1-Public reconnaît pas",
                                       "1-Public comportement hostile >=1", 
                                       "2-Public aucun comp hostile"
))

ct$CHEF_HOSTILE <- as.factor(if_else(ct$CHEF_MEPRISANT   == "1-Chef méprisant"
                                     | ct$CHEF_DEGRADANT == "1-Chef degradant"
                                     | ct$CHEF_DENI_RECO == "1-Chef ne reconnait pas",
                                     "1-Chef comportement hostile >=1", 
                                     "2-Chef aucun comp hostile"
))

ct$COLL_HOSTILE <-  as.factor(if_else(ct$COLL_MEPRISANT   == "1-Collègues méprisants"
                                      | ct$COLL_DEGRADANT == "1-Collégues degradants"
                                      | ct$COLL_DENI_RECO == "1-Collègues reconnaissent pas",
                                      "1-Collègues comportement hostile >=1",
                                      "2-Collègues aucun comp hostile"
))









vars_relations16 <-
  c(
    #   Organisation du travail
    "TRAVSEUL4", #Travailler seul, en autonomie
    "CHGTCOLL2", #Certains, la plupart, ou tous les collègues de travail ont changé depuis un an
    #   Manque de soutien social et de coopération
    "AIDCHEF2",  #Ne pas être aidé en cas de travail délicat par les supérieurs hiérarchiques
    "AIDCOLL2",  #Ne pas être aidé en cas de travail délicat par les collègues
    "COLLECT2",  #Absence de discussions collectives sur les questions d'organisation du travail
    "ACCHEF3",   #Etre toujours ou souvent en désaccord avec les supérieurs sur la façon de bien faire son travail
    "DICHEF2",   #Ne pas pouvoir discuter avec les supérieurs en cas de désaccord
    "ACCOL2",    #Etre toujours ou souvent en désaccord avec les collègues sur la façon de bien faire son travail
    "CONFSAL2",  #Absence de confiance de la hiérarchie dans les salariés pour bien faire leur tra-vail
    "INFOCONF2", #Absence de confiance dans les informations provenant de la hiérarchie
    "TENSION22", #Vivre des situations de tension avec les supérieurs hiérarchiques
    "TENSION32", #Vivre des situations de tension avec les collègues
    "TENSION42", #Vivre des situations de tension avec les personnes encadrées
    
    "RP_SUP_ATTE2",    #Absence d'attention prêtée par le supérieur à ce que dit le salarié
    "RP_SUP_AIDE2",    #Absence d'aide du supérieur pour que le salarié mène ses tâches à bien
    "RP_COL_AIDE2",  #Absence d'entraide ou de relation amicale entre collègues
    "RPE_EQUIPE3",   #N'avoir jamais (ou seulement parfois) l'impression de faire partie d'une équipe
    #     Comportements hostiles
    "RP_SUB_IGNORE",  #",Une (ou plusieurs) personnes ignore le salarié ou fait comme s'il n'était pas là
    "RP_SUB_TAIRE",  #",Une (ou plusieurs) personnes empêche le salarié de s'exprimer
    "DENI_RECO",  #",Avoir subi un déni de reconnaissance du travail au cours des 12 derniers mois
    "DEGRADANT", #",Avoir subi des atteintes dégradantes au cours des 12 derniers mois
    "RP_AGR_VERB_COL"  #",Etre victime d'agressions verbales au cours des 12 derniers mois
  )
# vars_relations16 %in% names(ct)


vars_relations <-
  c(#   Organisation du travail
    "TRAVSEUL4", #"CHGTCOLL2",    #Pas 2019
    
    # Manque de soutien social et de coopération
    "AIDCHEF2",      
    "AIDCOLL2",      
    "COLLECT2",      
    #"ACCHEF3",       #Pas 2019
    #"DICHEF2",       #Pas 2019
    #"ACCOL2",        #Pas 2019
    #"CONFSAL2",      #Seulement 2016
    #"INFOCONF2",     #Seulement 2016
    "TENSION22",     
    "TENSION32",     
    "TENSION42",      
    "RP_SUP_ATTE2"#,  
    #"RP_SUP_AIDE2",  #Pas 2019
    #"RP_COL_AIDE2",  #Pas 2019
    #"RPE_EQUIPE3",   #Pas 2019
    
    # Comportements hostiles #Pas 2019
  )

vars_relations13 <-
  c(#   Organisation du travail
    "TRAVSEUL4", "CHGTCOLL2",              #Pas 2019
    
    # Manque de soutien social et de coopération
    "AIDCHEF2", "AIDCOLL2", "COLLECT2",      
    "ACCHEF3", "DICHEF2", "ACCOL2",        #Pas 2019
    #"CONFSAL2", "INFOCONF2",     #Seulement 2016
    "TENSION22", "TENSION32", "TENSION42",      
    "RP_SUP_ATTE2", "RP_SUP_AIDE2",
    "RP_COL_AIDE2", "RPE_EQUIPE3", #,      #Pas 2019
    
    # Comportements hostiles               #Pas 2019
    "RP_SUB_IGNORE", "RP_SUB_TAIRE", "DENI_RECO", "DEGRADANT", "RP_AGR_VERB_COL"
  )

# vars_relations16 |> map(~pull(ct, .) |> levels())
ct <- ct |> score_from_lv1("score_relations16", vars_relations16)
ct <- ct |> score_from_lv1("score_relations"  , vars_relations  )
ct <- ct |> score_from_lv1("score_relations13", vars_relations13)
# tab(ct[salariat & `2016`,], score_relations16, pct = "col")
# tab(ct[salariat & (`2013` | `2016` | `2019`),], score_relations, pct = "col")
# tab(ct[salariat & `2013`,], score_relations13, pct = "col")



# Relations avec les collègues / relations hiérarchiques

# Relations hiérarchiques
vars_relations_chef16 <- c(
  "AIDCHEF2", "ACCHEF3", "DICHEF2", 
  "RP_SUP_ATTE2", "RP_SUP_AIDE2",
  "TENSION22","TENSION42",
  "CONFSAL2", "INFOCONF2",       #Seulement 2016
  
  # Comportements hostiles chef  #Seulement 2016
  "CHEF_IGNORE", "CHEF_TAIRE", "CHEF_DENI_RECO", "CHEF_DEGRADANT"
)

vars_relations_collegues16 <- c(
  "TRAVSEUL4", "CHGTCOLL2",              #Pas 2019
  "AIDCOLL2", "COLLECT2",      
  "ACCOL2",        #Pas 2019
  "TENSION32",       
  
  "RP_COL_AIDE2", "RPE_EQUIPE3", #,      #Pas 2019
  
  # Comportements hostiles collègues     #Seulement 2016
  "COLL_IGNORE", "COLL_TAIRE", "COLL_DENI_RECO", "COLL_DEGRADANT"
)

vars_relations_chef13 <- c(
  "RP_SUP_ATTE2", "AIDCHEF2", "ACCHEF3", "DICHEF2", 
  #"RP_SUP_AIDE2", # double emploi avec AIDCHEF2
  "TENSION22","TENSION42"
)

vars_relations_collegues13 <- c(
  "TRAVSEUL4", "CHGTCOLL2",              #Pas 2019
  "AIDCOLL2", "COLLECT2",      
  "ACCOL2",        #Pas 2019
  "TENSION32",       
  "RP_COL_AIDE2", "RPE_EQUIPE3" #,      #Pas 2019
)

ct <- ct |> score_from_lv1("score_relations_chef16"     , vars_relations_chef16     )
ct <- ct |> score_from_lv1("score_relations_collegues16", vars_relations_collegues16)
ct <- ct |> score_from_lv1("score_relations_chef13"     , vars_relations_chef13     )
ct <- ct |> score_from_lv1("score_relations_collegues13", vars_relations_collegues13)
# tab(ct[salariat & `2016`,], score_relations_chef16, pct = "col")
# tab(ct[salariat & `2016`,], score_relations_collegues16, pct = "col")
# tab(ct[salariat & (`2013` | `2016`),], score_relations_chef13, ANNEE, pct = "col")
# tab(ct[salariat & (`2013` | `2016`),], score_relations_collegues13, ANNEE, pct = "col")


vars_hostile <- c(
  "RP_SUB_IGNORE", "RP_SUB_TAIRE"    , "RP_SUB_RIDICULISE", "RP_SUB_CRITIQUE", "RP_SUB_INUTILE", "RP_SUB_SABOTE", 
  "RP_SUB_FOU"   , "RP_SUB_DEGRADANT", "RP_SUB_AVANCES"   , "RP_SUB_MOQUE" , 
  "RP_AGR_VERB_COL", "RP_AGR_PHYS_COL"
)
vars_hostile_synthese <- c("RP_SUB_IGNORE","RP_SUB_TAIRE", "RP_SUB_MOQUE", "MEPRISANT",
                           "DENI_RECO", "DEGRADANT", "RP_AGR_VERB_COL", "HOSTILE")

vars_orga_hostile <- c(
  "RP_ORGA_IGNORE", "RP_ORGA_TAIRE"    , "RP_ORGA_RIDICULISE", "RP_ORGA_CRITIQUE", "RP_ORGA_INUTILE", "RP_ORGA_SABOTE", 
  "RP_ORGA_FOU"   , "RP_ORGA_DEGRADANT", "RP_ORGA_AVANCES"   , "RP_ORGA_MOQUE" , 
  "RP_AGR_VERB_COL", "RP_AGR_PHYS_COL"
)

vars_orga_hostile_synthese <- c("RP_ORGA_IGNORE","RP_ORGA_TAIRE", "RP_ORGA_MOQUE",
                                "ORGA_MEPRISANT", "ORGA_DENI_RECO", "ORGA_DEGRADANT",
                                "RP_AGR_VERB_COL", "ORGA_HOSTILE")

vars_public_hostile <- c(
  "RP_PUBLIC_IGNORE", "RP_PUBLIC_TAIRE"    , "RP_PUBLIC_RIDICULISE", "RP_PUBLIC_CRITIQUE", "RP_PUBLIC_INUTILE", "RP_PUBLIC_SABOTE", 
  "RP_PUBLIC_FOU"   , "RP_PUBLIC_DEGRADANT", "RP_PUBLIC_AVANCES"   , "RP_PUBLIC_MOQUE" , 
  "RP_AGR_VERB_PUB", "RP_AGR_PHYS_PUB" 
)

vars_public_hostile_synthese <- c("RP_PUBLIC_IGNORE","RP_PUBLIC_TAIRE", "RP_PUBLIC_MOQUE",
                                  "PUBLIC_MEPRISANT", "PUBLIC_DENI_RECO", "PUBLIC_DEGRADANT",
                                  "RP_AGR_VERB_PUB", "PUBLIC_HOSTILE")

#vars_hostile5 <- c("RP_SUB_IGNORE", "RP_SUB_TAIRE", "DENI_RECO", "DEGRADANT", "RP_AGR_VERB_COL")

ct <- ct |> 
  score_from_lv1("score_hostile", vars_hostile) |>
  score_from_lv1("score_orga_hostile", vars_orga_hostile) |>
  score_from_lv1("score_public_hostile", vars_public_hostile)
#tab(ct[salariat & (`2013` | `2016`),], score_hostile, ANNEE, pct = "col")
#ct <- ct |> score_from_lv1("score_hostile5", vars_hostile5)
##tab(ct[salariat & (`2013` | `2016`),], score_hostile5, ANNEE, pct = "col")









#Horaires et organisation du temps de travail :
ct <- ct |> 
  mutate(
    HH2   = as.factor(if_else(HH >= 40, "1-40h ou plus", "2-Moins de 40h")), 
    
    HSUP2 = fct_recode(HSUP,
                       "1-Dépasse souvent horaires"  = "1-Tj dépasse horaires",
                       "1-Dépasse souvent horaires"  = "2-Sv dépasse h"       ,
                       "2-Dépasse rarement horaires" = "3-Pf dépasse h"       ,
                       "2-Dépasse rarement horaires" = "4-Pas dépasse h"      ,
                       "NULL"                        = "8"                    ,
                       "NULL"                        = "9"                    
    ) |> fct_relevel(sort), 
    
    JOINDRE2 = fct_recode(JOINDRE,
                          "1-Joint hors travail"  = "1-Joint hors travail"        ,
                          "2-Pas joint hors trav" = "2-Pas joint hors trav"       ,
                          "2-Pas joint hors trav" = "3-Sans objet: travaille seul"
    ) |> fct_relevel(sort), 
    
    MAISON2 = fct_recode(MAISON,
                         "1-Emporter travail maison" = "1-Tj travail maison",
                         "1-Emporter travail maison" = "2-Sv travail maison",
                         "1-Emporter travail maison" = "3-Pf travail maison",
                         "2-Pas emporter travail"    = "4-Jm travail maison",
                         "2-Pas emporter travail"    = "5-Sans objet (travail à domicile, impossibilité)"
    ) |> fct_relevel(sort), 
    
    MISSION2 = fct_recode(MISSION,
                          "1-Dormir hors maison" = "1-Déplacement 1 /sem ou +",
                          "1-Dormir hors maison" = "2-Déplacement 1 à 3 /mois",
                          "1-Dormir hors maison" = "3-Déplacement parfois"    ,
                          "2-Pas hors maison"    = "4-Pas dormir hors maison" 
    ) |> fct_relevel(sort), 
    
    HORANGT2 = fct_recode(HORANGT,
                          "2-Modifier horaires" = "1-Modifier horaires"            ,
                          "1-Pas mod horaires"  = "2-Pas mod horaires"             ,
                          "NULL"                = "3-Sans objet (pas de collègues)"
    )  |> fct_relevel(sort), 
    
    PREMOIS = fct_recode(PREVIS,
                         "1-Connait pas horaires mois" = "2-Horaires semaine pro",
                         "1-Connait pas horaires mois" = "3-Horaires demain"     ,
                         "1-Connait pas horaires mois" = "4-Pas previsions h"    ,
                         "2-Connait horaires mois"     = "1-Horaires mois"       
    ) |> fct_relevel(sort), 
    
    HORVAR2 = fct_recode(HORVAR,
                         "1-Horaires variables"     = "2-2x8"               ,
                         "1-Horaires variables"     = "3-3x8 ou +"          ,
                         "1-Horaires variables"     = "4-Horaires variables",
                         "2-Mêmes h tous les jours" = "1-Horaires stables"  ,
                         
    ) |> fct_relevel(sort), 
    
    CONTROLE2 = fct_recode(CONTROLE,
                           "1-Contrôle horaires" = "2-Pointeuse ou badge"        ,
                           "1-Contrôle horaires" = "3-Signature ou fiche horaire",
                           "1-Contrôle horaires" = "4-Contrôle encadrement"      ,
                           "1-Contrôle horaires" = "5-Contrôle autres"           ,
                           "1-Contrôle horaires" = "6-Autre"                     ,
                           "2-Pas contrôle h"    = "1-Aucun contrôle"            ,
                           
    ) |> fct_relevel(sort), 
    
    URGFAM2 = fct_recode(URGFAM,
                         "1-Difficile absence imprévue" = "2-Pas facile absence"     ,
                         "1-Difficile absence imprévue" = "3-Impossible absence"     ,
                         "2-Facile absence imprévue"   = "1-Facile absence imprévue",
                         
    ) |> fct_relevel(sort), 
    
    PTMATIN2 = fct_recode(PTMATIN,
                          "1-Matin habituel" = "1-Matin habituel",
                          "2-Pas matin hab"  = "2-Matin occasion",
                          "2-Pas matin hab"  = "3-Pas matin"     
    ) |> fct_relevel(sort), 
    
    SOIR2 = fct_recode(SOIR,
                       "1-Soir habituel" = "1-Soir habituel",
                       "2-Pas soir hab"  = "2-Soir occasion",
                       "2-Pas soir hab"  = "3-Pas soir"     
    ) |> fct_relevel(sort), 
    
    NUIT2 = fct_recode(NUIT,
                       "1-Travail de nuit"  = "1-Nuit habituel",
                       "2-Pas travail nuit" = "2-Nuit occasion",
                       "2-Pas travail nuit" = "3-Pas nuit"     
    ) |> fct_relevel(sort), 
    
    REPOS2 = fct_recode(REPOS,
                        "1-48h repos non" = "2-Pas 48h"  ,
                        "2-48h repos"     = "1-48h repos",
    ) |> fct_relevel(sort), 
    
    CVFVP2 = fct_recode(CVFVP,
                        "1-Horaires non accordés"   = "3-h pas très bien"        ,
                        "1-Horaires non accordés"   = "4-h pas bien du tout"    , 
                        "2-Horaires accordés vie perso" = "1-horaires/vie sociale TB",
                        "2-Horaires accordés vie perso" = "2-h bien"                 ,
    ) |> fct_relevel(sort)
    
    
  )

vars_horaires <- 
  c(#Horaires "débordants"
    "HH2", "HSUP2", "JOINDRE2", "MAISON2",   
    #"MISSION2",  #Pas 2019
    #Contraintes horaires
    #"HORANGT2",  #Pas 2019
    "PREMOIS",   
    "HORVAR2",   
    "CONTROLE2", 
    #"URGFAM2",   #Pas 2013
    #Horaires atypiques
    #"SAMEDI1", "DIMANCHE1", 
    "PTMATIN2",  
    "SOIR2",     
    "NUIT2",     
    "REPOS2",    
    "CVFVP2"     
  )
# vars_horaires %in% names(ct)

vars_horaires13 <- 
  c(#Horaires "débordants"
    "HH2", "HSUP2", "JOINDRE2", "MAISON2",   
    "MISSION2",  #Pas 2019
    #Contraintes horaires
    "PREMOIS","HORVAR2","CONTROLE2", 
    "HORANGT2",  #Pas 2019
    #"URGFAM2",  #Pas 2013
    #Horaires atypiques
    #"SAMEDI1", "DIMANCHE1", 
    "PTMATIN2", "SOIR2", "NUIT2", "REPOS2", "CVFVP2"     
  )
# vars_horaires %in% names(ct)


vars_horaires05 <- 
  c(#Horaires "débordants"
    #"HH2", "HSUP2", #Autres variables 2005 ? 
    "JOINDRE2", "MAISON2",   
    
    #Contraintes horaires
    "HORANGT2",  #Pas 2019
    "HORVAR2",   
    "CONTROLE2",
    
    #Horaires atypiques
    "REPOS2"
  )


# vars_horaires13 |> map(~pull(ct, .) |> levels())
ct <- ct |> score_from_lv1("score_horaires"  , vars_horaires  )
ct <- ct |> score_from_lv1("score_horaires13", vars_horaires13)
ct <- ct |> score_from_lv1("score_horaires05", vars_horaires05)
# tab(ct[salariat & (`2013` | `2016` | `2019`),], score_horaires, ANNEE, pct = "col")
# tab(ct[salariat & (`2013`  | `2016`),], score_relations13, ANNEE, pct = "col")
# tab(ct[salariat05 & !`2019` | (salariat & `2016`),], score_horaires05, ANNEE, pct = "col")





# Conflits éthiques :
ct$RP_PLAISE3 <- fct_recode(
  ct$RP_PLAISE2,
  "1-Rarement choses plaisantes" = "2-Rarement choses plaisantes" ,
  "2-Souvent choses plaisantes"  = "1-Souvent choses plaisantes" ,
) |> fct_relevel(sort)


ct$RP_DESAPPROUVE2 <- fct_recode(
  ct$RP_DESAPPROUVE,
  "1-Souvent choses désapprouvées"  = "1-Choses que je désapprouve: Toujours",
  "1-Souvent choses désapprouvées"  = "2-Choses que je désapprouve: Souvent" ,
  "2-Rarement choses désapprouvées" = "3-Choses que je désapprouve: Parfois" ,
  "2-Rarement choses désapprouvées" = "4-Choses que je désapprouve: Jamais"  
)|> fct_relevel(sort)


ct$RP_MENTIR2 <- fct_recode(
  ct$RP_MENTIR,
  "1-Parfois mentir" = "1-Mentir: Toujours",
  "1-Parfois mentir" = "2-Mentir: Souvent" ,
  "1-Parfois mentir" = "3-Mentir: Parfois" ,
  "2-Jamais mentir"  = "4-Mentir: Jamais"  
)|> fct_relevel(sort)


ct$RP_RISQUE_AUTRUI2 <- fct_recode(
  ct$RP_RISQUE_AUTRUI,
  "1-Parfois risques santé d'autrui" = "1-Risques santé d'autrui: Toujours",
  "1-Parfois risques santé d'autrui"  = "2-Risques santé d'autrui: Souvent" ,
  "1-Parfois risques santé d'autrui"  = "3-Risques santé d'autrui: Parfois" ,
  "2-Jamais risques santé d'autrui"   = "4-Risques santé d'autrui: Jamais"  
)|> fct_relevel(sort)


ct$RP_INJUSTEMENT2 <- fct_recode(
  ct$RP_INJUSTEMENT,
  "1-Parfois traiter injustement"  = "1-Traiter injustement: Toujours",
  "1-Parfois traiter injustement"  = "2-Traiter injustement: Souvent" ,
  "1-Parfois traiter injustement"  = "3-Traiter injustement: Parfois" ,
  "2-Jamais traiter injustement"   = "4-Traiter injustement: Jamais"  
)|> fct_relevel(sort)


ct$RPE_FIERTE2 <- fct_recode(
  ct$RPE_FIERTE,
  "1-Rarement fierté" = "3-Fierté: Parfois" ,
  "1-Rarement fierté" = "4-Fierté: Jamais"  ,
  "2-Souvent fierté"  = "1-Fierté: Toujours",
  "2-Souvent fierté"  = "2-Fierté: Souvent" ,
)|> fct_relevel(sort)


ct$RPE_UTILE2 <- fct_recode(
  ct$RPE_UTILE,
  "1-Rarement utile" = "3-Utile: Parfois" ,
  "1-Rarement utile" = "4-Utile: Jamais"  ,
  "2-Souvent utile"  = "1-Utile: Toujours",
  "2-Souvent utile"  = "2-Utile: Souvent" ,
)|> fct_relevel(sort)

ct$RP_TROP_VITE2 <- fct_recode(
  ct$RP_TROP_VITE,
  "1-Souvent trop vite"  = "1-Trop vite: Toujours",
  "1-Souvent trop vite"  = "2-Trop vite: Souvent" ,
  "2-Rarement trop vite" = "3-Trop vite: Parfois" ,
  "2-Rarement trop vite" = "4-Trop vite: Jamais"  
)

ct$RPE_EXLOITE2 <- fct_recode(
  ct$RPE_EXLOITE,
  "1-Souvent exploité"  = "1-Exploité: Toujours",
  "1-Souvent exploité"  = "2-Exploité: Souvent" ,
  "2-Rarement exploité" = "3-Exploité: Parfois" ,
  "2-Rarement exploité" = "4-Exploité: Jamais"  
)|> fct_relevel(sort)


ct$CORRTAN2 <- fct_recode(
  ct$CORRTAN,
  "1-Temps insuffisant" = "2-Pas SUFF tps",
  "2-Temps ok"   = "1-SUFF temps"  ,
)|> fct_relevel(sort)

ct$CORRCOL2 <- fct_recode(
  ct$CORRCOL,
  "1-Collègues insuffisants"  = "2-Pas SUFF coll" ,
  "2-Collègues ok"            = "1-SUFF collègues",
  "2-Collègues ok"            = "3-Sans objet"    
)|> fct_relevel(sort)

ct$CORRINF2 <- fct_recode(
  ct$CORRINF,
  "1-Information insuffisante" = "2-Pas SUFF infos",
  "2-Info ok"     = "1-SUFF infos"    ,
)|> fct_relevel(sort)


ct$CORRCOP2 <- fct_recode(
  ct$CORRCOP,
  "1-Coopération insuffisante" = "2-Coopération insuffisante",
  "2-Coopération ok"   = "1-Coopération suffisante"  ,
)|> fct_relevel(sort)


ct$CORRLOG2 <- fct_recode(
  ct$CORRLOG,
  "1-Logiciels insuffisants"  = "2-Pas SUFF logi" ,
  "2-Logiciels ok" = "1-SUFF logiciels",
  "2-Logiciels ok" = "3-Sans objet"    
)|> fct_relevel(sort)


ct$CORRMAT2 <- fct_recode(
  ct$CORRMAT,
  "1-Matériel insuffisant"  = "2-Pas SUFF mat" ,
  "2-Matériel ok" = "1-SUFF matériel",
  "2-Matériel ok" = "3-Sans objet"   
)|> fct_relevel(sort)


ct$CORRFORM2 <- fct_recode(
  ct$CORRFORM,
  "1-Formation insuffisante"  = "2-Pas SUFF form" ,
  "2-Formation ok" = "1-SUFF formation",
  "2-Formation ok" = "3-Sans objet"    
) |> fct_relevel(sort)


vars_moyens <- c(
  "CORRTAN2" , #Ne pas avoir assez de temps pour effectuer correctement son travail
  "CORRCOL2",  #NE pas avoir des collègues en nombre suffisant
  "CORRINF2" , #Ne pas avoir des informations claires et suffisantes pour effectuer correctement son travail
  "CORRCOP2" , #Ne pas avoir la possibilité de coopérer ou des collaborateurs en nombre suffisant
  "CORRLOG2" , #Ne pas avoir des logiciels et des programmes informatiques bien adaptés
  "CORRMAT2" , #Ne pas avoir un matériel suffisant et adapté pour effectuer correctement son travail
  "CORRFORM2"  #Ne pas avoir une formation continue suffisante et adaptée
)    

vars_conflits_ethiques16 <- c(
  "RP_PLAISE3"         ,#N'avoir jamais ou seulement parfois la possibilité de faire des choses qui plaisent
  "RP_DESAPPROUVE2"    ,#Devoir toujours ou souvent faire des choses qu'on désapprouve
  "RP_MAUVAIS_TRAVAIL2",#Ne pas pouvoir, toujours ou souvent, faire du bon travail, sacrifier la qualité
  "RP_INUTILE2"        ,#Devoir, au moins parfois, faire des choses inutiles ou dégradantes
  "RP_MENTIR2"         ,#Devoir, au moins parfois, mentir aux clients, patients, usagers, collègues
  "RP_RISQUE_AUTRUI2"  ,#Devoir, au moins parfois, prendre des risques pour la santé physique et mentale des usa-gers, clients…
  "RP_INJUSTEMENT2"    ,#Etre amené, au moins parfois, à traiter injustement ou à favoriser des personnes
  "RPE_FIERTE2"        ,#Ne jamais avoir, ou seulement parfois, la fierté du travail bien fait
  "RPE_UTILE2"         ,#Ne jamais avoir, ou seulement parfois, l'impression de faire qq chose d'utile aux autres
  "RP_TROP_VITE2"     #,#Faire toujours ou souvent trop vite une opération qui demanderait davantage de soin
  #"RPE_EXLOITE2"       ,#Avoir toujours ou souvent le sentiment d'être exploité
  
)
#vars_conflits_ethiques16 %in% names(ct)
#Trois classes : « Sentiment de faire du sale boulot » « Tous types de conflits de valeur » « Pas de conflit de valeur » 

vars_conflits_ethiques <- 
  c("RP_DESAPPROUVE2", "RPE_FIERTE2", "RPE_UTILE2"#, #"RPE_EXLOITE2", 
    #"RP_PLAISE3", "RP_TROP_VITE2", #Pas 2019
    #"RP_MAUVAIS_TRAVAIL2",         #Pas 2013
    
    #"RP_INUTILE2"        , #Seulement 2016
    #"RP_MENTIR2"         , #Seulement 2016
    #"RP_RISQUE_AUTRUI2"  , #Seulement 2016
    #"RP_INJUSTEMENT2"    , #Seulement 2016
    
    #"CORRTAN2", "CORRINF2", "CORRCOP2", "CORRLOG2", "CORRMAT2", "CORRFORM2"           
  )

vars_conflits_ethiques13 <- 
  c("RP_DESAPPROUVE2", "RPE_FIERTE2", "RPE_UTILE2", #"RPE_EXLOITE2", 
    "RP_PLAISE3", "RP_TROP_VITE2"#, #Pas 2019
    #"RP_MAUVAIS_TRAVAIL2",        #Pas 2013
    
    #"CORRTAN2", "CORRINF2", "CORRCOP2", "CORRLOG2", "CORRMAT2", "CORRFORM2"           
  )

vars_ethique_moyens16 <- c(vars_conflits_ethiques16, vars_moyens)
vars_ethique_moyens   <- c(vars_conflits_ethiques  , vars_moyens)
vars_ethique_moyens13 <- c(vars_conflits_ethiques13, vars_moyens)




# vars_conflits_ethiques16 |> map(~pull(ct, .) |> levels())
# tab_many(ct, all_of(vars_moyens), ANNEE, pct = "col")
ct <- ct |> score_from_lv1("score_moyens", vars_moyens)
#tab(ct, score_moyens, ANNEE, pct = "col", wt = pondcal)

ct <- ct |> score_from_lv1("score_conflits_ethiques16", vars_conflits_ethiques16)
ct <- ct |> score_from_lv1("score_conflits_ethiques"  , vars_conflits_ethiques)
ct <- ct |> score_from_lv1("score_conflits_ethiques13", vars_conflits_ethiques13)
#tab(ct[salariat & `2016`,], score_conflits_ethiques16, pct = "col")
#tab(ct[salariat & (`2013` | `2016` | `2019`),], score_conflits_ethiques, ANNEE, pct = "col")
#tab(ct[salariat & `2013`,], score_conflits_ethiques13, ANNEE, pct = "col")

ct <- ct |> score_from_lv1("score_ethique_moyens16", c(vars_conflits_ethiques16, vars_moyens) )
ct <- ct |> score_from_lv1("score_ethique_moyens"  , c(vars_conflits_ethiques  , vars_moyens) )
ct <- ct |> score_from_lv1("score_ethique_moyens13", c(vars_conflits_ethiques13, vars_moyens) )
#tab(ct[salariat & `2016`,], score_ethique_moyens16, pct = "col")
#tab(ct[salariat & (`2013` | `2016` | `2019`),], score_ethique_moyens, ANNEE, pct = "col")
#tab(ct[salariat & `2013`,], score_ethique_moyens13, ANNEE, pct = "col")





# Non-reconnaissance du travail :
ct$RP_ESTIME2 <- fct_recode(
  ct$RP_ESTIME,
  "1-Ni respect ni estime" = "1-Estime mérite: PDT d’accord",
  "1-Ni respect ni estime" = "2-Estime mérite: Pas d’accord",
  "2-Respect du travail"   = "3-Estime mérite: D’accord"    ,
  "2-Respect du travail"   = "4-Estime mérite: TAF d’accord",
  "2-Respect du travail"   = "5-Non concerné"               
) |> fct_relevel(sort)


ct$RP_PROMOTION2 <- fct_recode(
  ct$RP_PROMOTION,
  "1-Perspective promotion absente" = "1-Persp promotion: PDT d’accord",
  "1-Perspective promotion absente" = "2-Persp promotion: Pas d’accord",
  "2-Perspective promotion"         = "3-Persp promotion: D’accord"    ,
  "2-Perspective promotion"         = "4-Persp promotion: TAF d’accord",
  "2-Perspective promotion"         = "5-Non concerné"                 
) |> fct_relevel(sort)


ct$PAYECOM2 <- fct_recode(
  ct$PAYECOM,
  "1-Mal payé"     = "4-Plutôt mal payé" ,
  "1-Mal payé"     = "5-Très mal payé"   ,
  "2-Pas mal payé" = "1-Très bien payé"  ,
  "2-Pas mal payé" = "2-Bien payé"       ,
  "2-Pas mal payé" = "3-Normalement payé",
) |> fct_relevel(sort)


ct$SIEG2 <- fct_recode(
  ct$SIEG34,
  "1-Poste non lié formation" = "2-Pas poste lié form",
  "2-Poste lié formation"     = "1-Poste lié formation",
) |> fct_relevel(sort)


ct$RP_SUP_EVAL3 <- fct_recode(
  ct$RP_SUP_EVAL2,
  "1-Éval connaissent pas travail" = "1-Éval connaissent pas travail",
  "2-Éval connaissent travail"     = "2-Éval connaissent travail"    ,
  "2-Éval connaissent travail"     = "5-Non concerné"                
) |> fct_relevel(sort)

ct$RP_FIER_ORGA2 <- fct_recode(
  ct$RP_FIER_ORGA,
  "1-Rarement fier orga" = "3-Fier orga: Parfois" ,
  "1-Rarement fier orga" = "4-Fier orga: Jamais"  ,
  "2-Souvent fier orga"  = "1-Fier orga: Toujours",
  "2-Souvent fier orga"  = "2-Fier orga: Souvent" ,
) |> fct_relevel(sort)

vars_non_reconnaissance <- 
  c("RP_ESTIME2"   , #Non respect ni estime du travail, compte tenu des efforts réalisés
    "RP_PROMOTION2", #Perspectives de promotion insatisfaisantes, compte tenu des efforts réalisés
    "PAYECOM2",      #Se considérer comme mal ou très mal payé compte tenu du travail réalisé
    "SIEG2"   ,      #La position professionnelle correspond mal à la formation
    "RP_SUP_EVAL3" , #Les personnes qui évaluent le travail le connaissent mal
    "RP_FIER_ORGA2"  #Ne pas être fier de travailler dans cette entreprise
  )
# vars_non_reconnaissance %in% names(ct)
#Trois classes : « Pas de problème de manque de reconnaissance » ; « Des problèmes de manque de reconnaissance » ; « Sentiment de déclassement »

vars_non_reconnaissance19 <- 
  c("RP_ESTIME2", "RP_PROMOTION2", "PAYECOM2", "SIEG2", "RP_SUP_EVAL3"
    #"RP_FIER_ORGA2"  #Pas 2019
  )

ct <- ct |> score_from_lv1("score_non_reconnaissance", vars_non_reconnaissance)
ct <- ct |> score_from_lv1("score_non_reconnaissance19", vars_non_reconnaissance19)
#tab(ct[salariat & (`2013` | `2016`),], score_non_reconnaissance, ANNEE, pct = "col")
#tab(ct[salariat & (`2013` | `2016` | `2019`),], score_non_reconnaissance19, ANNEE, pct = "col")




# Exigences émotionnelles: 
ct$TENSION12  <- ct$TENSION1 %>% fct_explicit_na(last(levels(.)))

ct$EMOTION2 <- fct_recode(
  ct$EMOTION,
  "1-Parfois ému/secoué" = "1-Tj ému/secoué",
  "1-Parfois ému/secoué" = "2-Sv ému/secoué",
  "1-Parfois ému/secoué" = "3-Pf ému/secoué",
  "2-Jamais ému"         = "4-Jm ému/secoué",
  "NULL"                 = "8-Refus"        
)


ct$RP_CACHER_EMOTION2 <- fct_recode(
  ct$RP_CACHER_EMOTION,
  "1-Souvent cacher émotions"  = "1-Cacher émotions: Toujours",
  "1-Souvent cacher émotions"  = "2-Cacher émotions: Souvent" ,
  "2-Parfois cacher émotions"  = "3-Cacher émotions: Parfois" ,
  "2-Parfois cacher émotions"  = "4-Cacher émotions: Jamais"  
)


ct$RP_PAS_AVIS2 <- fct_recode(
  ct$RP_PAS_AVIS,
  "1-Souvent éviter avis"  = "1-Éviter avis: Toujours",
  "1-Souvent éviter avis"  = "2-Éviter avis: Souvent" ,
  "2-Rarement éviter avis" = "3-Éviter avis: Parfois" ,
  "2-Rarement éviter avis" = "4-Éviter avis: Jamais"  
)


ct$RP_PEUR2 <- fct_recode(
  ct$RP_PEUR,
  "1-Parfois peur sécurité"  = "1-Peur sécurité: Toujours",
  "1-Parfois peur sécurité"  = "2-Peur sécurité: Souvent" ,
  "1-Parfois peur sécurité"  = "3-Peur sécurité: Parfois" ,
  "2-Jamais peur sécurité"   = "4-Peur sécurité: Jamais"  
)

ct$AGRES1 = if_else(ct$RP_AGR_PHYS_PUB == "1-Agression physique ou sexuelle public" |
                      ct$RP_AGR_VERB_PUB == "1-Agression verbale public", 
                    factor("1-Agression public", c("1-Agression public", "2-Pas agression public")), 
                    factor("2-Pas agression public"))

vars_emotions16 <- 
  c("DETRESSE" , #Etre en contact avec des personnes en situation de détresse
    "CALMER"   , #Devoir calmer des gens
    "TENSION12", #Vivre des situations de tension avec le public
    "CONFLIT"  , #Devoir gérer des conflits
    "EMOTION2" , #Avoir été, au moins parfois, bouleversé(e), secoué(e), ému(e) dans son travail
    "RP_CACHER_EMOTION2", #Devoir toujours ou souvent cacher ses émotions
    "RP_PAS_AVIS2"      , #Devoir toujours ou souvent éviter de donner son avis
    "RP_PEUR2" , #Avoir, au moins parfois, peur pour sa sécurité ou celle des autres pendant son travail
    "AGRES1"     #Avoir été victime d'une agression (verbale, physique ou sexuelle) de la part du public
  )
# vars_emotions16 %in% names(ct)
#Trois classes : « Pas d'exigences émotionnelles » ; « Détresse »; « Tout type d'exigences émotionnelles »

vars_emotions <- 
  c("TENSION12", "RP_CACHER_EMOTION2", "RP_PEUR2"#, 
    #"EMOTION2" ,    #Pas 2013
    
    #"DETRESSE" ,    #Pas 2019
    #"CALMER"   ,    #Pas 2019
    #"RP_PAS_AVIS2", #Pas 2019
    #"AGRES1"      , #Pas 2019
    
    #"CONFLIT"       #Seulement 2016
  )

vars_emotions13 <- 
  c("TENSION12", "RP_CACHER_EMOTION2", "RP_PEUR2", 
    #"EMOTION2" ,    #Pas 2013
    
    "DETRESSE" ,    #Pas 2019
    "CALMER"   ,    #Pas 2019
    "RP_PAS_AVIS2", #Pas 2019
    "AGRES1"     #, #Pas 2019
  )


vars_emopub13 <- c("DETRESSE", "CALMER", "TENSION12", "AGRES1", "PUBLIC_MEPRISANT", 
                   "PUBLIC_DENI_RECO")
# ct13_reg |> tab_many(DEGRADANT, RP_SUB_PAR_PUBLIC, pct = "all", wt = pondqaa)

## Pour faire un score émotions + un score public : pas assez de variables ? 
# c("DETRESSE", "TENSION12", "AGRES1")
# c("RP_CACHER_EMOTION2", "RP_PEUR2", "CALMER", "RP_PAS_AVIS2")


# vars_emotions16 |> map(~pull(ct, .) |> levels())
# tab_many(ct, vars_emotions16, ANNEE, pct = "col")
ct <- ct |> score_from_lv1("score_emotions16", vars_emotions16)
ct <- ct |> score_from_lv1("score_emotions"  , vars_emotions)
ct <- ct |> score_from_lv1("score_emotions13", vars_emotions13)
ct <- ct |> score_from_lv1("score_emopub13"  , vars_emopub13)
#tab(ct[salariat & `2016`,], score_emotions16, pct = "col")
#tab(ct[salariat & (`2013` | `2016` | `2019`),], score_emotions, ANNEE, pct = "col")
#tab(ct[salariat & `2013`,], score_emotions13, ANNEE, pct = "col")







# Charge mentale
ct$RP_QUANTITE2 <- fct_recode(
  ct$RP_QUANTITE,
  "1-Quantité excessive"  = "3-Quantité excessive: D’accord"    ,
  "1-Quantité excessive"  = "4-Quantité excessive: TAF d’accord",
  "2-Pas pb quanti exces" = "1-Quantité excessive: PDT d’accord",
  "2-Pas pb quanti exces" = "2-Quantité excessive: Pas d’accord",
  "2-Pas pb quanti exces" = "5-Non concerné"                    
) |> fct_relevel(sort)

ct$RP_PRESSION2 <- fct_recode(
  ct$RP_PRESSION,
  "1-Souvent pression"  = "1-Pression: Toujours",
  "1-Souvent pression"  = "2-Pression: Souvent" ,
  "2-Parfois pression"  = "3-Pression: Parfois" ,
  "2-Parfois pression"  = "4-Pression: Jamais"  
) |> fct_relevel(sort)

ct$RP_CHR_MENTALE2 <- fct_recode(
  ct$RP_CHR_MENTALE,
  "1-Souvent charge mentale"  = "1-Charge mentale: Toujours",
  "1-Souvent charge mentale"  = "2-Charge mentale: Souvent" ,
  "2-Parfois charge mentale"  = "3-Charge mentale: Parfois" ,
  "2-Parfois charge mentale"  = "4-Charge mentale: Jamais"  
) |> fct_relevel(sort)

ct$RP_PENSER_TRAVAIL2 <- fct_recode(
  ct$RP_PENSER_TRAVAIL,
  "1-Souvent penser travail"  = "1-Penser travail après: Toujours",
  "1-Souvent penser travail"  = "2-Penser travail après: Souvent" ,
  "2-Rarement penser travail" = "3-Penser travail après: Parfois" ,
  "2-Rarement penser travail" = "4-Penser travail après: Jamais"  
) |> fct_relevel(sort)

vars_charge_mentale <- 
  c("RP_CHR_MENTALE2",    # Devoir penser souvent ou toujours à trop de choses à la fois
    "RP_PRESSION2",       # Travailler souvent ou toujours sous pression
    "RP_PENSER_TRAVAIL2",  # Penser souvent ou toujours à son travail
    "RP_QUANTITE2"       # Devoir effectuer une quantité de travail excessive
  )
#vars_charge_mentale16 %in% names(ct)

vars_charge_mentale19 <- 
  c("RP_PRESSION2", "RP_PENSER_TRAVAIL2", "RP_QUANTITE2"  
    #"RP_CHR_MENTALE2",   #Pas 2019 
  )

# vars_charge_mentale |> map(~pull(ct, .) |> levels())
# tab_many(ct[salariat,], all_of(vars_charge_mentale), ANNEE, pct = "col")
ct <- ct |> score_from_lv1("score_charge_mentale"  , vars_charge_mentale)
ct <- ct |> score_from_lv1("score_charge_mentale19", vars_charge_mentale19)
# tab(ct[salariat & (`2013` | `2016`),], vars_charge_mentale, ANNEE, pct = "col")
# tab(ct[salariat & (`2013` | `2016` | `2019`),], vars_charge_mentale19, ANNEE, pct = "col")




# Contradictions organisationnelles :
ct$DEBORD_NEG <- as.factor(if_else(ct$DEBORD == "1-Interrompu" & ct$INTERACT == "1-Interrompu négatif" &
                                     !is.na(ct$INTERACT), 
                                   "1-Fréquement interrompu", 
                                   "2-Pas interrompu ou positif"
))

ct$ATTENTE2 <- fct_recode(
  ct$ATTENTE,
  "1-Ne sait pas tj ce qu'on attend" = "2-Sv sait attendus",
  "1-Ne sait pas tj ce qu'on attend" = "3-Pf sait attendus",
  "1-Ne sait pas tj ce qu'on attend" = "4-Jm sait attendus",
  "2-Sait toujours ce qu'on attend"  = "1-Tj sait attendus",
)

vars_contrad16 <- 
  c("CONTRAD" ,     #Recevoir des ordres contradictoires
    "RP_ORGA2",     #Ne pas pouvoir s'organiser comme on le souhaite
    "DEBORD_NEG" ,   #Devoir s'interrompre fréquemment et considérer cela comme négatif pour le travail
    
    #"RP_TROP_VITE2",#Faire toujours ou souvent trop vite une opération qui demanderait davantage de soin
    "ATTENTE2" ,   #Ne pas toujours savoir ce qu'on attend de nous au travail : ne varie pas selon orga
    "RP_SUP_EVAL3"#, #Ceux qui évaluient mon travail ne le connaissent pas
    #"CRITERE",      #Les critères utilisés pour évaluer votre travail paraissent-ils pertinents ?
    #"CONFSAL", #les supérieurs font confiance aux salariés
    #"INFOCONF", #on peut faire confiance aux infos des supérieurs
    #"DIFFIC" 
    
    #"STARK",
    #"RP_CHANGEMENTS"
    #"OBJpasmod", 
  )


#vars_contrad16 %in% names(ct)

vars_contrad13 <- 
  c("CONTRAD", "DEBORD_NEG", "RP_SUP_EVAL3", "RP_ORGA2"  
    #"ATTENTE2",     #Pas 2013
  )

# # vars_contrad16 |> map(~pull(ct, .) |> levels())
# # tab_many(ct[salariat,], all_of(vars_contrad16), ANNEE, pct = "col")
# tab_many(ct[salariat & `2016`,], all_of(vars_contrad16), cah_ORGAext, pct = "col",
#          wt = pondqaa, na = "drop", cleannames = TRUE, color = "diff")
ct <- ct |> score_from_lv1("score_contrad16", vars_contrad16)
ct <- ct |> score_from_lv1("score_contrad13", vars_contrad13)
# #tab(ct[salariat & `2016`,], score_contrad16, ANNEE, pct = "col")
# #tab(ct[salariat & (`2013` | `2016`),], score_contrad13, ANNEE, pct = "col")






# Santé et accidents du travail :
ct$BSANTE2 <- fct_recode(
  ct$BSANTE,
  "1-Santé moyenne ou mauvaise" = "3-Assez bonne santé"  ,
  "1-Santé moyenne ou mauvaise" = "4-Mauvaise santé"     ,
  "1-Santé moyenne ou mauvaise" = "5-Très mauvaise santé",
  "2-Bonne santé"               = "1-Très bonne santé"   ,
  "2-Bonne santé"               = "2-Bonne santé"        ,
  "NULL"                        = "98"                   
) |> fct_relevel(sort)


ct$ACCIDT2 <- fct_recode(
  ct$ACCIDT,
  "1-Un accident ou plus" = "1-1 accidents",
  "1-Un accident ou plus" = "2-2 accidents",
  "1-Un accident ou plus" = "3-3 accid +"  ,
  "1-Un accident ou plus" = "4"            ,
  "2-Pas d'accident"      = "0-0 accidents",
) |> fct_relevel(sort)

ct$ARET5 <- 
  if_else(!is.na(ct$NBJARR) & ct$NBJARR >= 5, 
          factor("1-Arret maladie >5j", c("1-Arret maladie >5j", "2-Pas arrêt >5")),
          factor("2-Pas arrêt >5"), 
  )



ct$ARETMAL2 <- if_else(ct$ATMAL >= 5, 
                       true  = "1-Au moins un arrêt maladie >= 5j (hors maternité)",
                       false = "2-Pas arrêt mal") |> 
  as.factor()
ct$ARETMAL3 <- if_else(ct$ATMAL - ct$NBJARR >= 5, 
                       true  = "1-Au moins un arrêt maladie >= 5j (hors maternité, accident)",
                       false = "2-Pas arrêt mal") |> 
  as.factor()


ct$SOMTBL2 <- fct_recode(
  ct$SOMTBL,
  "1-Insomnies semaine tlj" = "3-Insomnies semaine"       ,
  "1-Insomnies semaine tlj" = "4-Insomnies tous les jours",
  "2-Insomnies rarement"    = "1-Insomnies rarement"      ,
  "2-Insomnies rarement"    = "2-Insomnies mois"          ,
) |> fct_relevel(sort)

ct$DORMED2 <- fct_recode(
  ct$DORMED,
  "1-Somnifères parfois" = "1-Somnifères tous les jours",
  "1-Somnifères parfois" = "2-Somnifères semaine"       ,
  "1-Somnifères parfois" = "3-Somnifères mois"          ,
  "1-Somnifères parfois" = "4-Somnifères rarement"      ,
  "2-Somnifères jamais"  = "5"                          
) |> fct_relevel(sort)

ct$DORMED3 <- fct_recode(
  ct$DORMED,
  "1-Somnifères semaine tlj"    = "1-Somnifères tous les jours",
  "1-Somnifères semaine tlj"    = "2-Somnifères semaine"       ,
  "2-Somnifères parfois jamais" = "3-Somnifères mois"          ,
  "2-Somnifères parfois jamais" = "4-Somnifères rarement"      ,
  "2-Somnifères parfois jamais" = "5"                          
) |> fct_relevel(sort)





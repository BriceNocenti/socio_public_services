source("Demarrage.R", encoding = "UTF-8")

#Importer bases de données CT 2005 2013, 2016 et 2019 ----
ct05 <- read_sav(
  "~\\Data\\Conditions de travail\\CT2005\\ct05diffusion.sav",
  NULL)
names(ct05) <- str_to_upper(names(ct05))

ct13 <- read_sas(
  "~\\Data\\Conditions de travail\\CT2013_IND\\SAS\\individus_ct2013.sas7bdat",
  NULL)

ct16 <- read_sas(
  "~\\Data\\Conditions de travail\\CT2016_IND\\SAS\\individus_ao_rps2016_quetelet.sas7bdat",
  NULL)

ct19 <- read_sas(
  "~\\Data\\Conditions de travail\\CT2019_IND\\SAS\\individus_ao_ct2019_adisp.sas7bdat",
  NULL)



ct05$ANNEE <- "2005"
ct13$ANNEE <- "2013"
ct16$ANNEE <- "2016"
ct19$ANNEE <- "2019"

ct05$base <- "AO"
ct13$base <- "AO"
ct16$base <- "AO"
ct19$base <- "AO"

ct05$CHAMP_CT2005 <- "1"

ct05 <- ct05 |>
  rename(all_of(c(
    #Renommer, mêmes modalités que 2013 : 
    "PE"          = "P", 
    #"CSE"         = "CSTOT",
    #"CSER"        = "CSTOTR", 
    "CSEPER"      = "CSPP", 
    "CSEMER"      = "CSPM", 
    "FONCTION"    = "FONCTC", 
    "pondcal"     = "NEWPOND",
    "IDENT_IND"    = "IDENTNOI", 
    
    #Variables spécifiques à CT2005/EE
    "NAF_05"      = "NAF", 
    "NAF36_05"    = "NAFG36U", 
    "NAF16_05"    = "NAFG16",
    "NAF4_05"     = "NAFG4", 
    "NEWPUB_05"   = "NEWPUB",
    #"GOLDEN_05"   = "GOLDEN05", 
    "STATUT_05"   = "STATUT", 
    "STATOEP_05"  = "STATOEP", 
    "STAT2_05"    = "STAT2", 
    "CJUR_05"     = "CJUR", 
    "CHPUB_05"    = "CHPUB", 
    "CONTRA_05"   = "CONTRA", 
    "TITC_05"     = "TITC", 
    "CHEF_05"     = "CHEF", 
    "CHEFNBR_05"  = "CHEFNBR",
    "CHEFPROM_05" = "CHEFPROM",
    #"DDIPL_05"    = "DDIPL", 
    "DIP_05"      = "DIP", 
    "NEWTEFEN_05" = "NEWTEFEN", 
    "NEWTEFET_05" = "NEWTEFET"
  )))
ct05 <- ct05 |> mutate(PE = str_to_upper(PE))

#VARIABLES CT05 à ajouter si équivalent : anciennete, rvemensc, HH, HSUP, PROFDEB,
# AGFINETU (FORDAT), 

#VARIABLES JUSTE 2005 à ajouter : COMPLEXE



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


not_in_ctsel <- function(data, ctsel) {
  sel_case_to_upper <-
    ctsel[which(!ctsel %in% names(data) & (str_to_upper(ctsel) %in% names(data) ))]
  
  sel_change_to_lower <-
    ctsel[which(!ctsel %in% names(data) & (str_to_lower(ctsel) %in% names(data) ))]
  
  sel <- ctsel[which(
    ctsel %in% c(sel_case_to_upper, sel_change_to_lower) | ctsel %in% names(data)
  )] %>%
    set_names(.)
  
  sel[which(sel %in% sel_case_to_upper  )] <-
    set_names(str_to_upper(sel[which(sel %in% sel_case_to_upper  )]),
              sel_case_to_upper
    )
  sel[which(sel %in% sel_change_to_lower)] <-
    set_names(str_to_lower(sel[which(sel %in% sel_change_to_lower)]),
              sel_change_to_lower
    )
  
  sel_warning <-
    ctsel[which(!ctsel %in% names(data) & ! ctsel %in% c(sel_case_to_upper,
                                                         sel_change_to_lower))]
  if (length(sel_warning) != 0) message(paste0("Variables not found in data : ",
                                               paste(sel_warning, collapse = ", ")))
  
  sel
}

sel05 <- not_in_ctsel(ct05, ctsel)
#Variables not found in ct05 : 
# ANAIS, DEPNAIS, region, COUPLE, ETAMATRI, PACS, NBENFM3, NBENF3A17, NBENF18P, NPERS, 
# FAP225, FAP87, STATUT, AUTENT, ACTIVFIN, naf38, naf17, NAF4, EVOLEFF, NBSALENTC, CLASSIF,
# ENCADR, LIEUW, CLIENT, POURCLIEN, TYPEMPLOI, TITPUBR, TITPUB, anciennete, revmensc, 
# REMPERF, PAYECOM, HH, PREVIS, PTMATIN, SOIR, NUIT, NBNUIT, HSUP, HSUPCOMP, JOINEXT, 
# COMBEXT, TPMAISON, MISSION, ARETMAL, ATMAL, URGFAM, CVFVP, FATIGUE, INDISPO, DOMEST,
# KMANNU, DOCEVAL, OBJMODIF, OBJATTEIN, PUBLIC3, QUANTI, INITIAT, IDEE, RESTMAI, RESTMAIN,
# SIEG34, AIDCOLLNS, AIDOM, TRAVSEUL, CHGTCOLL, ACCHEF, DICHEF, ACCOL, DICOL, CONFSAL, 
# INFOCONF, DIFFIC, BIENETR1, BIENETR2, CONFLIT, EMOTION, ATTENTE, CRITERE, SYNDIC, 
# DISCURP, SYNDICNS, METIER, NOCHOM, MUTE, FORTMOD6, FORTMOD7, CHANGOP, CHGTINFO, 
# CHGTCONS, CHGTINFL, BSANTE, BCHRO, BLIMI, BRECO, SDOUL, SDOULOC_NB, SOMTBL, DORMED,
# MEDEC, EDM, TAG, NATIO1N1, NATIO1N2, NATIO, IMMI, nati, lienmig, DIPLOME, AGFINETU,
# ANFINETU, TCHOLE, TCHOLP, TCHOCE, TCHOCP, TSANE, TSANP, TINAE, TINAP, TINDEPE, 
# TINDEPP, TINTERIME, TINTERIMP, TCDDE, TCDDP, NBEMPE, NBEMPP, PROFDEB, CSEDEB, cserdeb,
# STATUTA, DEMPRO, DEMENA, perimmi, merimmi, NAIP, lnaisper, lnaismer, natnaisper, 
# natnaismer, ACTIP, PROFPER, cserper, ACTIM, STATUTM, PROFMER, csermer, typrepqaa,
# REPQAA, IDENT_IND, CHAMP_EMPL, pondqaa, ACCNIR

sel13 <- not_in_ctsel(ct13, ctsel)

# Variables not found in ct13 :
# AUTENT, EVOLEFF, TITPUBR, REMPERF, JOINEXT, COMBEXT, URGFAM, FATIGUE, INDISPO,
# DOMEST, PUBLIC3, QUANTI, INITIAT, IDEE, AIDCOLLNS, CONFSAL, INFOCONF, DIFFIC,
# BIENETR1, BIENETR2, CONFLIT, EMOTION, ATTENTE, CRITERE, MUTE, SDOUL,
# SDOULOC_NB, SOMTBL, DORMED, MEDEC, EDM, TAG, NATIO1N1, NATIO1N2, TCHOLE,
# TCHOLP, TCHOCE, TCHOCP, TSANE, TSANP, TINAE, TINAP, TINDEPE, TINDEPP,
# TINTERIME, TINTERIMP, TCDDE, TCDDP, NBEMPE, NBEMPP, ACCNIR

sel16 <- not_in_ctsel(ct16, ctsel)
# Variables not found in ct16 : POLY, POLYQUAL, MONOTON, AIDAUTR, AIDEXT, AIDOM
# + variables spécifiques à 2005

sel19 <- not_in_ctsel(ct19, ctsel)

# Variables not found in ct19 :
# REMPERF, HORANGT, JOINEXT, COMBEXT, MISSION, FATIGUE, INDISPO, DOMEST, CHAINE,
# INTERACT, OBJMODIF, OBJATTEIN, INITIAT, IDEE, RESTMAI, RESTMAIN, CHGTCOLL,
# ACCHEF, DICHEF, ACCOL, DICOL, CONFSAL, INFOCONF, DIFFIC, BIENETR1, BIENETR2,
# DETRESSE, CALMER, CONFLIT, SOMTBL, DORMED, EDM, TAG

sel <- intersect(intersect(names(sel13), names(sel16)), names(sel19))

sel_not_in_all_bases <- unique(c(#"OBJMODIF", "OBJATTEIN", "TITPUBR", "TITPUB",
  ctsel[which(!ctsel %in% sel)]))

# CATASTROPHE : il n'y a ni OBJMODIF ni OBJATTEIN en 2019 !
# => Pas possible de garder ma définition de la direction par objectifs.

sel05 <- sel05[which(names(sel05) %in% sel)]
sel13 <- sel13[which(names(sel13) %in% sel)]
sel16 <- sel16[which(names(sel16) %in% sel)]
sel19 <- sel19[which(names(sel19) %in% sel)]

ct05b <- select(ct05, all_of(sel05), any_of(sel_not_in_all_bases))

ct13b <- rename_with(ct13, ~ paste0("RP", .), 
                     starts_with(c("A1", "A2", "B1", "B2", "B3", "B4", "B5", "C1", "C2")))
ct13b <- select(ct13b, all_of(sel13), any_of(sel_not_in_all_bases), starts_with(c("RP", "WHO5")))

ct16b <- select(ct16, all_of(sel16), #all_of(ctsel[which(ctsel %in% names(sel13) & ctsel %in% names(sel19))]),
                any_of(sel_not_in_all_bases), starts_with(c("RP", "WHO5")))

ct19b <- select(ct19, all_of(sel19), any_of(sel_not_in_all_bases), starts_with(c("RP", "WHO5")))
# %>%
#   mutate(OBJMODIF  = NA_character_,
#                                                 OBJATTEIN = NA_character_)

# if (!(all(names(ct13b) == names(ct16b)) & all(names(ct19b) == names(ct16b)))) {
#   stop("Variables are not the same in the same order for all three bases")
# }



# Joindre les bases de données des trois années
# (Vérifier que les variables sont bien du même type,
#  sinon passer au type de 2016)

# change_class13 <-
# map2_lgl(ct13b, ct16b, ~ class(..1) == class(..2) ) %>%
#   keep(. == FALSE) %>% names()
#
# change_class19 <-
# map2_lgl(ct19b, ct16b, ~ class(..1) == class(..2) ) %>%
#   keep(. == FALSE) %>% names()

ctraw <-
  map(list(ct05b, ct13b, ct16b, ct19b),
      ~ mutate(., 
               across(any_of(c("PREVIS", "NATIO", "IMMI", "lienmig", "perimmi", "merimmi")),
                      as.character
               ), 
               
               across(any_of(c("NBNUIT", "COMBIEN", "AGE", "JOURTR", "NBSAME", "NBDIMA")),
                      as.integer
               ),
              
               across(any_of(c("NBEMP", "v1nbemp", "NBEMPE", "nbemp_13", "nbempe_16",
                               "nbempp_16",   "NBEMPP")),
                      ~ as.integer(.) |> na_if(98) |> na_if(99)
               ), 
               
               )
  ) %>%
  bind_rows()


# Passer caractères en facteurs et modalités vides en NA
ctraw <- ctraw %>%
  mutate(across(where(~ is.character(.) | (is.factor(.) & "" %in% levels(.))) & -any_of("IDENT_IND"),
                ~ fct_recode(., "NULL" = "")))

ct <- select(ctraw, -starts_with("RP"))




# Recoder et renommer variables individus à partir des formats de base --------
if("SEXE" %in% names(ct) & !is.numeric(ct$SEXE) ) {
  ct$SEXE <- forcats::fct_recode(ctraw$SEXE,
                                 
                                 "1-Homme"="1",
                                 "2-Femme"="2",
                                 
  ) } 
if("AGEQ" %in% names(ct) & !is.numeric(ct$AGEQ) ) {
  ct$AGEQ <- forcats::fct_recode(ctraw$AGEQ,
                                 
                                 "15-15 à 19 ans"="15",
                                 "20-20 à 24 ans"="20",
                                 "25-25 à 29 ans"="25",
                                 "30-30 à 34 ans"="30",
                                 "35-35 à 39 ans"="35",
                                 "40-40 à 44 ans"="40",
                                 "45-45 à 49 ans"="45",
                                 "50-50 à 54 ans"="50",
                                 "55-55 à 59 ans"="55",
                                 "60-60 à 64 ans"="60",
                                 "65-65 à 69 ans"="65",
                                 "70-70 à 74 ans"="70",
                                 "75-75 à 79 ans"="75",
                                 "80-80 à 84 ans"="80",
                                 
  ) } 
if("LNAIS" %in% names(ct) & !is.numeric(ct$LNAIS) ) {
  ct$LNAIS <- forcats::fct_recode(ctraw$LNAIS,
                                  
                                  "1-Né·e en France"="1",
                                  "2-Né·e à l'étranger"="2",
                                  
  ) } 
if("DEPNAIS" %in% names(ct) & !is.numeric(ct$DEPNAIS) ) {
  ct$DEPNAIS <- forcats::fct_recode(ctraw$DEPNAIS,
                                    
                                    "01-Ain"="01",
                                    "02-Aisne"="02",
                                    "03-Allier"="03",
                                    "04-Alpes-de-Haute-Provence"="04",
                                    "05-Hautes-Alpes"="05",
                                    "06-Alpes-Maritimes"="06",
                                    "07-Ardèche"="07",
                                    "08-Ardennes"="08",
                                    "09-Ariège"="09",
                                    "10-Aube"="10",
                                    "11-Aude"="11",
                                    "12-Aveyron"="12",
                                    "13-Bouches-du-Rhône"="13",
                                    "14-Calvados"="14",
                                    "15-Cantal"="15",
                                    "16-Charente"="16",
                                    "17-Charente-Maritime"="17",
                                    "18-Cher"="18",
                                    "19-Corrèze"="19",
                                    "21-Côte-d'Or"="21",
                                    "22-Côtes-d'Armor"="22",
                                    "23-Creuse"="23",
                                    "24-Dordogne"="24",
                                    "25-Doubs"="25",
                                    "26-Drôme"="26",
                                    "27-Eure"="27",
                                    "28-Eure-et-Loir"="28",
                                    "29-Finistère"="29",
                                    "2A-Corse-du-Sud"="2A",
                                    "2B-Haute-Corse"="2B",
                                    "30-Gard"="30",
                                    "31-Haute-Garonne"="31",
                                    "32-Gers"="32",
                                    "33-Gironde"="33",
                                    "34-Hérault"="34",
                                    "35-Ille-et-Vilaine"="35",
                                    "36-Indre"="36",
                                    "37-Indre-et-Loire"="37",
                                    "38-Isère"="38",
                                    "39-Jura"="39",
                                    "40-Landes"="40",
                                    "41-Loir-et-Cher"="41",
                                    "42-Loire"="42",
                                    "43-Haute-Loire"="43",
                                    "44-Loire-Atlantique"="44",
                                    "45-Loiret"="45",
                                    "46-Lot"="46",
                                    "47-Lot-et-Garonne"="47",
                                    "48-Lozère"="48",
                                    "49-Maine-et-Loire"="49",
                                    "50-Manche"="50",
                                    "51-Marne"="51",
                                    "52-Haute-Marne"="52",
                                    "53-Mayenne"="53",
                                    "54-Meurthe-et-Moselle"="54",
                                    "55-Meuse"="55",
                                    "56-Morbihan"="56",
                                    "57-Moselle"="57",
                                    "58-Nièvre"="58",
                                    "59-Nord"="59",
                                    "60-Oise"="60",
                                    "61-Orne"="61",
                                    "62-Pas-de-Calais"="62",
                                    "63-Puy-de-Dôme"="63",
                                    "64-Pyrénées-Atlantiques"="64",
                                    "65-Hautes-Pyrénées"="65",
                                    "66-Pyrénées-Orientales"="66",
                                    "67-Bas-Rhin"="67",
                                    "68-Haut-Rhin"="68",
                                    "69-Rhône"="69",
                                    "70-Haute-Saône"="70",
                                    "71-Saône-et-Loire"="71",
                                    "72-Sarthe"="72",
                                    "73-Savoie"="73",
                                    "74-Haute-Savoie"="74",
                                    "75-Paris"="75",
                                    "76-Seine-Maritime"="76",
                                    "77-Seine-et-Marne"="77",
                                    "78-Yvelines"="78",
                                    "79-Deux-Sèvres"="79",
                                    "80-Somme"="80",
                                    "81-Tarn"="81",
                                    "82-Tarn-et-Garonne"="82",
                                    "83-Var"="83",
                                    "84-Vaucluse"="84",
                                    "85-Vendée"="85",
                                    "86-Vienne"="86",
                                    "87-Haute-Vienne"="87",
                                    "88-Vosges"="88",
                                    "89-Yonne"="89",
                                    "90-Territoire de Belfort"="90",
                                    "91-Essonne"="91",
                                    "92-Hauts-de-Seine"="92",
                                    "93-Seine-Saint-Denis"="93",
                                    "94-Val-de-Marne"="94",
                                    "95-Val-d'Oise"="95",
                                    "9A-Guadeloupe"="9A",
                                    "9B-Martinique"="9B",
                                    "9C-Guyane"="9C",
                                    "9D-Réunion"="9D",
                                    "9E-Saint-Pierre-et-Miquelon"="9E",
                                    "9F-Mayotte"="9F",
                                    "9G-Wallis Et Futuna"="9G",
                                    "9H-Polynésie"="9H",
                                    "9I-Nouvelle-Calédonie"="9I",
                                    "9J-Saint-Barthélemy"="9J",
                                    "9K-Saint-Martin"="9K",
                                    "9L-Ile de Clipperton"="9L",
                                    "9M-Terres australes"="9M",
                                    
  ) } 
if("COUPLE" %in% names(ct) & !is.numeric(ct$COUPLE) ) {
  ct$COUPLE <- forcats::fct_recode(ctraw$COUPLE,
                                   
                                   "1-Couple même logement"="1",
                                   "2-Couple pas même logement"="2",
                                   "3-Pas en couple"="3",
                                   
  ) } 
if("ETAMATRI" %in% names(ct) & !is.numeric(ct$ETAMATRI) ) {
  ct$ETAMATRI <- forcats::fct_recode(ctraw$ETAMATRI,
                                     
                                     "1-Célibataire"="1",
                                     "2-Marié·e"="2",
                                     "3-Veu·ve"="3",
                                     "4-Divorcé·e"="4",
                                     
  ) } 
if("PACS" %in% names(ct) & !is.numeric(ct$PACS) ) {
  ct$PACS <- forcats::fct_recode(ctraw$PACS,
                                 
                                 "1-Pacsé·e"="1",
                                 "2-Pas pacsé·e"="2",
                                 
  ) } 
if("STATUT" %in% names(ct) & !is.numeric(ct$STATUT) ) {
  ct$STATUT <- forcats::fct_recode(ctraw$STATUT,
                                   
                                   "1-Salarié de l’État (ministères, établissements publics administratifs (EPA) nationaux, établissements publics d'enseignement, …)"="1",
                                   "10-Indépendant ou à votre compte"="10",
                                   "2-Salarié d'une collectivité territoriale (régions, départements, communes, OPHLM, établissements publics administratifs (EPA) des collectivités territoriales, …)"="2",
                                   "3-Salarié d’un hôpital public"="3",
                                   "4-Salarié d'un établissement de santé privé (à but lucratif ou non lucratif)"="4",
                                   "5-Salarié du secteur public social et médico-social (établissement d'hébergement pour personnes âgées, établissements sociaux ou médico-sociaux)"="5",
                                   "6-Salarié d’une entreprise, d’un artisan, d’une association (hors établissement de santé privé, hors secteur public social et médico-social)"="6",
                                   "7-Salarié d’un ou plusieurs particuliers"="7",
                                   "8-Vous aidez un membre de votre famille dans son travail sans être rémunéré"="8",
                                   "9-Chef d’entreprise salarié, PDG, gérant minoritaire, associé"="9",
                                   
  ) } 
if("AUTENT" %in% names(ct) & !is.numeric(ct$AUTENT) ) {
  ct$AUTENT <- forcats::fct_recode(ctraw$AUTENT,
                                   
                                   "1-Auto entrepreneur"="1",
                                   "2-Pas autoent"="2",
                                   "2-Pas autoent"="8",
                                   "9-REFUS"="9",
                                   
  ) } 
if("CLASSIF" %in% names(ct) & !is.numeric(ct$CLASSIF) ) {
  ct$CLASSIF <- forcats::fct_recode(ctraw$CLASSIF,
                                    
                                    "1-Manœuvre ou ouvrier spécialisé"="1",
                                    "10-Directeur général ou adjoint direct"="10",
                                    "2-Ouvrier qualifié ou hautement qualifié"="2",
                                    "3-Technicien"="3",
                                    "4-Personnel de catégorie B ou assimilé"="4",
                                    "5-Agent de maîtrise, maîtrise administrative ou commerciale, vrp (non cadre)"="5",
                                    "6-Personnel de catégorie A ou assimilé"="6",
                                    "7-Ingénieur, cadre (à l’exception des directeurs généraux ou de ses adjoints directs)"="7",
                                    "8-Personnel de catégorie C ou D ou assimilé"="8",
                                    "9-Employé de bureau, employé de commerce, personnel de services"="9",
                                    
  ) } 
if("TYPEMPLOI" %in% names(ct) & !is.numeric(ct$TYPEMPLOI) ) {
  # 2019 : 
  # "1-Contrat d'apprentissage"
  # "2-Intérim"
  # "3-Intérim en CDI"
  # "4-Stage rémunéré"
  # "5-Emploi aidé"
  # "6-Autre CDD"
  # "7-CDI ou titulaire FP"
  # "8-Travail sans contrat"
  
  # 2016 
  # 1. Contrat d'apprentissage
  # 2. Intérim
  # 3. Stage rémunéré
  # 4. Emploi aidé
  # 5. Autre CDD
  # 6. CDI ou titulaire FP"
  # 7. Travail sans contrat
  
   ct$TYPEMPLOI <- forcats::fct_recode(ctraw$TYPEMPLOI,
                                      
                                      "1-Contrat d'apprentissage"="1",
                                      "2-Placement par une agence d’intérim"="2",
                                      "3-Stage rémunéré en entreprise"="3",
                                      "4-Emploi aidé"="4",
                                      "5-Autre emploi à durée limitée"="5",
                                      "6-CDI titulaire FP"="6",
                                      "7-Travail sans contrat"="7",
                                      
  )
  
  ct <- ct |> 
    mutate(TYPEMPLOI = if_else(ANNEE == 2019, 
                               true = fct_recode(TYPEMPLOI,
                                                 "1-Contrat d'apprentissage"            = "1-Contrat d'apprentissage"           ,
                                                 "2-Placement par une agence d’intérim" = "2-Placement par une agence d’intérim",
                                                 "2-Placement par une agence d’intérim" = "3-Stage rémunéré en entreprise"      ,
                                                 "3-Stage rémunéré en entreprise"       = "4-Emploi aidé"                       ,
                                                 "4-Emploi aidé"                        = "5-Autre emploi à durée limitée"      ,
                                                 "5-Autre emploi à durée limitée"       = "6-CDI titulaire FP"                  ,
                                                 "6-CDI titulaire FP"                   = "7-Travail sans contrat"              ,
                                                 "7-Travail sans contrat"               = "8"                                   
                               ),
                               false = TYPEMPLOI)
           
    )
} 

if("FONCTION" %in% names(ct) & !is.numeric(ct$FONCTION) ) {
  ct$FONCTION <- forcats::fct_recode(ctraw$FONCTION,
                                     
                                     "01-Production, chantier, exploitation"="1",
                                     "02-Installation, réparation, maintenance"="2",
                                     "03-Gardiennage, nettoyage, entretien ménager"="3",
                                     "04-Manutention, magasinage, logistique"="4",
                                     "05-Secrétariat, saisie, accueil"="5",
                                     "06-Gestion, comptabilité"="6",
                                     "07-Commercial, technico-commercial"="7",
                                     "08-Études, recherche et développement, méthodes"="8",
                                     "09-Enseignement"="9",
                                     "10-Soin des personnes"="10",
                                     "11-Autre fonction"="11",
                                     
  ) } 
if("ENCADR" %in% names(ct) & !is.numeric(ct$ENCADR) ) {
  ct$ENCADR <- forcats::fct_recode(ctraw$ENCADR,
                                   
                                   "1-Encadrant"="1",
                                   "2-Superviseur"="2",
                                   "3-Pas encadrant"="3",
                                   "3-Pas encadrant"="9",
                                   
  )
  
  ct$ENCADR2 <- fct_recode(
    ct$ENCADR,
    "1-Encadrant"     = "1-Encadrant"    ,
    "3-Pas encadrant" = "2-Superviseur"  ,
    "3-Pas encadrant" = "3-Pas encadrant"
  )
  
  ct$ENCADR3 <- fct_recode(
    ct$ENCADR,
    "1-Encadrant/superviseur" = "1-Encadrant"    ,
    "1-Encadrant/superviseur" = "2-Superviseur"  ,
    "3-Pas encadrant"         = "3-Pas encadrant"
  )
  
  
  } 





if("TITPUBR" %in% names(ct) & !is.numeric(ct$TITPUBR) ) {
  ct$TITPUBR <- forcats::fct_recode(ctraw$TITPUBR,
                                    
                                    "1-Élève fonctionnaire ou fonctionnaire stagiaire"="1",
                                    "2-Titulaire civil"="2",
                                    "3-Militaire"="3",
                                    "4-Agent contractuel"="4",
                                    "5-Ouvrier d’État"="5",
                                    "6-Assistant(e) maternel(le)"="6",
                                    "7-Personnel médical hospitalier"="7",
                                    "8-Enseignant de l'enseignement privé sous contrat"="8",
                                    "9-Stagiaire"="9",
                                    
  ) } 

if("TITPUB" %in% names(ct) & !is.numeric(ct$TITPUB) ) {
  ct$TITPUB <- forcats::fct_recode(ctraw$TITPUB,
                                   "1-Élève fonctionnaire ou fonctionnaire stagiaire"="1",
                                   "2-Titulaire civil ou militaire"="2",
                                   "3-Agent contractuel, ouvrier d’État, assistante maternelle, praticien hospitalier"="3",
                                   "4-Stagiaire"="4"
                                   
  ) }

if("RDET" %in% names(ct) & !is.numeric(ct$RDET) ) {
  ct$RDET <- forcats::fct_recode(ctraw$RDET,
                                 
                                 "1-Précarité choisie"="1",
                                 "2-Précarité subie"="2",
                                 "NULL"="8",
                                 "2-Précarité subie"="9",
                                 
  ) } 
if("LIEUW" %in% names(ct) & !is.numeric(ct$LIEUW) ) {
  ct$LIEUW <- forcats::fct_recode(ctraw$LIEUW,
                                  
                                  "1-Établissement employeur"="1",
                                  "2-Différents étab employeur"="2",
                                  "3-Établissement client"="3",
                                  "4-Différents étab clients"="4",
                                  "5-En déplacement"="5",
                                  "6-Chez particuliers"="6",
                                  "7-A domicile"="7",
                                  "8-Chantiers"="8",
                                  "9-Autre situation"="9",
                                  
  ) } 
if("ACTIVFIN" %in% names(ct) & !is.numeric(ct$ACTIVFIN) ) {
  ct$ACTIVFIN <- forcats::fct_recode(ctraw$ACTIVFIN,
                                     
                                     "01-Culture et production animale, chasse et services annexes"="01",
                                     "02-Sylviculture et exploitation forestière"="02",
                                     "03-Pêche et aquaculture"="03",
                                     "05-Extraction de houille et de lignite"="05",
                                     "06-Extraction d'hydrocarbures"="06",
                                     "07-Extraction de minerais métalliques"="07",
                                     "08-Autres industries extractives"="08",
                                     "09-Services de soutien aux industries extractives"="09",
                                     "10-Industries alimentaires"="10",
                                     "11-Fabrication de boissons"="11",
                                     "12-Fabrication de produits à base de tabac"="12",
                                     "13-Fabrication de textiles"="13",
                                     "14-Industrie de l'habillement"="14",
                                     "15-Industrie du cuir et de la chaussure"="15",
                                     "16-Travail du bois et fabrication d'articles en bois et en liège, à l'exception des meubles ,fabrication d'articles en vannerie et sparterie"="16",
                                     "17-Industrie du papier et du carton"="17",
                                     "18-Imprimerie et reproduction d'enregistrements"="18",
                                     "19-Cokéfaction et raffinage"="19",
                                     "20-Industrie chimique"="20",
                                     "21-Industrie pharmaceutique"="21",
                                     "22-Fabrication de produits en caoutchouc et en plastique"="22",
                                     "23-Fabrication d'autres produits minéraux non métalliques"="23",
                                     "24-Métallurgie"="24",
                                     "25-Fabrication de produits métalliques, à l'exception des machines et des équipements"="25",
                                     "26-Fabrication de produits informatiques, électroniques et optiques"="26",
                                     "27-Fabrication d'équipements électriques"="27",
                                     "28-Fabrication de machines et équipements n.c.a."="28",
                                     "29-Industrie automobile"="29",
                                     "30-Fabrication d'autres matériels de transport"="30",
                                     "31-Fabrication de meubles"="31",
                                     "32-Autres industries manufacturières"="32",
                                     "33-Réparation et installation de machines et d'équipements"="33",
                                     "35-Production et distribution d'électricité, de gaz, de vapeur et d'air conditionné"="35",
                                     "36-Captage, traitement et distribution d'eau"="36",
                                     "37-Collecte et traitement des eaux usées"="37",
                                     "38-Collecte, traitement et élimination des déchets ,récupération"="38",
                                     "39-Dépollution et autres services de gestion des déchets"="39",
                                     "41-Construction de bâtiments"="41",
                                     "42-Génie civil"="42",
                                     "43-Travaux de construction spécialisés"="43",
                                     "45-Commerce et réparation d'automobiles et de motocycles"="45",
                                     "46-Commerce de gros, à l'exception des automobiles et des motocycles"="46",
                                     "47-Commerce de détail, à l'exception des automobiles et des motocycles"="47",
                                     "49-Transports terrestres et transport par conduites"="49",
                                     "50-Transports par eau"="50",
                                     "51-Transports aériens"="51",
                                     "52-Entreposage et services auxiliaires des transports"="52",
                                     "53-Activités de poste et de courrier"="53",
                                     "55-Hébergement"="55",
                                     "56-Restauration"="56",
                                     "58-Édition"="58",
                                     "59-Production de films cinématographiques, de vidéo et de programmes de télévision ,enregistrement sonore et édition musicale"="59",
                                     "60-Programmation et diffusion"="60",
                                     "61-Télécommunications"="61",
                                     "62-Programmation, conseil et autres activités informatiques"="62",
                                     "63-Services d'information"="63",
                                     "64-Activités des services financiers, hors assurance et caisses de retraite"="64",
                                     "65-Assurance"="65",
                                     "66-Activités auxiliaires de services financiers et d'assurance"="66",
                                     "68-Activités immobilières"="68",
                                     "69-Activités juridiques et comptables"="69",
                                     "70-Activités des sièges sociaux ,conseil de gestion"="70",
                                     "71-Activités d'architecture et d'ingénierie ,activités de contrôle et analyses techniques"="71",
                                     "72-Recherche-développement scientifique"="72",
                                     "73-Publicité et études de marché"="73",
                                     "74-Autres activités spécialisées, scientifiques et techniques"="74",
                                     "75-Activités vétérinaires"="75",
                                     "77-Activités de location et location-bail"="77",
                                     "78-Activités liées à l'emploi"="78",
                                     "79-Activités des agences de voyage, voyagistes, services de réservation et activités connexes"="79",
                                     "80-Enquêtes et sécurité"="80",
                                     "81-Services relatifs aux bâtiments et aménagement paysager"="81",
                                     "82-Activités administratives et autres activités de soutien aux entreprises"="82",
                                     "84-Administration publique et défense ,sécurité sociale obligatoire"="84",
                                     "85-Enseignement"="85",
                                     "86-Activités pour la santé humaine"="86",
                                     "87-Hébergement médico-social et social"="87",
                                     "88-Action sociale sans hébergement"="88",
                                     "90-Activités créatives, artistiques et de spectacle"="90",
                                     "91-Bibliothèques, archives, musées et autres activités culturelles"="91",
                                     "92-Organisation de jeux de hasard et d'argent"="92",
                                     "93-Activités sportives, récréatives et de loisirs"="93",
                                     "94-Activités des organisations associatives"="94",
                                     "95-Réparation d'ordinateurs et de biens personnels et domestiques"="95",
                                     "96-Autres services personnels"="96",
                                     "97-Activités des ménages en tant qu'employeurs de personnel domestique"="97",
                                     "98-Activités indifférenciées des ménages en tant que producteurs de biens et services pour usage propre"="98",
                                     "99-Activités des organisations et organismes extraterritoriaux"="99",
                                     
  ) } 
if("CLIENT" %in% names(ct) & !is.numeric(ct$CLIENT) ) {
  ct$CLIENT <- forcats::fct_recode(ctraw$CLIENT,
                                   
                                   "1-Particuliers"="1",
                                   "2-Entreprise unique"="2",
                                   "3-Entreprises"="3",
                                   "4-Entreprises et part"="4",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("FRANCE" %in% names(ct) & !is.numeric(ct$FRANCE) ) {
  ct$FRANCE <- forcats::fct_recode(ctraw$FRANCE,
                                   
                                   "1-Étab France"="1",
                                   "2-Pas étab France"="2",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 



if("EVOLEFF" %in% names(ct) & !is.numeric(ct$EVOLEFF) ) {
  ct$EVOLEFF <- forcats::fct_recode(ctraw$EVOLEFF,
                                    
                                    "1-Effectifs <"="1",
                                    "2-Effectifs >"="2",
                                    "3-Effectifs ="="3",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("REMPERF" %in% names(ct) & !is.numeric(ct$REMPERF) ) {
  ct$REMPERF <- forcats::fct_recode(ctraw$REMPERF,
                                    
                                    "1-Salaire performance fort"="1",
                                    "2-Salaire performance faible"="2",
                                    "3-Pas salaire perf"="3",
                                    "NULL"="8",
                                    "9-NSP"="9",
                                    
  ) } 
if("PAYECOM" %in% names(ct) & !is.numeric(ct$PAYECOM) ) {
  ct$PAYECOM <- forcats::fct_recode(ctraw$PAYECOM,
                                    
                                    "1-Très bien payé"="1",
                                    "2-Bien payé"="2",
                                    "3-Normalement payé"="3",
                                    "4-Plutôt mal payé"="4",
                                    "5-Très mal payé"="5",
                                    "NULL"="8",
                                    "3-Normalement payé"="9",
                                    
  ) } 
if("TPP" %in% names(ct) & !is.numeric(ct$TPP) ) {
  ct$TPP <- forcats::fct_recode(ctraw$TPP,
                                
                                "1-Temps complet"="1",
                                "2-Temps partiel"="2",
                                "NULL"="8",
                                "NULL"="9",
                                
  ) } 
if("TXTPPB" %in% names(ct) & !is.numeric(ct$TXTPPB) ) {
  ct$TXTPPB <- forcats::fct_recode(ctraw$TXTPPB,
                                   
                                   "1-Moins de 50%"="1",
                                   "2-Mi-temps"="2",
                                   "3-50 à 80 %"="3",
                                   "4-80%"="4",
                                   "5-+ de 80 %"="5",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("RAISTP" %in% names(ct) & !is.numeric(ct$RAISTP) ) {
  ct$RAISTP <- forcats::fct_recode(ctraw$RAISTP,
                                   
                                   "1-Pour exercer une autre activité professionnelle"="1",
                                   "2-Pour suivre des études ou une formation"="2",
                                   "3-Pour raison de santé"="3",
                                   "4-Vous n’avez pas trouvé d’emploi à temps plein"="4",
                                   "5-Pour vous occuper de vos enfants"="5",
                                   "6-Pour vous occuper d’un (autre) membre de votre famille"="6",
                                   "7-Pour faire des travaux domestiques"="7",
                                   "NULL"="8",
                                   "9-Pour une autre raison"="9",
                                   
  ) } 
if("REPOS" %in% names(ct) & !is.numeric(ct$REPOS) ) {
  ct$REPOS <- forcats::fct_recode(ctraw$REPOS,
                                  
                                  "1-48h repos"="1",
                                  "2-Pas 48h"="2",
                                  "NULL"="8",
                                  "2-Pas 48h"="9",
                                  
  ) } 
if("HORANGT" %in% names(ct) & !is.numeric(ct$HORANGT) ) {
  ct$HORANGT <- forcats::fct_recode(ctraw$HORANGT,
                                    
                                    "1-Modifier horaires"="1",
                                    "2-Pas mod horaires"="2",
                                    "3-Sans objet (pas de collègues)"="3",
                                    "NULL"="8",
                                    "2-Pas mod horaires"="9",
                                    
  ) } 
if("PREVIS" %in% names(ct) & !is.numeric(ct$PREVIS) ) {
  ct$PREVIS <- forcats::fct_recode(ctraw$PREVIS,
                                   
                                   "1-Horaires mois"="1",
                                   "2-Horaires semaine pro"="2",
                                   "3-Horaires demain"="3",
                                   "4-Pas previsions h"="4",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("HORVAR" %in% names(ct) & !is.numeric(ct$HORVAR) ) {
  ct$HORVAR <- forcats::fct_recode(ctraw$HORVAR,
                                   
                                   "1-Horaires stables"="1",
                                   "2-2x8"="2",
                                   "3-3x8 ou +"="3",
                                   "4-Horaires variables"="4",
                                   "NULL"="8",
                                   "4-Horaires variables"="9",
                                   
  ) } 
if("PERIODE" %in% names(ct) & !is.numeric(ct$PERIODE) ) {
  ct$PERIODE <- forcats::fct_recode(ctraw$PERIODE,
                                    
                                    "1-Journée morcelée >3h"="1",
                                    "2-Pas morcelée"="2",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("CONTROLE" %in% names(ct) & !is.numeric(ct$CONTROLE) ) {
  ct$CONTROLE <- forcats::fct_recode(ctraw$CONTROLE,
                                     
                                     "1-Aucun contrôle"="1",
                                     "2-Pointeuse ou badge"="2",
                                     "3-Signature ou fiche horaire"="3",
                                     "4-Contrôle encadrement"="4",
                                     "5-Contrôle autres"="5",
                                     "6-Autre"="6",
                                     "NULL"="8",
                                     "1-Aucun contrôle"="9",
                                     
  ) } 
if("PTMATIN" %in% names(ct) & !is.numeric(ct$PTMATIN) ) {
  ct$PTMATIN <- forcats::fct_recode(ctraw$PTMATIN,
                                    
                                    "1-Matin habituel"="1",
                                    "2-Matin occasion"="2",
                                    "3-Pas matin"="3",
                                    "NULL"="8",
                                    "3-Pas matin"="9",
                                    
  ) } 
if("SOIR" %in% names(ct) & !is.numeric(ct$SOIR) ) {
  ct$SOIR <- forcats::fct_recode(ctraw$SOIR,
                                 
                                 "1-Soir habituel"="1",
                                 "2-Soir occasion"="2",
                                 "3-Pas soir"="3",
                                 "NULL"="8",
                                 "3-Pas soir"="9",
                                 
  ) } 
if("NUIT" %in% names(ct) & !is.numeric(ct$NUIT) ) {
  ct$NUIT <- forcats::fct_recode(ctraw$NUIT,
                                 
                                 "1-Nuit habituel"="1",
                                 "2-Nuit occasion"="2",
                                 "3-Pas nuit"="3",
                                 "NULL"="8",
                                 "3-Pas nuit"="9",
                                 
  ) } 
if("NBNUIT" %in% names(ct) & !is.numeric(ct$NBNUIT) ) {
  ct$NBNUIT <- forcats::fct_recode(ctraw$NBNUIT,
                                   
                                   "0-Pas nuit"="0",
                                   "1-1 à 3 nuit/an"="1",
                                   "2-4 à 11"="2",
                                   "3-12 à 23"="3",
                                   "4-24 à 49"="4",
                                   "5-50 à 99"="5",
                                   "6-100 à 199"="6",
                                   "7-200 ou +"="7",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("HSUP" %in% names(ct) & !is.numeric(ct$HSUP) ) {
  ct$HSUP <- forcats::fct_recode(ctraw$HSUP,
                                 
                                 "1-Tj dépasse horaires"="1",
                                 "2-Sv dépasse h"="2",
                                 "3-Pf dépasse h"="3",
                                 "4-Pas dépasse h"="4",
                                 
  ) } 
if("HSUPCOMP" %in% names(ct) & !is.numeric(ct$HSUPCOMP) ) {
  ct$HSUPCOMP <- forcats::fct_recode(ctraw$HSUPCOMP,
                                     
                                     "1-HSUP compensées"="1",
                                     "2-HSUP en partie comp"="2",
                                     "3-HSUP pas compensées"="3",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("JOINDRE" %in% names(ct) & !is.numeric(ct$JOINDRE) ) {
  ct$JOINDRE <- forcats::fct_recode(ctraw$JOINDRE,
                                    
                                    "1-Joint hors travail"="1",
                                    "2-Pas joint hors trav"="2",
                                    "3-Sans objet: travaille seul"="3",
                                    "NULL"="8",
                                    "2-Pas joint hors trav"="9",
                                    
  ) } 
if("JOINEXT" %in% names(ct) & !is.numeric(ct$JOINEXT) ) {
  ct$JOINEXT <- forcats::fct_recode(ctraw$JOINEXT,
                                    
                                    "1-Joint HT client"="1",
                                    "2-Pas joint client"="2",
                                    "3-Sans objet: travaille seul"="3",
                                    "NULL"="8",
                                    "2-Pas joint client"="9",
                                    
  ) } 
if("COMBEXT" %in% names(ct) & !is.numeric(ct$COMBEXT) ) {
  ct$COMBEXT <- forcats::fct_recode(ctraw$COMBEXT,
                                    
                                    "1-Joint 1 à 5 fois"="1",
                                    "2-Joint 6 à 20 fois"="2",
                                    "3-Joint + 20 fois"="3",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("MAISON" %in% names(ct) & !is.numeric(ct$MAISON) ) {
  ct$MAISON <- forcats::fct_recode(ctraw$MAISON,
                                   
                                   "1-Tj travail maison"="1",
                                   "2-Sv travail maison"="2",
                                   "3-Pf travail maison"="3",
                                   "4-Jm travail maison"="4",
                                   "5-Sans objet (travail à domicile, impossibilité)"="5",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("MISSION" %in% names(ct) & !is.numeric(ct$MISSION) ) {
  ct$MISSION <- forcats::fct_recode(ctraw$MISSION,
                                    
                                    "1-Déplacement 1 /sem ou +"="1",
                                    "2-Déplacement 1 à 3 /mois"="2",
                                    "3-Déplacement parfois"="3",
                                    "4-Pas dormir hors maison"="4",
                                    "NULL"="8",
                                    "4-Pas dormir hors maison"="9",
                                    
  ) } 
if("ARETMAL" %in% names(ct) & !is.numeric(ct$ARETMAL) ) {
  ct$ARETMAL <- forcats::fct_recode(ctraw$ARETMAL,
                                    
                                    "0-Pas d’arrêt"="0",
                                    "1-Un arrêt"="1",
                                    "2-Deux arrêts"="2",
                                    "3-Trois arrêts +"="3",
                                    "NULL"="8",
                                    "0-Pas d’arrêt"="9",
                                    
  ) } 
if("URGFAM" %in% names(ct) & !is.numeric(ct$URGFAM) ) {
  ct$URGFAM <- forcats::fct_recode(ctraw$URGFAM,
                                   
                                   "1-Facile absence imprévue"="1",
                                   "2-Pas facile absence"="2",
                                   "3-Impossible absence"="3",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("CVFVP" %in% names(ct) & !is.numeric(ct$CVFVP) ) {
  ct$CVFVP <- forcats::fct_recode(ctraw$CVFVP,
                                  
                                  "1-horaires/vie sociale TB"="1",
                                  "2-h bien"="2",
                                  "3-h pas très bien"="3",
                                  "4-h pas bien du tout"="4",
                                  "NULL"="8",
                                  "NULL"="9",
                                  
  ) } 
if("FATIGUE" %in% names(ct) & !is.numeric(ct$FATIGUE) ) {
  ct$FATIGUE <- forcats::fct_recode(ctraw$FATIGUE,
                                    
                                    "1-Tj fatigué HT"="1",
                                    "2-Sv fatigué HT"="2",
                                    "3-Pf fatigué HT"="3",
                                    "4-Jm fatigué HT"="4",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("INDISPO" %in% names(ct) & !is.numeric(ct$INDISPO) ) {
  ct$INDISPO <- forcats::fct_recode(ctraw$INDISPO,
                                    
                                    "1-Tj indispo proches"="1",
                                    "2-Sv indispo proches"="2",
                                    "3-Pf indispo proches"="3",
                                    "4-Jm indispo proches"="4",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("DOMEST" %in% names(ct) & !is.numeric(ct$DOMEST) ) {
  ct$DOMEST <- forcats::fct_recode(ctraw$DOMEST,
                                   
                                   "1-Tdom - 2h"="1",
                                   "2-Tdom 3 à 6h"="2",
                                   "3-Tdom 7 et 9h"="3",
                                   "4-Tdom 10 à 12h"="4",
                                   "5-Tdom 12h +"="5",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("CWDEBOU" %in% names(ct) & !is.numeric(ct$CWDEBOU) ) {
  ct$CWDEBOU <- forcats::fct_recode(ctraw$CWDEBOU,
                                    
                                    "1-CP rester debout"="1",
                                    "2-Pas debout"="2",
                                    "NULL"="8",
                                    "2-Pas debout"="9",
                                    
  ) } 
if("CWPOSTU" %in% names(ct) & !is.numeric(ct$CWPOSTU) ) {
  ct$CWPOSTU <- forcats::fct_recode(ctraw$CWPOSTU,
                                    
                                    "1-CP posture"="1",
                                    "2-Pas CP posture"="2",
                                    "NULL"="8",
                                    "2-Pas CP posture"="9",
                                    
  ) } 
if("CWDEPLA" %in% names(ct) & !is.numeric(ct$CWDEPLA) ) {
  ct$CWDEPLA <- forcats::fct_recode(ctraw$CWDEPLA,
                                    
                                    "1-CP déplacements pied"="1",
                                    "2-Pas CP pied"="2",
                                    "NULL"="8",
                                    "2-Pas CP pied"="9",
                                    
  ) } 
if("CWLOURD" %in% names(ct) & !is.numeric(ct$CWLOURD) ) {
  ct$CWLOURD <- forcats::fct_recode(ctraw$CWLOURD,
                                    
                                    "1-CP charges" ="1",
                                    "2-Pas CP charges"="2",
                                    "NULL"="8",
                                    "2-Pas CP charges"="9",
                                    
  ) } 
if("CWMVT" %in% names(ct) & !is.numeric(ct$CWMVT) ) {
  ct$CWMVT <- forcats::fct_recode(ctraw$CWMVT,
                                  
                                  "1-CP mouvements fatiguants"="1",
                                  "2-Pas CP mv fatiguants"="2",
                                  "NULL"="8",
                                  "2-Pas CP mv fatiguants"="9",
                                  
  ) } 
if("CWVIB" %in% names(ct) & !is.numeric(ct$CWVIB) ) {
  ct$CWVIB <- forcats::fct_recode(ctraw$CWVIB,
                                  
                                  "1-CP secousses"="1",
                                  "2-Pas CP secousses"="2",
                                  "NULL"="8",
                                  "2-Pas CP secousses"="9",
                                  
  ) } 
if("CWVUE" %in% names(ct) & !is.numeric(ct$CWVUE) ) {
  ct$CWVUE <- forcats::fct_recode(ctraw$CWVUE,
                                  
                                  "1-CP fixer des yeux"="1",
                                  "2-Pas CP fixer"="2",
                                  "NULL"="8",
                                  "2-Pas CP fixer"="9",
                                  
  ) } 
if("CONDUITE" %in% names(ct) & !is.numeric(ct$CONDUITE) ) {
  ct$CONDUITE <- forcats::fct_recode(ctraw$CONDUITE,
                                     
                                     "1-Conduite véhicules"="1",
                                     "2-Pas conduite"="2",
                                     "NULL"="8",
                                     "2-Pas conduite"="9",
                                     
  ) } 
if("SECFUPOU" %in% names(ct) & !is.numeric(ct$SECFUPOU) ) {
  ct$SECFUPOU <- forcats::fct_recode(ctraw$SECFUPOU,
                                     
                                     "1-Fumées poussières"="1",
                                     "2-Pas fumées"="2",
                                     "NULL"="8",
                                     "2-Pas fumées"="9",
                                     
  ) } 
if("SECTOXNO" %in% names(ct) & !is.numeric(ct$SECTOXNO) ) {
  ct$SECTOXNO <- forcats::fct_recode(ctraw$SECTOXNO,
                                     
                                     "1-Prod dangereux"="1",
                                     "2-Pas prod dang"="2",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("ENTENDR" %in% names(ct) & !is.numeric(ct$ENTENDR) ) {
  ct$ENTENDR <- forcats::fct_recode(ctraw$ENTENDR,
                                    
                                    "1-Pas bruyant"="1",
                                    "2-Bruyant"="2",
                                    "3-Très bruyant"="3",
                                    "NULL"="8",
                                    "1-Pas bruyant"="9",
                                    
  ) } 
if("RISK" %in% names(ct) & !is.numeric(ct$RISK) ) {
  ct$RISK <- forcats::fct_recode(ctraw$RISK,
                                 
                                 "1-Info risques"="1",
                                 "2-Pas inf risq"="2",
                                 "NULL"="8",
                                 "2-Pas inf risq"="9",
                                 
  ) } 
if("FORMSEC" %in% names(ct) & !is.numeric(ct$FORMSEC) ) {
  ct$FORMSEC <- forcats::fct_recode(ctraw$FORMSEC,
                                    
                                    "1-Form risques"="1",
                                    "2-Pas form risq"="2",
                                    "NULL"="8",
                                    "2-Pas form risq"="9",
                                    
  ) } 
if("DOCEVAL" %in% names(ct) & !is.numeric(ct$DOCEVAL) ) {
  ct$DOCEVAL <- forcats::fct_recode(ctraw$DOCEVAL,
                                    
                                    "1-Docu risque"="1",
                                    "2-Pas docu risq"="2",
                                    "NULL"="8",
                                    "2-Pas docu risq"="9",
                                    
  ) } 
if("MEDECIN" %in% names(ct) & !is.numeric(ct$MEDECIN) ) {
  ct$MEDECIN <- forcats::fct_recode(ctraw$MEDECIN,
                                    
                                    "1-Visite médicale - 1 an"="1",
                                    "2-VM 1 à 2 ans"="2",
                                    "3-VM 2 à 5 ans"="3",
                                    "4-VM + 5 ans"="4",
                                    "5-Pas VM"="5",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("ACCIDT" %in% names(ct) & !is.numeric(ct$ACCIDT) ) {
  ct$ACCIDT <- forcats::fct_recode(ctraw$ACCIDT,
                                   
                                   "0-0 accidents"="0",
                                   "1-1 accidents"="1",
                                   "2-2 accidents"="2",
                                   "3-3 accid +"="3",
                                   "NULL"="8",
                                   "0-0 accidents"="9",
                                   
  ) } 
if("ARRET" %in% names(ct) & !is.numeric(ct$ARRET) ) {
  ct$ARRET <- forcats::fct_recode(ctraw$ARRET,
                                  
                                  "1-Arrêt accident"="1",
                                  "2-Pas arrêt accident"="2",
                                  "NULL"="8",
                                  "2-Pas arrêt accident"="9",
                                  
  ) } 
if("RWDEP" %in% names(ct) & !is.numeric(ct$RWDEP) ) {
  ct$RWDEP <- forcats::fct_recode(ctraw$RWDEP,
                                  
                                  "1-CR chaîne"="1",
                                  "2-Pas CR chaîne"="2",
                                  "NULL"="8",
                                  "2-Pas CR chaîne"="9",
                                  
  ) } 
if("RWCAD" %in% names(ct) & !is.numeric(ct$RWCAD) ) {
  ct$RWCAD <- forcats::fct_recode(ctraw$RWCAD,
                                  
                                  "1-CR cadence"="1",
                                  "2-Pas CR cadence"="2",
                                  "NULL"="8",
                                  "2-Pas CR cadence"="9",
                                  
  ) } 
if("RWTECH" %in% names(ct) & !is.numeric(ct$RWTECH) ) {
  ct$RWTECH <- forcats::fct_recode(ctraw$RWTECH,
                                   
                                   "1-CR tech"="1",
                                   "2-Pas CR tech"="2",
                                   "NULL"="8",
                                   "2-Pas CR tech"="9",
                                   
  ) } 
if("RWCOLEG" %in% names(ct) & !is.numeric(ct$RWCOLEG) ) {
  ct$RWCOLEG <- forcats::fct_recode(ctraw$RWCOLEG,
                                    "1-CR dépendance collègues"="1",
                                    "2-Pas CR coll"="2",
                                    "NULL"="8",
                                    "2-Pas CR coll"="9",
                                    
  ) } 
if("RWNORMH" %in% names(ct) & !is.numeric(ct$RWNORMH) ) {
  ct$RWNORMH <- forcats::fct_recode(ctraw$RWNORMH,
                                    
                                    "1-CR delais <1h"="1",
                                    "2-Pas CR delais 1h"="2",
                                    "NULL"="8",
                                    "2-Pas CR delais 1h"="9",
                                    
  ) } 
if("RWNORMJ" %in% names(ct) & !is.numeric(ct$RWNORMJ) ) {
  ct$RWNORMJ <- forcats::fct_recode(ctraw$RWNORMJ,
                                    
                                    "1-CR delais <1j"="1",
                                    "2-Pas CR delais 1j"="2",
                                    "NULL"="8",
                                    "2-Pas CR delais 1j"="9",
                                    
  ) } 
if("RWDEM" %in% names(ct) & !is.numeric(ct$RWDEM) ) {
  ct$RWDEM <- forcats::fct_recode(ctraw$RWDEM,
                                  
                                  "1-CR public"="1",
                                  "2-Pas CR public"="2",
                                  "NULL"="8",
                                  "2-Pas CR public"="9",
                                  
  ) } 
if("RWDEMAND" %in% names(ct) & !is.numeric(ct$RWDEMAND) ) {
  ct$RWDEMAND <- forcats::fct_recode(ctraw$RWDEMAND,
                                     
                                     "1-CR public non immédiat"="1",
                                     "2-Pas CR public non immédiat"="2",
                                     "NULL"="8",
                                     "2-Pas CR public non immédiat"="9",
                                     
  ) } 
if("RWSURV" %in% names(ct) & !is.numeric(ct$RWSURV) ) {
  ct$RWSURV <- forcats::fct_recode(fct_explicit_na(ctraw$RWSURV, na_level = "2-Pas CR surv chef"),
                                   "1-CR surveillance chef" ="1",
                                   "2-Pas CR surv chef" ="2",
                                   "NULL"="8",
                                   "2-Pas CR surv chef" ="9",
                                   
  ) } 
if("RWINFO" %in% names(ct) & !is.numeric(ct$RWINFO) ) {
  ct$RWINFO <- forcats::fct_recode(ctraw$RWINFO,
                                   
                                   "1-CR informatisées" ="1",
                                   "2-Pas CR info"="2",
                                   "NULL"="8",
                                   "2-Pas CR info"="9",
                                   
  ) } 

if("CHAINE" %in% names(ct) & !is.numeric(ct$CHAINE) ) {
  ct$CHAINE <- forcats::fct_recode(ctraw$CHAINE,
                                   
                                   "1-À la chaîne"="1",
                                   "2-Pas chaîne"="2",
                                   "NULL"="8",
                                   "2-Pas chaîne"="9",
                                   
  ) } 
if("DEPECH" %in% names(ct) & !is.numeric(ct$DEPECH) ) {
  ct$DEPECH <- forcats::fct_recode(ctraw$DEPECH,
                                   
                                   "1-Tj dépêcher"="1",
                                   "2-Sv dépêcher"="2",
                                   "3-Pf dépêcher"="3",
                                   "4-Jm dépêcher"="4",
                                   "NULL"="8",
                                   "4-Jm dépêcher"="9",
                                   
  ) } 
if("DEBORD" %in% names(ct) & !is.numeric(ct$DEBORD) ) {
  ct$DEBORD <- forcats::fct_recode(ctraw$DEBORD,
                                   
                                   "1-Interrompu"="1",
                                   "2-Pas interrompu"="2",
                                   "NULL"="8",
                                   "2-Pas interrompu"="9",
                                   
  ) } 
if("INTERACT" %in% names(ct) & !is.numeric(ct$INTERACT) ) {
  ct$INTERACT <- forcats::fct_recode(ctraw$INTERACT,
                                     
                                     "1-Interrompu négatif"="1",
                                     "2-Interrompu sans csq"="2",
                                     "3-Interrompu positif"="3",
                                     "NULL"="8",
                                     "2-Interrompu sans csq"="9",
                                     
  ) } 
if("OBJECTIF" %in% names(ct) & !is.numeric(ct$OBJECTIF) ) {
  ct$OBJECTIF <- forcats::fct_recode(ctraw$OBJECTIF,
                                     
                                     "1-Objectifs chiffrés"="1",
                                     "2-Pas obj chiffrés"="2",
                                     "NULL"="8",
                                     "2-Pas obj chiffrés"="9",
                                     
  ) } 
if("OBJMODIF" %in% names(ct) & !is.numeric(ct$OBJMODIF) ) {
  ct$OBJMODIF <- forcats::fct_recode(ctraw$OBJMODIF,
                                     
                                     "1-Obj modif seul"="1",
                                     "2-Obj modif chef"="2",
                                     "3-Obj modif collectif"="3",
                                     "4-Obj pas modif"="4",
                                     "NULL"="8",
                                     "4-Obj pas modif"="9",
                                     
  ) } 
if("OBJATTEIN" %in% names(ct) & !is.numeric(ct$OBJATTEIN) ) {
  ct$OBJATTEIN <- forcats::fct_recode(ctraw$OBJATTEIN,
                                      
                                      "1-Tj du mal objectifs"="1",
                                      "2-Sv du mal"="2",
                                      "3-Pf du mal"="3",
                                      "4-Jm du mal"="4",
                                      "NULL"="8",
                                      "4-Jm du mal"="9",
                                      
  ) } 
if("DELAIS" %in% names(ct) & !is.numeric(ct$DELAIS) ) {
  ct$DELAIS <- forcats::fct_recode(ctraw$DELAIS,
                                   
                                   "1-Faire varier délais"="1",
                                   "2-Pas varier délais"="2",
                                   "3-Pas de délais"="3",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("PUBLIC" %in% names(ct) & !is.numeric(ct$PUBLIC) ) {
  ct$PUBLIC <- forcats::fct_recode(ctraw$PUBLIC,
                                   
                                   "1-Contact public"="1",
                                   "2-Pas contact pub"="2",
                                   "NULL"="8",
                                   "2-Pas contact pub"="9",
                                   
  ) } 
if("PUBLIC1" %in% names(ct) & !is.numeric(ct$PUBLIC1) ) {
  ct$PUBLIC1 <- forcats::fct_recode(ctraw$PUBLIC1,
                                    
                                    "1-Tj face à face"="1",
                                    "2-Sv face à face"="2",
                                    "3-Pf face à face"="3",
                                    "4-Jm face à face"="4",
                                    "NULL"="8",
                                    "4-Jm face à face"="9",
                                    
  ) } 
if("PUBLIC2" %in% names(ct) & !is.numeric(ct$PUBLIC2) ) {
  ct$PUBLIC2 <- forcats::fct_recode(ctraw$PUBLIC2,
                                    
                                    "1-Tj téléphone"="1",
                                    "2-Sv téléphone"="2",
                                    "3-Pf téléphone"="3",
                                    "4-Jm téléphone"="4",
                                    "NULL"="8",
                                    "4-Jm téléphone"="9",
                                    
  ) } 
if("PUBLIC3" %in% names(ct) & !is.numeric(ct$PUBLIC3) ) {
  ct$PUBLIC3 <- forcats::fct_recode(ctraw$PUBLIC3,
                                    
                                    "1-Tj mail"="1",
                                    "2-Sv mail"="2",
                                    "3-Pf mail"="3",
                                    "4-Jm mail"="4",
                                    "NULL"="8",
                                    "4-Jm mail"="9",
                                    
  ) } 
if("INTERUP" %in% names(ct) & !is.numeric(ct$INTERUP) ) {
  ct$INTERUP <- forcats::fct_recode(ctraw$INTERUP,
                                    
                                    "1-S'interrompre"="1",
                                    "2-Pas s'interrompre"="2",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("REMPLAS" %in% names(ct) & !is.numeric(ct$REMPLAS) ) {
  ct$REMPLAS <- forcats::fct_recode(ctraw$REMPLAS,
                                    
                                    "1-S'interrompre: remplacé"="1",
                                    "2-S'interrompre: pas remplacé"="2",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("COMMENT" %in% names(ct) & !is.numeric(ct$COMMENT) ) {
  ct$COMMENT <- forcats::fct_recode(fct_explicit_na(ctraw$COMMENT , na_level = "1-Autonomie procédurale"), 
                                    "2-Pas autonomie procédurale"="1",
                                    "1-Autonomie procédurale"="2",
                                    "1-Autonomie procédurale"="", #Indépendants
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) |> fct_relevel(sort) } 


if("STARK" %in% names(ct) & !is.numeric(ct$STARK) ) {
  ct$STARK <- forcats::fct_recode(ctraw$STARK,
                                  "4-Consignes respect strict"="1",
                                  "3-Consignes pf autrement"="2",
                                  "2-Consignes sv autrement"="3",
                                  "1-Pas de consignes"="4",
                                  "NULL"="8",
                                  "NULL"="9",
                                  
  ) |> 
    fct_relevel(sort) 
  } 

if("INCIDENT" %in% names(ct) & !is.numeric(ct$INCIDENT) ) {
  ct$INCIDENT <- forcats::fct_recode(ctraw$INCIDENT,
                                     "1-Autonomie incidents imprévus" ="1",
                                     "2-Autonomie incidents prévus"   ="2",
                                     "3-Pas autonomie incidents"      ="3",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 


if("PROCEDUR" %in% names(ct) & !is.numeric(ct$PROCEDUR) ) {
  ct$PROCEDUR <- forcats::fct_recode(ctraw$PROCEDUR,
                                     
                                     "1-Procédures 'qualité'"="1",
                                     "2-Pas proc 'qualité'"="2",
                                     "NULL"="8",
                                     "2-Pas proc 'qualité'"="9",
                                     
  ) } 
if("REPETE" %in% names(ct) & !is.numeric(ct$REPETE) ) {
  ct$REPETE <- forcats::fct_recode(ctraw$REPETE,
                                   
                                   "1-Gestes répétitifs"="1",
                                   "2-Pas gestes répétitifs"="2",
                                   "NULL"="8",
                                   "2-Pas gestes répétitifs"="9",
                                   
  ) } 


if("CYCLE" %in% names(ct) & !is.numeric(ct$CYCLE) ) {
  ct$CYCLE <- forcats::fct_recode(ctraw$CYCLE,
                                  
                                  "1-Cycle 1 minute -"="1",
                                  "2-Pas cycle 1m-"="2",
                                  "NULL"="8",
                                  "NULL"="9",
                                  
  ) } 
if("QUANTI" %in% names(ct) & !is.numeric(ct$QUANTI) ) {
  ct$QUANTI <- forcats::fct_recode(ctraw$QUANTI,
                                   
                                   "1-Tj chg quanti travail"="1",
                                   "2-Sv chg quanti travail"="2",
                                   "3-Pf chg quanti travail"="3",
                                   "4-Jm chg quanti travail"="4",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("INITIAT" %in% names(ct) & !is.numeric(ct$INITIAT) ) {
  ct$INITIAT <- forcats::fct_recode(ctraw$INITIAT,
                                    
                                    "1-Tj initiatives"="1",
                                    "2-Sv initiatives"="2",
                                    "3-Pf initiatives"="3",
                                    "4-Jm initiatives"="4",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("IDEE" %in% names(ct) & !is.numeric(ct$IDEE) ) {
  ct$IDEE <- forcats::fct_recode(ctraw$IDEE,
                                 
                                 "1-Tj propres idées"="1",
                                 "2-Sv propres idées"="2",
                                 "3-Pf propres idées"="3",
                                 "4-Jm propres idées"="4",
                                 "NULL"="8",
                                 "NULL"="9",
                                 
  ) } 
if("NOUVELLE" %in% names(ct) & !is.numeric(ct$NOUVELLE) ) {
  ct$NOUVELLE <- forcats::fct_recode(ctraw$NOUVELLE,
                                     
                                     "1-Apprendre des choses"="1",
                                     "2-Pas apprendre"="2",
                                     "NULL"="8",
                                     "2-Pas apprendre"="9",
                                     
  ) } 
if("RESTMAI" %in% names(ct) & !is.numeric(ct$RESTMAI) ) {
  ct$RESTMAI <- forcats::fct_recode(ctraw$RESTMAI,
                                    
                                    "1-Travail malade"="1",
                                    "2-Pas tr malade"="2",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("SIEG34" %in% names(ct) & !is.numeric(ct$SIEG34) ) {
  ct$SIEG34 <- forcats::fct_recode(ctraw$SIEG34,
                                   
                                   "1-Poste lié formation"="1",
                                   "2-Pas poste lié form"="2",
                                   "NULL"="8",
                                   "2-Pas poste lié form"="9",
                                   
  ) } 
if("AIDCHEF" %in% names(ct) & !is.numeric(ct$AIDCHEF) ) {
  ct$AIDCHEF <- forcats::fct_recode(ctraw$AIDCHEF,
                                    
                                    "1-Aide du chef"="1",
                                    "2-Pas aide chef"="2",
                                    "3-Pas de chef"="3",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("AIDCOLL" %in% names(ct) & !is.numeric(ct$AIDCOLL) ) {
  ct$AIDCOLL <- forcats::fct_recode(ctraw$AIDCOLL,
                                    
                                    "1-Aide collègues"="1",
                                    "2-Pas aide coll"="2",
                                    "3-Pas de collègue"="3",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("AIDCOLLNS" %in% names(ct) & !is.numeric(ct$AIDCOLLNS) ) {
  ct$AIDCOLLNS <- forcats::fct_recode(ctraw$AIDCOLLNS,
                                      
                                      "1-Aide collègues"="1",
                                      "2-Pas aide coll"="2",
                                      "3-Pas de collègue"="3",
                                      "NULL"="8",
                                      "NULL"="9",
                                      
  ) } 
if("TRAVSEUL" %in% names(ct) & !is.numeric(ct$TRAVSEUL) ) {
  ct$TRAVSEUL <- forcats::fct_recode(ctraw$TRAVSEUL,
                                     
                                     "1-Tj seul"="1",
                                     "2-Sv seul"="2",
                                     "3-Pf seul"="3",
                                     "4-Jm seul"="4",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("CHGTCOLL" %in% names(ct) & !is.numeric(ct$CHGTCOLL) ) {
  ct$CHGTCOLL <- forcats::fct_recode(ctraw$CHGTCOLL,
                                     
                                     "1-Mêmes collègues"="1",
                                     "2-Certains changé"="2",
                                     "3-Plupart changé"="3",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("COLLECT" %in% names(ct) & !is.numeric(ct$COLLECT) ) {
  ct$COLLECT <- forcats::fct_recode(ctraw$COLLECT,
                                    
                                    "1-Discussion orga"="1",
                                    "2-Pas disc orga"="2",
                                    "NULL"="8",
                                    "2-Pas disc orga"="9",
                                    
  ) } 
if("REUNION" %in% names(ct) & !is.numeric(ct$REUNION) ) {
  ct$REUNION <- forcats::fct_recode(fct_explicit_na(ctraw$REUNION, "NA"),
                                    "1-Réunions d'organisation"="1",
                                    "2-Pas réu orga"="2",
                                    "2-Pas réu orga"="NA",
                                    "NULL"="8",
                                    "2-Pas réu orga"="9",
                                    
  ) } 


if("ACCHEF" %in% names(ct) & !is.numeric(ct$ACCHEF) ) {
  ct$ACCHEF <- forcats::fct_recode(ctraw$ACCHEF,
                                   
                                   "1-Tj désaccord chef"="1",
                                   "2-Sv désaccord chef"="2",
                                   "3-Pf désaccord chef"="3",
                                   "4-Jm désaccord chef"="4",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("DICHEF" %in% names(ct) & !is.numeric(ct$DICHEF) ) {
  ct$DICHEF <- forcats::fct_recode(ctraw$DICHEF,
                                   
                                   "1-Désaccord chef: discuter"="1",
                                   "2-Désaccord chef: pas discuter"="2",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("ACCOL" %in% names(ct) & !is.numeric(ct$ACCOL) ) {
  ct$ACCOL <- forcats::fct_recode(ctraw$ACCOL,
                                  
                                  "1-Tj désaccord collègues"="1",
                                  "2-Sv désaccord collègues"="2",
                                  "3-Pf désaccord collègues"="3",
                                  "4-Jm désaccord collègues"="4",
                                  "5-Pas de collègues"="5",
                                  "NULL"="8",
                                  "NULL"="9",
                                  
  ) } 
if("DICOL" %in% names(ct) & !is.numeric(ct$DICOL) ) {
  ct$DICOL <- forcats::fct_recode(ctraw$DICOL,
                                  
                                  "1-Désaccord collègues: discuter"="1",
                                  "2-Désaccord collègues: pas discuter"="2",
                                  "NULL"="8",
                                  "NULL"="9",
                                  
  ) } 
if("CONFSAL" %in% names(ct) & !is.numeric(ct$CONFSAL) ) {
  ct$CONFSAL <- forcats::fct_recode(ctraw$CONFSAL,
                                    
                                    "1-Tj chef fait confiance"="1",
                                    "2-Sv chef fait confiance"="2",
                                    "3-Pf chef fait confiance"="3",
                                    "4-Jm chef fait confiance"="4",
                                    "5-Pas de chef"="5",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("INFOCONF" %in% names(ct) & !is.numeric(ct$INFOCONF) ) {
  ct$INFOCONF <- forcats::fct_recode(ctraw$INFOCONF,
                                     
                                     "1-Tj confiance infos chef"="1",
                                     "2-Sv confiance infos chef"="2",
                                     "3-Pf confiance infos chef"="3",
                                     "4-Jm confiance infos chef"="4",
                                     "5-Pas de chef"="5",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("DIFFIC" %in% names(ct) & !is.numeric(ct$DIFFIC) ) {
  ct$DIFFIC <- forcats::fct_recode(ctraw$DIFFIC,
                                   
                                   "1-Direction consulte"="1",
                                   "2-Direction décide seule"="2",
                                   "3-Direction ne fait rien"="3",
                                   "4-Difficultés jamais"="4",
                                   "5-Sans objet"="5",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 



if("BIENETR1" %in% names(ct) & !is.numeric(ct$BIENETR1) ) {
  ct$BIENETR1 <- forcats::fct_recode(ctraw$BIENETR1,
                                     
                                     "1-Tj chef bien-être"="1",
                                     "2-Sv chef bien-être"="2",
                                     "3-Pf chef bien-être"="3",
                                     "4-Jm chef bien-être"="4",
                                     "5-Pas de chef"="5",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("BIENETR2" %in% names(ct) & !is.numeric(ct$BIENETR2) ) {
  ct$BIENETR2 <- forcats::fct_recode(ctraw$BIENETR2,
                                     
                                     "1-Tj chef equitable"="1",
                                     "2-Sv chef equitable"="2",
                                     "3-Pf chef equitable"="3",
                                     "4-Jm chef equitable"="4",
                                     "5-Pas de chef"="5",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("TENSION1" %in% names(ct) & !is.numeric(ct$TENSION1) ) {
  ct$TENSION1 <- forcats::fct_recode(ctraw$TENSION1,
                                     
                                     "1-Tension public"="1",
                                     "2-Pas tension pub"="2",
                                     "NULL"="8",
                                     "2-Pas tension pub"="9",
                                     
  ) } 
if("TENSION2" %in% names(ct) & !is.numeric(ct$TENSION2) ) {
  ct$TENSION2 <- forcats::fct_recode(ctraw$TENSION2,
                                     
                                     "1-Tension chef"="1",
                                     "2-Pas tens chef"="2",
                                     "3-Pas de chef"="3",
                                     "NULL"="8",
                                     "2-Pas tens chef"="9",
                                     
  ) } 
if("TENSION3" %in% names(ct) & !is.numeric(ct$TENSION3) ) {
  ct$TENSION3 <- forcats::fct_recode(ctraw$TENSION3,
                                     
                                     "1-Tension collègues"="1",
                                     "2-Pas tension coll"="2",
                                     "3-Pas de collègues"="3",
                                     "NULL"="8",
                                     "2-Pas tension coll"="9",
                                     
  ) } 
if("TENSION4" %in% names(ct) & !is.numeric(ct$TENSION4) ) {
  ct$TENSION4 <- forcats::fct_recode(ctraw$TENSION4,
                                     
                                     "1-Tension subordonnés"="1",
                                     "2-Pas tension sub"="2",
                                     "NULL"="8",
                                     "2-Pas tension sub"="9",
                                     
  ) } 
if("DETRESSE" %in% names(ct) & !is.numeric(ct$DETRESSE) ) {
  ct$DETRESSE <- forcats::fct_recode(ctraw$DETRESSE,
                                     
                                     "1-Contact detresse"="1",
                                     "2-Pas ctct detresse"="2",
                                     "NULL"="8",
                                     "2-Pas ctct detresse"="9",
                                     
  ) } 
if("CALMER" %in% names(ct) & !is.numeric(ct$CALMER) ) {
  ct$CALMER <- forcats::fct_recode(ctraw$CALMER,
                                   
                                   "1-Calmer des gens"="1",
                                   "2-Pas calmer"="2",
                                   "NULL"="8",
                                   "2-Pas calmer"="9",
                                   
  ) } 
if("CONFLIT" %in% names(ct) & !is.numeric(ct$CONFLIT) ) {
  ct$CONFLIT <- forcats::fct_recode(ctraw$CONFLIT,
                                    
                                    "1-Gérer conflits"="1",
                                    "2-Pas gérer conflits"="2",
                                    "NULL"="8",
                                    "2-Pas gérer conflits"="9",
                                    
  ) } 
if("EMOTION" %in% names(ct) & !is.numeric(ct$EMOTION) ) {
  ct$EMOTION <- forcats::fct_recode(ctraw$EMOTION,
                                    
                                    "1-Tj ému/secoué"="1",
                                    "2-Sv ému/secoué"="2",
                                    "3-Pf ému/secoué"="3",
                                    "4-Jm ému/secoué"="4",
                                    "8-Refus"="8",
                                    "4-Jm ému/secoué"="9",
                                    
  ) } 
if("ATTENTE" %in% names(ct) & !is.numeric(ct$ATTENTE) ) {
  ct$ATTENTE <- forcats::fct_recode(ctraw$ATTENTE,
                                    
                                    "1-Tj sait attendus"="1",
                                    "2-Sv sait attendus"="2",
                                    "3-Pf sait attendus"="3",
                                    "4-Jm sait attendus"="4",
                                    "5-Sans objet"="5",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("CRITERE" %in% names(ct) & !is.numeric(ct$CRITERE) ) {
  ct$CRITERE <- forcats::fct_recode(ctraw$CRITERE,
                                    
                                    "1-Critères d'évaluation pertinents"="1",
                                    "2-Critères d'évaluation non-pertinents"="2",
                                    "3-Non concerné"="3",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 

if("CONTRAD" %in% names(ct) & !is.numeric(ct$CONTRAD) ) {
  ct$CONTRAD <- forcats::fct_recode(ctraw$CONTRAD,
                                    
                                    "1-Ordres contradictoires"="1",
                                    "2-Pas ordres contrad"="2",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("EVA" %in% names(ct) & !is.numeric(ct$EVA) ) {
  ct$EVA <- forcats::fct_recode(ctraw$EVA,
                                
                                "1-Entretien d'évaluation"="1",
                                "2-Pas entretien éval"="2",
                                "NULL"="8",
                                "2-Pas entretien éval"="9",
                                
  ) } 
if("EVACRIT" %in% names(ct) & !is.numeric(ct$EVACRIT) ) {
  ct$EVACRIT <- forcats::fct_recode(fct_explicit_na(ctraw$EVACRIT, na_level = "2-Pas entretien éval"),
                                    "1-Entretien d'évaluation"="1",
                                    "2-Pas entretien éval"="2",
                                    "NULL"="8",
                                    "2-Pas entretien éval"="9"     )
}
if("SYNDIC" %in% names(ct) & !is.numeric(ct$SYNDIC) ) {
  ct$SYNDIC <- forcats::fct_recode(ctraw$SYNDIC,
                                   
                                   "1-Adhérent syndicat"="1",
                                   "2-Sympath syndicat"="2",
                                   "3-Pas syndicat"="3",
                                   "NULL"="4",
                                   "3-Pas syndicat"="9",
                                   
  ) } 
if("DISCURP" %in% names(ct) & !is.numeric(ct$DISCURP) ) {
  ct$DISCURP <- forcats::fct_recode(ctraw$DISCURP,
                                    
                                    "1-Discussion délégué"="1",
                                    "2-Pas disc délégué"="2",
                                    "3-Pas de représentant du personnel"="3",
                                    "NULL"="8",
                                    "2-Pas disc délégué"="9",
                                    
  ) } 
if("SYNDICNS" %in% names(ct) & !is.numeric(ct$SYNDICNS) ) {
  ct$SYNDICNS <- forcats::fct_recode(ctraw$SYNDICNS,
                                     
                                     "1-Adhérent orga pro"="1",
                                     "2-Sympathisant orga pro"="2",
                                     "3-Pas orga pro"="3",
                                     "NULL"="8",
                                     "3-Pas orga pro"="9",
                                     
  ) } 
if("CORRTAN" %in% names(ct) & !is.numeric(ct$CORRTAN) ) {
  ct$CORRTAN <- forcats::fct_recode(ctraw$CORRTAN,
                                    
                                    "1-SUFF temps"="1",
                                    "2-Pas SUFF tps"="2",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("CORRINF" %in% names(ct) & !is.numeric(ct$CORRINF) ) {
  ct$CORRINF <- forcats::fct_recode(ctraw$CORRINF,
                                    
                                    "1-SUFF infos"="1",
                                    "2-Pas SUFF infos"="2",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("CORRCOP" %in% names(ct) & !is.numeric(ct$CORRCOP) ) {
  ct$CORRCOP <- forcats::fct_recode(ctraw$CORRCOP,
                                    
                                    "1-Coopération suffisante"="1",
                                    "2-Coopération insuffisante" ="2",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 


if("CORRCOL" %in% names(ct) & !is.numeric(ct$CORRCOL) ) {
  ct$CORRCOL <- forcats::fct_recode(ctraw$CORRCOL,
                                    
                                    "1-SUFF collègues"="1",
                                    "2-Pas SUFF coll"="2",
                                    "3-Sans objet"="3",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 

if("CORRLOG" %in% names(ct) & !is.numeric(ct$CORRLOG) ) {
  ct$CORRLOG <- forcats::fct_recode(fct_explicit_na(ctraw$CORRLOG, "3-Sans objet"),
                                    "1-SUFF logiciels"="1",
                                    "2-Pas SUFF logi"="2",
                                    "3-Sans objet"="3",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 

if("CORRMAT" %in% names(ct) & !is.numeric(ct$CORRMAT) ) {
  ct$CORRMAT <- forcats::fct_recode(ctraw$CORRMAT,
                                    
                                    "1-SUFF matériel"="1",
                                    "2-Pas SUFF mat"="2",
                                    "3-Sans objet"="3",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("CORRFORM" %in% names(ct) & !is.numeric(ct$CORRFORM) ) {
  ct$CORRFORM <- forcats::fct_recode(ctraw$CORRFORM,
                                     
                                     "1-SUFF formation"="1",
                                     "2-Pas SUFF form"="2",
                                     "3-Sans objet"="3",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("CRAINTE" %in% names(ct) & !is.numeric(ct$CRAINTE) ) {
  ct$CRAINTE <- forcats::fct_recode(ctraw$CRAINTE,
                                    
                                    "1-Crainte emploi"="1",
                                    "2-Pas crainte empl"="2",
                                    "NULL"="8",
                                    "2-Pas crainte empl"="9",
                                    
  ) } 
if("METIER" %in% names(ct) & !is.numeric(ct$METIER) ) {
  ct$METIER <- forcats::fct_recode(ctraw$METIER,
                                   
                                   "1-Pas changer de métier"="1",
                                   "2-Changer de métier"="2",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("NOCHOM" %in% names(ct) & !is.numeric(ct$NOCHOM) ) {
  ct$NOCHOM <- forcats::fct_recode(ctraw$NOCHOM,
                                   
                                   "1-Retrouver emploi"="1",
                                   "2-Chômage difficile"="2",
                                   "NULL"="8",
                                   "NULL"="9",
                                   
  ) } 
if("TENIR" %in% names(ct) & !is.numeric(ct$TENIR) ) {
  ct$TENIR <- forcats::fct_recode(ctraw$TENIR,
                                  
                                  "1-Tenir jusqu'à retraite"="1",
                                  "2-Pas tenir"="2",
                                  "NULL"="8",
                                  "NULL"="9",
                                  
  ) } 
if("SOUHAIT" %in% names(ct) & !is.numeric(ct$SOUHAIT) ) {
  ct$SOUHAIT <- forcats::fct_recode(ctraw$SOUHAIT,
                                    
                                    "1-Même travail retraite"="1",
                                    "2-Pas même travail"="2",
                                    "NULL"="8",
                                    "2-Pas même travail"="9",
                                    
  ) } 
if("MUTE" %in% names(ct) & !is.numeric(ct$MUTE) ) {
  ct$MUTE <- forcats::fct_recode(ctraw$MUTE,
                                 
                                 "1-Crainte mutation forcée"="1",
                                 "2-Pas crainte mutation"="2",
                                 "NULL"="8",
                                 "2-Pas crainte mutation"="9",
                                 
  ) } 
if("FORTMOD1" %in% names(ct) & !is.numeric(ct$FORTMOD1) ) {
  ct$FORTMOD1 <- forcats::fct_recode(fct_explicit_na(ctraw$FORTMOD1, "2-Pas chg poste"),
                                     
                                     "1-Chg poste"="1",
                                     "2-Pas chg poste"="2",
                                     "NULL"="8",
                                     "2-Pas chg poste"="9",
                                     
  ) } 
if("FORTMOD2" %in% names(ct) & !is.numeric(ct$FORTMOD2) ) {
  ct$FORTMOD2 <- forcats::fct_recode(fct_explicit_na(ctraw$FORTMOD2, "2-Pas chg tech"),
                                     
                                     "1-Chg tech"="1",
                                     "2-Pas chg tech"="2",
                                     "NULL"="8",
                                     "2-Pas chg tech"="9",
                                     
  ) } 
if("FORTMOD3" %in% names(ct) & !is.numeric(ct$FORTMOD3) ) {
  ct$FORTMOD3 <- forcats::fct_recode(fct_explicit_na(ctraw$FORTMOD3, "2-Pas restruct"),
                                     
                                     "1-Restructuration"="1",
                                     "2-Pas restruct"="2",
                                     "NULL"="8",
                                     "2-Pas restruct"="9",
                                     
  ) } 
if("FORTMOD4" %in% names(ct) & !is.numeric(ct$FORTMOD4) ) {
  ct$FORTMOD4 <- forcats::fct_recode(fct_explicit_na(ctraw$FORTMOD4, "2-Pas chg orga"),
                                     
                                     "1-Chg orga travail"="1",
                                     "2-Pas chg orga"="2",
                                     "NULL"="8",
                                     "2-Pas chg orga"="9",
                                     
  ) } 
if("FORTMOD5" %in% names(ct) & !is.numeric(ct$FORTMOD5) ) {
  ct$FORTMOD5 <- forcats::fct_recode(fct_explicit_na(ctraw$FORTMOD5, "2-Pas chg plan social"),
                                     
                                     "1-Chg plan social"="1",
                                     "2-Pas chg plan social"="2",
                                     "NULL"="8",
                                     "2-Pas chg plan social"="9",
                                     
  ) } 
if("FORTMOD6" %in% names(ct) & !is.numeric(ct$FORTMOD6) ) {
  ct$FORTMOD6 <- forcats::fct_recode(fct_explicit_na(ctraw$FORTMOD6, "2-Pas chg dir"),
                                     
                                     "1-Chg direction"="1",
                                     "2-Pas chg dir"="2",
                                     "NULL"="8",
                                     "2-Pas chg dir"="9",
                                     
  ) } 
if("FORTMOD7" %in% names(ct) & !is.numeric(ct$FORTMOD7) ) {
  ct$FORTMOD7 <- forcats::fct_recode(fct_explicit_na(ctraw$FORTMOD7, "2-Pas chg autres"),
                                     
                                     "1-Chg autres"="1",
                                     "2-Pas chg autres"="2",
                                     "NULL"="8",
                                     "2-Pas chg autres"="9",
                                     
  ) } 

if("CHANGOP" %in% names(ct) & !is.numeric(ct$CHANGOP) ) {
  ct$CHANGOP <- forcats::fct_recode(ctraw$CHANGOP,
                                    
                                    "1-ENVMOD positif"="1",
                                    "2-ENVMOD négatif"="2",
                                    "3-ENVMOD se compensent"="3",
                                    "NULL"="8",
                                    "3-ENVMOD se compensent"="9",
                                    
  ) } 
if("CHGTINFO" %in% names(ct) & !is.numeric(ct$CHGTINFO) ) {
  ct$CHGTINFO <- forcats::fct_recode(ctraw$CHGTINFO,
                                     
                                     "1-ENVMOD infos"="1",
                                     "2-ENVMOD pas infos"="2",
                                     "NULL"="8",
                                     "2-ENVMOD pas infos"="9",
                                     
  ) } 
if("CHGTCONS" %in% names(ct) & !is.numeric(ct$CHGTCONS) ) {
  ct$CHGTCONS <- forcats::fct_recode(ctraw$CHGTCONS,
                                     
                                     
                                     "1-ENVMOD consulté"="1",
                                     "2-ENVMOD pas consulté"="2",
                                     "NULL"="8",
                                     "2-ENVMOD pas consulté"="9",
                                     
  ) } 
if("CHGTINFL" %in% names(ct) & !is.numeric(ct$CHGTINFL) ) {
  ct$CHGTINFL <- forcats::fct_recode(ctraw$CHGTINFL,
                                     
                                     "1-ENVMOD influence"="1",
                                     "2-ENVMOD pas influ"="2",
                                     "NULL"="8",
                                     "2-ENVMOD pas influ"="9",
                                     
  ) } 
if("BSANTE" %in% names(ct) & !is.numeric(ct$BSANTE) ) {
  ct$BSANTE <- forcats::fct_recode(ctraw$BSANTE,
                                   
                                   "1-Très bonne santé"="1",
                                   "2-Bonne santé"="2",
                                   "3-Assez bonne santé"="3",
                                   "4-Mauvaise santé"="4",
                                   "5-Très mauvaise santé"="5",
                                   "NULL"="8",
                                   "3-Assez bonne santé"="9",
                                   
  ) } 
if("BCHRO" %in% names(ct) & !is.numeric(ct$BCHRO) ) {
  ct$BCHRO <- forcats::fct_recode(ctraw$BCHRO,
                                  
                                  "1-Maladie chronique"="1",
                                  "2-Pas maladie chro"="2",
                                  "NULL"="8",
                                  "2-Pas maladie chro"="9",
                                  
  ) } 
if("BLIMI" %in% names(ct) & !is.numeric(ct$BLIMI) ) {
  ct$BLIMI <- forcats::fct_recode(ctraw$BLIMI,
                                  
                                  "1-Fortement limité par santé"="1",
                                  "2-Limité par santé"="2",
                                  "3-Pas limité par santé"="3",
                                  "NULL"="8",
                                  "3-Pas limité par santé"="9",
                                  
  ) } 
if("BRECO" %in% names(ct) & !is.numeric(ct$BRECO) ) {
  ct$BRECO <- forcats::fct_recode(ctraw$BRECO,
                                  
                                  "1-Handicap"="1",
                                  "2-En cours"="2",
                                  "3-Pas handicap"="3",
                                  "NULL"="8",
                                  "3-Pas handicap"="9",
                                  
  ) } 
if("SDOUL" %in% names(ct) & !is.numeric(ct$SDOUL) ) {
  ct$SDOUL <- forcats::fct_recode(ctraw$SDOUL,
                                  
                                  "1-Douleurs"="1",
                                  "2-Pas douleurs"="2",
                                  "NULL"="8",
                                  "2-Pas douleurs"="9",
                                  
  ) } 
if("SOMTBL" %in% names(ct) & !is.numeric(ct$SOMTBL) ) {
  ct$SOMTBL <- forcats::fct_recode(ctraw$SOMTBL,
                                   
                                   "1-Insomnies rarement"="1",
                                   "2-Insomnies mois"="2",
                                   "3-Insomnies semaine"="3",
                                   "4-Insomnies tous les jours"="4",
                                   "NULL"="8",
                                   "1-Insomnies rarement"="9",
                                   
  ) } 
if("DORMED" %in% names(ct) & !is.numeric(ct$DORMED) ) {
  ct$DORMED <- forcats::fct_recode(ctraw$DORMED,
                                   
                                   "1-Somnifères tous les jours"="1",
                                   "2-Somnifères semaine"="2",
                                   "3-Somnifères mois"="3",
                                   "4-Somnifères rarement"="4",
                                   "NULL"="8",
                                   "4-Somnifères rarement"="9",
                                   
  ) } 
if("MEDEC" %in% names(ct) & !is.numeric(ct$MEDEC) ) {
  ct$MEDEC <- forcats::fct_recode(ctraw$MEDEC,
                                  
                                  "1-Vu médecin"="1",
                                  "2-Pas vu médecin"="2",
                                  "NULL"="8",
                                  "NULL"="9",
                                  
  ) } 
if("EDM" %in% names(ct) ) {
  ct$EDM <- forcats::fct_recode(ctraw$EDM,
                                "1-EDM"     = "1",
                                "2-Pas EDM" = "0",
                                
  )|> fct_relabel(sort) 
  } 
if("TAG" %in% names(ct) ) {
  ct$TAG <- forcats::fct_recode(as.factor(ctraw$TAG),
                                "1-TAG"     = "1",
                                "2-Pas TAG" = "0",
                                
  )|> fct_relabel(sort) 
} 


if("NATIO1N1" %in% names(ct) & !is.numeric(ct$NATIO1N1) ) {
  ct$NATIO1N1 <- forcats::fct_recode(ctraw$NATIO1N1,
                                     
                                     "1-Français de naissance"="1",
                                     "2-Français par naturalisation"="2",
                                     "3-Étranger"="3",
                                     "4-Apatride"="4",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("NATIO1N2" %in% names(ct) & !is.numeric(ct$NATIO1N2) ) {
  ct$NATIO1N2 <- forcats::fct_recode(ctraw$NATIO1N2,
                                     
                                     "1-Français de naissance"="1",
                                     "2-Français par naturalisation"="2",
                                     "3-Étranger"="3",
                                     "4-Apatride"="4",
                                     "NULL"="8",
                                     "NULL"="9",
                                     
  ) } 
if("NATIO" %in% names(ct) & !is.numeric(ct$NATIO) ) {
  ct$NATIO <- forcats::fct_recode(ctraw$NATIO,
                                  
                                  "1-Français de naissance"="1",
                                  "2-Français par naturalisation"="2",
                                  "3-Etranger"="3",
                                  "4-Apatride"="4",
                                  "5-Français de naissance et étranger"="5",
                                  "6-Français par acquisition et étranger"="6",
                                  
  ) } 
if("IMMI" %in% names(ct) & !is.numeric(ct$IMMI) ) {
  ct$IMMI <- forcats::fct_recode(ctraw$IMMI,
                                 
                                 "0-Non immigré"="0",
                                 "1-Immigré"="1",
                                 
  ) } 
if("DIPLOME" %in% names(ct) & !is.numeric(ct$DIPLOME) ) {
  ct$DIPLOME <- forcats::fct_recode(ctraw$DIPLOME,
                                    
                                    "0-Aucun diplôme"="0",
                                    "1-Certificat d'études"="1",
                                    "2-Brevet"="2",
                                    "3-CAP BEP"="3",
                                    "4-Bac pro"="4",
                                    "5-Bac général"="5",
                                    "6-Bac+2"="6",
                                    "7-Bac+4"="7",
                                    "8-Bac+5"="8",
                                    "NULL"="98",
                                    "NULL"="99",
                                    
  ) } 
if("TCHOLP" %in% names(ct) & !is.numeric(ct$TCHOLP) ) {
  ct$TCHOLP <- forcats::fct_recode(ctraw$TCHOLP,
                                   
                                   "1-1 fois chômage>1an"="1",
                                   "2-x fois chômage>1an"="2",
                                   "3-Jm chômage>1an"="3",
                                   "NULL"="8",
                                   "3-Jm chômage>1an"="9",
                                   
  ) } 
if("TCHOCP" %in% names(ct) & !is.numeric(ct$TCHOCP) ) {
  ct$TCHOCP <- forcats::fct_recode(ctraw$TCHOCP,
                                   
                                   "1-1 fois chômage>3m"="1",
                                   "2-x fois chômage>3m"="2",
                                   "3-Jm  chômage>3m"="3",
                                   "NULL"="8",
                                   "3-Jm  chômage>3m"="9",
                                   
  ) } 
if("TSANP" %in% names(ct) & !is.numeric(ct$TSANP) ) {
  ct$TSANP <- forcats::fct_recode(ctraw$TSANP,
                                  
                                  "1-1 fois arrêt>1an santé"="1",
                                  "2-x fois arrêt>1an santé"="2",
                                  "3-Jm arrêt>1an santé"="3",
                                  "NULL"="8",
                                  "3-Jm arrêt>1an santé"="9",
                                  
  ) } 
if("TINAP" %in% names(ct) & !is.numeric(ct$TINAP) ) {
  ct$TINAP <- forcats::fct_recode(ctraw$TINAP,
                                  
                                  "1-1 fois arrêt>1an autres"="1",
                                  "2-x fois arrêt>1an autres"="2",
                                  "3-Jm arrêt>1an autres"="3",
                                  "NULL"="8",
                                  "3-Jm arrêt>1an autres"="9",
                                  
  ) } 
if("TINDEPP" %in% names(ct) & !is.numeric(ct$TINDEPP) ) {
  ct$TINDEPP <- forcats::fct_recode(ctraw$TINDEPP,
                                    
                                    "1-1 fois indep"="1",
                                    "2-x fois indep"="2",
                                    "3-Jm indep"="3",
                                    "NULL"="8",
                                    "3-Jm indep"="9",
                                    
  ) } 
if("TINTERIMP" %in% names(ct) & !is.numeric(ct$TINTERIMP) ) {
  ct$TINTERIMP <- forcats::fct_recode(ctraw$TINTERIMP,
                                      
                                      "1-1 fois interim"="1",
                                      "2-x fois interim"="2",
                                      "3-Jm interim"="3",
                                      "NULL"="8",
                                      "3-Jm interim"="9",
                                      
  ) } 
if("TCDDP" %in% names(ct) & !is.numeric(ct$TCDDP) ) {
  ct$TCDDP <- forcats::fct_recode(ctraw$TCDDP,
                                  
                                  "1-1 fois CDD"="1",
                                  "2-x fois CDD"="2",
                                  "3-Jm CDD"="3",
                                  "NULL"="8",
                                  "3-Jm CDD"="9",
                                  
  ) } 
# if("NBEMPP" %in% names(ct) & !is.numeric(ct$NBEMPP) ) {
#   ct$NBEMPP <- forcats::fct_recode(ctraw$NBEMPP,
#                                    
#                                    "0-0 employeur"="0",
#                                    "1-1 employeur"="1",
#                                    "10-10+ employeurs"="10",
#                                    "2-2 employeurs"="2",
#                                    "3-3 employeurs"="3",
#                                    "4-4 employeurs"="4",
#                                    "5-5 employeurs"="5",
#                                    "6-6 employeurs"="6",
#                                    "7-7 employeurs"="7",
#                                    "8-8 employeurs"="8",
#                                    "9-9 employeurs"="9",
#                                    
#   ) } 
if("ACCNIR" %in% names(ct) & !is.numeric(ct$ACCNIR) ) {
  ct$ACCNIR <- forcats::fct_recode(ctraw$ACCNIR,
                                   
                                   "1-A donné NIR"="1",
                                   "2-Ne souhaite pas donner NIR"="2",
                                   "3-Ne connait pas NIR"="3",
                                   
  ) } 
if("REPQAA" %in% names(ct) & !is.numeric(ct$REPQAA) ) {
  ct$REPQAA <- forcats::fct_recode(ctraw$REPQAA,
                                   
                                   "1-QAA accepté"="1",
                                   "2-QAA refusé"="2",
                                   "3-QAA impossible"="3",
                                   "4-QAA abandonné"="4",
                                   
  ) } 
if("PE" %in% names(ct) & !is.numeric(ct$PE) ) {
  ct$PE <- forcats::fct_recode(ctraw$PE,
                               
                               "111A-Agriculteurs sur petite exploitation de céréales-grandes cultures"="111A",
                               "111B-Maraîchers, horticulteurs sur petite exploitation"="111B",
                               "111C-Viticulteurs, arboriculteurs fruitiers, sur petite exploitation"="111C",
                               "111D-Éleveurs d'herbivores, sur petite exploitation"="111D",
                               "111E-Éleveurs de granivores et éleveurs mixtes, sur petite exploitation"="111E",
                               "111F-Agriculteurs sur petite exploitation sans orientation dominante"="111F",
                               "121A-Agriculteurs sur moyenne exploitation de céréales-grandes cultures"="121A",
                               "121B-Maraîchers, horticulteurs sur moyenne exploitation"="121B",
                               "121C-Viticulteurs, arboriculteurs fruitiers, sur moyenne exploitation"="121C",
                               "121D-Éleveurs d'herbivores sur moyenne exploitation"="121D",
                               "121E-Éleveurs de granivores et éleveurs mixtes, sur moyenne exploitation"="121E",
                               "121F-Agriculteurs sur moyenne exploitation sans orientation dominante"="121F",
                               "122A-Entrepreneurs de travaux agricoles à façon, de 0 à 9 salariés"="122A",
                               "122B-Exploitants forestiers indépendants, de 0 à 9 salariés"="122B",
                               "122C-Patrons pêcheurs et aquaculteurs, de 0 à 9 salariés"="122C",
                               "131A-Agriculteurs sur grande exploitation de céréales-grandes cultures"="131A",
                               "131B-Maraîchers, horticulteurs, sur grande exploitation"="131B",
                               "131C-Viticulteurs, arboriculteurs fruitiers, sur grande exploitation"="131C",
                               "131D-Éleveurs d'herbivores, sur grande exploitation"="131D",
                               "131E-Éleveurs de granivores et éleveurs mixtes, sur grande exploitation"="131E",
                               "131F-Agriculteurs sur grande exploitation sans orientation dominante"="131F",
                               "211A-Artisans maçons"="211A",
                               "211B-Artisans menuisiers du bâtiment, charpentiers en bois"="211B",
                               "211C-Artisans couvreurs"="211C",
                               "211D-Artisans plombiers, chauffagistes"="211D",
                               "211E-Artisans électriciens du bâtiment"="211E",
                               "211F-Artisans de la peinture et des finitions du bâtiment"="211F",
                               "211G-Artisans serruriers, métalliers"="211G",
                               "211H-Artisans en terrassement, travaux publics"="211H",
                               "211J-Entrepreneurs en parcs et jardins, paysagistes"="211J",
                               "212A-Artisans mécaniciens en machines agricoles"="212A",
                               "212B-Artisans chaudronniers"="212B",
                               "212C-Artisans en mécanique générale, fabrication et travail des métaux (hors horlogerie et matériel de précision)"="212C",
                               "212D-Artisans divers de fabrication de machines"="212D",
                               "213A-Artisans de l'habillement, du textile et du cuir"="213A",
                               "214A-Artisans de l'ameublement"="214A",
                               "214B-Artisans du travail mécanique du bois"="214B",
                               "214C-Artisans du papier, de l'imprimerie et de la reproduction"="214C",
                               "214D-Artisans de fabrication en matériaux de construction (hors artisanat d'art)"="214D",
                               "214E-Artisans d'art"="214E",
                               "214F-Autres artisans de fabrication (y.c. horlogers, matériel de précision)"="214F",
                               "215A-Artisans boulangers, pâtissiers, de 0 à 9 salariés"="215A",
                               "215B-Artisans bouchers, de 0 à 9 salariés"="215B",
                               "215C-Artisans charcutiers, de 0 à 9 salariés"="215C",
                               "215D-Autres artisans de l'alimentation, de 0 à 9 salariés"="215D",
                               "216A-Artisans mécaniciens réparateurs d'automobiles"="216A",
                               "216B-Artisans tôliers-carrossiers d'automobiles"="216B",
                               "216C-Artisans réparateurs divers"="216C",
                               "217A-Conducteurs de taxis, ambulanciers et autres artisans du transport, de 0 à 9 salariés"="217A",
                               "217B-Artisans déménageurs, de 0 à 9 salariés"="217B",
                               "217C-Artisans coiffeurs, manucures, esthéticiens, de 0 à 9 salariés"="217C",
                               "217D-Artisans teinturiers, blanchisseurs, de 0 à 9 salariés"="217D",
                               "217E-Artisans des services divers, de 0 à 9 salariés"="217E",
                               "218A-Transporteurs indépendants routiers et fluviaux, de 0 à 9 salariés"="218A",
                               "219A-Aides familiaux non salariés ou associés d'artisans, effectuant un travail administratif ou commercial"="219A",
                               "221A-Petits et moyens grossistes en alimentation, de 0 à 9 salariés"="221A",
                               "221B-Petits et moyens grossistes en produits non alimentaires, de 0 à 9 salariés"="221B",
                               "222A-Petits et moyens détaillants en alimentation spécialisée, de 0 à 9 salariés"="222A",
                               "222B-Petits et moyens détaillants en alimentation générale, de 0 à 9 salariés"="222B",
                               "223A-Détaillants en ameublement, décor, équipement du foyer, de 0 à 9 salariés"="223A",
                               "223B-Détaillants en droguerie, bazar, quincaillerie, bricolage, de 0 à 9 salariés"="223B",
                               "223C-Fleuristes, de 0 à 9 salariés"="223C",
                               "223D-Détaillants en habillement et articles de sport, de 0 à 9 salariés"="223D",
                               "223E-Détaillants en produits de beauté, de luxe (hors biens culturels), de 0 à 9 salariés"="223E",
                               "223F-Détaillants en biens culturels (livres, disques, multimédia, objets d'art), de 0 à 9 salariés"="223F",
                               "223G-Détaillants en tabac, presse et articles divers, de 0 à 9 salariés"="223G",
                               "223H-Exploitants et gérants libres de station-service, de 0 à 9 salariés"="223H",
                               "224A-Exploitants de petit restaurant, café-restaurant, de 0 à 2 salariés"="224A",
                               "224B-Exploitants de petit café, débit de boisson, associé ou non à une autre activité hors restauration, de 0 à 2 salariés"="224B",
                               "224C-Exploitants de petit hôtel, hôtel-restaurant, de 0 à 2 salariés"="224C",
                               "224D-Exploitants de café, restaurant, hôtel, de 3 à 9 salariés"="224D",
                               "225A-Intermédiaires indépendants du commerce, de 0 à 9 salariés"="225A",
                               "226A-Agents généraux et courtiers d'assurance indépendants, de 0 à 9 salariés"="226A",
                               "226B-Agents de voyage et auxiliaires de transports indépendants, de 0 à 9 salariés"="226B",
                               "226C-Agents immobiliers indépendants, de 0 à 9 salariés"="226C",
                               "227A-Indépendants gestionnaires de spectacle ou de service récréatif, de 0 à 9 salariés"="227A",
                               "227B-Indépendants gestionnaires d'établissements privés (enseignement, santé, social), de 0 à 9 salariés"="227B",
                               "227C-Astrologues, professionnels de la parapsychologie, guérisseurs, de 0 à 9 salariés"="227C",
                               "227D-Autres indépendants divers prestataires de services, de 0 à 9 salariés"="227D",
                               "231A-Chefs de grande entreprise de 500 salariés et plus"="231A",
                               "232A-Chefs de moyenne entreprise, de 50 à 499 salariés"="232A",
                               "233A-Chefs d'entreprise du bâtiment et des travaux publics, de 10 à 49 salariés"="233A",
                               "233B-Chefs d'entreprise de l'industrie ou des transports, de 10 à 49 salariés"="233B",
                               "233C-Chefs d'entreprise commerciale, de 10 à 49 salariés"="233C",
                               "233D-Chefs d'entreprise de services, de 10 à 49 salariés"="233D",
                               "311A-Médecins libéraux spécialistes"="311A",
                               "311B-Médecins libéraux généralistes"="311B",
                               "311C-Chirurgiens dentistes (libéraux ou salariés)"="311C",
                               "311D-Psychologues, psychanalystes, psychothérapeutes (non médecins)"="311D",
                               "311E-Vétérinaires (libéraux ou salariés)"="311E",
                               "311F-Pharmaciens libéraux"="311F",
                               "312A-Avocats"="312A",
                               "312B-Notaires"="312B",
                               "312C-Experts comptables, comptables agréés, libéraux"="312C",
                               "312D-Conseils et experts libéraux en études économiques, organisation et recrutement, gestion et fiscalité"="312D",
                               "312E-Ingénieurs conseils libéraux en études techniques"="312E",
                               "312F-Architectes libéraux"="312F",
                               "312G-Géomètres-experts, huissiers de justice, officiers ministériels, professions libérales diverses"="312G",
                               "313A-Aides familiaux non salariés de professions libérales effectuant un travail administratif"="313A",
                               "331A-Personnels de direction de la fonction publique (État, collectivités locales, hôpitaux)"="331A",
                               "332A-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés"="332A",
                               "332B-Ingénieurs des collectivités locales et des hôpitaux"="332B",
                               "333A-Magistrats"="333A",
                               "333B-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes"="333B",
                               "333C-Cadres de la Poste"="333C",
                               "333D-Cadres administratifs de France Télécom (statut public)"="333D",
                               "333E-Autres personnels administratifs de catégorie A de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="333E",
                               "333F-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)"="333F",
                               "334A-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)"="334A",
                               "335A-Personnes exerçant un mandat politique ou syndical"="335A",
                               "341A-Professeurs agrégés et certifiés de l'enseignement secondaire"="341A",
                               "341B-Chefs d'établissement de l'enseignement secondaire et inspecteurs"="341B",
                               "342A-Enseignants de l'enseignement supérieur"="342A",
                               "342E-Chercheurs de la recherche publique"="342E",
                               "343A-Psychologues spécialistes de l'orientation scolaire et professionnelle"="343A",
                               "344A-Médecins hospitaliers sans activité libérale"="344A",
                               "344B-Médecins salariés non hospitaliers"="344B",
                               "344C-Internes en médecine, odontologie et pharmacie"="344C",
                               "344D-Pharmaciens salariés"="344D",
                               "351A-Bibliothécaires, archivistes, conservateurs et autres cadres du patrimoine (fonction publique)"="351A",
                               "352A-Journalistes (y. c. rédacteurs en chef)"="352A",
                               "352B-Auteurs littéraires, scénaristes, dialoguistes"="352B",
                               "353A-Directeurs de journaux, administrateurs de presse, directeurs d'éditions (littéraire, musicale, audiovisuelle et multimédia)"="353A",
                               "353B-Directeurs, responsables de programmation et de production de l'audiovisuel et des spectacles"="353B",
                               "353C-Cadres artistiques et technico-artistiques de la réalisation de l'audiovisuel et des spectacles"="353C",
                               "354A-Artistes plasticiens"="354A",
                               "354B-Artistes+B493 de la musique et du chant"="354B",
                               "354C-Artistes dramatiques"="354C",
                               "354D-Artistes de la danse, du cirque et des spectacles divers"="354D",
                               "354G-Professeurs d'art (hors établissements scolaires)"="354G",
                               "371A-Cadres d'état-major administratifs, financiers, commerciaux des grandes entreprises"="371A",
                               "372A-Cadres chargés d'études économiques, financières, commerciales"="372A",
                               "372B-Cadres de l'organisation ou du contrôle des services administratifs et financiers"="372B",
                               "372C-Cadres spécialistes des ressources humaines et du recrutement"="372C",
                               "372D-Cadres spécialistes de la formation"="372D",
                               "372E-Juristes"="372E",
                               "372F-Cadres de la documentation, de l'archivage (hors fonction publique)"="372F",
                               "373A-Cadres des services financiers ou comptables des grandes entreprises"="373A",
                               "373B-Cadres des autres services administratifs des grandes entreprises"="373B",
                               "373C-Cadres des services financiers ou comptables des petites et moyennes entreprises"="373C",
                               "373D-Cadres des autres services administratifs des petites et moyennes entreprises"="373D",
                               "374A-Cadres de l'exploitation des magasins de vente du commerce de détail"="374A",
                               "374B-Chefs de produits, acheteurs du commerce et autres cadres de la mercatique"="374B",
                               "374C-Cadres commerciaux des grandes entreprises (hors commerce de détail)"="374C",
                               "374D-Cadres commerciaux des petites et moyennes entreprises (hors commerce de détail)"="374D",
                               "375A-Cadres de la publicité"="375A",
                               "375B-Cadres des relations publiques et de la communication"="375B",
                               "376A-Cadres des marchés financiers"="376A",
                               "376B-Cadres des opérations bancaires"="376B",
                               "376C-Cadres commerciaux de la banque"="376C",
                               "376D-Chefs d'établissements et responsables de l'exploitation bancaire"="376D",
                               "376E-Cadres des services techniques des assurances"="376E",
                               "376F-Cadres des services techniques des organismes de sécurité sociale et assimilés"="376F",
                               "376G-Cadres de l'immobilier"="376G",
                               "377A-Cadres de l'hôtellerie et de la restauration"="377A",
                               "380A-Directeurs techniques des grandes entreprises"="380A",
                               "381A-Ingénieurs et cadres d'étude et d'exploitation de l'agriculture, la pêche, les eaux et forêts"="381A",
                               "382A-Ingénieurs et cadres d'étude du bâtiment et des travaux publics"="382A",
                               "382B-Architectes salariés"="382B",
                               "382C-Ingénieurs, cadres de chantier et conducteurs de travaux (cadres) du bâtiment et des travaux publics"="382C",
                               "382D-Ingénieurs et cadres technico-commerciaux en bâtiment, travaux publics"="382D",
                               "383A-Ingénieurs et cadres d'étude, recherche et développement en électricité, électronique"="383A",
                               "383B-Ingénieurs et cadres de fabrication en matériel électrique, électronique"="383B",
                               "383C-Ingénieurs et cadres technico-commerciaux en matériel électrique ou électronique professionnel"="383C",
                               "384A-Ingénieurs et cadres d'étude, recherche et développement en mécanique et travail des métaux"="384A",
                               "384B-Ingénieurs et cadres de fabrication en mécanique et travail des métaux"="384B",
                               "384C-Ingénieurs et cadres technico-commerciaux en matériel mécanique professionnel"="384C",
                               "385A-Ingénieurs et cadres d'étude, recherche et développement des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385A",
                               "385B-Ingénieurs et cadres de fabrication des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385B",
                               "385C-Ingénieurs et cadres technico-commerciaux des industries de transformations (biens intermédiaires)"="385C",
                               "386A-Ingénieurs et cadres d'étude, recherche et développement des autres industries (imprimerie, matériaux souples, ameublement et bois, énergie, eau)"="386A",
                               "386D-Ingénieurs et cadres de la production et de la distribution d'énergie, eau"="386D",
                               "386E-Ingénieurs et cadres de fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="386E",
                               "387A-Ingénieurs et cadres des achats et approvisionnements industriels"="387A",
                               "387B-Ingénieurs et cadres de la logistique, du planning et de l'ordonnancement"="387B",
                               "387C-Ingénieurs et cadres des méthodes de production"="387C",
                               "387D-Ingénieurs et cadres du contrôle-qualité"="387D",
                               "387E-Ingénieurs et cadres de la maintenance, de l'entretien et des travaux neufs"="387E",
                               "387F-Ingénieurs et cadres techniques de l'environnement"="387F",
                               "388A-Ingénieurs et cadres d'étude, recherche et développement en informatique"="388A",
                               "388B-Ingénieurs et cadres d'administration, maintenance, support et services aux utilisateurs en informatique"="388B",
                               "388C-Chefs de projets informatiques, responsables informatiques"="388C",
                               "388D-Ingénieurs et cadres technico-commerciaux en informatique et télécommunications"="388D",
                               "388E-Ingénieurs et cadres spécialistes des télécommunications"="388E",
                               "389A-Ingénieurs et cadres techniques de l'exploitation des transports"="389A",
                               "389B-Officiers et cadres navigants techniques et commerciaux de l'aviation civile"="389B",
                               "389C-Officiers et cadres navigants techniques de la marine marchande"="389C",
                               "421A-Instituteurs"="421A",
                               "421B-Professeurs des écoles"="421B",
                               "422A-Professeurs d'enseignement général des collèges"="422A",
                               "422B-Professeurs de lycée professionnel"="422B",
                               "422C-Maîtres auxiliaires et professeurs contractuels de l'enseignement secondaire"="422C",
                               "422D-Conseillers principaux d'éducation"="422D",
                               "422E-Surveillants et aides-éducateurs des établissements d'enseignement"="422E",
                               "423A-Moniteurs d'école de conduite"="423A",
                               "423B-Formateurs et animateurs de formation continue"="423B",
                               "424A-Moniteurs et éducateurs sportifs, sportifs professionnels"="424A",
                               "425A-Sous-bibliothécaires, cadres intermédiaires du patrimoine"="425A",
                               "431A-Cadres infirmiers et assimilés"="431A",
                               "431B-Infirmiers psychiatriques"="431B",
                               "431C-Puéricultrices"="431C",
                               "431D-Infirmiers spécialisés (autres qu'infirmiers psychiatriques et puéricultrices)"="431D",
                               "431E-Sages-femmes (libérales ou salariées)"="431E",
                               "431F-Infirmiers en soins généraux, salariés"="431F",
                               "431G-Infirmiers libéraux"="431G",
                               "432A-Masseurs-kinésithérapeutes rééducateurs, libéraux"="432A",
                               "432B-Masseurs-kinésithérapeutes rééducateurs, salariés"="432B",
                               "432C-Autres spécialistes de la rééducation, libéraux"="432C",
                               "432D-Autres spécialistes de la rééducation, salariés"="432D",
                               "433A-Techniciens médicaux"="433A",
                               "433B-Opticiens lunetiers et audioprothésistes (indépendants et salariés)"="433B",
                               "433C-Autres spécialistes de l'appareillage médical (indépendants et salariés)"="433C",
                               "433D-Préparateurs en pharmacie"="433D",
                               "434A-Cadres de l'intervention socio-éducative"="434A",
                               "434B-Assistants de service social"="434B",
                               "434C-Conseillers en économie sociale familiale"="434C",
                               "434D-Éducateurs spécialisés"="434D",
                               "434E-Moniteurs éducateurs"="434E",
                               "434F-Éducateurs techniques spécialisés, moniteurs d'atelier"="434F",
                               "434G-Éducateurs de jeunes enfants"="434G",
                               "435A-Directeurs de centres socioculturels et de loisirs"="435A",
                               "435B-Animateurs socioculturels et de loisirs"="435B",
                               "441A-Clergé séculier"="441A",
                               "441B-Clergé régulier"="441B",
                               "451A-Professions intermédiaires de la Poste"="451A",
                               "451B-Professions intermédiaires administratives de France Télécom (statut public)"="451B",
                               "451C-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés"="451C",
                               "451D-Ingénieurs du contrôle de la navigation aérienne"="451D",
                               "451E-Autres personnels administratifs de catégorie B de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="451E",
                               "451F-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)"="451F",
                               "452A-Inspecteurs et officiers de police"="452A",
                               "452B-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie"="452B",
                               "461A-Personnel de secrétariat de niveau supérieur, secrétaires de direction (non cadres)"="461A",
                               "461D-Maîtrise et techniciens des services financiers ou comptables"="461D",
                               "461E-Maîtrise et techniciens administratifs des services juridiques ou du personnel"="461E",
                               "461F-Maîtrise et techniciens administratifs des autres services administratifs"="461F",
                               "462A-Chefs de petites surfaces de vente (salariés ou mandataires)"="462A",
                               "462B-Maîtrise de l'exploitation des magasins de vente"="462B",
                               "462C-Acheteurs non classés cadres, aides-acheteurs"="462C",
                               "462D-Animateurs commerciaux des magasins de vente, marchandiseurs (non cadres)"="462D",
                               "462E-Autres professions intermédiaires commerciales (sauf techniciens des forces de vente)"="462E",
                               "463A-Techniciens commerciaux et technico-commerciaux, représentants en informatique"="463A",
                               "463B-Techniciens commerciaux et technico-commerciaux, représentants en biens d'équipement, en biens intermédiaires, commerce interindustriel (hors informatique)"="463B",
                               "463C-Techniciens commerciaux et technico-commerciaux, représentants en biens de consommation auprès d'entreprises"="463C",
                               "463D-Techniciens commerciaux et technico-commerciaux, représentants en services auprès d'entreprises ou de professionnels (hors banque, assurance, informatique)"="463D",
                               "463E-Techniciens commerciaux et technico-commerciaux, représentants auprès de particuliers (hors banque, assurance, informatique)"="463E",
                               "464A-Assistants de la publicité, des relations publiques (indépendants ou salariés)"="464A",
                               "464B-Interprètes, traducteurs (indépendants ou salariés)"="464B",
                               "465A-Concepteurs et assistants techniques des arts graphiques, de la mode et de la décoration (indépendants et salariés)"="465A",
                               "465B-Assistants techniques de la réalisation des spectacles vivants et audiovisuels (indépendants ou salariés)"="465B",
                               "465C-Photographes (indépendants et salariés)"="465C",
                               "466A-Responsables commerciaux et administratifs des transports de voyageurs et du tourisme (non cadres)"="466A",
                               "466B-Responsables commerciaux et administratifs des transports de marchandises (non cadres)"="466B",
                               "466C-Responsables d'exploitation des transports de voyageurs et de marchandises (non cadres)"="466C",
                               "467A-Chargés de clientèle bancaire"="467A",
                               "467B-Techniciens des opérations bancaires"="467B",
                               "467C-Professions intermédiaires techniques et commerciales des assurances"="467C",
                               "467D-Professions intermédiaires techniques des organismes de sécurité sociale"="467D",
                               "468A-Maîtrise de restauration : salle et service"="468A",
                               "468B-Maîtrise de l'hébergement : hall et étages"="468B",
                               "471A-Techniciens d'étude et de conseil en agriculture, eaux et forêt"="471A",
                               "471B-Techniciens d'exploitation et de contrôle de la production en agriculture, eaux et forêt"="471B",
                               "472A-Dessinateurs en bâtiment, travaux publics"="472A",
                               "472B-Géomètres, topographes"="472B",
                               "472C-Métreurs et techniciens divers du bâtiment et des travaux publics"="472C",
                               "472D-Techniciens des travaux publics de l'État et des collectivités locales"="472D",
                               "473A-Dessinateurs en électricité, électromécanique et électronique"="473A",
                               "473B-Techniciens de recherche-développement et des méthodes de fabrication en électricité, électromécanique et électronique"="473B",
                               "473C-Techniciens de fabrication et de contrôle-qualité en électricité, électromécanique et électronique"="473C",
                               "474A-Dessinateurs en construction mécanique et travail des métaux"="474A",
                               "474B-Techniciens de recherche-développement et des méthodes de fabrication en construction mécanique et travail des métaux"="474B",
                               "474C-Techniciens de fabrication et de contrôle-qualité en construction mécanique et travail des métaux"="474C",
                               "475A-Techniciens de recherche-développement et des méthodes de production des industries de transformation"="475A",
                               "475B-Techniciens de production et de contrôle-qualité des industries de transformation"="475B",
                               "476A-Assistants techniques, techniciens de l'imprimerie et de l'édition"="476A",
                               "476B-Techniciens de l'industrie des matériaux souples, de l'ameublement et du bois"="476B",
                               "477A-Techniciens de la logistique, du planning et de l'ordonnancement"="477A",
                               "477B-Techniciens d'installation et de maintenance des équipements industriels (électriques, électromécaniques, mécaniques, hors informatique)"="477B",
                               "477C-Techniciens d'installation et de maintenance des équipements non industriels (hors informatique et télécommunications)"="477C",
                               "477D-Techniciens de l'environnement et du traitement des pollutions"="477D",
                               "478A-Techniciens d'étude et de développement en informatique"="478A",
                               "478B-Techniciens de production, d'exploitation en informatique"="478B",
                               "478C-Techniciens d'installation, de maintenance, support et services aux utilisateurs en informatique"="478C",
                               "478D-Techniciens des télécommunications et de l'informatique des réseaux"="478D",
                               "479A-Techniciens des laboratoires de recherche publique ou de l'enseignement"="479A",
                               "479B-Experts salariés ou indépendants de niveau technicien, techniciens divers"="479B",
                               "480A-Contremaîtres et agents d'encadrement (non cadres) en agriculture, sylviculture"="480A",
                               "480B-Maîtres d'équipage de la marine marchande et de la pêche"="480B",
                               "481A-Conducteurs de travaux (non cadres)"="481A",
                               "481B-Chefs de chantier (non cadres)"="481B",
                               "482A-Agents de maîtrise en fabrication de matériel électrique, électronique"="482A",
                               "483A-Agents de maîtrise en construction mécanique, travail des métaux"="483A",
                               "484A-Agents de maîtrise en fabrication : agroalimentaire, chimie, plasturgie, pharmacie."="484A",
                               "484B-Agents de maîtrise en fabrication : métallurgie, matériaux lourds et autres industries de transformation"="484B",
                               "485A-Agents de maîtrise et techniciens en production et distribution d'énergie, eau, chauffage"="485A",
                               "485B-Agents de maîtrise en fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="485B",
                               "486A-Agents de maîtrise en maintenance, installation en électricité, électromécanique et électronique"="486A",
                               "486D-Agents de maîtrise en maintenance, installation en mécanique"="486D",
                               "486E-Agents de maîtrise en entretien général, installation, travaux neufs (hors mécanique, électromécanique, électronique)"="486E",
                               "487A-Responsables d'entrepôt, de magasinage"="487A",
                               "487B-Responsables du tri, de l'emballage, de l'expédition et autres responsables de la manutention"="487B",
                               "488A-Maîtrise de restauration : cuisine/production"="488A",
                               "488B-Maîtrise de restauration : gestion d'établissement"="488B",
                               "521A-Employés de la Poste"="521A",
                               "521B-Employés de France Télécom (statut public)"="521B",
                               "522A-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes"="522A",
                               "523A-Adjoints administratifs de la fonction publique (y.c. enseignement)"="523A",
                               "524A-Agents administratifs de la fonction publique (y.c. enseignement)"="524A",
                               "525A-Agents de service des établissements primaires"="525A",
                               "525B-Agents de service des autres établissements d'enseignement"="525B",
                               "525C-Agents de service de la fonction publique (sauf écoles, hôpitaux)"="525C",
                               "525D-Agents de service hospitaliers (de la fonction publique ou du secteur privé)"="525D",
                               "526A-Aides-soignants (de la fonction publique ou du secteur privé)"="526A",
                               "526B-Assistants dentaires, médicaux et vétérinaires, aides de techniciens médicaux"="526B",
                               "526C-Auxiliaires de puériculture"="526C",
                               "526D-Aides médico-psychologiques"="526D",
                               "526E-Ambulanciers salariés (du secteur public ou du secteur privé)"="526E",
                               "531A-Agents de police de l'État"="531A",
                               "531B-Agents des polices municipales"="531B",
                               "531C-Surveillants de l'administration pénitentiaire"="531C",
                               "532A-Gendarmes (de grade inférieur à adjudant)"="532A",
                               "532B-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)"="532B",
                               "532C-Hommes du rang (sauf pompiers militaires)"="532C",
                               "533A-Pompiers (y.c. pompiers militaires)"="533A",
                               "533B-Agents techniques forestiers, gardes des espaces naturels"="533B",
                               "533C-Agents de surveillance du patrimoine et des administrations"="533C",
                               "534A-Agents civils de sécurité et de surveillance"="534A",
                               "534B-Convoyeurs de fonds, gardes du corps, enquêteurs privés et métiers assimilés (salariés)"="534B",
                               "541A-Agents et hôtesses d'accueil et d'information (hors hôtellerie)"="541A",
                               "541D-Standardistes, téléphonistes"="541D",
                               "542A-Secrétaires"="542A",
                               "542B-Dactylos, sténodactylos (sans secrétariat), opérateurs de traitement de texte"="542B",
                               "543A-Employés des services comptables ou financiers"="543A",
                               "543D-Employés administratifs divers d'entreprises"="543D",
                               "544A-Employés et opérateurs d'exploitation en informatique"="544A",
                               "545A-Employés administratifs des services techniques de la banque"="545A",
                               "545B-Employés des services commerciaux de la banque"="545B",
                               "545C-Employés des services techniques des assurances"="545C",
                               "545D-Employés des services techniques des organismes de sécurité sociale et assimilés"="545D",
                               "546A-Contrôleurs des transports (personnels roulants)"="546A",
                               "546B-Agents des services commerciaux des transports de voyageurs et du tourisme"="546B",
                               "546C-Employés administratifs d'exploitation des transports de marchandises"="546C",
                               "546D-Hôtesses de l'air et stewards"="546D",
                               "546E-Autres agents et hôtesses d'accompagnement (transports, tourisme)"="546E",
                               "551A-Employés de libre service du commerce et magasiniers"="551A",
                               "552A-Caissiers de magasin"="552A",
                               "553A-Vendeurs non spécialisés"="553A",
                               "554A-Vendeurs en alimentation"="554A",
                               "554B-Vendeurs en ameublement, décor, équipement du foyer"="554B",
                               "554C-Vendeurs en droguerie, bazar, quincaillerie, bricolage"="554C",
                               "554D-Vendeurs du commerce de fleurs"="554D",
                               "554E-Vendeurs en habillement et articles de sport"="554E",
                               "554F-Vendeurs en produits de beauté, de luxe (hors biens culturels) et optique"="554F",
                               "554G-Vendeurs de biens culturels (livres, disques, multimédia, objets d'art)"="554G",
                               "554H-Vendeurs de tabac, presse et articles divers"="554H",
                               "554J-Pompistes et gérants de station-service (salariés ou mandataires)"="554J",
                               "555A-Vendeurs par correspondance, télévendeurs"="555A",
                               "556A-Vendeurs en gros de biens d'équipement, biens intermédiaires"="556A",
                               "561A-Serveurs, commis de restaurant, garçons (bar, brasserie, café ou restaurant)"="561A",
                               "561D-Aides de cuisine, apprentis de cuisine et employés polyvalents de la restauration"="561D",
                               "561E-Employés de l'hôtellerie : réception et hall"="561E",
                               "561F-Employés d'étage et employés polyvalents de l'hôtellerie"="561F",
                               "562A-Manucures, esthéticiens (salariés)"="562A",
                               "562B-Coiffeurs salariés"="562B",
                               "563A-Assistantes maternelles, gardiennes d'enfants, familles d'accueil"="563A",
                               "563B-Aides à domicile, aides ménagères, travailleuses familiales"="563B",
                               "563C-Employés de maison et personnels de ménage chez des particuliers"="563C",
                               "564A-Concierges, gardiens d'immeubles"="564A",
                               "564B-Employés des services divers"="564B",
                               "621A-Chefs d'équipe du gros œuvre et des travaux publics"="621A",
                               "621B-Ouvriers qualifiés du travail du béton"="621B",
                               "621C-Conducteurs qualifiés d'engins de chantiers du bâtiment et des travaux publics"="621C",
                               "621D-Ouvriers des travaux publics en installations électriques et de télécommunications"="621D",
                               "621E-Autres ouvriers qualifiés des travaux publics"="621E",
                               "621F-Ouvriers qualifiés des travaux publics (salariés de l'État et des collectivités locales)"="621F",
                               "621G-Mineurs de fond qualifiés et autres ouvriers qualifiés des industries d'extraction (carrières, pétrole, gaz...)"="621G",
                               "622A-Opérateurs qualifiés sur machines automatiques en production électrique ou électronique"="622A",
                               "622B-Câbleurs qualifiés, bobiniers qualifiés"="622B",
                               "622G-Plateformistes, contrôleurs qualifiés de matériel électrique ou électronique"="622G",
                               "623A-Chaudronniers-tôliers industriels, opérateurs qualifiés du travail en forge, conducteurs qualifiés d'équipement de formage, traceurs qualifiés"="623A",
                               "623B-Tuyauteurs industriels qualifiés"="623B",
                               "623C-Soudeurs qualifiés sur métaux"="623C",
                               "623F-Opérateurs qualifiés d'usinage des métaux travaillant à l'unité ou en petite série, moulistes qualifiés"="623F",
                               "623G-Opérateurs qualifiés d'usinage des métaux sur autres machines (sauf moulistes)"="623G",
                               "624A-Monteurs qualifiés d'ensembles mécaniques"="624A",
                               "624D-Monteurs qualifiés en structures métalliques"="624D",
                               "624E-Ouvriers qualifiés de contrôle et d'essais en mécanique"="624E",
                               "624F-Ouvriers qualifiés des traitements thermiques et de surface sur métaux"="624F",
                               "624G-Autres mécaniciens ou ajusteurs qualifiés (ou spécialité non reconnue)"="624G",
                               "625A-Pilotes d'installation lourde des industries de transformation : agroalimentaire, chimie, plasturgie, énergie"="625A",
                               "625B-Ouvriers qualifiés et agents qualifiés de laboratoire : agroalimentaire, chimie, biologie, pharmacie"="625B",
                               "625C-Autres opérateurs et ouvriers qualifiés de la chimie (y.c. pharmacie) et de la plasturgie"="625C",
                               "625D-Opérateurs de la transformation des viandes"="625D",
                               "625E-Autres opérateurs et ouvriers qualifiés de l'industrie agricole et alimentaire (hors transformation des viandes)"="625E",
                               "625H-Ouvriers qualifiés des autres industries (eau, gaz, énergie, chauffage)"="625H",
                               "626A-Pilotes d'installation lourde des industries de transformation : métallurgie, production verrière, matériaux de construction"="626A",
                               "626B-Autres opérateurs et ouvriers qualifiés : métallurgie, production verrière, matériaux de construction"="626B",
                               "626C-Opérateurs et ouvriers qualifiés des industries lourdes du bois et de la fabrication du papier-carton"="626C",
                               "627A-Opérateurs qualifiés du textile et de la mégisserie"="627A",
                               "627B-Ouvriers qualifiés de la coupe des vêtements et de l'habillement, autres opérateurs de confection qualifiés"="627B",
                               "627C-Ouvriers qualifiés du travail industriel du cuir"="627C",
                               "627D-Ouvriers qualifiés de scierie, de la menuiserie industrielle et de l'ameublement"="627D",
                               "627E-Ouvriers de la photogravure et des laboratoires photographiques et cinématographiques"="627E",
                               "627F-Ouvriers de la composition et de l'impression, ouvriers qualifiés de la brochure, de la reliure et du façonnage du papier-carton"="627F",
                               "628A-Mécaniciens qualifiés de maintenance, entretien : équipements industriels"="628A",
                               "628B-Électromécaniciens, électriciens qualifiés d'entretien : équipements industriels"="628B",
                               "628C-Régleurs qualifiés d'équipements de fabrication (travail des métaux, mécanique)"="628C",
                               "628D-Régleurs qualifiés d'équipements de fabrication (hors travail des métaux et mécanique)"="628D",
                               "628E-Ouvriers qualifiés de l'assainissement et du traitement des déchets"="628E",
                               "628F-Agents qualifiés de laboratoire (sauf chimie, santé)"="628F",
                               "628G-Ouvriers qualifiés divers de type industriel"="628G",
                               "631A-Jardiniers"="631A",
                               "632A-Maçons qualifiés"="632A",
                               "632B-Ouvriers qualifiés du travail de la pierre"="632B",
                               "632C-Charpentiers en bois qualifiés"="632C",
                               "632D-Menuisiers qualifiés du bâtiment"="632D",
                               "632E-Couvreurs qualifiés"="632E",
                               "632F-Plombiers et chauffagistes qualifiés"="632F",
                               "632G-Peintres et ouvriers qualifiés de pose de revêtements sur supports verticaux"="632G",
                               "632H-Soliers moquetteurs et ouvriers qualifiés de pose de revêtements souples sur supports horizontaux"="632H",
                               "632J-Monteurs qualifiés en agencement, isolation"="632J",
                               "632K-Ouvriers qualifiés d'entretien général des bâtiments"="632K",
                               "633A-Électriciens qualifiés de type artisanal (y.c. bâtiment)"="633A",
                               "633B-Dépanneurs qualifiés en radiotélévision, électroménager, matériel électronique (salariés)"="633B",
                               "633C-Électriciens, électroniciens qualifiés en maintenance entretien, réparation : automobile"="633C",
                               "633D-Électriciens, électroniciens qualifiés en maintenance, entretien : équipements non industriels"="633D",
                               "634A-Carrossiers d'automobiles qualifiés"="634A",
                               "634B-Métalliers, serruriers qualifiés"="634B",
                               "634C-Mécaniciens qualifiés en maintenance, entretien, réparation : automobile"="634C",
                               "634D-Mécaniciens qualifiés de maintenance, entretien : équipements non industriels"="634D",
                               "635A-Tailleurs et couturières qualifiés, ouvriers qualifiés du travail des étoffes (sauf fabrication de vêtements), ouvriers qualifiés de type artisanal du travail du cuir"="635A",
                               "636A-Bouchers (sauf industrie de la viande)"="636A",
                               "636B-Charcutiers (sauf industrie de la viande)"="636B",
                               "636C-Boulangers, pâtissiers (sauf activité industrielle)"="636C",
                               "636D-Cuisiniers et commis de cuisine"="636D",
                               "637A-Modeleurs (sauf modeleurs de métal), mouleurs-noyauteurs à la main, ouvriers qualifiés du travail du verre ou de la céramique à la main"="637A",
                               "637B-Ouvriers d'art"="637B",
                               "637C-Ouvriers et techniciens des spectacles vivants et audiovisuels"="637C",
                               "637D-Ouvriers qualifiés divers de type artisanal"="637D",
                               "641A-Conducteurs routiers et grands routiers (salariés)"="641A",
                               "641B-Conducteurs de véhicule routier de transport en commun (salariés)"="641B",
                               "642A-Conducteurs de taxi (salariés)"="642A",
                               "642B-Conducteurs de voiture particulière (salariés)"="642B",
                               "643A-Conducteurs livreurs, coursiers (salariés)"="643A",
                               "644A-Conducteurs de véhicule de ramassage des ordures ménagères"="644A",
                               "651A-Conducteurs d'engin lourd de levage"="651A",
                               "651B-Conducteurs d'engin lourd de manœuvre"="651B",
                               "652A-Ouvriers qualifiés de la manutention, conducteurs de chariots élévateurs, caristes"="652A",
                               "652B-Dockers"="652B",
                               "653A-Magasiniers qualifiés"="653A",
                               "654A-Conducteurs qualifiés d'engins de transport guidés"="654A",
                               "655A-Autres agents et ouvriers qualifiés (sédentaires) des services d'exploitation des transports"="655A",
                               "656A-Matelots de la marine marchande, capitaines et matelots timoniers de la navigation fluviale (salariés)"="656A",
                               "671A-Ouvriers non qualifiés des travaux publics de l'État et des collectivités locales"="671A",
                               "671B-Ouvriers non qualifiés des travaux publics, du travail du béton et de l'extraction, hors État et collectivités locales"="671B",
                               "672A-Ouvriers non qualifiés de l'électricité et de l'électronique"="672A",
                               "673A-Ouvriers de production non qualifiés travaillant par enlèvement de métal"="673A",
                               "673B-Ouvriers de production non qualifiés travaillant par formage de métal"="673B",
                               "673C-Ouvriers non qualifiés de montage, contrôle en mécanique et travail des métaux"="673C",
                               "674A-Ouvriers de production non qualifiés : chimie, pharmacie, plasturgie"="674A",
                               "674B-Ouvriers de production non qualifiés de la transformation des viandes"="674B",
                               "674C-Autres ouvriers de production non qualifiés : industrie agro-alimentaire"="674C",
                               "674D-Ouvriers de production non qualifiés : métallurgie, production verrière, céramique, matériaux de construction"="674D",
                               "674E-Ouvriers de production non qualifiés : industrie lourde du bois, fabrication des papiers et cartons"="674E",
                               "675A-Ouvriers de production non qualifiés du textile et de la confection, de la tannerie-mégisserie et du travail du cuir"="675A",
                               "675B-Ouvriers de production non qualifiés du travail du bois et de l'ameublement"="675B",
                               "675C-Ouvriers de production non qualifiés de l'imprimerie, presse, édition"="675C",
                               "676A-Manutentionnaires non qualifiés"="676A",
                               "676B-Déménageurs (hors chauffeurs-déménageurs), non qualifiés"="676B",
                               "676C-Ouvriers du tri, de l'emballage, de l'expédition, non qualifiés"="676C",
                               "676D-Agents non qualifiés des services d'exploitation des transports"="676D",
                               "676E-Ouvriers non qualifiés divers de type industriel"="676E",
                               "681A-Ouvriers non qualifiés du gros œuvre du bâtiment"="681A",
                               "681B-Ouvriers non qualifiés du second œuvre du bâtiment"="681B",
                               "682A-Métalliers, serruriers, réparateurs en mécanique non qualifiés"="682A",
                               "683A-Apprentis boulangers, bouchers, charcutiers"="683A",
                               "684A-Nettoyeurs"="684A",
                               "684B-Ouvriers non qualifiés de l'assainissement et du traitement des déchets"="684B",
                               "685A-Ouvriers non qualifiés divers de type artisanal"="685A",
                               "691A-Conducteurs d'engin agricole ou forestier"="691A",
                               "691B-Ouvriers de l'élevage"="691B",
                               "691C-Ouvriers du maraîchage ou de l'horticulture"="691C",
                               "691D-Ouvriers de la viticulture ou de l'arboriculture fruitière"="691D",
                               "691E-Ouvriers agricoles sans spécialisation particulière"="691E",
                               "691F-Ouvriers de l'exploitation forestière ou de la sylviculture"="691F",
                               "692A-Marins-pêcheurs et ouvriers de l'aquaculture"="692A",
                               "7100-Anciens agriculteurs exploitants"="7100",
                               "7200-Anciens artisans, commerçants, chefs d'entreprise"="7200",
                               "7400-Anciens cadres"="7400",
                               "7500-Anciennes professions intermédiaires"="7500",
                               "7700-Anciens employés"="7700",
                               "7800-Anciens ouvriers"="7800",
                               "8100-Chômeurs n'ayant jamais travaillé"="8100",
                               "8300-Militaires du contingent"="8300",
                               "8400-Élèves, étudiants"="8400",
                               "8500-Personnes diverses sans activité professionnelle de moins de 60 ans (sauf retraites)"="8500",
                               "8600-Personnes diverses sans activité professionnelle de 60 ans et plus (sauf retraites)"="8600",
                               
  ) } 
if("CSE" %in% names(ct) & !is.numeric(ct$CSE) ) {
  ct$CSE <- forcats::fct_recode(ctraw$CSE,
                                
                                "11-Agriculteurs sur petite exploitation"="11",
                                "12-Agriculteurs sur moyenne exploitation"="12",
                                "13-Agriculteurs sur grande exploitation"="13",
                                "21-Artisans"="21",
                                "22-Commerçants et assimilés"="22",
                                "23-Chefs d'entreprise de 10 salariés ou plus"="23",
                                "31-Professions libérales"="31",
                                "33-Cadres de la fonction publique"="33",
                                "34-Professeurs, professions scientifiques"="34",
                                "35-Professions de l'information, des arts et des spectacles"="35",
                                "37-Cadres administratifs et commerciaux d'entreprise"="37",
                                "38-Ingénieurs et cadres techniques d'entreprise"="38",
                                "42-Professeurs des écoles, instituteurs et assimilés"="42",
                                "43-Professions intermédiaires de la santé et du travail social"="43",
                                "44-Clergé, religieux"="44",
                                "45-Professions intermédiaires administratives de la fonction publique"="45",
                                "46-Professions intermédiaires administratives et commerciales des entreprises"="46",
                                "47-Techniciens"="47",
                                "48-Contremaîtres, agents de maîtrise"="48",
                                "52-Employés civils et agents de service de la fonction publique"="52",
                                "53-Policiers et militaires"="53",
                                "54-Employés administratifs d'entreprise"="54",
                                "55-Employés de commerce"="55",
                                "56-Personnels des services directs aux particuliers"="56",
                                "62-Ouvriers qualifiés de type industriel"="62",
                                "63-Ouvriers qualifiés de type artisanal"="63",
                                "64-Chauffeurs"="64",
                                "65-Ouvriers qualifiés de la manutention, du magasinage et du transport"="65",
                                "67-Ouvriers non qualifiés de type industriel"="67",
                                "68-Ouvriers non qualifiés de type artisanal"="68",
                                "69-Ouvriers agricoles"="69",
                                
  ) } 
if("CHAMP_EMPL" %in% names(ct) & !is.numeric(ct$CHAMP_EMPL) ) {
  ct$CHAMP_EMPL <- forcats::fct_recode(ctraw$CHAMP_EMPL,
                                       
                                       "1-Oui"="1",
                                       "2-Non"="2",
                                       
  ) } 
if("CSEPER" %in% names(ct) & !is.numeric(ct$CSEPER) ) {
  ct$CSEPER <- forcats::fct_recode(ctraw$CSEPER,
                                   
                                   "11-Agriculteurs sur petite exploitation"="11",
                                   "12-Agriculteurs sur moyenne exploitation"="12",
                                   "13-Agriculteurs sur grande exploitation"="13",
                                   "21-Artisans"="21",
                                   "22-Commerçants et assimilés"="22",
                                   "23-Chefs d'entreprise de 10 salariés ou plus"="23",
                                   "31-Professions libérales"="31",
                                   "33-Cadres de la fonction publique"="33",
                                   "34-Professeurs, professions scientifiques"="34",
                                   "35-Professions de l'information, des arts et des spectacles"="35",
                                   "37-Cadres administratifs et commerciaux d'entreprise"="37",
                                   "38-Ingénieurs et cadres techniques d'entreprise"="38",
                                   "42-Professeurs des écoles, instituteurs et assimilés"="42",
                                   "43-Professions intermédiaires de la santé et du travail social"="43",
                                   "44-Clergé, religieux"="44",
                                   "45-Professions intermédiaires administratives de la fonction publique"="45",
                                   "46-Professions intermédiaires administratives et commerciales des entreprises"="46",
                                   "47-Techniciens"="47",
                                   "48-Contremaîtres, agents de maîtrise"="48",
                                   "52-Employés civils et agents de service de la fonction publique"="52",
                                   "53-Policiers et militaires"="53",
                                   "54-Employés administratifs d'entreprise"="54",
                                   "55-Employés de commerce"="55",
                                   "56-Personnels des services directs aux particuliers"="56",
                                   "62-Ouvriers qualifiés de type industriel"="62",
                                   "63-Ouvriers qualifiés de type artisanal"="63",
                                   "64-Chauffeurs"="64",
                                   "65-Ouvriers qualifiés de la manutention, du magasinage et du transport"="65",
                                   "67-Ouvriers non qualifiés de type industriel"="67",
                                   "68-Ouvriers non qualifiés de type artisanal"="68",
                                   "69-Ouvriers agricoles"="69",
                                   
  ) } 
if("CSEMER" %in% names(ct) & !is.numeric(ct$CSEMER) ) {
  ct$CSEMER <- forcats::fct_recode(ctraw$CSEMER,
                                   
                                   "11-Agriculteurs sur petite exploitation"="11",
                                   "12-Agriculteurs sur moyenne exploitation"="12",
                                   "13-Agriculteurs sur grande exploitation"="13",
                                   "21-Artisans"="21",
                                   "22-Commerçants et assimilés"="22",
                                   "23-Chefs d'entreprise de 10 salariés ou plus"="23",
                                   "31-Professions libérales"="31",
                                   "33-Cadres de la fonction publique"="33",
                                   "34-Professeurs, professions scientifiques"="34",
                                   "35-Professions de l'information, des arts et des spectacles"="35",
                                   "37-Cadres administratifs et commerciaux d'entreprise"="37",
                                   "38-Ingénieurs et cadres techniques d'entreprise"="38",
                                   "42-Professeurs des écoles, instituteurs et assimilés"="42",
                                   "43-Professions intermédiaires de la santé et du travail social"="43",
                                   "44-Clergé, religieux"="44",
                                   "45-Professions intermédiaires administratives de la fonction publique"="45",
                                   "46-Professions intermédiaires administratives et commerciales des entreprises"="46",
                                   "47-Techniciens"="47",
                                   "48-Contremaîtres, agents de maîtrise"="48",
                                   "52-Employés civils et agents de service de la fonction publique"="52",
                                   "53-Policiers et militaires"="53",
                                   "54-Employés administratifs d'entreprise"="54",
                                   "55-Employés de commerce"="55",
                                   "56-Personnels des services directs aux particuliers"="56",
                                   "62-Ouvriers qualifiés de type industriel"="62",
                                   "63-Ouvriers qualifiés de type artisanal"="63",
                                   "64-Chauffeurs"="64",
                                   "65-Ouvriers qualifiés de la manutention, du magasinage et du transport"="65",
                                   "67-Ouvriers non qualifiés de type industriel"="67",
                                   "68-Ouvriers non qualifiés de type artisanal"="68",
                                   "69-Ouvriers agricoles"="69",
                                   
  ) } 
if("PROFDEB" %in% names(ct) & !is.numeric(ct$PROFDEB) ) {
  ct$PROFDEB <- forcats::fct_recode(ctraw$PROFDEB,
                                    
                                    "111A-Agriculteurs sur petite exploitation de céréales-grandes cultures"="111A",
                                    "111B-Maraîchers, horticulteurs sur petite exploitation"="111B",
                                    "111C-Viticulteurs, arboriculteurs fruitiers, sur petite exploitation"="111C",
                                    "111D-Éleveurs d'herbivores, sur petite exploitation"="111D",
                                    "111E-Éleveurs de granivores et éleveurs mixtes, sur petite exploitation"="111E",
                                    "111F-Agriculteurs sur petite exploitation sans orientation dominante"="111F",
                                    "121A-Agriculteurs sur moyenne exploitation de céréales-grandes cultures"="121A",
                                    "121B-Maraîchers, horticulteurs sur moyenne exploitation"="121B",
                                    "121C-Viticulteurs, arboriculteurs fruitiers, sur moyenne exploitation"="121C",
                                    "121D-Éleveurs d'herbivores sur moyenne exploitation"="121D",
                                    "121E-Éleveurs de granivores et éleveurs mixtes, sur moyenne exploitation"="121E",
                                    "121F-Agriculteurs sur moyenne exploitation sans orientation dominante"="121F",
                                    "122A-Entrepreneurs de travaux agricoles à façon, de 0 à 9 salariés"="122A",
                                    "122B-Exploitants forestiers indépendants, de 0 à 9 salariés"="122B",
                                    "122C-Patrons pêcheurs et aquaculteurs, de 0 à 9 salariés"="122C",
                                    "131A-Agriculteurs sur grande exploitation de céréales-grandes cultures"="131A",
                                    "131B-Maraîchers, horticulteurs, sur grande exploitation"="131B",
                                    "131C-Viticulteurs, arboriculteurs fruitiers, sur grande exploitation"="131C",
                                    "131D-Éleveurs d'herbivores, sur grande exploitation"="131D",
                                    "131E-Éleveurs de granivores et éleveurs mixtes, sur grande exploitation"="131E",
                                    "131F-Agriculteurs sur grande exploitation sans orientation dominante"="131F",
                                    "211A-Artisans maçons"="211A",
                                    "211B-Artisans menuisiers du bâtiment, charpentiers en bois"="211B",
                                    "211C-Artisans couvreurs"="211C",
                                    "211D-Artisans plombiers, chauffagistes"="211D",
                                    "211E-Artisans électriciens du bâtiment"="211E",
                                    "211F-Artisans de la peinture et des finitions du bâtiment"="211F",
                                    "211G-Artisans serruriers, métalliers"="211G",
                                    "211H-Artisans en terrassement, travaux publics"="211H",
                                    "211J-Entrepreneurs en parcs et jardins, paysagistes"="211J",
                                    "212A-Artisans mécaniciens en machines agricoles"="212A",
                                    "212B-Artisans chaudronniers"="212B",
                                    "212C-Artisans en mécanique générale, fabrication et travail des métaux (hors horlogerie et matériel de précision)"="212C",
                                    "212D-Artisans divers de fabrication de machines"="212D",
                                    "213A-Artisans de l'habillement, du textile et du cuir"="213A",
                                    "214A-Artisans de l'ameublement"="214A",
                                    "214B-Artisans du travail mécanique du bois"="214B",
                                    "214C-Artisans du papier, de l'imprimerie et de la reproduction"="214C",
                                    "214D-Artisans de fabrication en matériaux de construction (hors artisanat d'art)"="214D",
                                    "214E-Artisans d'art"="214E",
                                    "214F-Autres artisans de fabrication (y.c. horlogers, matériel de précision)"="214F",
                                    "215A-Artisans boulangers, pâtissiers, de 0 à 9 salariés"="215A",
                                    "215B-Artisans bouchers, de 0 à 9 salariés"="215B",
                                    "215C-Artisans charcutiers, de 0 à 9 salariés"="215C",
                                    "215D-Autres artisans de l'alimentation, de 0 à 9 salariés"="215D",
                                    "216A-Artisans mécaniciens réparateurs d'automobiles"="216A",
                                    "216B-Artisans tôliers-carrossiers d'automobiles"="216B",
                                    "216C-Artisans réparateurs divers"="216C",
                                    "217A-Conducteurs de taxis, ambulanciers et autres artisans du transport, de 0 à 9 salariés"="217A",
                                    "217B-Artisans déménageurs, de 0 à 9 salariés"="217B",
                                    "217C-Artisans coiffeurs, manucures, esthéticiens, de 0 à 9 salariés"="217C",
                                    "217D-Artisans teinturiers, blanchisseurs, de 0 à 9 salariés"="217D",
                                    "217E-Artisans des services divers, de 0 à 9 salariés"="217E",
                                    "218A-Transporteurs indépendants routiers et fluviaux, de 0 à 9 salariés"="218A",
                                    "219A-Aides familiaux non salariés ou associés d'artisans, effectuant un travail administratif ou commercial"="219A",
                                    "221A-Petits et moyens grossistes en alimentation, de 0 à 9 salariés"="221A",
                                    "221B-Petits et moyens grossistes en produits non alimentaires, de 0 à 9 salariés"="221B",
                                    "222A-Petits et moyens détaillants en alimentation spécialisée, de 0 à 9 salariés"="222A",
                                    "222B-Petits et moyens détaillants en alimentation générale, de 0 à 9 salariés"="222B",
                                    "223A-Détaillants en ameublement, décor, équipement du foyer, de 0 à 9 salariés"="223A",
                                    "223B-Détaillants en droguerie, bazar, quincaillerie, bricolage, de 0 à 9 salariés"="223B",
                                    "223C-Fleuristes, de 0 à 9 salariés"="223C",
                                    "223D-Détaillants en habillement et articles de sport, de 0 à 9 salariés"="223D",
                                    "223E-Détaillants en produits de beauté, de luxe (hors biens culturels), de 0 à 9 salariés"="223E",
                                    "223F-Détaillants en biens culturels (livres, disques, multimédia, objets d'art), de 0 à 9 salariés"="223F",
                                    "223G-Détaillants en tabac, presse et articles divers, de 0 à 9 salariés"="223G",
                                    "223H-Exploitants et gérants libres de station-service, de 0 à 9 salariés"="223H",
                                    "224A-Exploitants de petit restaurant, café-restaurant, de 0 à 2 salariés"="224A",
                                    "224B-Exploitants de petit café, débit de boisson, associé ou non à une autre activité hors restauration, de 0 à 2 salariés"="224B",
                                    "224C-Exploitants de petit hôtel, hôtel-restaurant, de 0 à 2 salariés"="224C",
                                    "224D-Exploitants de café, restaurant, hôtel, de 3 à 9 salariés"="224D",
                                    "225A-Intermédiaires indépendants du commerce, de 0 à 9 salariés"="225A",
                                    "226A-Agents généraux et courtiers d'assurance indépendants, de 0 à 9 salariés"="226A",
                                    "226B-Agents de voyage et auxiliaires de transports indépendants, de 0 à 9 salariés"="226B",
                                    "226C-Agents immobiliers indépendants, de 0 à 9 salariés"="226C",
                                    "227A-Indépendants gestionnaires de spectacle ou de service récréatif, de 0 à 9 salariés"="227A",
                                    "227B-Indépendants gestionnaires d'établissements privés (enseignement, santé, social), de 0 à 9 salariés"="227B",
                                    "227C-Astrologues, professionnels de la parapsychologie, guérisseurs, de 0 à 9 salariés"="227C",
                                    "227D-Autres indépendants divers prestataires de services, de 0 à 9 salariés"="227D",
                                    "231A-Chefs de grande entreprise de 500 salariés et plus"="231A",
                                    "232A-Chefs de moyenne entreprise, de 50 à 499 salariés"="232A",
                                    "233A-Chefs d'entreprise du bâtiment et des travaux publics, de 10 à 49 salariés"="233A",
                                    "233B-Chefs d'entreprise de l'industrie ou des transports, de 10 à 49 salariés"="233B",
                                    "233C-Chefs d'entreprise commerciale, de 10 à 49 salariés"="233C",
                                    "233D-Chefs d'entreprise de services, de 10 à 49 salariés"="233D",
                                    "311A-Médecins libéraux spécialistes"="311A",
                                    "311B-Médecins libéraux généralistes"="311B",
                                    "311C-Chirurgiens dentistes (libéraux ou salariés)"="311C",
                                    "311D-Psychologues, psychanalystes, psychothérapeutes (non médecins)"="311D",
                                    "311E-Vétérinaires (libéraux ou salariés)"="311E",
                                    "311F-Pharmaciens libéraux"="311F",
                                    "312A-Avocats"="312A",
                                    "312B-Notaires"="312B",
                                    "312C-Experts comptables, comptables agréés, libéraux"="312C",
                                    "312D-Conseils et experts libéraux en études économiques, organisation et recrutement, gestion et fiscalité"="312D",
                                    "312E-Ingénieurs conseils libéraux en études techniques"="312E",
                                    "312F-Architectes libéraux"="312F",
                                    "312G-Géomètres-experts, huissiers de justice, officiers ministériels, professions libérales diverses"="312G",
                                    "313A-Aides familiaux non salariés de professions libérales effectuant un travail administratif"="313A",
                                    "331A-Personnels de direction de la fonction publique (État, collectivités locales, hôpitaux)"="331A",
                                    "332A-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés"="332A",
                                    "332B-Ingénieurs des collectivités locales et des hôpitaux"="332B",
                                    "333A-Magistrats"="333A",
                                    "333B-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes"="333B",
                                    "333C-Cadres de la Poste"="333C",
                                    "333D-Cadres administratifs de France Télécom (statut public)"="333D",
                                    "333E-Autres personnels administratifs de catégorie A de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="333E",
                                    "333F-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)"="333F",
                                    "334A-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)"="334A",
                                    "335A-Personnes exerçant un mandat politique ou syndical"="335A",
                                    "341A-Professeurs agrégés et certifiés de l'enseignement secondaire"="341A",
                                    "341B-Chefs d'établissement de l'enseignement secondaire et inspecteurs"="341B",
                                    "342A-Enseignants de l'enseignement supérieur"="342A",
                                    "342E-Chercheurs de la recherche publique"="342E",
                                    "343A-Psychologues spécialistes de l'orientation scolaire et professionnelle"="343A",
                                    "344A-Médecins hospitaliers sans activité libérale"="344A",
                                    "344B-Médecins salariés non hospitaliers"="344B",
                                    "344C-Internes en médecine, odontologie et pharmacie"="344C",
                                    "344D-Pharmaciens salariés"="344D",
                                    "351A-Bibliothécaires, archivistes, conservateurs et autres cadres du patrimoine (fonction publique)"="351A",
                                    "352A-Journalistes (y. c. rédacteurs en chef)"="352A",
                                    "352B-Auteurs littéraires, scénaristes, dialoguistes"="352B",
                                    "353A-Directeurs de journaux, administrateurs de presse, directeurs d'éditions (littéraire, musicale, audiovisuelle et multimédia)"="353A",
                                    "353B-Directeurs, responsables de programmation et de production de l'audiovisuel et des spectacles"="353B",
                                    "353C-Cadres artistiques et technico-artistiques de la réalisation de l'audiovisuel et des spectacles"="353C",
                                    "354A-Artistes plasticiens"="354A",
                                    "354B-Artistes+B493 de la musique et du chant"="354B",
                                    "354C-Artistes dramatiques"="354C",
                                    "354D-Artistes de la danse, du cirque et des spectacles divers"="354D",
                                    "354G-Professeurs d'art (hors établissements scolaires)"="354G",
                                    "371A-Cadres d'état-major administratifs, financiers, commerciaux des grandes entreprises"="371A",
                                    "372A-Cadres chargés d'études économiques, financières, commerciales"="372A",
                                    "372B-Cadres de l'organisation ou du contrôle des services administratifs et financiers"="372B",
                                    "372C-Cadres spécialistes des ressources humaines et du recrutement"="372C",
                                    "372D-Cadres spécialistes de la formation"="372D",
                                    "372E-Juristes"="372E",
                                    "372F-Cadres de la documentation, de l'archivage (hors fonction publique)"="372F",
                                    "373A-Cadres des services financiers ou comptables des grandes entreprises"="373A",
                                    "373B-Cadres des autres services administratifs des grandes entreprises"="373B",
                                    "373C-Cadres des services financiers ou comptables des petites et moyennes entreprises"="373C",
                                    "373D-Cadres des autres services administratifs des petites et moyennes entreprises"="373D",
                                    "374A-Cadres de l'exploitation des magasins de vente du commerce de détail"="374A",
                                    "374B-Chefs de produits, acheteurs du commerce et autres cadres de la mercatique"="374B",
                                    "374C-Cadres commerciaux des grandes entreprises (hors commerce de détail)"="374C",
                                    "374D-Cadres commerciaux des petites et moyennes entreprises (hors commerce de détail)"="374D",
                                    "375A-Cadres de la publicité"="375A",
                                    "375B-Cadres des relations publiques et de la communication"="375B",
                                    "376A-Cadres des marchés financiers"="376A",
                                    "376B-Cadres des opérations bancaires"="376B",
                                    "376C-Cadres commerciaux de la banque"="376C",
                                    "376D-Chefs d'établissements et responsables de l'exploitation bancaire"="376D",
                                    "376E-Cadres des services techniques des assurances"="376E",
                                    "376F-Cadres des services techniques des organismes de sécurité sociale et assimilés"="376F",
                                    "376G-Cadres de l'immobilier"="376G",
                                    "377A-Cadres de l'hôtellerie et de la restauration"="377A",
                                    "380A-Directeurs techniques des grandes entreprises"="380A",
                                    "381A-Ingénieurs et cadres d'étude et d'exploitation de l'agriculture, la pêche, les eaux et forêts"="381A",
                                    "382A-Ingénieurs et cadres d'étude du bâtiment et des travaux publics"="382A",
                                    "382B-Architectes salariés"="382B",
                                    "382C-Ingénieurs, cadres de chantier et conducteurs de travaux (cadres) du bâtiment et des travaux publics"="382C",
                                    "382D-Ingénieurs et cadres technico-commerciaux en bâtiment, travaux publics"="382D",
                                    "383A-Ingénieurs et cadres d'étude, recherche et développement en électricité, électronique"="383A",
                                    "383B-Ingénieurs et cadres de fabrication en matériel électrique, électronique"="383B",
                                    "383C-Ingénieurs et cadres technico-commerciaux en matériel électrique ou électronique professionnel"="383C",
                                    "384A-Ingénieurs et cadres d'étude, recherche et développement en mécanique et travail des métaux"="384A",
                                    "384B-Ingénieurs et cadres de fabrication en mécanique et travail des métaux"="384B",
                                    "384C-Ingénieurs et cadres technico-commerciaux en matériel mécanique professionnel"="384C",
                                    "385A-Ingénieurs et cadres d'étude, recherche et développement des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385A",
                                    "385B-Ingénieurs et cadres de fabrication des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385B",
                                    "385C-Ingénieurs et cadres technico-commerciaux des industries de transformations (biens intermédiaires)"="385C",
                                    "386A-Ingénieurs et cadres d'étude, recherche et développement des autres industries (imprimerie, matériaux souples, ameublement et bois, énergie, eau)"="386A",
                                    "386D-Ingénieurs et cadres de la production et de la distribution d'énergie, eau"="386D",
                                    "386E-Ingénieurs et cadres de fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="386E",
                                    "387A-Ingénieurs et cadres des achats et approvisionnements industriels"="387A",
                                    "387B-Ingénieurs et cadres de la logistique, du planning et de l'ordonnancement"="387B",
                                    "387C-Ingénieurs et cadres des méthodes de production"="387C",
                                    "387D-Ingénieurs et cadres du contrôle-qualité"="387D",
                                    "387E-Ingénieurs et cadres de la maintenance, de l'entretien et des travaux neufs"="387E",
                                    "387F-Ingénieurs et cadres techniques de l'environnement"="387F",
                                    "388A-Ingénieurs et cadres d'étude, recherche et développement en informatique"="388A",
                                    "388B-Ingénieurs et cadres d'administration, maintenance, support et services aux utilisateurs en informatique"="388B",
                                    "388C-Chefs de projets informatiques, responsables informatiques"="388C",
                                    "388D-Ingénieurs et cadres technico-commerciaux en informatique et télécommunications"="388D",
                                    "388E-Ingénieurs et cadres spécialistes des télécommunications"="388E",
                                    "389A-Ingénieurs et cadres techniques de l'exploitation des transports"="389A",
                                    "389B-Officiers et cadres navigants techniques et commerciaux de l'aviation civile"="389B",
                                    "389C-Officiers et cadres navigants techniques de la marine marchande"="389C",
                                    "421A-Instituteurs"="421A",
                                    "421B-Professeurs des écoles"="421B",
                                    "422A-Professeurs d'enseignement général des collèges"="422A",
                                    "422B-Professeurs de lycée professionnel"="422B",
                                    "422C-Maîtres auxiliaires et professeurs contractuels de l'enseignement secondaire"="422C",
                                    "422D-Conseillers principaux d'éducation"="422D",
                                    "422E-Surveillants et aides-éducateurs des établissements d'enseignement"="422E",
                                    "423A-Moniteurs d'école de conduite"="423A",
                                    "423B-Formateurs et animateurs de formation continue"="423B",
                                    "424A-Moniteurs et éducateurs sportifs, sportifs professionnels"="424A",
                                    "425A-Sous-bibliothécaires, cadres intermédiaires du patrimoine"="425A",
                                    "431A-Cadres infirmiers et assimilés"="431A",
                                    "431B-Infirmiers psychiatriques"="431B",
                                    "431C-Puéricultrices"="431C",
                                    "431D-Infirmiers spécialisés (autres qu'infirmiers psychiatriques et puéricultrices)"="431D",
                                    "431E-Sages-femmes (libérales ou salariées)"="431E",
                                    "431F-Infirmiers en soins généraux, salariés"="431F",
                                    "431G-Infirmiers libéraux"="431G",
                                    "432A-Masseurs-kinésithérapeutes rééducateurs, libéraux"="432A",
                                    "432B-Masseurs-kinésithérapeutes rééducateurs, salariés"="432B",
                                    "432C-Autres spécialistes de la rééducation, libéraux"="432C",
                                    "432D-Autres spécialistes de la rééducation, salariés"="432D",
                                    "433A-Techniciens médicaux"="433A",
                                    "433B-Opticiens lunetiers et audioprothésistes (indépendants et salariés)"="433B",
                                    "433C-Autres spécialistes de l'appareillage médical (indépendants et salariés)"="433C",
                                    "433D-Préparateurs en pharmacie"="433D",
                                    "434A-Cadres de l'intervention socio-éducative"="434A",
                                    "434B-Assistants de service social"="434B",
                                    "434C-Conseillers en économie sociale familiale"="434C",
                                    "434D-Éducateurs spécialisés"="434D",
                                    "434E-Moniteurs éducateurs"="434E",
                                    "434F-Éducateurs techniques spécialisés, moniteurs d'atelier"="434F",
                                    "434G-Éducateurs de jeunes enfants"="434G",
                                    "435A-Directeurs de centres socioculturels et de loisirs"="435A",
                                    "435B-Animateurs socioculturels et de loisirs"="435B",
                                    "441A-Clergé séculier"="441A",
                                    "441B-Clergé régulier"="441B",
                                    "451A-Professions intermédiaires de la Poste"="451A",
                                    "451B-Professions intermédiaires administratives de France Télécom (statut public)"="451B",
                                    "451C-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés"="451C",
                                    "451D-Ingénieurs du contrôle de la navigation aérienne"="451D",
                                    "451E-Autres personnels administratifs de catégorie B de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="451E",
                                    "451F-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)"="451F",
                                    "452A-Inspecteurs et officiers de police"="452A",
                                    "452B-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie"="452B",
                                    "461A-Personnel de secrétariat de niveau supérieur, secrétaires de direction (non cadres)"="461A",
                                    "461D-Maîtrise et techniciens des services financiers ou comptables"="461D",
                                    "461E-Maîtrise et techniciens administratifs des services juridiques ou du personnel"="461E",
                                    "461F-Maîtrise et techniciens administratifs des autres services administratifs"="461F",
                                    "462A-Chefs de petites surfaces de vente (salariés ou mandataires)"="462A",
                                    "462B-Maîtrise de l'exploitation des magasins de vente"="462B",
                                    "462C-Acheteurs non classés cadres, aides-acheteurs"="462C",
                                    "462D-Animateurs commerciaux des magasins de vente, marchandiseurs (non cadres)"="462D",
                                    "462E-Autres professions intermédiaires commerciales (sauf techniciens des forces de vente)"="462E",
                                    "463A-Techniciens commerciaux et technico-commerciaux, représentants en informatique"="463A",
                                    "463B-Techniciens commerciaux et technico-commerciaux, représentants en biens d'équipement, en biens intermédiaires, commerce interindustriel (hors informatique)"="463B",
                                    "463C-Techniciens commerciaux et technico-commerciaux, représentants en biens de consommation auprès d'entreprises"="463C",
                                    "463D-Techniciens commerciaux et technico-commerciaux, représentants en services auprès d'entreprises ou de professionnels (hors banque, assurance, informatique)"="463D",
                                    "463E-Techniciens commerciaux et technico-commerciaux, représentants auprès de particuliers (hors banque, assurance, informatique)"="463E",
                                    "464A-Assistants de la publicité, des relations publiques (indépendants ou salariés)"="464A",
                                    "464B-Interprètes, traducteurs (indépendants ou salariés)"="464B",
                                    "465A-Concepteurs et assistants techniques des arts graphiques, de la mode et de la décoration (indépendants et salariés)"="465A",
                                    "465B-Assistants techniques de la réalisation des spectacles vivants et audiovisuels (indépendants ou salariés)"="465B",
                                    "465C-Photographes (indépendants et salariés)"="465C",
                                    "466A-Responsables commerciaux et administratifs des transports de voyageurs et du tourisme (non cadres)"="466A",
                                    "466B-Responsables commerciaux et administratifs des transports de marchandises (non cadres)"="466B",
                                    "466C-Responsables d'exploitation des transports de voyageurs et de marchandises (non cadres)"="466C",
                                    "467A-Chargés de clientèle bancaire"="467A",
                                    "467B-Techniciens des opérations bancaires"="467B",
                                    "467C-Professions intermédiaires techniques et commerciales des assurances"="467C",
                                    "467D-Professions intermédiaires techniques des organismes de sécurité sociale"="467D",
                                    "468A-Maîtrise de restauration : salle et service"="468A",
                                    "468B-Maîtrise de l'hébergement : hall et étages"="468B",
                                    "471A-Techniciens d'étude et de conseil en agriculture, eaux et forêt"="471A",
                                    "471B-Techniciens d'exploitation et de contrôle de la production en agriculture, eaux et forêt"="471B",
                                    "472A-Dessinateurs en bâtiment, travaux publics"="472A",
                                    "472B-Géomètres, topographes"="472B",
                                    "472C-Métreurs et techniciens divers du bâtiment et des travaux publics"="472C",
                                    "472D-Techniciens des travaux publics de l'État et des collectivités locales"="472D",
                                    "473A-Dessinateurs en électricité, électromécanique et électronique"="473A",
                                    "473B-Techniciens de recherche-développement et des méthodes de fabrication en électricité, électromécanique et électronique"="473B",
                                    "473C-Techniciens de fabrication et de contrôle-qualité en électricité, électromécanique et électronique"="473C",
                                    "474A-Dessinateurs en construction mécanique et travail des métaux"="474A",
                                    "474B-Techniciens de recherche-développement et des méthodes de fabrication en construction mécanique et travail des métaux"="474B",
                                    "474C-Techniciens de fabrication et de contrôle-qualité en construction mécanique et travail des métaux"="474C",
                                    "475A-Techniciens de recherche-développement et des méthodes de production des industries de transformation"="475A",
                                    "475B-Techniciens de production et de contrôle-qualité des industries de transformation"="475B",
                                    "476A-Assistants techniques, techniciens de l'imprimerie et de l'édition"="476A",
                                    "476B-Techniciens de l'industrie des matériaux souples, de l'ameublement et du bois"="476B",
                                    "477A-Techniciens de la logistique, du planning et de l'ordonnancement"="477A",
                                    "477B-Techniciens d'installation et de maintenance des équipements industriels (électriques, électromécaniques, mécaniques, hors informatique)"="477B",
                                    "477C-Techniciens d'installation et de maintenance des équipements non industriels (hors informatique et télécommunications)"="477C",
                                    "477D-Techniciens de l'environnement et du traitement des pollutions"="477D",
                                    "478A-Techniciens d'étude et de développement en informatique"="478A",
                                    "478B-Techniciens de production, d'exploitation en informatique"="478B",
                                    "478C-Techniciens d'installation, de maintenance, support et services aux utilisateurs en informatique"="478C",
                                    "478D-Techniciens des télécommunications et de l'informatique des réseaux"="478D",
                                    "479A-Techniciens des laboratoires de recherche publique ou de l'enseignement"="479A",
                                    "479B-Experts salariés ou indépendants de niveau technicien, techniciens divers"="479B",
                                    "480A-Contremaîtres et agents d'encadrement (non cadres) en agriculture, sylviculture"="480A",
                                    "480B-Maîtres d'équipage de la marine marchande et de la pêche"="480B",
                                    "481A-Conducteurs de travaux (non cadres)"="481A",
                                    "481B-Chefs de chantier (non cadres)"="481B",
                                    "482A-Agents de maîtrise en fabrication de matériel électrique, électronique"="482A",
                                    "483A-Agents de maîtrise en construction mécanique, travail des métaux"="483A",
                                    "484A-Agents de maîtrise en fabrication : agroalimentaire, chimie, plasturgie, pharmacie."="484A",
                                    "484B-Agents de maîtrise en fabrication : métallurgie, matériaux lourds et autres industries de transformation"="484B",
                                    "485A-Agents de maîtrise et techniciens en production et distribution d'énergie, eau, chauffage"="485A",
                                    "485B-Agents de maîtrise en fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="485B",
                                    "486A-Agents de maîtrise en maintenance, installation en électricité, électromécanique et électronique"="486A",
                                    "486D-Agents de maîtrise en maintenance, installation en mécanique"="486D",
                                    "486E-Agents de maîtrise en entretien général, installation, travaux neufs (hors mécanique, électromécanique, électronique)"="486E",
                                    "487A-Responsables d'entrepôt, de magasinage"="487A",
                                    "487B-Responsables du tri, de l'emballage, de l'expédition et autres responsables de la manutention"="487B",
                                    "488A-Maîtrise de restauration : cuisine/production"="488A",
                                    "488B-Maîtrise de restauration : gestion d'établissement"="488B",
                                    "521A-Employés de la Poste"="521A",
                                    "521B-Employés de France Télécom (statut public)"="521B",
                                    "522A-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes"="522A",
                                    "523A-Adjoints administratifs de la fonction publique (y.c. enseignement)"="523A",
                                    "524A-Agents administratifs de la fonction publique (y.c. enseignement)"="524A",
                                    "525A-Agents de service des établissements primaires"="525A",
                                    "525B-Agents de service des autres établissements d'enseignement"="525B",
                                    "525C-Agents de service de la fonction publique (sauf écoles, hôpitaux)"="525C",
                                    "525D-Agents de service hospitaliers (de la fonction publique ou du secteur privé)"="525D",
                                    "526A-Aides-soignants (de la fonction publique ou du secteur privé)"="526A",
                                    "526B-Assistants dentaires, médicaux et vétérinaires, aides de techniciens médicaux"="526B",
                                    "526C-Auxiliaires de puériculture"="526C",
                                    "526D-Aides médico-psychologiques"="526D",
                                    "526E-Ambulanciers salariés (du secteur public ou du secteur privé)"="526E",
                                    "531A-Agents de police de l'État"="531A",
                                    "531B-Agents des polices municipales"="531B",
                                    "531C-Surveillants de l'administration pénitentiaire"="531C",
                                    "532A-Gendarmes (de grade inférieur à adjudant)"="532A",
                                    "532B-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)"="532B",
                                    "532C-Hommes du rang (sauf pompiers militaires)"="532C",
                                    "533A-Pompiers (y.c. pompiers militaires)"="533A",
                                    "533B-Agents techniques forestiers, gardes des espaces naturels"="533B",
                                    "533C-Agents de surveillance du patrimoine et des administrations"="533C",
                                    "534A-Agents civils de sécurité et de surveillance"="534A",
                                    "534B-Convoyeurs de fonds, gardes du corps, enquêteurs privés et métiers assimilés (salariés)"="534B",
                                    "541A-Agents et hôtesses d'accueil et d'information (hors hôtellerie)"="541A",
                                    "541D-Standardistes, téléphonistes"="541D",
                                    "542A-Secrétaires"="542A",
                                    "542B-Dactylos, sténodactylos (sans secrétariat), opérateurs de traitement de texte"="542B",
                                    "543A-Employés des services comptables ou financiers"="543A",
                                    "543D-Employés administratifs divers d'entreprises"="543D",
                                    "544A-Employés et opérateurs d'exploitation en informatique"="544A",
                                    "545A-Employés administratifs des services techniques de la banque"="545A",
                                    "545B-Employés des services commerciaux de la banque"="545B",
                                    "545C-Employés des services techniques des assurances"="545C",
                                    "545D-Employés des services techniques des organismes de sécurité sociale et assimilés"="545D",
                                    "546A-Contrôleurs des transports (personnels roulants)"="546A",
                                    "546B-Agents des services commerciaux des transports de voyageurs et du tourisme"="546B",
                                    "546C-Employés administratifs d'exploitation des transports de marchandises"="546C",
                                    "546D-Hôtesses de l'air et stewards"="546D",
                                    "546E-Autres agents et hôtesses d'accompagnement (transports, tourisme)"="546E",
                                    "551A-Employés de libre service du commerce et magasiniers"="551A",
                                    "552A-Caissiers de magasin"="552A",
                                    "553A-Vendeurs non spécialisés"="553A",
                                    "554A-Vendeurs en alimentation"="554A",
                                    "554B-Vendeurs en ameublement, décor, équipement du foyer"="554B",
                                    "554C-Vendeurs en droguerie, bazar, quincaillerie, bricolage"="554C",
                                    "554D-Vendeurs du commerce de fleurs"="554D",
                                    "554E-Vendeurs en habillement et articles de sport"="554E",
                                    "554F-Vendeurs en produits de beauté, de luxe (hors biens culturels) et optique"="554F",
                                    "554G-Vendeurs de biens culturels (livres, disques, multimédia, objets d'art)"="554G",
                                    "554H-Vendeurs de tabac, presse et articles divers"="554H",
                                    "554J-Pompistes et gérants de station-service (salariés ou mandataires)"="554J",
                                    "555A-Vendeurs par correspondance, télévendeurs"="555A",
                                    "556A-Vendeurs en gros de biens d'équipement, biens intermédiaires"="556A",
                                    "561A-Serveurs, commis de restaurant, garçons (bar, brasserie, café ou restaurant)"="561A",
                                    "561D-Aides de cuisine, apprentis de cuisine et employés polyvalents de la restauration"="561D",
                                    "561E-Employés de l'hôtellerie : réception et hall"="561E",
                                    "561F-Employés d'étage et employés polyvalents de l'hôtellerie"="561F",
                                    "562A-Manucures, esthéticiens (salariés)"="562A",
                                    "562B-Coiffeurs salariés"="562B",
                                    "563A-Assistantes maternelles, gardiennes d'enfants, familles d'accueil"="563A",
                                    "563B-Aides à domicile, aides ménagères, travailleuses familiales"="563B",
                                    "563C-Employés de maison et personnels de ménage chez des particuliers"="563C",
                                    "564A-Concierges, gardiens d'immeubles"="564A",
                                    "564B-Employés des services divers"="564B",
                                    "621A-Chefs d'équipe du gros œuvre et des travaux publics"="621A",
                                    "621B-Ouvriers qualifiés du travail du béton"="621B",
                                    "621C-Conducteurs qualifiés d'engins de chantiers du bâtiment et des travaux publics"="621C",
                                    "621D-Ouvriers des travaux publics en installations électriques et de télécommunications"="621D",
                                    "621E-Autres ouvriers qualifiés des travaux publics"="621E",
                                    "621F-Ouvriers qualifiés des travaux publics (salariés de l'État et des collectivités locales)"="621F",
                                    "621G-Mineurs de fond qualifiés et autres ouvriers qualifiés des industries d'extraction (carrières, pétrole, gaz...)"="621G",
                                    "622A-Opérateurs qualifiés sur machines automatiques en production électrique ou électronique"="622A",
                                    "622B-Câbleurs qualifiés, bobiniers qualifiés"="622B",
                                    "622G-Plateformistes, contrôleurs qualifiés de matériel électrique ou électronique"="622G",
                                    "623A-Chaudronniers-tôliers industriels, opérateurs qualifiés du travail en forge, conducteurs qualifiés d'équipement de formage, traceurs qualifiés"="623A",
                                    "623B-Tuyauteurs industriels qualifiés"="623B",
                                    "623C-Soudeurs qualifiés sur métaux"="623C",
                                    "623F-Opérateurs qualifiés d'usinage des métaux travaillant à l'unité ou en petite série, moulistes qualifiés"="623F",
                                    "623G-Opérateurs qualifiés d'usinage des métaux sur autres machines (sauf moulistes)"="623G",
                                    "624A-Monteurs qualifiés d'ensembles mécaniques"="624A",
                                    "624D-Monteurs qualifiés en structures métalliques"="624D",
                                    "624E-Ouvriers qualifiés de contrôle et d'essais en mécanique"="624E",
                                    "624F-Ouvriers qualifiés des traitements thermiques et de surface sur métaux"="624F",
                                    "624G-Autres mécaniciens ou ajusteurs qualifiés (ou spécialité non reconnue)"="624G",
                                    "625A-Pilotes d'installation lourde des industries de transformation : agroalimentaire, chimie, plasturgie, énergie"="625A",
                                    "625B-Ouvriers qualifiés et agents qualifiés de laboratoire : agroalimentaire, chimie, biologie, pharmacie"="625B",
                                    "625C-Autres opérateurs et ouvriers qualifiés de la chimie (y.c. pharmacie) et de la plasturgie"="625C",
                                    "625D-Opérateurs de la transformation des viandes"="625D",
                                    "625E-Autres opérateurs et ouvriers qualifiés de l'industrie agricole et alimentaire (hors transformation des viandes)"="625E",
                                    "625H-Ouvriers qualifiés des autres industries (eau, gaz, énergie, chauffage)"="625H",
                                    "626A-Pilotes d'installation lourde des industries de transformation : métallurgie, production verrière, matériaux de construction"="626A",
                                    "626B-Autres opérateurs et ouvriers qualifiés : métallurgie, production verrière, matériaux de construction"="626B",
                                    "626C-Opérateurs et ouvriers qualifiés des industries lourdes du bois et de la fabrication du papier-carton"="626C",
                                    "627A-Opérateurs qualifiés du textile et de la mégisserie"="627A",
                                    "627B-Ouvriers qualifiés de la coupe des vêtements et de l'habillement, autres opérateurs de confection qualifiés"="627B",
                                    "627C-Ouvriers qualifiés du travail industriel du cuir"="627C",
                                    "627D-Ouvriers qualifiés de scierie, de la menuiserie industrielle et de l'ameublement"="627D",
                                    "627E-Ouvriers de la photogravure et des laboratoires photographiques et cinématographiques"="627E",
                                    "627F-Ouvriers de la composition et de l'impression, ouvriers qualifiés de la brochure, de la reliure et du façonnage du papier-carton"="627F",
                                    "628A-Mécaniciens qualifiés de maintenance, entretien : équipements industriels"="628A",
                                    "628B-Électromécaniciens, électriciens qualifiés d'entretien : équipements industriels"="628B",
                                    "628C-Régleurs qualifiés d'équipements de fabrication (travail des métaux, mécanique)"="628C",
                                    "628D-Régleurs qualifiés d'équipements de fabrication (hors travail des métaux et mécanique)"="628D",
                                    "628E-Ouvriers qualifiés de l'assainissement et du traitement des déchets"="628E",
                                    "628F-Agents qualifiés de laboratoire (sauf chimie, santé)"="628F",
                                    "628G-Ouvriers qualifiés divers de type industriel"="628G",
                                    "631A-Jardiniers"="631A",
                                    "632A-Maçons qualifiés"="632A",
                                    "632B-Ouvriers qualifiés du travail de la pierre"="632B",
                                    "632C-Charpentiers en bois qualifiés"="632C",
                                    "632D-Menuisiers qualifiés du bâtiment"="632D",
                                    "632E-Couvreurs qualifiés"="632E",
                                    "632F-Plombiers et chauffagistes qualifiés"="632F",
                                    "632G-Peintres et ouvriers qualifiés de pose de revêtements sur supports verticaux"="632G",
                                    "632H-Soliers moquetteurs et ouvriers qualifiés de pose de revêtements souples sur supports horizontaux"="632H",
                                    "632J-Monteurs qualifiés en agencement, isolation"="632J",
                                    "632K-Ouvriers qualifiés d'entretien général des bâtiments"="632K",
                                    "633A-Électriciens qualifiés de type artisanal (y.c. bâtiment)"="633A",
                                    "633B-Dépanneurs qualifiés en radiotélévision, électroménager, matériel électronique (salariés)"="633B",
                                    "633C-Électriciens, électroniciens qualifiés en maintenance entretien, réparation : automobile"="633C",
                                    "633D-Électriciens, électroniciens qualifiés en maintenance, entretien : équipements non industriels"="633D",
                                    "634A-Carrossiers d'automobiles qualifiés"="634A",
                                    "634B-Métalliers, serruriers qualifiés"="634B",
                                    "634C-Mécaniciens qualifiés en maintenance, entretien, réparation : automobile"="634C",
                                    "634D-Mécaniciens qualifiés de maintenance, entretien : équipements non industriels"="634D",
                                    "635A-Tailleurs et couturières qualifiés, ouvriers qualifiés du travail des étoffes (sauf fabrication de vêtements), ouvriers qualifiés de type artisanal du travail du cuir"="635A",
                                    "636A-Bouchers (sauf industrie de la viande)"="636A",
                                    "636B-Charcutiers (sauf industrie de la viande)"="636B",
                                    "636C-Boulangers, pâtissiers (sauf activité industrielle)"="636C",
                                    "636D-Cuisiniers et commis de cuisine"="636D",
                                    "637A-Modeleurs (sauf modeleurs de métal), mouleurs-noyauteurs à la main, ouvriers qualifiés du travail du verre ou de la céramique à la main"="637A",
                                    "637B-Ouvriers d'art"="637B",
                                    "637C-Ouvriers et techniciens des spectacles vivants et audiovisuels"="637C",
                                    "637D-Ouvriers qualifiés divers de type artisanal"="637D",
                                    "641A-Conducteurs routiers et grands routiers (salariés)"="641A",
                                    "641B-Conducteurs de véhicule routier de transport en commun (salariés)"="641B",
                                    "642A-Conducteurs de taxi (salariés)"="642A",
                                    "642B-Conducteurs de voiture particulière (salariés)"="642B",
                                    "643A-Conducteurs livreurs, coursiers (salariés)"="643A",
                                    "644A-Conducteurs de véhicule de ramassage des ordures ménagères"="644A",
                                    "651A-Conducteurs d'engin lourd de levage"="651A",
                                    "651B-Conducteurs d'engin lourd de manœuvre"="651B",
                                    "652A-Ouvriers qualifiés de la manutention, conducteurs de chariots élévateurs, caristes"="652A",
                                    "652B-Dockers"="652B",
                                    "653A-Magasiniers qualifiés"="653A",
                                    "654A-Conducteurs qualifiés d'engins de transport guidés"="654A",
                                    "655A-Autres agents et ouvriers qualifiés (sédentaires) des services d'exploitation des transports"="655A",
                                    "656A-Matelots de la marine marchande, capitaines et matelots timoniers de la navigation fluviale (salariés)"="656A",
                                    "671A-Ouvriers non qualifiés des travaux publics de l'État et des collectivités locales"="671A",
                                    "671B-Ouvriers non qualifiés des travaux publics, du travail du béton et de l'extraction, hors État et collectivités locales"="671B",
                                    "672A-Ouvriers non qualifiés de l'électricité et de l'électronique"="672A",
                                    "673A-Ouvriers de production non qualifiés travaillant par enlèvement de métal"="673A",
                                    "673B-Ouvriers de production non qualifiés travaillant par formage de métal"="673B",
                                    "673C-Ouvriers non qualifiés de montage, contrôle en mécanique et travail des métaux"="673C",
                                    "674A-Ouvriers de production non qualifiés : chimie, pharmacie, plasturgie"="674A",
                                    "674B-Ouvriers de production non qualifiés de la transformation des viandes"="674B",
                                    "674C-Autres ouvriers de production non qualifiés : industrie agro-alimentaire"="674C",
                                    "674D-Ouvriers de production non qualifiés : métallurgie, production verrière, céramique, matériaux de construction"="674D",
                                    "674E-Ouvriers de production non qualifiés : industrie lourde du bois, fabrication des papiers et cartons"="674E",
                                    "675A-Ouvriers de production non qualifiés du textile et de la confection, de la tannerie-mégisserie et du travail du cuir"="675A",
                                    "675B-Ouvriers de production non qualifiés du travail du bois et de l'ameublement"="675B",
                                    "675C-Ouvriers de production non qualifiés de l'imprimerie, presse, édition"="675C",
                                    "676A-Manutentionnaires non qualifiés"="676A",
                                    "676B-Déménageurs (hors chauffeurs-déménageurs), non qualifiés"="676B",
                                    "676C-Ouvriers du tri, de l'emballage, de l'expédition, non qualifiés"="676C",
                                    "676D-Agents non qualifiés des services d'exploitation des transports"="676D",
                                    "676E-Ouvriers non qualifiés divers de type industriel"="676E",
                                    "681A-Ouvriers non qualifiés du gros œuvre du bâtiment"="681A",
                                    "681B-Ouvriers non qualifiés du second œuvre du bâtiment"="681B",
                                    "682A-Métalliers, serruriers, réparateurs en mécanique non qualifiés"="682A",
                                    "683A-Apprentis boulangers, bouchers, charcutiers"="683A",
                                    "684A-Nettoyeurs"="684A",
                                    "684B-Ouvriers non qualifiés de l'assainissement et du traitement des déchets"="684B",
                                    "685A-Ouvriers non qualifiés divers de type artisanal"="685A",
                                    "691A-Conducteurs d'engin agricole ou forestier"="691A",
                                    "691B-Ouvriers de l'élevage"="691B",
                                    "691C-Ouvriers du maraîchage ou de l'horticulture"="691C",
                                    "691D-Ouvriers de la viticulture ou de l'arboriculture fruitière"="691D",
                                    "691E-Ouvriers agricoles sans spécialisation particulière"="691E",
                                    "691F-Ouvriers de l'exploitation forestière ou de la sylviculture"="691F",
                                    "692A-Marins-pêcheurs et ouvriers de l'aquaculture"="692A",
                                    
  ) } 
if("CSEDEB" %in% names(ct) & !is.numeric(ct$CSEDEB) ) {
  ct$CSEDEB <- forcats::fct_recode(ctraw$CSEDEB,
                                   
                                   "11-Agriculteurs sur petite exploitation"="11",
                                   "12-Agriculteurs sur moyenne exploitation"="12",
                                   "13-Agriculteurs sur grande exploitation"="13",
                                   "21-Artisans"="21",
                                   "22-Commerçants et assimilés"="22",
                                   "23-Chefs d'entreprise de 10 salariés ou plus"="23",
                                   "31-Professions libérales"="31",
                                   "33-Cadres de la fonction publique"="33",
                                   "34-Professeurs, professions scientifiques"="34",
                                   "35-Professions de l'information, des arts et des spectacles"="35",
                                   "37-Cadres administratifs et commerciaux d'entreprise"="37",
                                   "38-Ingénieurs et cadres techniques d'entreprise"="38",
                                   "42-Professeurs des écoles, instituteurs et assimilés"="42",
                                   "43-Professions intermédiaires de la santé et du travail social"="43",
                                   "44-Clergé, religieux"="44",
                                   "45-Professions intermédiaires administratives de la fonction publique"="45",
                                   "46-Professions intermédiaires administratives et commerciales des entreprises"="46",
                                   "47-Techniciens"="47",
                                   "48-Contremaîtres, agents de maîtrise"="48",
                                   "52-Employés civils et agents de service de la fonction publique"="52",
                                   "53-Policiers et militaires"="53",
                                   "54-Employés administratifs d'entreprise"="54",
                                   "55-Employés de commerce"="55",
                                   "56-Personnels des services directs aux particuliers"="56",
                                   "62-Ouvriers qualifiés de type industriel"="62",
                                   "63-Ouvriers qualifiés de type artisanal"="63",
                                   "64-Chauffeurs"="64",
                                   "65-Ouvriers qualifiés de la manutention, du magasinage et du transport"="65",
                                   "67-Ouvriers non qualifiés de type industriel"="67",
                                   "68-Ouvriers non qualifiés de type artisanal"="68",
                                   "69-Ouvriers agricoles"="69",
                                   
  ) } 
if("STATUTA" %in% names(ct) & !is.numeric(ct$STATUTA) ) {
  ct$STATUTA <- forcats::fct_recode(ctraw$STATUTA,
                                    
                                    "1-A son compte"="1",
                                    "2-Aide familial"="2",
                                    "3-Salarié État collectivités locales"="3",
                                    "4-Autre salarié"="4",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("lnaismer" %in% names(ct) & !is.numeric(ct$lnaismer) ) {
  ct$lnaismer <- forcats::fct_recode(ctraw$lnaismer,
                                     
                                     "11-France"="11",
                                     "21-Algérie"="21",
                                     "22-Maroc"="22",
                                     "23-Tunisie"="23",
                                     "24-Autres pays d'Afrique"="24",
                                     "31-Vietnam, Laos, Cambodge"="31",
                                     "34-Turquie"="34",
                                     "35-Autres Asie"="35",
                                     "41-Portugal"="41",
                                     "42-Espagne"="42",
                                     "43-Italie"="43",
                                     "44-Autres pays de l'UE 15"="44",
                                     "45-Autres pays de l'UE 27"="45",
                                     "46-Autres Europe"="46",
                                     "51-Autres"="51",
                                     "NULL"="99",
                                     
  ) } 
if("lnaisper" %in% names(ct) & !is.numeric(ct$lnaisper) ) {
  ct$lnaisper <- forcats::fct_recode(ctraw$lnaisper,
                                     
                                     "11-France"="11",
                                     "21-Algérie"="21",
                                     "22-Maroc"="22",
                                     "23-Tunisie"="23",
                                     "24-Autres pays d'Afrique"="24",
                                     "31-Vietnam, Laos, Cambodge"="31",
                                     "34-Turquie"="34",
                                     "35-Autres Asie"="35",
                                     "41-Portugal"="41",
                                     "42-Espagne"="42",
                                     "43-Italie"="43",
                                     "44-Autres pays de l'UE 15"="44",
                                     "45-Autres pays de l'UE 27"="45",
                                     "46-Autres Europe"="46",
                                     "51-Autres"="51",
                                     "NULL"="99",
                                     
  ) } 
if("natnaisper" %in% names(ct) & !is.numeric(ct$natnaisper) ) {
  ct$natnaisper <- forcats::fct_recode(ctraw$natnaisper,
                                       
                                       "11-France"="11",
                                       "21-Algérie"="21",
                                       "22-Maroc"="22",
                                       "23-Tunisie"="23",
                                       "24-Autres pays d'Afrique"="24",
                                       "31-Vietnam, Laos, Cambodge"="31",
                                       "34-Turquie"="34",
                                       "35-Autres Asie"="35",
                                       "41-Portugal"="41",
                                       "42-Espagne"="42",
                                       "43-Italie"="43",
                                       "44-Autres pays de l'UE 15"="44",
                                       "45-Autres pays de l'UE 27"="45",
                                       "46-Autres Europe"="46",
                                       "51-Autres"="51",
                                       "NULL"="99",
                                       
  ) } 
if("natnaismer" %in% names(ct) & !is.numeric(ct$natnaismer) ) {
  ct$natnaismer <- forcats::fct_recode(ctraw$natnaismer,
                                       
                                       "11-France"="11",
                                       "21-Algérie"="21",
                                       "22-Maroc"="22",
                                       "23-Tunisie"="23",
                                       "24-Autres pays d'Afrique"="24",
                                       "31-Vietnam, Laos, Cambodge"="31",
                                       "34-Turquie"="34",
                                       "35-Autres Asie"="35",
                                       "41-Portugal"="41",
                                       "42-Espagne"="42",
                                       "43-Italie"="43",
                                       "44-Autres pays de l'UE 15"="44",
                                       "45-Autres pays de l'UE 27"="45",
                                       "46-Autres Europe"="46",
                                       "51-Autres"="51",
                                       "NULL"="99",
                                       
  ) } 
if("perimmi" %in% names(ct) & !is.numeric(ct$perimmi) ) {
  ct$perimmi <- forcats::fct_recode(ctraw$perimmi,
                                    
                                    "0-Non immigré"="0",
                                    "1-Immigré"="1",
                                    
  ) } 
if("merimmi" %in% names(ct) & !is.numeric(ct$merimmi) ) {
  ct$merimmi <- forcats::fct_recode(ctraw$merimmi,
                                    
                                    "0-Non immigrée"="0",
                                    "1-Immigrée"="1",
                                    
  ) } 
if("cser" %in% names(ct) & !is.numeric(ct$cser) ) {
  ct$cser <- forcats::fct_recode(ctraw$cser,
                                 
                                 "NULL"="0",
                                 "1-Agriculteurs exploitants"="1",
                                 "2-Artisans, commerçants et chefs d'entreprise"="2",
                                 "3-Cadres et professions intellectuelles supérieures"="3",
                                 "4-Professions intermédiaires"="4",
                                 "5-Employés"="5",
                                 "6-Ouvriers"="6",
                                 
  ) } 
if("csermer" %in% names(ct) & !is.numeric(ct$csermer) ) {
  ct$csermer <- forcats::fct_recode(ctraw$csermer,
                                    
                                    "NULL"="0",
                                    "1-Agriculteurs exploitants"="1",
                                    "2-Artisans, commerçants et chefs d'entreprise"="2",
                                    "3-Cadres et professions intellectuelles supérieures"="3",
                                    "4-Professions intermédiaires"="4",
                                    "5-Employés"="5",
                                    "6-Ouvriers"="6",
                                    
  ) } 
if("cserper" %in% names(ct) & !is.numeric(ct$cserper) ) {
  ct$cserper <- forcats::fct_recode(ctraw$cserper,
                                    
                                    "NULL"="0",
                                    "1-Agriculteurs exploitants"="1",
                                    "2-Artisans, commerçants et chefs d'entreprise"="2",
                                    "3-Cadres et professions intellectuelles supérieures"="3",
                                    "4-Professions intermédiaires"="4",
                                    "5-Employés"="5",
                                    "6-Ouvriers"="6",
                                    
  ) } 
if("cserdeb" %in% names(ct) & !is.numeric(ct$cserdeb) ) {
  ct$cserdeb <- forcats::fct_recode(ctraw$cserdeb,
                                    
                                    "NULL"="0",
                                    "1-Agriculteurs exploitants"="1",
                                    "2-Artisans, commerçants et chefs d'entreprise"="2",
                                    "3-Cadres et professions intellectuelles supérieures"="3",
                                    "4-Professions intermédiaires"="4",
                                    "5-Employés"="5",
                                    "6-Ouvriers"="6",
                                    
  ) } 
if("naf38" %in% names(ct) & !is.numeric(ct$naf38) ) {
  ct$naf38 <- forcats::fct_recode(ctraw$naf38,
                                  
                                  "NULL"="00",
                                  "AZ-Agriculture, sylviculture et pêche"="AZ",
                                  "BZ-Industries extractives"="BZ",
                                  "CA-Fabrication de denrées alimentaires, de boissons et de produits à base de tabac"="CA",
                                  "CB-Fabrication de textiles, industries de l'habillement, industrie du cuir et de la chaussure"="CB",
                                  "CC-Travail du bois, industries du papier et imprimerie"="CC",
                                  "CD-Cokéfaction et raffinage"="CD",
                                  "CE-Industrie chimique"="CE",
                                  "CF-Industrie pharmaceutique"="CF",
                                  "CG-Fabrication de produits en caoutchouc et en plastique ainsi que d'autres produits minéraux non métalliques"="CG",
                                  "CH-Métallurgie et fabrication de produits métalliques à l'exception des machines et des équipements"="CH",
                                  "CI-Fabrication de produits informatiques, électroniques et optiques"="CI",
                                  "CJ-Fabrication d'équipements électriques"="CJ",
                                  "CK-Fabrication de machines et équipements n.c.a."="CK",
                                  "CL-Fabrication de matériels de transport"="CL",
                                  "CM-Autres industries manufacturières ,réparation et installation de machines et d'équipements"="CM",
                                  "DZ-Production et distribution d'électricité, de gaz, de vapeur et d'air conditionné"="DZ",
                                  "EZ-Production et distribution d'eau ,assainissement, gestion des déchets et dépollution"="EZ",
                                  "FZ-Construction"="FZ",
                                  "GZ-Commerce ,réparation d'automobiles et de motocycles"="GZ",
                                  "HZ-Transports et entreposage"="HZ",
                                  "IZ-Hébergement et restauration"="IZ",
                                  "JA-Edition, audiovisuel et diffusion"="JA",
                                  "JB-Télécommunications"="JB",
                                  "JC-Activités informatiques et services d'information"="JC",
                                  "KZ-Activités financières et d'assurance"="KZ",
                                  "LI-dont : loyers imputés des logements occupés par leur propriétaire"="LI",
                                  "LZ-Activités immobilières"="LZ",
                                  "MA-Activités juridiques, comptables, de gestion, d'architecture, d'ingénierie, de contrôle et d'analyses techniques"="MA",
                                  "MB-Recherche-développement scientifique"="MB",
                                  "MC-Autres activités spécialisées, scientifiques et techniques"="MC",
                                  "NZ-Activités de services administratifs et de soutien"="NZ",
                                  "OZ-Administration publique"="OZ",
                                  "PZ-Enseignement"="PZ",
                                  "QA-Activités pour la santé humaine"="QA",
                                  "QB-Hébergement médico-social et social et action sociale sans hébergement"="QB",
                                  "RZ-Arts, spectacles et activités récréatives"="RZ",
                                  "SZ-Autres activités de services"="SZ",
                                  "TZ-Activités des ménages en tant qu'employeurs ,activités indifférenciées des ménages en tant que producteurs de biens et services pour usage propre"="TZ",
                                  "UZ-Activités extra-territoriales"="UZ",
                                  
  ) } 
if("naf17" %in% names(ct) & !is.numeric(ct$naf17) ) {
  ct$naf17 <- forcats::fct_recode(ctraw$naf17,
                                  
                                  "NULL"="00",
                                  "AZ-Agriculture, sylviculture et pêche"="AZ",
                                  "C1-Fabrication de denrées alimentaires, de boissons et de produits à base de tabac"="C1",
                                  "C2-Cokéfaction et raffinage"="C2",
                                  "C3-Fabrication d'équipements électriques, électroniques, informatiques ,fabrication de machines"="C3",
                                  "C4-Fabrication de matériels de transport"="C4",
                                  "C5-Fabrication d'autres produits industriels"="C5",
                                  "DE-Industries extractives, énergie, eau, gestion des déchets et dépollution"="DE",
                                  "FZ-Construction"="FZ",
                                  "GZ-Commerce ,réparation d'automobiles et de motocycles"="GZ",
                                  "HZ-Transports et entreposage"="HZ",
                                  "IZ-Hébergement et restauration"="IZ",
                                  "JZ-Information et communication"="JZ",
                                  "KZ-Activités financières et d'assurance"="KZ",
                                  "LZ-Activités immobilières"="LZ",
                                  "MN-Activités scientifiques et techniques ,services administratifs et de soutien"="MN",
                                  "OQ-Administration publique, enseignement, santé humaine et action sociale"="OQ",
                                  "RU-Autres activités de services"="RU",
                                  
  ) } 
if("NAF4" %in% names(ct) & !is.numeric(ct$NAF4) ) {
  ct$NAF4 <- forcats::fct_recode(ctraw$NAF4,
                                 
                                 "NULL"="00",
                                 "ES-Agriculture"="ES",
                                 "ET-Industrie"="ET",
                                 "EU-Construction"="EU",
                                 "EV-Tertiaire"="EV",
                                 
  ) } 
if("FAP225" %in% names(ct) & !is.numeric(ct$FAP225) ) {
  ct$FAP225 <- forcats::fct_recode(ctraw$FAP225,
                                   
                                   "A0Z00-Agriculteurs indépendants"="A0Z00",
                                   "A0Z01-Éleveurs indépendants"="A0Z01",
                                   "A0Z02-Bûcherons, sylviculteurs indépendants"="A0Z02",
                                   "A0Z40-Agriculteurs salariés"="A0Z40",
                                   "A0Z41-Éleveurs salariés"="A0Z41",
                                   "A0Z42-Bûcherons, sylviculteurs salariés et agents forestiers"="A0Z42",
                                   "A0Z43-Conducteurs d'engins agricoles ou forestiers"="A0Z43",
                                   "A1Z00-Maraîchers, horticulteurs indépendants"="A1Z00",
                                   "A1Z01-Viticulteurs, arboriculteurs indépendants"="A1Z01",
                                   "A1Z40-Maraîchers, horticulteurs salariés"="A1Z40",
                                   "A1Z41-Jardiniers salariés"="A1Z41",
                                   "A1Z42-Viticulteurs, arboriculteurs salariés"="A1Z42",
                                   "A2Z70-Techniciens et agents d'encadrement d'exploitations agricoles"="A2Z70",
                                   "A2Z90-Ingénieurs, cadres techniques de l'agriculture"="A2Z90",
                                   "A3Z00-Marins, pêcheurs, aquaculteurs indépendants"="A3Z00",
                                   "A3Z40-Pêcheurs, aquaculteurs salariés"="A3Z40",
                                   "A3Z41-Marins salariés"="A3Z41",
                                   "A3Z90-Cadres et maîtres d'équipage de la marine"="A3Z90",
                                   "B0Z20-Ouvriers non qualifiés des travaux publics, du béton et de l'extraction"="B0Z20",
                                   "B0Z21-Ouvriers non qualifiés du gros œuvre du bâtiment"="B0Z21",
                                   "B1Z40-Ouvriers qualifiés des travaux publics, du béton et de l'extraction"="B1Z40",
                                   "B2Z40-Maçons"="B2Z40",
                                   "B2Z41-Professionnels du travail de la pierre et des matériaux associés"="B2Z41",
                                   "B2Z42-Charpentiers (métal)"="B2Z42",
                                   "B2Z43-Charpentiers (bois)"="B2Z43",
                                   "B2Z44-Couvreurs"="B2Z44",
                                   "B3Z20-Ouvriers non qualifiés du second œuvre du bâtiment"="B3Z20",
                                   "B4Z41-Plombiers, chauffagistes"="B4Z41",
                                   "B4Z42-Menuisiers et ouvriers de l'agencement et de l'isolation"="B4Z42",
                                   "B4Z43-Électriciens du bâtiment"="B4Z43",
                                   "B4Z44-Ouvriers qualifiés de la peinture et de la finition du bâtiment"="B4Z44",
                                   "B5Z40-Conducteurs d'engins du bâtiment et des travaux publics"="B5Z40",
                                   "B6Z70-Géomètres"="B6Z70",
                                   "B6Z71-Techniciens et chargés d'études du bâtiment et des travaux publics"="B6Z71",
                                   "B6Z72-Dessinateurs en bâtiment et en travaux publics"="B6Z72",
                                   "B6Z73-Chefs de chantier, conducteurs de travaux (non cadres)"="B6Z73",
                                   "B7Z90-Architectes"="B7Z90",
                                   "B7Z91-Ingénieurs du bâtiment et des travaux publics, chefs de chantier et conducteurs de travaux (cadres)"="B7Z91",
                                   "C0Z20-Ouvriers non qualifiés de l'électricité et de l'électronique"="C0Z20",
                                   "C1Z40-Ouvriers qualifiés de l'électricité et de l'électronique"="C1Z40",
                                   "C2Z70-Techniciens en électricité et en électronique"="C2Z70",
                                   "C2Z71-Dessinateurs en électricité et en électronique"="C2Z71",
                                   "C2Z80-Agents de maîtrise et assimilés en fabrication de matériel électrique, électronique"="C2Z80",
                                   "D0Z20-Ouvriers non qualifiés travaillant par enlèvement ou formage de métal"="D0Z20",
                                   "D1Z40-Régleurs"="D1Z40",
                                   "D1Z41-Ouvriers qualifiés travaillant par enlèvement de métal"="D1Z41",
                                   "D2Z40-Chaudronniers, tôliers, traceurs, serruriers, métalliers, forgerons"="D2Z40",
                                   "D2Z41-Tuyauteurs"="D2Z41",
                                   "D2Z42-Soudeurs"="D2Z42",
                                   "D3Z20-Ouvriers non qualifiés métallerie, serrurerie, montage"="D3Z20",
                                   "D4Z40-Monteurs, ajusteurs et autres ouvriers qualifiés de la mécanique"="D4Z40",
                                   "D4Z41-Agents qualifiés de traitement thermique et de surface"="D4Z41",
                                   "D6Z70-Techniciens en mécanique et travail des métaux"="D6Z70",
                                   "D6Z71-Dessinateurs en mécanique et travail des métaux"="D6Z71",
                                   "D6Z80-Agents de maîtrise et assimilés en fabrication mécanique"="D6Z80",
                                   "E0Z20-Ouvriers non qualifiés des industries chimiques et plastiques"="E0Z20",
                                   "E0Z21-Ouvriers non qualifiés des industries agro-alimentaires"="E0Z21",
                                   "E0Z22-Ouvriers non qualifiés en métallurgie, verre, céramique et matériaux de construction"="E0Z22",
                                   "E0Z23-Ouvriers non qualifiés du papier-carton et du bois"="E0Z23",
                                   "E0Z24-Autres ouvriers non qualifiés de type industriel"="E0Z24",
                                   "E1Z40-Pilotes d'installation lourde des industries de transformation"="E1Z40",
                                   "E1Z41-Autres ouvriers qualifiés des industries chimiques et plastiques"="E1Z41",
                                   "E1Z42-Autres ouvriers qualifiés des industries agro-alimentaires (hors transformation des viandes)"="E1Z42",
                                   "E1Z43-Autres ouvriers qualifiés en verre, céramique, métallurgie, matériaux de construction et énergie "="E1Z43",
                                   "E1Z44-Ouvriers qualifiés des industries lourdes du bois et de la fabrication de papier-carton"="E1Z44",
                                   "E1Z46-Agents qualifiés de laboratoire"="E1Z46",
                                   "E1Z47-Autres ouvriers qualifiés de type industriel"="E1Z47",
                                   "E2Z70-Techniciens des industries de process"="E2Z70",
                                   "E2Z80-Agents de maîtrise et assimilés des industries de process"="E2Z80",
                                   "F0Z20-Ouvriers non qualifiés du textile et du cuir"="F0Z20",
                                   "F1Z40-Ouvriers qualifiés du travail industriel du textile et du cuir"="F1Z40",
                                   "F1Z41-Ouvriers qualifiés du travail artisanal du textile et du cuir"="F1Z41",
                                   "F2Z20-Ouvriers non qualifiés du travail du bois et de l'ameublement"="F2Z20",
                                   "F3Z40-Artisans du travail du bois et de l'ameublement"="F3Z40",
                                   "F3Z41-Ouvriers qualifiés du travail du bois et de l'ameublement"="F3Z41",
                                   "F4Z20-Ouvriers non qualifiés de l'imprimerie, de la presse et de l'édition"="F4Z20",
                                   "F4Z41-Ouvriers qualifiés de l'impression et du façonnage des industries graphiques"="F4Z41",
                                   "F5Z70-Techniciens et agents de maîtrise des matériaux souples, du bois et des industries graphiques"="F5Z70",
                                   "G0A40-Ouvriers qualifiés de la maintenance en mécanique"="G0A40",
                                   "G0A41-Ouvriers qualifiés de la maintenance en électricité et en électronique"="G0A41",
                                   "G0A42-Mainteniciens en biens électrodomestiques"="G0A42",
                                   "G0A43-Ouvriers qualifiés polyvalents d'entretien du bâtiment"="G0A43",
                                   "G0B40-Carrossiers automobiles"="G0B40",
                                   "G0B41-Mécaniciens et électroniciens de véhicules"="G0B41",
                                   "G1Z70-Techniciens et agents de maîtrise de la maintenance et de l'environnement"="G1Z70",
                                   "G1Z71-Techniciens experts"="G1Z71",
                                   "G1Z80-Agents de maîtrise en entretien"="G1Z80",
                                   "H0Z90-Ingénieurs et cadres de fabrication et de la production"="H0Z90",
                                   "H0Z91-Cadres techniques de la maintenance et de l'environnement"="H0Z91",
                                   "H0Z92-Ingénieurs des méthodes de production, du contrôle qualité"="H0Z92",
                                   "J0Z20-Ouvriers non qualifiés de l'emballage et manutentionnaires"="J0Z20",
                                   "J1Z40-Ouvriers qualifiés du magasinage et de la manutention"="J1Z40",
                                   "J1Z80-Responsables magasinage"="J1Z80",
                                   "J3Z40-Conducteurs de véhicules légers"="J3Z40",
                                   "J3Z41-Conducteurs de transport en commun sur route"="J3Z41",
                                   "J3Z42-Conducteurs et livreurs sur courte distance"="J3Z42",
                                   "J3Z43-Conducteurs routiers"="J3Z43",
                                   "J3Z44-Conducteurs sur rails et d'engins de traction"="J3Z44",
                                   "J4Z40-Agents d'exploitation des transports"="J4Z40",
                                   "J4Z60-Contrôleurs des transports"="J4Z60",
                                   "J4Z80-Responsables logistiques (non cadres)"="J4Z80",
                                   "J5Z60-Agents et hôtesses d'accompagnement"="J5Z60",
                                   "J5Z61-Agents administratifs des transports"="J5Z61",
                                   "J5Z62-Employés des transports et du tourisme"="J5Z62",
                                   "J5Z80-Techniciens des transports et du tourisme"="J5Z80",
                                   "J6Z90-Cadres des transports"="J6Z90",
                                   "J6Z91-Personnels navigants de l'aviation"="J6Z91",
                                   "J6Z92-Ingénieurs et cadres de la logistique, du planning et de l'ordonnancement"="J6Z92",
                                   "K0Z20-Ouvriers non qualifiés divers de type artisanal"="K0Z20",
                                   "K0Z40-Artisans et ouvriers qualifiés divers de type artisanal"="K0Z40",
                                   "L0Z60-Secrétaires bureautiques et assimilés"="L0Z60",
                                   "L1Z60-Employés de la comptabilité"="L1Z60",
                                   "L2Z60-Agents d'accueil et d'information"="L2Z60",
                                   "L2Z61-Agents administratifs divers"="L2Z61",
                                   "L3Z80-Secrétaires de direction"="L3Z80",
                                   "L4Z80-Techniciens des services administratifs"="L4Z80",
                                   "L4Z81-Techniciens des services comptables et financiers"="L4Z81",
                                   "L5Z90-Cadres administratifs, comptables et financiers (hors juristes)"="L5Z90",
                                   "L5Z91-Juristes"="L5Z91",
                                   "L5Z92-Cadres des ressources humaines et du recrutement"="L5Z92",
                                   "L6Z00-Dirigeants de petites et moyennes entreprises"="L6Z00",
                                   "L6Z90-Cadres dirigeants des grandes entreprises"="L6Z90",
                                   "M0Z60-Employés et opérateurs en informatique"="M0Z60",
                                   "M1Z80-Techniciens d'étude et de développement en informatique"="M1Z80",
                                   "M1Z81-Techniciens de production, d'exploitation, d'installation, et de maintenance, support et services aux utilisateurs en informatique"="M1Z81",
                                   "M2Z90-Ingénieurs et cadres d'étude, recherche et développement en informatique, chefs de projets informatiques"="M2Z90",
                                   "M2Z91-Ingénieurs et cadres d'administration, maintenance en informatique"="M2Z91",
                                   "M2Z92-Ingénieurs et cadres des télécommunications"="M2Z92",
                                   "N0Z90-Ingénieurs et cadres d'étude, recherche et développement (industrie)"="N0Z90",
                                   "N0Z91-Chercheurs (sauf industrie et enseignement supérieur)"="N0Z91",
                                   "P0Z60-Agents des impôts et des douanes"="P0Z60",
                                   "P0Z61-Employés des services au public"="P0Z61",
                                   "P0Z62-Employés de la Poste et des télécommunications"="P0Z62",
                                   "P1Z80-Contrôleurs des impôts et des douanes"="P1Z80",
                                   "P1Z81-Autres cadres B de la fonction publique"="P1Z81",
                                   "P1Z82-Professions intermédiaires de la Poste et des télécommunications"="P1Z82",
                                   "P2Z90-Cadres A de la fonction publique (hors spécialités juridiques) et assimilés"="P2Z90",
                                   "P2Z91-Cadres de la Poste et des télécommunications"="P2Z91",
                                   "P2Z92-Cadres de l'armée et de la gendarmerie"="P2Z92",
                                   "P3Z90-Professionnels du droit"="P3Z90",
                                   "P3Z91-Magistrats"="P3Z91",
                                   "P4Z60-Agents de sécurité et de l'ordre public"="P4Z60",
                                   "P4Z61-Agents de polices municipales"="P4Z61",
                                   "P4Z80-Cadres intermédiaires de la police et de l'armée"="P4Z80",
                                   "Q0Z60-Employés de la banque et des assurances"="Q0Z60",
                                   "Q1Z80-Techniciens de la banque"="Q1Z80",
                                   "Q1Z81-Techniciens des assurances"="Q1Z81",
                                   "Q2Z90-Cadres de la banque"="Q2Z90",
                                   "Q2Z91-Cadres des assurances"="Q2Z91",
                                   "R0Z60-Employés de libre service"="R0Z60",
                                   "R0Z61-Caissiers"="R0Z61",
                                   "R1Z60-Vendeurs en produits alimentaires"="R1Z60",
                                   "R1Z61-Vendeurs en ameublement, équipement du foyer, bricolage"="R1Z61",
                                   "R1Z62-Vendeurs en habillement et accessoires, articles de luxe, de sport, de loisirs et culturels"="R1Z62",
                                   "R1Z63-Vendeurs en gros de matériel et équipements"="R1Z63",
                                   "R1Z66-Vendeurs généralistes"="R1Z66",
                                   "R1Z67-Télévendeurs"="R1Z67",
                                   "R2Z80-Attachés commerciaux"="R2Z80",
                                   "R2Z83-Représentants auprès des particuliers"="R2Z83",
                                   "R3Z80-Maîtrise des magasins"="R3Z80",
                                   "R3Z81-Intermédiaires du commerce"="R3Z81",
                                   "R3Z82-Professions intermédiaires commerciales"="R3Z82",
                                   "R4Z90-Cadres commerciaux, acheteurs et cadres de la mercatique"="R4Z90",
                                   "R4Z91-Ingénieurs et cadres technico-commerciaux"="R4Z91",
                                   "R4Z92-Cadres des magasins"="R4Z92",
                                   "R4Z93-Agents immobiliers, syndics"="R4Z93",
                                   "S0Z20-Apprentis et ouvriers non qualifiés de l'alimentation (hors industries agro-alimentaires)"="S0Z20",
                                   "S0Z40-Bouchers"="S0Z40",
                                   "S0Z41-Charcutiers, traiteurs"="S0Z41",
                                   "S0Z42-Boulangers, pâtissiers"="S0Z42",
                                   "S1Z20-Aides de cuisine, apprentis de cuisine et employés polyvalents de la restauration"="S1Z20",
                                   "S1Z40-Cuisiniers"="S1Z40",
                                   "S1Z80-Chefs cuisiniers"="S1Z80",
                                   "S2Z60-Employés de l'hôtellerie"="S2Z60",
                                   "S2Z61-Serveurs de cafés restaurants"="S2Z61",
                                   "S2Z80-Maîtres d'hôtel"="S2Z80",
                                   "S2Z81-Maîtrise de l'hôtellerie"="S2Z81",
                                   "S3Z00-Patrons d'hôtels, cafés, restaurants"="S3Z00",
                                   "S3Z90-Cadres de l'hôtellerie et de la restauration"="S3Z90",
                                   "T0Z60-Coiffeurs, esthéticiens"="T0Z60",
                                   "T1Z60-Employés de maison et personnels de ménage"="T1Z60",
                                   "T2A60-Aides à domicile et aides ménagères"="T2A60",
                                   "T2B60-Assistantes maternelles"="T2B60",
                                   "T3Z60-Concierges"="T3Z60",
                                   "T3Z61-Agents de sécurité et de surveillance"="T3Z61",
                                   "T4Z60-Agents d'entretien de locaux"="T4Z60",
                                   "T4Z61-Agents de services hospitaliers"="T4Z61",
                                   "T4Z62-Ouvriers de l'assainissement et du traitement des déchets"="T4Z62",
                                   "T6Z61-Employés des services divers"="T6Z61",
                                   "U0Z80-Assistants de communication"="U0Z80",
                                   "U0Z81-Interprètes"="U0Z81",
                                   "U0Z90-Cadres de la communication"="U0Z90",
                                   "U0Z91-Cadres et techniciens de la documentation"="U0Z91",
                                   "U0Z92-Journalistes et cadres de l'édition"="U0Z92",
                                   "U1Z80-Professionnels des spectacles"="U1Z80",
                                   "U1Z81-Photographes"="U1Z81",
                                   "U1Z82-Graphistes, dessinateurs, stylistes, décorateurs et créateurs de supports de communication visuelle"="U1Z82",
                                   "U1Z91-Artistes (musique, danse, spectacles)"="U1Z91",
                                   "U1Z92-Écrivains"="U1Z92",
                                   "U1Z93-Artistes plasticiens"="U1Z93",
                                   "V0Z60-Aides-soignants"="V0Z60",
                                   "V1Z80-Infirmiers"="V1Z80",
                                   "V1Z81-Sages-femmes"="V1Z81",
                                   "V2Z90-Médecins"="V2Z90",
                                   "V2Z91-Dentistes"="V2Z91",
                                   "V2Z92-Vétérinaires"="V2Z92",
                                   "V2Z93-Pharmaciens"="V2Z93",
                                   "V3Z70-Techniciens médicaux et préparateurs"="V3Z70",
                                   "V3Z71-Spécialistes de l'appareillage médical"="V3Z71",
                                   "V3Z80-Autres professionnels para-médicaux"="V3Z80",
                                   "V3Z90-Psychologues, psychothérapeutes"="V3Z90",
                                   "V4Z80-Professionnels de l'orientation"="V4Z80",
                                   "V4Z83-Educateurs spécialisés"="V4Z83",
                                   "V4Z85-Professionnels de l'action sociale"="V4Z85",
                                   "V5Z00-Exploitants d'équipements sportifs et culturels"="V5Z00",
                                   "V5Z81-Professionnels de l'animation socioculturelle"="V5Z81",
                                   "V5Z82-Sportifs et animateurs sportifs"="V5Z82",
                                   "V5Z84-Surveillants d'établissements scolaires"="V5Z84",
                                   "W0Z80-Professeurs des écoles"="W0Z80",
                                   "W0Z90-Professeurs du secondaire"="W0Z90",
                                   "W0Z91-Directeurs d'établissement scolaire et inspecteurs"="W0Z91",
                                   "W0Z92-Professeurs du supérieur"="W0Z92",
                                   "W1Z80-Formateurs"="W1Z80",
                                   "X0Z00-Professionnels de la politique"="X0Z00",
                                   "X0Z01-Clergé"="X0Z01",
                                   "NULL" = "ZZZZZ") } 
if("FAP87" %in% names(ct) & !is.numeric(ct$FAP87) ) {
  ct$FAP87 <- forcats::fct_recode(ctraw$FAP87,
                                  
                                  "A0Z-Agriculteurs, éleveurs, sylviculteurs, bûcherons"="A0Z",
                                  "A1Z-Maraîchers, jardiniers, viticulteurs"="A1Z",
                                  "A2Z-Techniciens et cadres de l'agriculture"="A2Z",
                                  "A3Z-Marins, pêcheurs, aquaculteurs"="A3Z",
                                  "B0Z-Ouvriers non qualifiés du gros œuvre du bâtiment, des travaux publics, du béton et de l'extraction"="B0Z",
                                  "B1Z-Ouvriers qualifiés des travaux publics, du béton et de l'extraction"="B1Z",
                                  "B2Z-Ouvriers qualifiés du gros œuvre du bâtiment"="B2Z",
                                  "B3Z-Ouvriers non qualifiés du second œuvre du bâtiment"="B3Z",
                                  "B4Z-Ouvriers qualifiés du second œuvre du bâtiment"="B4Z",
                                  "B5Z-Conducteurs d'engins du bâtiment et des travaux publics"="B5Z",
                                  "B6Z-Techniciens et agents de maîtrise du bâtiment et des travaux publics"="B6Z",
                                  "B7Z-Cadres du bâtiment et des travaux publics"="B7Z",
                                  "C0Z-Ouvriers non qualifiés de l'électricité et de l'électronique"="C0Z",
                                  "C1Z-Ouvriers qualifiés de l'électricité et de l'électronique"="C1Z",
                                  "C2Z-Techniciens et agents de maîtrise de l'électricité et de l'électronique"="C2Z",
                                  "D0Z-Ouvriers non qualifiés travaillant par enlèvement ou formage de métal"="D0Z",
                                  "D1Z-Ouvriers qualifiés travaillant par enlèvement de métal"="D1Z",
                                  "D2Z-Ouvriers qualifiés travaillant par formage de métal"="D2Z",
                                  "D3Z-Ouvriers non qualifiés de la mécanique"="D3Z",
                                  "D4Z-Ouvriers qualifiés de la mécanique"="D4Z",
                                  "D6Z-Techniciens et agents de maîtrise des industries mécaniques"="D6Z",
                                  "E0Z-Ouvriers non qualifiés des industries de process"="E0Z",
                                  "E1Z-Ouvriers qualifiés des industries de process"="E1Z",
                                  "E2Z-Techniciens et agents de maîtrise des industries de process"="E2Z",
                                  "F0Z-Ouvriers non qualifiés du textile et du cuir"="F0Z",
                                  "F1Z-Ouvriers qualifiés du textile et du cuir"="F1Z",
                                  "F2Z-Ouvriers non qualifiés du travail du bois et de l'ameublement"="F2Z",
                                  "F3Z-Ouvriers qualifiés du travail du bois et de l'ameublement"="F3Z",
                                  "F4Z-Ouvriers des industries graphiques"="F4Z",
                                  "F5Z-Techniciens et agents de maîtrise des matériaux souples, du bois et des industries graphiques"="F5Z",
                                  "G0A-Ouvriers qualifiés de la maintenance"="G0A",
                                  "G0B-Ouvriers qualifiés de la réparation automobile"="G0B",
                                  "G1Z-Techniciens et agents de maîtrise de la maintenance"="G1Z",
                                  "H0Z-Ingénieurs et cadres techniques de l'industrie"="H0Z",
                                  "J0Z-Ouvriers non qualifiés de la manutention"="J0Z",
                                  "J1Z-Ouvriers qualifiés de la manutention"="J1Z",
                                  "J3Z-Conducteurs de véhicules"="J3Z",
                                  "J4Z-Agents d'exploitation des transports"="J4Z",
                                  "J5Z-Agents administratifs et commerciaux des transports et du tourisme"="J5Z",
                                  "J6Z-Cadres des transports, de la logistique et navigants de l'aviation"="J6Z",
                                  "K0Z-Artisans et ouvriers artisanaux"="K0Z",
                                  "L0Z-Secrétaires"="L0Z",
                                  "L1Z-Employés de la comptabilité"="L1Z",
                                  "L2Z-Employés administratifs d'entreprise"="L2Z",
                                  "L3Z-Secrétaires de direction"="L3Z",
                                  "L4Z-Techniciens des services administratifs, comptables et financiers"="L4Z",
                                  "L5Z-Cadres des services administratifs, comptables et financiers"="L5Z",
                                  "L6Z-Dirigeants d'entreprises"="L6Z",
                                  "M0Z-Employés et opérateurs de l'informatique"="M0Z",
                                  "M1Z-Techniciens de l'informatique"="M1Z",
                                  "M2Z-Ingénieurs de l'informatique"="M2Z",
                                  "N0Z-Personnels d'études et de recherche"="N0Z",
                                  "P0Z-Employés administratifs de la fonction publique (catégorie C et assimilés)"="P0Z",
                                  "P1Z-Professions intermédiaires administratives de la fonction publique (catégorie B et assimilés)"="P1Z",
                                  "P2Z-Cadres de la fonction publique (catégorie A et assimilés)"="P2Z",
                                  "P3Z-Professionnels du droit (hors juristes en entreprise)"="P3Z",
                                  "P4Z-Armée, police, pompiers"="P4Z",
                                  "Q0Z-Employés de la banque et des assurances"="Q0Z",
                                  "Q1Z-Techniciens de la banque et des assurances"="Q1Z",
                                  "Q2Z-Cadres de la banque et des assurances"="Q2Z",
                                  "R0Z-Caissiers, employés de libre service"="R0Z",
                                  "R1Z-Vendeurs"="R1Z",
                                  "R2Z-Attachés commerciaux et représentants"="R2Z",
                                  "R3Z-Maîtrise des magasins et intermédiaires du commerce"="R3Z",
                                  "R4Z-Cadres commerciaux et technico-commerciaux"="R4Z",
                                  "S0Z-Bouchers, charcutiers, boulangers"="S0Z",
                                  "S1Z-Cuisiniers"="S1Z",
                                  "S2Z-Employés et agents de maîtrise de l'hôtellerie et de la restauration"="S2Z",
                                  "S3Z-Patrons et cadres d'hôtels, cafés, restaurants"="S3Z",
                                  "T0Z-Coiffeurs, esthéticiens"="T0Z",
                                  "T1Z-Employés de maison"="T1Z",
                                  "T2A-Aides à domicile et aides ménagères"="T2A",
                                  "T2B-Assistantes maternelles"="T2B",
                                  "T3Z-Agents de gardiennage et de sécurité"="T3Z",
                                  "T4Z-Agents d'entretien"="T4Z",
                                  "T6Z-Employés des services divers"="T6Z",
                                  "U0Z-Professionnels de la communication et de l'information"="U0Z",
                                  "U1Z-Professionnels des arts et des spectacles"="U1Z",
                                  "V0Z-Aides-soignants"="V0Z",
                                  "V1Z-Infirmiers, sages-femmes"="V1Z",
                                  "V2Z-Médecins et assimilés"="V2Z",
                                  "V3Z-Professions para-médicales"="V3Z",
                                  "V4Z-Professionnels de l'action sociale et de l'orientation"="V4Z",
                                  "V5Z-Professionnels de l'action culturelle, sportive et surveillants"="V5Z",
                                  "W0Z-Enseignants"="W0Z",
                                  "W1Z-Formateurs"="W1Z",
                                  "X0Z-Professionnels de la politique et clergé"="X0Z",
                                  "NULL" = "ZZZ") } 
if("region" %in% names(ct) & !is.numeric(ct$region) ) {
  ct$region <- forcats::fct_recode(ctraw$region,
                                   
                                   "01-Guadeloupe"="01",
                                   "02-Martinique"="02",
                                   "03-Guyane"="03",
                                   "04-La Réunion"="04",
                                   "06-Mayotte"="06",
                                   "11-Île-de-France"="11",
                                   "21-Champagne-Ardenne"="21",
                                   "22-Picardie"="22",
                                   "23-Haute-Normandie"="23",
                                   "24-Centre"="24",
                                   "25-Basse-Normandie"="25",
                                   "26-Bourgogne"="26",
                                   "31-Nord-Pas-de-Calais"="31",
                                   "41-Lorraine"="41",
                                   "42-Alsace"="42",
                                   "43-Franche-Comté"="43",
                                   "52-Pays de la Loire"="52",
                                   "53-Bretagne"="53",
                                   "54-Poitou-Charentes"="54",
                                   "72-Aquitaine"="72",
                                   "73-Midi-Pyrénées"="73",
                                   "74-Limousin"="74",
                                   "82-Rhône-Alpes"="82",
                                   "83-Auvergne"="83",
                                   "91-Languedoc-Roussillon"="91",
                                   "93-Provence-Alpes-Côte d'Azur"="93",
                                   "94-Corse"="94",
                                   
  ) } 
if("nati" %in% names(ct) & !is.numeric(ct$nati) ) {
  ct$nati <- forcats::fct_recode(ctraw$nati,
                                 
                                 "11-Française"="11",
                                 "21-Algérienne"="21",
                                 "22-Marocaine"="22",
                                 "23-Tunisienne"="23",
                                 "24-Nationalité d'un autre pays d'Afrique"="24",
                                 "31-Vietnamienne, laotienne, cambodgienne"="31",
                                 "34-Turque"="34",
                                 "35-Nationalité d'un autre pays d'Asie"="35",
                                 "41-Portugaise"="41",
                                 "42-Espagnole"="42",
                                 "43-Italienne"="43",
                                 "44-Nationalité d'un autre pays de l'UE 15"="44",
                                 "45-Nationalité d'un autre pays de l'UE 27"="45",
                                 "46-Nationalité d'un autre pays d'Europe"="46",
                                 "51-Autres nationalités"="51",
                                 
  ) } 
if("lienmig" %in% names(ct) & !is.numeric(ct$lienmig) ) {
  ct$lienmig <- forcats::fct_recode(ctraw$lienmig,
                                    
                                    "1-Immigré"="1",
                                    "2-Deux parents immigrés"="2",
                                    "3-Un parent immigré"="3",
                                    "4-Autres situations"="4",
                                    
  ) } 
if("typmen5" %in% names(ct) & !is.numeric(ct$typmen5) ) {
  ct$typmen5 <- forcats::fct_recode(ctraw$typmen5,
                                    
                                    "1-Personne seule"="1",
                                    "2-Famille monoparentale"="2",
                                    "3-Couple sans enfant"="3",
                                    "4-Couple avec enfants"="4",
                                    "5-Ménage complexe"="5",
                                    
  ) } 
if("typmen15" %in% names(ct) & !is.numeric(ct$typmen15) ) {
  ct$typmen15 <- forcats::fct_recode(ctraw$typmen15,
                                     
                                     "10-Personne seule active"="10",
                                     "11-Personne seule inactive"="11",
                                     "21-Famille monoparentale, parent actif"="21",
                                     "22-Famille monoparentale, parent inactif et au moins un enfant actif"="22",
                                     "23-Famille monoparentale, tous inactifs"="23",
                                     "31-Couple sans enfant, un actif"="31",
                                     "32-Couple sans enfant, deux actifs"="32",
                                     "33-Couple sans enfant, tous inactifs"="33",
                                     "41-Couple avec enfant, un membre du couple actif"="41",
                                     "42-Couple avec enfant, deux membres du couple actif"="42",
                                     "43-Couple avec enfant, couple inactif et au moins un enfant actif"="43",
                                     "44-Couple avec enfant, tous inactif"="44",
                                     "51-Autre ménage, un actif"="51",
                                     "52-Autre ménage, deux actifs ou plus"="52",
                                     "53-Autre ménage, tous inactifs"="53",
                                     
  ) } 
if("typrepqaa" %in% names(ct) & !is.numeric(ct$typrepqaa) ) {
  ct$typrepqaa <- forcats::fct_recode(ctraw$typrepqaa,
                                      
                                      "1-Oui"="1",
                                      "2-Non"="2",
                                      
  ) } 
if("CHAMP_CT2005" %in% names(ct) & !is.numeric(ct$CHAMP_CT2005) ) {
  ct$CHAMP_CT2005 <- forcats::fct_recode(ctraw$CHAMP_CT2005,
                                         
                                         "1-Champ 2005"="1",
                                         "2-Pas champ 2005"="2",
                                         
  ) } 
if("NAIP" %in% names(ct) & !is.numeric(ct$NAIP) ) {
  ct$NAIP <- forcats::fct_recode(ctraw$NAIP,
                                 
                                 "1-Père né en France"="1",
                                 "2-Père né à l’étranger"="2",
                                 "NULL"="8",
                                 "NULL"="9",
                                 
  ) } 
if("NAIM" %in% names(ct) & !is.numeric(ct$NAIM) ) {
  ct$NAIM <- forcats::fct_recode(ctraw$NAIM,
                                 
                                 "1-Mère née en France"="1",
                                 "2-Mère née à l’étranger"="2",
                                 "NULL"="8",
                                 "NULL"="9",
                                 
  ) } 
if("ACTIP" %in% names(ct) & !is.numeric(ct$ACTIP) ) {
  ct$ACTIP <- forcats::fct_recode(ctraw$ACTIP,
                                  
                                  "1-Père travaillait"="1",
                                  "2-Père chômage"="2",
                                  "3-Père retraité"="3",
                                  "4-Père inactif"="4",
                                  "5-Père jamais travaillé"="5",
                                  "6-Père décédé"="6",
                                  "NULL"="8",
                                  "NULL"="9",
                                  
  ) } 
if("STATUTP" %in% names(ct) & !is.numeric(ct$STATUTP) ) {
  ct$STATUTP <- forcats::fct_recode(ctraw$STATUTP,
                                    
                                    "1-Père à son compte"="1",
                                    "2-Père aide familial"="2",
                                    "3-Père administration publique"="3",
                                    "4-Père autre salarié"="4",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("ACTIM" %in% names(ct) & !is.numeric(ct$ACTIM) ) {
  ct$ACTIM <- forcats::fct_recode(ctraw$ACTIM,
                                  
                                  "1-Mère travaillait"="1",
                                  "2-Mère chômage"="2",
                                  "3-Mère retraitée"="3",
                                  "4-Mère inactive"="4",
                                  "5-Mère jamais travaillée"="5",
                                  "6-Mère décédée"="6",
                                  "NULL"="8",
                                  "NULLs"="9",
                                  
  ) } 
if("STATUTM" %in% names(ct) & !is.numeric(ct$STATUTM) ) {
  ct$STATUTM <- forcats::fct_recode(ctraw$STATUTM,
                                    
                                    "1-Mère à son compte"="1",
                                    "2-Mère aide familiale"="2",
                                    "3-Mère administration publique"="3",
                                    "4-Mère autre salariée"="4",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("TCHOLE" %in% names(ct) & !is.numeric(ct$TCHOLE) ) {
  ct$TCHOLE <- forcats::fct_recode(ctraw$TCHOLE,
                                   
                                   "1-1 fois chômage>1an"="1",
                                   "2-x fois chômage>1an"="2",
                                   "3-Jm chômage>1an"="3",
                                   "NULL"="8",
                                   "3-Jm chômage>1an"="9",
                                   
  ) } 
if("TCHOCE" %in% names(ct) & !is.numeric(ct$TCHOCE) ) {
  ct$TCHOCE <- forcats::fct_recode(ctraw$TCHOCE,
                                   
                                   "1-1 fois chômage>3m"="1",
                                   "2-x fois chômage>3m"="2",
                                   "3-Jm chômage>3m"="3",
                                   "NULL"="8",
                                   "3-Jm chômage>3m"="9",
                                   
  ) } 
if("TSANE" %in% names(ct) & !is.numeric(ct$TSANE) ) {
  ct$TSANE <- forcats::fct_recode(ctraw$TSANE,
                                  
                                  "1-1 fois arrêt>1an santé"="1",
                                  "2-x fois arrêt>1an santé"="2",
                                  "3-Jm arrêt>1an santé"="3",
                                  "NULL"="8",
                                  "3-Jm arrêt>1an santé"="9",
                                  
  ) } 
if("TINAE" %in% names(ct) & !is.numeric(ct$TINAE) ) {
  ct$TINAE <- forcats::fct_recode(ctraw$TINAE,
                                  
                                  "1-1 fois arrêt>1an autres"="1",
                                  "2-x fois arrêt>1an autres"="2",
                                  "3-Jm arrêt>1an autres"="3",
                                  "NULL"="8",
                                  "3-Jm arrêt>1an autres"="9",
                                  
  ) } 
if("TINDEPE" %in% names(ct) & !is.numeric(ct$TINDEPE) ) {
  ct$TINDEPE <- forcats::fct_recode(ctraw$TINDEPE,
                                    
                                    "1-Tj ou presque à son compte"="1",
                                    "2-Pf à son compte"="2",
                                    "3-Jm à son compte"="3",
                                    "NULL"="8",
                                    "3-Jm à son compte"="9",
                                    
  ) } 
if("TINTERIME" %in% names(ct) & !is.numeric(ct$TINTERIME) ) {
  ct$TINTERIME <- forcats::fct_recode(ctraw$TINTERIME,
                                      
                                      "1-Tj ou presque intérim"="1",
                                      "2-Pf intérim"="2",
                                      "3-Jm intérim"="3",
                                      "NULL"="8",
                                      "3-Jm intérim"="9",
                                      
  ) } 
if("TCDDE" %in% names(ct) & !is.numeric(ct$TCDDE) ) {
  ct$TCDDE <- forcats::fct_recode(ctraw$TCDDE,
                                  
                                  "1-Tj ou presque CDD"="1",
                                  "2-Pf CDD"="2",
                                  "3-Jm CDD"="3",
                                  "NULL"="8",
                                  "3-Jm CDD"="9",
                                  
  ) } 
# if("NBEMPE" %in% names(ct) & !is.numeric(ct$NBEMPE) ) {
#   ct$NBEMPE <- forcats::fct_recode(ctraw$NBEMPE,
#                                    
#                                    "0-0 employeur"="0",
#                                    "1-1 employeur"="1",
#                                    "10-10+ employeurs"="10",
#                                    "2-2 employeurs"="2",
#                                    "3-3 employeurs"="3",
#                                    "4-4 employeurs"="4",
#                                    "5-5 employeurs"="5",
#                                    "6-6 employeurs"="6",
#                                    "7-7 employeurs"="7",
#                                    "8-8 employeurs"="8",
#                                    "9-9 employeurs"="9",
#                                    
#   ) } 
if("PROFPER" %in% names(ct) & !is.numeric(ct$PROFPER) ) {
  ct$PROFPER <- forcats::fct_recode(ctraw$PROFPER,
                                    
                                    "111A-Agriculteurs sur petite exploitation de céréales-grandes cultures"="111A",
                                    "111B-Maraîchers, horticulteurs sur petite exploitation"="111B",
                                    "111C-Viticulteurs, arboriculteurs fruitiers, sur petite exploitation"="111C",
                                    "111D-Éleveurs d'herbivores, sur petite exploitation"="111D",
                                    "111E-Éleveurs de granivores et éleveurs mixtes, sur petite exploitation"="111E",
                                    "111F-Agriculteurs sur petite exploitation sans orientation dominante"="111F",
                                    "121A-Agriculteurs sur moyenne exploitation de céréales-grandes cultures"="121A",
                                    "121B-Maraîchers, horticulteurs sur moyenne exploitation"="121B",
                                    "121C-Viticulteurs, arboriculteurs fruitiers, sur moyenne exploitation"="121C",
                                    "121D-Éleveurs d'herbivores sur moyenne exploitation"="121D",
                                    "121E-Éleveurs de granivores et éleveurs mixtes, sur moyenne exploitation"="121E",
                                    "121F-Agriculteurs sur moyenne exploitation sans orientation dominante"="121F",
                                    "122A-Entrepreneurs de travaux agricoles à façon, de 0 à 9 salariés"="122A",
                                    "122B-Exploitants forestiers indépendants, de 0 à 9 salariés"="122B",
                                    "122C-Patrons pêcheurs et aquaculteurs, de 0 à 9 salariés"="122C",
                                    "131A-Agriculteurs sur grande exploitation de céréales-grandes cultures"="131A",
                                    "131B-Maraîchers, horticulteurs, sur grande exploitation"="131B",
                                    "131C-Viticulteurs, arboriculteurs fruitiers, sur grande exploitation"="131C",
                                    "131D-Éleveurs d'herbivores, sur grande exploitation"="131D",
                                    "131E-Éleveurs de granivores et éleveurs mixtes, sur grande exploitation"="131E",
                                    "131F-Agriculteurs sur grande exploitation sans orientation dominante"="131F",
                                    "211A-Artisans maçons"="211A",
                                    "211B-Artisans menuisiers du bâtiment, charpentiers en bois"="211B",
                                    "211C-Artisans couvreurs"="211C",
                                    "211D-Artisans plombiers, chauffagistes"="211D",
                                    "211E-Artisans électriciens du bâtiment"="211E",
                                    "211F-Artisans de la peinture et des finitions du bâtiment"="211F",
                                    "211G-Artisans serruriers, métalliers"="211G",
                                    "211H-Artisans en terrassement, travaux publics"="211H",
                                    "211J-Entrepreneurs en parcs et jardins, paysagistes"="211J",
                                    "212A-Artisans mécaniciens en machines agricoles"="212A",
                                    "212B-Artisans chaudronniers"="212B",
                                    "212C-Artisans en mécanique générale, fabrication et travail des métaux (hors horlogerie et matériel de précision)"="212C",
                                    "212D-Artisans divers de fabrication de machines"="212D",
                                    "213A-Artisans de l'habillement, du textile et du cuir"="213A",
                                    "214A-Artisans de l'ameublement"="214A",
                                    "214B-Artisans du travail mécanique du bois"="214B",
                                    "214C-Artisans du papier, de l'imprimerie et de la reproduction"="214C",
                                    "214D-Artisans de fabrication en matériaux de construction (hors artisanat d'art)"="214D",
                                    "214E-Artisans d'art"="214E",
                                    "214F-Autres artisans de fabrication (y.c. horlogers, matériel de précision)"="214F",
                                    "215A-Artisans boulangers, pâtissiers, de 0 à 9 salariés"="215A",
                                    "215B-Artisans bouchers, de 0 à 9 salariés"="215B",
                                    "215C-Artisans charcutiers, de 0 à 9 salariés"="215C",
                                    "215D-Autres artisans de l'alimentation, de 0 à 9 salariés"="215D",
                                    "216A-Artisans mécaniciens réparateurs d'automobiles"="216A",
                                    "216B-Artisans tôliers-carrossiers d'automobiles"="216B",
                                    "216C-Artisans réparateurs divers"="216C",
                                    "217A-Conducteurs de taxis, ambulanciers et autres artisans du transport, de 0 à 9 salariés"="217A",
                                    "217B-Artisans déménageurs, de 0 à 9 salariés"="217B",
                                    "217C-Artisans coiffeurs, manucures, esthéticiens, de 0 à 9 salariés"="217C",
                                    "217D-Artisans teinturiers, blanchisseurs, de 0 à 9 salariés"="217D",
                                    "217E-Artisans des services divers, de 0 à 9 salariés"="217E",
                                    "218A-Transporteurs indépendants routiers et fluviaux, de 0 à 9 salariés"="218A",
                                    "219A-Aides familiaux non salariés ou associés d'artisans, effectuant un travail administratif ou commercial"="219A",
                                    "221A-Petits et moyens grossistes en alimentation, de 0 à 9 salariés"="221A",
                                    "221B-Petits et moyens grossistes en produits non alimentaires, de 0 à 9 salariés"="221B",
                                    "222A-Petits et moyens détaillants en alimentation spécialisée, de 0 à 9 salariés"="222A",
                                    "222B-Petits et moyens détaillants en alimentation générale, de 0 à 9 salariés"="222B",
                                    "223A-Détaillants en ameublement, décor, équipement du foyer, de 0 à 9 salariés"="223A",
                                    "223B-Détaillants en droguerie, bazar, quincaillerie, bricolage, de 0 à 9 salariés"="223B",
                                    "223C-Fleuristes, de 0 à 9 salariés"="223C",
                                    "223D-Détaillants en habillement et articles de sport, de 0 à 9 salariés"="223D",
                                    "223E-Détaillants en produits de beauté, de luxe (hors biens culturels), de 0 à 9 salariés"="223E",
                                    "223F-Détaillants en biens culturels (livres, disques, multimédia, objets d'art), de 0 à 9 salariés"="223F",
                                    "223G-Détaillants en tabac, presse et articles divers, de 0 à 9 salariés"="223G",
                                    "223H-Exploitants et gérants libres de station-service, de 0 à 9 salariés"="223H",
                                    "224A-Exploitants de petit restaurant, café-restaurant, de 0 à 2 salariés"="224A",
                                    "224B-Exploitants de petit café, débit de boisson, associé ou non à une autre activité hors restauration, de 0 à 2 salariés"="224B",
                                    "224C-Exploitants de petit hôtel, hôtel-restaurant, de 0 à 2 salariés"="224C",
                                    "224D-Exploitants de café, restaurant, hôtel, de 3 à 9 salariés"="224D",
                                    "225A-Intermédiaires indépendants du commerce, de 0 à 9 salariés"="225A",
                                    "226A-Agents généraux et courtiers d'assurance indépendants, de 0 à 9 salariés"="226A",
                                    "226B-Agents de voyage et auxiliaires de transports indépendants, de 0 à 9 salariés"="226B",
                                    "226C-Agents immobiliers indépendants, de 0 à 9 salariés"="226C",
                                    "227A-Indépendants gestionnaires de spectacle ou de service récréatif, de 0 à 9 salariés"="227A",
                                    "227B-Indépendants gestionnaires d'établissements privés (enseignement, santé, social), de 0 à 9 salariés"="227B",
                                    "227C-Astrologues, professionnels de la parapsychologie, guérisseurs, de 0 à 9 salariés"="227C",
                                    "227D-Autres indépendants divers prestataires de services, de 0 à 9 salariés"="227D",
                                    "231A-Chefs de grande entreprise de 500 salariés et plus"="231A",
                                    "232A-Chefs de moyenne entreprise, de 50 à 499 salariés"="232A",
                                    "233A-Chefs d'entreprise du bâtiment et des travaux publics, de 10 à 49 salariés"="233A",
                                    "233B-Chefs d'entreprise de l'industrie ou des transports, de 10 à 49 salariés"="233B",
                                    "233C-Chefs d'entreprise commerciale, de 10 à 49 salariés"="233C",
                                    "233D-Chefs d'entreprise de services, de 10 à 49 salariés"="233D",
                                    "311A-Médecins libéraux spécialistes"="311A",
                                    "311B-Médecins libéraux généralistes"="311B",
                                    "311C-Chirurgiens dentistes (libéraux ou salariés)"="311C",
                                    "311D-Psychologues, psychanalystes, psychothérapeutes (non médecins)"="311D",
                                    "311E-Vétérinaires (libéraux ou salariés)"="311E",
                                    "311F-Pharmaciens libéraux"="311F",
                                    "312A-Avocats"="312A",
                                    "312B-Notaires"="312B",
                                    "312C-Experts comptables, comptables agréés, libéraux"="312C",
                                    "312D-Conseils et experts libéraux en études économiques, organisation et recrutement, gestion et fiscalité"="312D",
                                    "312E-Ingénieurs conseils libéraux en études techniques"="312E",
                                    "312F-Architectes libéraux"="312F",
                                    "312G-Géomètres-experts, huissiers de justice, officiers ministériels, professions libérales diverses"="312G",
                                    "313A-Aides familiaux non salariés de professions libérales effectuant un travail administratif"="313A",
                                    "331A-Personnels de direction de la fonction publique (État, collectivités locales, hôpitaux)"="331A",
                                    "332A-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés"="332A",
                                    "332B-Ingénieurs des collectivités locales et des hôpitaux"="332B",
                                    "333A-Magistrats"="333A",
                                    "333B-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes"="333B",
                                    "333C-Cadres de la Poste"="333C",
                                    "333D-Cadres administratifs de France Télécom (statut public)"="333D",
                                    "333E-Autres personnels administratifs de catégorie A de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="333E",
                                    "333F-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)"="333F",
                                    "334A-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)"="334A",
                                    "335A-Personnes exerçant un mandat politique ou syndical"="335A",
                                    "341A-Professeurs agrégés et certifiés de l'enseignement secondaire"="341A",
                                    "341B-Chefs d'établissement de l'enseignement secondaire et inspecteurs"="341B",
                                    "342A-Enseignants de l'enseignement supérieur"="342A",
                                    "342E-Chercheurs de la recherche publique"="342E",
                                    "343A-Psychologues spécialistes de l'orientation scolaire et professionnelle"="343A",
                                    "344A-Médecins hospitaliers sans activité libérale"="344A",
                                    "344B-Médecins salariés non hospitaliers"="344B",
                                    "344C-Internes en médecine, odontologie et pharmacie"="344C",
                                    "344D-Pharmaciens salariés"="344D",
                                    "351A-Bibliothécaires, archivistes, conservateurs et autres cadres du patrimoine (fonction publique)"="351A",
                                    "352A-Journalistes (y. c. rédacteurs en chef)"="352A",
                                    "352B-Auteurs littéraires, scénaristes, dialoguistes"="352B",
                                    "353A-Directeurs de journaux, administrateurs de presse, directeurs d'éditions (littéraire, musicale, audiovisuelle et multimédia)"="353A",
                                    "353B-Directeurs, responsables de programmation et de production de l'audiovisuel et des spectacles"="353B",
                                    "353C-Cadres artistiques et technico-artistiques de la réalisation de l'audiovisuel et des spectacles"="353C",
                                    "354A-Artistes plasticiens"="354A",
                                    "354B-Artistes+B493 de la musique et du chant"="354B",
                                    "354C-Artistes dramatiques"="354C",
                                    "354D-Artistes de la danse, du cirque et des spectacles divers"="354D",
                                    "354G-Professeurs d'art (hors établissements scolaires)"="354G",
                                    "371A-Cadres d'état-major administratifs, financiers, commerciaux des grandes entreprises"="371A",
                                    "372A-Cadres chargés d'études économiques, financières, commerciales"="372A",
                                    "372B-Cadres de l'organisation ou du contrôle des services administratifs et financiers"="372B",
                                    "372C-Cadres spécialistes des ressources humaines et du recrutement"="372C",
                                    "372D-Cadres spécialistes de la formation"="372D",
                                    "372E-Juristes"="372E",
                                    "372F-Cadres de la documentation, de l'archivage (hors fonction publique)"="372F",
                                    "373A-Cadres des services financiers ou comptables des grandes entreprises"="373A",
                                    "373B-Cadres des autres services administratifs des grandes entreprises"="373B",
                                    "373C-Cadres des services financiers ou comptables des petites et moyennes entreprises"="373C",
                                    "373D-Cadres des autres services administratifs des petites et moyennes entreprises"="373D",
                                    "374A-Cadres de l'exploitation des magasins de vente du commerce de détail"="374A",
                                    "374B-Chefs de produits, acheteurs du commerce et autres cadres de la mercatique"="374B",
                                    "374C-Cadres commerciaux des grandes entreprises (hors commerce de détail)"="374C",
                                    "374D-Cadres commerciaux des petites et moyennes entreprises (hors commerce de détail)"="374D",
                                    "375A-Cadres de la publicité"="375A",
                                    "375B-Cadres des relations publiques et de la communication"="375B",
                                    "376A-Cadres des marchés financiers"="376A",
                                    "376B-Cadres des opérations bancaires"="376B",
                                    "376C-Cadres commerciaux de la banque"="376C",
                                    "376D-Chefs d'établissements et responsables de l'exploitation bancaire"="376D",
                                    "376E-Cadres des services techniques des assurances"="376E",
                                    "376F-Cadres des services techniques des organismes de sécurité sociale et assimilés"="376F",
                                    "376G-Cadres de l'immobilier"="376G",
                                    "377A-Cadres de l'hôtellerie et de la restauration"="377A",
                                    "380A-Directeurs techniques des grandes entreprises"="380A",
                                    "381A-Ingénieurs et cadres d'étude et d'exploitation de l'agriculture, la pêche, les eaux et forêts"="381A",
                                    "382A-Ingénieurs et cadres d'étude du bâtiment et des travaux publics"="382A",
                                    "382B-Architectes salariés"="382B",
                                    "382C-Ingénieurs, cadres de chantier et conducteurs de travaux (cadres) du bâtiment et des travaux publics"="382C",
                                    "382D-Ingénieurs et cadres technico-commerciaux en bâtiment, travaux publics"="382D",
                                    "383A-Ingénieurs et cadres d'étude, recherche et développement en électricité, électronique"="383A",
                                    "383B-Ingénieurs et cadres de fabrication en matériel électrique, électronique"="383B",
                                    "383C-Ingénieurs et cadres technico-commerciaux en matériel électrique ou électronique professionnel"="383C",
                                    "384A-Ingénieurs et cadres d'étude, recherche et développement en mécanique et travail des métaux"="384A",
                                    "384B-Ingénieurs et cadres de fabrication en mécanique et travail des métaux"="384B",
                                    "384C-Ingénieurs et cadres technico-commerciaux en matériel mécanique professionnel"="384C",
                                    "385A-Ingénieurs et cadres d'étude, recherche et développement des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385A",
                                    "385B-Ingénieurs et cadres de fabrication des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385B",
                                    "385C-Ingénieurs et cadres technico-commerciaux des industries de transformations (biens intermédiaires)"="385C",
                                    "386A-Ingénieurs et cadres d'étude, recherche et développement des autres industries (imprimerie, matériaux souples, ameublement et bois, énergie, eau)"="386A",
                                    "386D-Ingénieurs et cadres de la production et de la distribution d'énergie, eau"="386D",
                                    "386E-Ingénieurs et cadres de fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="386E",
                                    "387A-Ingénieurs et cadres des achats et approvisionnements industriels"="387A",
                                    "387B-Ingénieurs et cadres de la logistique, du planning et de l'ordonnancement"="387B",
                                    "387C-Ingénieurs et cadres des méthodes de production"="387C",
                                    "387D-Ingénieurs et cadres du contrôle-qualité"="387D",
                                    "387E-Ingénieurs et cadres de la maintenance, de l'entretien et des travaux neufs"="387E",
                                    "387F-Ingénieurs et cadres techniques de l'environnement"="387F",
                                    "388A-Ingénieurs et cadres d'étude, recherche et développement en informatique"="388A",
                                    "388B-Ingénieurs et cadres d'administration, maintenance, support et services aux utilisateurs en informatique"="388B",
                                    "388C-Chefs de projets informatiques, responsables informatiques"="388C",
                                    "388D-Ingénieurs et cadres technico-commerciaux en informatique et télécommunications"="388D",
                                    "388E-Ingénieurs et cadres spécialistes des télécommunications"="388E",
                                    "389A-Ingénieurs et cadres techniques de l'exploitation des transports"="389A",
                                    "389B-Officiers et cadres navigants techniques et commerciaux de l'aviation civile"="389B",
                                    "389C-Officiers et cadres navigants techniques de la marine marchande"="389C",
                                    "421A-Instituteurs"="421A",
                                    "421B-Professeurs des écoles"="421B",
                                    "422A-Professeurs d'enseignement général des collèges"="422A",
                                    "422B-Professeurs de lycée professionnel"="422B",
                                    "422C-Maîtres auxiliaires et professeurs contractuels de l'enseignement secondaire"="422C",
                                    "422D-Conseillers principaux d'éducation"="422D",
                                    "422E-Surveillants et aides-éducateurs des établissements d'enseignement"="422E",
                                    "423A-Moniteurs d'école de conduite"="423A",
                                    "423B-Formateurs et animateurs de formation continue"="423B",
                                    "424A-Moniteurs et éducateurs sportifs, sportifs professionnels"="424A",
                                    "425A-Sous-bibliothécaires, cadres intermédiaires du patrimoine"="425A",
                                    "431A-Cadres infirmiers et assimilés"="431A",
                                    "431B-Infirmiers psychiatriques"="431B",
                                    "431C-Puéricultrices"="431C",
                                    "431D-Infirmiers spécialisés (autres qu'infirmiers psychiatriques et puéricultrices)"="431D",
                                    "431E-Sages-femmes (libérales ou salariées)"="431E",
                                    "431F-Infirmiers en soins généraux, salariés"="431F",
                                    "431G-Infirmiers libéraux"="431G",
                                    "432A-Masseurs-kinésithérapeutes rééducateurs, libéraux"="432A",
                                    "432B-Masseurs-kinésithérapeutes rééducateurs, salariés"="432B",
                                    "432C-Autres spécialistes de la rééducation, libéraux"="432C",
                                    "432D-Autres spécialistes de la rééducation, salariés"="432D",
                                    "433A-Techniciens médicaux"="433A",
                                    "433B-Opticiens lunetiers et audioprothésistes (indépendants et salariés)"="433B",
                                    "433C-Autres spécialistes de l'appareillage médical (indépendants et salariés)"="433C",
                                    "433D-Préparateurs en pharmacie"="433D",
                                    "434A-Cadres de l'intervention socio-éducative"="434A",
                                    "434B-Assistants de service social"="434B",
                                    "434C-Conseillers en économie sociale familiale"="434C",
                                    "434D-Éducateurs spécialisés"="434D",
                                    "434E-Moniteurs éducateurs"="434E",
                                    "434F-Éducateurs techniques spécialisés, moniteurs d'atelier"="434F",
                                    "434G-Éducateurs de jeunes enfants"="434G",
                                    "435A-Directeurs de centres socioculturels et de loisirs"="435A",
                                    "435B-Animateurs socioculturels et de loisirs"="435B",
                                    "441A-Clergé séculier"="441A",
                                    "441B-Clergé régulier"="441B",
                                    "451A-Professions intermédiaires de la Poste"="451A",
                                    "451B-Professions intermédiaires administratives de France Télécom (statut public)"="451B",
                                    "451C-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés"="451C",
                                    "451D-Ingénieurs du contrôle de la navigation aérienne"="451D",
                                    "451E-Autres personnels administratifs de catégorie B de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="451E",
                                    "451F-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)"="451F",
                                    "452A-Inspecteurs et officiers de police"="452A",
                                    "452B-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie"="452B",
                                    "461A-Personnel de secrétariat de niveau supérieur, secrétaires de direction (non cadres)"="461A",
                                    "461D-Maîtrise et techniciens des services financiers ou comptables"="461D",
                                    "461E-Maîtrise et techniciens administratifs des services juridiques ou du personnel"="461E",
                                    "461F-Maîtrise et techniciens administratifs des autres services administratifs"="461F",
                                    "462A-Chefs de petites surfaces de vente (salariés ou mandataires)"="462A",
                                    "462B-Maîtrise de l'exploitation des magasins de vente"="462B",
                                    "462C-Acheteurs non classés cadres, aides-acheteurs"="462C",
                                    "462D-Animateurs commerciaux des magasins de vente, marchandiseurs (non cadres)"="462D",
                                    "462E-Autres professions intermédiaires commerciales (sauf techniciens des forces de vente)"="462E",
                                    "463A-Techniciens commerciaux et technico-commerciaux, représentants en informatique"="463A",
                                    "463B-Techniciens commerciaux et technico-commerciaux, représentants en biens d'équipement, en biens intermédiaires, commerce interindustriel (hors informatique)"="463B",
                                    "463C-Techniciens commerciaux et technico-commerciaux, représentants en biens de consommation auprès d'entreprises"="463C",
                                    "463D-Techniciens commerciaux et technico-commerciaux, représentants en services auprès d'entreprises ou de professionnels (hors banque, assurance, informatique)"="463D",
                                    "463E-Techniciens commerciaux et technico-commerciaux, représentants auprès de particuliers (hors banque, assurance, informatique)"="463E",
                                    "464A-Assistants de la publicité, des relations publiques (indépendants ou salariés)"="464A",
                                    "464B-Interprètes, traducteurs (indépendants ou salariés)"="464B",
                                    "465A-Concepteurs et assistants techniques des arts graphiques, de la mode et de la décoration (indépendants et salariés)"="465A",
                                    "465B-Assistants techniques de la réalisation des spectacles vivants et audiovisuels (indépendants ou salariés)"="465B",
                                    "465C-Photographes (indépendants et salariés)"="465C",
                                    "466A-Responsables commerciaux et administratifs des transports de voyageurs et du tourisme (non cadres)"="466A",
                                    "466B-Responsables commerciaux et administratifs des transports de marchandises (non cadres)"="466B",
                                    "466C-Responsables d'exploitation des transports de voyageurs et de marchandises (non cadres)"="466C",
                                    "467A-Chargés de clientèle bancaire"="467A",
                                    "467B-Techniciens des opérations bancaires"="467B",
                                    "467C-Professions intermédiaires techniques et commerciales des assurances"="467C",
                                    "467D-Professions intermédiaires techniques des organismes de sécurité sociale"="467D",
                                    "468A-Maîtrise de restauration : salle et service"="468A",
                                    "468B-Maîtrise de l'hébergement : hall et étages"="468B",
                                    "471A-Techniciens d'étude et de conseil en agriculture, eaux et forêt"="471A",
                                    "471B-Techniciens d'exploitation et de contrôle de la production en agriculture, eaux et forêt"="471B",
                                    "472A-Dessinateurs en bâtiment, travaux publics"="472A",
                                    "472B-Géomètres, topographes"="472B",
                                    "472C-Métreurs et techniciens divers du bâtiment et des travaux publics"="472C",
                                    "472D-Techniciens des travaux publics de l'État et des collectivités locales"="472D",
                                    "473A-Dessinateurs en électricité, électromécanique et électronique"="473A",
                                    "473B-Techniciens de recherche-développement et des méthodes de fabrication en électricité, électromécanique et électronique"="473B",
                                    "473C-Techniciens de fabrication et de contrôle-qualité en électricité, électromécanique et électronique"="473C",
                                    "474A-Dessinateurs en construction mécanique et travail des métaux"="474A",
                                    "474B-Techniciens de recherche-développement et des méthodes de fabrication en construction mécanique et travail des métaux"="474B",
                                    "474C-Techniciens de fabrication et de contrôle-qualité en construction mécanique et travail des métaux"="474C",
                                    "475A-Techniciens de recherche-développement et des méthodes de production des industries de transformation"="475A",
                                    "475B-Techniciens de production et de contrôle-qualité des industries de transformation"="475B",
                                    "476A-Assistants techniques, techniciens de l'imprimerie et de l'édition"="476A",
                                    "476B-Techniciens de l'industrie des matériaux souples, de l'ameublement et du bois"="476B",
                                    "477A-Techniciens de la logistique, du planning et de l'ordonnancement"="477A",
                                    "477B-Techniciens d'installation et de maintenance des équipements industriels (électriques, électromécaniques, mécaniques, hors informatique)"="477B",
                                    "477C-Techniciens d'installation et de maintenance des équipements non industriels (hors informatique et télécommunications)"="477C",
                                    "477D-Techniciens de l'environnement et du traitement des pollutions"="477D",
                                    "478A-Techniciens d'étude et de développement en informatique"="478A",
                                    "478B-Techniciens de production, d'exploitation en informatique"="478B",
                                    "478C-Techniciens d'installation, de maintenance, support et services aux utilisateurs en informatique"="478C",
                                    "478D-Techniciens des télécommunications et de l'informatique des réseaux"="478D",
                                    "479A-Techniciens des laboratoires de recherche publique ou de l'enseignement"="479A",
                                    "479B-Experts salariés ou indépendants de niveau technicien, techniciens divers"="479B",
                                    "480A-Contremaîtres et agents d'encadrement (non cadres) en agriculture, sylviculture"="480A",
                                    "480B-Maîtres d'équipage de la marine marchande et de la pêche"="480B",
                                    "481A-Conducteurs de travaux (non cadres)"="481A",
                                    "481B-Chefs de chantier (non cadres)"="481B",
                                    "482A-Agents de maîtrise en fabrication de matériel électrique, électronique"="482A",
                                    "483A-Agents de maîtrise en construction mécanique, travail des métaux"="483A",
                                    "484A-Agents de maîtrise en fabrication : agroalimentaire, chimie, plasturgie, pharmacie."="484A",
                                    "484B-Agents de maîtrise en fabrication : métallurgie, matériaux lourds et autres industries de transformation"="484B",
                                    "485A-Agents de maîtrise et techniciens en production et distribution d'énergie, eau, chauffage"="485A",
                                    "485B-Agents de maîtrise en fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="485B",
                                    "486A-Agents de maîtrise en maintenance, installation en électricité, électromécanique et électronique"="486A",
                                    "486D-Agents de maîtrise en maintenance, installation en mécanique"="486D",
                                    "486E-Agents de maîtrise en entretien général, installation, travaux neufs (hors mécanique, électromécanique, électronique)"="486E",
                                    "487A-Responsables d'entrepôt, de magasinage"="487A",
                                    "487B-Responsables du tri, de l'emballage, de l'expédition et autres responsables de la manutention"="487B",
                                    "488A-Maîtrise de restauration : cuisine/production"="488A",
                                    "488B-Maîtrise de restauration : gestion d'établissement"="488B",
                                    "521A-Employés de la Poste"="521A",
                                    "521B-Employés de France Télécom (statut public)"="521B",
                                    "522A-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes"="522A",
                                    "523A-Adjoints administratifs de la fonction publique (y.c. enseignement)"="523A",
                                    "524A-Agents administratifs de la fonction publique (y.c. enseignement)"="524A",
                                    "525A-Agents de service des établissements primaires"="525A",
                                    "525B-Agents de service des autres établissements d'enseignement"="525B",
                                    "525C-Agents de service de la fonction publique (sauf écoles, hôpitaux)"="525C",
                                    "525D-Agents de service hospitaliers (de la fonction publique ou du secteur privé)"="525D",
                                    "526A-Aides-soignants (de la fonction publique ou du secteur privé)"="526A",
                                    "526B-Assistants dentaires, médicaux et vétérinaires, aides de techniciens médicaux"="526B",
                                    "526C-Auxiliaires de puériculture"="526C",
                                    "526D-Aides médico-psychologiques"="526D",
                                    "526E-Ambulanciers salariés (du secteur public ou du secteur privé)"="526E",
                                    "531A-Agents de police de l'État"="531A",
                                    "531B-Agents des polices municipales"="531B",
                                    "531C-Surveillants de l'administration pénitentiaire"="531C",
                                    "532A-Gendarmes (de grade inférieur à adjudant)"="532A",
                                    "532B-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)"="532B",
                                    "532C-Hommes du rang (sauf pompiers militaires)"="532C",
                                    "533A-Pompiers (y.c. pompiers militaires)"="533A",
                                    "533B-Agents techniques forestiers, gardes des espaces naturels"="533B",
                                    "533C-Agents de surveillance du patrimoine et des administrations"="533C",
                                    "534A-Agents civils de sécurité et de surveillance"="534A",
                                    "534B-Convoyeurs de fonds, gardes du corps, enquêteurs privés et métiers assimilés (salariés)"="534B",
                                    "541A-Agents et hôtesses d'accueil et d'information (hors hôtellerie)"="541A",
                                    "541D-Standardistes, téléphonistes"="541D",
                                    "542A-Secrétaires"="542A",
                                    "542B-Dactylos, sténodactylos (sans secrétariat), opérateurs de traitement de texte"="542B",
                                    "543A-Employés des services comptables ou financiers"="543A",
                                    "543D-Employés administratifs divers d'entreprises"="543D",
                                    "544A-Employés et opérateurs d'exploitation en informatique"="544A",
                                    "545A-Employés administratifs des services techniques de la banque"="545A",
                                    "545B-Employés des services commerciaux de la banque"="545B",
                                    "545C-Employés des services techniques des assurances"="545C",
                                    "545D-Employés des services techniques des organismes de sécurité sociale et assimilés"="545D",
                                    "546A-Contrôleurs des transports (personnels roulants)"="546A",
                                    "546B-Agents des services commerciaux des transports de voyageurs et du tourisme"="546B",
                                    "546C-Employés administratifs d'exploitation des transports de marchandises"="546C",
                                    "546D-Hôtesses de l'air et stewards"="546D",
                                    "546E-Autres agents et hôtesses d'accompagnement (transports, tourisme)"="546E",
                                    "551A-Employés de libre service du commerce et magasiniers"="551A",
                                    "552A-Caissiers de magasin"="552A",
                                    "553A-Vendeurs non spécialisés"="553A",
                                    "554A-Vendeurs en alimentation"="554A",
                                    "554B-Vendeurs en ameublement, décor, équipement du foyer"="554B",
                                    "554C-Vendeurs en droguerie, bazar, quincaillerie, bricolage"="554C",
                                    "554D-Vendeurs du commerce de fleurs"="554D",
                                    "554E-Vendeurs en habillement et articles de sport"="554E",
                                    "554F-Vendeurs en produits de beauté, de luxe (hors biens culturels) et optique"="554F",
                                    "554G-Vendeurs de biens culturels (livres, disques, multimédia, objets d'art)"="554G",
                                    "554H-Vendeurs de tabac, presse et articles divers"="554H",
                                    "554J-Pompistes et gérants de station-service (salariés ou mandataires)"="554J",
                                    "555A-Vendeurs par correspondance, télévendeurs"="555A",
                                    "556A-Vendeurs en gros de biens d'équipement, biens intermédiaires"="556A",
                                    "561A-Serveurs, commis de restaurant, garçons (bar, brasserie, café ou restaurant)"="561A",
                                    "561D-Aides de cuisine, apprentis de cuisine et employés polyvalents de la restauration"="561D",
                                    "561E-Employés de l'hôtellerie : réception et hall"="561E",
                                    "561F-Employés d'étage et employés polyvalents de l'hôtellerie"="561F",
                                    "562A-Manucures, esthéticiens (salariés)"="562A",
                                    "562B-Coiffeurs salariés"="562B",
                                    "563A-Assistantes maternelles, gardiennes d'enfants, familles d'accueil"="563A",
                                    "563B-Aides à domicile, aides ménagères, travailleuses familiales"="563B",
                                    "563C-Employés de maison et personnels de ménage chez des particuliers"="563C",
                                    "564A-Concierges, gardiens d'immeubles"="564A",
                                    "564B-Employés des services divers"="564B",
                                    "621A-Chefs d'équipe du gros œuvre et des travaux publics"="621A",
                                    "621B-Ouvriers qualifiés du travail du béton"="621B",
                                    "621C-Conducteurs qualifiés d'engins de chantiers du bâtiment et des travaux publics"="621C",
                                    "621D-Ouvriers des travaux publics en installations électriques et de télécommunications"="621D",
                                    "621E-Autres ouvriers qualifiés des travaux publics"="621E",
                                    "621F-Ouvriers qualifiés des travaux publics (salariés de l'État et des collectivités locales)"="621F",
                                    "621G-Mineurs de fond qualifiés et autres ouvriers qualifiés des industries d'extraction (carrières, pétrole, gaz...)"="621G",
                                    "622A-Opérateurs qualifiés sur machines automatiques en production électrique ou électronique"="622A",
                                    "622B-Câbleurs qualifiés, bobiniers qualifiés"="622B",
                                    "622G-Plateformistes, contrôleurs qualifiés de matériel électrique ou électronique"="622G",
                                    "623A-Chaudronniers-tôliers industriels, opérateurs qualifiés du travail en forge, conducteurs qualifiés d'équipement de formage, traceurs qualifiés"="623A",
                                    "623B-Tuyauteurs industriels qualifiés"="623B",
                                    "623C-Soudeurs qualifiés sur métaux"="623C",
                                    "623F-Opérateurs qualifiés d'usinage des métaux travaillant à l'unité ou en petite série, moulistes qualifiés"="623F",
                                    "623G-Opérateurs qualifiés d'usinage des métaux sur autres machines (sauf moulistes)"="623G",
                                    "624A-Monteurs qualifiés d'ensembles mécaniques"="624A",
                                    "624D-Monteurs qualifiés en structures métalliques"="624D",
                                    "624E-Ouvriers qualifiés de contrôle et d'essais en mécanique"="624E",
                                    "624F-Ouvriers qualifiés des traitements thermiques et de surface sur métaux"="624F",
                                    "624G-Autres mécaniciens ou ajusteurs qualifiés (ou spécialité non reconnue)"="624G",
                                    "625A-Pilotes d'installation lourde des industries de transformation : agroalimentaire, chimie, plasturgie, énergie"="625A",
                                    "625B-Ouvriers qualifiés et agents qualifiés de laboratoire : agroalimentaire, chimie, biologie, pharmacie"="625B",
                                    "625C-Autres opérateurs et ouvriers qualifiés de la chimie (y.c. pharmacie) et de la plasturgie"="625C",
                                    "625D-Opérateurs de la transformation des viandes"="625D",
                                    "625E-Autres opérateurs et ouvriers qualifiés de l'industrie agricole et alimentaire (hors transformation des viandes)"="625E",
                                    "625H-Ouvriers qualifiés des autres industries (eau, gaz, énergie, chauffage)"="625H",
                                    "626A-Pilotes d'installation lourde des industries de transformation : métallurgie, production verrière, matériaux de construction"="626A",
                                    "626B-Autres opérateurs et ouvriers qualifiés : métallurgie, production verrière, matériaux de construction"="626B",
                                    "626C-Opérateurs et ouvriers qualifiés des industries lourdes du bois et de la fabrication du papier-carton"="626C",
                                    "627A-Opérateurs qualifiés du textile et de la mégisserie"="627A",
                                    "627B-Ouvriers qualifiés de la coupe des vêtements et de l'habillement, autres opérateurs de confection qualifiés"="627B",
                                    "627C-Ouvriers qualifiés du travail industriel du cuir"="627C",
                                    "627D-Ouvriers qualifiés de scierie, de la menuiserie industrielle et de l'ameublement"="627D",
                                    "627E-Ouvriers de la photogravure et des laboratoires photographiques et cinématographiques"="627E",
                                    "627F-Ouvriers de la composition et de l'impression, ouvriers qualifiés de la brochure, de la reliure et du façonnage du papier-carton"="627F",
                                    "628A-Mécaniciens qualifiés de maintenance, entretien : équipements industriels"="628A",
                                    "628B-Électromécaniciens, électriciens qualifiés d'entretien : équipements industriels"="628B",
                                    "628C-Régleurs qualifiés d'équipements de fabrication (travail des métaux, mécanique)"="628C",
                                    "628D-Régleurs qualifiés d'équipements de fabrication (hors travail des métaux et mécanique)"="628D",
                                    "628E-Ouvriers qualifiés de l'assainissement et du traitement des déchets"="628E",
                                    "628F-Agents qualifiés de laboratoire (sauf chimie, santé)"="628F",
                                    "628G-Ouvriers qualifiés divers de type industriel"="628G",
                                    "631A-Jardiniers"="631A",
                                    "632A-Maçons qualifiés"="632A",
                                    "632B-Ouvriers qualifiés du travail de la pierre"="632B",
                                    "632C-Charpentiers en bois qualifiés"="632C",
                                    "632D-Menuisiers qualifiés du bâtiment"="632D",
                                    "632E-Couvreurs qualifiés"="632E",
                                    "632F-Plombiers et chauffagistes qualifiés"="632F",
                                    "632G-Peintres et ouvriers qualifiés de pose de revêtements sur supports verticaux"="632G",
                                    "632H-Soliers moquetteurs et ouvriers qualifiés de pose de revêtements souples sur supports horizontaux"="632H",
                                    "632J-Monteurs qualifiés en agencement, isolation"="632J",
                                    "632K-Ouvriers qualifiés d'entretien général des bâtiments"="632K",
                                    "633A-Électriciens qualifiés de type artisanal (y.c. bâtiment)"="633A",
                                    "633B-Dépanneurs qualifiés en radiotélévision, électroménager, matériel électronique (salariés)"="633B",
                                    "633C-Électriciens, électroniciens qualifiés en maintenance entretien, réparation : automobile"="633C",
                                    "633D-Électriciens, électroniciens qualifiés en maintenance, entretien : équipements non industriels"="633D",
                                    "634A-Carrossiers d'automobiles qualifiés"="634A",
                                    "634B-Métalliers, serruriers qualifiés"="634B",
                                    "634C-Mécaniciens qualifiés en maintenance, entretien, réparation : automobile"="634C",
                                    "634D-Mécaniciens qualifiés de maintenance, entretien : équipements non industriels"="634D",
                                    "635A-Tailleurs et couturières qualifiés, ouvriers qualifiés du travail des étoffes (sauf fabrication de vêtements), ouvriers qualifiés de type artisanal du travail du cuir"="635A",
                                    "636A-Bouchers (sauf industrie de la viande)"="636A",
                                    "636B-Charcutiers (sauf industrie de la viande)"="636B",
                                    "636C-Boulangers, pâtissiers (sauf activité industrielle)"="636C",
                                    "636D-Cuisiniers et commis de cuisine"="636D",
                                    "637A-Modeleurs (sauf modeleurs de métal), mouleurs-noyauteurs à la main, ouvriers qualifiés du travail du verre ou de la céramique à la main"="637A",
                                    "637B-Ouvriers d'art"="637B",
                                    "637C-Ouvriers et techniciens des spectacles vivants et audiovisuels"="637C",
                                    "637D-Ouvriers qualifiés divers de type artisanal"="637D",
                                    "641A-Conducteurs routiers et grands routiers (salariés)"="641A",
                                    "641B-Conducteurs de véhicule routier de transport en commun (salariés)"="641B",
                                    "642A-Conducteurs de taxi (salariés)"="642A",
                                    "642B-Conducteurs de voiture particulière (salariés)"="642B",
                                    "643A-Conducteurs livreurs, coursiers (salariés)"="643A",
                                    "644A-Conducteurs de véhicule de ramassage des ordures ménagères"="644A",
                                    "651A-Conducteurs d'engin lourd de levage"="651A",
                                    "651B-Conducteurs d'engin lourd de manœuvre"="651B",
                                    "652A-Ouvriers qualifiés de la manutention, conducteurs de chariots élévateurs, caristes"="652A",
                                    "652B-Dockers"="652B",
                                    "653A-Magasiniers qualifiés"="653A",
                                    "654A-Conducteurs qualifiés d'engins de transport guidés"="654A",
                                    "655A-Autres agents et ouvriers qualifiés (sédentaires) des services d'exploitation des transports"="655A",
                                    "656A-Matelots de la marine marchande, capitaines et matelots timoniers de la navigation fluviale (salariés)"="656A",
                                    "671A-Ouvriers non qualifiés des travaux publics de l'État et des collectivités locales"="671A",
                                    "671B-Ouvriers non qualifiés des travaux publics, du travail du béton et de l'extraction, hors État et collectivités locales"="671B",
                                    "672A-Ouvriers non qualifiés de l'électricité et de l'électronique"="672A",
                                    "673A-Ouvriers de production non qualifiés travaillant par enlèvement de métal"="673A",
                                    "673B-Ouvriers de production non qualifiés travaillant par formage de métal"="673B",
                                    "673C-Ouvriers non qualifiés de montage, contrôle en mécanique et travail des métaux"="673C",
                                    "674A-Ouvriers de production non qualifiés : chimie, pharmacie, plasturgie"="674A",
                                    "674B-Ouvriers de production non qualifiés de la transformation des viandes"="674B",
                                    "674C-Autres ouvriers de production non qualifiés : industrie agro-alimentaire"="674C",
                                    "674D-Ouvriers de production non qualifiés : métallurgie, production verrière, céramique, matériaux de construction"="674D",
                                    "674E-Ouvriers de production non qualifiés : industrie lourde du bois, fabrication des papiers et cartons"="674E",
                                    "675A-Ouvriers de production non qualifiés du textile et de la confection, de la tannerie-mégisserie et du travail du cuir"="675A",
                                    "675B-Ouvriers de production non qualifiés du travail du bois et de l'ameublement"="675B",
                                    "675C-Ouvriers de production non qualifiés de l'imprimerie, presse, édition"="675C",
                                    "676A-Manutentionnaires non qualifiés"="676A",
                                    "676B-Déménageurs (hors chauffeurs-déménageurs), non qualifiés"="676B",
                                    "676C-Ouvriers du tri, de l'emballage, de l'expédition, non qualifiés"="676C",
                                    "676D-Agents non qualifiés des services d'exploitation des transports"="676D",
                                    "676E-Ouvriers non qualifiés divers de type industriel"="676E",
                                    "681A-Ouvriers non qualifiés du gros œuvre du bâtiment"="681A",
                                    "681B-Ouvriers non qualifiés du second œuvre du bâtiment"="681B",
                                    "682A-Métalliers, serruriers, réparateurs en mécanique non qualifiés"="682A",
                                    "683A-Apprentis boulangers, bouchers, charcutiers"="683A",
                                    "684A-Nettoyeurs"="684A",
                                    "684B-Ouvriers non qualifiés de l'assainissement et du traitement des déchets"="684B",
                                    "685A-Ouvriers non qualifiés divers de type artisanal"="685A",
                                    "691A-Conducteurs d'engin agricole ou forestier"="691A",
                                    "691B-Ouvriers de l'élevage"="691B",
                                    "691C-Ouvriers du maraîchage ou de l'horticulture"="691C",
                                    "691D-Ouvriers de la viticulture ou de l'arboriculture fruitière"="691D",
                                    "691E-Ouvriers agricoles sans spécialisation particulière"="691E",
                                    "691F-Ouvriers de l'exploitation forestière ou de la sylviculture"="691F",
                                    "692A-Marins-pêcheurs et ouvriers de l'aquaculture"="692A",
                                    
  ) } 
if("PROFMER" %in% names(ct) & !is.numeric(ct$PROFMER) ) {
  ct$PROFMER <- forcats::fct_recode(ctraw$PROFMER,
                                    
                                    "111A-Agriculteurs sur petite exploitation de céréales-grandes cultures"="111A",
                                    "111B-Maraîchers, horticulteurs sur petite exploitation"="111B",
                                    "111C-Viticulteurs, arboriculteurs fruitiers, sur petite exploitation"="111C",
                                    "111D-Éleveurs d'herbivores, sur petite exploitation"="111D",
                                    "111E-Éleveurs de granivores et éleveurs mixtes, sur petite exploitation"="111E",
                                    "111F-Agriculteurs sur petite exploitation sans orientation dominante"="111F",
                                    "121A-Agriculteurs sur moyenne exploitation de céréales-grandes cultures"="121A",
                                    "121B-Maraîchers, horticulteurs sur moyenne exploitation"="121B",
                                    "121C-Viticulteurs, arboriculteurs fruitiers, sur moyenne exploitation"="121C",
                                    "121D-Éleveurs d'herbivores sur moyenne exploitation"="121D",
                                    "121E-Éleveurs de granivores et éleveurs mixtes, sur moyenne exploitation"="121E",
                                    "121F-Agriculteurs sur moyenne exploitation sans orientation dominante"="121F",
                                    "122A-Entrepreneurs de travaux agricoles à façon, de 0 à 9 salariés"="122A",
                                    "122B-Exploitants forestiers indépendants, de 0 à 9 salariés"="122B",
                                    "122C-Patrons pêcheurs et aquaculteurs, de 0 à 9 salariés"="122C",
                                    "131A-Agriculteurs sur grande exploitation de céréales-grandes cultures"="131A",
                                    "131B-Maraîchers, horticulteurs, sur grande exploitation"="131B",
                                    "131C-Viticulteurs, arboriculteurs fruitiers, sur grande exploitation"="131C",
                                    "131D-Éleveurs d'herbivores, sur grande exploitation"="131D",
                                    "131E-Éleveurs de granivores et éleveurs mixtes, sur grande exploitation"="131E",
                                    "131F-Agriculteurs sur grande exploitation sans orientation dominante"="131F",
                                    "211A-Artisans maçons"="211A",
                                    "211B-Artisans menuisiers du bâtiment, charpentiers en bois"="211B",
                                    "211C-Artisans couvreurs"="211C",
                                    "211D-Artisans plombiers, chauffagistes"="211D",
                                    "211E-Artisans électriciens du bâtiment"="211E",
                                    "211F-Artisans de la peinture et des finitions du bâtiment"="211F",
                                    "211G-Artisans serruriers, métalliers"="211G",
                                    "211H-Artisans en terrassement, travaux publics"="211H",
                                    "211J-Entrepreneurs en parcs et jardins, paysagistes"="211J",
                                    "212A-Artisans mécaniciens en machines agricoles"="212A",
                                    "212B-Artisans chaudronniers"="212B",
                                    "212C-Artisans en mécanique générale, fabrication et travail des métaux (hors horlogerie et matériel de précision)"="212C",
                                    "212D-Artisans divers de fabrication de machines"="212D",
                                    "213A-Artisans de l'habillement, du textile et du cuir"="213A",
                                    "214A-Artisans de l'ameublement"="214A",
                                    "214B-Artisans du travail mécanique du bois"="214B",
                                    "214C-Artisans du papier, de l'imprimerie et de la reproduction"="214C",
                                    "214D-Artisans de fabrication en matériaux de construction (hors artisanat d'art)"="214D",
                                    "214E-Artisans d'art"="214E",
                                    "214F-Autres artisans de fabrication (y.c. horlogers, matériel de précision)"="214F",
                                    "215A-Artisans boulangers, pâtissiers, de 0 à 9 salariés"="215A",
                                    "215B-Artisans bouchers, de 0 à 9 salariés"="215B",
                                    "215C-Artisans charcutiers, de 0 à 9 salariés"="215C",
                                    "215D-Autres artisans de l'alimentation, de 0 à 9 salariés"="215D",
                                    "216A-Artisans mécaniciens réparateurs d'automobiles"="216A",
                                    "216B-Artisans tôliers-carrossiers d'automobiles"="216B",
                                    "216C-Artisans réparateurs divers"="216C",
                                    "217A-Conducteurs de taxis, ambulanciers et autres artisans du transport, de 0 à 9 salariés"="217A",
                                    "217B-Artisans déménageurs, de 0 à 9 salariés"="217B",
                                    "217C-Artisans coiffeurs, manucures, esthéticiens, de 0 à 9 salariés"="217C",
                                    "217D-Artisans teinturiers, blanchisseurs, de 0 à 9 salariés"="217D",
                                    "217E-Artisans des services divers, de 0 à 9 salariés"="217E",
                                    "218A-Transporteurs indépendants routiers et fluviaux, de 0 à 9 salariés"="218A",
                                    "219A-Aides familiaux non salariés ou associés d'artisans, effectuant un travail administratif ou commercial"="219A",
                                    "221A-Petits et moyens grossistes en alimentation, de 0 à 9 salariés"="221A",
                                    "221B-Petits et moyens grossistes en produits non alimentaires, de 0 à 9 salariés"="221B",
                                    "222A-Petits et moyens détaillants en alimentation spécialisée, de 0 à 9 salariés"="222A",
                                    "222B-Petits et moyens détaillants en alimentation générale, de 0 à 9 salariés"="222B",
                                    "223A-Détaillants en ameublement, décor, équipement du foyer, de 0 à 9 salariés"="223A",
                                    "223B-Détaillants en droguerie, bazar, quincaillerie, bricolage, de 0 à 9 salariés"="223B",
                                    "223C-Fleuristes, de 0 à 9 salariés"="223C",
                                    "223D-Détaillants en habillement et articles de sport, de 0 à 9 salariés"="223D",
                                    "223E-Détaillants en produits de beauté, de luxe (hors biens culturels), de 0 à 9 salariés"="223E",
                                    "223F-Détaillants en biens culturels (livres, disques, multimédia, objets d'art), de 0 à 9 salariés"="223F",
                                    "223G-Détaillants en tabac, presse et articles divers, de 0 à 9 salariés"="223G",
                                    "223H-Exploitants et gérants libres de station-service, de 0 à 9 salariés"="223H",
                                    "224A-Exploitants de petit restaurant, café-restaurant, de 0 à 2 salariés"="224A",
                                    "224B-Exploitants de petit café, débit de boisson, associé ou non à une autre activité hors restauration, de 0 à 2 salariés"="224B",
                                    "224C-Exploitants de petit hôtel, hôtel-restaurant, de 0 à 2 salariés"="224C",
                                    "224D-Exploitants de café, restaurant, hôtel, de 3 à 9 salariés"="224D",
                                    "225A-Intermédiaires indépendants du commerce, de 0 à 9 salariés"="225A",
                                    "226A-Agents généraux et courtiers d'assurance indépendants, de 0 à 9 salariés"="226A",
                                    "226B-Agents de voyage et auxiliaires de transports indépendants, de 0 à 9 salariés"="226B",
                                    "226C-Agents immobiliers indépendants, de 0 à 9 salariés"="226C",
                                    "227A-Indépendants gestionnaires de spectacle ou de service récréatif, de 0 à 9 salariés"="227A",
                                    "227B-Indépendants gestionnaires d'établissements privés (enseignement, santé, social), de 0 à 9 salariés"="227B",
                                    "227C-Astrologues, professionnels de la parapsychologie, guérisseurs, de 0 à 9 salariés"="227C",
                                    "227D-Autres indépendants divers prestataires de services, de 0 à 9 salariés"="227D",
                                    "231A-Chefs de grande entreprise de 500 salariés et plus"="231A",
                                    "232A-Chefs de moyenne entreprise, de 50 à 499 salariés"="232A",
                                    "233A-Chefs d'entreprise du bâtiment et des travaux publics, de 10 à 49 salariés"="233A",
                                    "233B-Chefs d'entreprise de l'industrie ou des transports, de 10 à 49 salariés"="233B",
                                    "233C-Chefs d'entreprise commerciale, de 10 à 49 salariés"="233C",
                                    "233D-Chefs d'entreprise de services, de 10 à 49 salariés"="233D",
                                    "311A-Médecins libéraux spécialistes"="311A",
                                    "311B-Médecins libéraux généralistes"="311B",
                                    "311C-Chirurgiens dentistes (libéraux ou salariés)"="311C",
                                    "311D-Psychologues, psychanalystes, psychothérapeutes (non médecins)"="311D",
                                    "311E-Vétérinaires (libéraux ou salariés)"="311E",
                                    "311F-Pharmaciens libéraux"="311F",
                                    "312A-Avocats"="312A",
                                    "312B-Notaires"="312B",
                                    "312C-Experts comptables, comptables agréés, libéraux"="312C",
                                    "312D-Conseils et experts libéraux en études économiques, organisation et recrutement, gestion et fiscalité"="312D",
                                    "312E-Ingénieurs conseils libéraux en études techniques"="312E",
                                    "312F-Architectes libéraux"="312F",
                                    "312G-Géomètres-experts, huissiers de justice, officiers ministériels, professions libérales diverses"="312G",
                                    "313A-Aides familiaux non salariés de professions libérales effectuant un travail administratif"="313A",
                                    "331A-Personnels de direction de la fonction publique (État, collectivités locales, hôpitaux)"="331A",
                                    "332A-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés"="332A",
                                    "332B-Ingénieurs des collectivités locales et des hôpitaux"="332B",
                                    "333A-Magistrats"="333A",
                                    "333B-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes"="333B",
                                    "333C-Cadres de la Poste"="333C",
                                    "333D-Cadres administratifs de France Télécom (statut public)"="333D",
                                    "333E-Autres personnels administratifs de catégorie A de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="333E",
                                    "333F-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)"="333F",
                                    "334A-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)"="334A",
                                    "335A-Personnes exerçant un mandat politique ou syndical"="335A",
                                    "341A-Professeurs agrégés et certifiés de l'enseignement secondaire"="341A",
                                    "341B-Chefs d'établissement de l'enseignement secondaire et inspecteurs"="341B",
                                    "342A-Enseignants de l'enseignement supérieur"="342A",
                                    "342E-Chercheurs de la recherche publique"="342E",
                                    "343A-Psychologues spécialistes de l'orientation scolaire et professionnelle"="343A",
                                    "344A-Médecins hospitaliers sans activité libérale"="344A",
                                    "344B-Médecins salariés non hospitaliers"="344B",
                                    "344C-Internes en médecine, odontologie et pharmacie"="344C",
                                    "344D-Pharmaciens salariés"="344D",
                                    "351A-Bibliothécaires, archivistes, conservateurs et autres cadres du patrimoine (fonction publique)"="351A",
                                    "352A-Journalistes (y. c. rédacteurs en chef)"="352A",
                                    "352B-Auteurs littéraires, scénaristes, dialoguistes"="352B",
                                    "353A-Directeurs de journaux, administrateurs de presse, directeurs d'éditions (littéraire, musicale, audiovisuelle et multimédia)"="353A",
                                    "353B-Directeurs, responsables de programmation et de production de l'audiovisuel et des spectacles"="353B",
                                    "353C-Cadres artistiques et technico-artistiques de la réalisation de l'audiovisuel et des spectacles"="353C",
                                    "354A-Artistes plasticiens"="354A",
                                    "354B-Artistes+B493 de la musique et du chant"="354B",
                                    "354C-Artistes dramatiques"="354C",
                                    "354D-Artistes de la danse, du cirque et des spectacles divers"="354D",
                                    "354G-Professeurs d'art (hors établissements scolaires)"="354G",
                                    "371A-Cadres d'état-major administratifs, financiers, commerciaux des grandes entreprises"="371A",
                                    "372A-Cadres chargés d'études économiques, financières, commerciales"="372A",
                                    "372B-Cadres de l'organisation ou du contrôle des services administratifs et financiers"="372B",
                                    "372C-Cadres spécialistes des ressources humaines et du recrutement"="372C",
                                    "372D-Cadres spécialistes de la formation"="372D",
                                    "372E-Juristes"="372E",
                                    "372F-Cadres de la documentation, de l'archivage (hors fonction publique)"="372F",
                                    "373A-Cadres des services financiers ou comptables des grandes entreprises"="373A",
                                    "373B-Cadres des autres services administratifs des grandes entreprises"="373B",
                                    "373C-Cadres des services financiers ou comptables des petites et moyennes entreprises"="373C",
                                    "373D-Cadres des autres services administratifs des petites et moyennes entreprises"="373D",
                                    "374A-Cadres de l'exploitation des magasins de vente du commerce de détail"="374A",
                                    "374B-Chefs de produits, acheteurs du commerce et autres cadres de la mercatique"="374B",
                                    "374C-Cadres commerciaux des grandes entreprises (hors commerce de détail)"="374C",
                                    "374D-Cadres commerciaux des petites et moyennes entreprises (hors commerce de détail)"="374D",
                                    "375A-Cadres de la publicité"="375A",
                                    "375B-Cadres des relations publiques et de la communication"="375B",
                                    "376A-Cadres des marchés financiers"="376A",
                                    "376B-Cadres des opérations bancaires"="376B",
                                    "376C-Cadres commerciaux de la banque"="376C",
                                    "376D-Chefs d'établissements et responsables de l'exploitation bancaire"="376D",
                                    "376E-Cadres des services techniques des assurances"="376E",
                                    "376F-Cadres des services techniques des organismes de sécurité sociale et assimilés"="376F",
                                    "376G-Cadres de l'immobilier"="376G",
                                    "377A-Cadres de l'hôtellerie et de la restauration"="377A",
                                    "380A-Directeurs techniques des grandes entreprises"="380A",
                                    "381A-Ingénieurs et cadres d'étude et d'exploitation de l'agriculture, la pêche, les eaux et forêts"="381A",
                                    "382A-Ingénieurs et cadres d'étude du bâtiment et des travaux publics"="382A",
                                    "382B-Architectes salariés"="382B",
                                    "382C-Ingénieurs, cadres de chantier et conducteurs de travaux (cadres) du bâtiment et des travaux publics"="382C",
                                    "382D-Ingénieurs et cadres technico-commerciaux en bâtiment, travaux publics"="382D",
                                    "383A-Ingénieurs et cadres d'étude, recherche et développement en électricité, électronique"="383A",
                                    "383B-Ingénieurs et cadres de fabrication en matériel électrique, électronique"="383B",
                                    "383C-Ingénieurs et cadres technico-commerciaux en matériel électrique ou électronique professionnel"="383C",
                                    "384A-Ingénieurs et cadres d'étude, recherche et développement en mécanique et travail des métaux"="384A",
                                    "384B-Ingénieurs et cadres de fabrication en mécanique et travail des métaux"="384B",
                                    "384C-Ingénieurs et cadres technico-commerciaux en matériel mécanique professionnel"="384C",
                                    "385A-Ingénieurs et cadres d'étude, recherche et développement des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385A",
                                    "385B-Ingénieurs et cadres de fabrication des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385B",
                                    "385C-Ingénieurs et cadres technico-commerciaux des industries de transformations (biens intermédiaires)"="385C",
                                    "386A-Ingénieurs et cadres d'étude, recherche et développement des autres industries (imprimerie, matériaux souples, ameublement et bois, énergie, eau)"="386A",
                                    "386D-Ingénieurs et cadres de la production et de la distribution d'énergie, eau"="386D",
                                    "386E-Ingénieurs et cadres de fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="386E",
                                    "387A-Ingénieurs et cadres des achats et approvisionnements industriels"="387A",
                                    "387B-Ingénieurs et cadres de la logistique, du planning et de l'ordonnancement"="387B",
                                    "387C-Ingénieurs et cadres des méthodes de production"="387C",
                                    "387D-Ingénieurs et cadres du contrôle-qualité"="387D",
                                    "387E-Ingénieurs et cadres de la maintenance, de l'entretien et des travaux neufs"="387E",
                                    "387F-Ingénieurs et cadres techniques de l'environnement"="387F",
                                    "388A-Ingénieurs et cadres d'étude, recherche et développement en informatique"="388A",
                                    "388B-Ingénieurs et cadres d'administration, maintenance, support et services aux utilisateurs en informatique"="388B",
                                    "388C-Chefs de projets informatiques, responsables informatiques"="388C",
                                    "388D-Ingénieurs et cadres technico-commerciaux en informatique et télécommunications"="388D",
                                    "388E-Ingénieurs et cadres spécialistes des télécommunications"="388E",
                                    "389A-Ingénieurs et cadres techniques de l'exploitation des transports"="389A",
                                    "389B-Officiers et cadres navigants techniques et commerciaux de l'aviation civile"="389B",
                                    "389C-Officiers et cadres navigants techniques de la marine marchande"="389C",
                                    "421A-Instituteurs"="421A",
                                    "421B-Professeurs des écoles"="421B",
                                    "422A-Professeurs d'enseignement général des collèges"="422A",
                                    "422B-Professeurs de lycée professionnel"="422B",
                                    "422C-Maîtres auxiliaires et professeurs contractuels de l'enseignement secondaire"="422C",
                                    "422D-Conseillers principaux d'éducation"="422D",
                                    "422E-Surveillants et aides-éducateurs des établissements d'enseignement"="422E",
                                    "423A-Moniteurs d'école de conduite"="423A",
                                    "423B-Formateurs et animateurs de formation continue"="423B",
                                    "424A-Moniteurs et éducateurs sportifs, sportifs professionnels"="424A",
                                    "425A-Sous-bibliothécaires, cadres intermédiaires du patrimoine"="425A",
                                    "431A-Cadres infirmiers et assimilés"="431A",
                                    "431B-Infirmiers psychiatriques"="431B",
                                    "431C-Puéricultrices"="431C",
                                    "431D-Infirmiers spécialisés (autres qu'infirmiers psychiatriques et puéricultrices)"="431D",
                                    "431E-Sages-femmes (libérales ou salariées)"="431E",
                                    "431F-Infirmiers en soins généraux, salariés"="431F",
                                    "431G-Infirmiers libéraux"="431G",
                                    "432A-Masseurs-kinésithérapeutes rééducateurs, libéraux"="432A",
                                    "432B-Masseurs-kinésithérapeutes rééducateurs, salariés"="432B",
                                    "432C-Autres spécialistes de la rééducation, libéraux"="432C",
                                    "432D-Autres spécialistes de la rééducation, salariés"="432D",
                                    "433A-Techniciens médicaux"="433A",
                                    "433B-Opticiens lunetiers et audioprothésistes (indépendants et salariés)"="433B",
                                    "433C-Autres spécialistes de l'appareillage médical (indépendants et salariés)"="433C",
                                    "433D-Préparateurs en pharmacie"="433D",
                                    "434A-Cadres de l'intervention socio-éducative"="434A",
                                    "434B-Assistants de service social"="434B",
                                    "434C-Conseillers en économie sociale familiale"="434C",
                                    "434D-Éducateurs spécialisés"="434D",
                                    "434E-Moniteurs éducateurs"="434E",
                                    "434F-Éducateurs techniques spécialisés, moniteurs d'atelier"="434F",
                                    "434G-Éducateurs de jeunes enfants"="434G",
                                    "435A-Directeurs de centres socioculturels et de loisirs"="435A",
                                    "435B-Animateurs socioculturels et de loisirs"="435B",
                                    "441A-Clergé séculier"="441A",
                                    "441B-Clergé régulier"="441B",
                                    "451A-Professions intermédiaires de la Poste"="451A",
                                    "451B-Professions intermédiaires administratives de France Télécom (statut public)"="451B",
                                    "451C-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés"="451C",
                                    "451D-Ingénieurs du contrôle de la navigation aérienne"="451D",
                                    "451E-Autres personnels administratifs de catégorie B de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="451E",
                                    "451F-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)"="451F",
                                    "452A-Inspecteurs et officiers de police"="452A",
                                    "452B-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie"="452B",
                                    "461A-Personnel de secrétariat de niveau supérieur, secrétaires de direction (non cadres)"="461A",
                                    "461D-Maîtrise et techniciens des services financiers ou comptables"="461D",
                                    "461E-Maîtrise et techniciens administratifs des services juridiques ou du personnel"="461E",
                                    "461F-Maîtrise et techniciens administratifs des autres services administratifs"="461F",
                                    "462A-Chefs de petites surfaces de vente (salariés ou mandataires)"="462A",
                                    "462B-Maîtrise de l'exploitation des magasins de vente"="462B",
                                    "462C-Acheteurs non classés cadres, aides-acheteurs"="462C",
                                    "462D-Animateurs commerciaux des magasins de vente, marchandiseurs (non cadres)"="462D",
                                    "462E-Autres professions intermédiaires commerciales (sauf techniciens des forces de vente)"="462E",
                                    "463A-Techniciens commerciaux et technico-commerciaux, représentants en informatique"="463A",
                                    "463B-Techniciens commerciaux et technico-commerciaux, représentants en biens d'équipement, en biens intermédiaires, commerce interindustriel (hors informatique)"="463B",
                                    "463C-Techniciens commerciaux et technico-commerciaux, représentants en biens de consommation auprès d'entreprises"="463C",
                                    "463D-Techniciens commerciaux et technico-commerciaux, représentants en services auprès d'entreprises ou de professionnels (hors banque, assurance, informatique)"="463D",
                                    "463E-Techniciens commerciaux et technico-commerciaux, représentants auprès de particuliers (hors banque, assurance, informatique)"="463E",
                                    "464A-Assistants de la publicité, des relations publiques (indépendants ou salariés)"="464A",
                                    "464B-Interprètes, traducteurs (indépendants ou salariés)"="464B",
                                    "465A-Concepteurs et assistants techniques des arts graphiques, de la mode et de la décoration (indépendants et salariés)"="465A",
                                    "465B-Assistants techniques de la réalisation des spectacles vivants et audiovisuels (indépendants ou salariés)"="465B",
                                    "465C-Photographes (indépendants et salariés)"="465C",
                                    "466A-Responsables commerciaux et administratifs des transports de voyageurs et du tourisme (non cadres)"="466A",
                                    "466B-Responsables commerciaux et administratifs des transports de marchandises (non cadres)"="466B",
                                    "466C-Responsables d'exploitation des transports de voyageurs et de marchandises (non cadres)"="466C",
                                    "467A-Chargés de clientèle bancaire"="467A",
                                    "467B-Techniciens des opérations bancaires"="467B",
                                    "467C-Professions intermédiaires techniques et commerciales des assurances"="467C",
                                    "467D-Professions intermédiaires techniques des organismes de sécurité sociale"="467D",
                                    "468A-Maîtrise de restauration : salle et service"="468A",
                                    "468B-Maîtrise de l'hébergement : hall et étages"="468B",
                                    "471A-Techniciens d'étude et de conseil en agriculture, eaux et forêt"="471A",
                                    "471B-Techniciens d'exploitation et de contrôle de la production en agriculture, eaux et forêt"="471B",
                                    "472A-Dessinateurs en bâtiment, travaux publics"="472A",
                                    "472B-Géomètres, topographes"="472B",
                                    "472C-Métreurs et techniciens divers du bâtiment et des travaux publics"="472C",
                                    "472D-Techniciens des travaux publics de l'État et des collectivités locales"="472D",
                                    "473A-Dessinateurs en électricité, électromécanique et électronique"="473A",
                                    "473B-Techniciens de recherche-développement et des méthodes de fabrication en électricité, électromécanique et électronique"="473B",
                                    "473C-Techniciens de fabrication et de contrôle-qualité en électricité, électromécanique et électronique"="473C",
                                    "474A-Dessinateurs en construction mécanique et travail des métaux"="474A",
                                    "474B-Techniciens de recherche-développement et des méthodes de fabrication en construction mécanique et travail des métaux"="474B",
                                    "474C-Techniciens de fabrication et de contrôle-qualité en construction mécanique et travail des métaux"="474C",
                                    "475A-Techniciens de recherche-développement et des méthodes de production des industries de transformation"="475A",
                                    "475B-Techniciens de production et de contrôle-qualité des industries de transformation"="475B",
                                    "476A-Assistants techniques, techniciens de l'imprimerie et de l'édition"="476A",
                                    "476B-Techniciens de l'industrie des matériaux souples, de l'ameublement et du bois"="476B",
                                    "477A-Techniciens de la logistique, du planning et de l'ordonnancement"="477A",
                                    "477B-Techniciens d'installation et de maintenance des équipements industriels (électriques, électromécaniques, mécaniques, hors informatique)"="477B",
                                    "477C-Techniciens d'installation et de maintenance des équipements non industriels (hors informatique et télécommunications)"="477C",
                                    "477D-Techniciens de l'environnement et du traitement des pollutions"="477D",
                                    "478A-Techniciens d'étude et de développement en informatique"="478A",
                                    "478B-Techniciens de production, d'exploitation en informatique"="478B",
                                    "478C-Techniciens d'installation, de maintenance, support et services aux utilisateurs en informatique"="478C",
                                    "478D-Techniciens des télécommunications et de l'informatique des réseaux"="478D",
                                    "479A-Techniciens des laboratoires de recherche publique ou de l'enseignement"="479A",
                                    "479B-Experts salariés ou indépendants de niveau technicien, techniciens divers"="479B",
                                    "480A-Contremaîtres et agents d'encadrement (non cadres) en agriculture, sylviculture"="480A",
                                    "480B-Maîtres d'équipage de la marine marchande et de la pêche"="480B",
                                    "481A-Conducteurs de travaux (non cadres)"="481A",
                                    "481B-Chefs de chantier (non cadres)"="481B",
                                    "482A-Agents de maîtrise en fabrication de matériel électrique, électronique"="482A",
                                    "483A-Agents de maîtrise en construction mécanique, travail des métaux"="483A",
                                    "484A-Agents de maîtrise en fabrication : agroalimentaire, chimie, plasturgie, pharmacie."="484A",
                                    "484B-Agents de maîtrise en fabrication : métallurgie, matériaux lourds et autres industries de transformation"="484B",
                                    "485A-Agents de maîtrise et techniciens en production et distribution d'énergie, eau, chauffage"="485A",
                                    "485B-Agents de maîtrise en fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="485B",
                                    "486A-Agents de maîtrise en maintenance, installation en électricité, électromécanique et électronique"="486A",
                                    "486D-Agents de maîtrise en maintenance, installation en mécanique"="486D",
                                    "486E-Agents de maîtrise en entretien général, installation, travaux neufs (hors mécanique, électromécanique, électronique)"="486E",
                                    "487A-Responsables d'entrepôt, de magasinage"="487A",
                                    "487B-Responsables du tri, de l'emballage, de l'expédition et autres responsables de la manutention"="487B",
                                    "488A-Maîtrise de restauration : cuisine/production"="488A",
                                    "488B-Maîtrise de restauration : gestion d'établissement"="488B",
                                    "521A-Employés de la Poste"="521A",
                                    "521B-Employés de France Télécom (statut public)"="521B",
                                    "522A-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes"="522A",
                                    "523A-Adjoints administratifs de la fonction publique (y.c. enseignement)"="523A",
                                    "524A-Agents administratifs de la fonction publique (y.c. enseignement)"="524A",
                                    "525A-Agents de service des établissements primaires"="525A",
                                    "525B-Agents de service des autres établissements d'enseignement"="525B",
                                    "525C-Agents de service de la fonction publique (sauf écoles, hôpitaux)"="525C",
                                    "525D-Agents de service hospitaliers (de la fonction publique ou du secteur privé)"="525D",
                                    "526A-Aides-soignants (de la fonction publique ou du secteur privé)"="526A",
                                    "526B-Assistants dentaires, médicaux et vétérinaires, aides de techniciens médicaux"="526B",
                                    "526C-Auxiliaires de puériculture"="526C",
                                    "526D-Aides médico-psychologiques"="526D",
                                    "526E-Ambulanciers salariés (du secteur public ou du secteur privé)"="526E",
                                    "531A-Agents de police de l'État"="531A",
                                    "531B-Agents des polices municipales"="531B",
                                    "531C-Surveillants de l'administration pénitentiaire"="531C",
                                    "532A-Gendarmes (de grade inférieur à adjudant)"="532A",
                                    "532B-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)"="532B",
                                    "532C-Hommes du rang (sauf pompiers militaires)"="532C",
                                    "533A-Pompiers (y.c. pompiers militaires)"="533A",
                                    "533B-Agents techniques forestiers, gardes des espaces naturels"="533B",
                                    "533C-Agents de surveillance du patrimoine et des administrations"="533C",
                                    "534A-Agents civils de sécurité et de surveillance"="534A",
                                    "534B-Convoyeurs de fonds, gardes du corps, enquêteurs privés et métiers assimilés (salariés)"="534B",
                                    "541A-Agents et hôtesses d'accueil et d'information (hors hôtellerie)"="541A",
                                    "541D-Standardistes, téléphonistes"="541D",
                                    "542A-Secrétaires"="542A",
                                    "542B-Dactylos, sténodactylos (sans secrétariat), opérateurs de traitement de texte"="542B",
                                    "543A-Employés des services comptables ou financiers"="543A",
                                    "543D-Employés administratifs divers d'entreprises"="543D",
                                    "544A-Employés et opérateurs d'exploitation en informatique"="544A",
                                    "545A-Employés administratifs des services techniques de la banque"="545A",
                                    "545B-Employés des services commerciaux de la banque"="545B",
                                    "545C-Employés des services techniques des assurances"="545C",
                                    "545D-Employés des services techniques des organismes de sécurité sociale et assimilés"="545D",
                                    "546A-Contrôleurs des transports (personnels roulants)"="546A",
                                    "546B-Agents des services commerciaux des transports de voyageurs et du tourisme"="546B",
                                    "546C-Employés administratifs d'exploitation des transports de marchandises"="546C",
                                    "546D-Hôtesses de l'air et stewards"="546D",
                                    "546E-Autres agents et hôtesses d'accompagnement (transports, tourisme)"="546E",
                                    "551A-Employés de libre service du commerce et magasiniers"="551A",
                                    "552A-Caissiers de magasin"="552A",
                                    "553A-Vendeurs non spécialisés"="553A",
                                    "554A-Vendeurs en alimentation"="554A",
                                    "554B-Vendeurs en ameublement, décor, équipement du foyer"="554B",
                                    "554C-Vendeurs en droguerie, bazar, quincaillerie, bricolage"="554C",
                                    "554D-Vendeurs du commerce de fleurs"="554D",
                                    "554E-Vendeurs en habillement et articles de sport"="554E",
                                    "554F-Vendeurs en produits de beauté, de luxe (hors biens culturels) et optique"="554F",
                                    "554G-Vendeurs de biens culturels (livres, disques, multimédia, objets d'art)"="554G",
                                    "554H-Vendeurs de tabac, presse et articles divers"="554H",
                                    "554J-Pompistes et gérants de station-service (salariés ou mandataires)"="554J",
                                    "555A-Vendeurs par correspondance, télévendeurs"="555A",
                                    "556A-Vendeurs en gros de biens d'équipement, biens intermédiaires"="556A",
                                    "561A-Serveurs, commis de restaurant, garçons (bar, brasserie, café ou restaurant)"="561A",
                                    "561D-Aides de cuisine, apprentis de cuisine et employés polyvalents de la restauration"="561D",
                                    "561E-Employés de l'hôtellerie : réception et hall"="561E",
                                    "561F-Employés d'étage et employés polyvalents de l'hôtellerie"="561F",
                                    "562A-Manucures, esthéticiens (salariés)"="562A",
                                    "562B-Coiffeurs salariés"="562B",
                                    "563A-Assistantes maternelles, gardiennes d'enfants, familles d'accueil"="563A",
                                    "563B-Aides à domicile, aides ménagères, travailleuses familiales"="563B",
                                    "563C-Employés de maison et personnels de ménage chez des particuliers"="563C",
                                    "564A-Concierges, gardiens d'immeubles"="564A",
                                    "564B-Employés des services divers"="564B",
                                    "621A-Chefs d'équipe du gros œuvre et des travaux publics"="621A",
                                    "621B-Ouvriers qualifiés du travail du béton"="621B",
                                    "621C-Conducteurs qualifiés d'engins de chantiers du bâtiment et des travaux publics"="621C",
                                    "621D-Ouvriers des travaux publics en installations électriques et de télécommunications"="621D",
                                    "621E-Autres ouvriers qualifiés des travaux publics"="621E",
                                    "621F-Ouvriers qualifiés des travaux publics (salariés de l'État et des collectivités locales)"="621F",
                                    "621G-Mineurs de fond qualifiés et autres ouvriers qualifiés des industries d'extraction (carrières, pétrole, gaz...)"="621G",
                                    "622A-Opérateurs qualifiés sur machines automatiques en production électrique ou électronique"="622A",
                                    "622B-Câbleurs qualifiés, bobiniers qualifiés"="622B",
                                    "622G-Plateformistes, contrôleurs qualifiés de matériel électrique ou électronique"="622G",
                                    "623A-Chaudronniers-tôliers industriels, opérateurs qualifiés du travail en forge, conducteurs qualifiés d'équipement de formage, traceurs qualifiés"="623A",
                                    "623B-Tuyauteurs industriels qualifiés"="623B",
                                    "623C-Soudeurs qualifiés sur métaux"="623C",
                                    "623F-Opérateurs qualifiés d'usinage des métaux travaillant à l'unité ou en petite série, moulistes qualifiés"="623F",
                                    "623G-Opérateurs qualifiés d'usinage des métaux sur autres machines (sauf moulistes)"="623G",
                                    "624A-Monteurs qualifiés d'ensembles mécaniques"="624A",
                                    "624D-Monteurs qualifiés en structures métalliques"="624D",
                                    "624E-Ouvriers qualifiés de contrôle et d'essais en mécanique"="624E",
                                    "624F-Ouvriers qualifiés des traitements thermiques et de surface sur métaux"="624F",
                                    "624G-Autres mécaniciens ou ajusteurs qualifiés (ou spécialité non reconnue)"="624G",
                                    "625A-Pilotes d'installation lourde des industries de transformation : agroalimentaire, chimie, plasturgie, énergie"="625A",
                                    "625B-Ouvriers qualifiés et agents qualifiés de laboratoire : agroalimentaire, chimie, biologie, pharmacie"="625B",
                                    "625C-Autres opérateurs et ouvriers qualifiés de la chimie (y.c. pharmacie) et de la plasturgie"="625C",
                                    "625D-Opérateurs de la transformation des viandes"="625D",
                                    "625E-Autres opérateurs et ouvriers qualifiés de l'industrie agricole et alimentaire (hors transformation des viandes)"="625E",
                                    "625H-Ouvriers qualifiés des autres industries (eau, gaz, énergie, chauffage)"="625H",
                                    "626A-Pilotes d'installation lourde des industries de transformation : métallurgie, production verrière, matériaux de construction"="626A",
                                    "626B-Autres opérateurs et ouvriers qualifiés : métallurgie, production verrière, matériaux de construction"="626B",
                                    "626C-Opérateurs et ouvriers qualifiés des industries lourdes du bois et de la fabrication du papier-carton"="626C",
                                    "627A-Opérateurs qualifiés du textile et de la mégisserie"="627A",
                                    "627B-Ouvriers qualifiés de la coupe des vêtements et de l'habillement, autres opérateurs de confection qualifiés"="627B",
                                    "627C-Ouvriers qualifiés du travail industriel du cuir"="627C",
                                    "627D-Ouvriers qualifiés de scierie, de la menuiserie industrielle et de l'ameublement"="627D",
                                    "627E-Ouvriers de la photogravure et des laboratoires photographiques et cinématographiques"="627E",
                                    "627F-Ouvriers de la composition et de l'impression, ouvriers qualifiés de la brochure, de la reliure et du façonnage du papier-carton"="627F",
                                    "628A-Mécaniciens qualifiés de maintenance, entretien : équipements industriels"="628A",
                                    "628B-Électromécaniciens, électriciens qualifiés d'entretien : équipements industriels"="628B",
                                    "628C-Régleurs qualifiés d'équipements de fabrication (travail des métaux, mécanique)"="628C",
                                    "628D-Régleurs qualifiés d'équipements de fabrication (hors travail des métaux et mécanique)"="628D",
                                    "628E-Ouvriers qualifiés de l'assainissement et du traitement des déchets"="628E",
                                    "628F-Agents qualifiés de laboratoire (sauf chimie, santé)"="628F",
                                    "628G-Ouvriers qualifiés divers de type industriel"="628G",
                                    "631A-Jardiniers"="631A",
                                    "632A-Maçons qualifiés"="632A",
                                    "632B-Ouvriers qualifiés du travail de la pierre"="632B",
                                    "632C-Charpentiers en bois qualifiés"="632C",
                                    "632D-Menuisiers qualifiés du bâtiment"="632D",
                                    "632E-Couvreurs qualifiés"="632E",
                                    "632F-Plombiers et chauffagistes qualifiés"="632F",
                                    "632G-Peintres et ouvriers qualifiés de pose de revêtements sur supports verticaux"="632G",
                                    "632H-Soliers moquetteurs et ouvriers qualifiés de pose de revêtements souples sur supports horizontaux"="632H",
                                    "632J-Monteurs qualifiés en agencement, isolation"="632J",
                                    "632K-Ouvriers qualifiés d'entretien général des bâtiments"="632K",
                                    "633A-Électriciens qualifiés de type artisanal (y.c. bâtiment)"="633A",
                                    "633B-Dépanneurs qualifiés en radiotélévision, électroménager, matériel électronique (salariés)"="633B",
                                    "633C-Électriciens, électroniciens qualifiés en maintenance entretien, réparation : automobile"="633C",
                                    "633D-Électriciens, électroniciens qualifiés en maintenance, entretien : équipements non industriels"="633D",
                                    "634A-Carrossiers d'automobiles qualifiés"="634A",
                                    "634B-Métalliers, serruriers qualifiés"="634B",
                                    "634C-Mécaniciens qualifiés en maintenance, entretien, réparation : automobile"="634C",
                                    "634D-Mécaniciens qualifiés de maintenance, entretien : équipements non industriels"="634D",
                                    "635A-Tailleurs et couturières qualifiés, ouvriers qualifiés du travail des étoffes (sauf fabrication de vêtements), ouvriers qualifiés de type artisanal du travail du cuir"="635A",
                                    "636A-Bouchers (sauf industrie de la viande)"="636A",
                                    "636B-Charcutiers (sauf industrie de la viande)"="636B",
                                    "636C-Boulangers, pâtissiers (sauf activité industrielle)"="636C",
                                    "636D-Cuisiniers et commis de cuisine"="636D",
                                    "637A-Modeleurs (sauf modeleurs de métal), mouleurs-noyauteurs à la main, ouvriers qualifiés du travail du verre ou de la céramique à la main"="637A",
                                    "637B-Ouvriers d'art"="637B",
                                    "637C-Ouvriers et techniciens des spectacles vivants et audiovisuels"="637C",
                                    "637D-Ouvriers qualifiés divers de type artisanal"="637D",
                                    "641A-Conducteurs routiers et grands routiers (salariés)"="641A",
                                    "641B-Conducteurs de véhicule routier de transport en commun (salariés)"="641B",
                                    "642A-Conducteurs de taxi (salariés)"="642A",
                                    "642B-Conducteurs de voiture particulière (salariés)"="642B",
                                    "643A-Conducteurs livreurs, coursiers (salariés)"="643A",
                                    "644A-Conducteurs de véhicule de ramassage des ordures ménagères"="644A",
                                    "651A-Conducteurs d'engin lourd de levage"="651A",
                                    "651B-Conducteurs d'engin lourd de manœuvre"="651B",
                                    "652A-Ouvriers qualifiés de la manutention, conducteurs de chariots élévateurs, caristes"="652A",
                                    "652B-Dockers"="652B",
                                    "653A-Magasiniers qualifiés"="653A",
                                    "654A-Conducteurs qualifiés d'engins de transport guidés"="654A",
                                    "655A-Autres agents et ouvriers qualifiés (sédentaires) des services d'exploitation des transports"="655A",
                                    "656A-Matelots de la marine marchande, capitaines et matelots timoniers de la navigation fluviale (salariés)"="656A",
                                    "671A-Ouvriers non qualifiés des travaux publics de l'État et des collectivités locales"="671A",
                                    "671B-Ouvriers non qualifiés des travaux publics, du travail du béton et de l'extraction, hors État et collectivités locales"="671B",
                                    "672A-Ouvriers non qualifiés de l'électricité et de l'électronique"="672A",
                                    "673A-Ouvriers de production non qualifiés travaillant par enlèvement de métal"="673A",
                                    "673B-Ouvriers de production non qualifiés travaillant par formage de métal"="673B",
                                    "673C-Ouvriers non qualifiés de montage, contrôle en mécanique et travail des métaux"="673C",
                                    "674A-Ouvriers de production non qualifiés : chimie, pharmacie, plasturgie"="674A",
                                    "674B-Ouvriers de production non qualifiés de la transformation des viandes"="674B",
                                    "674C-Autres ouvriers de production non qualifiés : industrie agro-alimentaire"="674C",
                                    "674D-Ouvriers de production non qualifiés : métallurgie, production verrière, céramique, matériaux de construction"="674D",
                                    "674E-Ouvriers de production non qualifiés : industrie lourde du bois, fabrication des papiers et cartons"="674E",
                                    "675A-Ouvriers de production non qualifiés du textile et de la confection, de la tannerie-mégisserie et du travail du cuir"="675A",
                                    "675B-Ouvriers de production non qualifiés du travail du bois et de l'ameublement"="675B",
                                    "675C-Ouvriers de production non qualifiés de l'imprimerie, presse, édition"="675C",
                                    "676A-Manutentionnaires non qualifiés"="676A",
                                    "676B-Déménageurs (hors chauffeurs-déménageurs), non qualifiés"="676B",
                                    "676C-Ouvriers du tri, de l'emballage, de l'expédition, non qualifiés"="676C",
                                    "676D-Agents non qualifiés des services d'exploitation des transports"="676D",
                                    "676E-Ouvriers non qualifiés divers de type industriel"="676E",
                                    "681A-Ouvriers non qualifiés du gros œuvre du bâtiment"="681A",
                                    "681B-Ouvriers non qualifiés du second œuvre du bâtiment"="681B",
                                    "682A-Métalliers, serruriers, réparateurs en mécanique non qualifiés"="682A",
                                    "683A-Apprentis boulangers, bouchers, charcutiers"="683A",
                                    "684A-Nettoyeurs"="684A",
                                    "684B-Ouvriers non qualifiés de l'assainissement et du traitement des déchets"="684B",
                                    "685A-Ouvriers non qualifiés divers de type artisanal"="685A",
                                    "691A-Conducteurs d'engin agricole ou forestier"="691A",
                                    "691B-Ouvriers de l'élevage"="691B",
                                    "691C-Ouvriers du maraîchage ou de l'horticulture"="691C",
                                    "691D-Ouvriers de la viticulture ou de l'arboriculture fruitière"="691D",
                                    "691E-Ouvriers agricoles sans spécialisation particulière"="691E",
                                    "691F-Ouvriers de l'exploitation forestière ou de la sylviculture"="691F",
                                    "692A-Marins-pêcheurs et ouvriers de l'aquaculture"="692A",
                                    
  ) } 
if("DEMPRO" %in% names(ct) & !is.numeric(ct$DEMPRO) ) {
  ct$DEMPRO <- forcats::fct_recode(ctraw$DEMPRO,
                                   
                                   "1-Déménagement pro"="1",
                                   "2-Pas dem pro"="2",
                                   "NULL"="8",
                                   "2-Pas dem pro"="9",
                                   
  ) } 
if("DEMENA" %in% names(ct) & !is.numeric(ct$DEMENA) ) {
  ct$DEMENA <- forcats::fct_recode(ctraw$DEMENA,
                                   
                                   "1-Déménagement csq pro"="1",
                                   "2-Pas dem csp pro"="2",
                                   "NULL"="8",
                                   "2-Pas dem csp pro"="9",
                                   
  ) } 


# Variables seulement dans ct13
if("MONOTON" %in% names(ct) & !is.numeric(ct$MONOTON) ) {
  ct$MONOTON <- forcats::fct_recode(ctraw$MONOTON,
                                    
                                    "1-Tj monotone"="1", #  2 044 (7.0%)
                                    "2-Sv monotone"="2", #  3 767 (13 %)
                                    "3-Pf monotone"="3", # 10 001 (34 %)
                                    "4-Jm monotone"="4", # 13 252 (46 %)
                                    "NULL" = "8", 
                                    "NULL" = "9"
  ) 
  
  # ct$MONOTON2 <- forcats::fct_recode(ctraw$MONOTON,
  #                                   
  #                                   "1-Sv monotone"="1", #  2 044 (7.0%)
  #                                   "1-Sv monotone"="2", #  3 767 (13 %)
  #                                   "2-Pf monotone"="3", # 10 001 (34 %)
  #                                   "3-Jm monotone"="4", # 13 252 (46 %)
  #                                   "NULL" = "8", 
  #                                   "NULL" = "9"
  # ) 
}

if("POLY" %in% names(ct) & !is.numeric(ct$POLY) ) {
  ct$POLY <- forcats::fct_recode(ctraw$POLY,
                                 
                                 "1-Rotation postes"="1",
                                 "2-Chg poste au besoin"="2",
                                 "3-Poste unique"="3",
                                 "NULL" = "8", 
                                 "NULL" = "9"
                                 
  ) }

if("POLYQUAL" %in% names(ct) & !is.numeric(ct$POLYQUAL) ) {
  ct$POLYQUAL <- forcats::fct_recode(ctraw$POLYQUAL,
                                     
                                     "1-Poly positive"="1",
                                     "2-Poly négative"="2",
                                     "3-Poly neutre"="3",
                                     "NULL" = "8", 
                                     "NULL" = "9"
                                     
  ) }

if("AIDAUTR" %in% names(ct) & !is.numeric(ct$AIDAUTR) ) {
  ct$AIDAUTR <- forcats::fct_recode(ctraw$AIDAUTR,
                                    
                                    "1-Aide autres"="1",
                                    "2-Pas aide autres"="2",
                                    "NULL"="3",
                                    "NULL" = "8", 
                                    "NULL" = "9"
                                    
  ) }

if("AIDEXT" %in% names(ct) & !is.numeric(ct$AIDEXT) ) {
  ct$AIDEXT <- forcats::fct_recode(ctraw$AIDEXT,
                                   
                                   "1-Aide extérieure"="1",
                                   "2-Pas aide ext"="2",
                                   "NULL" = "8", 
                                   "NULL" = "9"
                                   
  ) }




















# Auto-questionnaire ---------------------------------------------------------------------
#Variables non sélectionnées ci-dessus, car directement renommées (names and levels) :
#Vie personnelle 
if("RP6" %in% names(ctraw)  ) {
  ct$RP_SUICIDE <- forcats::fct_recode(ctraw$RP6,
                                       "1-Pensé suicide"="1",
                                       "2-Pas pensé suicide"="2",
                                       "NULL"="8",
                                       
  ) } 
if("RP7A" %in% names(ctraw)  ) {
  ct$RP_SUICIDE_PRO <- forcats::fct_recode(ctraw$RP7A,
                                           
                                           "1-Pensé suicide raisons pro"="1",
                                           "2-Pas pensé suicide pro"="2",
                                           "NULL"="8",
                                           
  ) } 

if("RP9" %in% names(ctraw)  ) {
  ct$RP_SUICIDE_T <- forcats::fct_recode(ctraw$RP9,
                                         
                                         "1-Déjà fait une TS"="1",
                                         "2-Pas TS"="2",
                                         "NULL"="8",
                                         
  ) } 
if("RP10" %in% names(ctraw)  ) {
  ct$RP_SUICIDE_T2 <- forcats::fct_recode(ctraw$RP10,
                                          
                                          "1-TS cette année"="1",
                                          "2-Pas TS année"="2",
                                          "NULL"="8",
                                          
  ) } 

#Satisfaction et difficultés au travail (dans l'année)
if("RPB1A" %in% names(ctraw)  ) {
  ct$RP_SUB_IGNORE <- forcats::fct_recode(ctraw$RPB1A,
                                          
                                          "1-Comme si pas là"="1",
                                          "2-Pas ignore"="2",
                                          "NULL"="8",
                                          "2-Pas ignore"="9",
                                          
  ) } 
if("RPB1B" %in% names(ctraw)  ) {
  ct$RP_SUB_TAIRE <- forcats::fct_recode(ctraw$RPB1B,
                                         
                                         "1-Empêchent exprimer"="1",
                                         "2-Pas empêchent exprimer"="2",
                                         "NULL"="8",
                                         "2-Pas empêchent exprimer"="9",
                                         
  ) } 
if("RPB1C" %in% names(ctraw)  ) {
  ct$RP_SUB_RIDICULISE <- forcats::fct_recode(ctraw$RPB1C,
                                              
                                              "1-Ridiculisent en public"="1",
                                              "2-Pas ridiculisent"="2",
                                              "NULL"="8",
                                              "2-Pas ridiculisent"="9",
                                              
  ) } 
if("RPB1D" %in% names(ctraw)  ) {
  ct$RP_SUB_CRITIQUE <- forcats::fct_recode(ctraw$RPB1D,
                                            
                                            "1-Critique injustement travail"="1",
                                            "2-Pas critique"="2",
                                            "NULL"="8",
                                            "2-Pas critique"="9",
                                            
  ) } 
if("RPB1E" %in% names(ctraw)  ) {
  ct$RP_SUB_INUTILE <- forcats::fct_recode(ctraw$RPB1E,
                                           
                                           "1-Tâches inutiles/dégradantes"="1",
                                           "2-Pas tâches inutiles"="2",
                                           "NULL"="8",
                                           "2-Pas tâches inutiles"="9",
                                           
  ) } 
if("RPB1F" %in% names(ctraw)  ) {
  ct$RP_SUB_SABOTE <- forcats::fct_recode(ctraw$RPB1F,
                                          
                                          "1-Sabote mon travail"="1",
                                          "2-Pas sabote"="2",
                                          "NULL"="8",
                                          "2-Pas sabote"="9",
                                          
  ) } 
if("RPB1G" %in% names(ctraw)  ) {
  ct$RP_SUB_FOU <- forcats::fct_recode(ctraw$RPB1G,
                                       
                                       "1-Traite de fou"="1",
                                       "2-Pas traite fou"="2",
                                       "NULL"="8",
                                       "2-Pas traite fou"="9",
                                       
  ) } 
if("RPB1H" %in% names(ctraw)  ) {
  ct$RP_SUB_DEGRADANT <- forcats::fct_recode(ctraw$RPB1H,
                                             
                                             "1-Paroles dégradantes"="1",
                                             "2-Pas dégradantes"="2",
                                             "NULL"="8",
                                             "2-Pas dégradantes"="9",
                                             
  ) } 
if("RPB1I" %in% names(ctraw)  ) {
  ct$RP_SUB_AVANCES <- forcats::fct_recode(ctraw$RPB1I,
                                           
                                           "1-Propositions sexuelles"="1",
                                           "2-Pas avances"="2",
                                           "NULL"="8",
                                           "2-Pas avances"="9",
                                           
  ) } 
if("RPB1J" %in% names(ctraw)  ) {
  ct$RP_SUB_MOQUE <- forcats::fct_recode(ctraw$RPB1J,
                                         
                                         "1-Se moque de moi"="1",
                                         "2-Pas moque"="2",
                                         "NULL"="8",
                                         "2-Pas moque"="9",
                                         
  ) } 
if("RPB2A" %in% names(ctraw)  ) {
  ct$RP_SUB_PAR_PUBLIC <- forcats::fct_recode(ctraw$RPB2A,
                                              
                                              "1-Par qq public"="1",
                                              "2-Pas pub"="2",
                                              "NULL"="8",
                                              "2-Pas pub"="9",
                                              
  ) } 
if("RPB2B" %in% names(ctraw)  ) {
  ct$RP_SUB_PAR_ORGA <- forcats::fct_recode(ctraw$RPB2B,
                                            
                                            "1-Par qq orga"="1",
                                            "2-Pas org"="2",
                                            "NULL"="8",
                                            "2-Pas org"="9",
                                            
  ) } 

if("RP13" %in% names(ctraw)  ) { #Si orga
  ct$RP_SUB_PAR_GENRE <- forcats::fct_recode(ctraw$RP13,
                                             
                                             "1-Par hommes orga"="1",
                                             "2-Par femmes orga"="2",
                                             "3-Par deux orga"="3",
                                             "NULL"="8",
                                             "3-Par deux orga"="9",
                                             
  ) } 
if("RP14A" %in% names(ctraw)  ) {
  ct$RP_SUB_PAR_SUPERIEUR <- forcats::fct_recode(ctraw$RP14A,
                                                 
                                                 "1-Par supérieurs"="1",
                                                 "2-Pas sup"="2",
                                                 "NULL"="8",
                                                 "2-Pas sup"="9",
                                                 
  ) } 
if("RP14B" %in% names(ctraw)  ) {
  ct$RP_SUB_PAR_COLLEGUE <- forcats::fct_recode(ctraw$RP14B,
                                                
                                                "1-Par collègues"="1",
                                                "2-Pas coll"="2",
                                                "NULL"="8",
                                                "2-Pas coll"="9",
                                                
  ) } 
if("RPB3A" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_SEXE <- forcats::fct_recode(ctraw$RPB3A,
                                            
                                            "1-Lié sexe"="1",
                                            "2-Pas lié sexe"="2",
                                            "NULL"="8",
                                            "2-Pas lié sexe"="9",
                                            
  ) } 
if("RPB3B" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_HAND <- forcats::fct_recode(ctraw$RPB3B,
                                            
                                            "1-Lié santé handicap"="1",
                                            "2-Pas lié handicap"="2",
                                            "NULL"="8",
                                            "2-Pas lié handicap"="9",
                                            
  ) } 
if("RPB3CBIS" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_ENC <- forcats::fct_recode(ctraw$RPB3CBIS,
                                           
                                           "1-Lié enceinte"="1",
                                           "2-Pas lié enc"="2",
                                           "NULL"="8",
                                           "2-Pas lié enc"="9",
                                           
  ) } 
if("RPB3C" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_RACE <- forcats::fct_recode(ctraw$RPB3C,
                                            
                                            "1-Lié couleur peau"="1",
                                            "2-Pas lié race"="2",
                                            "NULL"="8",
                                            "2-Pas lié race"="9",
                                            
  ) } 
if("RPB3D" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_ORIG <- forcats::fct_recode(ctraw$RPB3D,
                                            
                                            "1-Lié origines"="1",
                                            "2-Pas lié orig"="2",
                                            "NULL"="8",
                                            "2-Pas lié orig"="9",
                                            
  ) } 
if("RPB3E" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_VET <- forcats::fct_recode(ctraw$RPB3E,
                                           
                                           "1-Lié habillement"="1",
                                           "2-Pas lié habil"="2",
                                           "NULL"="8",
                                           "2-Pas lié habil"="9",
                                           
  ) } 
if("RPB3F" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_AGE <- forcats::fct_recode(ctraw$RPB3F,
                                           
                                           "1-Lié âge"="1",
                                           "2-Pas lié âge"="2",
                                           "NULL"="8",
                                           "2-Pas lié âge"="9",
                                           
  ) } 
if("RPB3G" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_OSEX <- forcats::fct_recode(ctraw$RPB3G,
                                            
                                            "1-Lié orient sexuelle"="1",
                                            "2-Pas lié orient"="2",
                                            "NULL"="8",
                                            "2-Pas lié orient"="9",
                                            
  ) } 
if("RPB3H" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_PROF <- forcats::fct_recode(ctraw$RPB3H,
                                            
                                            "1-Lié profession"="1",
                                            "2-Pas lié prof"="2",
                                            "NULL"="8",
                                            "2-Pas lié prof"="9",
                                            
  ) } 
if("RPB3I" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_POIDS <- forcats::fct_recode(ctraw$RPB3I,
                                             
                                             "1-Lié poids taille"="1",
                                             "2-Pas lié poids"="2",
                                             "NULL"="8",
                                             "2-Pas lié poids"="9",
                                             
  ) } 
if("RPB3J" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_SYND <- forcats::fct_recode(ctraw$RPB3J,
                                            
                                            "1-Lié syndicat"="1",
                                            "2-Pas lié synd"="2",
                                            "NULL"="8",
                                            "2-Pas lié synd"="9",
                                            
  ) } 
if("RPB3K" %in% names(ctraw)  ) {
  ct$RP_SUB_RAI_OPIN <- forcats::fct_recode(ctraw$RPB3K,
                                            
                                            "1-Lié opinions"="1",
                                            "2-Pas lié opin"="2",
                                            "NULL"="8",
                                            "2-Pas lié opin"="9",
                                            
  ) } 
if("RPB4A" %in% names(ctraw)  ) {
  ct$RP_AGR_VERB_PUB <- forcats::fct_recode(ctraw$RPB4A,
                                            
                                            "1-Agression verbale public"="1",
                                            "2-Pas verbale public"="2",
                                            "NULL"="8",
                                            "2-Pas verbale public"="9",
                                            
  ) } 
if("RPB4B" %in% names(ctraw)  ) {
  ct$RP_AGR_PHYS_PUB <- forcats::fct_recode(ctraw$RPB4B,
                                            
                                            "1-Agression physique ou sexuelle public"="1",
                                            "2-Pas physique public"="2",
                                            "NULL"="8",
                                            "2-Pas physique public"="9",
                                            
  ) } 
if("RPB4C" %in% names(ctraw)  ) {
  ct$RP_AGR_VERB_COL <- forcats::fct_recode(ctraw$RPB4C,
                                            
                                            "1-Agression verbale collègues ou sup"="1",
                                            "2-Pas verbale collègues"="2",
                                            "NULL"="8",
                                            "2-Pas verbale collègues"="9",
                                            
  ) } 
if("RPB4D" %in% names(ctraw)  ) {
  ct$RP_AGR_PHYS_COL <- forcats::fct_recode(ctraw$RPB4D,
                                            
                                            "1-Agression physique collègues ou sup"="1",
                                            "2-Pas physique collègues"="2",
                                            "NULL"="8",
                                            "2-Pas physique collègues"="9",
                                            
  ) } 
if("RP17" %in% names(ctraw)  ) {
  ct$RP_AGR_PAR_GENRE <- forcats::fct_recode(ctraw$RP17,
                                             
                                             "1-Par hommes"="1",
                                             "2-Par femmes"="2",
                                             "3-Par les deux"="3",
                                             "NULL"="8",
                                             "3-Par les deux"="9",
                                             
  ) } 
if("RP18A" %in% names(ctraw)  ) {
  ct$RP_AGR_PAR_SUP <- forcats::fct_recode(ctraw$RP18A,
                                           
                                           "1-Par un supérieur"="1",
                                           "2-Pas supérieur"="2",
                                           "NULL"="8",
                                           "2-Pas supérieur" ="9",
                                           
  ) } 
if("RP18B" %in% names(ctraw)  ) {
  ct$RP_AGR_PAR_COL <- forcats::fct_recode(ctraw$RP18B,
                                           
                                           "1-Par un collègue"="1",
                                           "2-Pas collègue"="2",
                                           "NULL"="8",
                                           "2-Pas collègue"="9",
                                           
  ) } 

#Relations avec les autres au travail 
if("RPA1A" %in% names(ctraw)  ) {
  ct$RP_SUP_ATTE <- forcats::fct_recode(ctraw$RPA1A,
                                        
                                        "1-Sup attention: PDT d’accord"="1",
                                        "2-Sup attention: Pas d’accord"="2",
                                        "3-Sup attention: D’accord"="3",
                                        "4-Sup attention: TAF d’accord"="4",
                                        "5-Non concerné"="5",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPA1B" %in% names(ctraw)  ) {
  ct$RP_SUP_AIDE <- forcats::fct_recode(ctraw$RPA1B,
                                        
                                        "1-Supérieur aide: PDT d’accord"="1",
                                        "2-Supérieur aide: Pas d’accord"="2",
                                        "3-Supérieur aide: D’accord"="3",
                                        "4-Supérieur aide: TAF d’accord"="4",
                                        "5-Non concerné"="5",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPA1C" %in% names(ctraw)  ) {
  ct$RP_SUP_EVAL <- forcats::fct_recode(ctraw$RPA1C,
                                        
                                        "1-Éval connaissent: PDT d’accord"="1",
                                        "2-Éval connaissent: Pas d’accord"="2",
                                        "3-Éval connaissent: D’accord"="3",
                                        "4-Éval connaissent: TAF d’accord"="4",
                                        "5-Non concerné"="5",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) 
  
  ct$RP_SUP_EVAL2 <- forcats::fct_recode(ctraw$RPA1C,
                                         
                                         "1-Éval connaissent pas"     = "1",
                                         "1-Éval connaissent pas"     = "2",
                                         "2-Éval connaissent travail" = "3",
                                         "2-Éval connaissent travail" = "4",
                                         "3-Éval NC"                  = "5",
                                         "NULL"="8",
                                         "NULL"="9",
                                         
  ) 
  
  
} 
if("RPA1E" %in% names(ctraw)  ) {
  ct$RP_COL_AIDE <- forcats::fct_recode(ctraw$RPA1E,
                                        
                                        "1-Collègues aident: PDT d’accord"="1",
                                        "2-Collègues aident: Pas d’accord"="2",
                                        "3-Collègues aident: D’accord"="3",
                                        "4-Collègues aident: TAF d’accord"="4",
                                        "5-Non concerné"="5",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPA1F" %in% names(ctraw)  ) {
  ct$RP_COL_AMIC <- forcats::fct_recode(ctraw$RPA1F,
                                        
                                        "1-Collègues amicaux: PDT d’accord"="1",
                                        "2-Collègues amicaux: Pas d’accord"="2",
                                        "3-Collègues amicaux: D’accord"="3",
                                        "4-Collègues amicaux: TAF d’accord"="4",
                                        "5-Non concerné"="5",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPA1G" %in% names(ctraw)  ) {
  ct$RP_ESTIME <- forcats::fct_recode(ctraw$RPA1G,
                                      
                                      "1-Estime mérite: PDT d’accord"="1",
                                      "2-Estime mérite: Pas d’accord"="2",
                                      "3-Estime mérite: D’accord"="3",
                                      "4-Estime mérite: TAF d’accord"="4",
                                      "5-Non concerné"="5",
                                      "NULL"="8",
                                      "NULL"="9",
                                      
  ) } 
if("RPA1H" %in% names(ctraw)  ) {
  ct$RP_PROMOTION <- forcats::fct_recode(ctraw$RPA1H,
                                         
                                         "1-Persp promotion: PDT d’accord"="1",
                                         "2-Persp promotion: Pas d’accord"="2",
                                         "3-Persp promotion: D’accord"="3",
                                         "4-Persp promotion: TAF d’accord"="4",
                                         "5-Non concerné"="5",
                                         "NULL"="8",
                                         "NULL"="9",
                                         
  ) } 
if("RPA1I" %in% names(ctraw)  ) {
  ct$RP_QUANTITE <- forcats::fct_recode(ctraw$RPA1I,
                                        
                                        "1-Quantité excessive: PDT d’accord"="1",
                                        "2-Quantité excessive: Pas d’accord"="2",
                                        "3-Quantité excessive: D’accord"="3",
                                        "4-Quantité excessive: TAF d’accord"="4",
                                        "5-Non concerné"="5",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPA1J" %in% names(ctraw)  ) {
  ct$RP_ORGA <- forcats::fct_recode(ctraw$RPA1J,
                                    
                                    "1-Peut s'organiser: PDT d’accord"="1",
                                    "2-Peut s'organiser: Pas d’accord"="2",
                                    "3-Peut s'organiser: D’accord"="3",
                                    "4-Peut s'organiser: TAF d’accord"="4",
                                    "5-Non concerné"="5",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("RPA1K" %in% names(ctraw)  ) {
  ct$RP_FINANCIEREMENT <- forcats::fct_recode(ctraw$RPA1K,
                                              
                                              "1-À l'abri financièrement: PDT d’accord"="1",
                                              "2-À l'abri financièrement: Pas d’accord"="2",
                                              "3-À l'abri financièrement: D’accord"="3",
                                              "4-À l'abri financièrement: TAF d’accord"="4",
                                              "5-Non concerné"="5",
                                              "NULL"="8",
                                              "NULL"="9",
                                              
  ) } 
if("RPA1L" %in% names(ctraw)  ) {
  ct$RP_COMPETENCES <- forcats::fct_recode(ctraw$RPA1L,
                                           
                                           "1-Développe compétences: PDT d’accord"="1",
                                           "2-Développe compétences: Pas d’accord"="2",
                                           "3-Développe compétences: D’accord"="3",
                                           "4-Développe compétences: TAF d’accord"="4",
                                           "5-Non concerné"="5",
                                           "NULL"="8",
                                           "NULL"="9",
                                           
  ) } 
if("RPA1M" %in% names(ctraw)  ) {
  ct$RP_EQUITABLE <- forcats::fct_recode(ctraw$RPA1M,
                                         
                                         "1-Traitement équitable: PDT d’accord"="1",
                                         "2-Traitement équitable: Pas d’accord"="2",
                                         "3-Traitement équitable: D’accord"="3",
                                         "4-Traitement équitable: TAF d’accord"="4",
                                         "5-Non concerné"="5",
                                         "NULL"="8",
                                         "NULL"="9",
                                         
  ) } 
if("RP20" %in% names(ctraw)  ) {
  ct$RP_SEXISTE <- forcats::fct_recode(ctraw$RP20,
                                       
                                       "1-Propos sexistes: Toujours"="1",
                                       "2-Propos sexistes: Souvent"="2",
                                       "3-Propos sexistes: Parfois"="3",
                                       "4-Propos sexistes: Jamais"="4",
                                       "NULL"="8",
                                       "NULL"="9",
                                       
  ) } 
if("RP21" %in% names(ctraw)  ) {
  ct$RP_SEXISTE2 <- forcats::fct_recode(ctraw$RP21,
                                        
                                        "1-Propos sexistes: dérangent bcp"="1",
                                        "2-Propos sexistes: dérangent un peu"="2",
                                        "3-Propos sexistes: dérangent pas"="3",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPA2A" %in% names(ctraw)  ) {
  ct$RP_PRESSION <- forcats::fct_recode(ctraw$RPA2A,
                                        
                                        "1-Pression: Toujours"="1",
                                        "2-Pression: Souvent"="2",
                                        "3-Pression: Parfois"="3",
                                        "4-Pression: Jamais"="4",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPA2B" %in% names(ctraw)  ) {
  ct$RP_CHR_MENTALE <- forcats::fct_recode(ctraw$RPA2B,
                                           
                                           "1-Charge mentale: Toujours"="1",
                                           "2-Charge mentale: Souvent"="2",
                                           "3-Charge mentale: Parfois"="3",
                                           "4-Charge mentale: Jamais"="4",
                                           "NULL"="8",
                                           "NULL"="9",
                                           
  ) } 
if("RPA2C" %in% names(ctraw)  ) {
  ct$RP_PENSER_TRAVAIL <- forcats::fct_recode(ctraw$RPA2C,
                                              
                                              "1-Penser travail après: Toujours"="1",
                                              "2-Penser travail après: Souvent"="2",
                                              "3-Penser travail après: Parfois"="3",
                                              "4-Penser travail après: Jamais"="4",
                                              "NULL"="8",
                                              "NULL"="9",
                                              
  ) } 
if("RPA2D" %in% names(ctraw)  ) {
  ct$RP_TROP_VITE <- forcats::fct_recode(ctraw$RPA2D,
                                         
                                         "1-Trop vite: Toujours"="1",
                                         "2-Trop vite: Souvent"="2",
                                         "3-Trop vite: Parfois"="3",
                                         "4-Trop vite: Jamais"="4",
                                         "NULL"="8",
                                         "NULL"="9",
                                         
  ) } 
if("RPA2E" %in% names(ctraw)  ) {
  ct$RP_ANTICIPER <- forcats::fct_recode(ctraw$RPA2E,
                                         
                                         "1-Peut anticiper mois sv : Toujours"="1",
                                         "2-Peut anticiper mois sv : Souvent"="2",
                                         "3-Peut anticiper mois sv : Parfois"="3",
                                         "4-Peut anticiper mois sv : Jamais"="4",
                                         "NULL"="8",
                                         "NULL"="9",
                                         
  ) } 
if("RPA2F" %in% names(ctraw)  ) {
  ct$RP_CHANGEMENTS <- forcats::fct_recode(ctraw$RPA2F,
                                           
                                           "1-Chg mal préparés: Toujours"="1",
                                           "2-Chg mal préparés: Souvent"="2",
                                           "3-Chg mal préparés: Parfois"="3",
                                           "4-Chg mal préparés: Jamais"="4",
                                           "NULL"="8",
                                           "NULL"="9",
                                           
  ) } 
if("RPA2G" %in% names(ctraw)  ) {
  ct$RP_PLAISE <- forcats::fct_recode(ctraw$RPA2G,
                                      
                                      "1-Choses qui me plaisent: Toujours"="1",
                                      "2-Choses qui me plaisent: Souvent"="2",
                                      "3-Choses qui me plaisent: Parfois"="3",
                                      "4-Choses qui me plaisent: Jamais"="4",
                                      "NULL"="8",
                                      "NULL"="9",
                                      
  ) } 
if("RPA2H" %in% names(ctraw)  ) {
  ct$RP_DESAPPROUVE <- forcats::fct_recode(ctraw$RPA2H,
                                           
                                           "1-Choses que je désapprouve: Toujours"="1",
                                           "2-Choses que je désapprouve: Souvent"="2",
                                           "3-Choses que je désapprouve: Parfois"="3",
                                           "4-Choses que je désapprouve: Jamais"="4",
                                           "NULL"="8",
                                           "NULL"="9",
                                           
  ) } 
if("RP22I" %in% names(ctraw)  ) {
  ct$RP_MAUVAIS_TRAVAIL <- forcats::fct_recode(ctraw$RP22I,
                                               
                                               "1-Sacrifier la qualité: Toujours"="1",
                                               "2-Sacrifier la qualité: Souvent"="2",
                                               "3-Sacrifier la qualité: Parfois"="3",
                                               "4-Sacrifier la qualité: Jamais"="4",
                                               "NULL"="8",
                                               "NULL"="9",
                                               
  ) } 
if("RP22J" %in% names(ctraw)  ) {
  ct$RP_INUTILE <- forcats::fct_recode(ctraw$RP22J,
                                       
                                       "1-Choses inutiles: Toujours"="1",
                                       "2-Choses inutiles: Souvent"="2",
                                       "3-Choses inutiles: Parfois"="3",
                                       "4-Choses inutiles: Jamais"="4",
                                       "NULL"="8",
                                       "NULL"="9",
                                       
  ) } 
if("RP22K" %in% names(ctraw)  ) {
  ct$RP_MENTIR <- forcats::fct_recode(ctraw$RP22K,
                                      
                                      "1-Mentir: Toujours"="1",
                                      "2-Mentir: Souvent"="2",
                                      "3-Mentir: Parfois"="3",
                                      "4-Mentir: Jamais"="4",
                                      "NULL"="8",
                                      "NULL"="9",
                                      
  ) } 
if("RP22L" %in% names(ctraw)  ) {
  ct$RP_RISQUE_SANTE <- forcats::fct_recode(ctraw$RP22L,
                                            
                                            "1-Risques santé: Toujours"="1",
                                            "2-Risques santé: Souvent"="2",
                                            "3-Risques santé: Parfois"="3",
                                            "4-Risques santé: Jamais"="4",
                                            "NULL"="8",
                                            "NULL"="9",
                                            
  ) } 
if("RP22M" %in% names(ctraw)  ) {
  ct$RP_RISQUE_AUTRUI <- forcats::fct_recode(ctraw$RP22M,
                                             
                                             "1-Risques santé d'autrui: Toujours"="1",
                                             "2-Risques santé d'autrui: Souvent"="2",
                                             "3-Risques santé d'autrui: Parfois"="3",
                                             "4-Risques santé d'autrui: Jamais"="4",
                                             "NULL"="8",
                                             "NULL"="9",
                                             
  ) } 
if("RP22N" %in% names(ctraw)  ) {
  ct$RP_INJUSTEMENT <- forcats::fct_recode(ctraw$RP22N,
                                           
                                           "1-Traiter injustement: Toujours"="1",
                                           "2-Traiter injustement: Souvent"="2",
                                           "3-Traiter injustement: Parfois"="3",
                                           "4-Traiter injustement: Jamais"="4",
                                           "NULL"="8",
                                           "NULL"="9",
                                           
  ) } 
if("RP22O" %in% names(ctraw)  ) {
  ct$RP_CONTRAINDRE <- forcats::fct_recode(ctraw$RP22O,
                                           
                                           "1-Exercer contrainte: Toujours"="1",
                                           "2-Exercer contrainte: Souvent"="2",
                                           "3-Exercer contrainte: Parfois"="3",
                                           "4-Exercer contrainte: Jamais"="4",
                                           "NULL"="8",
                                           "NULL"="9",
                                           
  ) } 
if("RP22P" %in% names(ctraw)  ) {
  ct$RP_PAS_RESSORT <- forcats::fct_recode(ctraw$RP22P,
                                           
                                           "1-Pas de mon ressort: Toujours"="1",
                                           "2-Pas de mon ressort: Souvent"="2",
                                           "3-Pas de mon ressort: Parfois"="3",
                                           "4-Pas de mon ressort: Jamais"="4",
                                           "NULL"="8",
                                           "NULL"="9",
                                           
  ) } 
if("RPA2I" %in% names(ctraw)  ) {
  ct$RP_CACHER_EMOTION <- forcats::fct_recode(ctraw$RPA2I,
                                              
                                              "1-Cacher émotions: Toujours"="1",
                                              "2-Cacher émotions: Souvent"="2",
                                              "3-Cacher émotions: Parfois"="3",
                                              "4-Cacher émotions: Jamais"="4",
                                              "NULL"="8",
                                              "NULL"="9",
                                              
  ) } 

if("RPA2J" %in% names(ctraw)  ) {
  ct$RP_PAS_AVIS <- forcats::fct_recode(ctraw$RPA2J,
                                        
                                        "1-Éviter avis: Toujours"="1",
                                        "2-Éviter avis: Souvent"="2",
                                        "3-Éviter avis: Parfois"="3",
                                        "4-Éviter avis: Jamais"="4",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPA2K" %in% names(ctraw)  ) {
  ct$RP_PEUR <- forcats::fct_recode(ctraw$RPA2K,
                                    
                                    "1-Peur sécurité: Toujours"="1",
                                    "2-Peur sécurité: Souvent"="2",
                                    "3-Peur sécurité: Parfois"="3",
                                    "4-Peur sécurité: Jamais"="4",
                                    "NULL"="8",
                                    "NULL"="9",
                                    
  ) } 
if("RPA2L" %in% names(ctraw)  ) {
  ct$RP_FIER_ORGA <- forcats::fct_recode(ctraw$RPA2L,
                                         
                                         "1-Fier orga: Toujours"="1",
                                         "2-Fier orga: Souvent"="2",
                                         "3-Fier orga: Parfois"="3",
                                         "4-Fier orga: Jamais"="4",
                                         "NULL"="8",
                                         "NULL"="9",
                                         
  ) } 
if("RPB5A" %in% names(ctraw)  ) {
  ct$RPE_FIERTE <- forcats::fct_recode(ctraw$RPB5A,
                                       
                                       "1-Fierté: Toujours"="1",
                                       "2-Fierté: Souvent"="2",
                                       "3-Fierté: Parfois"="3",
                                       "4-Fierté: Jamais"="4",
                                       "NULL"="8",
                                       "NULL"="9",
                                       
  ) } 
if("RPB5B" %in% names(ctraw)  ) {
  ct$RPE_UTILE <- forcats::fct_recode(ctraw$RPB5B,
                                      
                                      "1-Utile: Toujours"="1",
                                      "2-Utile: Souvent"="2",
                                      "3-Utile: Parfois"="3",
                                      "4-Utile: Jamais"="4",
                                      "NULL"="8",
                                      "NULL"="9",
                                      
  ) } 
if("RPB5C" %in% names(ctraw)  ) {
  ct$RPE_EXLOITE <- forcats::fct_recode(ctraw$RPB5C,
                                        
                                        "1-Exploité: Toujours"="1",
                                        "2-Exploité: Souvent"="2",
                                        "3-Exploité: Parfois"="3",
                                        "4-Exploité: Jamais"="4",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPB5D" %in% names(ctraw)  ) {
  ct$RPE_DEPASSE <- forcats::fct_recode(ctraw$RPB5D,
                                        
                                        "1-Dépassé chg: Toujours"="1",
                                        "2-Dépassé chg: Souvent"="2",
                                        "3-Dépassé chg: Parfois"="3",
                                        "4-Dépassé chg: Jamais"="4",
                                        "NULL"="8",
                                        "NULL"="9",
                                        
  ) } 
if("RPB5E" %in% names(ctraw)  ) {
  ct$RPE_ENNUI <- forcats::fct_recode(ctraw$RPB5E,
                                      
                                      "1-Ennui: Toujours"="1", #  	583 (2.1%)
                                      "2-Ennui: Souvent" ="2", # 	1,829 (6.6%)
                                      "3-Ennui: Parfois" ="3", # 	8,671 ( 31%)
                                      "4-Ennui: Jamais"  ="4", # 16,492 ( 60%)
                                      "NULL"="8",
                                      "NULL"="9",
                                      
  )
  
  ct$RPE_ENNUI2 <- forcats::fct_recode(ctraw$RPB5E,
                                       
                                       "1-Ennui souvent" = "1", #  	583 (2.1%)
                                       "1-Ennui souvent" = "2", # 	1,829 (6.6%)
                                       "2-Ennui parfois" = "3", # 	8,671 ( 31%)
                                       "3-Ennui jamais"  = "4", # 16,492 ( 60%)
                                       "NULL"="8",
                                       "NULL"="9",
                                       
  )
} 
if("RPB5F" %in% names(ctraw)  ) {
  ct$RPE_EQUIPE <- forcats::fct_recode(ctraw$RPB5F,
                                       
                                       "1-Faire partie équipe: Toujours"="1",
                                       "2-Faire partie équipe: Souvent"="2",
                                       "3-Faire partie équipe: Parfois"="3",
                                       "4-Faire partie équipe: Jamais"="4",
                                       "NULL"="8",
                                       "NULL"="9",
                                       
  ) } 
if("RPB6" %in% names(ctraw)  ) {
  ct$RPE_TRAVAIL_SANTE <- forcats::fct_recode(ctraw$RPB6,
                                              
                                              "1-Mon travail n’influence pas ma santé"="1",
                                              "2-Mon travail est plutôt bon pour ma santé"="2",
                                              "3-Mon travail est plutôt mauvais pour ma santé"="3",
                                              "NULL"="8",
                                              "NULL"="9",
                                              
  ) } 
if("RPC2" %in% names(ctraw)  ) {
  ct$RP_MEME_ENFANT <- forcats::fct_recode(ctraw$RPC2,
                                           
                                           "1-Heureux enfants même travail"="1",
                                           "2-Pas heureux enf même travail"="2",
                                           "NULL"="8",
                                           "NULL"="9",
                                           
  ) } 

ct$RP_SATISFAIT10      <- ctraw$RP35
ct$RP_SATISFAIT_PRIV10 <- ctraw$RP36
ct$RP_WHO5             <- ctraw$WHO5



# # Les questions le public agresse / qq de l'orga agresse sont inversées dans le dico des 
# #  codes (mauvais nom de variable au départ ?) ?
# tab_many(ct[salariat & `2016` & !is.na(ct$pondqaa), ],
#          RP_SUB_PAR_SUPERIEUR, RP_SUB_PAR_COLLEGUE, RP_SUB_PAR_PUBLIC, cleannames = TRUE)
# tab_many(ct[salariat & `2016` & !is.na(ct$pondqaa), ],
#          RP_SUB_PAR_SUPERIEUR, RP_SUB_PAR_COLLEGUE, RP_SUB_PAR_ORGA, cleannames = TRUE)
# tab_many(ct[salariat & !is.na(ct$pondqaa), ],
#           c(RP_SUB_PAR_PUBLIC, RP_SUB_PAR_ORGA), ANNEE, pct = "col", cleannames = TRUE)

ct$RP_SUB_PAR_PUBLIC2 <- ct$RP_SUB_PAR_ORGA   |> `levels<-`(levels(ct$RP_SUB_PAR_PUBLIC))
ct$RP_SUB_PAR_ORGA   <- ct$RP_SUB_PAR_PUBLIC |> `levels<-`(levels(ct$RP_SUB_PAR_ORGA))
ct$RP_SUB_PAR_PUBLIC <- ct$RP_SUB_PAR_PUBLIC2 
ct$RP_SUB_PAR_PUBLIC2 <- NULL

ct <- ct |>
  mutate(across(c(RP_SUB_PAR_PUBLIC, RP_SUB_PAR_ORGA, RP_SUB_PAR_SUPERIEUR, 
                  RP_SUB_PAR_COLLEGUE), 
                ~ if_else(ANNEE %in% c(2013, 2016) & !is.na(pondqaa), 
                          fct_explicit_na(., levels(.)[2]), 
                          .) 
  ))
# tab_many(ct, starts_with("RP_SUB_PAR"), ANNEE)




#Variables spécifiques à CT2005 ------------------------------------------------
# c("NAF_05"      = "NAF", 
#   "NAF36_05"    = "NAFG36U", 
#   "NAF16_05"    = "NAFG16",
#   "NAF4_05"     = "NAFG4",
#   "CJUR_05"     = "CJUR", 
#  )

ct$NEWTEFEN_05 <- fct_recode(
  ctraw$NEWTEFEN_05,
  "0-Pas de salariés"     = "0",
  "1-1 à 9 salariés"      = "1",
  "2-10 à 49 salariés"    = "2",
  "3-50 à 199 salariés"   = "3",
  "4-200 à 499 salariés"  = "4",
  "5-500 à 1000 salariés" = "5",
  "6-1000 salariés ou +"  = "6",
  "NULL"                  = "N"
)

ct$NEWTEFET_05 <- fct_recode(
  ctraw$NEWTEFET_05,
  "0-Pas de salariés"     = "0",
  "1-1 à 9 salariés"      = "1",
  "2-10 à 49 salariés"    = "2",
  "3-50 à 199 salariés"   = "3",
  "4-200 à 499 salariés"  = "4",
  "5-500 à 1000 salariés" = "5",
  "6-1000 salariés ou +"  = "6",
  "NULL"                  = "N"
)

ct$CHEF_05 <- fct_recode(
  ctraw$CHEF_05,
  "1-Encadrant"     = "1",
  "2-Pas encadrant" = "2"
)

ct$CHEFPROM_05 <- fct_recode(
  ctraw$CHEFPROM_05,
  "1-Promotion dépend de moi" = "1",
  "2-Pas promotion dépend" = "2"
)

ct$NEWPUB_05 <- forcats::fct_relabel(ctraw$NEWPUB_05, ~ case_when(
  str_detect(., "^1")   ~ "1-État"                  ,
  str_detect(., "^2")   ~ "2-Collectivités locales" ,
  str_detect(., "^3")   ~ "3-Hôpitaux publics"      ,
  str_detect(., "^4")   ~ "4-Sécurité Sociale"      ,
  str_detect(., "^5")   ~ "5-Entreprises publiques" ,
  str_detect(., "^6")   ~ "6-Particuliers"          ,
  str_detect(., "^7")   ~ "7-Entreprises privées"   ,
))


if("DIP_05" %in% names(ctraw) & !is.numeric(ctraw$DIP_05) ) {
  ct$DIP_05 <- fct_recode(ctraw$DIP_05,
                          "10-Master (recherche ou professionnel), DEA, DESS, Doctorat"="10",
                          "12-Ecoles niveau licence et au-del?"="12",
                          "21-Licence (L3)"="21",
                          "22-Maitrise (M1)"="22",
                          "30-DEUG"="30",
                          "31-DUT, BTS"="31",
                          "32-Autre dipl?me (niveau bac+2)"="32",
                          "33-Paramedical et social (niveau bac+2)"="33",
                          "41-Bac general"="41",
                          "42-Bac technologique"="42",
                          "43-Bac professionnel"="43",
                          "44-Brevet de technicien, brevet professionnel"="44",
                          "50-CAP, BEP"="50",
                          "60-Brevet des collèges"="60",
                          "70-Certificat d'études primaires"="70",
                          "71-Sans diplôme"="71",
                          
  ) } 



# ct$GOLDEN_05 <- forcats::fct_recode(ctraw$GOLDEN_05,
#                                     "1-Etat" = "1",
#                                     "2-Collectivités locales, HLM" = "2",
#                                     "3-Hôpitaux publics" = "3",
#                                     "4-Sécurité sociale" = "4",
#                                     "5-Entreprises publiques" = "5",
#                                     "6-Particuliers" = "6",
#                                     "7N-Entreprise taille inconnue" = "7N",
#                                     "70-Entreprise pas de salarié" = "70",
#                                     "71-Entreprise 1 à 9 salariés" = "71",
#                                     "72-Entreprise 10 à 49 salariés" = "72",
#                                     "73-Entreprise 50 à 199 salariés" = "73",
#                                     "74-Entreprise 200 à 499 salariés" = "74",
#                                     "75-Entreprise 500 à 999 salariés" = "75",
#                                     "76-Entreprise 1000 salariés et plus" = "76",
# ) 


if("STAT2_05" %in% names(ctraw) & !is.numeric(ctraw$STAT2_05) ) {
  ct$STAT2_05 <- fct_recode(ctraw$STAT2_05,
                            
                            "1-Non salarié"="1",
                            "2-Salarié"="2",
                            
  ) } 
if("STATUT_05" %in% names(ctraw) & !is.numeric(ctraw$STATUT_05) ) {
  ct$STATUT_05 <- fct_recode(ctraw$STATUT_05,
                             
                             "11-Indépendants"="11",
                             "12-Employeurs"="12",
                             "13-Aides familiaux"="13",
                             "21-Intérimaires"="21",
                             "22-Apprentis"="22",
                             "33-CDD (hors Etat, collectivités locales), hors contrats aides"="33",
                             "34-Stagiaires et contrats aides (hors Etat, collectivités locales)"="34",
                             "35-Autres contrats (hors Etat, collectivités locales)"="35",
                             "43-CDD (Etat, collectivités locales), hors contrats aides"="43",
                             "44-Stagiaires et contrats aides (Etat, collectivités locales)"="44",
                             "45-Autres contrats (Etat, collectivités locales)"="45",
                             
  ) } 
# if("STATUTR" %in% names(ct) & !is.numeric(ct$STATUTR) ) {
#   ct$STATUTR <- fct_recode(ct$STATUTR,
#                                      
#                                      "1-Non salariés (indépendants, employeurs)"="1",
#                                      "2-Intérimaires"="2",
#                                      "3-Apprentis"="3",
#                                      "4-CDD"="4",
#                                      "5-CDI"="5",
#                                      
#   ) } 

if("STATOEP_05" %in% names(ctraw) & !is.numeric(ctraw$STATOEP_05) ) {
  ct$STATOEP_05 <- fct_recode(ctraw$STATOEP_05,
                              
                              "11-Indépendants"="11",
                              "12-Employeurs"="12",
                              "13-Aides familiaux"="13",
                              "21-Intérimaires"="21",
                              "22-Apprentis"="22",
                              "33-CDD (hors Etat, collectivit?s locales), hors contrats aides"="33",
                              "34-Stagiaires et contrats aides (hors Etat, collectivités locales)"="34",
                              "35-Autres contrats (hors Etat, collectivités locales)"="35",
                              "43-CDD (Etat, collectivités locales), hors contrats aides"="43",
                              "44-Stagiaires et contrats aides (Etat, collectivités locales)"="44",
                              "45-Autres contrats (Etat, collectivités locales)"="45",
                              
  ) } 

if("CHPUB_05" %in% names(ctraw) & !is.numeric(ctraw$CHPUB_05) ) {
  ct$CHPUB_05 <- fct_recode(ctraw$CHPUB_05,
                            "1-Etat"="1",
                            "2-Collectivités locales, HLM"="2",
                            "3-Hôpitaux publics"="3",
                            "4-Particulier"="4",
                            "5-Entreprise"="5",
                            "5-Entreprise"="6",
                            
  ) } 

if("CONTRA_05" %in% names(ctraw) & !is.numeric(ctraw$CONTRA_05) ) {
  ct$CONTRA_05 <- fct_recode(ctraw$CONTRA_05,
                             "1-CDI"="1",
                             "2-CDD"="2",
                             "3-Saisonnier"="3",
                             "4-Intérim"="4",
                             "5-Apprentissage"="5",
  ) } 

if("TITC_05" %in% names(ctraw) & !is.numeric(ctraw$TITC_05) ) {
  ct$TITC_05 <- fct_recode(ctraw$TITC_05,
                           "1-Eleve fonctionnaire ou stagiaire"="1",
                           "2-Agent titulaire"="2",
                           "3-Contractuel"="3",
                           
  ) } 


ct$NAF36_05 <- forcats::fct_recode(ctraw$NAF36_05,
                                   "A0-Agriculture, sylviculture et pêche" = "A0",
                                   "B0-Industries agricoles et alimentaires" = "B0",
                                   "C1-Habillement, cuir" = "C1",
                                   "C2-Edition, imprimerie, reproduction" = "C2",
                                   "C3-Pharmacie, parfumerie et entretien" = "C3",
                                   "C4-Industries des équipements du foyer" = "C4",
                                   "D0-Industrie automobile" = "D0",
                                   "E1-Construction navale; aéronautique et ferroviaire" = "E1",
                                   "E2-Industries des équipements mécaniques" = "E2",
                                   "E3-Industries des équipements électriques et électroniques" = "E3",
                                   "F1-Industries des produits minéraux" = "F1",
                                   "F2-Industrie textile" = "F2",
                                   "F3-Industries du bois et du papier" = "F3",
                                   "F4-Chimie, caoutchouc, plastiques" = "F4",
                                   "F5-Métallurgie et transformation des métaux" = "F5",
                                   "F6-Industries des composants électriques et électroniques" = "F6",
                                   "G1-Production de combustibles et de carburants" = "G1",
                                   "G2-Eau, gaz, électricité" = "G2",
                                   "H0-Construction" = "H0",
                                   "J1-Commerce et réparation automobile" = "J1",
                                   "J2-Commerce de gros" = "J2",
                                   "J3-Commerce de détail, réparations" = "J3",
                                   "K0-Transports" = "K0",
                                   "L0-Activités financières" = "L0",
                                   "M0-Activités immobilières" = "M0",
                                   "N1-Postes et télécommunications" = "N1",
                                   "N2-Conseils et assistance" = "N2",
                                   "N3-Services opérationnels" = "N3",
                                   "N4-Recherche et développement" = "N4",
                                   "P1-Hôtels et restaurants" = "P1",
                                   "P2-Activités récréatives, culturelles et sportives" = "P2",
                                   "P3-Services personnels et domestiques" = "P3",
                                   "Q1-Education" = "Q1",
                                   "Q2-Santé, action sociale" = "Q2",
                                   "R1-Administrations publiques" = "R1",
                                   "R2-Activités associatives et extra-territoriale" = "R2",
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
ct$DIPLOMEcadres <- fct_recode(
  ct$DIPLOME,
  "1-Bac ou moins" = "0-Aucun diplôme"      ,
  "1-Bac ou moins" = "1-Certificat d'études",
  "1-Bac ou moins" = "2-Brevet"             ,
  "1-Bac ou moins" = "3-CAP BEP"            ,
  "1-Bac ou moins" = "4-Bac pro"            ,
  "1-Bac ou moins" = "5-Bac général"        ,
  "6-Bac+2"        = "6-Bac+2"              ,
  "7-Bac+4"        = "7-Bac+4"              ,
  "8-Bac+5"        = "8-Bac+5"              
)






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

ct$COMMENT <- ct$COMMENT %>% fct_rev()

ct$AUT <- ctraw %>% 
  transmute(AUT = as.factor(case_when(
    COMMENT %in% c("2", "") & INCIDENT == "1" ~ "1-Autonomie même imprévus",
    COMMENT %in% c("2", "") & INCIDENT == "2" ~ "2-Autonomie cas prévus",
    COMMENT %in% c("2", "") & INCIDENT == "3" ~ "3-Autonomie sauf incident",
    COMMENT == "1"                            ~ "4-Pas d’autonomie d’exécution",
  ))) %>% deframe()



ct <- ct |> mutate(REMPERF  = fct_recode(REMPERF, "3-Pas salaire perf" = "9-NSP"), 
                   REMPERF2 = fct_recode(
                     REMPERF,
                     "1-Salaire à la performance"   = "1-Salaire performance fort"  ,
                     "1-Salaire à la performance"   = "2-Salaire performance faible",
                     "3-Pas salaire perf"           = "3-Pas salaire perf"          
                   ))




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



if("RWDEP" %in% names(ct) & "RWCAD" %in% names(ct)) {
  ct$RWCH <- as.factor(case_when(
    ct$RWDEP == "1-CR chaîne" | ct$RWCAD == "1-CR cadence"  ~ "1-CR machine", 
    is.na(ct$RWDEP) & is.na(ct$RWCAD )                      ~ NA_character_ ,
    TRUE                                                   ~ "2-Pas CR machine"
  ))
}


ct$OBJVRAI <- ctraw %>% 
  transmute(new_var = as.factor(case_when(
    OBJECTIF == "1" & OBJMODIF %in% c("2", "4") ~ "1-Objectifs chiffrés",
    TRUE                                        ~ "2-Pas obj chiffrés"
  ))) %>% deframe()

## Harmoniser taille entreprises ----
if("NBSALA" %in% names(ct) & !is.numeric(ct$NBSALA) ) {
  ct$NBSALA <- if_else(ct$ANNEE == "2005", 
                       true  = forcats::fct_recode(ctraw$NBSALA,
                                                   "1-1 à 4"     = "1", #Aucun salarié
                                                   "1-1 à 4"     = "2",
                                                   "2-5 à 9"     = "3",
                                                   "3-10 à 19"   = "4",
                                                   "4-20 à 49"   = "5",
                                                   "5-50 à 199"  = "6",
                                                   "6-200 à 499" = "7",
                                                   "7-500 à 999" = "8",
                                                   "8-1000 et +" = "9",
                                                   "NULL"        = "98",
                                                   "NULL"        = "99",     
                                                   "NULL"        = "998"
                       ),  
                       false = forcats::fct_recode(ctraw$NBSALA, 
                                                   "1-1 à 4"     = "1",
                                                   "2-5 à 9"     = "2",
                                                   "3-10 à 19"   = "3",
                                                   "4-20 à 49"   = "4",
                                                   "5-50 à 199"  = "5",
                                                   "6-200 à 499" = "6",
                                                   "7-500 à 999" = "7",
                                                   "8-1000 et +" = "8",
                                                   "NULL"        = "98",
                                                   "NULL"        = "99",
                                                   "NULL"        = "998"
                                                   
                       )
  )
  ct$NBSALA2 <- fct_recode(ct$NBSALA,
                           "1-1 à 49 salariés" = "1-1 à 4"    ,
                           "1-1 à 49 salariés" = "2-5 à 9"    ,
                           "1-1 à 49 salariés" = "3-10 à 19"  ,
                           "1-1 à 49 salariés" = "4-20 à 49"  ,
                           "2-50 à 499"        = "5-50 à 199" ,
                           "2-50 à 499"        = "6-200 à 499",
                           "3-500 et +"        = "7-500 à 999",
                           "3-500 et +"        = "8-1000 et +"
  )
} 
#tab(ct, NBSALA, ANNEE, pct = "col", wt = pondcal) # Comparable sur 2005 à 2019

if("NBSALB" %in% names(ct) & !is.numeric(ct$NBSALB) ) {
  ct$NBSALB <- if_else(ct$ANNEE == "2005", 
                       true  = forcats::fct_recode(ctraw$NBSALB,
                                                   "1-1 à 49 salariés" = "1",
                                                   "1-1 à 49 salariés" = "2",
                                                   "1-1 à 49 salariés" = "3",
                                                   "1-1 à 49 salariés" = "4",
                                                   "2-50 à 499"        = "5",
                                                   "2-50 à 499"        = "6",
                                                   "2-50 à 499"        = "7",
                                                   "3-500 et +"        = "8",
                                                   "3-500 et +"        = "9",
                                                   "NULL"              = "98",    
                                                   "NULL"              = "99", 
                                                   "NULL"              = "998",
                       ), 
                       false = forcats::fct_recode(ctraw$NBSALB,
                                                   "1-1 à 49 salariés"="1",
                                                   "2-50 à 499"="2",
                                                   "3-500 et +"="3",
                                                   "NULL"="8",
                                                   "NULL"="9",
                                                   "NULL"="98",    
                                                   
                       )
  )
  
  
  
} 

if("NBSALENTC" %in% names(ct) & !is.numeric(ct$NBSALENTC) ) {
  ct$NBSALENTC <- forcats::fct_recode(ctraw$NBSALENTC,
                                      "1-1 à 49 salariés"="1",
                                      "2-50 à 499"="2",
                                      "3-500 et +"="3",
                                      "NULL"="8",
                                      "NULL"="9",
  )
} 

ct$NBSALENTC2 <- as.factor(case_when(
  ct$NBSALA2 == as.character(ct$NBSALB) | is.na(ct$NBSALB) ~ as.character(ct$NBSALA2), 
  TRUE                                                     ~ as.character(ct$NBSALB), 
))
##tab(ct, NBSALENTC2, NBSALENTC, ANNEE) #Idem NBSALENTC sur 2013:2019, sans NA administrations
##tab(ct, NBSALENTC2, ANNEE, pct = "col", wt = pondcal) 
##tab(ct, CHPUB_05, NBSALENTC2, ANNEE, pct = "col", wt = pondcal)

#Seulement salariés des entreprises, assos, cliniques privées
ct$NBSALENTC3 <- as.factor(case_when(
  ct$ANNEE != "2005" & !str_detect(ct$STATUT, "^4|^6")     ~ NA_character_,
  ct$ANNEE == "2005" & !str_detect(ct$CHPUB_05, "^5|^6")   ~ NA_character_,
  ct$NBSALA2 == as.character(ct$NBSALB) | is.na(ct$NBSALB) ~ as.character(ct$NBSALA2), 
  TRUE                                                     ~ as.character(ct$NBSALB), 
)) 
#tab(ct, NBSALENTC3, ANNEE, pct = "col", wt = pondcal)
#tab(ct, NBSALENTC3, NBSALENTC, ANNEE)
#tab(ct, NBSALENTC, ANNEE, pct = "col", wt = pondcal)


## Nouvelle variable employeur ----

#Emp à partir en premier lieu des déclarations des salariés (2005 comme ensuite)
ct$EMP <- ctraw %>%
  bind_cols(select(ct, NBSALENTC2)) %>% 
  transmute(new_var = as.factor(
    if_else(ANNEE == "2005", 
            true  = case_when(
              STAT2_05 == "1" | cser %in% c("1", "2") | str_detect(PE, "^312C|^312D|^312E")
              ~ "I-Pas d’employeur", 
              
              str_detect(CHPUB_05, "^3") |
                str_detect(CJUR_05, "^7364|^7365|^7366") |
                # => CJUR %in% c("7364-Établissement d'hospitalisation",
                # "7365-Syndicat inter hospitalier",
                # "7366-Établissement public local social et médico-social")
                #( (is.na(CJUR_05) | CJUR_05 == "") & str_detect(NAF_05, "^851A") & str_detect(NEWPUB_05, "^1|^2") ) |
                str_detect(PE, "^344C") #Internes en médecine
              ~ "3-Hôpitaux publics",
              
              str_detect(PE, "^333C|^451A|^521A") |                                  
                str_detect(PE, "^333D|^451B|^521B") | 
                str_detect(NAF_05, "^641A|^642A")  #"641A-Postes nationales" "642A-Télécommunications nationales"
              ~ "6-Grandes entreprises", 
              
              #Professions spécifiques aux CL (une fois hôpitaux classés par CJUR)
              str_detect(PE, "^332B|^333F|^451F|^531B")
              # => P %in% c("332B-Ingénieurs des collectivités locales et des hôpitaux",
              # "333F-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)",
              # "451F-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)",
              # "531B-Agents des polices municipales")
              ~ "1-Collectivités locales",
              
              
              #État : 
              # On enlève les Groupement de droit privé non doté de la personnalité morale (aucun dans public EE)
              !str_detect(CJUR_05, "^2") & ( #|^4|^5 #entreprises de droit public ou de droit privé
                str_detect(CHPUB_05, "^1") | 
                  str_detect(PE, "^376F|^467D|^545D") | #Profession Sécurité sociale
                  (str_detect(CHPUB_05, "^2") & str_detect(PE, "^421|^422|^341|^342")) | #Enseignants, qui tombent dans CL par CJUR 
                  #   Professions spécifiques à l'État
                  str_detect(PE, "^332A|^333A|^333B|^333E|^334A|^451C|^452A|^452B|^451E|^531A|^532A|^532B|^532C|^522A")
              )
              # =>  P %in% c("332a-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés",
              # "333a-Magistrats",
              # "333b-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes",
              # "333e-Autres personnels administratifs de catégorie A de l'Etat (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
              # "334a-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)",
              # "451c-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés",
              # "452b-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie",
              # "451e-Autres personnels administratifs de catégorie B de État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
              # "531a-Agents de police de État",
              # "532a-Gendarmes (de grade inférieur à adjudant)",
              # "532b-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)",
              # "532c-Hommes du rang (sauf pompiers militaires)",
              # "522a-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes",
              # "452a-Inspecteurs et officiers de police")
              ~ "2-État", 
              
              
              str_detect(CHPUB_05, "^2") #& !str_detect(CJUR_05, "^2|^3|^4|^5")  #On enlève les entreprises de droit public ou privé des CL (offices HLM, etc.) 
              ~ "1-Collectivités locales", # str_detect(CJUR_05, "^5546|^5547|^5646|^5647") #7371 = étab pub
              
              
              # Si CJUR est public on ajoute dans le public, pour ensuite mettre y compris des NA à CHPUB_05 en entreprises
              
              # Au sein de la CJ1 "7-Personne morale et organisme soumis au droit administratif" : 
              #État 
              str_detect(CJUR_05, "^71") 
              # => CJ2 == "71-Administration de l'État"
              # c-a-d CJUR %in% c("7111-Autorité constitutionnelle","7112-Autorité administrative indépendante",
              # "7113-Ministère", "7120-Service central d'un ministère",
              # "7160-Service déconcentré à compétence nationale d'un ministère (hors Défense)",
              # "7171-Service déconcentré de l'État à compétence (inter) régionale",
              # "7172-Service déconcentré de l'État à compétence (inter) départementale",
              # "7179-(Autre) Service déconcentré de l'État à compétence territoriale")
              | str_detect(CJUR_05, "^738") 
              # => CJ2 == "72-Etablissement public administratif", national
              ~ "2-État",
              
              #Collectivités locales
              str_detect(CJUR_05, "^72|^73") 
              # => CJ2 == "72-Collectivité territoriale"
              # => CJ2 == "73-Etablissement public administratif"
              # sauf établissement nationaux et hospitaliers/profs, déjà codés plus haut État/HP
              ~ "1-Collectivités locales",
              
              str_detect(CHPUB_05, "^4") ~ "8-Particuliers",
              
              #(str_detect(CHPUB_05, "^5") #| (str_detect(CHPUB_05, "^1") & str_detect(CJUR_05, "^2|^3|^4|^5"))
              #)
              #& 
              str_detect(NBSALENTC2, "^1|^2") #NEWTEFEN_05 %in% c("1", "2", "3", "4") 
              ~ "5-PME (et assos)", 
              
              #(str_detect(CHPUB_05, "^5") #| (str_detect(CHPUB_05, "^1") & str_detect(CJUR_05, "^2|^3|^4|^5")) 
              #)
              #& 
              str_detect(NBSALENTC2, "^3") #NEWTEFEN_05 %in% c("5", "6")
              ~ "6-Grandes entreprises", 
              
              #(str_detect(CHPUB_05, "^5") #| (str_detect(CHPUB_05, "^1")  & str_detect(CJUR_05, "^2|^3|^4|^5"))
              #)
              #&
              is.na(NBSALENTC2)
              ~ "7-Ent taille inconnue",
            ),         
            
            false = case_when(
              !(STATUT %in% c("10", "9", "8")) 
              & (cser %in% c("1", "2") | str_detect(PE, "^312C|^312D|^312E")) 
              ~ "I-Pas d’employeur",
              
              STATUT == "3"                          ~ "3-Hôpitaux publics",
              STATUT == "5"                          ~ "3-Hôpitaux publics",
              str_detect(PE, "^344C")                ~ "3-Hôpitaux publics",#Internes en médecine
              
              str_detect(PE, "^333C|^451A|^521A") |                                  
                str_detect(PE, "^333D|^451B|^521B")       ~ "6-Grandes entreprises", 
              
              #Professions spécifiques aux CL (une fois hôpitaux classés par CJUR)
              str_detect(PE, "^332B|^333F|^451F|^531B")
              # => P %in% c("332b-Ingénieurs des collectivités locales et des hôpitaux",
              # "333f-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)",
              # "451f-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)",
              # "531b-Agents des polices municipales")
              ~ "1-Collectivités locales",
              
              #État : + Enseignants, qui tombent dans CL par CJUR ; + Sécurité sociale
              STATUT == "1" | 
                str_detect(PE, "^376F|^467D|^545D") |
                (STATUT == "2" & str_detect(PE, "^421|^422|^341|^342")) | 
                #   Professions spécifiques à l'État
                str_detect(PE, "^332A|^333A|^333B|^333E|^334A|^451C|^452A|^452B|^451E|^531A|^532A|^532B|^532C|^522A")
              # =>  P %in% c("332a-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés",
              # "333a-Magistrats",
              # "333b-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes",
              # "333e-Autres personnels administratifs de catégorie A de l'Etat (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
              # "334a-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)",
              # "451c-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés",
              # "452b-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie",
              # "451e-Autres personnels administratifs de catégorie B de État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
              # "531a-Agents de police de État",
              # "532a-Gendarmes (de grade inférieur à adjudant)",
              # "532b-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)",
              # "532c-Hommes du rang (sauf pompiers militaires)",
              # "522a-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes",
              # "452a-Inspecteurs et officiers de police")
              ~ "2-État", 
              
              STATUT == "2"         ~ "1-Collectivités locales",
              
              STATUT %in% c("4", "6")
              & str_detect(NBSALENTC2, "^1|^2") #NBSALENTC %in% c("1", "2") 
              ~ "5-PME (et assos)",
              
              STATUT %in% c("4", "6")
              & str_detect(NBSALENTC2, "^3")  #NBSALENTC == "3"          
              ~ "6-Grandes entreprises",
              
              STATUT %in% c("4", "6")
              ~ "7-Ent taille inconnue",
              
              STATUT == "7"                  ~ "8-Particuliers",
              STATUT %in% c("10", "9") ~ "I-Pas d’employeur"
            ) 
    )
  )) %>% deframe()
# tab(ct, EMP, ANNEE)
# tab(ct, EMP, ANNEE, pct = "col")
# tab(ct, EMP, ANNEE, pct = "col", wt = pondcal, digits = 1)
##  en 2005 4% de NA à employeur : 618 NA à CHPUB / 598 à NEMPUB parmi les salariés
## ct[ct$ANNEE == "2005" & is.na(ct$CHPUB_05) & ct$STAT2_05 == "2-Salarié", ] |> tab(CSE)

#14.2% en 2005 dans État, pour 10.7% en 2013... 
#14.7% en 2005 pour 10.7% en 2013 avec professions spécifiques à l'État (pas bcp plus)

#salariat0519 <- ct$CHAMP_CT2005 == "1-Champ 2005" & 
#  !ct$EMP %in% c("8-Particuliers", "I-Pas d’employeur") & !is.na(ct$EMP) &
#  !is.na(ct$pondcal)
#salariat0513 <- ct$CHAMP_CT2005 == "1-Champ 2005" & ct$ANNEE %in% c("2005", "2013") & 
#  !ct$EMP %in% c("8-Particuliers", "I-Pas d’employeur") & !is.na(ct$EMP) &
#  !is.na(ct$pondcal)
#tab(ct[salariat0519chpub, ], EMP, ANNEE, pct = "col", wt = pondcal, digits = 1, diff = 1, color = "after_ci")



#Tests cohérence variable employeur 2005/2013 : 

## Enquête Emploi NBSALA/NBSALB (pas dispo en 2013)
# EMP_PUB2                `2005` `2012`
# 1-Collectivités locales   8.5%   8.7% stable (cloche entre les deux)
# 2-État                   12.3%  11.3% baisse
# 3-Hôpitaux publics        4.4%   4.5% stable
# 5-TPE-PME                42.8%  41.4% stable
# 6-Grandes entreprises    28.4%  28.8% stable
# 7-Ent taille inconnue     3.5%   5.4% 
# Total                     100%   100% 

#                         CT   -  EE                    CT  -  EE
#EMP                `2005`  `2005`                `2013` `2012`
#1-Collectivités locales   7.3%    8.5% -1.2 CL         7.9%   8.7% -1.2 CL
#2-État                   14.5%   12.3% +2.2 État       10.7%  11.3% -0.6 État
#3-Hôpitaux publics        4.9%    4.4% +0.5 HP         5.1%   4.5% +0.5 HP
#5-PME (et assos)         41.2%   42.8% -1.6PME         41.0%  41.4% -0.4 PME
#6-Grandes entreprises    29.6%   28.4% +1.2 GE         32.0%  28.8% +3.2 GE
#7-Ent taille inconnue     2.5%    3.5% -1EntI          3.3%   5.4% -2.1 EntI
#Total                     100%    100%                 100%   100%

## Enquête Emploi EFEN : 
# EMP_PUB                 `2005` `2013`
# 1-Collectivités locales   8.5%   8.7% # stable (cloche entre les deux)
# 2-État                   12.3%  11.1% # baisse
# 3-Hôpitaux publics        4.4%   4.7% # augmentation légère
# 4-Entreprises publiques   3.3%   4.8%
# 5-TPE-PME                28.1%  28.8%
# 6-Grandes entreprises    11.4%  10.3%
# 7-Ent taille inconnue    31.9%  31.5%
# Total                     100%   100%



## Déséquilibres, au départ (parfois évolution réelle?) : 
## - "342A-Enseignants de l'enseignement supérieur" : 9% ent 2005, 19% ent 2019
## - Plus d'enseignants classés dans entreprises en 2005 (instits 16% -> 3%)
## - Dans le social et le loisir, la frontière CL/entreprises est floue
## - Un peu plus d'infirmiers spé, d'aides soignantes, d'ambulanciers dans cliniques en 2013 ?
## - La frontière État/CL est en fait plus solide.
#
#tab(ct[salariat0513, ], ANNEE, EMP, CSE, wt = pondcal, pct = "row",
#    color = "diff", diff = 1, totaltab = "table") |>
#  mutate(n = mutate(Total, display = "n")) |>
#  filter(!is_totrow(Total) & sum(n) >= 60) |> 
#  print(n = 500)
#
#
#
##Cohérence frontière administrations/entreprises
#tab(ct[salariat0513, ], ANNEE, EMP_ADM_ENTchpub, CSE, wt = pondcal, pct = "row",
#    color = "diff", diff = 1, totaltab = "table") |>
#  mutate(n = mutate(Total, display = "n")) |>
#  filter(!is_totrow(Total) & sum(n) >= 60) |> 
#  print(n = 500)


# #Medecins hospitaliers : 5% État 2005, mais 14% 2013 (HP -7%)
# #=> Mettre tous les internes en médecine dans HP
# # Reste encore 13% dans État en 2013
# tab(ct[salariat0513 & str_detect(ct$PPP2, "Médecins hospitalier·es"), ], 
#     ANNEE, EMP, PE, wt = pondcal, pct = "row",
#     color = "diff", diff = 1, totaltab = "table") |>
#   filter(!is_totrow(Total))
# 
# tab(ct[salariat0513 & str_detect(ct$PE, "^344A"), ], 
#     ANNEE, EMP, ACTIVFIN, wt = pondcal, pct = "row",
#     color = "diff", diff = 1, totaltab = "table") |>
#   filter(!is_totrow(Total))


#Déséquilibres entre 2005 et 2013, après V2, EMP : 
# CSE :
#  - "33-Cadres de la fonction publique"         : GE  11% -> 1%  / CL23% -> 36%
#  - "34-Professeurs, professions scientifiques" : PME 10% -> 18%
#  - "35-Info arts                               : GE  20% -> 31%
#  - Cadres admin comm                           : État 5% -> 1%
#  - 42-Professeurs des écoles                   : État 73% -> 63% (va dans ent)
#  - 45-PI admin FP : CL 29% -> 38% ; État 57% -> 48% ; GE 10% -> 4%
#  - 48-Contremaitres et maîtrise                : PME 51% -> 39% ; GE 40% -> 51%
#  - 53-Policiers et militaires : CL 8>15% ; État 68>39% ; PME 9>16% ; GE 12>23% ; normal agents de sécu
#  - 56-Personnels SDP                           : GE 14% -> 22% (détrim PME CL)
#  - "67-Ouvriers non qualifiés de type industriel" : PME 57% -> 47% ; CL 3% -> 7%
#  - "68-Ouvriers non qualifiés de type artisanal"  : PME 65% -> 59% (profit GE)
# CSE avec EMP_ADM_ENT : 
#  - Plus dans ENT en 2005: 12% cadres FP ; professeurs ; (PIadmFP, ONQI)
#  - Moins dans ENT 05: info_art, professeur, instits, agents sécu +20%, 

# # Qui sont les cadres FP dans entreprises en 2005 ? 
# # Essentiellement NAF "641A-Postes nationales". 
# # En 2013 ils ne sont pas classés cadres FP (presque aucun ACTIVFIN == 
# # "53-Activités de poste et de courrier" dans le public : 4 cadres, 14 empFP)
# tab(ct[salariat0513 & str_detect(ct$CSE, "^33") & ct$ANNEE == "2005", ], 
#     NAF_05, EMP_ADM_ENTchpub) |>
#   mutate(n = mutate(Total, display = "n"))
# 
# # ACTIVFIN  == "53-Activités de poste et de courrier"
# tab(ct[salariat0513 & str_detect(ct$CSE, "^33") & ct$ANNEE == "2013", ], 
#     ACTIVFIN, EMP_ADM_ENTchpub) |>
#   mutate(n = mutate(Total, display = "n")) 
# 
# tab(ct[salariat0513 & str_detect(ct$ACTIVFIN, "^53") & ct$ANNEE == "2013", ], 
#     CSE, EMP_ADM_ENTchpub) |>
#   mutate(n = mutate(Total, display = "n"))
#   print(n = 500)

## "53-Policiers et militaires"
# tab(ct[salariat0513 & str_detect(ct$CSE, "^53") & ct$ANNEE == "2005", ], 
#     PE, EMP) |>
#   mutate(n = mutate(Total, display = "n"))
# 
# # ACTIVFIN  == "53-Activités de poste et de courrier"
# tab(ct[salariat0513 & str_detect(ct$CSE, "^33") & ct$ANNEE == "2013", ], 
#     ACTIVFIN, EMP_ADM_ENTchpub) |>
#   mutate(n = mutate(Total, display = "n")) 
# 
# tab(ct[salariat0513 & str_detect(ct$ACTIVFIN, "^53") & ct$ANNEE == "2013", ], 
#     CSE, EMP_ADM_ENTchpub) |>
#   mutate(n = mutate(Total, display = "n"))
#   print(n = 500)

# #Avec trois catégories d'entreprise par tailles
# ct$EMP3 <- ctraw %>%
#   transmute(new_var = as.factor(case_when(
#     !(STATUT %in% c("10", "9", "8")) 
#     & (cser %in% c("1", "2") | str_detect(PE, "^312C|^312D|^312E")) #Consultants libéraux
#     ~ "Z-Pas d’employeur",
#     
#     STATUT == "3"                          ~ "13-Hôpitaux publics",
#     STATUT == "5"                          ~ "13-Hôpitaux publics",
#     
#     str_detect(PE, "^333C|^451A|^521A") |                                  # La Poste
#       str_detect(PE, "^333D|^451B|^521B")       ~ "16-Entreprises >500sal", # France Télécom 
#     
#     STATUT == "1" | str_detect(PE, "^376F|^467D|^545D")   ~ "12-État",   # Sécu
#     
#     STATUT == "2"         ~ "11-Collectivités locales",
#     
#     STATUT %in% c("4", "6")
#     & NBSALENTC == "1"           ~ "14-Entreprises <50sal",
#     
#     STATUT %in% c("4", "6")
#     & NBSALENTC == "2"           ~ "15-Entreprises 50_499_sal",
#     
#     STATUT %in% c("4", "6")
#     & NBSALENTC == "3"           ~ "16-Entreprises >500sal",
#     
#     STATUT %in% c("4", "6")
#     & (is.na(NBSALENTC)|NBSALENTC %in% c("", "8", "9")) ~ NA_character_,
#     
#     STATUT == "7"                  ~ "18-Particuliers",
#     STATUT %in% c("10", "9") ~ "30-Pas d’employeur"
#   ) )) %>% deframe()
# # tab(ct, EMP3)


   
   
   
   
   
   
   
   
   # ct$EMPnewpup <- ctraw %>%
   #   bind_cols(select(ct, NBSALENTC2)) %>% 
   #   transmute(new_var = as.factor(
   #     if_else(ANNEE == "2005", 
   #             true  = case_when(
   #               STAT2_05 == "1" | cser %in% c("1", "2") | str_detect(PE, "^312C|^312D|^312E")
   #               ~ "I-Pas d’employeur", 
   #               
   #               str_detect(NEWPUB_05, "^3") |
   #                 str_detect(CJUR_05, "^7364|^7365|^7366") |
   #                 # => CJUR %in% c("7364-Établissement d'hospitalisation",
   #                 # "7365-Syndicat inter hospitalier",
   #                 # "7366-Établissement public local social et médico-social")
   #                 ( (is.na(CJUR_05) | CJUR_05 == "") & str_detect(NAF_05, "^851A") & str_detect(NEWPUB_05, "^1|^2") ) |
   #                 str_detect(PE, "^344C") #Internes en médecine
   #               ~ "3-Hôpitaux publics",
   #               
   #               str_detect(PE, "^333C|^451A|^521A") |                                  
   #                 str_detect(PE, "^333D|^451B|^521B") | 
   #                 str_detect(NAF_05, "^641A|^642A")  #"641A-Postes nationales" "642A-Télécommunications nationales"
   #               ~ "6-Grandes entreprises", 
   #               
   #               #Professions spécifiques aux CL (une fois hôpitaux classés par CJUR)
   #               str_detect(PE, "^332B|^333F|^451F|^531B")
   #               # => P %in% c("332b-Ingénieurs des collectivités locales et des hôpitaux",
   #               # "333f-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)",
   #               # "451f-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)",
   #               # "531b-Agents des polices municipales")
   #               ~ "1-Collectivités locales",
   #               
   #               #État : + Enseignants, qui tombent dans CL par CJUR ; + Sécurité sociale
   #               #!str_detect(CJUR_05, "^2|^3|^4|^5") & (
   #               str_detect(NEWPUB_05, "^1|^4") | 
   #                 str_detect(PE, "^376F|^467D|^545D") |
   #                 (str_detect(NEWPUB_05, "^2") & str_detect(PE, "^421|^422|^341|^342")) | 
   #               #   Professions spécifiques à l'État
   #               str_detect(PE, "^332A|^333A|^333B|^333E|^334A|^451C|^452A|^452B|^451E|^531A|^532A|^532B|^532C|^522A")
   #               #)
   #               # =>  P %in% c("332a-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés",
   #               # "333a-Magistrats",
   #               # "333b-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes",
   #               # "333e-Autres personnels administratifs de catégorie A de l'Etat (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
   #               # "334a-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)",
   #               # "451c-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés",
   #               # "452b-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie",
   #               # "451e-Autres personnels administratifs de catégorie B de État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
   #               # "531a-Agents de police de État",
   #               # "532a-Gendarmes (de grade inférieur à adjudant)",
   #               # "532b-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)",
   #               # "532c-Hommes du rang (sauf pompiers militaires)",
   #               # "522a-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes",
   #               # "452a-Inspecteurs et officiers de police")
   #               ~ "2-État", 
   #               
   #               #On enlève les entreprises de droit public ou privé des CL (offices HLM, etc.) 
   #               str_detect(NEWPUB_05, "^2") #& !str_detect(CJUR_05, "^2|^3|^4|^5")  
   #               ~ "1-Collectivités locales", # str_detect(CJUR_05, "^5546|^5547|^5646|^5647") #7371 = étab pub
   #               
   #               (str_detect(NEWPUB_05, "^5|^7") #| (str_detect(NEWPUB_05, "^2") & str_detect(CJUR_05, "^2|^3|^4|^5"))
   #               )
   #               & str_detect(NBSALENTC2, "^1|^2") #NEWTEFEN_05 %in% c("1", "2", "3", "4") 
   #               ~ "5-PME (et assos)", 
   #               
   #               (str_detect(NEWPUB_05, "^5|^7") #| (str_detect(NEWPUB_05, "^2") & str_detect(CJUR_05, "^2|^3|^4|^5")) 
   #                )
   #               & str_detect(NBSALENTC2, "^3") #NEWTEFEN_05 %in% c("5", "6")
   #               ~ "6-Grandes entreprises", 
   #               
   #               (str_detect(NEWPUB_05, "^5|^7") #| (str_detect(NEWPUB_05, "^2") & str_detect(CJUR_05, "^2|^3|^4|^5"))
   #                )
   #               ~ "7-Ent taille inconnue",
   #               
   #               str_detect(NEWPUB_05, "^6") ~ "8-Particuliers",
   #             ),         
   #             
   #             
   #             false = case_when(
   #               !(STATUT %in% c("10", "9", "8")) 
   #               & (cser %in% c("1", "2") | str_detect(PE, "^312C|^312D|^312E")) 
   #               ~ "I-Pas d’employeur",
   #               
   #               STATUT == "3"                          ~ "3-Hôpitaux publics",
   #               STATUT == "5"                          ~ "3-Hôpitaux publics",
   #               str_detect(PE, "^344C")                ~ "3-Hôpitaux publics",#Internes en médecine
   #               
   #               str_detect(PE, "^333C|^451A|^521A") |                                  
   #                 str_detect(PE, "^333D|^451B|^521B")       ~ "6-Grandes entreprises", 
   #               
   #               #Professions spécifiques aux CL (une fois hôpitaux classés par CJUR)
   #               str_detect(PE, "^332B|^333F|^451F|^531B")
   #               # => P %in% c("332b-Ingénieurs des collectivités locales et des hôpitaux",
   #               # "333f-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)",
   #               # "451f-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)",
   #               # "531b-Agents des polices municipales")
   #               ~ "1-Collectivités locales",
   #               
   #               #État : + Enseignants, qui tombent dans CL par CJUR ; + Sécurité sociale
   #               STATUT == "1" | 
   #                 str_detect(PE, "^376F|^467D|^545D") |
   #                 (STATUT == "2" & str_detect(PE, "^421|^422|^341|^342")) | 
   #                 #   Professions spécifiques à l'État
   #                 str_detect(PE, "^332A|^333A|^333B|^333E|^334A|^451C|^452A|^452B|^451E|^531A|^532A|^532B|^532C|^522A")
   #               # =>  P %in% c("332a-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés",
   #               # "333a-Magistrats",
   #               # "333b-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes",
   #               # "333e-Autres personnels administratifs de catégorie A de l'Etat (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
   #               # "334a-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)",
   #               # "451c-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés",
   #               # "452b-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie",
   #               # "451e-Autres personnels administratifs de catégorie B de État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
   #               # "531a-Agents de police de État",
   #               # "532a-Gendarmes (de grade inférieur à adjudant)",
   #               # "532b-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)",
   #               # "532c-Hommes du rang (sauf pompiers militaires)",
   #               # "522a-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes",
   #               # "452a-Inspecteurs et officiers de police")
   #               ~ "2-État", 
   #               
   #               STATUT == "2"         ~ "1-Collectivités locales",
   #               
   #               STATUT %in% c("4", "6")
   #               & str_detect(NBSALENTC2, "^1|^2") #NBSALENTC %in% c("1", "2") 
   #               ~ "5-PME (et assos)",
   #               
   #               STATUT %in% c("4", "6")
   #               & str_detect(NBSALENTC2, "^3")  #NBSALENTC == "3"          
   #               ~ "6-Grandes entreprises",
   #               
   #               STATUT %in% c("4", "6")
   #               & is.na(NBSALENTC2)
   #               ~ "7-Ent taille inconnue",
   #               
   #               STATUT == "7"            ~ "8-Particuliers",
   #               STATUT %in% c("10", "9") ~ "I-Pas d’employeur"
   #             ) 
   #     )
   #   )) %>% deframe()
   # # tab(ct, EMPnewpup, ANNEE)
   # # tab(ct, EMPnewpup, ANNEE, pct = "col")
   # # tab(ct, EMPnewpup, ANNEE, pct = "col", wt = pondcal, digits = 1)
   # ##  en 2005 4% de NA à employeur : 618 NA à CHPUB / 598 à NEMPUB parmi les salariés
   # 
   # #Si on enlève les CJUR entreprises en 2005 : CL semble OK, mais État manque environ 1%
   # 
   # salariat0519 <- ct$CHAMP_CT2005 == "1-Champ 2005" & 
   #   !ct$EMPnewpup %in% c("8-Particuliers", "I-Pas d’employeur") & !is.na(ct$EMPnewpup) &
   #   !is.na(ct$pondcal)
   # tab(ct[salariat0519, ], EMPnewpup, ANNEE, pct = "col", wt = pondcal, digits = 1, diff = 1, color = "after_ci")
   
   
  


## Employeur, avec secteur social et médico-social dans collectivités locales 
# À partir des déclarations des salariés (puis harmonisation 2005 par CJUR)

# ct[`2019`,] |>  tab_many(ACTIVFIN, STATUT, cleannames = TRUE) |> tab_xl() #str_detect(ct$STATUT, "^4|^5")
# # En 2019, 
# # - "87-Hébergement médico-social et social" 50% HP, 50% secteur social et médico-social
# # - "88-Action sociale sans hébergement" : idem
# # Également dispersé dans les autres employeurs






   
## Employeur, plus strict entreprises taille inconnue ----
ct$NBSALENTCstrict <- as.factor(case_when(
  ct$ANNEE != "2005"                            ~ as.character(ct$NBSALENTC),
  ct$NBSALA2 == as.character(ct$NBSALB)         ~ as.character(ct$NBSALA2), 
  is.na(ct$NBSALB) & ct$NBSALA2 == "3-500 et +" ~ as.character(ct$NBSALA2),
  is.na(ct$NBSALB)                              ~ NA_character_,
  TRUE                                          ~ as.character(ct$NBSALB), 
))
## tab(ct, NBSALENTCstrict, NBSALENTC, ANNEE) #Idem NBSALENTC sur 2013:2019, sans NA administrations
## tab(ct, NBSALENTCstrict, ANNEE, pct = "col", wt = pondcal) 
## tab(ct, CHPUB_05, NBSALENTCstrict, ANNEE, pct = "col", wt = pondcal)

ct$EMPstrict <- ctraw %>%
  bind_cols(select(ct, NBSALENTCstrict)) %>% 
  transmute(new_var = as.factor(
    if_else(ANNEE == "2005", 
            true  = case_when(
              STAT2_05 == "1" | cser %in% c("1", "2") | str_detect(PE, "^312C|^312D|^312E")
              ~ "I-Pas d’employeur", 
              
              str_detect(CHPUB_05, "^3") |
                str_detect(CJUR_05, "^7364|^7365|^7366") |
                # => CJUR %in% c("7364-Établissement d'hospitalisation",
                # "7365-Syndicat inter hospitalier",
                # "7366-Établissement public local social et médico-social")
                #( (is.na(CJUR_05) | CJUR_05 == "") & str_detect(NAF_05, "^851A") & str_detect(NEWPUB_05, "^1|^2") ) |
                str_detect(PE, "^344C") #Internes en médecine
              ~ "3-Hôpitaux publics",
              
              str_detect(PE, "^333C|^451A|^521A") |                                  
                str_detect(PE, "^333D|^451B|^521B") | 
                str_detect(NAF_05, "^641A|^642A")  #"641A-Postes nationales" "642A-Télécommunications nationales"
              ~ "6-Grandes entreprises", 
              
              #Professions spécifiques aux CL (une fois hôpitaux classés par CJUR)
              str_detect(PE, "^332B|^333F|^451F|^531B")
              # => P %in% c("332B-Ingénieurs des collectivités locales et des hôpitaux",
              # "333F-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)",
              # "451F-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)",
              # "531B-Agents des polices municipales")
              ~ "1-Collectivités locales",
              
              
              #État : 
              # On enlève les Groupement de droit privé non doté de la personnalité morale (aucun dans public EE)
              !str_detect(CJUR_05, "^2") & ( #|^4|^5 #entreprises de droit public ou de droit privé
                str_detect(CHPUB_05, "^1") | 
                  str_detect(PE, "^376F|^467D|^545D") | #Profession Sécurité sociale
                  (str_detect(CHPUB_05, "^2") & str_detect(PE, "^421|^422|^341|^342")) | #Enseignants, qui tombent dans CL par CJUR 
                  #   Professions spécifiques à l'État
                  str_detect(PE, "^332A|^333A|^333B|^333E|^334A|^451C|^452A|^452B|^451E|^531A|^532A|^532B|^532C|^522A")
              )
              # =>  P %in% c("332a-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés",
              # "333a-Magistrats",
              # "333b-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes",
              # "333e-Autres personnels administratifs de catégorie A de l'Etat (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
              # "334a-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)",
              # "451c-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés",
              # "452b-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie",
              # "451e-Autres personnels administratifs de catégorie B de État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
              # "531a-Agents de police de État",
              # "532a-Gendarmes (de grade inférieur à adjudant)",
              # "532b-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)",
              # "532c-Hommes du rang (sauf pompiers militaires)",
              # "522a-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes",
              # "452a-Inspecteurs et officiers de police")
              ~ "2-État", 
              
              
              str_detect(CHPUB_05, "^2") #& !str_detect(CJUR_05, "^2|^3|^4|^5")  #On enlève les entreprises de droit public ou privé des CL (offices HLM, etc.) 
              ~ "1-Collectivités locales", # str_detect(CJUR_05, "^5546|^5547|^5646|^5647") #7371 = étab pub
              
              
              # Si CJUR est public on ajoute dans le public, pour ensuite mettre y compris des NA à CHPUB_05 en entreprises
              
              # Au sein de la CJ1 "7-Personne morale et organisme soumis au droit administratif" : 
              #État 
              str_detect(CJUR_05, "^71") 
              # => CJ2 == "71-Administration de l'État"
              # c-a-d CJUR %in% c("7111-Autorité constitutionnelle","7112-Autorité administrative indépendante",
              # "7113-Ministère", "7120-Service central d'un ministère",
              # "7160-Service déconcentré à compétence nationale d'un ministère (hors Défense)",
              # "7171-Service déconcentré de l'État à compétence (inter) régionale",
              # "7172-Service déconcentré de l'État à compétence (inter) départementale",
              # "7179-(Autre) Service déconcentré de l'État à compétence territoriale")
              | str_detect(CJUR_05, "^738") 
              # => CJ2 == "72-Etablissement public administratif", national
              ~ "2-État",
              
              #Collectivités locales
              str_detect(CJUR_05, "^72|^73") 
              # => CJ2 == "72-Collectivité territoriale"
              # => CJ2 == "73-Etablissement public administratif"
              # sauf établissement nationaux et hospitaliers/profs, déjà codés plus haut État/HP
              ~ "1-Collectivités locales",
              
              str_detect(CHPUB_05, "^4") ~ "8-Particuliers",
              
              #(str_detect(CHPUB_05, "^5") #| (str_detect(CHPUB_05, "^1") & str_detect(CJUR_05, "^2|^3|^4|^5"))
              #)
              #& 
              str_detect(NBSALENTCstrict, "^1|^2") #NEWTEFEN_05 %in% c("1", "2", "3", "4") 
              ~ "5-PME (et assos)", 
              
              #(str_detect(CHPUB_05, "^5") #| (str_detect(CHPUB_05, "^1") & str_detect(CJUR_05, "^2|^3|^4|^5")) 
              #)
              #& 
              str_detect(NBSALENTCstrict, "^3") #NEWTEFEN_05 %in% c("5", "6")
              ~ "6-Grandes entreprises", 
              
              #(str_detect(CHPUB_05, "^5") #| (str_detect(CHPUB_05, "^1")  & str_detect(CJUR_05, "^2|^3|^4|^5"))
              #)
              #&
              is.na(NBSALENTCstrict)
              ~ "7-Ent taille inconnue",
            ),         
            
            false = case_when(
              !(STATUT %in% c("10", "9", "8")) 
              & (cser %in% c("1", "2") | str_detect(PE, "^312C|^312D|^312E")) 
              ~ "I-Pas d’employeur",
              
              STATUT == "3"                          ~ "3-Hôpitaux publics",
              STATUT == "5"                          ~ "3-Hôpitaux publics",
              str_detect(PE, "^344C")                ~ "3-Hôpitaux publics",#Internes en médecine
              
              str_detect(PE, "^333C|^451A|^521A") |                                  
                str_detect(PE, "^333D|^451B|^521B")       ~ "6-Grandes entreprises", 
              
              #Professions spécifiques aux CL (une fois hôpitaux classés par CJUR)
              str_detect(PE, "^332B|^333F|^451F|^531B")
              # => P %in% c("332b-Ingénieurs des collectivités locales et des hôpitaux",
              # "333f-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)",
              # "451f-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)",
              # "531b-Agents des polices municipales")
              ~ "1-Collectivités locales",
              
              #État : + Enseignants, qui tombent dans CL par CJUR ; + Sécurité sociale
              STATUT == "1" | 
                str_detect(PE, "^376F|^467D|^545D") |
                (STATUT == "2" & str_detect(PE, "^421|^422|^341|^342")) | 
                #   Professions spécifiques à l'État
                str_detect(PE, "^332A|^333A|^333B|^333E|^334A|^451C|^452A|^452B|^451E|^531A|^532A|^532B|^532C|^522A")
              # =>  P %in% c("332a-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés",
              # "333a-Magistrats",
              # "333b-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes",
              # "333e-Autres personnels administratifs de catégorie A de l'Etat (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
              # "334a-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)",
              # "451c-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés",
              # "452b-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie",
              # "451e-Autres personnels administratifs de catégorie B de État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
              # "531a-Agents de police de État",
              # "532a-Gendarmes (de grade inférieur à adjudant)",
              # "532b-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)",
              # "532c-Hommes du rang (sauf pompiers militaires)",
              # "522a-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes",
              # "452a-Inspecteurs et officiers de police")
              ~ "2-État", 
              
              STATUT == "2"         ~ "1-Collectivités locales",
              
              STATUT %in% c("4", "6")
              & str_detect(NBSALENTCstrict, "^1|^2") #NBSALENTC %in% c("1", "2") 
              ~ "5-PME (et assos)",
              
              STATUT %in% c("4", "6")
              & str_detect(NBSALENTCstrict, "^3")  #NBSALENTC == "3"          
              ~ "6-Grandes entreprises",
              
              STATUT %in% c("4", "6")
              ~ "7-Ent taille inconnue",
              
              STATUT == "7"                  ~ "8-Particuliers",
              STATUT %in% c("10", "9") ~ "I-Pas d’employeur"
            ) 
    )
  )) %>% deframe()
# tab(ct, EMPstrict, ANNEE)
# tab(ct, EMPstrict, ANNEE, pct = "col")
# tab(ct, EMPstrict, ANNEE, pct = "col", wt = pondcal, digits = 1)
##  en 2005 4% de NA à employeur : 618 NA à CHPUB / 598 à NEMPUB parmi les salariés
## ct[ct$ANNEE == "2005" & is.na(ct$CHPUB_05) & ct$STAT2_05 == "2-Salarié", ] |> tab(CSE)

#14.2% en 2005 dans État, pour 10.7% en 2013... 
#14.7% en 2005 pour 10.7% en 2013 avec professions spécifiques à l'État (pas bcp plus)

#salariat0519 <- ct$CHAMP_CT2005 == "1-Champ 2005" & 
#  !ct$EMPstrict %in% c("8-Particuliers", "I-Pas d’employeur") & !is.na(ct$EMPstrict) &
#  !is.na(ct$pondcal)
#salariat0513 <- ct$CHAMP_CT2005 == "1-Champ 2005" & ct$ANNEE %in% c("2005", "2013") & 
#  !ct$EMPstrict %in% c("8-Particuliers", "I-Pas d’employeur") & !is.na(ct$EMPstrict) &
#  !is.na(ct$pondcal)
#tab(ct[salariat0519, ], EMPstrict, ANNEE, pct = "col", wt = pondcal, digits = 1, diff = 1, color = "after_ci")

   
   
   
   
   
   
   
   
   
   
   
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








#Variable comptant le nombre de contraintes : 
count_2lfactors <- function (data, var, name) {
  mutate(data, !!sym(name) := if_else(condition = !!sym(var) == levels(!!(sym(var)))[1],
                                      true = !!sym(name) + 1L, false = !!sym(name))
  )
}

ct <- ct %>% mutate(NBCONTR = 0L, NBCTREVAL = 0L, NBCTREXEC = 0L, NBRYTHME = 0L, NBCONTR_no_obj = 0L)
ct <-  list("OBJECTIF", "EVACRIT", "PROCEDUR", 
            "RWCAD", "RWDEP", "RWNORMH", "RWSURV", "RWINFO", "RWTECH", "RWCOLEG", "RWDEM") %>% 
  reduce(.init = ct, .f = ~ count_2lfactors(.x, .y, "NBCONTR")) 

ct <- list("OBJECTIF", "EVACRIT", "PROCEDUR", "RWINFO") %>% 
  reduce(.init = ct, .f = ~ count_2lfactors(.x, .y, "NBCTREVAL")) 

ct <- list("RWCAD", "RWDEP", "RWNORMH", "RWSURV", "RWTECH", "RWCOLEG", "RWDEM") %>% 
  reduce(.init = ct, .f = ~ count_2lfactors(.x, .y, "NBCTREXEC")) 

ct <- list("RWCAD", "RWDEP", "RWNORMH", "RWSURV", "RWTECH", "RWCOLEG", "RWDEM", "RWINFO") %>% 
  reduce(.init = ct, .f = ~ count_2lfactors(.x, .y, "NBRYTHME")) 

ct <- list("EVACRIT", "PROCEDUR", 
           "RWCAD", "RWDEP", "RWNORMH", "RWSURV", "RWINFO", "RWTECH", "RWCOLEG", "RWDEM") %>% 
  reduce(.init = ct, .f = ~ count_2lfactors(.x, .y, "NBCONTR_no_obj")) 
#ct |> select(all_of(active_vars), NBCONTR, NBCTREVAL, NBCTREXEC, NBRYTHME, NBCONTR_no_obj)


vars_contraintes <- c("OBJECTIF", "EVACRIT", "PROCEDUR", 
                      "RWCAD", "RWDEP", "RWNORMH", "RWSURV", "RWINFO", "RWTECH", "RWCOLEG", "RWDEM")
vars_intensite   <- c("RWCAD", "RWDEP", "RWNORMH", "RWSURV", "RWTECH", "RWCOLEG", "RWDEM", "RWINFO")
vars_eval        <- c("OBJECTIF", "EVACRIT", "PROCEDUR", "RWINFO")
vars_exec        <- c("RWCAD", "RWDEP", "RWNORMH", "RWSURV", "RWTECH", "RWCOLEG", "RWDEM")

ct <- ct |> mutate(
  score_contraintes = NBCONTR,
  score_intensite   = NBRYTHME,
  score_eval        = NBCTREVAL,
  score_exec        = NBCTREXEC,
)






##Contrat de travail (diff fonctionnaire / CDI) ----
ct$CONTR <- ctraw %>%
  mutate(CONTR = fct_cross(TYPEMPLOI, fct_explicit_na(TITPUBR, "NA-NA")) %>%
           tabxplor:::fct_replace("^([^-]+)-([^:]+):([^-]+)-(.+)", "\\1:\\3-\\2/\\4") %>%
           tabxplor:::fct_case_when_recode(c(NULL               = "^99",
                                             "1-Fonctionnaire"  = str_c("^[^:]+:", c("1", "2", "3"), collapse = "|"),
                                             "2-CDI"            = "^6|^7",
                                             # "3-CDD"            = "CDD|Emploi aidé|Stage rémunéré",
                                             # "4-Intérim"        = "Intérim",
                                             # "5-Apprentis"      = "Appr."#,
                                             "3-Précaire"            = "^1|^2|^3|^4|^5",
                                             "NULL"                  = "^.*:"
                                             #"I-Indépendants"   = "^11|^12|^13"
           )),
         
         CONTR13 = fct_cross(TYPEMPLOI, fct_explicit_na(TITPUB, "NA-NA")) %>%
           tabxplor:::fct_replace("^([^-]+)-([^:]+):([^-]+)-(.+)", "\\1:\\3-\\2/\\4") %>%
           tabxplor:::fct_case_when_recode(c(NULL               = "^99",
                                             "1-Fonctionnaire"  = str_c("^[^:]+:", c("1", "2"), collapse = "|"),
                                             "2-CDI"            = "^6|^7",
                                             # "3-CDD"            = "CDD|Emploi aidé|Stage rémunéré",
                                             # "4-Intérim"        = "Intérim",
                                             # "5-Apprentis"      = "Appr."#,
                                             "3-Précaire"            = "^1|^2|^3|^4|^5",
                                             "NULL"                  = "^.*:"
                                             #"I-Indépendants"   = "^11|^12|^13"
           )),
         
         CONTR05 = as.factor(case_when(
           str_detect(TITC_05, "^1|^2")           ~ "1-Fonctionnaire", 
           str_detect(CONTRA_05, "^1")            ~ "2-CDI", 
           str_detect(CONTRA_05, "^2|^3|^4|^5")   ~ "3-Précaire"
         ))
         
  ) %>%
  transmute(CONTR = case_when(ANNEE == "2005" ~ fct_expand(CONTR05, levels(CONTR)),
                              ANNEE == "2013" ~ CONTR13, 
                              TRUE            ~ CONTR)) %>% 
  deframe()
# tab(ct, CONTR, ANNEE, pct = "col", wt = pondcal)


# ct$ACTIV_ETAT <- ctraw %>%
#   transmute(ACTIV_ETAT = case_when(
#     str_detect(ACTIVFIN, "^53|^49|^61|^84|^85") ~ factor(ACTIVFIN, append(levels(ACTIVFIN), "99-Autres")), 
#     # "53-Activités de poste et de courrier"
#     # "49-Transports terrestres et transport par conduites"
#     # "61-Télécommunications"
#     # "84-Administration publique et défense ,sécurité sociale obligatoire"
#     # "85-Enseignement"
#     TRUE                                     ~ factor("99-Autres")
#   ) ) %>% deframe()


# [...]

## Contact avec le public -----------------------------------------------------------------

ct <- ct %>% 
  mutate(
    PUBLIC2acm = fct_recode(PUBLIC2,
                            "1-Toujours téléphone avec public" = "1-Tj téléphone",
                            "2-Souvent téléphone"              = "2-Sv téléphone",
                            "3-Parfois téléphone"              = "3-Pf téléphone",
                            "4-Seulement face-à-face"          = "4-Jm téléphone"
    ) %>% 
      fct_explicit_na("5-Pas contact avec public"),
    
    PUBLICtel = fct_recode(PUBLIC2acm, 
                           "2-Contact avec public" = "2-Souvent téléphone",    
                           "2-Contact avec public" = "3-Parfois téléphone",   
                           "2-Contact avec public" = "4-Seulement face-à-face"
    ))
# tab(ct, PUBLIC2acm)
# tab(ct, PUBLICtel)

ct$PUBLICtel <- fct_recode(
  ct$PUBLICtel,
  "1-Toujours contact téléphone + face-à-face" = "1-Toujours téléphone avec public",
  "2-Contact avec le public"                   = "2-Contact avec public"           ,
  "5-Pas contact avec le public"               = "5-Pas contact avec public"       
)



# # Variable employeur basée sur déclaration des salariés (et catégories d'ent)
# ct$EMP <- ct %>%
#   transmute(new_var = as.factor(case_when(
#     !str_detect(STATUT, "^10|^9|^8")
#     & (cser %in% c("1", "2") | str_detect(PE, "^312C|^312D|^312E")) #Consultants libéraux
#     ~ "I-Pas d’employeur",
#     
#     str_detect(STATUT, "^3")                          ~ "3-Hôpitaux publics",
#     str_detect(STATUT, "^5")                          ~ "3-Hôpitaux publics",
#     
#     str_detect(PE, "^333C|^451A|^521A") |                                  # La Poste
#       str_detect(PE, "^333D|^451B|^521B")       ~ "7-Grandes entreprises", # France Télécom 
#     
#     str_detect(STATUT, "^1") | str_detect(PE, "^376F|^467D|^545D")   ~ "2-État",   # Sécu
#     str_detect(STATUT, "^2")         ~ "1-Collectivités locales",
#     
#     str_detect(STATUT, "^4|^6")
#     & cat_groupe %in% c("PME", "MICRO" ) ~ "5-PME", #Normal, 50-250 veut en fait dire 40-499
#     
#     str_detect(STATUT, "^4|^6")
#     & cat_groupe == "ETI"          ~ "6-Entreprises intermédiaires",
#     
#     str_detect(STATUT, "^4|^6")
#     & cat_groupe == "GE"           ~ "7-Grandes entreprises",
#     
#     str_detect(STATUT, "^4|^6")
#     & is.na(cat_groupe)            ~ "8-Ent taille inconnue",
#     
#     str_detect(STATUT, "^7")                 ~ "9-Particuliers",
#     str_detect(STATUT, "^9|^10")             ~ "I-Pas d’employeur"
#   ) )) %>% deframe()
# # tab(ct, EMP)
# # ct$cat_groupe %>% levels()
# 


## Catégories agrégrées d'employeurs ----
ct <- ct %>% 
  mutate(
    EMP_ADM_ENT = tabxplor:::fct_case_when_recode(EMP, c(
      "1-Administrations publiques" = "^1|^2|^3",
      "2-Entreprises" = "^4|^5|^6|^7|^8",
      NULL = "^9|^I") ),
    
    EMP2 = tabxplor:::fct_case_when_recode(EMP, c("4-Entreprises" = "^5|^6|^7"))
  )




#Employeurs par secteurs (État, Grandes entreprises de service, etc.)
ct <- ct %>%
  mutate(secteur = as.factor(
    if_else(ANNEE == "2005", 
            
            #2005
            true = case_when(
              # naf38 ==  "PZ - Enseignement" & 
              #   EMP %in% c("1-Collectivités locales","2-État", "3-Hôpitaux publics") ~ "31-Enseignement public", 
              # 
              # naf38 ==  "PZ - Enseignement"                                          ~ "32-Enseignement privé", 
              
              EMP == "1-Collectivités locales" ~ "10-Collectivités locales",
              EMP == "2-État"                  ~ "20-État",
              
              EMP == "3-Hôpitaux publics" ~ "41-Hôpitaux publics", 
              str_detect(NAF_05, "^851A") ~ "42-Cliniques privées",
              
              
              str_detect(NAF36_05, "^A0")                        ~ "50-Agriculture",
              #A0 Agriculture, sylviculture et pêche 
              
              
              str_detect(NAF36_05, "^B0|^F4|^G1|^C3") |  str_detect(NAF_05, "^27")  ~ "60-Industries de process",
              #B0 Industries agricoles et alimentaires 
              #F4 Chimie, caoutchouc, plastiques 
              #G1 Production de combustibles et de carburants 
              #C3 Pharmacie, parfumerie et entretien 
              #+ métallurgie
              
              str_detect(NAF36_05, "^C4|^D0|^E1|^E2|^E3")  ~ "61-Industries d’équipement", 
              #C4 Industries des équipements du foyer 
              #D0 Industrie automobile 
              #E1 Construction navale; aéronautique et ferroviaire 
              #E2 Industries des équipements mécaniques 
              #E3 Industries des équipements électriques et électroniques 
              
              str_detect(NAF36_05, "^C1|^C2|^F1|^F2|^F3|^F5|^F6|^G2") ~ "62-Industrie autres",
              #C1 Habillement, cuir 
              #C2 Edition, imprimerie, reproduction 
              #F1 Industries des produits minéraux 
              #F2 Industrie textile 
              #F3 Industries du bois et du papier
              #F5 Métallurgie et transformation des métaux 
              #F6 Industries des composants électriques et électroniques 
              #G2 Eau, gaz, électricité 
              
              str_detect(NAF36_05, "^H0")         ~ "70-Construction",
              str_detect(NAF36_05, "^J1|^J2|^J3") ~ "71-Commerce", 
              str_detect(NAF36_05, "^K0")         ~ "72-Logistique",
              
              #ACTIVFIN == "70-Activités des sièges sociaux ,conseil de gestion" ~ "Sièges sociaux",  25...
              
              str_detect(NAF36_05, "^P1")     ~ "73-Hôtellerie-restauration",
              
              str_detect(NAF_05, "^72|^642")  ~ "74-Numérique et télécoms",
              #NAF2008 : "JB - Télécommunications",
              #          "JC - Activités informatiques et services d'information"
              #ICI : "72-Activités informatiques", "642-Télécommunications"
              
              
              str_detect(NAF36_05, "^L0")     ~ "75-Secteur financier",
              
              str_detect(NAF36_05, "^M0|^N1|^N2|^N3|^N4|^P2|^Q1|^Q2|^R1|^R2") ~ "76-Autres services",
              #M0 Activités immobilières 
              #N1 Postes et télécommunications (sans les télécoms)
              #N2 Conseils et assistance 
              #N3 Services opérationnels 
              #N4 Recherche et développement 
              #P2 Activités récréatives, culturelles et sportives 
              #Q1 Education 
              #Q2 Santé, action sociale 
              #R1 Administrations publiques 
              #R2 Activités associatives et extra-territoriale
              
              str_detect(NAF36_05, "^P3") ~ "80-Particuliers employeurs"
              #P3 Services personnels et domestiques 
            ), 
            
            #2013 à 2019 
            false = case_when(
   
              # naf38 ==  "PZ - Enseignement" & 
              #   EMP %in% c("1-Collectivités locales","2-État", "3-Hôpitaux publics") ~ "31-Enseignement public", 
              # 
              # naf38 ==  "PZ - Enseignement"                                          ~ "32-Enseignement privé", 
              
              EMP == "1-Collectivités locales" ~ "10-Collectivités locales",
              EMP == "2-État"                  ~ "20-État",
              
              str_detect(STATUT, "^3|^5")      ~ "41-Hôpitaux publics",
              str_detect(STATUT, "^4")         ~ "42-Cliniques privées",
              
              
              str_detect(naf38, "^AZ")                        ~ "50-Agriculture",
              # "AZ-Agriculture, sylviculture et pêche" 
              
              
              str_detect(naf38, "^CA|^CD|^CE|^CF") |  ACTIVFIN == "24-Métallurgie"  ~ "60-Industries de process",
              #"CA - Fabric. denrées alimentaires, boissons et prdts à base de tabac",
              #"CD - Cokéfaction et raffinage", "CE - Industrie chimique", "CF - Industrie pharmaceutique"
              
              str_detect(naf38, "^CJ|^CK|^CL|^CM")  ~ "61-Industries d’équipement", 
              #"CJ - Fabrication d'équipements électriques",
              #"CK - Fabrication de machines et équipements n.c.a.",
              #"CL - Fabrication de matériels de transport",
              #"CM - Autres ind. manuf. ,répar. & install. de machines et d'équipemnts"
              
              str_detect(naf38, "^BZ|^CB|^CC|^CG|^CH|^CI|^DZ|^EZ") ~ "62-Industrie autres",
              #"BZ - Industries extractives ",
              #"CB - Fabric. textiles, industries habillement, cuir et chaussure",
              #"CC - Travail du bois, industries du papier et imprimerie ",
              #"CG - Fab. prdts en caoutc. & plastiq. & aut. prdts minéraux non métal.",
              #"CH - Métallurgie & fab. de prdts métalliques sauf machines & équipmnts",
              #"CI - Fabrication de produits informatiques, électroniques et optiques",
              #"DZ - Prod. & distribution électricité, gaz, vapeur & air conditionné",
              #"EZ - Prod. & distrib. eau assainisst, gestion déchets & dépollution"
              
              str_detect(naf38, "^FZ") ~ "70-Construction",
              str_detect(naf38, "^GZ") ~ "71-Commerce",
              str_detect(naf38, "^HZ") ~ "72-Logistique",
              
              #ACTIVFIN == "70-Activités des sièges sociaux ,conseil de gestion" ~ "Sièges sociaux",  25...
              
              str_detect(naf38, "^IZ")     ~ "73-Hôtellerie-restauration",
              
              str_detect(naf38, "^JB|^JC") ~ "74-Numérique et télécoms",
              #"JB - Télécommunications",
              #"JC - Activités informatiques et services d'information"
              
              str_detect(naf38, "^KZ")     ~ "75-Secteur financier",
              
              str_detect(naf38, "^JA|^LZ|^MA|^MB|^MC|^NZ|^QB|^RZ|^SZ|^UZ|^OZ|^QA")   ~ "76-Autres services",
              #"JA - Édition, audiovisuel et diffusion",
              #"LZ - Activités immobilières",
              #"MA - Act. juri., compta., de gest., archi., ingé., ctrle & anal. tech.",
              #"MB - Recherche-développement scientifique",
              #"MC - Autres activités spécialisées, scientifiques et techniques",
              #"NZ - Activités de services administratifs et de soutien",
              #"QB - Hébergement médico-social & social et action sociale sans hébgmnt",
              #"RZ - Arts, spectacles et activités récréatives",
              #"SZ - Autres activités de services ",
              #"UZ - Activités extra-territoriales",
              #"OZ - Administration publique",
              #"QA - Activités pour la santé humaine"
              
              ##En 2019, rien dans "activité des ménages en tant qu'employeurs" (NA) : on code avec STATUT
              #tab(ct, naf38, ANNEE)
              #tab(ct[ct$ANNEE == "2019", ], naf38, STATUT)
              str_detect(naf38, "^TZ") | EMP == "8-Particuliers" ~ "80-Particuliers employeurs"
            )
    )))
# tab(ct[ct$ANNEE != "2005"], naf38, secteur)
# tab(ct, secteur, ANNEE)
# tab(ct, secteur, ANNEE, pct = "col", wt = pondcal)











ct <- ct %>%
  mutate(secteurEMP = fct_cross(EMP, secteur), 
         
         secteurEMP = fct_relabel(secteurEMP, ~ case_when(
           str_detect(., "^5")  ~ str_replace(., "(5)-PME[^\\d]+(\\d{2})-"                , "\\1\\2-PME "),
           str_detect(., "^6")  ~ str_replace(., "(6)-Grandes entreprises[^\\d]+(\\d{2})-", "\\1\\2-GE "),
           str_detect(., "^7")  ~ str_replace(., "(7)-Ent taille inconnue[^\\d]+(\\d{2})-", "\\1\\2-EntI ")
         )) 
  ) %>% 
  mutate(secteurEMP = as.factor(case_when(EMP2 != "4-Entreprises"  ~ str_c(str_sub(EMP2, 1L, 1L), 
                                                                           "00", 
                                                                           str_sub(EMP2, 2L, -1L)
  ), 
  TRUE                     ~ as.character(secteurEMP)
  ))
  )
# tab(ct, secteurEMP)

#tabxplor:::fct_recode_helper(ct, secteurEMP)
ct <- ct |> 
  mutate(EMP4 = fct_recode(secteurEMP,
                           "41-Collectivités locales"           = "100-Collectivités locales"       ,
                           "11-État"                            = "200-État"                        ,
                           "21-Hôpitaux publics"                = "300-Hôpitaux publics"            ,
                           
                           "22-Cliniques privées"               = "542-PME Cliniques privées"       ,
                           "90-Autres (EntI, Ag, GCstr)"        = "550-PME Agriculture"             ,
                           "42-PME (et assos)"                  = "560-PME Industries de process"   ,
                           "42-PME (et assos)"                  = "561-PME Industries d’équipement" ,
                           "42-PME (et assos)"                  = "562-PME Industrie autres"        ,
                           "42-PME (et assos)"                  = "570-PME Construction"            ,
                           "42-PME (et assos)"                  = "571-PME Commerce"                ,
                           "42-PME (et assos)"                  = "572-PME Logistique"              ,
                           "42-PME (et assos)"                  = "573-PME Hôtellerie-restauration" ,
                           "42-PME (et assos)"                  = "574-PME Numérique et télécoms"   ,
                           "42-PME (et assos)"                  = "575-PME Secteur financier"       ,
                           "42-PME (et assos)"                  = "576-PME Autres services"         ,
                           "42-PME (et assos)"                  = "580-PME Particuliers employeurs" ,
                           
                           "22-Cliniques privées"               = "642-GE Cliniques privées"        ,
                           "90-Autres (EntI, Ag, GCstr)"        = "650-GE Agriculture"              ,
                           "30-Grande industrie"                = "660-GE Industries de process"    ,
                           "30-Grande industrie"                = "661-GE Industries d’équipement"  ,
                           "30-Grande industrie"                = "662-GE Industrie autres"         ,
                           "90-Autres (EntI, Ag, GCstr)"        = "670-GE Construction"             ,
                           "12-Grandes entreprises de services" = "671-GE Commerce"                 ,
                           "12-Grandes entreprises de services" = "672-GE Logistique"               ,
                           "12-Grandes entreprises de services" = "673-GE Hôtellerie-restauration"  ,
                           "12-Grandes entreprises de services" = "674-GE Numérique et télécoms"    ,
                           "12-Grandes entreprises de services" = "675-GE Secteur financier"        ,
                           "12-Grandes entreprises de services" = "676-GE Autres services"          ,
                           "90-Autres (EntI, Ag, GCstr)"        = "680-GE Particuliers employeurs"  ,
                           
                           "22-Cliniques privées"               = "742-EntI Cliniques privées"      ,
                           "90-Autres (EntI, Ag, GCstr)"        = "750-EntI Agriculture"            ,
                           "90-Autres (EntI, Ag, GCstr)"        = "760-EntI Industries de process"  ,
                           "90-Autres (EntI, Ag, GCstr)"        = "761-EntI Industries d’équipement",
                           "90-Autres (EntI, Ag, GCstr)"        = "762-EntI Industrie autres"       ,
                           "90-Autres (EntI, Ag, GCstr)"        = "770-EntI Construction"           ,
                           "90-Autres (EntI, Ag, GCstr)"        = "771-EntI Commerce"               ,
                           "90-Autres (EntI, Ag, GCstr)"        = "772-EntI Logistique"             ,
                           "90-Autres (EntI, Ag, GCstr)"        = "773-EntI Hôtellerie-restauration",
                           "90-Autres (EntI, Ag, GCstr)"        = "774-EntI Numérique et télécoms"  ,
                           "90-Autres (EntI, Ag, GCstr)"        = "775-EntI Secteur financier"      ,
                           "90-Autres (EntI, Ag, GCstr)"        = "776-EntI Autres services"        ,
                           "90-Autres (EntI, Ag, GCstr)"        = "780-EntI Particuliers employeurs", 
                           
                           "NULL"                               = "800-Particuliers"                ,
                           "NULL"                               = "I00-Pas d’employeur"             
  ) |> 
    fct_relevel(sort)
  )
# tab(ct, EMP4)


ct <- ct %>% 
  mutate(EMP4_ord = fct_relevel(EMP4, "41-Collectivités locales","42-PME (et assos)",
                                "21-Hôpitaux publics","22-Cliniques privées",
                                "11-État","12-Grandes entreprises de services",
                                "30-Grande industrie",
                                "90-Autres (EntI, Ag, GCstr)" ) , 
         
         EMP5 = fct_recode(EMP4, 
                           "11-État"                            = "21-Hôpitaux publics",
                           "12-Grandes entreprises de services" = "22-Cliniques privées",
         ) %>% fct_relabel(~ str_replace(., "État", "État_HP")), 
         
         EMP5_ETAT_GES = case_when(
           EMP5 %in% c("11-État_HP","12-Grandes entreprises de services") ~ EMP5, 
           TRUE                                                           ~ factor(NA_character_)
         ),
         
         # EMP6 = fct_recode(EMP, 
         #                   "1-État_HP"               = "2-État",
         #                   "1-État_HP"               = "3-Hôpitaux publics",
         #                   "2-Grandes entreprises"   = "6-Grandes entreprises",
         #                   "3-Collectivités locales" = "1-Collectivités locales",
         #                   "4-PME (et assos)"        = "5-PME (et assos)",
         #                   "5-Ent taille inconnue"   = "7-Ent taille inconnue"   ) %>% 
         #  fct_relevel(sort)
  )
# tab(ct, EMP5, EMP4)
# tab(ct, EMP6)


ct <- ct |>
  mutate(EMP4ab = fct_recode(EMP4,
                             "11-État"           = "11-État"                           ,
                             "12-GE services"    = "12-Grandes entreprises de services",
                             "30-GE industrie"   = "30-Grande industrie"               ,
                             "21-Hôpitaux pub"   = "21-Hôpitaux publics"               ,
                             "22-Cliniques priv" = "22-Cliniques privées"              ,
                             "41-Coll loc"       = "41-Collectivités locales"          ,
                             "42-PME"            = "42-PME (et assos)"                 ,
                             "90-Autres"         = "90-Autres (EntI, Ag, GCstr)"       
  ))

# EMP6 après PcsPP, ci-dessous







# saveRDS(ct, file = "~\\Data\\Conditions de travail\\ct_temp.rds", compress = TRUE)

##PcsPP : CSP recodées pour comparaison public privé --------------------------
ct <- ct %>% 
  pcspp::pcspp(profession = PE, admin_ent = EMP_ADM_ENT, sexe = SEXE, 
               sexe_wt = pondcal, gender_sign = FALSE) %>% 
  mutate(across(starts_with("PPP"), 
                ~ fct_relabel(., ~ str_remove_all(., 
                                                  str_c("\\u2642", "\\u2640", 
                                                        collapse = "|")
                ) %>%
                  str_squish()
                )
  )) %>% 
  mutate(PPP1 = fct_relabel(PPP1, ~ case_when(
    str_detect(., "^1")    ~ "1-Chefs d’entreprise",
    str_detect(., "^2")    ~ "2-Cadres",
    str_detect(., "^3")    ~ "3-Cols blancs subalternes" , 
    str_detect(., "^4")    ~ "4-Professions organisées",
    str_detect(., "^5")    ~ "5-Employées",
    str_detect(., "^6")    ~ "6-Ouvriers",
    str_detect(., "^7")    ~ "6-Ouvriers",
  ))
  )
# tab(ct, PPP1)

#Croisements PcsPP / employeur
ct <- ct %>% 
  mutate(
    PPP1_EGES = fct_cross(EMP5_ETAT_GES, PPP1) %>% 
      fct_relabel(~ str_remove_all(., stringi::stri_unescape_unicode("\\u2642|\\u2640")) %>% 
                    str_replace("11-État_HP:(.)(.+$)", "\\1E\\2 État_HP") %>% 
                    str_replace("12-Grandes entreprises de services:(.)(.+$)", "\\1G\\2 GES") %>% 
                    str_squish() 
      )  %>% 
      fct_lump_min(200, other_level = "ZZ-Autres"), #%>% fct_to_na("Autres")
    
    PPP1_ord_EGES = fct_cross(EMP5_ETAT_GES, PPP1) %>% 
      fct_relabel(~ str_remove_all(., stringi::stri_unescape_unicode("\\u2642|\\u2640")) %>% 
                    str_replace("11-État_HP:(.)(.+$)", "E\\1\\2 État_HP") %>% 
                    str_replace("12-Grandes entreprises de services:(.)(.+$)", "G\\1\\2 GES") %>% 
                    str_squish()
      ) %>% 
      fct_lump_min(200, other_level = "ZZ-Autres") %>%  #%>% fct_to_na("Autres")
      fct_relevel(sort), 
    
    PPP1ex = fct_recode(PPP1,
                        "5-Exécutant·es"  = "5-Employées",
                        "5-Exécutant·es"  = "6-Ouvriers" , 
                        "5-Exécutant·es"  = "7-Agri"        
    )
  )
# tab(ct, PPP1_EGES)
# tab(ct, PPP1_ord_EGES)


ct <- ct |> 
  mutate(PE1 = fct_cross(PPP1, fct_recode(
    EMP4,
    "11-État"    = "11-État"                           ,
    "12-GES"     = "12-Grandes entreprises de services",
    "21-HP"      = "21-Hôpitaux publics"               ,
    "21-HP"      = "22-Cliniques privées"              ,
    "30-GI"      = "30-Grande industrie"               ,
    "41-CL"      = "41-Collectivités locales"          ,
    "42-PME"     = "42-PME (et assos)"                 ,
    "90-Autres"  = "90-Autres (EntI, Ag, GCstr)"       
  )) |> 
    fct_lump_min(150, other_level = "Autres") |> 
    fct_relabel(~ str_replace(., "^(.)-([^:]+):(..)-", "\\1\\3-\\2 ")) |>
    fct_relevel(sort), 
  
  PE2 = fct_cross(PPP2, fct_recode(
    EMP4,
    "11-État"    = "11-État"                           ,
    "12-GES"     = "12-Grandes entreprises de services",
    "21-HP"      = "21-Hôpitaux publics"               ,
    "21-HP"      = "22-Cliniques privées"              ,
    "30-GI"      = "30-Grande industrie"               ,
    "41-CL"      = "41-Collectivités locales"          ,
    "42-PME"     = "42-PME (et assos)"                 ,
    "90-Autres"  = "90-Autres (EntI, Ag, GCstr)"       
  )) |> 
    fct_lump_min(90, other_level = "Autres") |> 
    fct_relabel(~ str_replace(., "^(...)-([^:]+):(..)-", "\\1\\3-\\2 "))|>
    fct_relevel(sort)
  )






# #Une modalité 5 majoritaire manque à STATUTA dans la doc...
# ct |> tab(STATUTA, ANNEE) 
# ct[`2019`, ] |> tab(EMP, STATUTA, AGE4, cleannames = TRUE)
# # La modalité 4 est majoritaire, dans chaque d'âge, pour les salarié·es des hôpitaux 
# # publics, donc à priori c'est des salariés des hôpitaux qu'il s'agit, et la modalité 
# # 5 désigne les "autres salarié·es"

ct <- ct |> 
  mutate(
    # STATUTA = fct_recode(STATUTA, 
    #                      "1-A votre compte"                                 = "1",
    #                      "2-Aide salarié"                                   = "2",
    #                      "3-Salarié de l'État ou des collectivités locales" = "3",
    #                      "4-Salarié d’un établissement hospitalier"         = "4",
    #                      "5-Autre salarié"                                  = "5"
    #               
    # ), 
    
    EMP_ADM_ENT_A = as.factor(case_when(
      str_detect(STATUTA, "^3|^4") ~ "1-Début administration publique",
      str_detect(STATUTA, "^5")    ~ "2-Début entreprise",
      str_detect(STATUTA, "^1|^2") ~ "3-Début à son compte"
    )), 
    
    EMP_ADM_ENT_P = as.factor(case_when(
      ANNEE == 2005                        ~ NA_character_,
      str_detect(STATUTP, "^3")            ~ "1-Père administration publique",
      str_detect(STATUTP, "^4")            ~ "2-Père entreprise",
      str_detect(STATUTP, "^1|^2")         ~ "3-Père à son compte", 
      str_detect(ACTIP, "^1|^2|^3|^4|^5|") ~ "4-Père sans travail"
    )), 
    
    EMP_ADM_ENT_M = as.factor(case_when(
      ANNEE == 2005                ~ NA_character_,
      str_detect(STATUTM, "^3")    ~ "1-Mère administration publique",
      str_detect(STATUTM, "^4")    ~ "2-Mère entreprise",
      str_detect(STATUTM, "^1|^2") ~ "3-Mère à son compte", 
      str_detect(ACTIM, "^1|^2|^3|^4|^5|") ~ "4-Mère sans travail"
      
    )), 
  )

ct <- 
  bind_cols(
    select(ct, -any_of(c("PPPA1", "FAPPPA", "PPPA2", "PPPA3", "PPPA4" ))),
    
    select(ct, EMP_ADM_ENT_A, PROFDEB, SEXE, pondcal) |> 
      pcspp::pcspp("PROFDEB", admin_ent = EMP_ADM_ENT_A, sexe = SEXE, sexe_wt = pondcal) |>
      select(all_of(c("PPPA1"  = "PPP1",
                      "FAPPPA" = "FAPPP",
                      "PPPA2"  = "PPP2",  
                      "PPPA3"  = "PPP3", 
                      "PPPA4"  = "PPP4"))) |>
      mutate(PPPA1 = fct_relabel(PPPA1, ~ case_when(
        str_detect(., "^1")    ~ "1-Début patron",
        str_detect(., "^2")    ~ "2-Début cadre",
        str_detect(., "^3")    ~ "3-Début CBI" , 
        str_detect(., "^4")    ~ "4-Début profession organisée",
        str_detect(., "^5")    ~ "5-Début employée",
        str_detect(., "^6")    ~ "6-Début ouvrier",
        str_detect(., "^7")    ~ "6-Début ouvrier",
      )))
  )
# tab(ct, PPPA1, ANNEE) # => 40% de NA à la première profession.

# ct[`2019`, ] |> tab(NBEMPE, EMP_ADM_ENT_A)
# (!is.na( ct[`2019`, ]$EMP_ADM_ENT_A )) |> sum() # 15397 statut du premier emploi
# (!is.na( ct[`2019`, ]$PPPA1 ))   |> sum() # 14773 CSP recodée du premier emploi
# 
# (!is.na( ct[`2019`, ]$cserdeb )) |> sum() # 15378 CSP du premier emploi renseignées
# (!is.na( ct[`2019`, ]$STATUTA )) |> sum() # 15398 statut premier emploi renseignés
# (!is.na( ct[`2019`, ]$PROFDEB )) |> sum() # 15651 profession du premier emploi renseignées
# 
# # # On perd seulement 512 professions détaillées, directement classées dans les CSE
# # ct[`2019`, ] |> tab(PROFDEB) |> filter(str_detect(PROFDEB, "0$")) |> pull(n) |> sum()
# # ct[`2019`, ] |> tab(PROFDEB) |> filter(!str_detect(PROFDEB, "0$")) |> print(n = 500)
# 
# ## Les 9500 NA au premier emploi sont de tous les ages, donc ce ne sont pas 
# ##   particulièrement les jeunes
# ct[`2019`, ] |> filter(is.na(STATUTA)) |> tab_many(c(AGE4, EMP, PPP2, anciennete))
# 
# # Par contre, les 50 ans et plus ont en moyenne 28 ans d'ancienneté, les 4* ans et plus
# #  en moyenne 18 ans, donc ils ont commencé entre 20 et 30 ans, donc c'est peut-être 
# #  leur premier emploi...
# ct[`2019`, ] |> filter(is.na(STATUTA)) |> tab_many(AGE4, anciennete) |> new_tab() |> 
#   mutate(anciennete = anciennete/12)
# ct[`2019`, ] |> filter(!is.na(STATUTA)) |> tab_many(AGE4, anciennete) |> new_tab() |> 
#   mutate(anciennete = anciennete/12)
# 
# # ct[`2019`, ] |> filter(is.na(STATUTA)) |> 
# #   select(AGE, anciennete, ANFINETU, DIPLOME) |>
# #   mutate(anciennete = anciennete/12, sum = ANFINETU + anciennete) |> 
# #   new_tab()

# # Est-ce que les NA correspondent aux personnes entrées en 2013 dans le panel ?
# ct |> tab_many(NBEMPE, NBEMPP, ANNEE)
#ct[`2019`, ] |> select(NBEMPE, NBEMPP) |> new_tab()

ct <- ct |> mutate(
  
  # # Pour le faire proprement, il faut prendre en compte les changements d'emplois
  # #  depuis 2013 pour le panel
  # across(
  #   starts_with("PPPA"),
  #   ~ as.factor(if_else(NBEMPall == 0 & !is.na(NBEMPall), #& is.na(.),
  #                       true  = "0-Premier emploi",
  #                       false = as.character(.)
  #   )), 
  #   .names = "{.col}_premier"
  #),
  
  PPPA1 = case_when(
    NBEMPall == 0 & !is.na(NBEMPall) & PPP1 == "1-Chefs d’entreprise"                ~  "1-Début patron"               ,
    NBEMPall == 0 & !is.na(NBEMPall) & PPP1 == "2-Cadres"                 ~  "2-Début cadre"                ,
    NBEMPall == 0 & !is.na(NBEMPall) & PPP1 == "3-CBI"                    ~  "3-Début CBI"                  ,
    NBEMPall == 0 & !is.na(NBEMPall) & PPP1 == "4-Professions organisées" ~  "4-Début profession organisée" ,
    NBEMPall == 0 & !is.na(NBEMPall) & PPP1 == "5-Employées"              ~  "5-Début employée"             ,
    NBEMPall == 0 & !is.na(NBEMPall) & PPP1 == "6-Ouvriers"               ~  "6-Début ouvrier"              , 
    NBEMPall == 0 & !is.na(NBEMPall)                                      ~  NA_character_,
    TRUE                  ~ as.character(PPPA1)
  ),
  
  EMP_ADM_ENT_A = as.factor(case_when(
    NBEMPall == 0 & !is.na(NBEMPall) & EMP_ADM_ENT == "1-Administrations publiques"
    ~  factor("1-Début administration publique", levels(EMP_ADM_ENT_A)),
    
    NBEMPall == 0 & !is.na(NBEMPall) & EMP_ADM_ENT == "2-Entreprises"
    ~  factor("2-Début entreprise"),
    
    NBEMPall == 0 & !is.na(NBEMPall) & is.na(EMP_ADM_ENT)
    ~  factor("3-Début à son compte"),
    
    TRUE  ~ EMP_ADM_ENT_A
  ))
)

# ct |> tab_many(NBEMPall, cserdeb, ANNEE)
# ct |> tab_many(PPPA1_premier, ANNEE)



# ct |> #filter(, champ_Etat_HP_cj19) |>
#   tab(PPPA1, PPP1cj, ANNEE, pct = "col", wt = pondcal, cleannames = TRUE, color = "diff")


# # Problème : il y a 1,5 à 2 fois plus de NA dans les administrations publiques...
# ct[`2019`, ] |> tab(EMPcj, PPPA1, wt = pondcal, pct = "row", cleannames = TRUE, color = "diff")
# 
# ct[`2019`, ] |> 
#   mutate(PPPA1 = fct_explicit_na(PPPA1, "0-Premier emploi" )) |> 
#   tab(EMPcj, PPPA1, wt = pondcal, pct = "row", cleannames = TRUE, color = "diff")




ct <- 
  bind_cols(
    select(ct, -any_of(c("PPPPER1", "FAPPPPER", "PPPPER2", "PPPPER2b", "PPPPER3", "PPPPER4" ))),
    
    select(ct, EMP_ADM_ENT_P, PROFPER, pondcal) |> 
      pcspp::pcspp(PROFPER, admin_ent = EMP_ADM_ENT_P,  masculiniser = TRUE) |>
      select(all_of(c("PPPPER1"  = "PPP1",
                      "FAPPPPER" = "FAPPP",
                      "PPPPER2"  = "PPP2",  
                      "PPPPER2b" = "PPP2b",  
                      "PPPPER3"  = "PPP3", 
                      "PPPPER4"  = "PPP4"))) |>
      mutate(PPPPER1 = fct_relabel(PPPPER1, ~ case_when(
        str_detect(., "^1")    ~ "1-Père patron",
        str_detect(., "^2")    ~ "2-Père cadre",
        str_detect(., "^3")    ~ "3-Père CBI" , 
        str_detect(., "^4")    ~ "4-Père profession organisée",
        str_detect(., "^5")    ~ "5-Père employé",
        str_detect(., "^6")    ~ "6-Père ouvrier",
        str_detect(., "^7")    ~ "6-Père ouvrier",
      )))
  ) 
# tab(ct, PPPPER1, ANNEE)
# => Un tiers de NA à la profession du père. 

ct <- 
  bind_cols(
    select(ct, -any_of(c("PPPMER1", "FAPPPMER", "PPPMER2", "PPPMER2b", "PPPMER3", "PPPMER4"))),
    
    select(ct, EMP_ADM_ENT_M, PROFMER, pondcal) |> 
      mutate(SEXE = factor("2-Femme")) |>
      pcspp::pcspp(PROFMER, admin_ent = EMP_ADM_ENT_M, sexe = SEXE) |>
      select(all_of(c("PPPMER1"  = "PPP1",
                      "FAPPPMER" = "FAPPP",
                      "PPPMER2"  = "PPP2",  
                      "PPPMER2b" = "PPP2b",  
                      "PPPMER3"  = "PPP3", 
                      "PPPMER4"  = "PPP4"))) |>
      mutate(PPPMER1 = fct_relabel(PPPMER1, ~ case_when(
        str_detect(., "^1")    ~ "1-Mère patronne",
        str_detect(., "^2")    ~ "2-Mère cadre",
        str_detect(., "^3")    ~ "3-Mère CBI" , 
        str_detect(., "^4")    ~ "4-Mère profession organisée",
        str_detect(., "^5")    ~ "5-Mère employée",
        str_detect(., "^6")    ~ "6-Mère ouvrière",
        str_detect(., "^7")    ~ "6-Mère ouvrière",
      )))
  ) 
# tab(ct, PPPMER1, ANNEE)


ct <- ct |>
  mutate(
    across(
      any_of(c("PPPMER1", "PPPMER2", "PPPMER2b", "PPPMER3", "PPPMER4")),
      ~   as.factor(if_else(
        ACTIM %in% c("5-Mère jamais travaillée"), 
        true  = "Z-Mère inactive", 
        false = as.character(.)))
      
    ), 
    
    across(
      any_of(c("PPPPER1", "PPPPER2", "PPPPER2b", "PPPPER3", "PPPPER4")),
      ~   as.factor(if_else(
        ACTIP %in% c("5-Père jamais travaillé"), 
        true  = "Z-Père inactif", 
        false = as.character(.)))
    )
  )

ct <- ct |> 
  mutate(
    PPPMER1 = case_when(
      !is.na(PPPMER1)                            ~ PPPMER1,
      is.na(PPPMER1) & str_detect(csermer, "^5") ~ factor("5-Mère employée"),
      is.na(PPPMER1) & str_detect(csermer, "^6") ~ factor("6-Mère ouvrière"),
    ), 
    
    PPPPER1 = case_when(
      !is.na(PPPPER1)                            ~ PPPPER1,
      is.na(PPPPER1) & str_detect(csermer, "^5") ~ factor("5-Père employé"),
      is.na(PPPPER1) & str_detect(csermer, "^6") ~ factor("6-Père ouvrier"),
    ), 
  )
# tab(ct, PPPPER1, ANNEE, pct = "col", wt = pondcal) # => 20% de NA à la profession du père.
# tab(ct, PPPMER1, ANNEE, pct = "col", wt = pondcal) # => 16% de NA à la profession de la mère





ct <- ct |> mutate(cserper3 = fct_recode(
  cserper,
  "1-Père indépendant"                 = "1-Agriculteurs exploitants"                         ,
  "1-Père indépendant"                 = "2-Artisans, commerçants et chefs d'entreprise"      ,
  "2-Père classes moyennes salariées"   = "3-Cadres et professions intellectuelles supérieures",
  "2-Père classes moyennes salariées"   = "4-Professions intermédiaires"                       ,
  "3-Père employé" = "5-Employés"                                         ,
  "4-Père ouvrier" = "6-Ouvriers"                                         ,
  "NULL"                           = "Z"                                                  
), 
cserper3 = as.factor(if_else(EMP_ADM_ENT_P == "4-Père sans travail ",  
                             true   = "5-Père sans travail",
                             false  = as.character(cserper3 )))

)


ct <- ct |> mutate(csermer3 = fct_recode(
  csermer,
  "1-Mère indépendante"                  = "1-Agriculteurs exploitants"                         ,
  "1-Mère indépendante"                  = "2-Artisans, commerçants et chefs d'entreprise"      ,
  "2-Mère classes moyennes salariées"    = "3-Cadres et professions intellectuelles supérieures",
  "2-Mère classes moyennes salariées"    = "4-Professions intermédiaires"                       ,
  "3-Mère employée ou ouvrière"          = "5-Employés"                                         ,
  "3-Mère employée ou ouvrière"          = "6-Ouvriers"     ,        # #"4-Mère ouvrière"
  "NULL"                                 = "8"                                                  
), 
csermer3 = as.factor(if_else(EMP_ADM_ENT_M == "4-Mère sans travail",  
                             true   = "5-Mère sans travail",
                             false  = as.character(csermer3 )))
) 
# tab(ct, csermer3, ANNEE)


ct$EMP_ADM_PM <- as.factor(case_when(
  ct$EMP_ADM_ENT_P == "1-Père administration publique" | 
    ct$EMP_ADM_ENT_M == "1-Mère administration publique"   ~ "1-Un parent admin publique",
  TRUE                                                   ~ "2-Aucun parent admin"
))  




ct <- ct |> mutate(EMP6 = as.factor(case_when(
  EMP4 == "11-État" & PPP2 == "4I1-Enseignant·es" ~ "11-État: enseignant·es", 
  EMP4 == "11-État"                               ~ "12-État hors enseignant·es", 
  EMP4 == "12-Grandes entreprises de services"    ~ "13-Grandes entreprises de services", 
  TRUE                                            ~ as.character(EMP4)
)))









## DPO-------------------------------------------------------------------------------------

#DPO, définition au niveau de l'individus (objectifs reçus du chef + autonomie proc)
ct <- ct %>% 
  mutate(DPO = case_when(
    OBJMODIF %in% c("2-Obj modif chef", "3-Obj modif collectif", "4-Obj pas modif") & 
      ct$COMMENT == "1-Autonomie procédurale"                    ~ "1-DPO",
    
    #OBJVRAI == "1-Objectifs chiffrés" & COMMENT == "1-Autonomie procédurale" ~ "1-DPO",
    TRUE ~ "2-Pas DPO"
  ) |> as.factor()
  )
# tab(ct, DPO, wt = pondcal, pct = "col")

ct <- ct %>% 
  mutate(DPO19 = case_when(
    OBJECTIF == "1-Objectifs chiffrés" & COMMENT == "1-Autonomie procédurale" ~ "1-DPO",
    TRUE ~ "2-Pas DPO"
  ) |> as.factor()
  )
# tab(ct, ANNEE, DPO2, wt = pondcal, pct = "row")


ct$OBJtous13 <- 
  case_when(
    ct$OBJMODIF %in% c("2-Obj modif chef", "4-Obj pas modif") & 
      ct$COMMENT == "1-Autonomie procédurale"                    ~ "1-DPO",
    ct$OBJMODIF %in% c("2-Obj modif chef", "4-Obj pas modif")    ~ "2-Obj sans autonomie",
    
    ct$OBJMODIF == "3-Obj modif collectif" &  
      ct$COMMENT == "1-Autonomie procédurale"                    ~ "3-'DPO' collective",
    
    ct$OBJMODIF == "1-Obj modif seul"                            ~  "4-Chiffres fixés à soi-même",
    
    TRUE  ~ "5-Pas objectifs chiffrés"
  ) |> as.factor()

ct$OBJtous13_hors_coll <- 
  case_when(
    ct$OBJMODIF %in% c("2-Obj modif chef", "4-Obj pas modif") & 
      ct$COMMENT == "1-Autonomie procédurale"                    ~ "1-Direction par objectifs",
    
    ct$OBJMODIF %in% c("2-Obj modif chef", "4-Obj pas modif") ~ "2-Objectifs sans autonomie",
    
    #ct$OBJMODIF == "1-Obj modif seul"                            ~  "4-Chiffres fixés à soi-même",
    
    TRUE  ~ "4-Pas objectifs chiffrés"
  ) |> as.factor()

ct$OBJtous13_2 <- 
  case_when(
    ct$OBJMODIF %in% c("2-Obj modif chef", "3-Obj modif collectif", "4-Obj pas modif") & 
      ct$COMMENT == "1-Autonomie procédurale"                    ~ "1-Direction par objectifs",
    
    ct$OBJMODIF %in% c("2-Obj modif chef", "3-Obj modif collectif", "4-Obj pas modif") ~ "2-Objectifs sans autonomie",
    
    #ct$OBJMODIF == "1-Obj modif seul"                            ~  "4-Chiffres fixés à soi-même",
    
    TRUE  ~ "4-Pas objectifs chiffrés"
  ) |> as.factor()


ct$OBJtous <- 
  case_when(
    ct$OBJECTIF == "1-Objectifs chiffrés" & 
      ct$COMMENT == "1-Autonomie procédurale"            ~ "1-Direction par objectifs",
    ct$OBJECTIF == "1-Objectifs chiffrés"                ~ "2-Objectifs sans autonomie" ,
    TRUE                                                 ~ "3-Pas objectifs chiffrés"
  ) |> as.factor()
# c("OBJtous", "OBJtous19") |> map(~ tab(ct, !!sym(.), ANNEE,  pct = "col", wt = pondqaa))
# tab_many(ct[salariat, ], c(OBJtous, OBJtous19), PPP1, ANNEE, pct = "col", wt = pondqaa, color = "diff")

ct$DPOmodif <- 
  as.factor(case_when(
    
    ct$OBJMODIF == "2-Obj modif chef" & ct$COMMENT == "1-Autonomie procédurale"       ~ "1-DPO négociable",
    
    ct$OBJMODIF == "3-Obj modif collectif" & ct$COMMENT == "1-Autonomie procédurale"  ~ "2-'DPO' collective négociable",
    
    ct$OBJMODIF == "4-Obj pas modif" & ct$COMMENT == "1-Autonomie procédurale"        ~ "3-DPO non négociable",
    
    
    ct$OBJMODIF %in% c("2-Obj modif chef", "3-Obj modif collectif", "4-Obj pas modif")~ "4-Objectifs sans autonomie",
    
    
    #ct$OBJMODIF == "1-Obj modif seul"                            ~  "4-Chiffres fixés à soi-même",
    
    TRUE  ~ "5-Pas objectifs chiffrés"
  ))

ct$DPOmodif_hors_coll <- 
  as.factor(case_when(
    ct$OBJMODIF == "2-Obj modif chef" & ct$COMMENT == "1-Autonomie procédurale" ~ "1-DPO négociable",
    
    ct$OBJMODIF == "4-Obj pas modif" & ct$COMMENT == "1-Autonomie procédurale"  ~ "2-DPO non négociable",
    
    
    ct$OBJMODIF %in% c("2-Obj modif chef", "4-Obj pas modif")                   ~ "3-Objectifs sans autonomie",
    
    #ct$OBJMODIF == "1-Obj modif seul"                            ~  "4-Chiffres fixés à soi-même",
    
    TRUE  ~ "5-Pas objectifs chiffrés"
  ))


ct$DPOmodif2 <- 
  as.factor(case_when(
    ct$OBJMODIF %in% c("1-Obj modif seul" , "2-Obj modif chef", "3-Obj modif collectif") & 
      ct$COMMENT == "1-Autonomie procédurale"                                          ~ "1-DPO négociable",
    
    ct$OBJMODIF == "4-Obj pas modif" & ct$COMMENT == "1-Autonomie procédurale"       ~ "2-DPO non négociable",
    
    
    ct$OBJMODIF %in% c("1-Obj modif seul", "2-Obj modif chef", "3-Obj modif collectif", "4-Obj pas modif") ~ "3-Objectifs sans autonomie",
    
    #ct$OBJMODIF == "1-Obj modif seul"                            ~  "4-Chiffres fixés à soi-même",
    
    TRUE  ~ "5-Pas objectifs chiffrés"
  ))


ct <- ct |> mutate(DPOEVAL = fct_cross(DPO19, EVACRIT) |> 
                     fct_recode(
                       "1-DPO avec entretien" = "1-DPO:1-Entretien d'évaluation"    ,
                       "3-Pas DPO"            = "2-Pas DPO:1-Entretien d'évaluation",
                       "2-DPO sans entretien" = "1-DPO:2-Pas entretien éval"        ,
                       "3-Pas DPO"            = "2-Pas DPO:2-Pas entretien éval"    
                     )
) 


ct$OBJMODIF3 <- fct_recode(
  ct$OBJMODIF,
  "1-Objectifs négociables" = "1-Obj modif seul"     ,
  "1-Objectifs négociables" = "2-Obj modif chef"     ,
  "1-Objectifs négociables" = "3-Obj modif collectif",
  "2-Obj non négociables"   = "4-Obj pas modif"      
)

ct$OBJMODIF3 <- as.factor(if_else(
  ct$ANNEE %in% c("2013", "2016") & is.na(ct$OBJMODIF), 
  true  = "3-Pas obj chiffrés", 
  false = as.character(ct$OBJMODIF3)
))






## Salaires ---------------------------------------------------------------------
#Salaires corrigés de l'inflation, en euros 2019 
ct <- ct %>% 
  mutate(rev19 = case_when(
    ANNEE == "2013" ~ revmensc / 95.72 * 100,
    ANNEE == "2016" ~ revmensc / 96.60 * 100,
    ANNEE == "2019" ~ revmensc               ))

# Terciles de salaire par CSP recodée

# # Salaire par années (utiliser par années)
# ct <- ct %>%
#   group_by(ANNEE, PPP1) %>%
#   mutate(revt = as.factor(dineq::ntiles.wtd(revmensc, 3, weights = pondcal))) %>%
#   ungroup() %>%
#   mutate(
#     revt = fct_cross(revt, fct_relabel(PPP1, ppp1_recode_short)) %>%
#       fct_relabel(~ str_replace(., "(^.):(.)-(.+$)", "\\2\\1-\\3 T\\1") %>%
#                     str_replace("^(.)1", "\\14") %>%
#                     str_replace("^(.)3", "\\11") %>%
#                     str_replace("^(.)4", "\\13"))
#   ) %>%
#   group_by(ANNEE, revt) %>%
#   mutate(revt_range = as.factor(case_when(
#     str_detect(revt, "^.1") ~ str_c(" ", round(min(revmensc, na.rm = TRUE)), euros, "+"),
#     str_detect(revt, "^.2") ~ str_c(" ", round(min(revmensc, na.rm = TRUE)), "_",
#                                        round(max(revmensc, na.rm = TRUE)), euros),
#     str_detect(revt, "^.3") ~ str_c(" ", round(max(revmensc, na.rm = TRUE)), euros, "-"),
#   ))) %>%
#   ungroup() %>%
#   mutate(revt = fct_cross(revt, revt_range, sep = "") %>% fct_relevel(sort) ) %>%
#   select(-any_of(c("revt_range")))
# # tab(ct, revt)


# Salaire par secteur
ct <- ct %>%
  group_by(ANNEE, PPP1, naf17) %>%
  mutate(revt = as.factor(dineq::ntiles.wtd(revmensc, 3, weights = pondcal))) %>%
  ungroup() %>%
  mutate(
    revt = fct_cross(revt, PPP1) %>%
      fct_relabel(~ str_replace(., "(^.):(.)-(.+$)", "\\2\\1-\\3 T\\1") %>%
                    str_replace("^(.)1", "\\14") %>%
                    str_replace("^(.)3", "\\11") %>%
                    str_replace("^(.)4", "\\13"))
  ) %>%
  # group_by(ANNEE, revt) %>%
  # mutate(revt_range = as.factor(case_when(
  #   str_detect(revt, "^.1") ~ str_c(" ", round(min(revmensc, na.rm = TRUE)), euros, "+"),
  #   str_detect(revt, "^.2") ~ str_c(" ", round(min(revmensc, na.rm = TRUE)), "_",
  #                                   round(max(revmensc, na.rm = TRUE)), euros),
  #   str_detect(revt, "^.3") ~ str_c(" ", round(max(revmensc, na.rm = TRUE)), euros, "-"),
  # ))) %>%
  ungroup() %>% 
  mutate(revt =  fct_relevel(revt, sort))
# tab(ct, revt)

#Par secteur plus fin pour les administrations publiques ?
# REVcadresEGESNAF = case_when(is.na(REVcadresEGES)         ~ factor(NA_character_, append(levels(ACTIVFIN), levels(NAF17))),
#                              str_detect(ACTIVFIN, "^8")   ~ ACTIVFIN,
#                              TRUE                         ~ NAF17
# )













## Employeur CJ CASD -------------------
#   CJ : catégories juridiques SIRENE
CT2016_EMPcj      <- readr::read_csv("~\\Data\\Conditions de travail\\Variables CASD\\CT2016_EMPcj.csv"     , col_types = "c") |> deframe()
CT2019_EMPcj      <- readr::read_csv("~\\Data\\Conditions de travail\\Variables CASD\\CT2019_EMPcj.csv"     , col_types = "c") |> deframe()

ct$EMPcj <- NA_character_
ct$EMPcj[ct$ANNEE == "2016"] <- CT2016_EMPcj
ct$EMPcj[ct$ANNEE == "2019"] <- CT2019_EMPcj
ct$EMPcj <- as.factor(ct$EMPcj) |> fct_recode("NULL" = "NA")
# tab(ct, EMPcj, EMP, ANNEE, cleannames = TRUE)

CT2016_EMPcj500   <- readr::read_csv("~\\Data\\Conditions de travail\\Variables CASD\\CT2016_EMPcj500.csv"  , col_types = "c") |> deframe()
CT2019_EMPcj500   <- readr::read_csv("~\\Data\\Conditions de travail\\Variables CASD\\CT2019_EMPcj500.csv"  , col_types = "c") |> deframe()
ct$EMPcj500 <- NA_character_
ct$EMPcj500[ct$ANNEE == "2016"] <- CT2016_EMPcj500
ct$EMPcj500[ct$ANNEE == "2019"] <- CT2019_EMPcj500
ct$EMPcj500 <- as.factor(ct$EMPcj500) |> fct_recode("NULL" = "NA")
# tab(ct, EMPcj500, EMP, ANNEE, cleannames = TRUE)


CT2016_EMPcj500_2 <- readr::read_csv("~\\Data\\Conditions de travail\\Variables CASD\\CT2016_EMPcj500_2.csv", col_types = "c") |> deframe()
CT2019_EMPcj500_2 <- readr::read_csv("~\\Data\\Conditions de travail\\Variables CASD\\CT2019_EMPcj500_2.csv", col_types = "c") |> deframe()
ct$EMPcj500_2 <- NA_character_
ct$EMPcj500_2[ct$ANNEE == "2016"] <- CT2016_EMPcj500_2
ct$EMPcj500_2[ct$ANNEE == "2019"] <- CT2019_EMPcj500_2
ct$EMPcj500_2 <- as.factor(ct$EMPcj500_2) |> fct_recode("NULL" = "NA")
# tab(ct, EMPcj500_2, EMP, ANNEE, cleannames = TRUE)

ct <- ct |> mutate(EMPcj2 = fct_recode(
                       EMPcj500,
                       "1-Collectivités locales" = "1-Collectivités locales",
                       "2-État"                  = "2-État"                 ,
                       "3-Hôpitaux publics"      = "3-Hôpitaux publics"     ,
                       "4-Sécurité sociale"      = "4-Sécurité sociale"     ,
                       "5-Entreprises publiques" = "5-Entreprises publiques",
                       "6-Entreprises privées"   = "6-PME"                  ,
                       "6-Entreprises privées"   = "7-Grandes entreprises"  ,
                       "H-Particuliers"          = "H-Particuliers"         ,
                       "I-Pas d'employeur"       = "I-Pas d'employeur"      
                     )
)


# # CODE CASD (appariemment SIASP/FARE/LIFI/SIRENE sur le champ base couplée 2016-2019)
# ct <- ct %>%
#   mutate(EMPcj2 = as.factor(case_when(
#     ANNEE %in% c("2005", "2013") ~ NA_character_,
#     
#     #Logique :
#     # Avec case_when on code en premier les informations les plus certaines,
#     # qui ensuite ne bougent plus. Donc il faut procéder dans l'ordre.
#     
#     #1) On isole : indépendants,salariés de la Sécurité, entreprises publiques
#     
#     #Pas d'employeur pour les personnes codées Indépendantes:
#     str_detect(STATUT, "^10|^9|^8")
#     | str_detect(PE, regex("^312c|^312d|^312e", ignore_case = TRUE)) #Consultants libéraux
#     | str_detect(cser, "^1|^2") #"1-Agriculteurs exploitants", "2-Artisans, commerçants et chefs d'entreprises"
#     ~ "I-Pas d'employeur",
#     
#     # Sécurité sociale (PCS + CJ + NAF)
#     str_detect(fct_explicit_na(
#       PE), "^376F|^467D|^545D") |
#       str_detect(fct_explicit_na(CJ_CIRC), "^8110|^8120") |
#       str_detect(fct_explicit_na(NAF), "^8430") |
#       str_detect(fct_explicit_na(NAF_05), "^753")
#     ~ "4-Sécurité sociale",
#     # # "8430A-Activités générales de sécurité sociale", #753A NAF1993 NAF2003
#     # # "8430B-Gestion des retraites complémentaires",   #753B
#     # # "8430C-Distribution sociale de revenus")         #753C
#     
#     # #Entreprises publiques
#     (!is.na(LIFI_PUB) & LIFI_PUB == "1-Groupe public") | 
#       (!is.na(CJ) & str_detect(CJ, "^4")) | 
#       str_detect(PE, "^333c|^451a|^521a") #Salariés de La Poste
#     ~ "5-Entreprises publiques", 
#     
#     EMP_NOM == "ORANGE" ~ "6-Entreprises privées", #Sinon certains se retrouvent dans État
#     
#     
#     #2) Reclassements au sein du secteur public (État, CL, hôpitaux, entreprises publiques)
#     # à partir de la catégorie juridique SIRENE de l'établissement
#     # Certaines CJ désignent l'État ou les CL de manière sûre
#     # (mais sont restrictives car le secteur public ne s'y limite pas,
#     #  il agrège des entités de statuts divers contrôlés par un ministère ou une CL)
#     
#     #Hôpitaux publics (3 catégories juridiques spécifiques, codage idem PUB3FP)
#     str_detect(CJ, "^7364|^7365|^7366")
#     # => CJ %in% c("7364-Établissement d'hospitalisation",
#     # "7365-Syndicat inter hospitalier", 
#     # "7366-Établissement public local social et médico-social")
#     | (is.na(CJ) & str_detect(NAF, "^8402|^8404")
#        & str_detect(STATUT, "^1|^2|^3") ) #Etat CL HP
#     #| (is.na(CJ) & (sect_FP == "FPH" | Sect_SH == "FPH") )
#     ~ "3-Hôpitaux publics",
#     
#     # #Secteur social et médico-social (surtout EPHAD) dans collectivités locales ?
#     # str_detect(CJ, "^7366")
#     # # "7366-Établissement public local social et médico-social"
#     # | (is.na(CJ) & str_detect(STATUT, "^5") ) #Etat CL HP
#     # #| (is.na(CJ) & (sect_FP == "FPH" | Sect_SH == "FPH") )
#     # ~ "1-Collectivités locales"
#     
#     
#     TITPUBR == "8-Enseignant de l'enseignement privé sous contrat" ~ "6-Entreprises privées",
#     
#     #Certaines conseillères Pôle emploi classées dans entreprises
#     (PPP1 == "3-CBI" | str_detect(cser, "^4")) & str_detect(PROFESS, "EMPLOI") & str_detect(PROFESS, "CONSEIL") 
#     ~ "2-État",
#     
#     #On commence par verrouiller certaines professions de l'État
#     # que les CJ d'établissement classeraient faussement CL
#     #Les professeurs des écoles mat, collèges, lycées, cert., sup., CPE, surveillants, proviseurs
#     (str_detect(PE, "^421|^422|^341|^342")
#      & ! ( str_detect(STATUT, "^4|^6")  )  ) #hors privé
#     | str_detect(PE, regex("^3317|^4514|^333e|^451e", ignore_case = TRUE)) #Corps catégories A et B de l'État
#     ~ "2-État",
#     
#     # Le reste de la population des écoles, collèges, lycées, 
#     # est gérée par les CL depuis 1985 (ou par les écoles privées) :
#     str_detect(NAF, "^8510|^8520|^8531|^8532")
#     #profs etc., et corps de l'État., viennent d'être classés État
#     & !str_detect(STATUT, "^4|^6") #hors privé
#     # "801Z-Enseignement primaire"
#     # "802A-Enseignement secondaire général"
#     # "802C-Enseignement secondaire technique ou professionnel"
#     # après 2008 : "8510Z-Enseignement pré-primaire"
#     # "8520Z-Enseignement primaire"
#     # "8531Z-Enseignement secondaire général"
#     # "8532Z-Enseignement secondaire technique ou professionnel"
#     ~ "1-Collectivités locales",
#     
#     str_detect(NAF, "^8510|^8520|^8531|^8532")
#     ~ "6-Entreprises privées",
#     
#     
#     # Au sein de la CJ1 "7-Personne morale et organisme soumis au droit administratif" :
#     #État
#     str_detect(CJ, "^71") # => CJ2 == "71-Administration de l'État"
#     | str_detect(CJ, "^738") # => CJ2 == "72-Etablissement public administratif", national
#     #| (is.na(CJ) & sect_FP == "FPE")
#     ~ "2-État",
#     
#     
#     #Collectivités locales
#     str_detect(CJ, "^72|^73")
#     # => CJ2 == "72-Collectivité territoriale"
#     # => CJ2 == "73-Etablissement public administratif"
#     # sauf établissement nationaux et hospitaliers/profs, déjà codés plus haut État/HP
#     ~ "1-Collectivités locales",
#     
#     
#     
#     #3) Quelques professions, en petit nombre, donnent l'employeur de manière plausible :
#     # l'INSEE a déjà recoupé beaucoup d'infos différentes pour réaliser le classement.
#     # Surtout  celles où le nom de l'entreprise/l'employeur/la catégorie de la FP
#     # apparaissent explicitement (et sont des conditions lors du codage initial)
#     # Pour l'État j'exclue néanmoins ceux qui répondent « entreprise privée »
#     # et dont la catégorie juridique correspond effectivement à une société commerciale
#     # (Ne s'applique qu'à ce qui n'a pas été codé plus haut : public sans qu'on puisse
#     #  dire si c'est État, CL ou EP ; déclaré public mais pas de catégorie juridique
#     #  SIRENE pour l'assurer ; déclaré secteur privé)
#     
#     
#     #État
#     !str_detect(CJ, "^5|^6")
#     # => Tout sauf !(CJ1 %in% c("5-Société commerciale",
#     #            "6-Autre personne morale immatriculée au RCS"))
#     & str_detect(PE, regex("^332a|^333a|^333b|^333e|^334a|^451c|^452a|^452b|^451e|^531a|^532a|^532b|^532c|^522a", ignore_case = TRUE))
#     # =>  PE %in% c("332a-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés",
#     # "333a-Magistrats",
#     # "333b-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes",
#     # "333e-Autres personnels administratifs de catégorie A de l'Etat (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
#     # "334a-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)",
#     # "451c-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés",
#     # "452b-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie",
#     # "451e-Autres personnels administratifs de catégorie B de État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)",
#     # "531a-Agents de police de État",
#     # "532a-Gendarmes (de grade inférieur à adjudant)",
#     # "532b-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)",
#     # "532c-Hommes du rang (sauf pompiers militaires)",
#     # "522a-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes",
#     # "452a-Inspecteurs et officiers de police")
#     ~ "2-État",
#     
#     
#     # CL (agents des hôpitaux publics déjà classés plus haut au travers de la CJ SIRENE)
#     str_detect(PE, regex("^531b", ignore_case = TRUE))
#     # => PE %in% c("531b-Agents des polices municipales")
#     ~ "1-Collectivités locales",
#     
#     # Pas possible ici car aussi basé sur déclarations "^332b|^333f|^451f|"
#     # "332b-Ingénieurs des collectivités locales et des hôpitaux",
#     # "333f-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)",
#     # "451f-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)",
#     
#     
#     
#     
#     #4) Enfin, on classe les personnes qui restent (NA à CJ, et donc le plus
#     #souvent SIRET manquant) selon l'employeur qu'elles déclarent (STATUT). Sauf
#     #si la catégorie juridique est 5 Société anonymique (vérif nom des employeurs).
#     
#     #État
#     str_detect(STATUT, "^1") & 
#       (is.na(SIRET) | 
#          (!is.na(SIRET) & !str_detect(CJ, "^5")) |
#          (!is.na(SIRET) & str_detect(CJ, "^5") & str_detect(EMP_NOM, "^MINISTERE")) )
#     ~ "2-État",
#     
#     #Collectivités locales (hors secteur social et médico-social |^5)
#     str_detect(STATUT, "^2")              ~ "1-Collectivités locales",
#     
#     #Hôpitaux publics
#     str_detect(STATUT, "^3|^5") &       
#       (is.na(SIRET) | (!is.na(SIRET) & !str_detect(CJ, "^5")) )
#     ~ "3-Hôpitaux publics",
#     
#     #str_detect(STATUT, "^5")              ~ "3-Médicosoc_decl",
#     
#     #Salariées des particuliers
#     str_detect(ACTIVFIN, "^97") | #"97-Activités des ménages en tant qu'employeurs de personnel domestique" 
#       str_detect(STATUT, "^7") # "7-Salarié d’un ou plusieurs particuliers"
#     ~ "H-Particuliers",
#     
#     TRUE ~ "6-Entreprises privées"
#   )), 
#   
#   
#   EMPcj = as.factor(case_when(
#     EMPcj2 == "6-Entreprises privées"
#     & (cat_groupe == "GE" | str_detect(UL_EF_SIRC, "^52|^53")) # Société juridique + de 5000 salariés
#     ~ "8-Grandes entreprises",
#     
#     EMPcj2 == "6-Entreprises privées"
#     &  (cat_groupe == "ETI" | str_detect(UL_EF_SIRC, "^51|^42|^41|^32")) # Société jur de 250 à 4999 salariés
#     ~ "7-Entreprises intermédiaires",
#     
#     EMPcj2 == "6-Entreprises privées"
#     & (cat_groupe %in% c("PME", "MICRO")
#        | (!is.na(UL_EF_SIRC) ) # y compris 0 salariés
#        | (is.na(UL_EF_SIRC) & str_detect(NBSALENTC, "^1")) #déclaration 1 à 50 Ent
#     )
#     ~ "6-PME",
#     
#     EMPcj2 == "6-Entreprises privées"
#     &  (str_detect(NBSALENTC, "^3") | str_detect(NBSALA, "^7|^8")) # ent ou étab > 500
#     ~ "9-Ent inconnu >500",
#     
#     EMPcj2 == "6-Entreprises privées" ~ NA_character_,
#     
#     TRUE ~ as.character(EMPcj2)
#   )), 
#   
#   
#   EMPcj500 = as.factor(case_when(
#     EMPcj2 == "6-Entreprises privées" & 
#       (cat_groupe %in% c("GE") |  #"ETI"       # Groupe >= 5000 salariés
#          str_detect(UL_EF_SIRC, "^5|^4") |     # Unité légale >= 500
#          str_detect(NBSALENTC, "^3") |         # Décl : Étab >= 500 
#          str_detect(NBSALA, "^7|^8"))          # Décl : unité légale >= 500
#     ~ "7-Grandes entreprises",
#     
#     EMPcj2 == "6-Entreprises privées" ~ "6-PME",
#     
#     TRUE ~ as.character(EMPcj2)
#   )),
  


ct <- ct %>%
 mutate(
  EMPcj3 = fct_recode(
    EMPcj500,
    "1-Collectivités locales" = "1-Collectivités locales",
    "2-État"                  = "2-État"                 ,
    "3-Hôpitaux publics"      = "3-Hôpitaux publics"     ,
    "6-Grandes entreprises"   = "4-Sécurité sociale"     ,
    "6-Grandes entreprises"   = "5-Entreprises publiques",
    "5-PME"                   = "6-PME"                  ,
    "6-Grandes entreprises"   = "7-Grandes entreprises"  ,
    "H-Particuliers"          = "H-Particuliers"         ,
    "I-Pas d'employeur"       = "I-Pas d'employeur"
  ) |> fct_relevel(sort),
  
  EMP_ADM_ENTcj = tabxplor:::fct_case_when_recode(EMPcj2, c(
    "1-Administrations publiques" = "^1|^2|^3",
    "2-Entreprises" = "^4|^5|^6",
    NULL = "^H|^I") )
  
  )

ct <- ct |>  
  mutate(
    FONCTIONcadres = fct_relabel(FONCTION, ~ case_when(
      str_detect(str_replace(., "^ ", "0"), "^01") ~ "4-Autres"    , # Production
      str_detect(str_replace(., "^ ", "0"), "^02") ~ "4-Autres"    , # Maintenance/ins
      str_detect(str_replace(., "^ ", "0"), "^03") ~ "4-Autres"    , # Nettoyage/gard
      str_detect(str_replace(., "^ ", "0"), "^04") ~ "4-Autres"    , # Manutention
      str_detect(str_replace(., "^ ", "0"), "^05") ~ "4-Autres"    , # Secrétariat/acc
      str_detect(str_replace(., "^ ", "0"), "^06") ~ "1-Gestion"   , # Gestion/cpt
      str_detect(str_replace(., "^ ", "0"), "^07") ~ "3-Commercial", # Commercial
      str_detect(str_replace(., "^ ", "0"), "^08") ~ "2-Études"    , # Recherche/mth
      str_detect(str_replace(., "^ ", "0"), "^09") ~ "4-Autres"    , # Enseignement
      str_detect(str_replace(., "^ ", "0"), "^10") ~ "4-Autres"    , # Soin
      str_detect(str_replace(., "^ ", "0"), "^11") ~ "4-Autres"    , # Autres
    )) |> 
      fct_explicit_na("4-Autres") |> fct_relevel(sort), 
    
    FONCTIONcadres = case_when(
      EMP_ADM_ENTcj == "1-Administrations publiques" & FONCTIONcadres == "3-Commercial"
      ~ factor("4-Autres", levels(FONCTIONcadres)),
      
      TRUE ~ FONCTIONcadres
    )
  )
# tab(ct, FONCTIONcadres, ANNEE)

# Pour l'instant, avec pas de déclaration SIRET employés en 2019, 
#   part codée à partir des informations employeurs / déclarations salarié·es :
#     CL : 4366 emp + 2090 decl (130 SIRET, surtout "9220-Association déclarée")
#     État : 4772 emp + 2000 decl (118 CJ, ORANGE + assos publiques + SA privées)
#     HP : 3778 emp + 1315 decl (126 CJ, assos)
#                   + 190 decl médico-social (60 CJ, associations)
#   EMPcj500 par rapport à EMP (déclaration des salariées) : 
#     CL : +500 pris à État (personnel écoles collèges lycées, deps, pompiers) 
#         + 75 pris à HP (centres communaux action sociale, deps)
#     État : +150 pris Ent (Pôle emploi, IN, chambre commerce, Poste), 
#            +50 CL (Pôle emploi, univ, ARS, mins) ; 
#            -500 donné CL, -500 HP, -70 Sécu, -100 EP, -86 Ent
#     HP : +450 pris à État (hôpitaux, APHP, EHPAD),
#          +50 Ent, + 50 CL (hôpitaux, EHPAD) ;
#          -75 donné CL, -50 Ent

# tab(ct[ct$ANNEE %in% c("2016", "2019"), ], EMPcj, wt = pondcal, pct = "col")
# tab(ct[ct$ANNEE %in% c("2016", "2019"), ], EMPcj2)
# tab(ct[ct$ANNEE %in% c("2016", "2019"), ], EMPcj500)
# 
# 
# 
# ct[ct$ANNEE %in% c("2016", "2019"), ] |> 
#   tab(EMPcj500, EMP, cleannames = TRUE) |> tab_kable()
# 
# ct[ct$ANNEE %in% c("2016", "2019"), ] |>
#   #filter(EMPcj500 == "2-État" & !EMP== "2-État") |>
#   #filter(EMPcj500 == "1-Collectivités locales" & !EMP == "1-Collectivités locales") |>
#   filter(EMPcj500 ==  "3-Hôpitaux publics" & !EMP ==  "3-Hôpitaux publics") |>
#    select(EMP, EMP_NOM, STATUT, PE) |>
#   arrange(EMP, EMP_NOM) |> new_tab() |> group_by(EMP) |> print(n = 900)
#
# ct[ct$ANNEE %in% c("2016", "2019") & ct$EMPcj == "decl", ] |>
#   select(SIREN, EMP_NOM, CJ) |> 
#   arrange(CJ, EMP_NOM) |> new_tab() |> group_by(EMP_NOM) |> print(n = 600 )
# 
# 
# ct[ct$ANNEE %in% c("2016", "2019"), ] |>
#   filter(!is.na(EMP_NOM) & EMP_NOM == "ORANGE") |> 
#   mutate(SIREN = as.character(SIREN)) |>
#   select(SIREN, cat_groupe, UL_EF_SIRC, EMPce, CJ) |> 
#   arrange(SIREN) |> new_tab() |> group_by(SIREN) |> print(n = 600 )



### PcsPP employeur CJ ----
#Différences infimes par rapport à EMP déclaré 
#   (mais dans les tableaux évite les lignes à 2 individus)
ct <-  
  bind_cols(
    select(ct, -any_of(c("PPP1cj", "FAPPPcj", "PPP2cj", "PPP2bcj", "PPP3cj", "PPP4cj"))), 
    
    ct |>
      select(ANNEE, PE, EMP_ADM_ENTcj) |> 
      # PCS_create("PE",  EMP_ADM_ENT = "EMP_ADM_ENTcj", 
      #            res = c("PPP1", "FAPPP", "PPP2", "PPP3", "PPP4")) |> 
      pcspp::pcspp("PE", admin_ent = "EMP_ADM_ENTcj") |> 
      mutate(across(any_of(c("PPP1", "FAPPP", "PPP2", "PPP2b", "PPP3", "PPP4")), 
                    ~ if_else(ANNEE %in% c("2016", "2019"), ., factor(NA_character_)))) |>
      select(-ANNEE, -PE, -EMP_ADM_ENTcj, "PPP1cj" = "PPP1", "FAPPPcj" = "FAPPP", 
             "PPP2cj" = "PPP2", "PPP2bcj" = "PPP2b", 
             "PPP3cj" =  "PPP3", "PPP4cj" = "PPP4") |> 
      mutate(PPP1cj = fct_relabel(PPP1cj, ~ case_when(
        str_detect(., "^1")    ~ "1-Chefs d’entreprise",
        str_detect(., "^2")    ~ "2-Cadres",
        str_detect(., "^3")    ~ "3-Cols blancs subalternes" , 
        str_detect(., "^4")    ~ "4-Professions organisées",
        str_detect(., "^5")    ~ "5-Employées",
        str_detect(., "^6")    ~ "6-Ouvriers",
        str_detect(., "^7")    ~ "6-Ouvriers",
      ))
      )
  )
#tab(ct[ct$ANNEE %in% c("2016", "2019"), ], PPP2cj, PPP2, cleannames = TRUE) |> tab_xl(colwidth = 8)



ct <- ct |> 
  mutate(PPP2cj_secu = as.factor(case_when(
    EMPcj == "4-Sécurité sociale" & PPP1cj == "2-Cadres"    ~ "200-Cadres Sécurité sociale",
    EMPcj == "4-Sécurité sociale" & PPP1cj == "3-Cols blancs subalternes" ~ "300-CBI Sécurité sociale",
    EMPcj == "4-Sécurité sociale" & PPP1cj == "5-Employées" ~ "500-Employées Sécurité sociale",
    TRUE                                                    ~ as.character(PPP2cj)
  )), 
  
  PPP2cj_secu = fct_drop(if_else(!(str_detect(PPP2cj_secu, "Ouvri") & !str_detect(PPP2cj_secu, "agri")), 
                                 true  = fct_expand(PPP2cj_secu, levels(PPP1cj)), 
                                 false = PPP1cj)), 
  
  
  FAPPP_secu = as.factor(if_else(PPP2cj_secu %in% c("200-Cadres Sécurité sociale",
                                                    "300-CBI Sécurité sociale",
                                                    "500-Employées Sécurité sociale"), 
                                 true  = "0-Filière Sécurité sociale", 
                                 false = as.character(FAPPPcj)))
  )
# tab(ct, PPP2cj_secu, FAPPP_secu)









### Croisements PcsPP / employeur ----


ct <- ct %>%
  mutate(secteurcj = as.factor(
    if_else(ANNEE == "2005", 
            
            #2005
            true = NA_character_
            #   case_when(
            #   # naf38 ==  "PZ - Enseignement" & 
            #   #   EMP %in% c("1-Collectivités locales","2-État", "3-Hôpitaux publics") ~ "31-Enseignement public", 
            #   # 
            #   # naf38 ==  "PZ - Enseignement"                                          ~ "32-Enseignement privé", 
            #   
            #   EMPcj3  == "1-Collectivités locales" ~ "10-Collectivités locales",
            #   EMPcj3  == "2-État"                  ~ "20-État",
            #   
            #   EMPcj3 == "3-Hôpitaux publics"       ~ "41-Hôpitaux publics",
            #   str_detect(NAF_05, "^851A")          ~ "42-Cliniques privées",
            #   
            #   
            #   str_detect(NAF36_05, "^A0")                        ~ "50-Agriculture",
            #   #A0 Agriculture, sylviculture et pêche 
            #   
            #   
            #   str_detect(NAF36_05, "^B0|^F4|^G1|^C3") |  str_detect(NAF_05, "^27")  ~ "60-Industries de process",
            #   #B0 Industries agricoles et alimentaires 
            #   #F4 Chimie, caoutchouc, plastiques 
            #   #G1 Production de combustibles et de carburants 
            #   #C3 Pharmacie, parfumerie et entretien 
            #   #+ métallurgie
            #   
            #   str_detect(NAF36_05, "^C4|^D0|^E1|^E2|^E3")  ~ "61-Industries d’équipement", 
            #   #C4 Industries des équipements du foyer 
            #   #D0 Industrie automobile 
            #   #E1 Construction navale; aéronautique et ferroviaire 
            #   #E2 Industries des équipements mécaniques 
            #   #E3 Industries des équipements électriques et électroniques 
            #   
            #   str_detect(NAF36_05, "^C1|^C2|^F1|^F2|^F3|^F5|^F6|^G2") ~ "62-Industrie autres",
            #   #C1 Habillement, cuir 
            #   #C2 Edition, imprimerie, reproduction 
            #   #F1 Industries des produits minéraux 
            #   #F2 Industrie textile 
            #   #F3 Industries du bois et du papier
            #   #F5 Métallurgie et transformation des métaux 
            #   #F6 Industries des composants électriques et électroniques 
            #   #G2 Eau, gaz, électricité 
            #   
            #   str_detect(NAF36_05, "^H0")         ~ "70-Construction",
            #   str_detect(NAF36_05, "^J1|^J2|^J3") ~ "71-Commerce", 
            #   str_detect(NAF36_05, "^K0")         ~ "72-Logistique",
            #   
            #   #ACTIVFIN == "70-Activités des sièges sociaux ,conseil de gestion" ~ "Sièges sociaux",  25...
            #   
            #   str_detect(NAF36_05, "^P1")     ~ "73-Hôtellerie-restauration",
            #   
            #   str_detect(NAF_05, "^72|^642")  ~ "74-Numérique et télécoms",
            #   #NAF2008 : "JB - Télécommunications",
            #   #          "JC - Activités informatiques et services d'information"
            #   #ICI : "72-Activités informatiques", "642-Télécommunications"
            #   
            #   
            #   str_detect(NAF36_05, "^L0")     ~ "75-Secteur financier",
            #   
            #   str_detect(NAF36_05, "^M0|^N1|^N2|^N3|^N4|^P2|^Q1|^Q2|^R1|^R2") ~ "76-Autres services",
            #   #M0 Activités immobilières 
            #   #N1 Postes et télécommunications (sans les télécoms)
            #   #N2 Conseils et assistance 
            #   #N3 Services opérationnels 
            #   #N4 Recherche et développement 
            #   #P2 Activités récréatives, culturelles et sportives 
            #   #Q1 Education 
            #   #Q2 Santé, action sociale 
            #   #R1 Administrations publiques 
            #   #R2 Activités associatives et extra-territoriale
            #   
            #   str_detect(NAF36_05, "^P3") ~ "80-Particuliers employeurs"
            #   #P3 Services personnels et domestiques 
            # )
            , 
            
            #2013 à 2019 
            false = case_when(
              
              # naf38 ==  "PZ - Enseignement" & 
              #   EMP %in% c("1-Collectivités locales","2-État", "3-Hôpitaux publics") ~ "31-Enseignement public", 
              # 
              # naf38 ==  "PZ - Enseignement"                                          ~ "32-Enseignement privé", 
              
              EMPcj3  == "1-Collectivités locales" ~ "10-Collectivités locales",
              EMPcj3  == "2-État"                  ~ "20-État",
              
              EMPcj3 == "3-Hôpitaux publics"       ~ "41-Hôpitaux publics",
              #str_detect(NAF, "8610Z") | # "8610Z-Activités hospitalières" hors public
                str_detect(STATUT, "^4")           ~ "42-Cliniques privées", 
              
              str_detect(naf38, "^AZ")                        ~ "50-Agriculture",
              # "AZ-Agriculture, sylviculture et pêche" 
              
              
              str_detect(naf38, "^CA|^CD|^CE|^CF") |  ACTIVFIN == "24-Métallurgie"  ~ "60-Industries de process",
              #"CA - Fabric. denrées alimentaires, boissons et prdts à base de tabac",
              #"CD - Cokéfaction et raffinage", "CE - Industrie chimique", "CF - Industrie pharmaceutique"
              
              str_detect(naf38, "^CJ|^CK|^CL|^CM")  ~ "61-Industries d’équipement", 
              #"CJ - Fabrication d'équipements électriques",
              #"CK - Fabrication de machines et équipements n.c.a.",
              #"CL - Fabrication de matériels de transport",
              #"CM - Autres ind. manuf. ,répar. & install. de machines et d'équipemnts"
              
              str_detect(naf38, "^BZ|^CB|^CC|^CG|^CH|^CI|^DZ|^EZ") ~ "62-Industrie autres",
              #"BZ - Industries extractives ",
              #"CB - Fabric. textiles, industries habillement, cuir et chaussure",
              #"CC - Travail du bois, industries du papier et imprimerie ",
              #"CG - Fab. prdts en caoutc. & plastiq. & aut. prdts minéraux non métal.",
              #"CH - Métallurgie & fab. de prdts métalliques sauf machines & équipmnts",
              #"CI - Fabrication de produits informatiques, électroniques et optiques",
              #"DZ - Prod. & distribution électricité, gaz, vapeur & air conditionné",
              #"EZ - Prod. & distrib. eau assainisst, gestion déchets & dépollution"
              
              str_detect(naf38, "^FZ") ~ "70-Construction",
              str_detect(naf38, "^GZ") ~ "71-Commerce",
              str_detect(naf38, "^HZ") ~ "72-Logistique",
              
              #ACTIVFIN == "70-Activités des sièges sociaux ,conseil de gestion" ~ "Sièges sociaux",  25...
              
              str_detect(naf38, "^IZ")     ~ "73-Hôtellerie-restauration",
              
              str_detect(naf38, "^JB|^JC") ~ "74-Numérique et télécoms",
              #"JB - Télécommunications",
              #"JC - Activités informatiques et services d'information"
              
              str_detect(naf38, "^KZ")     ~ "75-Secteur financier",
              
              str_detect(naf38, "^JA|^LZ|^MA|^MB|^MC|^NZ|^QB|^RZ|^SZ|^UZ|^OZ|^QA")   ~ "76-Autres services",
              #"JA - Édition, audiovisuel et diffusion",
              #"LZ - Activités immobilières",
              #"MA - Act. juri., compta., de gest., archi., ingé., ctrle & anal. tech.",
              #"MB - Recherche-développement scientifique",
              #"MC - Autres activités spécialisées, scientifiques et techniques",
              #"NZ - Activités de services administratifs et de soutien",
              #"QB - Hébergement médico-social & social et action sociale sans hébgmnt",
              #"RZ - Arts, spectacles et activités récréatives",
              #"SZ - Autres activités de services ",
              #"UZ - Activités extra-territoriales",
              #"OZ - Administration publique",
              #"QA - Activités pour la santé humaine"
              
              ##En 2019, rien dans "activité des ménages en tant qu'employeurs" (NA) : on code avec STATUT
              #tab(ct, naf38, ANNEE)
              #tab(ct[ct$ANNEE == "2019", ], naf38, STATUT)
              str_detect(naf38, "^TZ") | EMPcj == "H-Particuliers"  ~ "H0-Particuliers employeurs"
            )
    )))
# tab(ct[ct$ANNEE != "2005", ], naf38, secteurcj)
# tab(ct, secteurcj, ANNEE)
# tab(ct, secteurcj, ANNEE, pct = "col", wt = pondcal)




ct <- ct %>%
  mutate(secteurEMPcj = fct_cross(EMPcj3, secteurcj), 
         
         secteurEMPcj = fct_relabel(secteurEMPcj, ~ case_when(
           str_detect(., "^5")  ~ str_replace(., "(5)-PME[^\\d]+(\\d{2})-"                , "\\1\\2-PME "),
           str_detect(., "^6")  ~ str_replace(., "(6)-Grandes entreprises[^\\d]+(\\d{2})-", "\\1\\2-GE "),
           #str_detect(., "^7")  ~ str_replace(., "(7)-Ent taille inconnue[^\\d]+(\\d{2})-", "\\1\\2-EntI ")
         )) 
  ) %>% 
  mutate(secteurEMPcj = as.factor(case_when(
    !EMPcj3 %in% c("5-PME", "6-Grandes entreprises")  
    ~ str_c(str_sub(EMPcj3, 1L, 1L), "00", str_sub(EMPcj3, 2L, -1L) ), 
    TRUE                     ~ as.character(secteurEMPcj)
  ))
  )
# tab(ct, secteurEMPcj)

#tabxplor:::fct_recode_helper(ct, secteurEMPcj)
ct <- ct |> 
  mutate(EMPcj4 = fct_recode(
    secteurEMPcj,
    "41-Collectivités locales"           = "100-Collectivités locales"      ,
    "11-État"                            = "200-État"                       ,
    "21-Hôpitaux publics"                = "300-Hôpitaux publics"           ,
    
    "22-Cliniques privées"               = "542-PME Cliniques privées"      ,
    "90-Agriculture construction"        = "550-PME Agriculture"            ,
    "42-PME"                             = "560-PME Industries de process"  ,
    "42-PME"                             = "561-PME Industries d’équipement",
    "42-PME"                             = "562-PME Industrie autres"       ,
    "90-Agriculture construction"        = "570-PME Construction"           ,
    "42-PME"                             = "571-PME Commerce"               ,
    "42-PME"                             = "572-PME Logistique"             ,
    "42-PME"                             = "573-PME Hôtellerie-restauration",
    "42-PME"                             = "574-PME Numérique et télécoms"  ,
    "42-PME"                             = "575-PME Secteur financier"      ,
    "42-PME"                             = "576-PME Autres services"        ,
    
    "22-Cliniques privées"               = "642-GE Cliniques privées"       ,
    "90-Agriculture construction"        = "650-GE Agriculture"             ,
    "30-Grande industrie"                = "660-GE Industries de process"   ,
    "30-Grande industrie"                = "661-GE Industries d’équipement" ,
    "30-Grande industrie"                = "662-GE Industrie autres"        ,
    "90-Agriculture construction"        = "670-GE Construction"            ,
    "12-Grandes entreprises de services" = "671-GE Commerce"                ,
    "12-Grandes entreprises de services" = "672-GE Logistique"              ,
    "12-Grandes entreprises de services" = "673-GE Hôtellerie-restauration" ,
    "12-Grandes entreprises de services" = "674-GE Numérique et télécoms"   ,
    "12-Grandes entreprises de services" = "675-GE Secteur financier"       ,
    "12-Grandes entreprises de services" = "676-GE Autres services"         ,
    
    "NULL"                               = "H00-Particuliers"               ,
    "NULL"                               = "I00-Pas d'employeur"            
  ) |> 
    fct_relevel(sort)
  )
# tab(ct[ct$ANNEE %in% c("2016", "2019"), ], EMPcj4) #628NA (entreprises taille inconnue)


ct <- ct %>% 
  mutate(EMPcj4_ord = fct_relevel(EMPcj4, "41-Collectivités locales","42-PME",
                                  "21-Hôpitaux publics","22-Cliniques privées",
                                  "11-État","12-Grandes entreprises de services",
                                  "30-Grande industrie",
                                  "90-Agriculture construction" ) , 
         
         EMPcj5 = fct_recode(EMPcj4, 
                             "11-État"                            = "21-Hôpitaux publics",
                             "12-Grandes entreprises de services" = "22-Cliniques privées",
         ) %>% fct_relabel(~ str_replace(., "État", "État_HP")), 
         
         EMPcj5_ETAT_GES = case_when(
           EMPcj5 %in% c("11-État_HP","12-Grandes entreprises de services") ~ EMPcj5, 
           TRUE                                                             ~ factor(NA_character_)
         ),
         
  )
# tab(ct[ct$ANNEE %in% c("2016", "2019"), ], EMPcj5, EMPcj4)


ct <- ct |> mutate(EMP_ens = as.factor(case_when(
  EMPcj4 == "11-État" & PPP2cj == "4I1-Enseignant·es" ~ "11-État: enseignant·es", 
  EMPcj4 == "11-État"                               ~ "12-État hors enseignant·es", 
  EMPcj4 == "12-Grandes entreprises de services"    ~ "13-Grandes entreprises de services", 
  TRUE                                            ~ as.character(EMPcj4)
)))

# ct <- ct %>% 
#   mutate(
#     PPP1cj_EGES = fct_cross(EMP5_ETAT_GES, PPP1cj) %>% 
#       fct_relabel(~ str_remove_all(., stringi::stri_unescape_unicode("\\u2642|\\u2640")) %>% 
#                     str_replace("11-État_HP:(.)(.+$)", "\\1E\\2 État_HP") %>% 
#                     str_replace("12-Grandes entreprises de services:(.)(.+$)", "\\1G\\2 GES") %>% 
#                     str_squish() 
#       )  %>% 
#       fct_lump_min(200, other_level = "ZZ-Autres"), #%>% fct_to_na("Autres")
#     
#     PPP1cj_ord_EGES = fct_cross(EMP5_ETAT_GES, PPP1cj) %>% 
#       fct_relabel(~ str_remove_all(., stringi::stri_unescape_unicode("\\u2642|\\u2640")) %>% 
#                     str_replace("11-État_HP:(.)(.+$)", "E\\1\\2 État_HP") %>% 
#                     str_replace("12-Grandes entreprises de services:(.)(.+$)", "G\\1\\2 GES") %>% 
#                     str_squish()
#       ) %>% 
#       fct_lump_min(200, other_level = "ZZ-Autres") %>%  #%>% fct_to_na("Autres")
#       fct_relevel(sort), 
#     
#     PPP1cjex = fct_recode(PPP1cj,
#                         "5-Exécutant·es"  = "5-Employées",
#                         "5-Exécutant·es"  = "6-Ouvriers"
#     )
#   )
# # tab(ct, PPP1_EGES)
# # tab(ct, PPP1_ord_EGES)


ct <- ct |> 
  mutate(PE1cj = fct_cross(PPP1cj, fct_recode(
    EMPcj4,
    "11-État"    = "11-État"                           ,
    "12-GES"     = "12-Grandes entreprises de services",
    "21-HP"      = "21-Hôpitaux publics"               ,
    "21-HP"      = "22-Cliniques privées"              ,
    "30-GI"      = "30-Grande industrie"               ,
    "41-CL"      = "41-Collectivités locales"          ,
    "42-PME"     = "42-PME"                 ,
    "90-Autres"  = "90-Agriculture construction"       
  )) |> 
    fct_lump_min(150, other_level = "Autres") |> 
    fct_relabel(~ str_replace(., "^(.)-([^:]+):(..)-", "\\1\\3-\\2 ")) |>
    fct_relevel(sort), 
  
  PE2cj = fct_cross(PPP2cj, fct_recode(
    EMPcj4,
    "11-État"    = "11-État"                           ,
    "12-GES"     = "12-Grandes entreprises de services",
    "21-HP"      = "21-Hôpitaux publics"               ,
    "21-HP"      = "22-Cliniques privées"              ,
    "30-GI"      = "30-Grande industrie"               ,
    "41-CL"      = "41-Collectivités locales"          ,
    "42-PME"     = "42-PME"                 ,
    "90-Autres"  = "90-Agriculture construction"      
  )) |> 
    fct_lump_min(90, other_level = "Autres") |> 
    fct_relabel(~ str_replace(., "^(...)-([^:]+):(..)-", "\\1\\3-\\2 ")) |>
    fct_relevel(sort)
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
  "2-Pas pb aide chef"    = "3-Supérieur aide: D’accord"    ,
  "2-Pas pb aide chef"    = "4-Supérieur aide: TAF d’accord",
  "2-Pas pb aide chef"    = "5-Non concerné"                
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

# # Sur 22% qui se font ignorer au travail, 
# #  16% disent "par qq orga" (on perd 5%), et 4% disent "par qq public"
# tab_many(ct[salariat,], c(RP_SUB_IGNORE), #RP_ORGA_IGNORE
#          RP_SUB_PAR_ORGA, ANNEE,  wt = pondqaa, pct = "all")
# tab_many(ct[salariat,], c(RP_SUB_IGNORE), #RP_PUBLIC_IGNORE
#          RP_SUB_PAR_PUBLIC, ANNEE,  wt = pondqaa, pct = "all")




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


ct$ATMAL  <- replace_na(ct$ATMAL, 0)  # Nombre de jours d'arrêt maladie
ct$NBJARR <- replace_na(ct$NBJARR, 0) # Nombre de jours d'arrêt après un accident de travail
ct$ARETMAL2 <- if_else(ct$ATMAL >= 5, 
                       true  = "1-Au moins un arrêt maladie >= 5j (hors maternité)",
                       false = "2-Pas arrêt mal") |> 
  as.factor()
ct$ARETMAL3 <- if_else(ct$ATMAL - ct$NBJARR >= 5, 
                       true  = "1-Au moins un arrêt maladie >= 5j (hors maternité, accident)",
                       false = "2-Pas arrêt mal") |> 
  as.factor()
# tab(ct, ARETMAL2, ANNEE, pct = "col")
# tab(ct, ARETMAL3, ANNEE, pct = "col")
# tab(ct, ARETMAL2, ARRET, ANNEE, pct = "all")

# ct$ARETMAL2 <- fct_recode(
#   ct$ARETMAL,
#   "1-Un arrêt ou plus" = "1-Un arrêt"      ,
#   "1-Un arrêt ou plus" = "2-Deux arrêts"   ,
#   "1-Un arrêt ou plus" = "3-Trois arrêts +",
#   "2-Pas d’arrêt"      = "0-Pas d’arrêt"   ,
# ) |> fct_relevel(sort)


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





vars_sante <- c("BSANTE2", "ACCIDT2", "ARETMAL3")
vars_sante16 <- c("BSANTE2", "ACCIDT2", "ARETMAL3", "SOMTBL2", "DORMED2", "EDM", "TAG") #"ARET5"
vars_sante16b <- c("BSANTE2", "ACCIDT2", "ARETMAL3", "SOMTBL2", "DORMED2", "DORMED3", "EDM", "TAG") #"ARET5"


# ACCIDT2       #Proportion de salariés déclarant au moins un accident du travail au cours des 12 derniers mois
# ARRET         #Présence d'AT avec arrêt travail
# ARRET/ACCIDT2 #Proportion des arrêts dans les accidents















#tictoc::toc()

# Save to RDS (no compression) -----------------------------------------------------------
# saveRDS(ct, file = "~\\Data\\Conditions de travail\\ct.rds", compress = FALSE)





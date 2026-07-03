




pc18 <- pc18 |> 
  mutate(TELE      = C1, 
         JV        = B2,
         RADIO     = E1, 
         VIDEOS    = C15, 
         RESEAUX   = I6,
         THEATRE   = G134,
         CLASSIQUE = G2512, 
         POP_ROCK  = G2509 ,
         JAZZ      = G2510 ,
         CIRQUE    = G132,
         MUSEE_EXPO= H210, 
         MONUMENT  = H209,
  ) |> 
  select(
    POND,
    
    # Musique
    E7, E81:E87,
    
    # Télévision
    TELE,

    # Jeux-Vidéo
    JV, # jv12 = B1,
    
    # Radio
    RADIO, 
    
    # Vidéos en ligne
    VIDEOS, 
    
    # Utilisation des réseaux sociaux
    RESEAUX, # I6, D35, #freq # comme moyen d'information
    
     # Livres (dont BD)
    F11, F13, F13_C_1, F14, F15, F15_C_1, # BD
    
    # Cinéma 
    G3A, # ??
    G4_C_1, G5, # fréquence : valeur déclarée + unité
    
    # Spectacle de danse
    G131, G161:G165, 
    
    # Pièce de théatre 
    THEATRE, # hors spectacles de rue
    
    # Concert de musique classique
    CLASSIQUE, 
    
    # Concert de rock ou de jazz
    POP_ROCK, # "concert de pop, rock"                                                        
    JAZZ    , # "concert de Jazz"                                                             
    
    G2501 , # "concert de chansons ou variétés françaises"                                  
    G2502 , # "concert de musiques du monde"                                                
    G2503 , # "concert de musiques traditionnelles"                                         
    G2504 , # "concert de variétés internationales"                                         
    G2505 , # "concert de RnB"                                                              
    G2506 , # "concert de musiques électroniques, techno"                                   
    G2507 , # "concert de Hip hop, rap"                                                     
    G2508 , # "concert de Metal, hard rock"                                                 
    
    # Spectacle de cirque
    CIRQUE,
    
    # Patrimoine
    MUSEE_EXPO, H211,
    
    MONUMENT,
    
    
    CLASS_univprat, CLASS_univprat_name,
    
    # "Sociodémo" 
    SEXE, AGE, CRITAGE, CRITREVENU, VITENCOUPLE, G_PCS_MENAGE_, SG_PCS_MENAGE_, 
    
    # "Travail
    SITUA, STATUT, TYPEMPLOI, TEMPTRAV, SUPERVISION, CSTOT, CSTOT_conj,
    HH = S10_C_1, # heures de travail par semaines
    
    # "Cap_cult"
    DIPLOM, SPECIAL, DIPLOM_conj, SPECIAL_conj,
    
    # "Enfance"
    CSTOT_PER, CSTOT_MER,
    
    # "Logement"
    STOC, REG, 
    
    everything()
  ) 
# tabxplor:::fct_recode_helper(pc18)
# dico |> filter(str_detect(libelles, regex("vidéo", ignore_case = TRUE))) |> print(n = 100)


pc18 <- pc18 |>
  # Ils font : hors radio ; j'ajoute hors télévision
  mutate(MUSIQUE_B = as.factor(case_when(
    (E86 =="1-Radio" | E87 == "1-Télévision") &
      !(E81 !=  "2-Non" | 
          E82 != "2-Non" |
          E83 !=  "2-Non" |
          E84 !=  "2-Non" |
          E85 !=  "2-Non" #|
          #E87 !=  "2-Non"
          )
    ~ "3-Musique: jamais", 
    
    E7 == "1-Oui, tous les jours ou presque" 
    ~ "1-Musique: quotidien", 
    
    E7 %in% c("2-Oui, environ 3 ou 4 jours par semaine",
              "3-Oui, environ 1 ou 2 jours par semaine",
              "4-Oui, environ 1 à 3 jours par mois",    
              "5-Oui, plus rarement")  
    ~ "2-Musique: occasionnel", 
    
    TRUE ~ "3-Musique: jamais", 
  )), 
  
  MUSIQUE = as.factor(case_when(
    (E86 =="1-Radio") &
      !(E81 !=  "2-Non" | 
          E82 != "2-Non" |
          E83 !=  "2-Non" |
          E84 !=  "2-Non" |
          E85 !=  "2-Non" |
          E87 !=  "2-Non"
      )
    ~ "3-Musique: jamais", 
    
    E7 == "1-Oui, tous les jours ou presque" 
    ~ "1-Musique: quotidien", 
    
    E7 %in% c("2-Oui, environ 3 ou 4 jours par semaine",
              "3-Oui, environ 1 ou 2 jours par semaine",
              "4-Oui, environ 1 à 3 jours par mois",    
              "5-Oui, plus rarement")  
    ~ "2-Musique: occasionnel", 
    
    TRUE ~ "3-Musique: jamais", 
  )), 
  )
# pc18 |> tab_many(c(E7, E86, E87, MUSIQUE, MUSIQUE_B), wt = POND, pct = "col")


# pc18 |> tab_many(c(F13, F13_C_1, F14, F15, F15_C_1), pct = "col", wt = POND)

# En réalité ils n'ajoutent pas toujours les BD...
pc18 <- pc18 |>
  mutate(
  LIVRES = case_when(
    F13_C_1 >= 20 ~ "1-Livre: 20+", 
    F13_C_1 >= 10 ~ "2-Livre: 10-19", 
    F13_C_1 >= 1  ~ "3-Livre: 1-9", 
    F13_C_1 == 0  ~ "4-Livre: aucun", 
  ) |> as.factor()
  
  )
  # pc18 |> tab_many(c(LIVRES), pct = "col", wt = POND)
  # pc18 |> tab_many(F15_C_1, F14, pct = "row", wt = POND)


pc18 <- pc18 |> 
  mutate(nb_cine = case_when(
    is.na(G4_C_1)                   ~ 0L,  # "3-NSP", "4-REF" G3A %in% c("2-Non")
    G5 ==  "1-Par semaine"          ~ G4_C_1*51L,
    G5 ==  "2-Par mois"             ~ G4_C_1*12L,
    G5 ==  "3-Par an"               ~ G4_C_1,
  ),
  
  CINEMA = as.factor(case_when(
    nb_cine >= 12 ~ "1-Cinéma: 12+", 
    nb_cine >= 3  ~ "2-Cinéma: 3-11", 
    nb_cine >= 1  ~ "3-Cinéma: 1-2", 
    nb_cine == 0  ~ "4-Cinéma: aucun", 
  )), 
  
  CINEMA3 = as.factor(case_when(
    nb_cine >= 10 ~ "1-Cinéma: 10+", 
    nb_cine >= 1  ~ "3-Cinéma: 1-9", 
    nb_cine == 0  ~ "4-Cinéma: aucun", 
  )), 
  )
# pc18 |> tab_many(c(G3A, G5, G4_C_1, CINEMA), pct = "col", wt = POND)
# pc18 |> tab_many(G4_C_1, G3A, pct = "row", wt = POND)
# pc18 |> tab(CINEMA3, pct = "col", wt = POND)


# pc18$DANSE <- fct_recode(
#   pc18$DANSE,
#   "1-Danse"     = "1-Aller à un spectacle de danse",
#   "2-Pas danse" = "2-Non"                          
# ) |> fct_na_value_to_level("2-Pas danse")
# # pc18 |> tab_many(G161:G167, pct = "col", wt = POND)

pc18 <- pc18 |>
  mutate(DANSE = as.factor(case_when(
    ( G161 == "1-De la danse classique" & !is.na(G161) ) |
      #( G162 == "1-De la danse traditionnelle ou folklorique" & !is.na(G162) ) |
      ( G163 == "1-De la danse modern jazz" & !is.na(G163) ) |
      ( G164 == "1-De la danse contemporaine" & !is.na(G164) ) #|
      #( G165 == "1-D’un autre genre" & !is.na(G165) ) 
    ~  "1-Danse", 
    
    TRUE ~ "2-Pas danse"
  ))) #|>
  #pc18 |> tab_many(DANSE, pct = "col", wt = POND)




pc18$THEATRE <- fct_recode(
  pc18$THEATRE,
  "1-Théâtre"    = "1-Aller voir une pièce de théâtre y compris one man show, improvisation",
  "2-Pas théâtre" = "2-Non"                                                                  
) |> fct_na_value_to_level("2-Pas théâtre")


pc18$CLASSIQUE <- fct_recode(
  pc18$CLASSIQUE,
  "1-Concert classique"     = "1-Aller à un concert de musique classique",
  "2-Pas concert classique" = "2-Non"                                    
) |> fct_na_value_to_level( "2-Pas concert classique" )



pc18$CHANSON <- fct_recode(
  pc18$G2501,
  "1-Concert chanson française"     = "1-Aller à un concert de chansons ou variétés françaises",
  "2-Pas chanson française" = "2-Non"                                                  
) |> fct_na_value_to_level("2-Pas chanson française"  )


pc18$WORLD <- fct_recode(
  pc18$G2502,
  "1-Concert de musiques du monde" = "1-Aller à un concert de musiques du monde",
  "2-Pas musiques de monde"        = "2-Non"                                    
) |> fct_na_value_to_level("2-Pas musiques de monde" )


pc18$TRADI <- fct_recode(
  pc18$G2503,
  "1-Concert tradi"     = "1-Aller à un concert de musiques traditionnelles",
  "2-Pas concert tradi" = "2-Non"                                           
) |> fct_na_value_to_level( "2-Pas concert tradi")


pc18$VARIETE <- fct_recode(
  pc18$G2504,
  "1-Concert variété internationale" = "1-Aller à un concert de variétés internationales",
  "2-Pas variété internationale"    = "2-Non"                                           
) |> fct_na_value_to_level(  "2-Pas variété internationale" )


pc18$RNB <- fct_recode(
  pc18$G2505,
  "1-Concert RnB" = "1-Aller à un concert de RnB",
  "2-Pas RnB"     = "2-Non"                      
) |> fct_na_value_to_level( "2-Pas RnB")


pc18$ELECTRO <- fct_recode(
  pc18$G2506,
  "1-Concert électro" = "1-Aller à un concert de musiques électroniques, techno",
  "2-Pas électro"     = "2-Non"                                                 
) |> fct_na_value_to_level(  "2-Pas électro" )


pc18$RAP <- fct_recode(
  pc18$G2507,
  "1-Concert hip hop/rap" = "1-Aller à un concert de Hip hop, rap",
  "2-Pas concert rap"     = "2-Non"                               
) |> fct_na_value_to_level("2-Pas concert rap")


pc18$METAL <- fct_recode(
  pc18$G2508,
  "1-Concert métal/hard rock" = "1-Aller à un concert de Metal, hard rock",
  "2-Pas métal/hard rock"                                    = "2-Non"                                   
) |> fct_na_value_to_level("2-Pas métal/hard rock" )

pc18$POP_ROCK_JAZZ <- as.factor(case_when(
  pc18$POP_ROCK == "1-Aller à un concert de pop, rock" | 
    pc18$JAZZ == "1-Aller à un concert de Jazz" |
    pc18$METAL == "1-Concert métal/hard rock"
  ~ "1-Concert pop/rock/jazz", 
  
  TRUE ~ "2-Pas concert pop/rock/jazz"
))







pc18 <- pc18 |>
  mutate(
    NATIO2 = as.factor(case_when(
      
      NATIO1N1 == "1-Français/e de naissance, y compris par réintégration" & !is.na(NATIO1N1)
      ~ "1-Français·e de naissance", 
      
      NATIO1N2 == "1-oui" & !is.na(NATIO1N1)   
      ~ "2-Français·e naturalisé·e", 
      
      TRUE ~ "3-Étranger·e (ou apatride)"
    )), 
    
    NATIO = as.factor(if_else(str_detect(NATIO2, "^1|^2"), "1-Français·e", "2-Étranger·e")), 
    
    IMMI = as.factor(if_else(str_detect(NATIO2, "^1"), 
                             true  = "1-Français·e de naissance", 
                             false = "2-Immigré·e")), 
    
  )
# pc18 |> tab_many(c(NATIO, NATIO2, IMMI, LNAIS))



pc18 <- pc18 |> 
  mutate(
  NATIO_PARENTS = as.factor(case_when(
    NATIOM == "1-Française" & !is.na(NATIOM) & 
      NATIOP == "1-Française" & !is.na(NATIOP) 
    ~ "1-Deux parents nés français", 
    
    (NATIOM == "1-Française" & !is.na(NATIOM)) |
      (NATIOP == "1-Française" & !is.na(NATIOP))
    ~ "2-Un parent né français", 
    
    (NATIOM == "2-Etrangère" & !is.na(NATIOM)) |
      (NATIOP == "2-Etrangère" & !is.na(NATIOP))
    ~ "3-Deux parents nés étrangers", 
    
    )), 
  
  NATIO_PARENTS2 = as.factor(case_when(
    #LNAIS == "1-En France (métropole ou outremer)" &
      NATIO_PARENTS == "1-Deux parents nés français"
    ~ "1-Deux parents nés français", 

    LNAIS == "1-En France (métropole ou outremer)" & 
    NATIO_PARENTS == "2-Un parent né français" 
    ~ "2-Né·e en France, 1 parent né français", 
    
    LNAIS == "1-En France (métropole ou outremer)" & 
      NATIO_PARENTS == "3-Deux parents nés étrangers" 
    ~ "3-Né·e en France, 2 parents nés étrangers", 

    # LNAIS == "2-A l’étranger" &
    #   NATIO_PARENTS == "1-Deux parents nés français"
    # ~ "4-Hors France, 2 parents nés français", 
    
    LNAIS == "2-A l’étranger" & 
      NATIO_PARENTS == "2-Un parent né français" 
    ~ "4-Né·e hors France, 1 parent né français", 
    
    LNAIS == "2-A l’étranger" & 
      NATIO_PARENTS == "3-Deux parents nés étrangers" 
    ~ "5-Né·e hors France, 2 parents nés étrangers",
    
    
  )), 
  )
# pc18 |> tab_many(c(NATIO_PARENTS, NATIO_PARENTS2), pct = "col", wt = POND)

pc18 <- pc18 |>
  mutate(
    MERE = case_when(adulte_enfance1 == "1-Votre mère" ~ 1L,
                     adulte_enfance2 == "1-Votre mère" ~ 2L,
    ), 
    
    PERE = case_when(adulte_enfance1 == "2-Votre père" ~ 1L,
                     adulte_enfance2 == "2-Votre père" ~ 2L,
    ), 
    
    LANGUE_MERE = as.factor(case_when(
      MERE == 1L & M9_BIS2_adulte1 == "1-Une ou plusieurs langues étrangères"
      ~ "1-Mère langue étrangère", 
      
      MERE == 1L ~ "2-Mère français/autre", 
      
      
    )), 
    
    LANGUE_PERE = as.factor(case_when(
      (PERE == 1L & M9_BIS2_adulte1 == "1-Une ou plusieurs langues étrangères")
      | (PERE == 2L & M9_BIS2_adulte2 == "1-Une ou plusieurs langues étrangères")
      ~ "1-Père langue étrangère", 
      
      PERE %in% 1:2 ~ "2-Père français/autre", 
      
    )), 
    
    LANGUE_PARENTS = as.factor(case_when(
      LANGUE_MERE == "1-Mère langue étrangère" & !is.na(LANGUE_MERE) &
        LANGUE_PERE == "1-Père langue étrangère" & !is.na(LANGUE_PERE)
      ~ "2 parents", 
      
      (LANGUE_MERE == "1-Mère langue étrangère" & !is.na(LANGUE_MERE)) |
        (LANGUE_PERE == "1-Père langue étrangère" & !is.na(LANGUE_PERE))
      ~ "1 parent", 
      
      is.na(LANGUE_MERE) & is.na(LANGUE_PERE) ~ NA_character_,
      
      !(LANGUE_MERE == "1-Mère langue étrangère" & !is.na(LANGUE_MERE)) &
        !(LANGUE_PERE == "1-Père langue étrangère" & !is.na(LANGUE_PERE))
      ~ "0 parent langue étrangère", 
    ))
  )


pc18 <- pc18 |>
  group_by(NOIKISH) |> 
  mutate(ANARRIV = eval_tidy(sym(unique(paste0("ANARRIV_pers", NOIKISH, "_C_1")))), 
         AGARRIV = ANARRIV - eval_tidy(sym(unique(paste0("ANAIS_pers", NOIKISH))))
         ) |> 
  ungroup() |>
  mutate(
    NB_ANARRIV = 2018 - ANARRIV, 
    ANARRIV_TR = as.factor(case_when(
      ANARRIV < 1970  ~ "1-Installé avant 1970", 
      ANARRIV < 1980  ~ "2-1970-1979", 
      ANARRIV < 1990  ~ "3-1980-1989", 
      ANARRIV < 2000  ~ "4-1990-1999", 
      ANARRIV < 2010  ~ "5-2000-2009", 
      ANARRIV >= 2010 ~ "6-2010 ou après", 
    )), 
    ANARRIV_TR3 = as.factor(case_when(
      ANARRIV < 1970  ~ "1-Installé avant 1970", 
      ANARRIV < 2000  ~ "2-1970-1999", 
      ANARRIV >= 2000 ~ "3-2000 ou après", 
    )), 
    
    AGARRIV_TR = as.factor(case_when(
      AGARRIV < 6   ~ "1-Installé en France avant 6 ans", 
      AGARRIV < 17  ~ "2-6 à 16 ans", 
      AGARRIV < 25  ~ "3-17 à 24 ans", 
      AGARRIV < 40  ~ "4-25 à 39 ans", 
      AGARRIV >= 40 ~ "5-40 ans ou plus", 
    )), 
    
    AGARRIV_TR3 = as.factor(case_when(
      AGARRIV < 17  ~ "1-Installé en France avant 16 ans", 
      AGARRIV < 25  ~ "2-17 à 24 ans", 
      AGARRIV >= 25 ~ "3-25 ans ou plus", 
    )), 
    
  )

# # nombre d'enfants (pers < 18, mais pas en première personne) dans le logement
pc18 <- pc18 |>
  select(-starts_with("ENF_")) |> 
  mutate(
    across(starts_with("AGE_pers"), 
           ~ if_else(AGE >= 18 & . < 18 & !is.na(.), 1L, 0L), 
           .names = "ENF_{.col}"
    ), 
  ) |> 
  rename_with(~ str_replace(., "ENF_AGE_pers", "ENF_")) |>
  # tab_many(starts_with("ENF_"))
  mutate(NB_ENF = ENF_2 + ENF_3 + ENF_4 + ENF_5 + ENF_6 + ENF_7 + ENF_8, 
         NB_ENF2 = as.factor(if_else(NB_ENF > 0, 
                                     "1-Mineur·es dans le logement",
                                     "2-Pas de mineur·es")), 
         NB_ENF = fct_recode(as.factor(NB_ENF), 
                             "0 mineur·e dans le logement" = "0", 
                             "4 ou plus" = "4", 
                             "4 ou plus" = "5",
                             "4 ou plus" = "6"), 

  ) 

# Écoute même musique que les parents ? 

# % des styles écoutés par les parents écoutés par l'enquêté
pc18 <- pc18 |> 
  score_from_lv1(
    "NB_MUS_PARENTS", 
    vars_list = c("M301", "M302", "M303", "M304", "M305", "M306", 
                  "M307", "M308", "M309", "M310", "M311", "M312" )
  ) |> 
  mutate(
    ZZ1001 = replace_na(str_detect(M301, "^1") & str_detect(E1001, "^1"), FALSE),
    ZZ1002 = replace_na(str_detect(M302, "^1") & str_detect(E1002, "^1"), FALSE),
    ZZ1003 = replace_na(str_detect(M303, "^1") & str_detect(E1003, "^1"), FALSE),
    ZZ1004 = replace_na(str_detect(M304, "^1") & str_detect(E1004, "^1"), FALSE),
    ZZ1005 = replace_na(str_detect(M305, "^1") & str_detect(E1005, "^1"), FALSE),
    ZZ1006 = replace_na(str_detect(M306, "^1") & str_detect(E1006, "^1"), FALSE),
    ZZ1007 = replace_na(str_detect(M307, "^1") & str_detect(E1007, "^1"), FALSE),
    ZZ1008 = replace_na(str_detect(M308, "^1") & str_detect(E1008, "^1"), FALSE),
    ZZ1009 = replace_na(str_detect(M309, "^1") & str_detect(E1009, "^1"), FALSE),
    ZZ1010 = replace_na(str_detect(M310, "^1") & str_detect(E1010, "^1"), FALSE),
    ZZ1011 = replace_na(str_detect(M311, "^1") & str_detect(E1011, "^1"), FALSE),
    ZZ1012 = replace_na(str_detect(M312, "^1") & str_detect(E1012, "^1"), FALSE), 
    
    YY1001 = replace_na(str_detect(E1001, "^1") & str_detect(M301, "^2"), FALSE),
    YY1002 = replace_na(str_detect(E1002, "^1") & str_detect(M302, "^2"), FALSE),
    YY1003 = replace_na(str_detect(E1003, "^1") & str_detect(M303, "^2"), FALSE),
    YY1004 = replace_na(str_detect(E1004, "^1") & str_detect(M304, "^2"), FALSE),
    YY1005 = replace_na(str_detect(E1005, "^1") & str_detect(M305, "^2"), FALSE),
    YY1006 = replace_na(str_detect(E1006, "^1") & str_detect(M306, "^2"), FALSE),
    YY1007 = replace_na(str_detect(E1007, "^1") & str_detect(M307, "^2"), FALSE),
    YY1008 = replace_na(str_detect(E1008, "^1") & str_detect(M308, "^2"), FALSE),
    YY1009 = replace_na(str_detect(E1009, "^1") & str_detect(M309, "^2"), FALSE),
    YY1010 = replace_na(str_detect(E1010, "^1") & str_detect(M310, "^2"), FALSE),
    YY1011 = replace_na(str_detect(E1011, "^1") & str_detect(M311, "^2"), FALSE),
    YY1012 = replace_na(str_detect(E1012, "^1") & str_detect(M312, "^2"), FALSE), 
    
    PCT_MEME_MUS_PARENTS_num = (ZZ1001 + ZZ1002 + ZZ1003 + ZZ1004 + ZZ1005 + ZZ1006 + 
                                  ZZ1007 + ZZ1008 + ZZ1009 + ZZ1010 + ZZ1011 + ZZ1012)/NB_MUS_PARENTS, 
    
    NB_MUS_PAS_PARENTS_num = YY1001 + YY1002 + YY1003 + YY1004 + YY1005 + YY1006 + 
      YY1007 + YY1008 + YY1009 + YY1010 + YY1011 + YY1012, 
    
    PCT_MEME_MUS_PARENTS = as.factor(case_when(
      is.nan(PCT_MEME_MUS_PARENTS_num) ~ "5-Parents : pas de musique", 
      PCT_MEME_MUS_PARENTS_num == 0    ~ "1-N'écoute aucun style des parents", 
      PCT_MEME_MUS_PARENTS_num <= 0.5  ~ "2-La moitié ou moins des styles", 
      PCT_MEME_MUS_PARENTS_num <  1    ~ "3-Entre 55 et 90% des styles", 
      PCT_MEME_MUS_PARENTS_num == 1    ~ "4-Écoute tous les styles des parents", 
    )), 
    
    NB_MUS_PAS_PARENTS = as.factor(case_when(
      NB_MUS_PAS_PARENTS_num == 0 ~ "1-Aucun style de plus que ses parents", 
      NB_MUS_PAS_PARENTS_num <= 2 ~ "2-1 ou 2 styles de plus",
      NB_MUS_PAS_PARENTS_num <= 4 ~ "3-3 à 5 styles de plus",
      NB_MUS_PAS_PARENTS_num <= 12~ "4-6 à 12 styles de plus",
    )), 
    
    
  )
# pc18 |> tab(PCT_MEME_MUS_PARENTS)
# pc18 |> tab(NB_MUS_PAS_PARENTS)

pc18 <- pc18 |> 
  mutate(
    SERIES_AVEC = 
      as.factor(case_when(
        C2003 == "1-Avec vos enfants" ~ "1-Regarde séries avec ses enfants", 
        C2002 == "1-En couple"        ~ "2-Séries en couple, sans les enfants", 
        C2001 == "1-Seul(e)" | C2006 == "1-Avec un ou des amis" |
          C2007 == "1-En groupe organisé"
        ~ "3-Séries toujours seul, ami·es, groupes",
        !is.na(C2001) ~ "4-Autres", 
        is.na(C2001) ~ "5-Jamais de séries", 
      ))
  )
# pc18 |> tab(SERIES_AVEC)
# pc18 |> tab_many(starts_with("C20"))

 
 pc18$DIPLOME_PERE <- fct_recode(
   as.factor(pc18$M19),
   "1-Aucun diplôme"    = "1",
   "1-Aucun diplôme"    = "2",
   "1-Aucun diplôme"    = "3",
   "2-CAP BEP"          = "4",
   "2-CAP BEP"          = "5",
   "2-CAP BEP"          = "6",
   "4-Bac"              = "7",
   "4-Bac"              = "8",
   "3-Bac pro"          = "9",
   "5-Bac+2"            = "10",
   "6-Licence"          = "11",
   "7-Bac+5"            = "12",
   "7-Bac+5"            = "13",
   "8-NSP"              = "14",
   "8-NSP"              = "15"
 ) |> fct_relevel(sort)
 
 
 pc18$DIPLOME_MERE <- fct_recode(
   as.factor(pc18$M21),
   "1-Aucun diplôme"    = "1",
   "1-Aucun diplôme"    = "2",
   "1-Aucun diplôme"    = "3",
   "2-CAP BEP"          = "4",
   "2-CAP BEP"          = "5",
   "2-CAP BEP"          = "6",
   "4-Bac"              = "7",
   "4-Bac"              = "8",
   "3-Bac pro"          = "9",
   "5-Bac+2"            = "10",
   "6-Licence"          = "11",
   "7-Bac+5"            = "12",
   "7-Bac+5"            = "13",
   "8-NSP"              = "14",
   "8-NSP"              = "15"
 ) |> fct_relevel(sort)
 

pc18 <- pc18 |> mutate(CSTOTR_MOB = fct_cross(CSTOTR_PER, CSTOTR))

pc18$CSTOTR_MOB <- fct_recode(
  pc18$CSTOTR_MOB,
  "2-Reproduction"         = "1-Père agriculteur:1-Agriculteurs"                      ,
  "2-Reproduction"         = "2-Père artisan etc.:1-Agriculteurs"                     ,
  "3-Déclassement"         = "3-Père CPIS:1-Agriculteurs"                             ,
  "3-Déclassement"         = "4-Père PI:1-Agriculteurs"                               ,
  "3-Déclassement"         = "5-Père employé:1-Agriculteurs"                          ,
  "4-Indécidable"          = "6-Père ouvrier:1-Agriculteurs"                          ,
  "1-Ascension sociale"    = "1-Père agriculteur:2-Artisans commercants patrons"      ,
  "2-Reproduction"         = "2-Père artisan etc.:2-Artisans commercants patrons"     ,
  "4-Indécidable"          = "3-Père CPIS:2-Artisans commercants patrons"             ,
  "4-Indécidable"          = "4-Père PI:2-Artisans commercants patrons"               ,
  "1-Ascension sociale"    = "5-Père employé:2-Artisans commercants patrons"          ,
  "1-Ascension sociale"    = "6-Père ouvrier:2-Artisans commercants patrons"          ,
  "1-Ascension sociale"    = "1-Père agriculteur:3-Cadres et professions supérieures" ,
  "4-Indécidable"          = "2-Père artisan etc.:3-Cadres et professions supérieures",
  "2-Reproduction"         = "3-Père CPIS:3-Cadres et professions supérieures"        ,
  "1-Ascension sociale"    = "4-Père PI:3-Cadres et professions supérieures"          ,
  "1-Ascension sociale"    = "5-Père employé:3-Cadres et professions supérieures"     ,
  "1-Ascension sociale"    = "6-Père ouvrier:3-Cadres et professions supérieures"     ,
  "1-Ascension sociale"    = "1-Père agriculteur:4-Professions intermédiaires"        ,
  "4-Indécidable"          = "2-Père artisan etc.:4-Professions intermédiaires"       ,
  "3-Déclassement"         = "3-Père CPIS:4-Professions intermédiaires"               ,
  "2-Reproduction"         = "4-Père PI:4-Professions intermédiaires"                 ,
  "1-Ascension sociale"    = "5-Père employé:4-Professions intermédiaires"            ,
  "1-Ascension sociale"    = "6-Père ouvrier:4-Professions intermédiaires"            ,
  "3-Déclassement"         = "1-Père agriculteur:5-Employées"                         ,
  "3-Déclassement"         = "2-Père artisan etc.:5-Employées"                        ,
  "3-Déclassement"         = "3-Père CPIS:5-Employées"                                ,
  "3-Déclassement"         = "4-Père PI:5-Employées"                                  ,
  "3-Déclassement"         = "5-Père employé:5-Employées"                             ,
  "2-Reproduction"         = "6-Père ouvrier:5-Employées"                             ,
  "3-Déclassement"         = "1-Père agriculteur:6-Ouvriers"                          ,
  "3-Déclassement"         = "2-Père artisan etc.:6-Ouvriers"                         ,
  "3-Déclassement"         = "3-Père CPIS:6-Ouvriers"                                 ,
  "3-Déclassement"         = "4-Père PI:6-Ouvriers"                                   ,
  "3-Déclassement"         = "5-Père employé:6-Ouvriers"                              ,
  "2-Reproduction"         = "6-Père ouvrier:6-Ouvriers"                              
) |>
  fct_relevel(sort)
# pc18 |> tab(CSTOTR_MOB)




### New variables 3 -----
pc18$TELE5 <- fct_recode(
  pc18$C1,
  "1-Télé: quotidien" = "1-Tous les jours ou presque"       ,
  "2-3 ou 4 jrs/sem"  = "2-Environ 3 ou 4 jours par semaine",
  "3-1 ou 2 jours"    = "3-Environ 1 ou 2 jours par semaine",
  "4-Plus rarement"   = "4-Plus rarement"                   ,
  "5-Jamais"          = "5-Jamais ou pratiquement jamais"   ,
  "5-Jamais"          = "6-NSP"                             ,
  "5-Jamais"          = "7-REF"
)

pc18 <- pc18 |>
  mutate(
    FESTI_MUSIQUE = if_else(
      str_detect(G332, "^1-") |  str_detect(G333, "^1-") |  str_detect(G334, "^1-"), 
      factor("1-Festival musical"), 
      factor("2-Pas festi mus"),
    ) |> fct_na_value_to_level("2-Pas festi mus")
  )

# Nombre de spectacles par an concerts (chaque style + total), théatre, danse... 
pc18 <- pc18 |> 
  mutate(
    across(starts_with(c("G26val", "G14val")), ~ case_when(
      rlang::eval_tidy(sym(str_replace(cur_column(), "val", "unit"))) == "1-Par semaine" ~ .*50,
      rlang::eval_tidy(sym(str_replace(cur_column(), "val", "unit"))) == "2-Par mois"    ~ .*12,
      rlang::eval_tidy(sym(str_replace(cur_column(), "val", "unit"))) == "3-Par an"      ~ .
    ) |>
      as.integer() |>
      replace_na(0)
    ), 
    
    NB_MUSEE_EXPO = case_when(
      H4F == "1-Par semaine" ~ H4_C_1*50,
      H4F == "2-Par mois"    ~ H4_C_1*12,
      H4F == "3-Par an"      ~ H4_C_1
    ) |>
      as.integer() |>
      replace_na(0), 
    
    NB_CONCERTS=G26val_variet_francaise + G26val_musiques_monde + G26val_musiques_tradi +
      G26val_variet_internationale + G26val_rnb + G26val_electro_techno +
      G26val_hip_hop_rap + G26val_metal_hard + G26val_pop_rock + G26val_jazz +
      G26val_opera + G26val_musique_classique, 
    
    CONCERTS = as.factor(case_when(
      NB_CONCERTS == 0      ~ "0-Aucun concert", 
      NB_CONCERTS == 1      ~ "1-1 concert", 
      #NB_CONCERTS == 2      ~ "2-2 concerts", 
      NB_CONCERTS %in% 2:4  ~ "2-2 à 4 concerts", 
      NB_CONCERTS >= 5      ~ "3-5 ou plus", 
    )), 
    
    THEATRE4 = as.factor(case_when(
      G14val_theatre == 0      ~ "0-Théâtre: jamais", 
      G14val_theatre == 1      ~ "1-1 fois", 
      G14val_theatre == 2      ~ "2-2 fois", 
      G14val_theatre >= 3      ~ "3-3 fois ou +", 
    )), 
    
    MUSEE_EXPO_4 = as.factor(case_when(
      NB_MUSEE_EXPO == 0      ~ "0-Musée expo: jamais", 
      NB_MUSEE_EXPO == 1      ~ "1-1 musée", 
      NB_MUSEE_EXPO %in% 2:4  ~ "2-2 à 4 musées", 
      NB_MUSEE_EXPO >= 5      ~ "3-5 musées ou +", 
    ))
    
  )
# nombre de concerts

# pc18 |> tab(G14val_danse, pct = "col", wt = POND)
# pc18 |> tab(G14val_cirque, pct = "col", wt = POND)
# pc18 |> tab(G14val_spectacle_rue, pct = "col", wt = POND)
# pc18 |> tab(G14val_theatre, pct = "col", wt = POND)
# pc18 |> tab(NB_MUSEE_EXPO, pct = "col", wt = POND)


# tab(pc18, DIPLOM, MUSEE_EXPO_4, wt = POND, pct = "row", color = "diff", na = "drop", ref = 7, digits = 1) |> tab_kable()

#   activité qui a la plus d'importance pour la personne, parmi celles pratiquées dans les 12 derniers mois 
pc18 <- pc18 |> 
  mutate(
    PRATIQUE_ARTISTIQUE = case_when( 
      !is.na(RECODE_A21) & RECODE_A21 >= 2     ~ A32BIS, 
      is.na(RECODE_A21) | RECODE_A21 == 0      ~ factor("14-Aucune en particulier"), 
      
      str_detect(A21_musique, "^1")              ~ factor("1-Faire de la musique ou du chant"                                          ),
      str_detect(A21_journal, "^1")              ~ factor("2-Tenir un journal intime ou personnel, noter vos impressions ou réflexions"),
      str_detect(A21_romans, "^1")               ~ factor("3-Ecrire des poèmes, des nouvelles ou un roman"                             ),
      str_detect(A21_peinture, "^1")             ~ factor("4-Faire de la peinture, sculpture ou gravure"                               ),
      str_detect(A21_montages, "^1")             ~ factor("5-Faire des montages audio, vidéo"                                          ),
      str_detect(A21_cirque, "^1")               ~ factor("6-Faire du cirque"                                                          ),
      str_detect(A21_poterie, "^1")              ~ factor("7-Faire de la poterie, céramique, reliure ou autre artisanat d’art"         ),
      str_detect(A21_theatre, "^1")              ~ factor("8-Faire du théâtre"                                                         ),
      str_detect(A21_dessin, "^1")               ~ factor("9-Faire du dessin"                                                          ),
      str_detect(A21_danse, "^1")                ~ factor("10-Faire de la danse"                                                       ),
      str_detect(A21_photo, "^1")                ~ factor("11-Faire de la photographie"                                                ),
      str_detect(A21_genealogie, "^1")           ~ factor("12-Faire des recherches généalogiques ou historiques"                       ),
      str_detect(A21_activite_scientifique, "^1")~ factor("13-Pratiquer une activité scientifique ou technique"                        ),
      #str_detect(A1914, "^1")  ~ factor("14-Aucune en particulier" )
      
    ) |> 
      fct_recode(
        "01-Musique ou chant"      = "1-Faire de la musique ou du chant"                                          ,
        "02-Journal personnel"     = "2-Tenir un journal intime ou personnel, noter vos impressions ou réflexions",
        "03-Poèmes, littérature"   = "3-Ecrire des poèmes, des nouvelles ou un roman"                             ,
        "04-Peinture, sculpture"   = "4-Faire de la peinture, sculpture ou gravure"                               ,
        "05-Montages audio, vidéo" = "5-Faire des montages audio, vidéo"                                          ,
        "06-Cirque"                = "6-Faire du cirque"                                                          ,
        "07-Artisanat d’art"       = "7-Faire de la poterie, céramique, reliure ou autre artisanat d’art"         ,
        "08-Théâtre"               = "8-Faire du théâtre"                                                         ,
        "09-Dessin"                = "9-Faire du dessin"                                                          ,
        "10-Danse"                 = "10-Faire de la danse"                                                       ,
        "11-Photographie"          = "11-Faire de la photographie"                                                ,
        "12-Histoire"              = "12-Faire des recherches généalogiques ou historiques"                       ,
        "13-Science, technique"    = "13-Pratiquer une activité scientifique ou technique"                        ,
        "14-Aucune"                = "14-Aucune en particulier"                                                   ,
        "NULL"                     = "15-NSP"                                                                     ,
        "NULL"                     = "16-REF"                                                                     ,
      ) |> 
      fct_relevel(sort)
  )
# fct_recode_helper(pc18, PRATIQUE_ARTISTIQUE)

pc18 <- pc18 |>
  mutate(
    ART_MUSIQUE = fct_recode(A1901,   # "new" = "old" 
                             "1-Musique ou chant" = "1-Musique ou chant",
                             "2-Non"              = "2-Non"             
    ),
    
    
    ART_JOURNAL = fct_recode(A1902,   # "new" = "old" 
                             "1-Journal personnel" = "1-Journal personnel, noter impressions ou réflexions",
                             "2-Non"                                                = "2-Non"                                               
    ),
    
    
    ART_LITTERATURE = fct_recode(A1903,   # "new" = "old" 
                                 "1-Écrire littérature" = "1-Ecrire poèmes, nouvelles, roman",
                                 "2-Non"                             = "2-Non"                            
    ),
    
    
    ART_PEINTURE = fct_recode(A1904,   # "new" = "old" 
                              "1-Peindre, sculpter" = "1-Peinture, sculpture ou gravure",
                              "2-Non"                            = "2-Non"                           
    ),
    
    
    ART_MONTAGES = fct_recode(A1905,   # "new" = "old" 
                              "1-Montages audio, vidéo" = "1-Montages audio, vidéo",
                              "2-Non"                   = "2-Non"                  
    ),
    
    
    ART_CIRQUE = fct_recode(A1906,   # "new" = "old" 
                            "1-Cirque" = "1-Cirque",
                            "2-Non"    = "2-Non"   
    ),
    
    
    ART_ARTISANAT = fct_recode(A1907,   # "new" = "old" 
                               "1-Artisanat d'art" = "1-Artisanat d'art",
                               "2-Non"             = "2-Non"            
    ),
    
    
    ART_THEATRE = fct_recode(A1908,   # "new" = "old" 
                             "1-Théâtre" = "1-Théâtre",
                             "2-Non"     = "2-Non"    
    ),
    
    
    ART_DESSIN = fct_recode(A1909,   # "new" = "old" 
                            "1-Dessin" = "1-Dessin",
                            "2-Non"    = "2-Non"   
    ),
    
    
    ART_DANSE = fct_recode(A1910,   # "new" = "old" 
                           "1-Danser" = "1-Sanse",
                           "2-Non"   = "2-Non"  
    ),
    
    
    ART_PHOTO = fct_recode(A1911,   # "new" = "old" 
                           "1-Photographie" = "1-Photographie",
                           "2-Non"          = "2-Non"         
    ),
    
    
    ART_HIST = fct_recode(A1912,   # "new" = "old" 
                          "1-Histoire" = "1-Faire des recherches généalogiques ou historiques",
                          "2-Non"                                               = "2-Non"                                              
    ),
    
    
    ART_SCIENCE_TECH = fct_recode(A1913,   # "new" = "old" 
                                  "1-Science ou technique" = "1-Pratiquer une activité scientifique ou technique (comme observer les étoiles, faire des recherches historiques, etc…)",
                                  "2-Non"                                                                                                                   = "2-Non"                                                                                                                  
    ),
    
    
  )




pc18 <- pc18 |> 
  mutate(
    NB_PRATIQUE_ARTISTIQUE = RECODE_A21 |> replace_na(0L), 
    NB_PRATIQUE_ARTISTIQUE = case_when(
      NB_PRATIQUE_ARTISTIQUE == 0   ~ factor("Activités artistiques : 0 "), 
      NB_PRATIQUE_ARTISTIQUE == 1   ~ factor("1"), 
      NB_PRATIQUE_ARTISTIQUE == 2   ~ factor("2"), 
      NB_PRATIQUE_ARTISTIQUE == 3   ~ factor("3"), 
      NB_PRATIQUE_ARTISTIQUE >= 4   ~ factor("4 ou plus"),
      
    )
    
  )

# tab(pc18, RECODE_A21, wt = POND, pct = "col", na = "drop", color = "diff", ref = 2) |> tab_kable()
# 
# tab(pc18, NB_PRATIQUE_ARTISTIQUE, wt = POND, pct = "col", na = "drop", color = "diff", ref = 2) |> tab_kable()

# A32BIS	# Parmi les activités que vous pratiquez, laquelle a le plus d’importance pour vous ?


pc18 <- pc18 |> 
  mutate(STATUTOT = case_when(                       
    STATUT == "1-Salarié·es de l'État" | STATUTANTE == "1-Salarié.e Etat"                                       ~ factor("1-État"), 
    STATUT == "2-Salarié·es des collectivités locales" | STATUTANTE == "2-Salarié.e collectivité territoriale"  ~ factor("2-Collectivités locales"), 
    STATUT == "3-Salarié·es des entreprises" | STATUTANTE == "4-Salarié.e entreprise (ou asso)"                 ~ factor("3-Entreprises"), 
    STATUT == "4-Salarié·es des particuliers" | STATUTANTE == "5-Salarié.e d'un particulier"                    ~ factor("4-Particuliers"), 
    STATUT == "7-Indépendant·es" | STATUTANTE == "6-À son compte"                                               ~ factor("7-Indépendant·es"), 
  ) |> 
    fct_relevel(sort) #|>
  #fct_na_value_to_level("8-Jamais travaillé")
  )

pc18 <- pc18 |>
  mutate(
    AGE3  =  case_when(
      AGE < 40  ~ factor("1-15 à 39 ans"),
      AGE < 65  ~ factor("2-40 à 64 ans"),
      AGE >=65  ~ factor("3-65 ans et +"),
    )
  )



pc18 <- pc18 |>
  mutate(  
    AGE4 = fct_recode(CRITAGE,   # "new" = "old" 
                      "4-60 ans ou plus" = "4-60 à 74 ans"   ,
                      "4-60 ans ou plus" = "5-75 ans ou plus"
    ),
    DIP4= fct_recode(DIPLOM,   
                     "1-CAP BEP ou -"   = "1-Brevet ou -",
                     "1-CAP BEP ou -"   = "2-CAP BEP"    ,
                     "2-Bac ou bac pro" = "3-Bac pro"    ,
                     "2-Bac ou bac pro" = "4-Bac"        ,
                     "3-Bac+2"          = "5-Bac+2"      ,
                     "4-Bac+3 ou +"     = "6-Licence"    ,
                     "4-Bac+3 ou +"     = "7-Bac+5"      
    ),
    
    TT = case_when(
      S11_C_1 < 35 ~ factor("1-Moins de 35h/semaine"), 
      # S11_C_1 < 30 ~ factor("2-De 20 à 29h"),  
      # S11_C_1 < 35 ~ factor("3-De 30 à 34h"),  
      S11_C_1 < 40 ~ factor("4-De 35h à 39h"),  
      S11_C_1 < 50 ~ factor("5-De 40 à 49h"),  
      S11_C_1 >=50 ~ factor("6-50h ou plus"),  
      ACTEU != "1-En emploi" ~ factor("7-Pas en emploi")
    ),
    
    
    MUSIQUE2 = fct_recode(
      MUSIQUE,   # "new" = "old" 
      "1-Musique: quotidien"     = "1-Musique: quotidien"  ,
      "2-Musique: pas quotidien" = "2-Musique: occasionnel",
      "2-Musique: pas quotidien" = "3-Musique: jamais"     
    ),
    
    LIVRES3 = fct_recode(
      LIVRES,
      "1-Livre: 10+"   = "1-Livre: 20+"  ,
      "1-Livre: 10+"   = "2-Livre: 10-19",
      "2-Livre: 1-9"   = "3-Livre: 1-9"  ,
      "3-Livre: aucun" = "4-Livre: aucun"
    ), 
    
    LIVRES2 = fct_recode(
      LIVRES3,   # "new" = "old" 
      "1-Livre: 10+"      = "1-Livre: 10+"  ,
      "2-Livre: de 0 à 9" = "2-Livre: 1-9"  ,
      "2-Livre: de 0 à 9" = "3-Livre: aucun"
    ),
    
    CINEMA2 = fct_recode(
      CINEMA3,   # "new" = "old" 
      "1-Cinéma: 10+"        = "1-Cinéma: 10+"  ,
      "3-Cinéma: de 0 à 9"   = "3-Cinéma: 1-9"  ,
      "3-Cinéma: de 0 à 9"   = "4-Cinéma: aucun"
    ),
    
    REVENU4 = fct_recode(
      CRITREVENU,   # "new" = "old" 
      "1-< 2500€"    = "1-< 1200€"   ,
      "1-< 2500€" = "2-1200_2499€",
      "2-2500_3999€" = "3-2500_3999€",
      "3-4000_5999€" = "4-4000_5999€",
      "4-6000€+"     = "5-6000€+"    
    ),
    
    PROPRIETAIRE = fct_recode(
      STOC,   # "new" = "old" 
      "1-Locataire"               = "4-Locataire "             ,
      "2-Locataire"               = "5-Logé gratuitement"      ,
      "2-Accédant à la propriété" = "1-Accédant à la propriété",
      "3-Propriétaire"            = "2-Propriétaire"           ,
      "3-Propriétaire"            = "3-Usufruitier"            ,
    ) |> fct_relevel(sort),
    
    VILLE = fct_recode(
      TUU2016,   # "new" = "old" 
      "1-Commune rurale"     = "0-Commune rurale"                                ,
      "2-Ville < 50000 hab"  = "1-Unité urbaine de moins de 5 000 habitants"     ,
      "2-Ville < 50000 hab"  = "2-Unité urbaine de 5 000 à 9 999 habitants"      ,
      "2-Ville < 50000 hab"  = "3-Unité urbaine de 10 000 à 19 999 habitants"    ,
      "2-Ville < 50000 hab"  = "4-Unité urbaine de 20 000 à 49 999 habitants"    ,
      "3-Ville >= 50000 hab" = "5-Unité urbaine de 50 000 à 99 999 habitants"    ,
      "3-Ville >= 50000 hab" = "6-Unité urbaine de 100 000 à 199 999 habitants"  ,
      "3-Ville >= 50000 hab" = "7-Unité urbaine de 200 000 à 1 999 999 habitants",
      "4-Agglomération parisienne" = "8-Unité urbaine de Paris"                        
    ),
    
    AGE11 = case_when(
      AGE <  20 ~ factor("15-15 à 19 ans"), 
      AGE <  25 ~ factor("20-20 à 24 ans"), 
      AGE <  30 ~ factor("25-25 à 29 ans"), 
      AGE <  35 ~ factor("30-30 à 34 ans"), 
      AGE <  40 ~ factor("35-35 à 39 ans"), 
      AGE <  45 ~ factor("40-40 à 44 ans"), 
      AGE <  50 ~ factor("45-45 à 49 ans"), 
      AGE <  55 ~ factor("50-50 à 54 ans"), 
      AGE <  60 ~ factor("55-55 à 59 ans"), 
      AGE <  65 ~ factor("60-60 à 64 ans"), 
      AGE >= 65 ~ factor("65-65 ans et plus"), 
    )
  )

pc18 <- pc18 |> rename(all_of(c(
  COUTURE    = "A1001",	#	Faire du tricot, de la broderie ou de la couture, créer ou personnaliser des vêtements
  JEUX       = "A1002",	#	Jouer aux cartes, à des jeux de société, à des jeux de chiffres ou de lettres
  JEUX_ARGENT= "A1003",	#	Jouer à des jeux d’argent ou parier (jeux à gratter, Loto, belote, PMU, poker, casino…)
  BONS_PLATS = "A1004",	#	Faire de « bons plats » ou essayer de nouvelles recettes de cuisine
  BRICOLAGE  = "A1005",	#	Faire vous-même des travaux de bricolage ou de décoration
  POTAGER    = "A1006",	#	Vous occuper d'un jardin potager
  FLEURS     = "A1007",	#	Vous occuper d'un jardin d'agrément (fleurs, pelouse)
  CHASSE     = "A1008",	#	Aller à la pêche ou à la chasse
  COLLECTION = "A1009",	#	Faire une collection
  TUNING     = "A1010" 	#	Personnaliser/customiser un véhicule (voiture, moto, mobylette)
)))

# Quels sont les genres de films que vous regardez ?
pc18 <- pc18 |> 
  rename(all_of(c(
    COMIQUE      = "C2601", 
    ACTION       = "C2602", 
    HISTO_BIOPIC = "C2603", 
    POL_THRILL   = "C2604", 
    AVENTURE     = "C2605", 
    DRAME        = "C2606", 
    ANIMATION    = "C2607", 
    HORREUR      = "C2608", 
    AUTEUR       = "C2609", 
    DOCU         = "C2610", 
    WESTERNS     = "C2611", 
    EROTIQUE     = "C2612", 
    SF_FANTASY   = "C2613",
    AMOUR        = "C2614", 
    MUSICAL      = "C2615", 
    POLITIQUE    = "C2616"
  )))

pc18 <- pc18 |> 
  mutate( 
    ENF_CINE = fct_recode(
      M1_SQ5,   # "new" = "old" 
      "1-Enfance: jamais cinéma"   = "4-Cinéma jamais"           ,
      "2-Cinéma rarement"          = "3-Cinéma rarement"         ,
      "3-Cinéma de temps en temps" = "2-Cinéma de temps en temps",
      "4-Cinéma souvent"           = "1-Enfance: souvent cinéma" ,
    ) |> fct_relevel(sort),
    
    ENF_MUSEE = fct_recode(
      M1_SQ8,   # "new" = "old" 
      "1-Enfance: jamais musée"   = "4-Musée jamais"           ,
      "2-Musée rarement"          = "3-Musée rarement"         ,
      "3-Musée de temps en temps" = "2-Musée de temps en temps",
      "4-Musée souvent"           = "1-Enfance: souvent musée" ,
    ) |> fct_relevel(sort),
    
  )




pc18 <- pc18 |> 
  rename(
    MUS_CLASSIQUE	= H301, # Peinture, sculpture (Antiquité-début 20ème siècle)
    MUS_MODERNE		= H302, # Art moderne ou contemporain
    MUS_HISTOIRE	= H303, # Histoire, mémoire
    MUS_ARCHEO		= H304, # Préhistoire, archéologie
    MUS_SCIENCE		= H305, # Sciences et techniques, histoire naturelle, industrie
    MUS_ETHNO		  = H306, # Ethnographie, artisanat, société
    MUS_ARCHI		  = H307, # Architecture, design, arts décoratifs
    MUS_AUCUN		  = H308, # Aucun de ces lieux
  )







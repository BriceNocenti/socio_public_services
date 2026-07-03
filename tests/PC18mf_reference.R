# ============================================================
# PC18 — Variables recodées par rapport à la base originale
# Enquête Pratiques culturelles 2018 (Ministère de la Culture)
# ============================================================
#
# Ce fichier liste :
#   1. Les variables de la base RENOMMÉES par rapport au questionnaire original
#   2. Les nouvelles variables CRÉÉES (recodages, agrégations, calculs)
#
# Vous pouvez copier-coller directement les noms qui vous intéressent.
# ============================================================


# ============================================================
# 1. Variables RENOMMÉES par rapport au dictionnaire des codes
# ============================================================

vars_renommees <- c(

  # --- Médias ---
  "TELE",        # C1      : Fréquence de visionnage de la télévision
  "JV",          # B2      : Fréquence de jeu vidéo
  "RADIO",       # E1      : Fréquence d'écoute de la radio
  "VIDEOS",      # C15     : Fréquence de visionnage de vidéos en ligne (streaming, replay...)
  "RESEAUX",     # I6      : Fréquence d'utilisation des réseaux sociaux

  # --- Spectacles vivants (oui/non dans les 12 derniers mois)  ---
  "CLASSIQUE",   # G2512   : Aller à un concert de musique classique
  "POP_ROCK",    # G2509   : Aller à un concert de pop, rock
  "JAZZ",        # G2510   : Aller à un concert de jazz
  "CHANSON",      # A assisté à un concert de chansons ou variétés françaises
  "WORLD",        # A assisté à un concert de musiques du monde
  "TRADI",        # A assisté à un concert de musiques traditionnelles
  "VARIETE",      # A assisté à un concert de variétés internationales
  "RNB",          # A assisté à un concert de RnB
  "ELECTRO",      # A assisté à un concert de musiques électroniques / techno
  "RAP",          # A assisté à un concert de hip-hop / rap
  "METAL",        # A assisté à un concert de métal / hard rock
  "POP_ROCK_JAZZ",# A assisté à un concert de pop, rock, jazz ou métal (variable agrégée)

  # --- Sorties culturelles (oui/non dans les 12 derniers mois) ---
  "THEATRE",     # G134    : Aller voir une pièce de théâtre (y c. one man show, impro)
  "CIRQUE",      # G132    : Aller à un spectacle de cirque
  "MUSEE_EXPO",  # H210    : Aller dans un musée ou une exposition
  "MONUMENT",    # H209    : Visiter un monument historique
  "DANSE",        # A assisté à un spectacle de danse (classique, modern jazz ou contemporaine)


  # --- Loisirs domestiques (oui/non dans les 12 derniers mois) ---
  "COUTURE",     # A1001   : Faire du tricot, de la broderie ou de la couture
  "JEUX",        # A1002   : Jouer aux cartes, jeux de société, jeux de chiffres/lettres
  "JEUX_ARGENT", # A1003   : Jouer à des jeux d'argent ou parier (Loto, PMU, poker...)
  "BONS_PLATS",  # A1004   : Faire de « bons plats » ou essayer de nouvelles recettes
  "BRICOLAGE",   # A1005   : Faire des travaux de bricolage ou de décoration
  "POTAGER",     # A1006   : S'occuper d'un jardin potager
  "FLEURS",      # A1007   : S'occuper d'un jardin d'agrément (fleurs, pelouse)
  "CHASSE",      # A1008   : Aller à la pêche ou à la chasse
  "COLLECTION",  # A1009   : Faire une collection
  "TUNING",      # A1010   : Personnaliser/customiser un véhicule (voiture, moto...)

  # --- Pratiques artistiques amateurs (binaire : pratique oui/non) ---
  "ART_MUSIQUE",      # Faire de la musique ou du chant
  "ART_JOURNAL",      # Tenir un journal intime ou personnel
  "ART_LITTERATURE",  # Écrire des poèmes, nouvelles ou un roman
  "ART_PEINTURE",     # Faire de la peinture, sculpture ou gravure
  "ART_MONTAGES",     # Faire des montages audio ou vidéo
  "ART_CIRQUE",       # Faire du cirque
  "ART_ARTISANAT",    # Faire de la poterie, céramique, reliure ou artisanat d'art
  "ART_THEATRE",      # Faire du théâtre
  "ART_DESSIN",       # Faire du dessin
  "ART_DANSE",        # Faire de la danse
  "ART_PHOTO",        # Faire de la photographie
  "ART_HIST",         # Faire des recherches généalogiques ou historiques
  "ART_SCIENCE_TECH", # Pratiquer une activité scientifique ou technique

  # --- Genres de films regardés (oui/non) ---
  "COMIQUE",      # C2601  : Films comiques
  "ACTION",       # C2602  : Films d'action
  "HISTO_BIOPIC", # C2603  : Films historiques ou biopics
  "POL_THRILL",   # C2604  : Films policiers ou thrillers
  "AVENTURE",     # C2605  : Films d'aventure
  "DRAME",        # C2606  : Drames
  "ANIMATION",    # C2607  : Films d'animation
  "HORREUR",      # C2608  : Films d'horreur
  "AUTEUR",       # C2609  : Films d'auteur
  "DOCU",         # C2610  : Documentaires
  "WESTERNS",     # C2611  : Westerns
  "EROTIQUE",     # C2612  : Films érotiques
  "SF_FANTASY",   # C2613  : Science-fiction ou fantasy
  "AMOUR",        # C2614  : Films d'amour / romantiques
  "MUSICAL",      # C2615  : Comédies musicales
  "POLITIQUE",    # C2616  : Films politiques

  # --- Types de musées ou expositions fréquentés (oui/non) ---
  "MUS_CLASSIQUE", # H301  : Peinture, sculpture (Antiquité – début 20e siècle)
  "MUS_MODERNE",   # H302  : Art moderne ou contemporain
  "MUS_HISTOIRE",  # H303  : Histoire, mémoire
  "MUS_ARCHEO",    # H304  : Préhistoire, archéologie
  "MUS_SCIENCE",   # H305  : Sciences et techniques, histoire naturelle, industrie
  "MUS_ETHNO",     # H306  : Ethnographie, artisanat, société
  "MUS_ARCHI",     # H307  : Architecture, design, arts décoratifs
  "MUS_AUCUN",     # H308  : Aucun de ces types de lieux

  # --- Sociodémographiques --- 
  "HH" #,          # S10_C_1 : Heures de travail par semaine (déclarées)
)


# ============================================================
# 2. Variables CRÉÉES (recodages, agrégations, constructions)
# ============================================================

vars_creees <- c(

  # --- Musique (écoute) ---
  "MUSIQUE",     # Fréquence d'écoute musicale : quotidien / occasionnel / jamais (hors radio seule)
  "MUSIQUE_B",   # Idem, variante : exclut aussi ceux qui n'écoutent que via la télévision
  "MUSIQUE2",    # Version binaire : quotidien vs. non quotidien

  # --- Livres ---
  "LIVRES",      # Nombre de livres lus dans l'année : aucun / 1-9 / 10-19 / 20+
  "LIVRES3",     # Version 3 modalités : 10+ / 1-9 / aucun
  "LIVRES2",     # Version binaire : 10+ vs. de 0 à 9

  # --- Cinéma ---
  "nb_cine",     # Nombre de séances de cinéma par an (variable numérique calculée)
  "CINEMA",      # Fréquentation cinéma : aucun / 1-2 / 3-11 / 12+ séances/an
  "CINEMA3",     # Version 3 modalités : aucun / 1-9 / 10+
  "CINEMA2",     # Version binaire : 10+ vs. de 0 à 9


  # --- Patrimoine ---
  "NB_MUSEE_EXPO", # Nombre de visites de musées ou expositions par an (numérique)
  "MUSEE_EXPO_4",  # Fréquentation des musées : jamais / 1 / 2-4 / 5+

  # --- Télévision ---
  "TELE5",        # Fréquence TV recodée en 5 modalités : quotidien / 3-4j / 1-2j / rarement / jamais
  "SERIES_AVEC",  # Contexte de visionnage des séries : avec enfants / en couple / seul ou amis / autres / jamais

  # --- Spectacles vivants ---
  "FESTI_MUSIQUE",# A assisté à un festival de musique dans l'année
  "NB_CONCERTS",  # Nombre total de concerts par an (variable numérique calculée)
  "CONCERTS",     # Nombre de concerts : aucun / 1 / 2-4 / 5+
  "THEATRE4",     # Nombre de fois au théâtre : jamais / 1 / 2 / 3+

  # --- Pratiques artistiques amateurs (binaire : pratique oui/non) ---
  "PRATIQUE_ARTISTIQUE",   # Activité artistique la plus importante pour l'enquêté·e
  "NB_PRATIQUE_ARTISTIQUE",# Nombre d'activités artistiques amateurs : 0 / 1 / 2 / 3 / 4+

  # --- Sociodémographie ---
  "AGE3",         # Âge en 3 tranches : 15-39 / 40-64 / 65+
  "AGE4",         # Âge en 4 tranches : recodage de CRITAGE (60-74 et 75+ regroupés)
  "AGE11",        # Âge en 11 tranches quinquennales (de 15-19 à 65+)
  "DIP4",         # Diplôme en 4 niveaux : CAP BEP ou - / Bac ou bac pro / Bac+2 / Bac+3+
  "REVENU4",      # Revenu du ménage en 4 tranches : <2500€ / 2500-3999€ / 4000-5999€ / 6000€+
  "PROPRIETAIRE", # Statut d'occupation du logement : locataire / accédant / propriétaire
  "VILLE",        # Taille de commune : rurale / <50 000 hab / ≥50 000 hab / aggl. parisienne
  "TT",           # Temps de travail : <35h / 35-39h / 40-49h / 50h+ / pas en emploi
  "STATUTOT",     # Statut de l'employeur : État / collectivités locales / entreprise / particulier / indépendant

  # --- Enfants dans le logement ---
  "NB_ENF",       # Nombre d'enfants mineurs dans le logement (factor : 0 / 1 / 2 / 3 / 4+)
  "NB_ENF2",      # Présence de mineurs dans le logement : oui / non

  # --- Nationalité et immigration ---
  "NATIO2",        # Nationalité détaillée : né·e français·e / naturalisé·e / étranger·e
  "NATIO",         # Nationalité simplifiée : français·e / étranger·e
  "IMMI",          # Statut migratoire : né·e français·e / immigré·e
  "NATIO_PARENTS", # Nationalité des parents : 0 / 1 / 2 parents nés français
  "NATIO_PARENTS2",# Croisement lieu de naissance × nationalité des parents (5 modalités)
  "LANGUE_MERE",   # Langue parlée par la mère dans l'enfance : étrangère / français ou autre
  "LANGUE_PERE",   # Langue parlée par le père dans l'enfance : étrangère / français ou autre
  "LANGUE_PARENTS",# Nombre de parents de langue étrangère : 0 / 1 / 2
  "ANARRIV",       # Année d'arrivée en France (pour les immigré·es, numérique)
  "NB_ANARRIV",    # Nombre d'années depuis l'arrivée en France
  "ANARRIV_TR",    # Tranche d'année d'arrivée en France (6 modalités : avant 1970, 1970-1979...)
  "ANARRIV_TR3",   # Tranche d'année d'arrivée en France (3 modalités : avant 1970 / 1970-1999 / 2000+)
  "AGARRIV",       # Âge à l'arrivée en France (numérique)
  "AGARRIV_TR",    # Tranche d'âge à l'arrivée (5 modalités : <6 ans / 6-16 / 17-24 / 25-39 / 40+)
  "AGARRIV_TR3",   # Tranche d'âge à l'arrivée (3 modalités : <17 ans / 17-24 / 25+)

  # --- Origine sociale et mobilité intergénérationnelle ---
  "DIPLOME_PERE",  # Diplôme du père : aucun / CAP BEP / bac pro / bac / bac+2 / licence / bac+5 / NSP
  "DIPLOME_MERE",  # Diplôme de la mère : aucun / CAP BEP / bac pro / bac / bac+2 / licence / bac+5 / NSP
  "CSTOTR_MOB",    # Mobilité sociale intergénérationnelle : ascension / reproduction / déclassement / indécidable

  # --- Transmission culturelle musicale (rapport à la musique des parents) ---
  "NB_MUS_PARENTS",           # Nombre de styles musicaux écoutés par les parents (numérique)
  "PCT_MEME_MUS_PARENTS_num", # Part des styles des parents que l'enquêté·e écoute aussi (0 à 1)
  "PCT_MEME_MUS_PARENTS",     # Idem en factor : aucun / ≤50% / 55-90% / tous / parents sans musique
  "NB_MUS_PAS_PARENTS_num",   # Nombre de styles écoutés en plus que les parents (numérique)
  "NB_MUS_PAS_PARENTS",       # Idem en factor : 0 / 1-2 / 3-5 / 6-12 styles supplémentaires

  # --- Pratiques culturelles dans l'enfance ---
  "ENF_CINE",  # Fréquence des sorties au cinéma dans l'enfance : jamais / rarement / de temps en temps / souvent
  "ENF_MUSEE"  # Fréquence des visites de musées dans l'enfance : jamais / rarement / de temps en temps / souvent

)

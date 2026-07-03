# ============================================================
# PC18 — Variables originales de la base
# Enquête Pratiques culturelles 2018 (Ministère de la Culture)
# ============================================================
#
# Variables classées par thème (colonne « module » du questionnaire).
# Libellé de la question en commentaire inline.
# ============================================================


# ============================================================
# Sociodémo
# ============================================================

vars_sociodemo <- c(
  "IDENT18"            , # Identifiant de l'enquête
  "POND_INIT"          , # Pondération initiale
  "POND"               , # Pondération finale (issue du calage sur marges)
  "TYPMEN"             , # TYPMEN
  "NHAB"               , # En vous comptant, combien de personnes vivent habituellement dans ce logement, même si ce...
  "TUU2016"            ,
  "SEXE"               , # Sexe du répondant
  "AGE"                , # Age du répondant
  "CRITAGE"            , # Tranche d'âge
  "AUTRENF"            , # Avez-vous des enfants, même s’ils ne vivent pas habituellement avec vous dans ce logement...
  "PETITENF"           , # Etes-vous grand-parent ?
  "LNAIS"              , # Lieu de naissance : êtes-vous né…
  "REVENU"             , # Le répondant a donné une valeur exacte à la question "Quel est, au total, le revenu mensue...
  "TRANREV"            , # A défaut de donner une valeur exacte, pouvez-vous, néanmoins, le situer dans une des tranc...
  "CRITREVENU"         , # Tranche de revenu consolidée (les valeurs exactes ont été reventilées dans les tranches co...
  "CLASS_univprat"     , # Classe d'univers de pratiques (typologie réalisée dans le cadre de la publication "Cinquan...
  "CLASS_univprat_name", # Nom des classes
  "A15"                , # Comment est votre état de santé en général ?
  "A16"                , # Avez-vous une maladie ou un problème de santé qui soit chronique ou de caractère durable ?
  "A17"                , # Êtes-vous limité(e), depuis au moins 6 mois, à cause d’un problème de santé, dans les acti...
  "A18_SQ1"            , # Avez-vous des difficultés… ...à voir, même quand vous portez vos lunettes ?
  "A18_SQ2"            , # Avez-vous des difficultés… ...à entendre, même quand vous portez une aide auditive ?
  "A18_SQ3"            , # Avez-vous des difficultés… ...à marcher ou à monter un escalier ?
  "A18_SQ4"            , # Avez-vous des difficultés… ...de mémoire ou de concentration ?
  "VITENCOUPLE"        , # Le répondant vit en couple (variable construite avec COUPLE_X et NOIKISH)
  "NOIKISH"            , # Numéro d'identification du répondant (correspond à la position de l'individu parmi l'ensem...
  "NOICONJ"            , # Numéro d'identification du conjoint du répondant (correspond à la position de l'individu p...
  "G_PCS_MENAGE_"      , # Groupe de PCS du ménage
  "SG_PCS_MENAGE_"     , # Sous-groupe de la PCS du ménage
  "I_PCS_MENAGE"       , # Indicatrice dichotomique qui signale si la construction de la variable à eu lieu à partir...
  "SEXE_pers1"         , # Sexe de la personne X
  "ANAIS_pers1"        , # Année de naissance de la personne X
  "AGE_pers1"          , # Age de la personne X
  # etc. 
)

# ============================================================
# Loisirs
# ============================================================

vars_loisirs <- c(
  "A1"   , # D'une manière générale pour vos loisirs, préférez vous plutôt …
  "A2"   , # Vous arrive-t-il d’avoir le sentiment de manquer de temps libre pour faire tout ce dont vo...
  "A3"   , # Au cours des 12 derniers mois, êtes-vous parti.e en week-end ? (entre une et trois nuits h...
  "A4"   , # Combien de fois êtes-vous parti.e en week-end au cours de ces 12 derniers mois ?
  "A5"   , # Au cours des 12 derniers mois, êtes-vous parti.e en vacances, au moins quatre nuits conséc...
  "A6"   , # Combien de fois êtes-vous parti.e en vacances au cours de ces 12 derniers mois ?
  "A71"  , # Oui, en Europe
  "A72"  , # Oui, en dehors de l’Europe
  "A73"  , # Non
  "A74"  , # (NSP)
  "A75"  , # (REF)
  "A8"   , # En moyenne, à quelle fréquence sortez-vous, que ce soit en famille, avec des parents, des...
  "A9"   , # En moyenne, à quelle fréquence recevez-vous chez vous, que ce soit des parents, des amis,...
  "A1001", # Faire du tricot, de la broderie ou de la couture, créer ou personnaliser des vêtements
  "A1002", # Jouer aux cartes, à des jeux de société, à des jeux de chiffres ou de lettres
  "A1003", # Jouer à des jeux d’argent ou parier (jeux à gratter, Loto, belote, PMU, poker, casino…)
  "A1004", # Faire de « bons plats » ou essayer de nouvelles recettes de cuisine
  "A1005", # Faire vous-même des travaux de bricolage ou de décoration
  "A1006", # Vous occuper d'un jardin potager
  "A1007", # Vous occuper d'un jardin d'agrément (fleurs, pelouse)
  "A1008", # Aller à la pêche ou à la chasse
  "A1009", # Faire une collection
  "A1010", # Personnaliser/customiser un véhicule (voiture, moto, mobylette)
  "A1011", # Aucune de ces activités
  "A1012", # (NSP)
  "A1013", # (REF)
  "A1101", # Jeux de cartes et de dés (tarot, belote, poker, yam's…)
  "A1102", # Jeux de logique, de déduction et de stratégie (échecs, dames, Rubik's cube, Les aventurier...
  "A1103", # Jeux d'action et d'adresse (mikado, Baby-foot, flipper, billard…)
  "A1104", # Jeux de mémoire et d'observation (Le lynx, Dobble, Uno…)
  "A1105", # Jeux de lettres, chiffres et mots (Mots croisés, fléchés, sudoku, scrabble, domino…)
  "A1106", # Jeux de culture générale, quizz et devinettes (Trivial pursuit, Family Quizz, Eureka…)
  "A1107", # Jeux d'émission TV (Questions pour un champion, qui veut gagner des millions, le juste pri...
  "A1108", # Jeux de rapidité (Times'up, Jungle speed…)
  "A1109", # Jeux de coopération et de rôles (Loups Garous, Conan, Edge of the empire…)
  "A1110", # Jeux de cartes à collectionner (Pokémon, Le trône de fer…)
  "A1111", # Autres jeux
  "A1112", # (NSP)
  "A1113", # (REF)
  "A12"  , # Au cours des 12 derniers mois, avez-vous pratiqué une activité physique ou sportive ?
  "A13"    # A quelle fréquence ?
)

# ============================================================
# Amateur
# ============================================================

vars_amateur <- c(
  "A1901"                     , # Faire de la musique ou du chant
  "A1902"                     , # Tenir un journal intime ou personnel, noter vos impressions ou réflexions
  "A1903"                     , # Ecrire des poèmes, des nouvelles ou un roman
  "A1904"                     , # Faire de la peinture, sculpture ou gravure
  "A1905"                     , # Faire des montages audio, vidéo
  "A1906"                     , # Faire du cirque
  "A1907"                     , # Faire de la poterie, céramique, reliure ou autre artisanat d'art
  "A1908"                     , # Faire du théâtre
  "A1909"                     , # Faire du dessin
  "A1910"                     , # Faire de la danse
  "A1911"                     , # Faire de la photographie
  "A1912"                     , # Faire des recherches généalogiques ou historiques
  "A1913"                     , # Pratiquer une activité scientifique ou technique (comme observer les étoiles, faire des re...
  "A1914"                     , # (Aucune des activités cités)
  "A1915"                     , # (NSP)
  "A1916"                     , # (REF)
  "RECODE_A21"                , # Nombre d'activités réalisées dans les 12 derniers mois
  "A20_musique"               , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_musique"               , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_musique"               , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_musique"               , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_musique"              , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_musique"              , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_musique"              , # … des cours sur internet ?
  "A244_musique"              , # … autre ?
  "A245_musique"              , # NSP
  "A246_musique"              , # REF
  "A25_musique"               , # Avez-vous pris des cours dans le passé ?
  "A261_musique"              , # … de pratique instrumentale seul (soliste)
  "A262_musique"              , # … de pratique instrumentale en groupe
  "A263_musique"              , # … de chant choral
  "A264_musique"              , # … de chant individuel
  "A265_musique"              , # … de composition sur ordinateur
  "A266_musique"              , # NSP
  "A267_musique"              , # REF
  "A27_musique"               ,
  "A28_musique"               ,
  "A2901_musique"             , # Chansons ou variétés françaises
  "A2902_musique"             , # Musiques du monde
  "A2903_musique"             , # Musiques traditionnelles
  "A2904_musique"             , # Variétés internationales
  "A2905_musique"             , # RnB
  "A2906_musique"             , # Musiques électroniques, techno
  "A2907_musique"             , # Hip-hop, rap
  "A2908_musique"             , # Metal, hard-rock
  "A2909_musique"             , # Pop, rock
  "A2910_musique"             , # Jazz
  "A2911_musique"             , # Opera
  "A2912_musique"             , # Musique classique
  "A2913_musique"             , # Autre genre
  "A20_journal"               , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_journal"               , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_journal"               , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_journal"               , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_journal"              , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_journal"              , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_journal"              , # … des cours sur internet ?
  "A244_journal"              , # … autre ?
  "A245_journal"              , # NSP
  "A246_journal"              , # REF
  "A25_journal"               , # Avez-vous pris des cours dans le passé ?
  "A20_romans"                , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_romans"                , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_romans"                , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_romans"                , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_romans"               , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_romans"               , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_romans"               , # … des cours sur internet ?
  "A244_romans"               , # … autre ?
  "A245_romans"               , # NSP
  "A246_romans"               , # REF
  "A25_romans"                , # Avez-vous pris des cours dans le passé ?
  "A20_peinture"              , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_peinture"              , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_peinture"              , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_peinture"              , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_peinture"             , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_peinture"             , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_peinture"             , # … des cours sur internet ?
  "A244_peinture"             , # … autre ?
  "A245_peinture"             , # NSP
  "A246_peinture"             , # REF
  "A25_peinture"              , # Avez-vous pris des cours dans le passé ?
  "A20_montages"              , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_montages"              , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_montages"              , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_montages"              , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_montages"             , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_montages"             , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_montages"             , # … des cours sur internet ?
  "A244_montages"             , # … autre ?
  "A245_montages"             , # NSP
  "A246_montages"             , # REF
  "A25_montages"              , # Avez-vous pris des cours dans le passé ?
  "A20_cirque"                , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_cirque"                , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_cirque"                , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_cirque"                , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_cirque"               , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_cirque"               , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_cirque"               , # … des cours sur internet ?
  "A244_cirque"               , # … autre ?
  "A245_cirque"               , # NSP
  "A246_cirque"               , # REF
  "A25_cirque"                , # Avez-vous pris des cours dans le passé ?
  "A20_poterie"               , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_poterie"               , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_poterie"               , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_poterie"               , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_poterie"              , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_poterie"              , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_poterie"              , # … des cours sur internet ?
  "A244_poterie"              , # … autre ?
  "A245_poterie"              , # NSP
  "A246_poterie"              , # REF
  "A25_poterie"               , # Avez-vous pris des cours dans le passé ?
  "A20_theatre"               , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_theatre"               , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_theatre"               , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_theatre"               , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_theatre"              , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_theatre"              , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_theatre"              , # … des cours sur internet ?
  "A244_theatre"              , # … autre ?
  "A245_theatre"              , # NSP
  "A246_theatre"              , # REF
  "A25_theatre"               , # Avez-vous pris des cours dans le passé ?
  "A20_dessin"                , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_dessin"                , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_dessin"                , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_dessin"                , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_dessin"               , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_dessin"               , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_dessin"               , # … des cours sur internet ?
  "A244_dessin"               , # … autre ?
  "A245_dessin"               , # NSP
  "A246_dessin"               , # REF
  "A25_dessin"                , # Avez-vous pris des cours dans le passé ?
  "A20_danse"                 , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_danse"                 , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_danse"                 , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_danse"                 , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_danse"                , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_danse"                , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_danse"                , # … des cours sur internet ?
  "A244_danse"                , # … autre ?
  "A245_danse"                , # NSP
  "A246_danse"                , # REF
  "A25_danse"                 , # Avez-vous pris des cours dans le passé ?
  "A3001_danse"               , # Hip-hop, rap
  "A3002_danse"               , # Latino
  "A3003_danse"               , # Salon
  "A3004_danse"               , # Modern jazz
  "A3005_danse"               , # Classique
  "A3006_danse"               , # Contemporain
  "A3007_danse"               , # Traditionnelle (y compris orientale, africaine)
  "A3008_danse"               , # Autre
  "A3009_danse"               , # NSP
  "A3010_danse"               , # REF
  "A20_photo"                 , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_photo"                 , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_photo"                 , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_photo"                 , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_photo"                , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_photo"                , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_photo"                , # … des cours sur internet ?
  "A244_photo"                , # … autre ?
  "A245_photo"                , # NSP
  "A246_photo"                , # REF
  "A25_photo"                 , # Avez-vous pris des cours dans le passé ?
  "A20_genealogie"            , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_genealogie"            , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_genealogie"            , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_genealogie"            , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_genealogie"           , # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_genealogie"           , # … des cours particuliers à domicile ou chez un professeur ?
  "A243_genealogie"           , # … des cours sur internet ?
  "A244_genealogie"           , # … autre ?
  "A245_genealogie"           , # NSP
  "A246_genealogie"           , # REF
  "A25_genealogie"            , # Avez-vous pris des cours dans le passé ?
  "A20_activite_scientifique" , # A quel âge avez-vous commencé à pratiquer cette activité ?
  "A21_activite_scientifique" , # Avez-vous pratiqué cette activité au cours des 12 derniers mois ?
  "A22_activite_scientifique" , # A quel âge avez-vous cessé de pratiquer cette activité ?
  "A23_activite_scientifique" , # Prenez-vous actuellement des cours y-compris sur Internet pour apprendre à pratiquer cette...
  "A241_activite_scientifique", # … dans une structure spécialisée, conservatoire, école de musique, de danse, d'art dramati...
  "A242_activite_scientifique", # … des cours particuliers à domicile ou chez un professeur ?
  "A243_activite_scientifique", # … des cours sur internet ?
  "A244_activite_scientifique", # … autre ?
  "A245_activite_scientifique", # NSP
  "A246_activite_scientifique", # REF
  "A25_activite_scientifique" , # Avez-vous pris des cours dans le passé ?
  "A311"                      , # Pour vous former, apprendre
  "A312"                      , # Pour produire, créer
  "A313"                      , # Pour diffuser, faire connaître ce que vous faites, partager avec des proches ou des amis
  "A314"                      , # Vous n’utilisez aucun outil numérique
  "A315"                      , # (NSP)
  "A316"                      , # (REF)
  "A32"                       , # Déposez-vous vos productions sur un blog, un réseau social ou une plateforme spécialisée p...
  "A32BIS"                    , # Parmi les activités que vous pratiquez, laquelle a le plus d’importance pour vous ?
  "A33"                         # Si vous ne pouviez plus pratiquer cette activité, est ce que cela vous manquerait ?
)

# ============================================================
# Jeux-vidéo
# ============================================================

vars_jeux_video <- c(
  "B1"  , # Au cours des 12 derniers mois, avez-vous joué chez vous ou ailleurs à des jeux vidéo, 
que...
  "B2"  , # En général, à quelle fréquence jouez-vous à des jeux vidéo ?
  "B3"  , # Au cours des 12 derniers mois, vous est-il arrivé de jouer à des jeux en ligne, sur Intern...
  "B401", # Seul(e)
  "B402", # En couple
  "B403", # Avec vos enfants
  "B404", # Avec vos petits-enfants
  "B405", # Avec des proches (parents, grands-parents, frères, sœurs…)
  "B406", # Avec un ou des amis
  "B407", # En groupe organisé (à l’avance)
  "B408", # (Pas de règle générale)
  "B409", # (NSP)
  "B410", # (REF)
  "B5"  , # Au cours des 12 derniers mois, avez-vous joué à des jeux contre ou avec d’autres joueurs c...
  "B601", # Jeux de cartes, d'échecs, de société (poker, tarot, solitaires, échecs, scrabble, Monopoly...
  "B602", # Jeux d’argent en ligne (Loto, PMU, poker, casino, paris…)
  "B603", # Jeux de réflexion, d’énigmes, de quizz (Démineur, Professeur Layton, Docteur Kawashima…)
  "B604", # Jeux d’action/aventure et de rôle (Super Mario, Zelda, Assassin’s Creed, Final Fantasy…)
  "B605", # Jeux d’adresse et de puzzle (Angry Birds, Tétris, Snake, Fruit Ninja, Candy Crush…)
  "B606", # Jeux de combat et de tir / FPS (Doom, Street Fighter, Tekken, Call of Duty…)
  "B607", # Jeux de sport, course, pilotage (FIFA, Football Manager, Gran turismo, Super Mario Kart…)
  "B608", # Jeux musicaux/danse (Just dance, Singstar, Guitar Hero…)
  "B609", # Jeux de gestion (Farmville, Sim City, Les Sims…)
  "B610", # Jeux de stratégie et d’arène (Command & Conquer, Age of Empires, Civilization, Clash of Cl...
  "B611", # Jeux massivement multi-joueurs (World of Warcraft, Dofus,…)
  "B612", # Autres
  "B613", # (NSP)
  "B614", # (REF)
  "B7"    # Si vous ne pouviez plus jouer aux jeux vidéo, cela vous manquerait
)

# ============================================================
# Vidéo
# ============================================================

vars_video <- c(
  "C1"        , # Regardez vous la télévision, que ce soit chez vous ou ailleurs, chez des parents, des amis...
  "C2"        , # Vous arrive t il d'allumer la télévision en rentrant chez vous, sans connaître le programm...
  "C301"      , # Seul(e)
  "C302"      , # En couple
  "C303"      , # Avec vos enfants
  "C304"      , # Avec vos petits-enfants
  "C305"      , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "C306"      , # Avec un ou des amis
  "C307"      , # En groupe organisé
  "C308"      , # (Pas de règle générale)
  "C309"      , # (NSP)
  "C310"      , # (REF)
  "C4"        ,
  "C5"        ,
  "C61"       , # Un écran de télévision
  "C62"       , # Un ordinateur
  "C63"       , # Une tablette
  "C64"       , # Un smartphone
  "C65"       , # Sur écran avec un vidéoprojecteur
  "C66"       , # (NSP)
  "C67"       , # (REF)
  "C701"      , # Des films
  "C702"      , # Des séries, feuilletons
  "C703"      , # Des émissions d’informations
  "C704"      , # Des jeux, des programmes de divertissement
  "C705"      , # Des émissions de télé-réalité
  "C706"      , # Des talks show, émissions satiriques, humour
  "C707"      , # Des émissions sur le sport, matchs, compétitions sportives
  "C708"      , # Des émissions pour enfants
  "C709"      , # Des reportages, documentaires
  "C710"      , # Des vidéo-clips, émissions musicales
  "C711"      , # Autres
  "C712"      , # (NSP)
  "C713"      , # (REF)
  "C8"        , # Regardez-vous des chaînes étrangères non-francophones ?
  "C9"        ,
  "C9_RECODE" ,
  "C10"       , # Valeur déclarée par le répondant
  "C10_C_1"   , # Valeur déclarée
  "C11"       , # Unité choisie pour la valeur déclarée
  "C12"       ,
  "C12_C_1"   , # Valeur déclarée
  "C13"       , # Unité choisie pour la valeur déclarée
  "C14"       , # Si vous ne pouviez plus regarder la télévision, est ce que cela vous manquerait ?
  "C15"       , # Regardez-vous des vidéos sur Internet, qu'il s'agisse de vidéos diffusées par les réseaux...
  "C161"      , # un écran de télévision
  "C162"      , # un ordinateur
  "C163"      , # une tablette
  "C164"      , # un smartphone
  "C165"      , # sur écran avec un vidéoprojecteur
  "C166"      , # (NSP)
  "C167"      , # (REF)
  "C1701"     , # Des films
  "C1702"     , # Des séries, feuilletons
  "C1703"     , # Des bandes annonce
  "C1704"     , # Des vidéo-clips, émissions musicales
  "C1705"     , # Des vidéos d’informations
  "C1706"     , # Des vidéos sur le sport, matchs, compétitions sportives
  "C1707"     , # Des chaînes de vidéos animées par des internautes (youtubeurs, etc.)
  "C1708"     , # Des vidéos pour enfants
  "C1709"     , # Des reportages, documentaires
  "C1710"     , # Des vidéos sur des jeux
  "C1711"     , # Autres
  "C1712"     , # (NSP)
  "C1713"     , # (REF)
  "C18"       , # Si vous ne pouviez plus regarder de vidéos sur Internet, est-ce que cela vous manquerait ?
  "C19"       , # Au cours des 12 derniers mois, à quelle fréquence en moyenne avez-vous regardé des films ?
  "C2001"     , # Seul(e)
  "C2002"     , # En couple
  "C2003"     , # Avec vos enfants
  "C2004"     , # Avec vos petits-enfants
  "C2005"     , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "C2006"     , # Avec un ou des amis
  "C2007"     , # En groupe organisé
  "C2008"     , # (Pas de règle générale)
  "C2009"     , # (NSP)
  "C2010"     , # (REF)
  "C211"      , # Un écran de télévision
  "C212"      , # Un ordinateur
  "C213"      , # Une tablette
  "C214"      , # Un smartphone
  "C215"      , # Sur écran avec un vidéoprojecteur
  "C216"      , # (NSP)
  "C217"      , # (REF)
  "C221"      , # DVD ou blue ray
  "C222"      , # Vidéo à la demande : Netflix, Canal Play, myTF1vod...
  "C223"      , # Diffusion à la télévision chaîne gratuite ou payante
  "C224"      , # Rediffusion en replay
  "C225"      , # Vidéo en streaming
  "C226"      , # Fichier numérique
  "C227"      , # (NSP)
  "C228"      , # (REF)
  "C23"       , # Au cours des 12 derniers mois, avez-vous regardé des films sur un équipement mobile en deh...
  "C24"       , # Au cours des 12 derniers mois, avez-vous regardé des films dans une autre langue que le fr...
  "C25"       ,
  "C25_RECODE",
  "C2601"     , # Films comiques
  "C2602"     , # Films d’action
  "C2603"     , # Films historiques, biographies
  "C2604"     , # Films policiers ou d'espionnage, thrillers
  "C2605"     , # Films d’aventure
  "C2606"     , # Comédies dramatiques
  "C2607"     , # Films d’animation, dessins animés
  "C2608"     , # Films d'horreur et d'épouvante
  "C2609"     , # Films d'auteur
  "C2610"     , # Documentaires
  "C2611"     , # Westerns
  "C2612"     , # Films érotiques
  "C2613"     , # Films de science-fiction et fantastique
  "C2614"     , # Films d’amour ou sentimentaux
  "C2615"     , # Comédies musicales
  "C2616"     , # Films politiques ou juridiques
  "C2617"     , # (NSP)
  "C2618"     , # Aucun
  "C2701"     , # Films comiques
  "C2702"     , # Films d’action
  "C2703"     , # Films historiques, biographies
  "C2704"     , # Films policiers ou d'espionnage, thrillers
  "C2705"     , # Films d’aventure
  "C2706"     , # Comédies dramatiques
  "C2707"     , # Films d’animation, dessins animés
  "C2708"     , # Films d'horreur et d'épouvante
  "C2709"     , # Films d'auteur
  "C2710"     , # Documentaires
  "C2711"     , # Westerns
  "C2712"     , # Films érotiques
  "C2713"     , # Films de science-fiction et fantastique
  "C2714"     , # Films d’amour ou sentimentaux
  "C2715"     , # Comédies musicales
  "C2716"     , # Films politiques ou juridiques
  "C2717"     , # (NSP)
  "C2718"     , # Aucun
  "C2801"     , # Films comiques
  "C2802"     , # Films d’action
  "C2803"     , # Films historiques, biographies
  "C2804"     , # Films policiers ou d'espionnage, thrillers
  "C2805"     , # Films d’aventure
  "C2806"     , # Comédies dramatiques
  "C2807"     , # Films d’animation, dessins animés
  "C2808"     , # Films d'horreur et d'épouvante
  "C2809"     , # Films d'auteur
  "C2810"     , # Documentaires
  "C2811"     , # Westerns
  "C2812"     , # Films érotiques
  "C2813"     , # Films de science-fiction et fantastique
  "C2814"     , # Films d’amour ou sentimentaux
  "C2815"     , # Comédies musicales
  "C2816"     , # Films politiques ou juridiques
  "C2817"     , # (NSP)
  "C2818"     , # Aucun
  "C2901"     , # Pirates des Caraïbes
  "C2902"     , # Resident evil [1-6]
  "C2903"     , # Moi, Daniel Blake
  "C2904"     , # Fast and Furious [1-8]
  "C2905"     , # La la land
  "C2906"     , # Intouchables
  "C2907"     , # Bridget Jones [1-3]
  "C2908"     , # Star Wars [1-8]
  "C2909"     , # Kung-Fu Panda [1-3]
  "C2910"     , # Titanic
  "C2911"     , # Qu’est-ce qu’on a fait au bon dieu ?
  "C2912"     , # Les enfants du paradis
  "C2913"     , # Tout sur ma mère
  "C2914"     , # La grande vadrouille
  "C2915"     , # Aucun de ceux-là
  "C2916"     , # (NSP)
  "C2917"     , # (REF)
  "C30"       , # Si vous ne pouviez plus regarder de films, est-ce que cela vous manquerait ?
  "C31"       , # Au cours des 12 derniers mois, à quelle fréquence en moyenne avez-vous regardé des séries,...
  "C3201"     , # Seul(e)
  "C3202"     , # En couple
  "C3203"     , # Avec vos enfants
  "C3204"     , # Avec vos petits-enfants
  "C3205"     , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "C3206"     , # Avec un ou des amis
  "C3207"     , # En groupe organisé
  "C3208"     , # (Pas de règle générale)
  "C3209"     , # (NSP)
  "C3210"     , # (REF)
  "C331"      , # Un écran de télévision
  "C332"      , # Un ordinateur
  "C333"      , # Une tablette
  "C334"      , # Un smartphone
  "C335"      , # Sur écran avec un vidéoprojecteur
  "C336"      , # (NSP)
  "C337"      , # (REF)
  "C341"      , # DVD ou blue ray
  "C342"      , # Vidéo à la demande : Netflix, Canal Play, myTF1vod...
  "C343"      , # Diffusion à la télévision chaîne gratuite ou payante
  "C344"      , # Rediffusion en replay
  "C345"      , # Vidéo en streaming
  "C346"      , # Fichier numérique
  "C347"      , # (NSP)
  "C348"      , # (REF)
  "C35"       , # Au cours des 12 derniers mois, avez-vous regardé des séries ou feuilletons sur un équipeme...
  "C36"       , # Au cours des 12 derniers mois, avez-vous regardé des séries ou feuilletons dans une autre...
  "C37"       ,
  "C37_RECODE",
  "C3801"     , # Séries comiques
  "C3802"     , # Séries d’action
  "C3803"     , # Séries historiques, biographies
  "C3804"     , # Séries policières ou d'espionnage, thrillers
  "C3805"     , # Séries d’aventure
  "C3806"     , # Séries dramatiques
  "C3807"     , # Séries d’animation, dessins animés
  "C3808"     , # Séries d'horreur et d'épouvante
  "C3809"     , # Séries d'auteur
  "C3810"     , # Séries documentaires
  "C3811"     , # Westerns
  "C3812"     , # Séries érotiques
  "C3813"     , # Séries de science-fiction et fantastique
  "C3814"     , # Séries d’amour ou sentimentales
  "C3815"     , # Séries musicales
  "C3816"     , # Séries politiques ou juridiques
  "C3817"     , # (NSP)
  "C3818"     , # Aucun
  "C3901"     , # Séries comiques
  "C3902"     , # Séries d’action
  "C3903"     , # Séries historiques, biographies
  "C3904"     , # Séries policières ou d'espionnage, thrillers
  "C3905"     , # Séries d’aventure
  "C3906"     , # Séries dramatiques
  "C3907"     , # Séries d’animation, dessins animés
  "C3908"     , # Séries d'horreur et d'épouvante
  "C3909"     , # Séries d'auteur
  "C3910"     , # Séries documentaires
  "C3911"     , # Westerns
  "C3912"     , # Séries érotiques
  "C3913"     , # Séries de science-fiction et fantastique
  "C3914"     , # Séries d’amour ou sentimentaux
  "C3915"     , # Séries musicales
  "C3916"     , # Séries politiques ou juridiques
  "C3917"     , # (NSP)
  "C3918"     , # Aucun
  "C4001"     , # Séries comiques
  "C4002"     , # Séries d’action
  "C4003"     , # Séries historiques, biographies
  "C4004"     , # Séries policières ou d'espionnage, thrillers
  "C4005"     , # Séries d’aventure
  "C4006"     , # Séries dramatiques
  "C4007"     , # Séries d’animation, dessins animés
  "C4008"     , # Séries d'horreur et d'épouvante
  "C4009"     , # Séries d'auteur
  "C4010"     , # Séries documentaires
  "C4011"     , # Westerns
  "C4012"     , # Séries érotiques
  "C4013"     , # Séries de science-fiction et fantastique
  "C4014"     , # Séries d’amour ou sentimentaux
  "C4015"     , # Séries musicales
  "C4016"     , # Séries politiques ou juridiques
  "C4017"     , # (NSP)
  "C4018"     , # Aucun
  "C4101"     , # Game of Thrones
  "C4102"     , # Grey's Anatomy
  "C4103"     , # Le Bureau des légendes
  "C4104"     , # Les Experts
  "C4105"     , # Versailles
  "C4106"     , # Les Feux de l'Amour
  "C4107"     , # Mad Men
  "C4108"     , # Fais pas ci, fais pas ça
  "C4109"     , # Plus belle la vie
  "C4110"     , # Rizzoli and Isles
  "C4111"     , # The Big Bang Theory
  "C4112"     , # The Walking Dead
  "C4113"     , # La petite maison dans la prairie
  "C4114"     , # Chapeau melon et bottes de cuir
  "C4115"     , # Twin Peaks
  "C4116"     , # (NSP)
  "C4117"     , # Aucun de ceux là
  "C42"         # Si vous ne pouviez plus regarder de séries, feuilletons, est-ce que cela vous manquerait ?
)

# ============================================================
# Info
# ============================================================

vars_info <- c(
  "D101"     , # Politique
  "D102"     , # Économie
  "D103"     , # Social, société
  "D104"     , # Science
  "D105"     , # Voyage
  "D106"     , # Beauté
  "D107"     , # Mode
  "D108"     , # Enfant, éducation
  "D109"     , # Mécanique
  "D110"     , # Sports
  "D111"     , # Culture et arts
  "D112"     , # Cuisine
  "D113"     , # Santé
  "D114"     , # Médias
  "D115"     , # Aucun de ces thèmes
  "D116"     , # (NSP)
  "D117"     , # (REF)
  "D2"       , # De manière générale, à quelle fréquence vous tenez-vous informé(e) de l’actualité ?
  "D31"      , # La télévision (journaux, etc)
  "D32"      , # La radio (en direct ou par podcast)
  "D33"      , # La presse papier
  "D34"      , # La presse numérique / sites web d’information (cf. Slate, Mediapart, Huff...)
  "D35"      , # Les réseaux sociaux
  "D36"      , # Les blogs et forums
  "D37"      , # Autre Internet
  "D38"      , # (NSP)
  "D39"      , # (REF)
  "D41"      , # De la presse numérique payante
  "D42"      , # De la presse numérique gratuite
  "D43"      , # (NSP)
  "D44"      , # (REF)
  "D5"       , # Au cours des 12 derniers mois, avez-vous consulté, lu ou vu des informations sur un équipe...
  "D6"       , # Au cours des 12 derniers mois, avez-vous consulté des informations dans une autre langue q...
  "D7"       ,
  "D7_RECODE",
  "D8"         # Si vous ne pouviez plus vous informer sur l’actualité, est ce que cela vous manquerait ?
)

# ============================================================
# Musique
# ============================================================

vars_musique <- c(
  "E1"         , # Ecoutez vous la radio, que ce soit chez vous, en voiture ou ailleurs ?
  "E2"         , # Valeur déclarée par le répondant
  "E2_C_1"     , # Valeur déclarée
  "E3"         , # Unité choisie pour la valeur déclarée
  "E4"         , # Quelle station de radio écoutez-vous le plus ?
  "E4_RECODE"  ,
  "E501"       , # La matinale, le morning
  "E502"       , # Les libres antennes / les radios libres
  "E503"       , # Les émissions de divertissement, talk shows, jeux, chroniques humoristiques
  "E504"       , # Les informations, journaux, flash
  "E505"       , # Les émissions d’actualité, interviews, de débats
  "E506"       , # Les émissions de service (santé, juridique…)
  "E507"       , # Les émissions sur le sport
  "E508"       , # Les feuilletons, créations sonores
  "E509"       , # Les reportages, documentaires
  "E510"       , # Les émissions de littérature, cinéma, histoire, philosophie
  "E511"       , # Les émissions musicales
  "E512"       , # Autres émissions
  "E513"       , # (NSP)
  "E514"       , # (REF)
  "E6"         , # Au cours des 12 derniers mois, avez-vous écouté des programmes radiophoniques en différé p...
  "E7"         , # Avez-vous écouté au cours des 12 derniers mois de la musique ?
  "E81"        , # CD ou cassettes
  "E82"        , # Disques vinyle
  "E83"        , # Musique en streaming sur une plateforme spécialisée : spotify, deezer, qobuz,…
  "E84"        , # Musique en streaming sur une autre plateforme
  "E85"        , # Fichiers numériques : mp3 ou autre
  "E86"        , # Radio
  "E87"        , # Télévision
  "E88"        , # (NSP)
  "E89"        , # (REF)
  "E8BIS"      , # Au cours des 12 derniers mois, avez-vous écouté de la musique sur un équipement mobile en...
  "E9"         , # Pouvez-vous préciser exactement quel est votre type de musique préféré, si vous en avez un...
  "E1001"      , # Chansons ou variétés françaises
  "E1002"      , # Musiques du monde
  "E1003"      , # Musiques traditionnelles
  "E1004"      , # Variétés internationales
  "E1005"      , # RnB
  "E1006"      , # Musiques électroniques, techno
  "E1007"      , # Hip hop, rap
  "E1008"      , # Metal, hard rock
  "E1009"      , # Pop, rock
  "E1010"      , # Jazz
  "E1011"      , # Opéra
  "E1012"      , # Musique classique
  "E1013"      , # Autre(s) genre(s) de musique
  "E1014"      , # Aucun
  "E1015"      , # (NSP)
  "E11"        ,
  "E1201"      , # Chansons ou variétés françaises
  "E1202"      , # Musiques du monde
  "E1203"      , # Musiques traditionnelles
  "E1204"      , # Variétés internationales
  "E1205"      , # RnB
  "E1206"      , # Musiques électroniques, techno
  "E1207"      , # Hip hop, rap
  "E1208"      , # Metal, hard rock
  "E1209"      , # Pop, rock
  "E1210"      , # Jazz
  "E1211"      , # Opéra
  "E1212"      , # Musique classique
  "E1213"      , # Autre(s) genre(s) de musique
  "E1214"      , # Aucun
  "E1215"      , # (NSP)
  "E1301"      , # Chansons ou variétés françaises
  "E1302"      , # Musiques du monde
  "E1303"      , # Musiques traditionnelles
  "E1304"      , # Variétés internationales
  "E1305"      , # RnB
  "E1306"      , # Musiques électroniques, techno
  "E1307"      , # Hip hop, rap
  "E1308"      , # Metal, hard rock
  "E1309"      , # Pop, rock
  "E1310"      , # Jazz
  "E1311"      , # Opéra
  "E1312"      , # Musique classique
  "E1313"      , # Autre(s) genre(s) de musique
  "E1314"      , # Aucun
  "E1315"      , # (NSP)
  "E1401"      , # Bob Marley
  "E1402"      , # Miles Davis
  "E1403"      , # Julien Doré
  "E1404"      , # Coldplay
  "E1405"      , # Cesaria Evora
  "E1406"      , # Rihanna
  "E1407"      , # Booba
  "E1408"      , # NTM
  "E1409"      , # Metallica
  "E1410"      , # Jacques Brel
  "E1411"      , # David Bowie
  "E1412"      , # Beethoven
  "E1413"      , # Bob Sinclar
  "E1414"      , # Céline Dion
  "E1415"      , # Renaud
  "E1416"      , # (NSP)
  "E1417"      , # (REF)
  "E15"        , # Au cours des 12 derniers mois, avez-vous écouté de la musique dans une autre langue que le...
  "E16"        , # De quelles langues s'agissait-il ?
  "E16C"       , # De quelles autres langues s'agissait-il ?
  "E16_RECODE" ,
  "E16B"       , # Confirmez-vous avoir écouté seulement de la musique en français et en (langues déclarées e...
  "E16C_RECODE",
  "E17"        , # Vous arrive t il de mettre de la musique quand vous rentrez chez vous ? Diriez-vous...
  "E18"        , # Vous arrive t il d'écouter de la musique pour elle-même, sans rien faire d'autre ?
  "E19"          # Si vous ne pouviez plus écouter de musique, est-ce que cela vous manquerait ?
)

# ============================================================
# Livre
# ============================================================

vars_livre <- c(
  "F1"        , # Etes vous personnellement inscrit dans une bibliothèque ou médiathèque ?
  "F21"       , # Une bibliothèque ou médiathèque municipale
  "F22"       , # Une bibliothèque scolaire ou universitaire
  "F23"       , # Autres : bibliothèque d'entreprise ou de comité d’entreprise, privée ou paroissiale ou « b...
  "F24"       , # (NSP)
  "F25"       , # (REF)
  "F3"        , # En moyenne tous les combien êtes vous allé dans une bibliothèque ou une médiathèque au cou...
  "F3BIS01"   , # Seul(e)
  "F3BIS02"   , # En couple
  "F3BIS03"   , # Avec vos enfants
  "F3BIS04"   , # Avec vos petits-enfants
  "F3BIS05"   , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "F3BIS06"   , # Avec un ou des amis
  "F3BIS07"   , # En groupe organisé
  "F3BIS08"   , # (Pas de règle générale)
  "F3BIS09"   , # (NSP)
  "F3BIS10"   , # (REF)
  "F4"        , # Si vous ne pouviez plus aller à la bibliothèque, est-ce que cela vous manquerait ?
  "F5"        , # Au total, diriez-vous que vous êtes plutôt quelqu'un qui lit
  "F601"      , # Des œuvres de la littérature classique française ou étrangère (jusqu’au 20 siècle)
  "F602"      , # Des romans policiers ou d'espionnage
  "F603"      , # Des romans de science fiction, fantastiques, heroic-fantasy, horreur, ..
  "F604"      , # Des romans historiques
  "F605"      , # Des romans sentimentaux du type Harlequin
  "F606"      , # Des prix littéraires (Goncourt, Renaudot...)
  "F607"      , # Des biographies romancées
  "F608"      , # D’autres romans contemporains
  "F609"      , # Des BD
  "F610"      , # Des Comics
  "F611"      , # Des Mangas
  "F612"      , # Des livres de science, d’histoire, essais politiques, philosophiques, religieux
  "F613"      , # Des livres portant sur l’actualité, des questions de société
  "F614"      , # Des livres de développement personnel, psychologie
  "F615"      , # Des livres pratiques, arts de vivre et loisirs : cuisine, décoration, bricolage, jardinage...
  "F616"      , # Des livres d'art ou des beaux livres illustrés de photographies
  "F617"      , # Autres livres
  "F618"      , # Aucun de ces livres
  "F619"      , # (NSP)
  "F620"      , # (REF)
  "F701"      , # Des œuvres de la littérature classique française ou étrangère (jusqu’au 20 siècle)
  "F702"      , # Des romans policiers ou d'espionnage
  "F703"      , # Des romans de science fiction, fantastiques, heroic-fantasy, horreur, ..
  "F704"      , # Des romans historiques
  "F705"      , # Des romans sentimentaux du type Harlequin
  "F706"      , # Des prix littéraires (Goncourt, Renaudot...)
  "F707"      , # Des biographies romancées
  "F708"      , # D’autres romans contemporains
  "F709"      , # Des BD
  "F710"      , # Des Comics
  "F711"      , # Des Mangas
  "F712"      , # Des livres de science, d’histoire, essais politiques, philosophiques, religieux
  "F713"      , # Des livres portant sur l’actualité, des questions de société
  "F714"      , # Des livres de développement personnel, psychologie
  "F715"      , # Des livres pratiques, arts de vivre et loisirs : cuisine, décoration, bricolage, jardinage...
  "F716"      , # Des livres d'art ou des beaux livres illustrés de photographies
  "F717"      , # Autres livres
  "F718"      , # Aucun de ces livres
  "F719"      , # (NSP)
  "F720"      , # (REF)
  "F801"      , # Des œuvres de la littérature classique française ou étrangère (jusqu’au 20 siècle)
  "F802"      , # Des romans policiers ou d'espionnage
  "F803"      , # Des romans de science fiction, fantastiques, heroic-fantasy, horreur, ..
  "F804"      , # Des romans historiques
  "F805"      , # Des romans sentimentaux du type Harlequin
  "F806"      , # Des prix littéraires (Goncourt, Renaudot...)
  "F807"      , # Des biographies romancées
  "F808"      , # D’autres romans contemporains
  "F809"      , # Des BD
  "F810"      , # Des Comics
  "F811"      , # Des Mangas
  "F812"      , # Des livres de science, d’histoire, essais politiques, philosophiques, religieux
  "F813"      , # Des livres portant sur l’actualité, des questions de société
  "F814"      , # Des livres de développement personnel, psychologie
  "F815"      , # Des livres pratiques, arts de vivre et loisirs : cuisine, décoration, bricolage, jardinage...
  "F816"      , # Des livres d'art ou des beaux livres illustrés de photographies
  "F817"      , # Autres livres
  "F818"      , # Aucun de ces livres
  "F819"      , # (NSP)
  "F820"      , # (REF)
  "F9"        , # Vous arrive-t-il de lire des livres dans une autre langue que le français ?
  "F10"       , # De quelles langues s'agit-il ?
  "F10_RECODE",
  "F11"       , # Au cours des 12 derniers mois, à quelle fréquence en moyenne avez-vous lu des livres ?
  "F11BIS"    , # Au cours des douze derniers mois, avez-vous lu des livres plutôt pendant vos congés ou plu...
  "F11TER"    , # En dehors de vos congés, avez-vous lu des livres plutôt le week-end, ou plutôt le reste de...
  "F121"      , # Livres papier
  "F122"      , # Liseuse : kindle, kobo
  "F123"      , # Tablette
  "F124"      , # Autre
  "F125"      , # (NSP)
  "F126"      , # (REF)
  "F12BIS"    , # La plupart du temps, les livres que vous lisez sont-ils au format papier ?
  "F13"       , # Valeur déclarée par le répondant
  "F13_C_1"   , # Valeur déclarée
  "F14"       , # Avez vous inclus dans ce total des albums de bandes dessinées ou des mangas ?
  "F15"       , # Valeur déclarée par le répondant
  "F15_C_1"   , # Valeur déclarée
  "F1601"     , # Le Tartuffe de Molière
  "F1602"     , # Candide de Voltaire
  "F1603"     , # Le lys dans la vallée de Balzac
  "F1604"     , # Notre Dame de Paris de Victor Hugo
  "F1605"     , # Huis Clos de Sartre
  "F1606"     , # Apocalypse bébé de Virginie Despentes
  "F1607"     , # Où on va, papa ? de Jean-Louis Fournier
  "F1608"     , # La Carte et le Territoire de Michel Houellebecq
  "F1609"     , # Neige de Orhan Pamuk
  "F1610"     , # Chanson douce de Leila Slimani
  "F1611"     , # Les écureuils de Central Park sont tristes le lundi de Katherine Pancol
  "F1612"     , # Cinquante nuances de grey de E.L. James
  "F1613"     , # Central Park de Guillaume Musso
  "F1614"     , # La vérité sur l’affaire Harry Quebert de Joel Dickert
  "F1615"     , # Inferno de Dan Brown
  "F1616"     , # Aucun de ces livres
  "F1617"     , # (NSP)
  "F1618"     , # (REF)
  "F17"         # Si vous ne pouviez plus lire de livres, pensez-vous que cela vous manquerait :
)

# ============================================================
# Spectacles
# ============================================================

vars_spectacles <- c(
  "G2401"                        , # Aller à un concert de chansons ou variétés françaises
  "G2402"                        , # Aller à un concert de musiques du monde
  "G2403"                        , # Aller à un concert de musiques traditionnelles
  "G2404"                        , # Aller à un concert de variétés internationales
  "G2405"                        , # Aller à un concert de RnB
  "G2406"                        , # Aller à un concert de musiques électroniques, techno
  "G2407"                        , # Aller à un concert de Hip hop, rap
  "G2408"                        , # Aller à un concert de Metal, hard rock
  "G2409"                        , # Aller à un concert de pop, rock
  "G2410"                        , # Aller à un concert de Jazz
  "G2411"                        , # Aller à un spectacle d’opéra
  "G2412"                        , # Aller à un concert de musique classique
  "G2413"                        , # Aucune de ces activités
  "G2414"                        , # (NSP)
  "G2415"                        , # (REF)
  "G2501"                        , # Aller à un concert de chansons ou variétés françaises
  "G2502"                        , # Aller à un concert de musiques du monde
  "G2503"                        , # Aller à un concert de musiques traditionnelles
  "G2504"                        , # Aller à un concert de variétés internationales
  "G2505"                        , # Aller à un concert de RnB
  "G2506"                        , # Aller à un concert de musiques électroniques, techno
  "G2507"                        , # Aller à un concert de Hip hop, rap
  "G2508"                        , # Aller à un concert de Metal, hard rock
  "G2509"                        , # Aller à un concert de pop, rock
  "G2510"                        , # Aller à un concert de Jazz
  "G2511"                        , # Aller à un spectacle d’opéra
  "G2512"                        , # Aller à un concert de musique classique
  "G2513"                        , # Aucune de ces activités
  "G2514"                        , # (NSP)
  "G2515"                        , # (REF)
  "G26val_variet_francaise"      ,
  "G26val_musiques_monde"        ,
  "G26val_musiques_tradi"        ,
  "G26val_variet_internationale" ,
  "G26val_rnb"                   ,
  "G26val_electro_techno"        ,
  "G26val_hip_hop_rap"           ,
  "G26val_metal_hard"            ,
  "G26val_pop_rock"              ,
  "G26val_jazz"                  ,
  "G26val_opera"                 ,
  "G26val_musique_classique"     ,
  "G26unit_variet_francaise"     ,
  "G26unit_musiques_monde"       ,
  "G26unit_musiques_tradi"       ,
  "G26unit_variet_internationale",
  "G26unit_rnb"                  ,
  "G26unit_electro_techno"       ,
  "G26unit_hip_hop_rap"          ,
  "G26unit_metal_hard"           ,
  "G26unit_pop_rock"             ,
  "G26unit_jazz"                 ,
  "G26unit_opera"                ,
  "G26unit_musique_classique"    ,
  "G271"                         , # Dans la commune ou la région où vous habitez
  "G272"                         , # A Paris ou dans la région parisienne
  "G273"                         , # Dans une autre région française
  "G274"                         , # A l'étranger, en Europe
  "G275"                         , # A l'étranger, en dehors d’Europe
  "G276"                         , # (Ne se rappelle plus)
  "G277"                         , # (NSP)
  "G278"                         , # (REF)
  "G28"                          , # Au cours des douze derniers mois, êtes-vous allé à un concert plutôt pendant vos congés ou...
  "G29"                          , # En dehors de vos congés, êtes-vous allé à un concert plutôt le week-end, ou plutôt le rest...
  "G3001"                        , # Seul(e)
  "G3002"                        , # En couple
  "G3003"                        , # Avec vos enfants
  "G3004"                        , # Avec vos petits-enfants
  "G3005"                        , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "G3006"                        , # Avec un ou des amis
  "G3007"                        , # En groupe organisé
  "G3008"                        , # (Pas de règle générale)
  "G3009"                        , # (NSP)
  "G3010"                        , # (REF)
  "G31"                          , # Si vous ne pouviez plus aller au concert ou voir des spectacles musicaux est-ce que cela v...
  "G11"                          , # Aller à une rave party
  "G12"                          , # Participer à une soirée karaoké
  "G13"                          , # Aller danser dans un bal public
  "G14"                          , # Aller voir un match ou un autre spectacle sportif payant
  "G15"                          , # Aller danser dans une discothèque ou une boite de nuit
  "G16"                          , # Aucune de ces activités
  "G17"                          , # (NSP)
  "G18"                          , # (REF)
  "G211"                         , # Aller à une rave party
  "G212"                         , # Participer à une soirée karaoké
  "G213"                         , # Aller danser dans un bal public
  "G214"                         , # Aller voir un match ou un autre spectacle sportif payant
  "G215"                         , # Aller danser dans une discothèque ou une boite de nuit
  "G216"                         , # Aucune de ces activités
  "G217"                         , # (NSP)
  "G218"                         , # (REF)
  "G3A"                          , # Etes-vous allé au cinéma au cours des 12 derniers mois ?
  "G3B"                          , # Etes-vous déjà allé au cinéma au cours de votre vie ?
  "G6BIS"                        , # Au cours des 12 derniers mois, avez-vous vu des films dans un espace ou un lieu public aut...
  "G4"                           , # Valeur déclarée par le répondant
  "G4_C_1"                       , # Valeur déclarée
  "G5"                           , # Unité choisie pour la valeur déclarée
  "G6"                           , # Au cours des 12 derniers mois, avez-vous fréquenté des cinémas d’art et d’essai ?
  "G701"                         , # Films comiques
  "G702"                         , # Films d’action
  "G703"                         , # Films historiques, biographies
  "G704"                         , # Films policiers ou d'espionnage, thrillers
  "G705"                         , # Films d’aventure
  "G706"                         , # Comédies dramatiques
  "G707"                         , # Films d’animation, dessins animés
  "G708"                         , # Films d'horreur et d'épouvante
  "G709"                         , # Films d'auteur
  "G710"                         , # Documentaires
  "G711"                         , # Westerns
  "G712"                         , # Films érotiques
  "G713"                         , # Films de science-fiction et fantastique
  "G714"                         , # Films d’amour ou sentimentaux
  "G715"                         , # Comédies musicales
  "G716"                         , # Films politiques ou juridiques
  "G717"                         , # (NSP)
  "G718"                         , # Aucun
  "G8"                           , # Au cours des douze derniers mois, êtes-vous allé au cinéma plutôt pendant vos congés ou pl...
  "G9"                           , # En dehors de vos congés, êtes-vous allé au cinéma plutôt le week-end ou plutôt le reste de...
  "G1001"                        , # Seul(e)
  "G1002"                        , # En couple
  "G1003"                        , # Avec vos enfants
  "G1004"                        , # Avec vos petits-enfants
  "G1005"                        , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "G1006"                        , # Avec un ou des amis
  "G1007"                        , # En groupe organisé
  "G1008"                        , # (Pas de règle générale)
  "G1009"                        , # (NSP)
  "G1010"                        , # (REF)
  "G111"                         , # Si vous ne pouviez plus aller au cinéma, pensez-vous que cela vous manquerait :
  "G121"                         , # Aller à un spectacle de danse
  "G122"                         , # Aller voir un spectacle de cirque
  "G123"                         , # Assister à un spectacle de rue y compris grands évènements nationaux, fêtes locales (14 ju...
  "G124"                         , # Aller voir une pièce de théâtre y compris one man show, improvisation
  "G125"                         , # Aucune de ces activités
  "G126"                         , # (NSP)
  "G127"                         , # (REF)
  "G131"                         , # Aller à un spectacle de danse
  "G132"                         , # Aller voir un spectacle de cirque
  "G133"                         , # Assister à un spectacle de rue rue y compris grands évènements nationaux, fêtes locales (1...
  "G134"                         , # Aller voir une pièce de théâtre y compris one man show, improvisation
  "G135"                         , # Aucune de ces activités
  "G136"                         , # (NSP)
  "G137"                         , # (REF)
  "G14val_danse"                 ,
  "G14val_cirque"                ,
  "G14val_spectacle_rue"         ,
  "G14val_theatre"               ,
  "G14unit_danse"                ,
  "G14unit_cirque"               ,
  "G14unit_spectacle_rue"        ,
  "G14unit_theatre"              ,
  "G161"                         , # De la danse classique
  "G162"                         , # De la danse traditionnelle ou folklorique
  "G163"                         , # De la danse modern jazz
  "G164"                         , # De la danse contemporaine
  "G165"                         , # D’un autre genre
  "G166"                         , # (NSP)
  "G167"                         , # (REF)
  "G171"                         , # Cirque traditionnel ou classique, sous un chapiteau et avec des animaux
  "G172"                         , # Cirque traditionnel ou classique, mais sans animaux
  "G173"                         , # Nouveau cirque ou cirque contemporain sous chapiteau
  "G174"                         , # Nouveau cirque ou cirque contemporain ailleurs que sous un chapiteau
  "G175"                         , # (Ne sait pas répondre)
  "G176"                         , # (REF)
  "G17B01"                       , # Seul(e)
  "G17B02"                       , # En couple
  "G17B03"                       , # Avec vos enfants
  "G17B04"                       , # Avec vos petits-enfants
  "G17B05"                       , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "G17B06"                       , # Avec un ou des amis
  "G17B07"                       , # En groupe organisé
  "G17B08"                       , # (Pas de règle générale)
  "G17B09"                       , # (NSP)
  "G17B10"                       , # (REF)
  "G181"                         , # D’un grand événement national (14 juillet, Fête de la musique, …)
  "G182"                         , # D’une animation dans un lieu commercial (centre commercial, foire, brocante, marché…)
  "G183"                         , # D’une fête locale ou animation dans les rues d’une ville
  "G184"                         , # D’un musicien, jongleur, statue vivante, etc. dans la rue (métro inclus)
  "G185"                         , # D’un festival d’arts de la rue
  "G186"                         , # D’un autre festival (théâtre, musique, cinéma, …)
  "G187"                         , # Autres
  "G188"                         , # (NSP)
  "G189"                         , # (REF)
  "G20A1"                        , # Théâtre classique
  "G20A2"                        , # Théâtre contemporain
  "G20A3"                        , # Pièce de boulevard, vaudeville
  "G20A4"                        , # One man show, café théâtre, spectacle d’improvisation
  "G20A5"                        , # Autre genre
  "G20A6"                        , # (NSP)
  "G20A7"                        , # (REF)
  "G20B"                         , # Au cours des douze derniers mois, êtes-vous allé au théâtre plutôt pendant vos congés ou p...
  "G21"                          , # En dehors de vos congés, êtes-vous allé au théâtre plutôt le week-end ou plutôt le reste d...
  "G2201"                        , # Seul(e)
  "G2202"                        , # En couple
  "G2203"                        , # Avec vos enfants
  "G2204"                        , # Avec vos petits-enfants
  "G2205"                        , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "G2206"                        , # Avec un ou des amis
  "G2207"                        , # En groupe organisé
  "G2208"                        , # (Pas de règle générale)
  "G2209"                        , # (NSP)
  "G2210"                        , # (REF)
  "G23"                          , # Si vous ne pouviez plus aller voir de spectacles de danse, de cirque, de rue ou des pièces...
  "G32"                          , # Au cours des 12 derniers mois, avez-vous assisté à un festival, que ce soit en France ou à...
  "G331"                         , # Théâtre, danse, arts de la rue
  "G332"                         , # Musique classique, opéra, jazz
  "G333"                         , # Musiques du monde ou traditionnelles
  "G334"                         , # Rock, variétés et autres genres de musique, etc…
  "G335"                         , # Cinéma
  "G336"                         , # Photographie
  "G337"                         , # Autres
  "G338"                         , # (NSP)
  "G339"                         , # (REF)
  "G351"                         , # Dans la commune ou la région où vous habitez
  "G352"                         , # A Paris ou dans la région parisienne
  "G353"                         , # Dans une autre région française
  "G354"                         , # A l'étranger, en Europe
  "G355"                         , # A l'étranger, en dehors d’Europe
  "G356"                         , # (NSP)
  "G357"                         , # (REF)
  "G36"                          , # Au cours des douze derniers mois, êtes-vous allé à des festivals plutôt pendant vos congés...
  "G37"                          , # En dehors de vos congés ou vacances, êtes-vous allé à des festivals plutôt le week-end, pa...
  "G3801"                        , # Seul(e)
  "G3802"                        , # En couple
  "G3803"                        , # Avec vos enfants
  "G3804"                        , # Avec vos petits-enfants
  "G3805"                        , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "G3806"                        , # Avec un ou des amis
  "G3807"                        , # En groupe organisé
  "G3808"                        , # (Pas de règle générale)
  "G3809"                        , # (NSP)
  "G3810"                        , # (REF)
  "G39"                            # Si vous ne pouviez plus aller dans un festival, est ce que cela vous manquerait
)

# ============================================================
# Musées
# ============================================================

vars_musees <- c(
  "H101"   , # Aller dans un parc d'attraction comme Disneyland Paris ou le parc Astérix
  "H102"   , # Aller dans un parc comme le Futuroscope ou la Cité des sciences de la Villette.
  "H103"   , # Aller dans une fête foraine
  "H104"   , # Aller dans un zoo ou un parc avec des animaux en liberté
  "H105"   , # Aller dans un centre d’archives pour des recherches historiques ou généalogiques
  "H106"   , # Aller dans une galerie d'art
  "H107"   , # Assister à un spectacle son et lumière
  "H108"   , # Visiter un site archéologique ou un chantier de fouilles
  "H109"   , # Visiter un monument historique (château, monument religieux, quartier historique…)
  "H110"   , # Visiter un musée ou une exposition (que ce soit d’art, de sciences et techniques, d’histoi...
  "H111"   , # Un autre genre de musée ou d’exposition (y compris dans un espace public : gare, parc, ate...
  "H112"   , # (Aucune de ces activités)
  "H113"   , # (NSP)
  "H114"   , # (REF)
  "H201"   , # Aller dans un parc d'attraction comme Disneyland Paris ou le parc Astérix
  "H202"   , # Aller dans un parc comme le Futuroscope ou la Cité des sciences de la Villette.
  "H203"   , # Aller dans une fête foraine
  "H204"   , # Aller dans un zoo ou un parc avec des animaux en liberté
  "H205"   , # Aller dans un centre d’archives pour des recherches historiques ou généalogiques
  "H206"   , # Aller dans une galerie d'art
  "H207"   , # Assister à un spectacle son et lumière
  "H208"   , # Visiter un site archéologique ou un chantier de fouilles
  "H209"   , # Visiter un monument historique (château, monument religieux, quartier historique…)
  "H210"   , # Visiter un musée ou une exposition (que ce soit d’art, de sciences et techniques, d’histoi...
  "H211"   , # Un autre genre de musée ou d’exposition (y compris dans un espace public : gare, parc, ate...
  "H212"   , # (Aucune de ces activités)
  "H214"   , # (NSP)
  "H215"   , # (REF)
  "H301"   , # Musée ou exposition de peinture, sculpture, de l’Antiquité jusqu’au début du 20ème siècle
  "H302"   , # Musée ou exposition d’art moderne ou contemporain
  "H303"   , # Musée ou exposition d’histoire, de mémoire
  "H304"   , # Musée ou exposition de préhistoire, d’archéologie
  "H305"   , # Musée ou exposition de sciences et techniques, d’histoire naturelle, d’industrie...
  "H306"   , # Musée ou exposition d’ethnographie, d’artisanat, de société (arts et traditions populaires...
  "H307"   , # Musée ou exposition d’architecture, de design, d’arts décoratifs
  "H308"   , # Aucun de ces lieux
  "H309"   , # (NSP)
  "H310"   , # (REF)
  "H4"     , # Valeur déclarée par le répondant
  "H4_C_1" , # Valeur déclarée
  "H4F"    , # Unité choisie pour la valeur déclarée
  "H51"    , # Dans la commune ou la région où vous habitez
  "H52"    , # A Paris ou dans la région parisienne
  "H53"    , # Dans une autre région française
  "H54"    , # A l'étranger, en Europe
  "H55"    , # A l'étranger, en dehors d’Europe
  "H56"    , # (NSP)
  "H57"    , # (REF)
  "H6"     , # Au cours des douze derniers mois, avez-vous visité des musées ou expositions plutôt pendan...
  "H7"     , # En dehors de vos congés, avez-vous visité des musées ou expositions plutôt le week-end, ou...
  "H801"   , # Seul(e)
  "H802"   , # En couple
  "H803"   , # Avec vos enfants
  "H804"   , # Avec vos petits-enfants
  "H805"   , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "H806"   , # Avec un ou des amis
  "H807"   , # En groupe organisé
  "H808"   , # (Pas de règle générale)
  "H809"   , # (NSP)
  "H810"   , # (REF)
  "H9"     , # Si vous ne pouviez plus visiter un musée ou voir une exposition, est-ce que cela vous manq...
  "H1001"  , # Une cathédrale, une abbaye, un monastère, etc..
  "H1002"  , # Un château fort ou un autre monument ancien non religieux (palais, hôtel particulier, mais...
  "H1003"  , # Un quartier ancien dans une ville touristique ou un village de caractère
  "H1004"  , # Un site industriel (ancienne usine, exploitation minière, trains touristiques, anciennes g...
  "H1005"  , # Un parc ou jardin historique
  "H1006"  , # Un champ de bataille, un mémorial
  "H1007"  , # Autre
  "H1008"  , # Aucun de ces lieux
  "H1009"  , # (NSP)
  "H1010"  , # (REF)
  "H11"    , # Valeur déclarée par le répondant
  "H11_C_1", # Valeur déclarée
  "H11F"   , # Unité choisie pour la valeur déclarée
  "H121"   , # Dans la commune ou la région où vous habitez
  "H122"   , # A Paris ou dans la région parisienne
  "H123"   , # Dans une autre région française
  "H124"   , # A l'étranger, en Europe
  "H125"   , # A l'étranger, en dehors d’Europe
  "H126"   , # (NSP)
  "H127"   , # (REF)
  "H13"    , # Au cours des douze derniers mois, avez-vous visité des monuments ou sites patrimoniaux plu...
  "H14"    , # En dehors de vos congés, avez-vous visité des monuments ou sites patrimoniaux plutôt le we...
  "H1501"  , # Seul(e)
  "H1502"  , # En couple
  "H1503"  , # Avec vos enfants
  "H1504"  , # Avec vos petits-enfants
  "H1505"  , # Avec des proches (parents, grands-parents, frères, sœurs…)
  "H1506"  , # Avec un ou des amis
  "H1507"  , # En groupe organisé
  "H1508"  , # (Pas de règle générale)
  "H1509"  , # (NSP)
  "H1510"  , # (REF)
  "H16"      # Si vous ne pouviez plus visiter de monuments ou de sites patrimoniaux, est ce que cela vou...
)

# ============================================================
# Équip
# ============================================================

vars_equip <- c(
  "I101", # Poste de radio, transistor
  "I102", # Téléviseur (même non relié à une antenne ou à internet)
  "I103", # Platine ou électrophone pour écouter des disques vinyle
  "I104", # Lecteur de CD et/ou DVD
  "I105", # Console de jeux fixe (qui se branche sur télé)
  "I106", # Console de jeux portable (avec écran intégré)
  "I107", # Appareil photo numérique
  "I108", # Micro-ordinateur fixe («de bureau »)
  "I109", # Micro-ordinateur portable
  "I110", # Tablette
  "I111", # Téléphone portable, Smartphone
  "I112", # Accès à Internet
  "I113", # Rien de tout cela
  "I114", # (NSP)
  "I115", # (REF)
  "I2"  , # Ici dans votre foyer, combien y-a-t-il de postes de télévision que vous ou une autre perso...
  "I3"  , # Quel type de connexion à Internet avez-vous ?
  "I4"  , # A quelle fréquence vous personnellement vous connectez vous à Internet à votre domicile ou...
  "I5"  , # Utilisez-vous les réseaux sociaux ?
  "I6"  , # A quelle fréquence consultez-vous ces réseaux sociaux ?
  "I71" , # Rechercher des informations pratiques sur un événement culturel, une exposition, un specta...
  "I72" , # Faire une visite virtuelle d’une exposition, un musée
  "I73" , # Regarder un concert
  "I74" , # Regarder un spectacle de théâtre
  "I75" , # Regarder un spectacle de danse
  "I76" , # Regarder des contenus scientifiques et techniques
  "I77" , # Rien de tout cela
  "I78" , # (NSP)
  "I79"   # (REF)
)

# ============================================================
# Travail
# ============================================================

vars_travail <- c(
  "SITUA"               , # Quelle est votre situation vis-a-vis du travail ?
  "TRAVAIL"             , # Exercez-vous actuellement une activité rémunérée ?
  "ACTIVANTE"           , # Confirmez-vous avoir déjà exercé une activité professionnelle ?
  "STATUTECL"           , # Etes-vous…?
  "STATUT"              , # Même variable que STATUTECL avec la modalité 8 recodée en 2
  "CS2D"                , # Quelle est votre profession principale ?
  "TYPEMPLOI"           , # Quel est votre type d'emploi ?
  "TEMPTRAV"            , # Travaillez-vous…?
  "SUPERVISION"         , # Dans votre emploi principal, vous arrive-t-il de superviser le travail d'autres salariés (...
  "CLASSIF"             , # Dans votre emploi, êtes-vous classé comme…
  "CLASSIF2"            , # Dans votre emploi, êtes-vous classé comme…
  "SALARIES"            , # Combien de salariés employez-vous ? (ou la personne que vous aidez)
  "ACTIVLIB"            , # Quelle est l'activité principale de l'établissement qui vous emploie ou que vous dirigez ?
  "STATUTECLANTE"       , # Voir STATUTECL
  "STATUTANTE"          , # Voir STATUT
  "CS2D_ante"           , # Quelle est votre profession principale ?
  "CSTOT"               , # Profession consolidée (profession actuelle ou dernière profession connue)
  "SUPERVISIONANTE"     , # Voir SUPERVISION
  "CLASSIFANTE"         , # Voir CLASSIF
  "CLASSIFANTE2"        , # Voir CLASSIF2
  "RECHEMPLOI"          , # Cherchez-vous un autre emploi ?
  "S10"                 , # Valeur déclarée par le répondant
  "S10_C_1"             , # Valeur déclarée
  "S11"                 , # Valeur déclarée par le répondant
  "S11_C_1"             , # Valeur déclarée
  "S12"                 , # Valeur déclarée par le répondant
  "S12_C_1"             , # Valeur déclarée
  "SITUA_conj"          ,
  "TRAVAIL_conj"        ,
  "ACTIVANTE_conj"      ,
  "STATUTECL_conj"      ,
  "STATUT_conj"         ,
  "CS2D_conj"           ,
  "TYPEMPLOI_conj"      ,
  "TEMPTRAV_conj"       ,
  "SUPERVISION_conj"    ,
  "CLASSIF_conj"        ,
  "CLASSIF2_conj"       ,
  "SALARIES_conj"       ,
  "ACTIVLIB_conj"       ,
  "STATUTECLANTE_conj"  ,
  "STATUTANTE_conj"     ,
  "CS2D_ante_conj"      ,
  "CSTOT_conj"          ,
  "SUPERVISIONANTE_conj",
  "CLASSIFANTE_conj"    ,
  "CLASSIFANTE2_conj"   ,
  "RECHEMPLOI_conj"      
)

# ============================================================
# Cap_cult
# ============================================================

vars_cap_cult <- c(
  "AGDIP"                       , # Age d'obtention du diplôme
  "DATDIP"                      , # Valeur déclarée par le répondant
  "DATDIP_C_1"                  , # Valeur déclarée
  "DIPLOM"                      , # Quel est le niveau de diplôme le plus élevé que vous ayez obtenu ?
  "DIPLOMACT"                   , # De quel diplôme s'agit-il ?
  "FORMEL"                      , # Suivez-vous actuellement des études ou une formation conduisant à un diplôme ou à un titre...
  "NAIM"                        , # Votre mère est-elle née ?
  "NAIP"                        , # Votre père est-il né ?
  "NATIO1N1"                    , # Français/e de naissance, y compris par réintégration
  "NATIO1N2"                    , # Français/e par naturalisation, mariage, déclaration ou option à la majorité ?
  "NATIO1N3"                    , # Etranger
  "NATIO1N4"                    , # Apatride
  "NATIO1N5"                    , # NSP
  "NATIO1N6"                    , # REF
  "NATIO2N"                     , # Quelle est votre autre nationalité ?
  "NATIO2NO"                    , # Pourriez-vous préciser la nationalité ?
  "NATIOM"                      , # Quelle était la nationalité de votre mère (ou tutrice ou mère adoptive) à sa naissance ?
  "NATIOP"                      , # Quelle était la nationalité de votre père (ou tuteur ou père adoptif) à sa naissance ?
  "PAYM"                        , # Dans quel pays est-elle née ?
  "PAYMO"                       , # Pouvez-vous préciser ?
  "PAYP"                        , # Dans quel pays est-il né ?
  "PAYPO"                       , # Pouvez-vous préciser ?
  "SPECIAL"                     , # Quelle était la matière dominante de ce diplôme ?
  "SPECIAL_CODE_CITE_13"        , # Quelle était la matière dominante de ce diplôme (selon la nomenclature de la CITE)
  "SPECIALACT"                  , # Quelle était la matière dominante de ce diplôme ?
  "SPECIALACT_CODE_CITE_13"     , # Quelle était la matière dominante de ce diplôme (selon la nomenclature de la CITE)
  "AGDIP_conj"                  ,
  "DATDIP_conj"                 ,
  "DATDIP_C_1_conj"             ,
  "DIPLOM_conj"                 ,
  "DIPLOMACT_conj"              ,
  "FORMEL_conj"                 ,
  "NAIM_conj"                   ,
  "NAIP_conj"                   ,
  "NATIO1N1_conj"               ,
  "NATIO1N2_conj"               ,
  "NATIO1N3_conj"               ,
  "NATIO1N4_conj"               ,
  "NATIO1N5_conj"               ,
  "NATIO1N6_conj"               ,
  "NATIO2N_conj"                ,
  "NATIO2NO_conj"               ,
  "NATIOM_conj"                 ,
  "NATIOP_conj"                 ,
  "PAYM_conj"                   ,
  "PAYMO_conj"                  ,
  "PAYP_conj"                   ,
  "PAYPO_conj"                  ,
  "SPECIAL_conj"                ,
  "SPECIAL_CODE_CITE_13_conj"   ,
  "SPECIALACT_conj"             ,
  "SPECIALACT_CODE_CITE_13_conj" 
)

# ============================================================
# Enfance
# ============================================================

vars_enfance <- c(
  "M1_SQ1"            , # Regarder la télévision ?
  "M1_SQ2"            , # Ecouter de la musique ?
  "M1_SQ3"            , # Lire des livres ?
  "M1_SQ4"            , # Lire des BD ?
  "M1_SQ5"            , # Aller au cinéma ?
  "M1_SQ7"            , # Aller à la bibliothèque ou médiathèque municipale?
  "M1_SQ8"            , # Aller au musée, voir une exposition ou visiter un monument historique ?
  "M1_SQ9"            , # Aller au théâtre, voir un spectacle de danse ou un concert ?
  "M1_SQ10"           , # Aller au cirque ?
  "M1_SQ11"           , # Aller au zoo ?
  "M1_SQ12"           , # Aller dans un parc d’attraction ?
  "M201"              , # Chansons ou variétés françaises
  "M202"              , # Musiques du monde
  "M203"              , # Musiques traditionnelles
  "M204"              , # Variétés internationales
  "M205"              , # RnB
  "M206"              , # Musiques électroniques, techno
  "M207"              , # Hip hop, rap
  "M208"              , # Metal, hard rock
  "M209"              , # Pop, rock
  "M210"              , # Jazz
  "M211"              , # Opéra
  "M212"              , # Musique classique
  "M213"              , # Autre(s) genre(s) de musique
  "M214"              , # Aucun
  "M215"              , # (NSP)
  "M216"              , # (REF)
  "M301"              , # Chansons ou variétés françaises
  "M302"              , # Musiques du monde
  "M303"              , # Musiques traditionnelles
  "M304"              , # Variétés internationales
  "M305"              , # RnB
  "M306"              , # Musiques électroniques, techno
  "M307"              , # Hip hop, rap
  "M308"              , # Metal, hard rock
  "M309"              , # Pop, rock
  "M310"              , # Jazz
  "M311"              , # Opéra
  "M312"              , # Musique classique
  "M313"              , # Autre(s) genre(s) de musique
  "M314"              , # Aucun
  "M315"              , # (NSP)
  "M316"              , # (REF)
  "M401"              , # Votre mère
  "M402"              , # Votre père
  "M403"              , # La conjointe de votre père ou de votre mère
  "M404"              , # Le conjoint de votre père ou de votre mère
  "M405"              , # (Autre adulte 1)
  "M406"              , # (Autre adulte 2)
  "M407"              , # (Autre adulte 3)
  "M408"              , # (Autre cas (institution, famille d’accueil))
  "M409"              , # (NSP)
  "M410"              , # (REF)
  "M7_1"              , # Cet autre adulte 1
  "M7_2"              , # Cet autre adulte 2
  "M7_3"              , # Cet autre adulte 3
  "M81"               , # Vous nous avez déclaré précédemment avoir vécu le plus souvent avec plusieurs adultes, que...
  "M82"               , # Vous nous avez déclaré précédemment avoir vécu le plus souvent avec plusieurs adultes, que...
  "adulte_enfance1"   , # Premier adulte considéré comme parent (modalités de la question M4)
  "adulte_enfance2"   , # Second adulte considéré comme parent (modalités de la question M4)
  "adultes_enfance"   , # Adultes qui sont considérés comme les parents (deux digits : un digit représente un adulte...
  "M9_adulte1"        , # En quelle langue vous parlait habituellement la première des personnes que vous considérez...
  "M9_BIS1_adulte1"   , # Une ou plusieurs langues régionales de France
  "M9_BIS2_adulte1"   , # Une ou plusieurs langues étrangères
  "M10_adulte1"       , # De quelle/s langue/s s'agit-il ?
  "M10O_adulte1"      , # Pouvez-vous préciser ?
  "M10_RECODE_adulte1",
  "M11O_adulte1"      , # Pouvez-vous préciser ?
  "M11_adulte1"       , # Quelle est/était la langue maternelle de cette personne ?
  "M11_RECODE_adulte1",
  "M9_adulte2"        , # En quelle langue vous parlait habituellement la première des personnes que vous considérez...
  "M9_BIS1_adulte2"   , # Une ou plusieurs langues régionales de France
  "M9_BIS2_adulte2"   , # Une ou plusieurs langues étrangères
  "M10_adulte2"       , # De quelle/s langue/s s'agit-il ?
  "M10O_adulte2"      , # Pouvez-vous préciser ?
  "M10_RECODE_adulte2",
  "M11O_adulte2"      , # Pouvez-vous préciser ?
  "M11_adulte2"       , # Quelle est/était la langue maternelle de cette personne ?
  "M11_RECODE_adulte2",
  "M121"              , # Non, je maitrise uniquement le français
  "M122"              , # Oui, je maitrise une ou plusieurs langues étrangères
  "M123"              , # Oui, je maitrise une ou plusieurs langues régionales de France
  "M124"              , # (NSP)
  "M125"              , # (REF)
  "M13_RECODE"        , # De quelle/s langue/s s'agit-il ?
  "M14"               , # Et aujourd’hui, que ce soit chez vous, au travail ou pendant les vacances, vous arrive-t-i...
  "M15_RECODE"        , # De quelle/s langue/s s'agit-il ?
  "M16"               , # Avez-vous une religion ?
  "M17"               , # Quelle importance accordez-vous aujourd’hui à la religion dans votre vie ?
  "M18"               , # Quand vous aviez 12 ans, est-ce que vôtre père travaillait ou avait déjà travaillé ?
(si l...
  "STATUTECLCD_PER"   ,
  "STATUTCD_PER"      ,
  "CSTOT_PER"         ,
  "CLASSIFCD_PER1"    ,
  "CLASSIFCD_PER2"    ,
  "M19"               , # Quel est le diplôme le plus élevé que votre père a obtenu ?
  "M20"               , # Quand vous aviez 12 ans, est-ce que votre mère travaillait ou avait déjà travaillé ?
(si l...
  "STATUTECLCD_MER"   , # Dans cet emploi, votre mère était-elle
  "STATUTCD_MER"      , # STATUTCD_MER
  "CSTOT_MER"         ,
  "CLASSIFCD_MER1"    , # Dans cet emploi, votre mère était-elle classée comme...
  "CLASSIFCD_MER2"    , # Dans cet emploi, votre mère était-elle classée comme...
  "M21"                 # Quel est le diplôme le plus élevé que votre mère a obtenu ?
)

# ============================================================
# Logement
# ============================================================

vars_logement <- c(
  "CATLOGAC"   , # Pour vous, le logement où nous nous trouvons est plutôt ?
  "EMMENAG"    ,
  "EMMENAG_C_1", # Valeur déclarée
  "STOC"       , # Votre ménage occupe-t-il ce logement comme…
  "ACC"        , # Accepteriez-vous d’être recontacté(e) dans quelques mois pour reparler avec vous plus libr...
  "REG"          # Région dans laquelle se situe le logement du répondant
)

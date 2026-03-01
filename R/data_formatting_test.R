
source("Demarrage.R")  # setwd() + library() + import_survey() + extract_survey_metadata()


# df   <- import_survey("path/to/file.sas7bdat")
# meta <- extract_survey_metadata(df)
# # ... review/edit meta ...
# ai_classify_roles()
# meta <- ai_suggest_labels(meta)   # Haiku 4.5, auto-routes sync vs batch
# apply_survey_formats(df, meta) |> saveRDS("output.rds")
# generate_format_script(meta, "mysurvey", "path/to/file.sas7bdat")


# import_survey()	          # Auto-detect from extension; catalog_file for SAS
# extract_survey_metadata()	# Produces the varmod tibble; .detect_role() internal helper
# metadata_apply_codebook()	# Joins external Excel/CSV; updates variable and value labels
# metadata_fix_binary()	    # Standardises all Oui/Non vars via purrr::pmap
# apply_survey_formats()	  # Recode → rename → uppercase → re-label; wraps mf_from_varmod() logic
# generate_format_script()	# Writes a student-readable _recode.R with all 4 steps
# ai_call_claude()	       	# Synchronous httr2 call, Haiku 4.5 default
# ai_batch_submit()	      	# Submits batch job, returns batch $id
# ai_batch_retrieve()	    	# Polls until ended, retrieves JSONL, returns named list
# ai_classify_roles() 
# ai_suggest_labels()	    	# Routes sync/batch; validates label lengths before merge
# ai_suggest_varnames()	    # Routes sync/batch; warns on duplicate new_names

# # Test on : 
# gen1017_mf.R (SAS + external format files + binary vars)
# PRFE_mf.R (DTA + value labels via labelled)
# sc19_mf.R (SAS + Excel codebook)
# teo20_mf.R (Parquet + complex label cleaning → replace with ai_suggest_labels)
# Virage_mf.R (SAS catalog file)

source("data_formatting_pipeline.R")


datadir <- "~/Data/ELIPSS/Pratiques et représentations face à l’État"
df <- import_survey(file.path(datadir, "fr_cdsp_ddi_elipss_201601prfc1.dta"))

missing_num <- c(-1, 96, 99, 996, 999, 9996, 9999)

missing_chr <- c(
  "Non enquêté", "N'a pas répondu", "Valeur manquante", "Non concerné", "Ne sait pas",
  "Vous ne savez pas", # "Vous ne faites rien",
  "Refuse de répondre",
  "Je ne sais pas",
  "Non enquêté",
  "[Non concerné]",
  "[Non enquêté]",
  "Vous ne savez pas"
)
yes_labels = c("Oui", "Choisi")
no_labels  = c("Non", "Non choisi")


# Extract variable label and check manually on Excel file
meta <- extract_survey_metadata(df, 
  missing_num = missing_num, missing_chr = missing_chr, # max_levels_cat  = 20,
  yes_labels  = yes_labels,no_labels   = no_labels,
  # detected_roles  = detected_roles, desc_overrides  = desc_overrides
)
# ai_suggest_missing(meta, examples = missing_chr) # Find likely missing value labels 

# meta |> 
#   filter(var_name == "prfc1_CONTINST_1") |>
#   tidyr::unnest(c(values, labels, new_labels)) |> 
#   select(var_name, detected_role, values, labels, new_labels) |> 
#   print(n = 900)

export_metadata_excel(meta, path = file.path(datadir, "metadata_review.xlsx"), 
  # hide_cols = c("labels", "new_name", "doc_note"),
)




# # Classify variable roles with AI based on value labels
# ai_classify_roles(meta #, #, dry_run = TRUE    # role_examples    = list(O = "override / example"),
#   # max_labels_sent  = 8L, # batch_threshold  = 60L
# )

detected_roles <- c(
  prfc1_SATISFINSTA      = "factor_ordinal",  # "Satisfait(e)", "Ni satisfait(e), ni insatisfait(e)", "Insatisfait(e)"
  prfc1_SATISFINSTB      = "factor_ordinal",  # "Satisfait(e)", "Ni satisfait(e), ni insatisfait(e)", "Insatisfait(e)"
  prfc1_SATISFINSTC      = "factor_ordinal",  # "Satisfait(e)", "Ni satisfait(e), ni insatisfait(e)", "Insatisfait(e)"
  prfc1_SATISFINSTD      = "factor_ordinal",  # "Satisfait(e)", "Ni satisfait(e), ni insatisfait(e)", "Insatisfait(e)"
  prfc1_SATISFINSTE      = "factor_ordinal",  # "Satisfait(e)", "Ni satisfait(e), ni insatisfait(e)", "Insatisfait(e)"
  prfc1_TMPSINSTA        = "factor_ordinal",  # "Moins de 15 minutes", "Entre 15 et 30 minutes", "Plus de 30 minutes"
  prfc1_TMPSINSTB        = "factor_ordinal",  # "Moins de 15 minutes", "Entre 15 et 30 minutes", "Plus de 30 minutes"
  prfc1_TMPSINSTC        = "factor_ordinal",  # "Moins de 15 minutes", "Entre 15 et 30 minutes", "Plus de 30 minutes"
  prfc1_TMPSINSTD        = "factor_ordinal",  # "Moins de 15 minutes", "Entre 15 et 30 minutes", "Plus de 30 minutes"
  prfc1_TMPSINSTE        = "factor_ordinal",  # "Moins de 15 minutes", "Entre 15 et 30 minutes", "Plus de 30 minutes"
  prfc1_TMPSINSTF        = "factor_ordinal",  # "Moins de 15 minutes", "Entre 15 et 30 minutes", "Plus de 30 minutes"
  prfc1_ALAISE           = "factor_ordinal",  # "Très facile", "Facile", "Difficile"
  prfc1_DEMARC           = "factor_nominal",  # "Vous", "Votre conjoint(e)", "Un autre membre de la famille"
  prfc1_AIDES            = "factor_nominal",  # "Vous vous débrouillez seul(e)", "Un ami ou membre de la famille qui s'y connaît", "Un guichet de l'administration"
  prfc1_GROUPE_G11       = "factor_binary",  # "Groupe A", "Groupe B"
  prfc1_CONFINSTA        = "factor_ordinal",  # "Très confiance", "Plutôt confiance", "Plutôt pas confiance"
  prfc1_CONFINSTB        = "factor_ordinal",  # "Très confiance", "Plutôt confiance", "Plutôt pas confiance"
  prfc1_CONFINSTC        = "factor_ordinal",  # "Très confiance", "Plutôt confiance", "Plutôt pas confiance"
  prfc1_CONFINSTD        = "factor_ordinal",  # "Très confiance", "Plutôt confiance", "Plutôt pas confiance"
  prfc1_CONFINSTE        = "factor_ordinal",  # "Très confiance", "Plutôt confiance", "Plutôt pas confiance"
  prfc1_CONFINSTF        = "factor_ordinal",  # "Très confiance", "Plutôt confiance", "Plutôt pas confiance"
  prfc1_CONFINSTG        = "factor_ordinal",  # "Très confiance", "Plutôt confiance", "Plutôt pas confiance"
  prfc1_SUPSECSOC        = "factor_ordinal",  # "Très grave", "Assez grave", "Peu grave"
  prfc1_REDUCPRECIS      = "factor_nominal",  # "Police ou gendarmerie", "Justice", "École publique"
  prfc1_SITUIMPOT        = "factor_ordinal",  # "Vous n'avez pas rempli ou pas renvoyé de déclaration", "Vous avez rempli votre déclaration mais vous n'étiez pas imposable", "Vous avez rempli votre déclaration et vous étiez imposable"
  prfc1_MOYDECLA         = "factor_nominal",  # "Seul(e) ou avec votre conjoint(e)", "Avec un membre de votre famille", "Avec l'aide d'un ami ou d'un voisin"
  prfc1_ARGUEVIT         = "factor_nominal",  # "L'argent des impôts sert à financer des biens indispensables à la société", "Tous les autres contribuables payent donc il n'y a pas de raison que vous ne le", "Il y a trop de risques d'être sanctionné"
  prfc1_CONTEFISC        = "factor_nominal",  # "Vous avez contesté la décision et obtenu satisfaction", "Vous avez contesté la décision mais elle a été maintenue", "Vous avez accepté la décision sans la contester"
  prfc1_AVISIMPA         = "factor_nominal",  # "Juste", "Injuste", "Vous ne connaissez pas cet impôt"
  prfc1_AVISIMPB         = "factor_nominal",  # "Juste", "Injuste", "Vous ne connaissez pas cet impôt"
  prfc1_AVISIMPC         = "factor_nominal",  # "Juste", "Injuste", "Vous ne connaissez pas cet impôt"
  prfc1_AVISIMPD         = "factor_nominal",  # "Juste", "Injuste", "Vous ne connaissez pas cet impôt"
  prfc1_AVISIMPE         = "factor_nominal",  # "Juste", "Injuste", "Vous ne connaissez pas cet impôt"
  prfc1_AVISIMPF         = "factor_nominal",  # "Juste", "Injuste", "Vous ne connaissez pas cet impôt"
  prfc1_AVISIMPG         = "factor_nominal",  # "Juste", "Injuste", "Vous ne connaissez pas cet impôt"
  prfc1_AVISIMPH         = "factor_nominal",  # "Juste", "Injuste", "Vous ne connaissez pas cet impôt"
  prfc1_AVISIMPI         = "factor_nominal",  # "Juste", "Injuste", "Vous ne connaissez pas cet impôt"
  prfc1_CONNIMPA         = "factor_binary",  # "Proportionnel (le taux est fixe)", "Progressif (le taux augmente avec la richesse)", "Vous ne savez pas"
  prfc1_CONNIMPB         = "factor_binary",  # "Proportionnel (le taux est fixe)", "Progressif (le taux augmente avec la richesse)", "Vous ne savez pas"
  prfc1_CONNIMPC         = "factor_binary",  # "Proportionnel (le taux est fixe)", "Progressif (le taux augmente avec la richesse)", "Vous ne savez pas"
  prfc1_CONNIMPD         = "factor_binary",  # "Proportionnel (le taux est fixe)", "Progressif (le taux augmente avec la richesse)", "Vous ne savez pas"
  prfc1_EVALIMPOT        = "integer_scale",  # "Pas du tout satisfaisante", "--2--", "--3--"
  prfc1_REACIMP          = "factor_nominal",  # "De solliciter le conseil d'un professionnel que vous rémunérez pour trouver un m", "De vous rendre au guichet pour négocier un arrangement avec un agent", "De vous débrouiller pour diminuer ce que vous devez, même si c'est illégal"
  prfc1_SCAND1           = "factor_nominal",  # "Le commerçant qui omet de déclarer 20% de ses recettes", "Le couple de fonctionnaires qui ne déclare pas sa femme de ménage", "Le chef d'entreprise qui sous-estime son patrimoine pour payer moins d'impôt"
  prfc1_SCAND2           = "factor_nominal",  # "Le commerçant qui omet de déclarer 20% de ses recettes", "Le couple de fonctionnaires qui ne déclare pas sa femme de ménage", "Le chef d'entreprise qui sous-estime son patrimoine pour payer moins d''impôt"
  prfc1_DEPETAT1         = "factor_nominal",  # "Sécurité", "Défense", "Éducation et recherche"
  prfc1_DEPETAT2         = "factor_nominal",  # "Sécurité", "Défense", "Éducation et recherche"
  prfc1_DEPSUP           = "factor_nominal",  # "Augmenter les impôts de tous les ménages", "Augmenter les impôts des 10 % des ménages les plus riches", "Augmenter les impôts de toutes les entreprises"
  prfc1_SCOLPRIV         = "factor_ordinal",  # "Non, jamais", "Oui, une année scolaire ou moins", "Oui, entre deux et six années scolaires"
  prfc1_DEROG            = "factor_binary",  # "Oui, et je l'ai obtenue", "Oui, mais elle m'a été refusée", "Non, je n'ai jamais demandé de dérogation"
  prfc1_OPPOREAC         = "factor_nominal",  # "Vous avez contesté la décision et obtenu satisfaction", "Vous avez contesté la décision mais elle a été maintenue", "Vous avez accepté la décision sans la contester"
  prfc1_FEQREU1          = "factor_ordinal",  # "Plusieurs fois par an", "Une fois par an", "Pas tous les ans"
  prfc1_FEQREU2          = "factor_ordinal",  # "Plusieurs fois par an", "Une fois par an", "Pas tous les ans"
  prfc1_VOTPE1           = "factor_ordinal",  # "Oui, tous les ans", "Oui, mais pas tous les ans", "Jamais, ou presque jamais"
  prfc1_VOTPE2           = "factor_ordinal",  # "Oui, tous les ans", "Oui, mais pas tous les ans", "Jamais, ou presque jamais"
  prfc1_ASSOPE           = "factor_ordinal",  # "Oui, actuellement", "Oui, par le passé", "Non"
  prfc1_ENFPRIV          = "factor_ordinal",  # "Oui, certains", "Oui, tous", "Non"
  prfc1_SATISFEC         = "factor_ordinal",  # "Très satisfaisant", "Assez satisfaisant", "Peu satisfaisant"
  prfc1_OBJECO           = "factor_binary",  # "L'école devrait donner avant tout le sens de la discipline et de l'effort", "L'école devrait former avant tout des gens à l'esprit éveillé et critique", "Ne sait pas"
  prfc1_EVALECO          = "integer_scale",  # "Pas du tout satisfaisante", "--2--", "--3--"
  prfc1_PRIVSYMB         = "factor_ordinal",  # "Très positif", "Assez positif", "Ni positif, ni négatif"
  prfc1_POLGEN           = "factor_binary",  # "Un commissariat de police", "Une brigade de gendarmerie", "Vous ne savez pas"
  prfc1_CONTRID          = "factor_ordinal",  # "Aucune", "Une ou deux fois", "Entre trois et dix fois"
  prfc1_CONTPOL          = "factor_ordinal",  # "Aucune", "Une fois", "Deux fois ou plus"
  prfc1_DERCONT          = "factor_nominal",  # "La Police nationale", "La Police municipale", "La Gendarmerie nationale"
  prfc1_CONTMOTIF        = "factor_nominal",  # "À votre demande (dépôt de plainte ; demande d'intervention)", "Suite à une convocation", "Lors d'un contrôle ou d'une intervention"
  prfc1_POLACC           = "factor_nominal",  # "Courtoise", "Neutre", "Hostile"
  prfc1_POLREP           = "factor_ordinal",  # "Vous répondez avec bienveillance", "Vous vous limitez à répondre aux questions posées", "Vous êtes réticent(e) à coopérer"
  prfc1_POLPART          = "factor_nominal",  # "Oui", "Non", "Ça dépend du sujet"
  prfc1_INSEC            = "factor_ordinal",  # "Souvent", "Parfois", "Rarement"
  prfc1_EVALPOL          = "integer_scale",  # "Pas du tout satisfaisante", "--2--", "--3--"
  prfc1_JUSTICONT        = "factor_ordinal",  # "Non, jamais", "Oui, pour une affaire", "Oui, pour plusieurs affaires"
  prfc1_VOISREACT        = "factor_nominal",  # "Vous avez trouvé un arrangement", "Vous avez sollicité la police", "Vous avez saisi le juge de proximité"
  prfc1_VOISRESULT       = "factor_nominal",  # "Vous avez obtenu satisfaction", "Vous n'avez pas obtenu satisfaction", "Ce conflit n'est pas réglé"
  prfc1_TRAVRESULT       = "factor_nominal",  # "Vous avez obtenu satisfaction", "Vous n'avez pas obtenu satisfaction", "Ce conflit n'est pas réglé"
  prfc1_LOGRESULT        = "factor_nominal",  # "Vous avez obtenu satisfaction", "Vous n'avez pas obtenu satisfaction", "Ce conflit n'est pas réglé"
  prfc1_ADMINRESULT      = "factor_nominal",  # "Vous avez obtenu satisfaction", "Vous n'avez pas obtenu satisfaction", "Ce conflit n'est pas réglé"
  prfc1_COMMREACT        = "factor_nominal",  # "Vous avez trouvé un arrangement", "Vous avez contacté une association", "Vous avez contacté un avocat"
  prfc1_COMMRESULT       = "factor_nominal",  # "Vous avez obtenu satisfaction", "Vous n'avez pas obtenu satisfaction", "Ce conflit n'est pas réglé"
  prfc1_ESCRORESULT      = "factor_nominal",  # "Vous avez obtenu satisfaction", "Vous n'avez pas obtenu satisfaction", "Ce conflit n'est pas réglé"
  prfc1_STATUT           = "factor_nominal",  # "Fonctionnaire (État, hôpital ou collectivité locale)", "Salarié(e) dans le secteur public (sans le statut de fonctionnaire)", "Salarié(e) d'une entreprise, d'un artisan, d'une association ou d'un particulier"
  prfc1_EVALJUST         = "integer_scale",  # "Pas du tout satisfaisante", "--2--", "--3--"
  prfc1_AVISJUSTA        = "factor_ordinal",  # "Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord"
  prfc1_AVISJUSTB        = "factor_ordinal",  # "Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord"
  prfc1_AVISJUSTC        = "factor_ordinal",  # "Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord"
  prfc1_AVISJUSTD        = "factor_ordinal",  # "Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord"
  prfc1_AVISJUSTE        = "factor_ordinal",  # "Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord"
  prfc1_AVISINTERV       = "factor_binary",  # "C'est le rôle du juge de trancher en cas de désaccord entre deux personnes, quel", "Dans certains conflits, le juge n'est pas la personne la mieux à même de trouver", "Ne sait pas"
  prfc1_AVISJUGE_1       = "factor_ordinal",  # "Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord"
  prfc1_AVISJUGE_2       = "factor_ordinal",  # "Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord"
  prfc1_REPRETAT_ORDRE   = "other",  # "[Non concerné]", "1-2-5-3-7-8-4-6", "1-2-7-4-6-5-3-8"
  prfc1_CONFINST_ORDRE   = "other",  # "1-2-3-4-5-6-7", "1-2-3-4-7-5-6", "1-2-3-4-7-6-5"
  prfc1_AVISIMP_ORDRE    = "other",  # "1-2-3-5-6-4-7-8-9", "1-2-4-6-8-3-9-7-5", "1-2-4-7-9-8-5-6-3"
  prfc1_REPRETAT         = "factor_nominal",  # "Le président de la République", "Le gouvernement", "La Constitution"
  ea15_GRPB_SCN          = "factor_nominal",  # "Travail - travail", "Travail - pas travail", "Pas travail - travail"
  ea15_EAYYGRPB          = "other",  # "[Non concerné]", "[Non enquêté]", "Aucune variable reprise"
  ea15_GRPBCJT_SCN       = "factor_nominal",  # "Travail - travail", "Travail - pas travail", "Pas travail - travail"
  ea15_EAYYGRPBCJT       = "other",  # "[Non concerné]", "[Non enquêté]", "Aucune variable reprise"
  ea15_EAYYGRPBPRT       = "factor_nominal",  # "[Non concerné]", "[Non enquêté]", "Aucune variable reprise"
  ea15_EAYYANNEE         = "???",  # "Non enquêté"
  ea15_EAYYGRPD          = "other",  # "[Non concerné]", "[Non enquêté]", "Aucune variable reprise"
  eayy_VAGUE             = "factor_nominal",  # "Vague 1 : 2013", "Vague 2 : 2014", "Vague 3 : 2015"
  eayy_A1                = "factor_binary",  # "Un homme", "Une femme", "Non enquêté"
  ea15_A2A_rec           = "factor_ordinal",  # "Moins de 24 ans", "De 25 à 29 ans", "De 30 à 34 ans"
  ea15_A3_rec            = "factor_nominal",  # "Oui, de naissance", "(Oui par réintégration, naturalisation, par mariage, par déclaration ou option à", "Non"
  ea15_A3B_rec           = "factor_nominal",  # "France", "Union européenne", "Hors Union européenne"
  eayy_A3C_rec           = "factor_binary",  # "Union européenne", "Hors Union européenne", "Non concerné"
  ea15_A3E_rec           = "factor_binary",  # "Union européenne", "Hors Union européenne", "Non concerné"
  ea15_A4                = "factor_ordinal",  # "Célibataire (jamais légalement marié(e))", "Marié(e) (ou séparé(e) mais non divorcé(e))", "Divorcé(e)"
  ea15_A5                = "factor_ordinal",  # "Oui, avec quelqu'un qui vit dans votre logement", "Oui, avec quelqu'un qui ne vit pas dans votre logement", "Non"
  ea15_A5A               = "factor_nominal",  # "Marié(e)", "Pacsé(e)", "En union libre, en concubinage"
  ea15_A5C_rec           = "factor_ordinal",  # "Moins de 2 ans", "De 2 à 4 ans", "De 5 à 9 ans"
  ea15_B1                = "factor_nominal",  # "En emploi", "Apprenti(e) sous contrat ou en stage rémunéré", "Étudiant(e), élève, en formation ou en stage non rémunéré"
  ea15_B1B               = "factor_binary",  # "Est en activité", "N'est pas en activité", "Refuse de répondre"
  eayy_PCS18             = "factor_nominal",  # "Agriculteurs exploitants", "(Artisans, commerçants et chefs d'entreprise)", "Professions libérales et assimilés"
  eayy_PCS6              = "factor_nominal",  # "Agriculteurs exploitants", "Artisans, commerçants et chefs d'entreprise", "Cadres et professions intellectuelles supérieures"
  ea15_B2_rec            = "factor_nominal",  # "Salarié(e) de l'État", "Salarié(e) d'une collectivité territoriale, des HLM", "Salarié(e) de la fonction publique hospitalière"
  ea15_B4_rec            = "factor_binary",  # "Aucun salarié", "Au moins 1 salarié", "Non concerné"
  ea15_B5_rec            = "factor_nominal",  # "Contrat à durée indéterminée", "(Contrat à durée déterminée (dont contrat saisonnier))", "Contrat d'interim ou de travail temporaire"
  ea15_B5A               = "factor_binary",  # "À temps complet", "À temps partiel", "Non concerné"
  ea15_B5B_rec           = "factor_ordinal",  # "Moins d'un mi-temps (moins de 50%)", "Mi-temps (50%)", "Entre 50 et 80%"
  ea15_B5C               = "factor_nominal",  # "Pour exercer une autre activité professionnelle, ou suivre des études ou une for", "Pour raison de santé", "Vous n'avez pas trouvé d'emploi à temps plein"
  ea15_B6A_rec           = "factor_nominal",  # "Manœuvre ou ouvrier(e) spécialisé(e)", "Ouvrier(e) qualifié(e) ou hautement qualifié(e), technicien(ne) d'atelier", "Technicien(ne)"
  ea15_B6B_rec           = "factor_nominal",  # "Manœuvre ou ouvrier(e) spécialisé(e)", "Ouvrier(e) qualifié(e) ou hautement qualifié(e), technicien(ne) d'atelier", "Technicien(ne)"
  ea15_B7B_rec           = "factor_nominal",  # "Production, chantier, exploitation", "Installation, réparation, maintenance", "Gardiennage, nettoyage, entretien ménager"
  ea15_B8_rec            = "factor_ordinal",  # "Moins de 10", "10 et 49", "50 à 499"
  ea15_B10A              = "factor_ordinal",  # "Oui, depuis moins d'un an", "Oui, depuis un an ou plus", "Non"
  ea15_B10AA             = "factor_ordinal",  # "Oui, depuis moins d'un an", "Oui, depuis un an ou plus", "Non"
  ea15_B10D              = "factor_ordinal",  # "Oui, et la durée totale de la (des) période(s) de chômage n'a pas dépassé 3 mois", "Oui, et la durée totale de la (des) période(s) de chômage a dépassé 3 mois cumul", "Non, aucune"
  eayy_B18_rec           = "factor_ordinal",  # "(Aucun diplôme)", "CEP (certificat d'études primaires)", "BEPC, brevet élementaire, brevet des collèges"
  eayy_B18C              = "factor_ordinal",  # "Niveau VI : sans diplôme ou Brevet des collèges", "Niveau V : CAP ou BEP", "Niveau IV : Baccalauréat général, technologique ou professionnel"
  ea15_B1CJT             = "factor_nominal",  # "Occupe un emploi", "Apprenti(e) sous contrat ou en stage rémunéré", "Étudiant(e), élève, en formation ou en stage non rémunéré"
  ea15_B1BCJT            = "factor_binary",  # "Est en activité", "N'est pas en activité", "Non concerné"
  ea15_PCS6CJT           = "factor_nominal",  # "Agriculteurs exploitants", "Artisans, commerçants et chefs d'entreprise", "Cadres et professions intellectuelles supérieures"
  ea15_B2CJT_rec         = "factor_nominal",  # "Salarié(e) de l'État", "Salarié(e) d'une collectivité territoriale, des HLM", "Salarié(e) de la fonction publique hospitalière"
  ea15_B4CJT_rec         = "factor_binary",  # "Aucun salarié", "Au moins 1 salarié", "Non concerné"
  ea15_B5ACJT            = "factor_binary",  # "À temps complet", "À temps partiel", "Non concerné"
  ea15_B5BCJT_rec        = "factor_ordinal",  # "Moins d'un mi-temps (moins de 50%)", "Mi-temps (50%)", "Entre 50 et 80%"
  ea15_B6ACJT_rec        = "factor_nominal",  # "Manœuvre ou ouvrier(e) spécialisé(e)", "Ouvrier(e) qualifié(e) ou hautement qualifié(e), technicien(ne) d'atelier", "Technicien(ne)"
  ea15_B6BCJT_rec        = "factor_nominal",  # "Manœuvre ou ouvrier(e) spécialisé(e)", "Ouvrier(e) qualifié(e) ou hautement qualifié(e), technicien(ne) d'atelier", "Technicien(ne)"
  ea15_B7BCJT_rec        = "factor_nominal",  # "Production, chantier, exploitation", "Installation, réparation, maintenance", "Gardiennage, nettoyage, entretien ménager"
  ea15_B8CJT_rec         = "factor_ordinal",  # "Moins de 10", "10 et 49", "50 à 499"
  ea15_B10ACJT           = "factor_ordinal",  # "Oui, depuis moins d'un an", "Oui, depuis un an ou plus", "Non"
  ea15_B10AACJT          = "factor_ordinal",  # "Oui, depuis moins d'un an", "Oui, depuis un an ou plus", "Non"
  ea15_B10DCJT           = "factor_ordinal",  # "Oui, et la durée totale de la (des) période(s) de chômage n'a pas dépassé 3 mois", "Oui, et la durée totale de la (des) période(s) de chômage a dépassé 3 mois cumul", "Non, aucune"
  ea15_B18CJT_rec        = "factor_ordinal",  # "(Aucun diplôme)", "CEP (certificat d'études primaires)", "BEPC, brevet élementaire, brevet des collèges"
  ea15_B18CCJT           = "factor_ordinal",  # "Niveau VI : sans diplôme ou Brevet des collèges", "Niveau V : CAP ou BEP", "Niveau IV : Baccalauréat général, technologique ou professionnel"
  ea15_B20               = "factor_nominal",  # "Agriculteur exploitant, aide familiale dans l'agriculture", "Artisan, commerçant, chef d'entreprise", "Cadre, profession libérale"
  ea15_B21               = "factor_nominal",  # "Agriculteur exploitant, aide familiale dans l'agriculture", "Artisan, commerçant, chef d'entreprise", "Cadre, profession libérale"
  ea15_B22_rec           = "factor_nominal",  # "France", "Union européenne", "Hors Union européenne"
  ea15_B23_rec           = "factor_nominal",  # "France", "Union européenne", "Hors Union européenne"
  ea15_C1_rec            = "factor_ordinal",  # "1 habitant", "2 habitants", "3 habitants"
  ea15_C1JEU             = "integer_count",  # "0 jeune", "1 jeune", "2 jeunes"
  ea15_C8A_rec           = "factor_ordinal",  # "1 enfant", "2 enfants", "3 enfants ou plus"
  ea15_C8B_rec           = "integer_count",  # "0 enfant", "1 enfant", "2 enfants"
  ea15_D0_rec            = "factor_nominal",  # "Oui, dans la même commune", "Oui, dans une autre commune", "Non"
  ea15_D1_rec            = "factor_nominal",  # "Propriétaire (ou copropriétaire) : vous ne payez pas de crédit immobilier pour c", "Accédant à la propriété : vous payez toujours un crédit immobilier pour ce logem", "(Locataire, sous-locataire)"
  ea15_D2                = "factor_nominal",  # "Votre conjoint", "Un autre membre de votre foyer", "Une autre personne de votre famille"
  ea15_D3                = "factor_nominal",  # "Une maison individuelle", "Un appartement", "Autre"
  ea15_D4_rec            = "integer_count",  # "1 pièce", "2 pièces", "3 pièces"
  ea15_D5_rec            = "factor_ordinal",  # "Moins de 2 ans", "Entre 2 à 5 ans", "Entre 5 et 10 ans"
  ea15_D6                = "factor_nominal",  # "Maisons dispersées, hors agglomération", "Maisons en lotissement, en quartier pavillonnaire ou en ville", "Immeubles en ville (autres que cité ou grand ensemble)"
  eayy_E2A_rec           = "factor_ordinal",  # "(Moins de 800 €)", "De 800 à moins de 1000 €", "De 1000 à moins de 1200 €"
  eayy_E2AUC             = "factor_ordinal",  # "Moins de 650 €", "De 650 à moins de 950 €", "De 950 à moins de 1200 €"
  ea15_E5C               = "factor_ordinal",  # "S'améliorer", "Rester sensiblement la même", "S'aggraver"
  eayy_E5                = "factor_ordinal",  # "Vous ne pouvez pas y arriver sans faire de dettes", "Vous y arrivez difficilement", "C'est juste il faut faire attention"
  eayy_F1_rec            = "factor_ordinal",  # "Au moins une fois par semaine", "Une, deux ou trois fois par mois", "Plusieurs fois dans l'année (mais moins d'une fois par mois)"
  ea15_F3                = "factor_ordinal",  # "Il n'y en a pas", "1 ou 2 personnes", "Entre 3 et 5 personnes"
  ea15_F4                = "factor_ordinal",  # "Très seul(e)", "Plutôt seul(e)", "Plutôt entouré(e)"
  ea15_F5                = "factor_binary",  # "On peut faire confiance à la plupart des gens", "On n'est jamais assez prudent quand on a affaire aux autres", "Ne sait pas"
  ea15_G1                = "factor_ordinal",  # "Moins de 2 heures par jour", "De 2 à 4 heures par jour", "Plus de 4 heures par jour"
  ea15_G2                = "factor_ordinal",  # "Aucun", "Moins de 6", "De 6 à moins de 12"
  ea15_G3A               = "factor_ordinal",  # "Jamais", "1 à 3 fois", "4 à 6 fois"
  ea15_G3B               = "factor_ordinal",  # "Jamais", "1 à 3 fois", "4 à 6 fois"
  ea15_G3C               = "factor_ordinal",  # "Jamais", "1 à 3 fois", "4 à 6 fois"
  ea15_G4                = "factor_ordinal",  # "Régulièrement tout au long de l'année", "De temps en temps tout au long de l'année", "Seulement pendant certaines périodes ou pendant les vancances"
  ea15_H2                = "factor_ordinal",  # "Pas du tout d'importance", "Un peu d'importance", "Assez d'importance"
  eayy_I1                = "factor_ordinal",  # "Beaucoup", "Assez", "Peu"
  eayy_I8                = "integer_scale",  # "Gauche", "--1--", "--2--"
  eayy_J1                = "factor_ordinal",  # "Très bonne", "Bonne", "Assez bonne"
  eayy_K3                = "factor_ordinal",  # "Tous les jours ou presque", "Plusieurs fois par semaine", "Une fois par semaine"
  insee_NUTS1            = "factor_nominal",  # "FR1 : Région parisienne", "FR2 : Bassin parisien", "FR3 : Nord"
  insee_CATAEU2010       = "factor_nominal",  # "Commune appartenant à un grand pôle (10 000 emplois ou plus)", "Commune appartenant à la couronne d'un grand pôle", "Commune multipolarisée des grandes aires urbaines"
  insee_TAU2014          = "factor_nominal",  # "Commune hors aire urbaine", "Commune appartenant à une aire urbaine de moins de 15 000 habitants", "Commune appartenant à une aire urbaine de 15 000 à 19 999 habitants"
  insee_TUU2014          = "factor_nominal",  # "Commune rurale", "Commune appartenant à une unité urbaine de 2 000 à 4 999 habitants", "Commune appartenant à une unité urbaine de 5 000 à 9 999 habitants"
  cal_SEXE               = "factor_binary",  # "Homme", "Femme", "Valeur manquante"
  cal_AGE1               = "factor_ordinal",  # "18-24 ans", "25-34 ans", "35-44 ans"
  cal_AGE2               = "factor_ordinal",  # "18-22 ans", "23-34 ans", "35-44 ans"
  cal_NAT                = "factor_nominal",  # "Français(e)", "A acquis la nationalité française", "Nationalité étrangère"
  cal_DIPL               = "factor_ordinal",  # "Aucun/CEP/BEPC", "CAP/BEP", "Bac/Bac+2"
  cal_ZEAT               = "factor_nominal"#,  # "Région parisienne", "Bassin parisien", "Nord"
)

desc_overrides <- c(
  prfc1_SATISFINSTA      = TRUE,  # [factor_ordinal]
  prfc1_SATISFINSTB      = TRUE,  # [factor_ordinal]
  prfc1_SATISFINSTC      = TRUE,  # [factor_ordinal]
  prfc1_SATISFINSTD      = TRUE,  # [factor_ordinal]
  prfc1_SATISFINSTE      = TRUE,  # [factor_ordinal]
  prfc1_TMPSINSTA        = FALSE,  # [factor_ordinal]
  prfc1_TMPSINSTB        = FALSE,  # [factor_ordinal]
  prfc1_TMPSINSTC        = FALSE,  # [factor_ordinal]
  prfc1_TMPSINSTD        = FALSE,  # [factor_ordinal]
  prfc1_TMPSINSTE        = FALSE,  # [factor_ordinal]
  prfc1_TMPSINSTF        = FALSE,  # [factor_ordinal]
  prfc1_ALAISE           = TRUE,  # [factor_ordinal]
  prfc1_GROUPE_G11       = FALSE,  # [factor_binary]
  prfc1_CONFINSTA        = TRUE,  # [factor_ordinal]
  prfc1_CONFINSTB        = TRUE,  # [factor_ordinal]
  prfc1_CONFINSTC        = TRUE,  # [factor_ordinal]
  prfc1_CONFINSTD        = TRUE,  # [factor_ordinal]
  prfc1_CONFINSTE        = TRUE,  # [factor_ordinal]
  prfc1_CONFINSTF        = TRUE,  # [factor_ordinal]
  prfc1_CONFINSTG        = TRUE,  # [factor_ordinal]
  prfc1_SUPSECSOC        = TRUE,  # [factor_ordinal]
  prfc1_SITUIMPOT        = FALSE,  # [factor_ordinal]
  prfc1_CONNIMPA         = NA,  # [factor_binary]
  prfc1_CONNIMPB         = NA,  # [factor_binary]
  prfc1_CONNIMPC         = NA,  # [factor_binary]
  prfc1_CONNIMPD         = NA,  # [factor_binary]
  prfc1_SCOLPRIV         = FALSE,  # [factor_ordinal]
  prfc1_DEROG            = TRUE,  # [factor_binary]
  prfc1_FEQREU1          = TRUE,  # [factor_ordinal]
  prfc1_FEQREU2          = TRUE,  # [factor_ordinal]
  prfc1_VOTPE1           = TRUE,  # [factor_ordinal]
  prfc1_VOTPE2           = TRUE,  # [factor_ordinal]
  prfc1_ASSOPE           = FALSE,  # [factor_ordinal]
  prfc1_ENFPRIV          = FALSE,  # [factor_ordinal]
  prfc1_SATISFEC         = TRUE,  # [factor_ordinal]
  prfc1_OBJECO           = FALSE,  # [factor_binary]
  prfc1_PRIVSYMB         = TRUE,  # [factor_ordinal]
  prfc1_POLGEN           = NA,  # [factor_binary]
  prfc1_CONTRID          = FALSE,  # [factor_ordinal]
  prfc1_CONTPOL          = FALSE,  # [factor_ordinal]
  prfc1_POLREP           = FALSE,  # [factor_ordinal]
  prfc1_INSEC            = TRUE,  # [factor_ordinal]
  prfc1_JUSTICONT        = FALSE,  # [factor_ordinal]
  prfc1_AVISJUSTA        = TRUE,  # [factor_ordinal]
  prfc1_AVISJUSTB        = TRUE,  # [factor_ordinal]
  prfc1_AVISJUSTC        = TRUE,  # [factor_ordinal]
  prfc1_AVISJUSTD        = TRUE,  # [factor_ordinal]
  prfc1_AVISJUSTE        = TRUE,  # [factor_ordinal]
  prfc1_AVISINTERV       = NA,  # [factor_binary]
  prfc1_AVISJUGE_1       = TRUE,  # [factor_ordinal]
  prfc1_AVISJUGE_2       = TRUE,  # [factor_ordinal]
  eayy_A1                = TRUE,  # [factor_binary]
  ea15_A2A_rec           = FALSE,  # [factor_ordinal]
  eayy_A3C_rec           = FALSE,  # [factor_binary]
  ea15_A3E_rec           = FALSE,  # [factor_binary]
  ea15_A4                = FALSE,  # [factor_ordinal]
  ea15_A5                = FALSE,  # [factor_ordinal]
  ea15_A5C_rec           = FALSE,  # [factor_ordinal]
  ea15_B1B               = FALSE,  # [factor_binary]
  ea15_B4_rec            = FALSE,  # [factor_binary]
  ea15_B5A               = TRUE,  # [factor_binary]
  ea15_B5B_rec           = FALSE,  # [factor_ordinal]
  ea15_B8_rec            = FALSE,  # [factor_ordinal]
  ea15_B10A              = FALSE,  # [factor_ordinal]
  ea15_B10AA             = FALSE,  # [factor_ordinal]
  ea15_B10D              = FALSE,  # [factor_ordinal]
  eayy_B18_rec           = FALSE,  # [factor_ordinal]
  eayy_B18C              = FALSE,  # [factor_ordinal]
  ea15_B1BCJT            = FALSE,  # [factor_binary]
  ea15_B4CJT_rec         = FALSE,  # [factor_binary]
  ea15_B5ACJT            = TRUE,  # [factor_binary]
  ea15_B5BCJT_rec        = FALSE,  # [factor_ordinal]
  ea15_B8CJT_rec         = FALSE,  # [factor_ordinal]
  ea15_B10ACJT           = FALSE,  # [factor_ordinal]
  ea15_B10AACJT          = FALSE,  # [factor_ordinal]
  ea15_B10DCJT           = FALSE,  # [factor_ordinal]
  ea15_B18CJT_rec        = FALSE,  # [factor_ordinal]
  ea15_B18CCJT           = FALSE,  # [factor_ordinal]
  ea15_C1_rec            = FALSE,  # [factor_ordinal]
  ea15_D5_rec            = FALSE,  # [factor_ordinal]
  ea15_E5C               = FALSE,  # [factor_ordinal]
  eayy_E5                = FALSE,  # [factor_ordinal]
  eayy_F1_rec            = FALSE,  # [factor_ordinal]
  ea15_F3                = FALSE,  # [factor_ordinal]
  ea15_F4                = TRUE,  # [factor_ordinal]
  ea15_F5                = FALSE,  # [factor_binary]
  ea15_G1                = FALSE,  # [factor_ordinal]
  ea15_G3A               = FALSE,  # [factor_ordinal]
  ea15_G3B               = FALSE,  # [factor_ordinal]
  ea15_G3C               = FALSE,  # [factor_ordinal]
  ea15_G4                = TRUE,  # [factor_ordinal]
  ea15_H2                = FALSE,  # [factor_ordinal]
  eayy_I1                = FALSE,  # [factor_ordinal]
  eayy_J1                = TRUE,  # [factor_ordinal]
  eayy_K3                = TRUE,  # [factor_ordinal]
  cal_SEXE               = TRUE,  # [factor_binary]
  cal_AGE1               = FALSE,  # [factor_ordinal]
  cal_AGE2               = FALSE,  # [factor_ordinal]
  cal_DIPL               = FALSE#,  # [factor_ordinal]
)



meta <- extract_survey_metadata(df, 
  missing_num = missing_num, missing_chr = missing_chr,  # max_levels_cat  = 20,
  yes_labels  = yes_labels, no_labels   = no_labels,
  detected_roles  = detected_roles, desc_overrides  = desc_overrides
)
# meta |> 
#   filter(var_name == "prfc1_CONTINST_1") |>
#   tidyr::unnest(c(values, labels, new_labels)) |> 
#   select(var_name, detected_role, values, labels, new_labels) |> 
#   print(n = 900)

# export_metadata_excel(meta, path = file.path(datadir, "metadata_review.xlsx"), 
#   # hide_cols = c("labels", "new_name", "doc_note"),
# )


# Use AI to suggest new labels for factors (binary, ordinal, nominal)
ai_suggest_labels()







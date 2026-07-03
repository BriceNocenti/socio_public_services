
source("Demarrage.R")  # setwd() + library() + import_survey() + extract_survey_metadata()
source("R/data_formatting_pipeline.R")

datadir  <- "~/Data/Enquête Emploi 2023-2024"
data_name <- "ee"
# datapath     <- file.path(datadir, "Fichiers_de_données/SPSS/IE0233A_F.sav") |> path.expand()
# catalog_file <- file.path(datadir, "SAS/formats.sas7bcat"    ) |> path.expand()
datapath <- file.path(datadir, c(
  "lil-1687b-Donnees_PARQUET/indiv231.parquet", 
  "lil-1687b-Donnees_PARQUET/indiv232.parquet", 
  "lil-1687b-Donnees_PARQUET/indiv233.parquet",
  "lil-1687b-Donnees_PARQUET/indiv234.parquet",
  "lil-1734b-Donnees_PARQUET/indiv241.parquet", 
  "lil-1734b-Donnees_PARQUET/indiv242.parquet",
  "lil-1734b-Donnees_PARQUET/indiv243.parquet", 
  "lil-1734b-Donnees_PARQUET/indiv244.parquet"
  ))
sas_labels_script <- file.path(datadir, "lil-1734b-Documentation/Labels_modalités/labels_sas_lil-1734b.txt")

# Single JSON file that grows brick-by-brick through the workflow.
# Alternate AI suggestions and manual edits directly in this .json for best results.
meta_json            <- file.path(datadir, paste0(data_name, "_meta.json"))     # stringr::str_replace(datapath, "\\.[^\\.]+$", ".survey_meta.json")
metadata_review_path <- file.path(datadir, paste0(data_name, "_meta_review.xlsx")) # stringr::str_replace(datapath, "\\.[^\\.]+$", "_metadata_review.xlsx")
format_script_path   <- file.path(datadir, paste0(data_name, "_mf.R")) 


df <- import_survey(datapath, catalog_file = catalog_file)
# df |> labelled::get_variable_labels()
# df <- arrow::open_dataset(datapaths, unify_schemas = TRUE) |> collect() |> as_tibble()



missing_num <- c(-1, 9999, 99999999) # 99, 888, 999, 8888, 9999, 88,  96, 996, 9996,  # integer()

missing_chr <- c(
  "Non enquêté", "N'a pas répondu", "Valeur manquante", "Non concerné", "Ne sait pas",
  "Vous ne savez pas", "Non réponse", "Non réponse ou un seul parent",
  "Not applicable", "Not stated",
  "Refus", "Refuse de répondre", "Refus / Ne sait pas", 
  "Je ne sais pas",
  "[Non concerné]", "Non concerné-e", 
  "[Non enquêté]",  "[Aucune]", 
  "NVPD", "NSP", "NVPD/NSP", "Niveau indéterminé", "NSP/NVPD", "[NR]", 
  "Ne souhaite pas répondre", "Ne sais pas", "Ne veut pas dire", "incohérence", 
  "NSP ou préfère indiquer l'année",
  "NSP ou préfère indiquer l'année de naissance", "NSP ou préfère dire depuis quelle année", "NSP ou préfère dire à quel âge", "NSP ou préfère dire l'année de début de la cohabitation",
  "NSP ou préfère dire en quelle année","NSP (ou préfère dire en quelle année)", "NSP (ou préfère indiquer une période)", "NSP run",
  "NSP, non renseigné",
  "[Ne connait pas son père]", "[Ne connait pas sa mère]", "[N'a jamais quitté le domicile des parents/le lieu dans lequ",
  "[Ne sait pas]", "[Refus]", "[NSP la date mais il y a 15 ans ou moins]",
  "[NSP la date mais il y a plus de 15 ans]", "NSP la date mais il y a 15 ans ou moins", "NSP la date mais il y a plus de 15 ans", 
  "[NSP]",
  "Logement non-interrogé (LNI)",
  "Non réponse",
  "non réponse",
  "Personne non-interrogée (LNI, HANDISOL, INACTAGISOL)",
  "Ne sait pas",
  "Non codés",
  "Sans objet, non concerné",
  "Non-répondants",
  "Inconnu",
  "Non classés dans la catégorisation",
  "Hors champ",
  "Hors champ de la catégorisation (libellé ne figurant pas dans la liste ou profession non codée en PCS)",
  "Hors champ de la catégorisation (libellé ne figurant pas dans",
  "Hors liste spécialités (codage manuel)",
  "Non classés selon la catégorisation",
  "Inconnu ou ne vit qu'avec un seul parent (pour les moins de 15 ans)",
  "Not stated",
  "Hors champ de la catégorisation (profession non codée en PCS)" 
)
yes_labels <- c("Oui", "Choisi", "Déclare"   )
no_labels  <- c("Non", "Non choisi", "Ne déclare pas" )


# # Make dummy table for unitary tests
# df |> 
#   select(     # # Import R class -> role to be detected
#     METRODOM, # character -> factor_binary (should be detected algorithmically by existing util function, based on 2 non-null levels + value labels in metadata table )
#     PCS1    , # character -> factor_nominal
#     AGED    , # character -> factor_ordinal (should be detected as factor_nominal, then passed to AI for ordinal detection)
#     HCONT   , # numeric -> double  (should be detected algorithmically by existing util function)
#     NAIA      # numeric -> integer (should be detected algorithmically by existing util function)
#   ) |> 
#   map(class)
#   make_dummy_tibble(seed = 42, clipboard = TRUE)
# # df |> tab(NAIA)
# # df |> select(where(~ !is.character(.))) |> map(class) 


# ============================================================
# STEP 1 — First run: create meta_json + auto-detect roles
# ============================================================
extract_survey_metadata(df,
  missing_num = missing_num, missing_chr = missing_chr,
  yes_labels  = yes_labels,  no_labels   = no_labels, 
  meta_json   = meta_json,   # json created here
  sas_format_file = sas_labels_script
)
rstudioapi::navigateToFile(meta_json, line = 1)  # open JSON for review/editing

# options(arrow.unsafe_metadata = TRUE) # what does it do ?
ai_suggest_missing(meta_json) # examples = NULL, max_vals = 10L, 
# then add to missing_chr above, and re-run extract_survey_metadata() 

# # Liste des variables dont il faut aller chercher la nomenclature
# meta |> 
#   filter(detected_role == "factor_nominal" & map_int(values, length) > 10) |> 
#   select(var_name, var_label, n_distinct_data) |> 
#   print(n = 900)


# # Ajouter nomenclatures
# create_nomenclatures_json( # Une seule fois : créer JSON nomenclatures
#   naf_path = "~/Data/Nomenclatures/int_courts_naf_rev_2.xls",
#   fap_path = "~/Data/Nomenclatures/Dares_Arborescence_FAP2021.xlsx",
#   pcs_path = "~/Data/Nomenclatures/Nomenclature_4Nemboites_PCS2020.xlsx"
# )
# detect_nomenclature_vars(meta) |> dput()
apply_nomenclatures(meta_json, 
  mapping = list(FAP_PROFPR = "FAP2021_341", NAFG038N = "NAF_38N", 
    NAFG038UN = "NAF_38N", NAFG129N = "NAF_129N", NAFG129UN = "NAF_129N", 
    NAFN = "NAF_rev2", NAFUN = "NAF_rev2", PCS3 = "PCS2020_N3", 
    PCS4 = "PCS2020_N4",
    PCS2J3 = "PCS2020_N3", PCS2J4 = "PCS2020_N4", APCS3 = "PCS2020_N3", 
    APCS4 = "PCS2020_N4", FAP_PROFA = "FAP2021_341", 
    PCSPAR1_3 = "PCS2020_N3", PCSPAR1_4 = "PCS2020_N4", PCSPAR2_3 = "PCS2020_N3", 
    PCSPAR2_4 = "PCS2020_N4"), 
  nom_json  = "instructions/nomenclatures_INSEE.json"
)
# rstudioapi::navigateToFile("instructions/nomenclatures_INSEE.json", line = 1) 
rstudioapi::navigateToFile(meta_json, line = 1)  # open JSON for review/editing

# # Make dummy table for unitary tests
# df |> 
#   select(       # # Import R class -> role to be detected
#     FAP_PROFA   , # character -> factor_nominal
#     NAFG038N    , # character -> factor_nominal
#     NAFG129N    , # character -> factor_nominal
#     NAFN        , # character -> factor_nominal
#     PCS3        , # character -> factor_nominal
#     PCS4
#   ) |> 
#   # map(class)
#   make_dummy_tibble(seed = 42, clipboard = TRUE, max_unique = 60L)
# # df |> tab(NAIA)
# # df |> select(where(~ !is.character(.))) |> map(class) 


# ============================================================
# STEP 2 — AI classify roles (writes role/order directly to meta_json)
# ============================================================
ai_classify_roles(meta_json, ordinal_desc = TRUE, dry_run = TRUE) # preview prompts + write stats
ai_classify_roles(meta_json, ordinal_desc = TRUE) # , log_raw_answer = TRUE, max_labels_sent = 5L
# # Review role / order in meta_json manualy, then reimport modified meta
# # Search for "unclear" and "other" roles and decide their true role
rstudioapi::navigateToFile(meta_json, line = 1)  # open JSON for review/editing

# invert_ordinal_order(meta_json)

# # Export Excel for visual review (better to do it in json directly ; and Excel export bugging)
# export_metadata_excel(meta, path = metadata_review_path)
# # utils::browseURL(metadata_review_path)

# # # Build final classify role prompt to look for errors
# # source("R/data_formatting_pipeline.R")
# .build_classify_system_prompt("instructions/classify_roles_prompt.md", ordinal_desc = TRUE, max_labels_sent = 5L) |>
#   writeLines("tests/built_test_classify_system_prompt.md")


# =================================================================================
# STEP 4 — Add level counts/freqs in meta_json and merge ordinal levels (rare levels, etc.)
# =================================================================================
metadata_add_level_stats(meta_json, df = df)


# # Algorithmic merging of ordinal levels is not working : only AI could do it well
# meta <- metadata_merge_ordinal_levels(meta, meta_json = meta_json, min_pct = 0.05) # min_n = 0L

# ### WARNING #####################################################
# ### ai_merge_levels() messes with ordinal levels order ##########
# ### when very few counts, it can merge all in one level… ########
# #################################################################
# ai_merge_levels(meta_json, dry_run = TRUE)   # preview prompts + write stats
# ai_merge_levels(meta_json, use_batch = TRUE) # max_levels = 150L, max_levels_in_single_var = 30, vars = NULL, ordinal_desc  = FALSE,
# rstudioapi::navigateToFile(meta_json, line = 1) # open JSON for review/editing
# # Look at warnings() to check for levels missing (most where very special cases that failed)

# Just add a function to binarise batteries of questions ? 






# ============================================================
# STEP 5 — AI label suggestions (enriches meta_json with new_label)
# ============================================================
# df_sample <- df |> slice_sample(n = 50000)
# meta <- extract_survey_metadata(df_sample, meta_json = meta_json) # Reload after editing labels in meta_json
ai_suggest_labels(meta_json, use_batch = TRUE, dry_run = TRUE)   # preview prompts + write stats
ai_suggest_labels(meta_json, use_batch = TRUE) # max_levels = 150L, vars = NULL, ordinal_desc  = FALSE,
# rstudioapi::navigateToFile(meta_json, line = 1) # open JSON for manual review/editing

# # Check for factors that were not processed / have no "new_label" field
# ai_suggest_labels(meta_json, dry_run = TRUE) 
# ai_suggest_labels(meta_json, use_batch = TRUE) # RERUN only factors with missing new_label



# ============================================================
# STEP 6 — AI variable name suggestions (fills new_name in meta_json)
# ============================================================
ai_suggest_varnames(meta_json, use_batch = TRUE, dry_run = TRUE) # preview prompts
ai_suggest_varnames(meta_json, chunk_size = 400L, use_batch = TRUE)
# rstudioapi::navigateToFile(meta_json, line = 1) # open JSON for manual review/editing 



# ============================================================
# STEP 7 — Final reload and apply
# ============================================================
generate_format_script(meta_json, df_name = data_name, output_path = format_script_path)
# rstudioapi::navigateToFile(format_script_path, line = 1)




# Test the generated script  ---- 
source("R/data_formatting_pipeline.R", encoding = "UTF-8")

data_name # look here

ee <- df    # replace manually there
source(format_script_path, encoding = "UTF-8")


# Random tables
library(tabxplor)
options(tabxplor.compact = TRUE)
# options(tabxplor.print = "kable")

# ee |> select(all_of(sample(names(ee), 5))) # look at all data classes
ee |> 
  select(all_of(sample(names(ee)[map_lgl(ee, is.factor)], 5))) |> 
  # iwalk(~ cat(paste0(.y,"\n"))) |> 
  tab_many(everything(), pct = "col")
# ee |> select(starts_with(c("PCS"))) |> names() |> dput()


# # Mistakes (FECON10)





# # Mistakes (virage)
# # Binary wrong order
# C32A

# # Nominal 3 cats avec non en premier ? 
# COMMISSARIAT

# # Should be "other"
# VPHYS_AUTEUR_CAT






  
  
  
  
  
  
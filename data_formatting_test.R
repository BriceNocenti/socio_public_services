
source("Demarrage.R")  # setwd() + library() + import_survey() + extract_survey_metadata()
source("data_formatting_pipeline.R")

# df   <- import_survey("path/to/file.sas7bdat")
# meta <- extract_survey_metadata(df)
# # ... review/edit meta ...
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
# ai_suggest_labels()	    	# Routes sync/batch; validates label lengths before merge
# ai_suggest_varnames()	    # Routes sync/batch; warns on duplicate new_names

# # Test on : 
# gen1017_mf.R (SAS + external format files + binary vars)
# PRFE_mf.R (DTA + value labels via labelled)
# sc19_mf.R (SAS + Excel codebook)
# teo20_mf.R (Parquet + complex label cleaning → replace with ai_suggest_labels)
# Virage_mf.R (SAS catalog file)



datadir <- "~/Data/ELIPSS/Pratiques et représentations face à l’État"
df <- import_survey(file.path(datadir, "fr_cdsp_ddi_elipss_201601prfc1.dta"))
meta <- extract_survey_metadata(df, 
  missing_patterns = c(96, 99, 996, 999, 9996, 9999, 
    "Non enquêté", "N'a pas répondu", "Valeur manquante")
)

# value_labs %in% c("N'a pas répondu", "Non enquêté", "Valeur manquante") |
#   values %in% c(96, 996, 9996, 9999)


meta |> 
  filter(var_name == "prfc1_CONTINST_1") |>
  tidyr::unnest(c(values, labels, new_labels)) |> 
  select(var_name, detected_role, values, labels, new_labels) |> 
  print(n = 900)



source("Demarrage.R")  # setwd() + library() + import_survey() + extract_survey_metadata()
source("R/data_formatting_pipeline.R")




datadir  <- "~/Data/ELIPSS/Pratiques et représentations face à l'État"
datapath <- file.path(datadir, "fr_cdsp_ddi_elipss_201601prfc1.dta") |> path.expand()

# Single JSON file that grows brick-by-brick through the workflow.
# Edit it directly between steps for manual corrections.
meta_json            <- stringr::str_replace(datapath, "\\.[^\\.]+$", ".survey_meta.json")
metadata_review_path <- stringr::str_replace(datapath, "\\.[^\\.]+$", "_metadata_review.xlsx")

df <- import_survey(datapath)

missing_num <- c(-1, 96, 99, 996, 999, 9996, 9999)

missing_chr <- c(
  "Non enquêté", "N'a pas répondu", "Valeur manquante", "Non concerné", "Ne sait pas",
  "Vous ne savez pas",
  "Refuse de répondre",
  "Je ne sais pas",
  "[Non concerné]",
  "[Non enquêté]"
)
yes_labels <- c("Oui", "Choisi")
no_labels  <- c("Non", "Non choisi")


# ============================================================
# STEP 1 — First run: create meta_json + auto-detect roles
# → Edit role / desc fields directly in meta_json for corrections
# → After editing, re-run this block (reads your edits from JSON)
# ============================================================
meta <- extract_survey_metadata(df,
  missing_num = missing_num, missing_chr = missing_chr,
  yes_labels  = yes_labels,  no_labels   = no_labels,
  meta_json   = meta_json   # created on first run; merged on subsequent runs
)
# rstudioapi::navigateToFile(meta_json, line = 1)  # open JSON for review/editing


# ============================================================
# STEP 2 — [optional] AI classify roles (writes role/desc directly to meta_json)
# ============================================================
# ai_classify_roles(meta, meta_json = meta_json)
# → Review role / desc fields in meta_json, then re-run Step 1
meta <- extract_survey_metadata(df, meta_json = meta_json)

# STEP 3 — Export Excel for visual review (optional, read-only)
export_metadata_excel(meta, path = metadata_review_path)
# utils::browseURL(metadata_review_path)


# ============================================================
# STEP 3 — Add level counts/frequencies (writes n/pct into meta_json)
# ============================================================
meta <- metadata_add_level_stats(meta, df, meta_json = meta_json)


# ============================================================
# STEP 4 — AI label suggestions (enriches meta_json with new_label)
# → Edit new_label fields in meta_json after this step
# ============================================================
# ai_suggest_labels(meta, meta_json = meta_json, dry_run = TRUE) # preview prompts
ai_suggest_labels(meta, meta_json = meta_json) # chunk_size = 30L, use_batch = TRUE
# rstudioapi::navigateToFile(meta_json, line = 1)

# Reload after editing labels in meta_json, then re-run metadata_add_level_stats
meta <- extract_survey_metadata(df, meta_json = meta_json)
meta <- metadata_add_level_stats(meta, df)


# ============================================================
# STEP 5 — AI variable name suggestions (adds new_name to meta_json)
# → Edit new_name fields in meta_json after this step
# ============================================================
# ai_suggest_varnames(meta, meta_json = meta_json, dry_run = TRUE) # preview prompts
ai_suggest_varnames(meta, meta_json = meta_json, chunk_size = 400L)
# rstudioapi::navigateToFile(meta_json, line = 1)


# ============================================================
# STEP 6 — Final reload and apply
# ============================================================
meta   <- extract_survey_metadata(df, meta_json = meta_json)
df_out <- apply_survey_formats(df, meta)
saveRDS(df_out, path.expand(stringr::str_replace(datapath, "\\.[^\\.]+$", "_formatted.rds")))
generate_format_script(meta, "prfc1", datapath)


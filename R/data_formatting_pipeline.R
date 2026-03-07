# ============================================================
# Survey Formatting Pipeline — data_formatting_pipeline.R  v3
# ============================================================
# Source this file in any _mf.R script:
#   source("data_formatting_pipeline.R")
#
# METADATA SCHEMA — extract_survey_metadata() produces a tibble with:
#   var_name, var_label, r_class, n_distinct, detected_role, desc,
#   values, labels, missing_vals, new_labels, new_name
#
#   detected_role (7 values):
#     identifier    — ID column (unique per row or named IDENT/ID)
#     double        — continuous float, no value labels
#     integer       — discrete integer, no labels (or missing-only labels)
#     integer_scale — integer scale (Likert, left/right) — set by AI/user
#     integer_count — integer count (1 enfant, 2 enfants…) — set by AI/user
#     factor_binary — exactly 2 non-missing levels
#     factor_ordinal — ≥3 ordered levels — set by AI/user
#     factor_nominal — ≥3 unordered levels (default for labelled ≥3)
#
#   desc (logical):
#     factor_binary : TRUE = positive level is first; FALSE = second; NA = unknown
#     factor_ordinal: TRUE = descending (high→low); FALSE = ascending; NA = not set
#     double/integer: FALSE; identifier/nominal: NA
#
# USER WORKFLOW (iterate until metadata is good):
#   1. df   <- import_survey("file.sas7bdat")
#   2b.[optional] ai_suggest_missing(meta)
#      # Prints missing_chr / missing_num vectors — paste into step 2 args
#   2. meta <- extract_survey_metadata(df,
#                missing_chr = c(...), missing_num = c(...))
#      # Console shows factor_binary detections + n needing AI classification
#   3. [optional] ai_classify_roles(meta, meta_json = meta_json)
#      # Writes role / desc to meta_json — review, then re-run step 2
#   4. [optional] metadata_apply_codebook(meta, codebook_df, ...)
#   5. meta <- metadata_fix_binary(meta)
#   6. export_metadata_excel(meta, "meta_review.xlsx")
#      # Open Excel, check orange rows (factor_binary/nominal/integer), iterate
#   6b.[optional] meta <- metadata_add_level_stats(meta, df, meta_json = meta_json)
#      # Adds level_counts/level_freqs to metadata + writes n/pct into meta_json
#   7. [optional] ai_suggest_labels(meta, meta_json = meta_json)
#                 ai_suggest_varnames(meta, meta_json = meta_json)
#                 export_metadata_excel(meta, "meta_review2.xlsx")
#   8. df_out <- apply_survey_formats(df, meta)
#      generate_format_script(meta, "mysurvey", "path/to/file")
#
# Functions:
#   import_survey()             — auto-detect format and import
#   extract_survey_metadata()   — build standardised varmod tibble (v3 schema)
#   metadata_apply_codebook()   — merge external Excel/CSV codebook
#   metadata_fix_binary()       — standardise binary positive/negative labels
#   export_metadata_excel()     — review file (openxlsx, orange = needs attention)
#   apply_survey_formats()      — apply metadata → factors on real data
#   generate_format_script()    — write readable _recode.R for students
#
# AI helpers (require ANTHROPIC_API_KEY env var):
#   ai_suggest_missing()        — Haiku: identify missing-value label candidates
#   ai_classify_roles()         — Haiku: classify ambiguous vars (nominal/ordinal/scale/count)
#   ai_call_claude()            — synchronous single call (httr2)
#   ai_batch_submit()           — submit Message Batch job (httr2)
#   ai_batch_retrieve()         — poll + retrieve batch results (httr2)
#   metadata_add_level_stats()  — add level_counts/level_freqs to metadata
#   ai_suggest_labels()         — shorten labels + merge small ordinal levels (JSON)
#   ai_suggest_varnames()       — propose short R variable names + doc strings
# ============================================================


# ============================================================
# 1. import_survey()
# ============================================================

#' Auto-detect file format and import a survey dataset
#'
#' @param path            Path to the data file.
#' @param format          Optional override: "sas", "dta", "parquet", "sav", "rds".
#' @param catalog_file    Optional SAS catalog file (.sas7bcat).
#' @param encoding        Character encoding. NULL = haven default (usually correct).
#'                        Try "latin1" if accents are garbled in SAS files.
#' @param upper_names     If TRUE (default), convert all variable names to
#'                        UPPER_SNAKE_CASE via `toupper()`.
#' @param remove_prefixes Character vector of prefixes to strip from variable
#'                        names after uppercasing (default: none). Applied in
#'                        the order provided, case-insensitively.
#'
#' @return A tibble with preserved labels. No transformation applied to data.
import_survey <- function(
    path,
    format          = NULL,
    catalog_file    = NULL,
    encoding        = NULL,
    upper_names     = TRUE,
    remove_prefixes = character(0)
) {
  ext <- if (!is.null(format)) format else tolower(tools::file_ext(path))

  df <- switch(
    ext,
    "sas7bdat" = ,
    "sas"      = haven::read_sas(path, catalog_file = catalog_file,
                                 encoding = encoding),
    "dta"      = haven::read_dta(path, encoding = encoding),
    "sav"      = haven::read_sav(path, encoding = encoding),
    "parquet"  = arrow::read_parquet(path),
    "rds"      = readRDS(path),
    stop("Unrecognised format: '", ext, "'. Use sas/dta/sav/parquet/rds.")
  )

  if (isTRUE(upper_names)) {
    names(df) <- toupper(names(df))
  }

  if (length(remove_prefixes) > 0) {
    pfx_upper <- toupper(remove_prefixes)
    for (pfx in pfx_upper) {
      names(df) <- sub(paste0("^", pfx), "", names(df))
    }
  }

  df
}


# ============================================================
# 1b. .normalize_text() — shared text normalization
# ============================================================

# Normalize a character vector for consistent comparison and display:
#   - Detect and convert to UTF-8 (via stringi if available, iconv fallback)
#   - Non-breaking spaces (U+00A0, U+202F, U+2009) → regular space
#   - Typographic apostrophes (', ‛, ʼ, ʻ) → straight apostrophe (')
#   - Straight double quotes around words → French guillemets (« »)
#     when .to_guillemets = TRUE (default FALSE for comparisons)
#   - Trim leading/trailing whitespace, collapse internal runs of spaces
#   - Keep French accents and all other Unicode letters intact
#
# Parameters:
#   x              Character vector to normalize.
#   to_guillemets  If TRUE, replace "text" with «text» (for display/labels).
#                  Keep FALSE for comparisons (missing_chr matching, etc.).
.normalize_text <- function(x, to_guillemets = FALSE) {
  if (!is.character(x) || length(x) == 0) return(x)

  # 1. Ensure UTF-8 — use stringi when available, fall back to iconv
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE, validate = TRUE)
  } else {
    enc <- Encoding(x)
    needs_conv <- enc %in% c("latin1", "unknown") & !is.na(x)
    if (any(needs_conv)) {
      x[needs_conv] <- iconv(x[needs_conv], from = "latin1", to = "UTF-8",
                             sub = "\uFFFD")
    }
  }

  # 2. Non-breaking and narrow spaces → regular space
  x <- gsub("\u00a0|\u202f|\u2009|\u2007", " ", x, useBytes = FALSE)

  # 3. Typographic apostrophes → straight apostrophe
  x <- gsub("[\u2019\u2018\u201b\u02bc\u02bb]", "'", x, useBytes = FALSE)

  # 4. Straight double quotes "text" → «\u00a0text\u00a0» (display only)
  if (to_guillemets) {
    x <- gsub('"([^"]+)"', "\u00ab\\1\u00bb", x, useBytes = FALSE)
  }

  # 5. Trim and collapse internal whitespace runs
  x <- gsub("\\s+", " ", stringr::str_trim(x))

  x
}


# ============================================================
# 1c. Unified survey_meta.json helpers
# ============================================================

# ---------------------------------------------------------------------------
# Back up the current meta_json before overwriting it.
# Creates .survey_meta/ directory next to path if absent.
# Names: {stem}_{step}_{YYYYMMDD}.json  (appends _2, _3 if already exists).
.backup_meta_json <- function(path, step) {
  if (!file.exists(path)) return(invisible(NULL))
  dir_path  <- dirname(path)
  stem      <- tools::file_path_sans_ext(basename(path))
  backup_dir <- file.path(dir_path, ".survey_meta")
  if (!dir.exists(backup_dir))
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

  date_str  <- format(Sys.Date(), "%Y%m%d")
  base_name <- paste0(stem, "_", step, "_", date_str, ".json")
  dest      <- file.path(backup_dir, base_name)
  suffix    <- 2L
  while (file.exists(dest)) {
    dest <- file.path(backup_dir,
                      paste0(stem, "_", step, "_", date_str, "_", suffix, ".json"))
    suffix <- suffix + 1L
  }
  file.copy(path, dest, overwrite = FALSE)
  invisible(dest)
}

# ---------------------------------------------------------------------------
# Read the unified survey_meta.json.
# Returns list(config = list(), variables = list()) if file absent or malformed.
.read_meta_json <- function(path) {
  empty <- list(config = list(), variables = list())
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(empty)
  tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      warning(".read_meta_json: could not parse '", path, "': ", conditionMessage(e))
      empty
    }
  )
}

# ---------------------------------------------------------------------------
# Write the unified survey_meta.json.
# meta_list: list(config = list(...), variables = list(VAR = list(...)))
# Format:
#   - config section: one field per line, arrays compact
#   - variables section: each variable is a multi-line block, levels padded
.write_meta_json <- function(meta_list, path) {
  esc <- function(s) {
    s <- as.character(s)
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    gsub('"',  '\\"',  s,  fixed = TRUE)
  }
  rpad <- function(s, w) {
    n <- nchar(s, type = "chars")
    if (n < w) paste0(s, strrep(" ", w - n)) else s
  }
  # Compact JSON array (no space after commas inside arrays)
  arr_str <- function(v) {
    if (is.null(v) || length(v) == 0) return("[]")
    elems <- vapply(v, function(x) {
      if (is.na(x))          "null"
      else if (is.logical(x)) if (x) "true" else "false"
      else if (is.numeric(x)) as.character(x)
      else                    paste0('"', esc(as.character(x)), '"')
    }, character(1))
    paste0("[", paste(elems, collapse = ", "), "]")
  }
  # JSON scalar: string, number, bool, or null
  scalar_str <- function(x) {
    if (is.null(x) || (length(x) == 1 && is.na(x))) return("null")
    if (is.logical(x)) return(if (isTRUE(x)) "true" else "false")
    if (is.numeric(x)) return(as.character(x))
    paste0('"', esc(as.character(x)), '"')
  }

  # ---- config section -------------------------------------------------------
  cfg        <- meta_list$config
  cfg_fields <- c("dataset", "missing_num", "missing_chr", "yes_labels", "no_labels")
  cfg_lines  <- c('  "config": {')
  cfg_keys   <- intersect(cfg_fields, names(cfg))   # only present keys, in order
  for (i in seq_along(cfg_keys)) {
    k   <- cfg_keys[[i]]
    val <- cfg[[k]]
    # dataset is a scalar string; missing_num/chr/yes_labels/no_labels are arrays
    v_str <- if (k == "dataset" || (length(val) == 1 && is.character(val) && k == "dataset")) {
      scalar_str(val[[1]])
    } else if (length(val) > 1 || k %in% c("missing_num", "missing_chr", "yes_labels", "no_labels")) {
      arr_str(unlist(val))
    } else {
      scalar_str(val)
    }
    comma <- if (i < length(cfg_keys)) "," else ""
    cfg_lines <- c(cfg_lines, paste0('    "', k, '": ', v_str, comma))
  }
  cfg_lines <- c(cfg_lines, '  }')

  # ---- variables section ----------------------------------------------------
  vars      <- meta_list$variables
  var_names <- names(vars)
  n_vars    <- length(var_names)
  var_blocks <- character(n_vars)

  # scalar field names width (for colon alignment inside variable blocks)
  scalar_fields <- c("var_label", "role", "desc", "new_name")
  w_field        <- max(nchar(scalar_fields)) + 2L  # +2 for quotes

  for (vi in seq_along(var_names)) {
    vname <- var_names[[vi]]
    entry <- vars[[vname]]

    # -- scalar fields (padded colon alignment) --------------------------------
    s_lines <- character(length(scalar_fields))
    for (fi in seq_along(scalar_fields)) {
      fld    <- scalar_fields[[fi]]
      val    <- entry[[fld]]
      v_str  <- scalar_str(val)
      key_q  <- paste0('"', fld, '"')
      s_lines[[fi]] <- paste0(
        '      ', rpad(key_q, w_field), ': ', v_str, ','
      )
    }

    # -- levels sub-block (reuse existing alignment logic) --------------------
    levels <- entry$levels
    if (is.null(levels) || length(levels) == 0) {
      levels_body <- '      "levels": {}'
    } else {
      n_lev <- length(levels)
      # has_new_label: only non-null_coded levels with actual new_label
      has_new_label <- any(purrr::map_lgl(levels, ~ !isTRUE(.x$null_coded) && !is.null(.x$new_label)))
      has_n         <- any(purrr::map_lgl(levels, ~ !is.null(.x$n)))
      has_pct       <- any(purrr::map_lgl(levels, ~ !is.null(.x$pct)))

      val_keys  <- names(levels)
      f_key     <- paste0('"', purrr::map_chr(val_keys, esc), '"')
      f_label   <- purrr::map_chr(levels, function(lev)
                     paste0('"', esc(as.character(lev$label)), '"'))
      f_new_lbl <- if (has_new_label) purrr::map_chr(levels, function(lev)
                     if (!isTRUE(lev$null_coded) && !is.null(lev$new_label))
                       paste0('"', esc(as.character(lev$new_label)), '"')
                     else '""') else NULL
      f_n       <- if (has_n) purrr::map_chr(levels, function(lev)
                     if (!is.null(lev$n) && !is.na(lev$n)) as.character(as.integer(lev$n)) else "") else NULL
      f_pct     <- if (has_pct) purrr::map_chr(levels, function(lev)
                     if (!is.null(lev$pct) && !is.na(lev$pct)) as.character(as.integer(lev$pct)) else "") else NULL

      w_key   <- max(nchar(f_key,   type = "chars"), na.rm = TRUE)
      w_label <- max(nchar(f_label, type = "chars"), na.rm = TRUE)
      w_new   <- if (has_new_label) max(nchar(f_new_lbl, type = "chars"), na.rm = TRUE) else 0L
      w_n     <- if (has_n)   max(nchar(f_n,   type = "chars"), na.rm = TRUE) else 0L
      w_pct   <- if (has_pct) max(nchar(f_pct, type = "chars"), na.rm = TRUE) else 0L

      level_lines <- character(n_lev)
      for (i in seq_len(n_lev)) {
        lev     <- levels[[i]]
        is_null <- isTRUE(lev$null_coded)
        tokens  <- character(0)
        tokens  <- c(tokens, paste0('"label": ', rpad(f_label[[i]], w_label)))
        # new_label: only for non-null_coded levels
        if (has_new_label && !is_null)
          tokens <- c(tokens, paste0('"new_label": ', rpad(f_new_lbl[[i]], w_new)))
        if (is_null) {
          tokens <- c(tokens, '"null_coded": true')
        } else {
          if (has_n && !is.null(lev$n))
            tokens <- c(tokens, paste0('"n": ',   formatC(f_n[[i]],   width = w_n,   flag = " ")))
          if (has_pct && !is.null(lev$pct))
            tokens <- c(tokens, paste0('"pct": ', formatC(f_pct[[i]], width = w_pct, flag = " ")))
        }
        level_lines[[i]] <- paste0('        ', rpad(f_key[[i]], w_key), ': { ',
                                   paste(tokens, collapse = ", "), ' }')
      }
      for (i in seq_len(n_lev - 1L))
        level_lines[[i]] <- paste0(level_lines[[i]], ",")

      levels_body <- paste0(
        '      "levels": {
',
        paste(level_lines, collapse = "\n"), '\n',
        '      }'
      )
    }

    comma <- if (vi < n_vars) "," else ""
    var_blocks[[vi]] <- paste0(
      '    "', esc(vname), '": {
',
      paste(s_lines, collapse = "\n"), "\n",
      levels_body, "\n",
      '    }', comma
    )
  }

  # ---- _schema block (human/AI documentation) --------------------------------
  schema_block <- paste0(
    '  "_schema": {\n',
    '    "description": "Fichier de m\u00e9tadonn\u00e9es d\'une enqu\u00eate sociologique fran\u00e7aise.',
    ' Chaque variable de la section \'variables\' d\u00e9crit une question du questionnaire.',
    ' Ce fichier peut \u00eatre \u00e9dit\u00e9 manuellement entre les \u00e9tapes automatis\u00e9es.',
    ' Une IA qui re\u00e7oit ce fichier peut s\'en servir pour conseiller des \u00e9tudiant\u00b7es',
    ' sur les variables utiles \u00e0 leur analyse.',
    ' Toujours d\u00e9signer les variables par leur \'new_name\'',
    ' (nom dans le fichier de donn\u00e9es final).",\n',
    '    "fields": {\n',
    '      "config.dataset"                  : "Nom du fichier de donn\u00e9es source",\n',
    '      "config.missing_num"              : "Codes num\u00e9riques trait\u00e9s comme valeurs manquantes ou non-r\u00e9ponses (exclus de l\'analyse)",\n',
    '      "config.missing_chr"              : "Libell\u00e9s textuels trait\u00e9s comme valeurs manquantes ou non-r\u00e9ponses",\n',
    '      "config.yes_labels"               : "Libell\u00e9s qui d\u00e9signent la modalit\u00e9 positive des variables binaires (ex : Oui, Choisi)",\n',
    '      "config.no_labels"                : "Libell\u00e9s qui d\u00e9signent la modalit\u00e9 n\u00e9gative des variables binaires (ex : Non, Non choisi)",\n',
    '      "variables.VAR.var_label"         : "Intitul\u00e9 original de la question dans le questionnaire (peut \u00eatre modifi\u00e9 pour la documentation)",\n',
    '      "variables.VAR.role"              : "Type de variable : factor_binary = binaire, factor_ordinal = ordinale (ordre significatif), factor_nominal = nominale (cat\u00e9gories sans ordre), integer_scale = \u00e9chelle num\u00e9rique, integer_count = comptage, double = continu, identifier = identifiant, integer = entier, other = autre",\n',
    '      "variables.VAR.desc"              : "Sens du tri des modalit\u00e9s. Pour une binaire : true = la modalit\u00e9 positive est en premier. Pour une ordinale : true = ordre d\u00e9croissant (la modalit\u00e9 la plus \u00e9lev\u00e9e en premier). null = non applicable ou inconnu.",\n',
    '      "variables.VAR.new_name"          : "Nom de la variable dans le fichier de donn\u00e9es final \u2014 c\'est ce nom qu\'il faut utiliser pour conseiller les \u00e9tudiant\u00b7es",\n',
    '      "variables.VAR.doc_note"          : "Note de documentation libre sur la variable (ajout\u00e9e manuellement)",\n',
    '      "variables.VAR.levels.CODE.label"     : "Libell\u00e9 original de cette modalit\u00e9 de r\u00e9ponse",\n',
    '      "variables.VAR.levels.CODE.new_label" : "Libell\u00e9 court pour l\'affichage dans les tableaux (sugg\u00e9r\u00e9 par l\'IA, modifiable)",\n',
    '      "variables.VAR.levels.CODE.null_coded": "true = cette modalit\u00e9 est une non-r\u00e9ponse ou valeur manquante, exclue de l\'analyse",\n',
    '      "variables.VAR.levels.CODE.n"         : "Nombre de r\u00e9pondant\u00b7es ayant choisi cette modalit\u00e9",\n',
    '      "variables.VAR.levels.CODE.pct"       : "Pourcentage de r\u00e9pondant\u00b7es ayant choisi cette modalit\u00e9 (hors valeurs manquantes)"\n',
    '    }\n',
    '  }'
  )

  json_str <- paste0(
    "{\n",
    schema_block, ",\n",
    paste(cfg_lines, collapse = "\n"), ",\n",
    '  "variables": {\n',
    paste(var_blocks, collapse = "\n"), "\n",
    '  }\n',
    "}\n"
  )

  writeLines(enc2utf8(json_str), con = path, useBytes = TRUE)
  invisible(path)
}


# ============================================================
# 2. extract_survey_metadata()
# ============================================================

#' Extract variable and value metadata from a labelled tibble
#'
#' Produces a "varmod" tibble. Iterate: run, review console output, run
#' ai_classify_roles() for ambiguous variables, then re-run.
#' When satisfied, proceed to export_metadata_excel() and ai_suggest_labels().
#'
#' @param df              A tibble from import_survey().
#' @param missing_num     Numeric codes to flag as candidate missing values.
#'                        Default covers common French survey conventions.
#' @param missing_chr     Character strings to flag as candidate missing.
#'                        Matched against raw values AND label text (regex,
#'                        case-insensitive).
#' @param yes_labels      Character vector of keywords identifying the "positive"
#'                        level in binary variables (partial match, lowercase).
#'                        NULL uses built-in defaults.
#' @param no_labels       Keywords for the "negative" level. NULL = built-in.
#' @param max_levels_cat  Unlabelled numeric vars with ≤ this many distinct
#'                        non-missing values are classed "integer" (not factor).
#' @param meta_json       Recommended. Path to the unified \code{*.survey_meta.json}
#'                        file. If the file does not exist yet, it is created from
#'                        the current extraction (with auto-detected roles/desc and
#'                        the config parameters written to the JSON). If the file
#'                        exists, its \code{config} section is used as defaults for
#'                        missing_num/missing_chr/yes_labels/no_labels (R args
#'                        override if explicitly supplied), and its
#'                        \code{variables} section overrides auto-detected role,
#'                        desc, new_labels, new_name for each variable.
#'                        Edit the JSON between steps for manual corrections.
#'
#' @return A tibble with columns:
#'   var_name, var_label, r_class, n_distinct, detected_role, desc,
#'   values, labels, missing_vals, new_labels, new_name
#'
#' Role taxonomy:
#'   "identifier"     — ID column (unique per row or named IDENT/ID)
#'   "double"         — continuous float, no value labels, empty labels list
#'   "integer"        — discrete integer, no value labels (AI: integer_scale/integer_count)
#'   "integer_scale"  — integer with scale labels (Likert, left/right) — AI/user only
#'   "integer_count"  — integer with count labels (1 enfant, 2 enfants) — AI/user only
#'   "factor_binary"  — exactly 2 non-missing levels after missing removal
#'   "factor_ordinal" — ≥3 levels with natural order — AI/user only
#'   "factor_nominal" — ≥3 levels, default for all labelled vars ≥3
extract_survey_metadata <- function(
    df,
    missing_num     = c(96, 99, 996, 999, 9996, 9999), # 8, 9,
    missing_chr     = c("-1", "NSP", "NRP", "NR", "REFUS",
                        "Ne sait pas", "Refus"), # "8", "9",
    yes_labels      = NULL,
    no_labels       = NULL,
    max_levels_cat  = 20,
    meta_json       = NULL
) {
  # ---- Read config/variables from meta_json (if exists) ---------------------
  .meta_json_existed <- !is.null(meta_json) && nzchar(meta_json) && file.exists(meta_json)
  .meta_json_data <- .read_meta_json(meta_json)
  .cfg            <- .meta_json_data$config
  .json_vars      <- .meta_json_data$variables

  # Detect which config args were explicitly supplied by the caller (not defaults)
  .formals <- formals(sys.function())
  .call_args <- as.list(match.call())[-1]
  .missing_num_explicit  <- "missing_num" %in% names(.call_args)
  .missing_chr_explicit  <- "missing_chr" %in% names(.call_args)
  .yes_labels_explicit   <- "yes_labels"  %in% names(.call_args)
  .no_labels_explicit    <- "no_labels"   %in% names(.call_args)

  # Apply JSON config defaults when caller used the parameter default (not explicit)
  if (length(.cfg) > 0) {
    if (!is.null(.cfg$missing_num) && !.missing_num_explicit)
      missing_num <- as.numeric(unlist(.cfg$missing_num))
    if (!is.null(.cfg$missing_chr) && !.missing_chr_explicit)
      missing_chr <- as.character(unlist(.cfg$missing_chr))
    if (!is.null(.cfg$yes_labels) && !.yes_labels_explicit)
      yes_labels <- as.character(unlist(.cfg$yes_labels))
    if (!is.null(.cfg$no_labels) && !.no_labels_explicit)
      no_labels <- as.character(unlist(.cfg$no_labels))
  }

  # JSON variable-level role/desc overrides (auto-detected < JSON < R args)
  .json_role_overrides <- if (length(.json_vars) > 0) {
    purrr::compact(purrr::imap(.json_vars, ~ {
      r <- .x$role; if (!is.null(r) && nzchar(r)) r else NULL
    }))
  } else list()
  .json_desc_overrides <- if (length(.json_vars) > 0) {
    # Keep FALSE (explicit FALSE != NULL); drop NULL and empty list() from JSON null
    # (jsonlite::write_json serializes R NULL as {}, so filter both NULL and list())
    Filter(
      function(x) !is.null(x) && !(is.list(x) && length(x) == 0L),
      purrr::imap(.json_vars, ~ .x$desc)
    )
  } else list()

  # Role/desc overrides come from JSON variables section only
  .effective_roles <- .json_role_overrides
  .effective_descs <- .json_desc_overrides

  default_yes <- c("oui", "choisi", "yes", "vrai", "true",
                   "présent", "actif", "sélectionné", "concerné",
                   "a le", "dispose", "perçoit")
  default_no  <- c("non", "non choisi", "no", "faux", "false",
                   "absent", "inactif", "non sélectionné",
                   "pas ", "n'a pas", "ne dispose", "ne perçoit")

  # Normalize all user-supplied text inputs once at function entry
  missing_chr <- .normalize_text(missing_chr)
  yes_kw <- .normalize_text(if (!is.null(yes_labels)) yes_labels else default_yes)
  no_kw  <- .normalize_text(if (!is.null(no_labels))  no_labels  else default_no)

  var_labels_list <- labelled::get_variable_labels(df)
  n_rows          <- nrow(df)

  # --- missing label text regex (applied to label strings) ---
  missing_lbl_pattern <- paste0(
    "(?i)(\\bNSP\\b|\\bNRP\\b|\\bNR\\b|\\bREFUS\\b|",
    "ne sait pas|non r.pondu|sans r.ponse|",
    "\\[nsp\\]|\\[refus\\]|\\[nr\\])"
  )

  binary_lines <- character(0)  # collect console output

  meta <- purrr::imap(df, function(col, vname) {

    # --- variable label ---
    var_lbl <- var_labels_list[[vname]]
    if (is.null(var_lbl) || is.na(var_lbl)) var_lbl <- ""
    var_lbl <- .normalize_text(var_lbl)

    # --- R class: strip haven_labelled/vctrs_vctr to get underlying type ---
    all_classes <- class(col)
    r_class_raw <- all_classes[!all_classes %in% c("haven_labelled", "vctrs_vctr",
                                                     "haven_labelled_spss")]
    r_class <- if (length(r_class_raw) > 0) r_class_raw[[1]] else all_classes[[1]]

    # --- value labels ---
    val_labs     <- labelled::val_labels(col)
    has_val_labs <- !is.null(val_labs) && length(val_labs) > 0

    # --- distinct non-NA values (ALL, before missing removal — for identifier check) ---
    vals_present <- unique(col[!is.na(col)])
    n_dist_total <- length(vals_present)

    # --- values and labels vectors (ALL, including missing candidates) ---
    if (has_val_labs) {
      raw_values <- unname(val_labs)
      raw_labels <- .normalize_text(names(val_labs))
    } else if (is.factor(col)) {
      raw_values <- levels(col)
      raw_labels <- .normalize_text(levels(col))
    } else {
      sorted_vals <- sort(vals_present)
      raw_values  <- as.character(sorted_vals)
      raw_labels  <- .normalize_text(as.character(sorted_vals))
    }

    # --- flag candidate missing values (unified: numeric code + label text) ---
    is_miss <- purrr::map2_lgl(raw_values, raw_labels, function(v, l) {
      v_num   <- suppressWarnings(as.numeric(v))
      num_hit <- !is.na(v_num) && v_num %in% missing_num
      lbl_hit <- l %in% missing_chr || grepl(missing_lbl_pattern, l, perl = TRUE)
      num_hit || lbl_hit
    })
    missing_vals_vec <- raw_values[is_miss]

    # --- non-missing values/labels for role detection ---
    vals_clean <- raw_values[!is_miss]
    lbls_clean <- raw_labels[!is_miss]
    n_clean    <- length(vals_clean)

    # n_distinct = count of non-missing, non-NA levels (for display and binary detection)
    n_dist <- n_clean

    # --- detect role ---
    role_out <- .detect_role_v3(
      vname, col, has_val_labs, n_dist_total, n_rows,
      n_clean, vals_clean, lbls_clean, yes_kw, no_kw, r_class
    )
    detected_role <- role_out$role
    desc_auto     <- role_out$desc

    # Override role: from JSON variables section
    if (vname %in% names(.effective_roles)) {
      detected_role <- .effective_roles[[vname]]
    }
    # Override desc: from JSON variables section
    desc_val <- if (vname %in% names(.effective_descs)) {
      # Coerce to logical scalar; guard against stray list() from JSON null
      d <- .effective_descs[[vname]]
      if (is.null(d) || (is.list(d) && length(d) == 0L)) NA
      else as.logical(d[[1]])
    } else {
      desc_auto
    }

    # --- new_labels: copy of labels with missing candidates pre-marked as "NULL" ---
    new_labels_init <- ifelse(is_miss, "NULL", raw_labels)

    # --- For non-factor roles, labels are not meaningful — clear them ---
    if (detected_role %in% c("double", "integer", "identifier")) {
      raw_values      <- character(0)
      raw_labels      <- character(0)
      new_labels_init <- character(0)
    }

    # --- console output for factor_binary detection ---
    if (detected_role == "factor_binary") {
      lv1 <- if (length(lbls_clean) >= 1) lbls_clean[[1]] else "?"
      lv2 <- if (length(lbls_clean) >= 2) lbls_clean[[2]] else "?"
      if (is.na(desc_val)) {
        tag      <- "[factor_binary?]"
        desc_str <- "desc=NA, needs review"
      } else if (isTRUE(desc_val)) {
        tag      <- "[factor_binary] "
        desc_str <- "positive=first (desc=TRUE) \u2713"
      } else {
        tag      <- "[factor_binary] "
        desc_str <- "positive=second (desc=FALSE)"
      }
      binary_lines <<- c(binary_lines,
        sprintf("%s %-20s: \"%s\" vs \"%s\" \u2014 %s",
                tag, vname, lv1, lv2, desc_str))
    }

    tibble::tibble(
      var_name      = vname,
      var_label     = var_lbl,
      r_class       = r_class,
      n_distinct    = n_dist,
      detected_role = detected_role,
      desc          = desc_val,
      values        = list(raw_values),
      labels        = list(raw_labels),
      missing_vals  = list(missing_vals_vec),
      new_labels    = list(new_labels_init),
      new_name      = vname
    )
  }) |>
    dplyr::bind_rows()

  # --- Console summary ---
  n_needs_ai <- sum(meta$detected_role %in% c("factor_nominal", "integer") |
                    (meta$detected_role == "factor_binary" & is.na(meta$desc)))
  message("\nextract_survey_metadata: ", nrow(meta), " variables | ",
          nrow(df), " observations")
  message("  Roles: ",
          paste(names(table(meta$detected_role)),
                table(meta$detected_role), sep = "=", collapse = "  "))
  if (length(binary_lines) > 0) {
    message("\nBinary variables (factor_binary) detected:")
    purrr::walk(binary_lines, message)
  }
  if (n_needs_ai > 0) {
    message("\n[!] ", n_needs_ai, " variable(s) may need role refinement",
            " (factor_nominal / integer / factor_binary with desc=NA).")
    message("    Run ai_classify_roles(meta, meta_json = meta_json) to classify roles.")
  }

  # Compute pruned config arrays: keep only values/labels that exist in the data
  # (used for writing config — prune only when user explicitly supplied the arg)
  .all_vals_num <- suppressWarnings(as.numeric(unique(unlist(meta$values))))
  .all_vals_num <- .all_vals_num[!is.na(.all_vals_num)]
  .all_labels   <- .normalize_text(unique(unlist(meta$labels)))

  .missing_num_used <- if (.missing_num_explicit)
    missing_num[missing_num %in% .all_vals_num] else missing_num
  .missing_chr_used <- if (.missing_chr_explicit)
    missing_chr[missing_chr %in% .all_labels] else missing_chr
  .yes_labels_used  <- if (.yes_labels_explicit && !is.null(yes_labels))
    yes_labels[.normalize_text(yes_labels) %in% .all_labels] else yes_labels
  .no_labels_used   <- if (.no_labels_explicit && !is.null(no_labels))
    no_labels[.normalize_text(no_labels) %in% .all_labels] else no_labels

  # Apply unified meta_json (overrides new_labels, new_name, level stats)
  if (!is.null(meta_json)) {
    meta <- metadata_apply_meta_json(meta, .json_vars)

    if (!.meta_json_existed) {
      # Write initial JSON on first run
      .write_initial_meta_json(meta, meta_json,
                               missing_num = .missing_num_used,
                               missing_chr = .missing_chr_used,
                               yes_labels  = .yes_labels_used,
                               no_labels   = .no_labels_used,
                               datapath    = attr(df, "path"))
      message("extract_survey_metadata: created ", meta_json)
    } else {
      # On re-run: sync null_coded flags and optionally update config
      .updated_vars <- .update_null_coded_in_meta_json(.meta_json_data$variables, meta)
      .updated_cfg  <- .meta_json_data$config
      if (.missing_num_explicit)
        .updated_cfg$missing_num <- as.list(.missing_num_used)
      if (.missing_chr_explicit)
        .updated_cfg$missing_chr <- as.list(.missing_chr_used)
      if (.yes_labels_explicit)
        .updated_cfg$yes_labels  <- as.list(.yes_labels_used)
      if (.no_labels_explicit)
        .updated_cfg$no_labels   <- as.list(.no_labels_used)

      if (!identical(.updated_vars, .meta_json_data$variables) ||
          !identical(.updated_cfg,  .meta_json_data$config)) {
        .backup_meta_json(meta_json, "reextract")
        .write_meta_json(list(config = .updated_cfg, variables = .updated_vars), meta_json)
        message("extract_survey_metadata: updated null_coded/config in ", meta_json)
      }
    }
  }

  meta
}


# Internal role detection v3 — not exported
.detect_role_v3 <- function(
    vname, col, has_val_labs, n_dist_total, n_rows,
    n_clean, vals_clean, lbls_clean, yes_kw, no_kw, r_class
) {
  # 1. Identifier: ID name or all (total) values unique
  id_pattern <- "^(IDENT|IDENTIF|IDENTIFIANT|ID|_ID|ID_|NUMEN|NUMIDENT)$"
  if (grepl(id_pattern, vname, ignore.case = TRUE) || n_dist_total == n_rows) {
    return(list(role = "identifier", desc = NA))
  }

  # 2. Labelled column → factor_binary (2 clean levels) or factor_nominal (>=3)
  if (has_val_labs) {
    if (n_clean == 2) {
      desc <- .find_binary_desc(lbls_clean, yes_kw, no_kw)
      return(list(role = "factor_binary", desc = desc))
    }
    return(list(role = "factor_nominal", desc = NA))
  }

  # 3. Factor column (no val_labs but is.factor)
  if (is.factor(col)) {
    if (n_clean == 2) {
      desc <- .find_binary_desc(lbls_clean, yes_kw, no_kw)
      return(list(role = "factor_binary", desc = desc))
    }
    return(list(role = "factor_nominal", desc = NA))
  }

  # 4. Unlabelled numeric: distinguish double (any non-whole value) from integer
  if (r_class %in% c("double", "numeric", "integer") || is.numeric(col)) {
    vals_num <- suppressWarnings(as.numeric(vals_clean))
    vals_num <- vals_num[!is.na(vals_num)]
    if (length(vals_num) > 0 && any(vals_num != floor(vals_num))) {
      return(list(role = "double", desc = FALSE))
    }
    return(list(role = "integer", desc = FALSE))
  }

  # 5. Character column without labels
  if (is.character(col)) {
    all_int_str <- length(vals_clean) > 0 &&
      all(grepl("^-?[0-9]+$", vals_clean[vals_clean != ""]))
    if (all_int_str) return(list(role = "integer", desc = FALSE))
    return(list(role = "factor_nominal", desc = NA))
  }

  list(role = "other", desc = NA)
}


# Internal: determine desc for a 2-level binary variable
# desc=TRUE  → label order is [positive, negative] e.g. ["Oui","Non"] → "1-Oui","2-Non"
# desc=FALSE → label order is [negative, positive] e.g. ["Non","Oui"] → will be reordered
.find_binary_desc <- function(lbls_clean, yes_kw, no_kw) {
  if (length(lbls_clean) != 2) return(NA)

  lbl_lower    <- tolower(.normalize_text(lbls_clean))
  # Remove numeric prefix: "1-Oui", "1 - Oui", "1. Oui", "1 Oui" → "oui"
  # Requires at least one separator char (dash, period, or space) after the digit(s).
  lbl_stripped <- stringr::str_remove(lbl_lower, "^[0-9]+(?:\\s*[-.]\\s*|\\s+)")

  # Normalize and lowercase keywords
  yes_kw_lc <- tolower(.normalize_text(yes_kw))
  no_kw_lc  <- tolower(.normalize_text(no_kw))

  # Match a stripped label against a set of keywords.
  # Strategy: exact equality only. This is the most robust approach for short
  # survey labels and avoids "choisi" matching inside "non choisi".
  # The prefix strip above handles "1-Oui" → "oui" → matches "oui".
  lbl_matches_kw <- function(lbl, kws) lbl %in% kws

  is_yes <- purrr::map_lgl(lbl_stripped, lbl_matches_kw, kws = yes_kw_lc)
  is_no  <- purrr::map_lgl(lbl_stripped, lbl_matches_kw, kws = no_kw_lc)

  # Exactly one label matches yes and not no → use it
  yes_only <- is_yes & !is_no
  if (sum(yes_only) == 1) return(which(yes_only) == 1L)

  # If both labels claim yes (shouldn't happen with good kw lists), return NA
  if (sum(is_yes) == 1) return(which(is_yes) == 1L)

  NA
}


# ============================================================
# 2b. metadata_add_level_stats()
# ============================================================

#' Add per-level count and frequency columns to metadata
#'
#' Computes, for each factor variable, the count and non-missing frequency
#' of each level.  Call this AFTER extract_survey_metadata() has settled on
#' correct role assignments and BEFORE ai_suggest_labels().
#'
#' Two list-columns are added (or replaced if they already exist):
#'   - `level_counts`  : integer vector, one element per label (including "NULL")
#'   - `level_freqs`   : numeric vector, percent 0-100 rounded to 0 decimals.
#'                       Missing values (NULL-coded rows) are excluded from the
#'                       denominator, so the non-NULL entries sum to 100.
#'
#' Non-factor roles get empty integer(0) / numeric(0) vectors.
#' Uses data.table for fast tabulation across all factor columns at once.
#'
#' @param metadata  Varmod tibble from extract_survey_metadata().
#' @param df        The original survey tibble (from import_survey()).
#'
#' @return metadata with two new list-columns: level_counts, level_freqs.
metadata_add_level_stats <- function(metadata, df, meta_json = NULL) {
  factor_roles <- c("factor_binary", "factor_nominal", "factor_ordinal")

  # Restrict to factor variables present in df
  fac_meta <- metadata[
    metadata$detected_role %in% factor_roles &
    metadata$var_name      %in% names(df) &
    lengths(metadata$values) > 0, ]

  if (nrow(fac_meta) == 0) {
    # Nothing to count — attach empty columns and return
    metadata$level_counts <- vector("list", nrow(metadata))
    metadata$level_counts[] <- list(integer(0))
    metadata$level_freqs  <- vector("list", nrow(metadata))
    metadata$level_freqs[]  <- list(numeric(0))
    return(metadata)
  }

  var_names <- fac_meta$var_name

  # --- Unnest declared levels from metadata into a long table ----------------
  # One row per (variable, level position). val_chr is the coded value as
  # character; is_null flags NULL-coded (missing) levels excluded from freqs.
  meta_vals <- data.table::rbindlist(lapply(seq_len(nrow(fac_meta)), function(i) {
    vals <- as.character(fac_meta$values[[i]])
    nls  <- fac_meta$new_labels[[i]]
    n    <- max(length(vals), length(nls))
    data.table::data.table(
      var_name = fac_meta$var_name[i],
      position = seq_len(n),
      val_chr  = ifelse(seq_len(n) <= length(vals), vals[seq_len(n)], NA_character_),
      is_null  = ifelse(seq_len(n) <= length(nls),  nls[seq_len(n)] == "NULL", TRUE)
    )
  }))

  # --- Observed counts: one C-level groupby across all factor vars -----------
  dt_long <- data.table::rbindlist(lapply(var_names, function(vn)
    data.table::data.table(var_name = vn, val = as.character(df[[vn]]))))
  counts_dt <- dt_long[!is.na(val), .(n = .N), by = .(var_name, val)]

  # --- Mismatch report: observed values absent from metadata$values ----------
  unmatched <- counts_dt[!meta_vals, on = .(var_name, val = val_chr)]
  if (nrow(unmatched) > 0) {
    detail <- unmatched[order(var_name, val),
                        paste0("  ", var_name, ': "', val, '" (n=', n, ")")]
    message("metadata_add_level_stats: ",
            data.table::uniqueN(unmatched$var_name),
            " variable(s) have observed values not in metadata$values ",
            "(excluded from counts):\n",
            paste(detail, collapse = "\n"))
  }

  # --- Left join: fill observed counts for matched levels -------------------
  meta_vals[counts_dt, on = .(var_name, val_chr = val), n := i.n]
  meta_vals[is.na(n), n := 0L]

  # --- Compute pct within each variable (NULL-coded levels excluded) --------
  meta_vals[, total_valid := sum(n[!is_null]), by = var_name]
  meta_vals[, pct := data.table::fifelse(
    total_valid > 0L & !is_null,
    round(n / total_valid * 100, 0),
    NA_real_
  )]

  # --- Re-nest counts/freqs back into list-columns --------------------------
  data.table::setorder(meta_vals, var_name, position)
  nested <- meta_vals[, .(counts = list(n), freqs = list(pct)), by = var_name]

  # Write back into a full-length result (non-factor rows get empty vectors)
  result_counts <- vector("list", nrow(metadata))
  result_freqs  <- vector("list", nrow(metadata))
  result_counts[] <- list(integer(0))
  result_freqs[]  <- list(numeric(0))

  fac_idx <- match(fac_meta$var_name, metadata$var_name)
  for (i in seq_along(fac_idx)) {
    vn <- fac_meta$var_name[i]
    ni <- which(nested$var_name == vn)
    result_counts[[fac_idx[i]]] <- nested$counts[[ni]]
    result_freqs[[fac_idx[i]]]  <- nested$freqs[[ni]]
  }

  metadata$level_counts <- result_counts
  metadata$level_freqs  <- result_freqs

  # ---------- JSON integration -----------------------------------------------
  if (!is.null(meta_json)) {
    .backup_meta_json(meta_json, "level_stats")
    existing <- .read_meta_json(meta_json)
    n_updated <- 0L
    for (i in seq_along(fac_idx)) {
      vn   <- fac_meta$var_name[i]
      vals <- fac_meta$values[[i]]
      ni   <- which(nested$var_name == vn)
      cnts <- nested$counts[[ni]]
      frqs <- nested$freqs[[ni]]
      if (is.null(existing$variables[[vn]])) next
      for (j in seq_along(vals)) {
        key <- as.character(vals[[j]])
        if (is.null(existing$variables[[vn]]$levels[[key]])) next
        existing$variables[[vn]]$levels[[key]]$n <- cnts[j]
        pct <- frqs[j]
        if (!is.na(pct))
          existing$variables[[vn]]$levels[[key]]$pct <- as.integer(pct)
      }
      n_updated <- n_updated + 1L
    }
    .write_meta_json(existing, meta_json)
    message("metadata_add_level_stats: updated n/pct for ", n_updated,
            " variable(s) in ", basename(meta_json))
  }

  metadata
}



# ============================================================
# 3. metadata_apply_codebook()
# ============================================================

#' Merge an external codebook (Excel/CSV) into a metadata tibble
#'
#' @param metadata        Varmod tibble from extract_survey_metadata().
#' @param codebook_df     Data frame from readxl::read_excel() or similar.
#' @param var_col         Column in codebook_df with variable codes (→ var_name).
#' @param label_col       Column for variable-level labels. NULL to skip.
#' @param code_col        Column for value codes. NULL to skip value labels.
#' @param value_label_col Column for value label strings. NULL to skip.
#'
#' @return Updated metadata tibble.
metadata_apply_codebook <- function(
    metadata,
    codebook_df,
    var_col,
    label_col       = NULL,
    code_col        = NULL,
    value_label_col = NULL
) {
  if (!is.null(label_col)) {
    var_lbl_map <- codebook_df |>
      dplyr::select(var_name = !!rlang::sym(var_col),
                    new_var_label = !!rlang::sym(label_col)) |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(new_var_label)) |>
      dplyr::mutate(new_var_label = .normalize_text(new_var_label))

    metadata <- metadata |>
      dplyr::left_join(var_lbl_map, by = "var_name") |>
      dplyr::mutate(
        var_label = dplyr::if_else(!is.na(new_var_label), new_var_label, var_label)
      ) |>
      dplyr::select(-new_var_label)
  }

  if (!is.null(code_col) && !is.null(value_label_col)) {
    val_lbl_map <- codebook_df |>
      dplyr::select(
        var_name = !!rlang::sym(var_col),
        code     = !!rlang::sym(code_col),
        lbl      = !!rlang::sym(value_label_col)
      ) |>
      dplyr::filter(!is.na(code), !is.na(lbl)) |>
      dplyr::mutate(lbl = .normalize_text(lbl)) |>
      dplyr::group_by(var_name) |>
      dplyr::summarise(
        cb_values = list(as.character(code)),
        cb_labels = list(lbl),
        .groups   = "drop"
      )

    metadata <- metadata |>
      dplyr::left_join(val_lbl_map, by = "var_name") |>
      dplyr::mutate(
        values     = purrr::map2(values,     cb_values, ~ if (!is.null(.y)) .y else .x),
        labels     = purrr::map2(labels,     cb_labels, ~ if (!is.null(.y)) .y else .x),
        new_labels = purrr::map2(new_labels, cb_labels, ~ if (!is.null(.y)) .y else .x)
      ) |>
      dplyr::select(-cb_values, -cb_labels)
  }

  metadata
}


# ============================================================
# 4. metadata_fix_binary()
# ============================================================

#' Standardise binary variable labels using the desc column
#'
#' For rows with detected_role == "factor_binary" and desc != NA:
#'   - desc = TRUE  → labels[[1]] is positive, labels[[2]] is negative
#'   - desc = TRUE  → labels[[1]] is positive (already first)
#'   - desc = FALSE → labels[[2]] is positive; values and new_labels are both
#'     reordered so positive comes first (preserving values↔labels alignment)
#'   - Positive level → "1-<var_label>" (or original label if use_var_label=FALSE)
#'   - Negative level → "2-Non"
#'   - Any remaining levels (missing candidates already "NULL") → "NULL"
#'   - After fix: fct_relevel(..., sort) gives correct 1→2 order in factors
#'
#' For rows with desc == NA:
#'   - Warns and skips. Set desc in the meta_json variables section first.
#'
#' @param metadata       Varmod tibble.
#' @param use_var_label  If TRUE (default), positive level → "1-<var_label>".
#'                       If FALSE, keeps original positive label text (still adds "1-" prefix).
#'
#' @return Updated metadata tibble. Both new_labels and values reordered for desc=FALSE rows.
metadata_fix_binary <- function(metadata, use_var_label = TRUE) {
  unresolved <- metadata |>
    dplyr::filter(detected_role == "factor_binary", is.na(desc)) |>
    dplyr::pull(var_name)

  if (length(unresolved) > 0) {
    warning(length(unresolved), " factor_binary variable(s) with desc=NA skipped: ",
            paste(unresolved, collapse = ", "),
            "\nSet desc in the meta_json variables section, then re-run extract_survey_metadata().")
  }

  metadata |>
    dplyr::mutate(
      # Reorder values in sync with new_labels when desc=FALSE
      values = purrr::pmap(
        list(detected_role, values, new_labels, desc),
        function(role, vals, nls, dsc) {
          if (role != "factor_binary" || is.na(dsc) || isTRUE(dsc)) return(vals)
          # desc=FALSE: positive is currently second → reorder to [pos, neg, others…]
          non_null <- which(nls != "NULL")
          if (length(non_null) < 2) return(vals)
          pos_idx <- non_null[[2]]; neg_idx <- non_null[[1]]
          other   <- setdiff(seq_along(vals), c(pos_idx, neg_idx))
          vals[c(pos_idx, neg_idx, other)]
        }
      ),
      new_labels = purrr::pmap(
        list(detected_role, var_label, new_labels, desc),
        function(role, lbl, nls, dsc) {
          if (role != "factor_binary" || is.na(dsc)) return(nls)

          # Identify the two non-NULL positions
          non_null <- which(nls != "NULL")
          if (length(non_null) < 2) return(nls)

          # desc=TRUE: positive is first; desc=FALSE: positive is second
          pos_idx <- if (isTRUE(dsc)) non_null[[1]] else non_null[[2]]
          neg_idx <- if (isTRUE(dsc)) non_null[[2]] else non_null[[1]]

          pos_orig <- nls[[pos_idx]]
          pos_label <- if (use_var_label && lbl != "") {
            paste0("1-", lbl)
          } else {
            paste0("1-", pos_orig)
          }
          neg_label <- paste0("2-Non")

          result <- rep("NULL", length(nls))
          result[[pos_idx]] <- pos_label
          result[[neg_idx]] <- neg_label

          # If desc=FALSE, reorder so positive (now "1-…") is first
          if (!isTRUE(dsc)) {
            other <- setdiff(seq_along(result), c(pos_idx, neg_idx))
            result <- result[c(pos_idx, neg_idx, other)]
          }
          result
        }
      )
    )
}


# ============================================================
# 5. export_metadata_excel()
# ============================================================

#' Export metadata tibble to Excel for visual review
#'
#' Read-only review file. Do NOT modify the Excel file and re-import —
#' make corrections in R (override vectors in extract_survey_metadata()).
#' Orange rows = variables needing role refinement (AI or manual).
#'
#' @param metadata        Varmod tibble from extract_survey_metadata().
#' @param path            Output path. Default: "metadata_review.xlsx".
#' @param highlight_roles detected_role values to highlight orange.
#'                        Default: factor_nominal, factor_binary (desc=NA), integer.
#' @param show_missing    If FALSE (default), the labels column shows only
#'                        non-missing labels. If TRUE, shows all labels including
#'                        missing-flagged ones.
#' @param hide_cols       Column names to exclude from the Excel output.
#'                        Default hides new_labels, new_name (cluttered
#'                        before AI label/name suggestion steps are done).
#' @param max_labels      Max number of labels to show per variable in the labels
#'                        and new_labels columns. Default Inf (show all).
#'
#' @return Invisibly returns path.
export_metadata_excel <- function(
    metadata,
    path            = "metadata_review.xlsx",
    highlight_roles = c("factor_nominal", "integer"),
    show_missing    = FALSE,
    hide_cols       = c("labels", "new_name"),
    max_labels      = Inf
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx is required. Install with: install.packages('openxlsx')")
  }

  # --- Flatten list columns to readable strings ---
  n_lbl <- if (is.infinite(max_labels)) .Machine$integer.max else as.integer(max_labels)
  collapse_labels <- function(lbls, vals, miss) {
    # Filter missing if show_missing = FALSE
    if (!show_missing && length(miss) > 0) {
      keep <- !(vals %in% miss)
      lbls <- lbls[keep]
    }
    if (length(lbls) == 0) return("")
    paste(head(lbls, n_lbl), collapse = " / ")
  }
  collapse_miss_labels <- function(miss_vals, lbls, vals) {
    if (length(miss_vals) == 0) return("")
    # Show labels for missing values (not raw codes)
    miss_lbls <- lbls[vals %in% miss_vals]
    if (length(miss_lbls) == 0) miss_lbls <- as.character(miss_vals)
    paste(miss_lbls, collapse = "; ")
  }
  collapse_new_labels <- function(nls) {
    if (length(nls) == 0) return("")
    paste(head(nls, n_lbl), collapse = " / ")
  }

  df_excel <- metadata |>
    dplyr::mutate(
      missing_vals_str  = purrr::pmap_chr(
        list(missing_vals, labels, values),
        ~ collapse_miss_labels(..1, ..2, ..3)
      ),
      labels_str        = purrr::pmap_chr(
        list(labels, values, missing_vals),
        ~ collapse_labels(..1, ..2, ..3)
      ),
      new_labels_str    = purrr::map_chr(new_labels, collapse_new_labels),
      desc_str          = dplyr::case_when(
        is.na(desc)  ~ "NA",
        desc == TRUE ~ "TRUE",
        .default      = "FALSE"
      )
    ) |>
    dplyr::select(
      var_name, var_label, r_class, detected_role, desc = desc_str, n_distinct,
      missing_vals = missing_vals_str,
      labels       = labels_str,
      new_labels   = new_labels_str,
      new_name
    )

  # Also highlight factor_binary rows with desc=NA
  highlight_rows_extra <- if ("factor_binary" %in% metadata$detected_role) {
    which(metadata$detected_role == "factor_binary" & is.na(metadata$desc))
  } else integer(0)

  # Remove hidden columns
  if (length(hide_cols) > 0) {
    df_excel <- dplyr::select(df_excel, -dplyr::any_of(hide_cols))
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "metadata")

  openxlsx::writeData(wb, "metadata", df_excel, startRow = 1, startCol = 1,
                      headerStyle = openxlsx::createStyle(
                        fontColour = "#FFFFFF", fgFill = "#2F4F7F",
                        halign = "left", textDecoration = "bold"
                      ))

  openxlsx::freezePane(wb, "metadata", firstRow = TRUE)

  # Column widths — adapt to which columns remain
  all_cols   <- c("var_name", "var_label", "r_class", "detected_role", "desc",
                  "n_distinct", "missing_vals", "labels", "new_labels", "new_name")
  all_widths <- c(20,          40,           12,        22,              8,
                  10,           30,            45,        45,             20)
  shown_cols <- intersect(names(df_excel), all_cols)
  shown_w    <- all_widths[match(shown_cols, all_cols)]
  purrr::walk2(seq_along(shown_w), shown_w, function(col, w) {
    openxlsx::setColWidths(wb, "metadata", cols = col, widths = w)
  })

  # Orange rows = highlight_roles + factor_binary with desc=NA
  orange_idx <- union(
    which(df_excel$detected_role %in% highlight_roles),
    highlight_rows_extra
  )
  if (length(orange_idx) > 0) {
    openxlsx::addStyle(wb, "metadata",
      style = openxlsx::createStyle(fgFill = "#FFD580"),
      rows  = orange_idx + 1, cols = seq_len(ncol(df_excel)), gridExpand = TRUE
    )
  }

  # Alternating light grey for non-highlighted rows
  other_rows <- setdiff(seq_len(nrow(df_excel)), orange_idx)
  even_rows  <- other_rows[other_rows %% 2 == 0]
  if (length(even_rows) > 0) {
    openxlsx::addStyle(wb, "metadata",
      style = openxlsx::createStyle(fgFill = "#F5F5F5"),
      rows  = even_rows + 1, cols = seq_len(ncol(df_excel)), gridExpand = TRUE
    )
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  message("Metadata written to: ", path,
          " (", length(orange_idx), " orange rows need attention)")

  invisible(path)
}


# ============================================================
# 6. apply_survey_formats()
# ============================================================

#' Apply metadata to a dataframe: recode to factors, rename, re-label
#'
#' Terminal step. Applies factor recoding to factor_binary/nominal/ordinal roles.
#' Values in missing_vals are mapped to NA. new_labels == "NULL" → NA.
#' factor_ordinal variables keep explicit level order (not alphabetically sorted).
#'
#' @param df               Tibble from import_survey().
#' @param metadata         Fully-reviewed varmod tibble.
#' @param uppercase_names  UPPERCASE all output variable names. Default TRUE.
#'
#' @return A tibble with factors applied and variables renamed.
apply_survey_formats <- function(df, metadata, uppercase_names = TRUE) {
  meta <- metadata |> dplyr::filter(var_name %in% names(df))

  non_factor_roles <- c("double", "integer", "integer_scale", "integer_count",
                        "identifier", "other")

  for (i in seq_len(nrow(meta))) {
    row       <- meta[i, ]
    vname     <- row$var_name
    vals      <- row$values[[1]]
    nls       <- row$new_labels[[1]]
    miss_vals <- row$missing_vals[[1]]
    role      <- row$detected_role
    is_ord    <- role == "factor_ordinal"

    if (role %in% non_factor_roles) next
    if (length(vals) == 0 || length(nls) == 0) next

    null_mask  <- nls == "NULL"
    keep_mask  <- !null_mask
    recode_vec <- purrr::set_names(as.character(vals[keep_mask]), nls[keep_mask])

    raw_char  <- as.character(df[[vname]])
    recoded   <- dplyr::recode(raw_char, !!!recode_vec, .default = NA_character_)
    # Also NA-ify values in missing_vals (catches codes not in new_labels)
    miss_char <- as.character(miss_vals)
    recoded   <- dplyr::if_else(raw_char %in% miss_char, NA_character_, recoded)

    df[[vname]] <- factor(recoded)
    if (!is_ord) df[[vname]] <- forcats::fct_relevel(df[[vname]], sort)
  }

  # Rename
  rename_map <- meta |>
    dplyr::filter(new_name != var_name, new_name != "") |>
    dplyr::select(var_name, new_name)
  if (nrow(rename_map) > 0) {
    rename_vec <- purrr::set_names(rename_map$var_name, rename_map$new_name)
    df <- dplyr::rename(df, !!!rename_vec)
  }

  if (uppercase_names) names(df) <- toupper(names(df))

  # Re-apply variable labels
  label_map <- meta |>
    dplyr::mutate(
      final_name = dplyr::if_else(new_name != "" & new_name != var_name,
                                  new_name, var_name),
      final_name = if (uppercase_names) toupper(final_name) else final_name,
      lbl        = var_label
    ) |>
    dplyr::filter(lbl != "", final_name %in% names(df))

  if (nrow(label_map) > 0) {
    lbl_vec <- purrr::set_names(label_map$lbl, label_map$final_name)
    df <- labelled::set_variable_labels(df, .labels = lbl_vec)
  }

  df
}


# ============================================================
# 7. generate_format_script()
# ============================================================

#' Generate a readable _recode.R script for students
#'
#' Output script has no dependency on data_formatting_pipeline.R —
#' it uses base R, dplyr, and forcats only. Ordinal variables use
#' fct_relevel() with explicit level order instead of sort().
#'
#' @param metadata    Fully-reviewed varmod tibble.
#' @param df_name     Object name in generated script (e.g. "prfe").
#' @param input_path  Path to original data file (for documentation).
#' @param output_path Output .R file path. NULL = paste0(df_name, "_recode.R").
#' @param n_obs       Number of observations (optional, for header).
#'
#' @return Invisibly returns script text. Also writes to file.
generate_format_script <- function(
    metadata,
    df_name,
    input_path,
    output_path = NULL,
    n_obs       = NULL
) {
  if (is.null(output_path)) output_path <- paste0(df_name, "_recode.R")

  n_vars <- nrow(metadata)
  n_str  <- if (!is.null(n_obs)) formatC(n_obs, format = "d", big.mark = " ") else "?"
  today  <- format(Sys.Date(), "%Y-%m-%d")

  # Rename block
  rename_rows <- metadata |>
    dplyr::filter(new_name != var_name & new_name != "")
  if (nrow(rename_rows) > 0) {
    rename_lines <- purrr::pmap_chr(rename_rows, function(var_name, new_name, var_label, ...) {
      pad <- strrep(" ", max(0, 22 - nchar(new_name)))
      sprintf('  %s%s= %s,  # "%s"', new_name, pad, var_name,
              substr(var_label, 1, 60))
    })
    rename_block <- paste0(df_name, ' <- ', df_name, ' |> dplyr::rename(\n',
                           paste(rename_lines, collapse = "\n"), '\n)\n')
  } else {
    rename_block <- "# No variables renamed\n"
  }

  # Mutate block
  cat_meta <- metadata |>
    dplyr::filter(detected_role %in% c("factor_binary", "factor_nominal",
                                        "factor_ordinal"))

  if (nrow(cat_meta) > 0) {
    mutate_lines <- purrr::pmap_chr(cat_meta, function(
      var_name, var_label, new_name, new_labels, values, missing_vals,
      detected_role, ...
    ) {
      display <- if (!is.null(new_name) && new_name != "" && new_name != var_name)
        new_name else var_name

      recode_lines <- purrr::map2_chr(new_labels, values, function(nl, v) {
        if (nl == "NULL") sprintf('    "NULL"                         = "%s"  # missing',
                                  as.character(v))
        else sprintf('    "%-35s= "%s"', paste0(nl, '"'), as.character(v))
      })

      relevel_line <- if (detected_role == "factor_ordinal") {
        valid_nls <- new_labels[new_labels != "NULL"]
        lvls <- paste0('"', valid_nls, '"', collapse = ", ")
        paste0('    forcats::fct_relevel(', lvls, ')')
      } else {
        '    forcats::fct_relevel(sort)'
      }

      paste0('\n  # ', display, ': "', substr(var_label, 1, 70), '"\n',
             '  ', display, ' = factor(as.character(', display, ')) |>\n',
             '    forcats::fct_recode(\n',
             paste(recode_lines, collapse = ",\n"), '\n',
             '    ) |>\n', relevel_line, ',')
    })

    mutate_block <- paste0(df_name, ' <- ', df_name, ' |> dplyr::mutate(\n',
                           paste(mutate_lines, collapse = "\n"), '\n)\n')
  } else {
    mutate_block <- "# No categorical variables to format\n"
  }

  # NA-replacement block for integer_scale / integer_count variables.
  # Only replaces values that are confirmed null-coded (missing_vals) for that
  # specific variable — never touches genuine numeric values.
  int_meta <- metadata |>
    dplyr::filter(detected_role %in% c("integer_scale", "integer_count"))

  if (nrow(int_meta) > 0) {
    na_lines <- purrr::pmap_chr(int_meta, function(var_name, new_name,
                                                    missing_vals, var_label, ...) {
      display <- if (!is.null(new_name) && nzchar(new_name) && new_name != var_name)
        new_name else var_name
      miss_num <- suppressWarnings(as.numeric(unlist(missing_vals)))
      miss_num <- sort(unique(miss_num[!is.na(miss_num)]))
      if (length(miss_num) == 0) return(NA_character_)
      vals_str <- paste(miss_num, collapse = ", ")
      paste0('\n  # ', display, ': "', substr(var_label, 1, 70), '"\n',
             '  ', display, ' = dplyr::if_else(', display,
             ' %in% c(', vals_str, '), NA_real_, as.numeric(', display, ')),')
    })
    na_lines <- na_lines[!is.na(na_lines)]

    na_block <- if (length(na_lines) > 0) {
      paste0(df_name, ' <- ', df_name, ' |> dplyr::mutate(\n',
             paste(na_lines, collapse = "\n"), '\n)\n')
    } else {
      "# No integer_scale/integer_count variables with missing codes to recode\n"
    }
  } else {
    na_block <- "# No integer_scale/integer_count variables to recode\n"
  }

  script <- paste0(
    '# === SURVEY FORMATTING SCRIPT: ', toupper(df_name), ' ===\n',
    '# Generated : ', today, '\n',
    '# Source    : ', input_path, '\n',
    '# N = ', n_str, ' | Variables: ', n_vars, '\n',
    '#\n',
    '# HOW TO USE:\n',
    '#   1. Review rename and mutate sections.\n',
    '#   2. Edit labels or names as needed.\n',
    '#   3. Run once to produce ', df_name, '.rds\n',
    '#   4. In your analysis script: readRDS("', df_name, '.rds")\n',
    '\n',
    'library(dplyr)\n',
    'library(forcats)\n',
    'library(labelled)\n',
    '\n',
    '# To regenerate metadata:\n',
    '# source("data_formatting_pipeline.R")\n',
    '# ', df_name, '_raw  <- import_survey("', input_path, '")\n',
    '# ', df_name, '_meta <- extract_survey_metadata(', df_name, '_raw)\n',
    '\n\n',
    '# STEP 1: Import ----\n',
    df_name, ' <- import_survey("', input_path, '")\n',
    '\n\n',
    '# STEP 2: Rename variables ----\n', rename_block,
    '\n\n',
    '# STEP 3: Format categorical variables ----\n', mutate_block,
    '\n\n',
    '# STEP 4: Recode missing values in integer variables ----\n', na_block,
    '\n\n',
    '# STEP 5: Export ----\n',
    df_name, ' |> saveRDS("', df_name, '.rds")\n',
    'message("Done. ', df_name, '.rds written.")\n'
  )

  writeLines(script, output_path, useBytes = FALSE)
  message("Script written to: ", output_path)
  invisible(script)
}


# ============================================================
# 8. AI helpers — httr2 only, no reticulate
# ============================================================

#' Single synchronous call to Claude API
#'
#' @param prompt     User message string.
#' @param model      Model ID. Default: Haiku 4.5 (fast, cheap).
#'                   Use "claude-sonnet-4-6" for messy codebook parsing.
#' @param api_key    ANTHROPIC_API_KEY env var by default.
#' @param max_tokens Max response tokens.
#' @param system     Optional system prompt string.
#'
#' @return Parsed JSON response list.
ai_call_claude <- function(
    prompt,
    model      = "claude-haiku-4-5", # "claude-haiku-4-5-20251001"
    api_key    = Sys.getenv("ANTHROPIC_API_KEY"),
    max_tokens = 4096,
    system     = NULL
) {
  if (api_key == "") stop("ANTHROPIC_API_KEY not set. ",
                          "Run: Sys.setenv(ANTHROPIC_API_KEY = 'sk-...')")

  body <- list(
    model      = model,
    max_tokens = max_tokens,
    messages   = list(list(role = "user", content = prompt))
  )
  if (!is.null(system)) body$system <- system

  httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers("x-api-key" = api_key,
                       "anthropic-version" = "2023-06-01",
                       "content-type"      = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}


#' Submit a Message Batch job (50% cheaper, separate rate limits, async)
#'
#' @param requests  List of lists, each: list(custom_id = "...", prompt = "...")
#' @param model     Model ID. Default: Haiku 4.5.
#' @param api_key   ANTHROPIC_API_KEY env var by default.
#' @param max_tokens Max tokens per response.
#' @param system    Optional system prompt string (forwarded to every request).
#'
#' @return Parsed API response with $id = batch_id for ai_batch_retrieve().
ai_batch_submit <- function(
    requests,
    model      = "claude-haiku-4-5",
    api_key    = Sys.getenv("ANTHROPIC_API_KEY"),
    max_tokens = 4096,
    system     = NULL
) {
  if (api_key == "") stop("ANTHROPIC_API_KEY not set.")

  batch_requests <- unname(purrr::map(requests, function(req) {
    params <- list(model = model, max_tokens = max_tokens,
                   messages = list(list(role = "user", content = req$prompt)))
    if (!is.null(system)) params$system <- system
    list(custom_id = req$custom_id, params = params)
  }))

  resp <- httr2::request("https://api.anthropic.com/v1/messages/batches") |>
    httr2::req_headers("x-api-key" = api_key,
                       "anthropic-version" = "2023-06-01",
                       "content-type"      = "application/json") |>
    httr2::req_body_json(list(requests = batch_requests)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (!is.null(resp$type) && resp$type == "error") {
    stop("Batch submit failed [", resp$error$type, "]: ", resp$error$message)
  }
  if (is.null(resp$id)) {
    stop("Batch submit returned unexpected response (no $id): ",
         jsonlite::toJSON(resp, auto_unbox = TRUE))
  }
  resp
}


#' Poll batch until complete, retrieve results as named list
#'
#' @param batch_id      $id from ai_batch_submit().
#' @param poll_interval Seconds between status checks. Default 30.
#' @param api_key       ANTHROPIC_API_KEY env var by default.
#'
#' @return Named list: custom_id → response text (or list(error = ...) on fail).
ai_batch_retrieve <- function(
    batch_id,
    poll_interval = 30,
    api_key       = Sys.getenv("ANTHROPIC_API_KEY")
) {
  if (api_key == "") stop("ANTHROPIC_API_KEY not set.")

  status_url <- paste0("https://api.anthropic.com/v1/messages/batches/", batch_id)

  repeat {
    status_resp <- httr2::request(status_url) |>
      httr2::req_headers("x-api-key" = api_key,
                         "anthropic-version" = "2023-06-01") |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    proc_status <- status_resp$processing_status
    if (is.null(proc_status)) {
      stop("Batch status check failed for '", batch_id, "': ",
           jsonlite::toJSON(status_resp, auto_unbox = TRUE))
    }
    message("Batch ", batch_id, " \u2014 status: ", proc_status)
    if (proc_status == "ended") break
    Sys.sleep(poll_interval)
  }

  raw_lines <- httr2::request(status_resp$results_url) |>
    httr2::req_headers("x-api-key" = api_key,
                       "anthropic-version" = "2023-06-01") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    stringr::str_split("\n") |>
    purrr::pluck(1) |>
    purrr::discard(~ .x == "")

  parsed <- purrr::map(raw_lines, jsonlite::parse_json)
  purrr::set_names(
    purrr::map(parsed, function(r) {
      if (r[["result"]][["type"]] == "succeeded") {
        r[["result"]][["message"]][["content"]][[1]][["text"]]
      } else {
        type <- r[["result"]][["type"]] %||% "unknown"
        err  <- r[["result"]][["error"]]
        warning("Batch item ", r[["custom_id"]], " ", type,
                ": ", if (!is.null(err)) paste(err, collapse = " ") else "no details")
        list(error = r[["result"]][["error"]])
      }
    }),
    purrr::map_chr(parsed, "custom_id")
  )
}


# ============================================================
# 9. ai_classify_roles()
# ============================================================

#' Classify ambiguous variables with Haiku, print copy-pasteable R vectors
#'
#' Only sends variables that genuinely need refinement:
#'   - factor_nominal  (all: may be ordinal)
#'   - integer         (all: may be integer_scale or integer_count)
#'   - factor_binary with desc=NA (needs desc confirmed)
#' NOT sent: identifier, double, factor_binary with desc resolved,
#'           factor_ordinal/integer_scale/integer_count (already refined).
#'
#' Deduplicates by unique label set before sending — if 50 variables share
#' the same value labels, Haiku classifies the label set once, not 50 times.
#'
#' Role codes:
#'   F = factor_nominal  — unordered categories (professions, régions…)
#'   O = factor_ordinal  — ordered categories (niveau de diplôme, satisfaction…)
#'   B = factor_binary   — 2-level yes/no variable (desc=NA ones only)
#'   S = integer_scale   — numeric scale (1–7 gauche/droite, 0–10 satisfaction)
#'   C = integer_count   — count integer (nb enfants, nb pièces…)
#'   Q = double          — truly continuous (reclassify integer → double)
#'   X = other           — genuinely unclassifiable (mixed type, free text…)
#'   ? = unclear         — kept as current role, user must decide
#'
#' desc field (for B and O only, added as second tab-separated value):
#'   T = descending/positive-first  (O: high→low; B: positive level is listed first)
#'   F = ascending/positive-second  (O: low→high; B: positive level is listed second)
#'   ? = cannot determine
#'
#' @param metadata         Varmod tibble from extract_survey_metadata().
#' @param meta_json        Path to the unified \code{*.survey_meta.json} file
#'                         (required). Results (role, desc) are written directly
#'                         to this file after a backup. Re-run
#'                         extract_survey_metadata() to reload.
#' @param role_examples    Named list of character vectors: example label sets for
#'                         each role, to guide Haiku. Names must be role codes:
#'                         F, O, B, S, C.
#' @param api_key          ANTHROPIC_API_KEY env var by default.
#' @param model            Default: Haiku 4.5.
#' @param chunk_size       Number of unique label sets per API call. Default 400.
#'                         Large chunks preserve cross-variable context and are
#'                         safe: output is ~10 tokens/line, so 400 sets ≈ 4K
#'                         output tokens — well within Haiku's 8K limit.
#' @param use_batch        If TRUE, use the Anthropic batch API instead of
#'                         synchronous calls. Default FALSE.
#' @param dry_run          If TRUE, print the system/user prompts without calling
#'                         the API or writing any file. Default FALSE.
#' @param max_labels_sent  Max non-missing labels per unique label set sent to AI.
#'                         Default 8. Increase for variables with many levels.
#'
#' @return Invisibly returns \code{meta_json}.
#'         In dry_run mode: invisibly returns the list of user prompt strings.
ai_classify_roles <- function(
    metadata,
    meta_json        = NULL,
    role_examples    = list(),
    chunk_size       = 400L,
    use_batch        = FALSE,
    dry_run          = FALSE,
    api_key          = Sys.getenv("ANTHROPIC_API_KEY"),
    model            = "claude-haiku-4-5",
    max_labels_sent  = 8L
) {
  if (is.null(meta_json))
    stop("ai_classify_roles: meta_json is required. ",
         "Provide the path to your *.survey_meta.json file.")
  # --- Filter to ambiguous variables only ---
  target <- metadata |>
    dplyr::filter(
      detected_role %in% c("factor_nominal", "integer") |
        (detected_role == "factor_binary" & is.na(desc))
    )

  if (nrow(target) == 0) {
    message("ai_classify_roles: No ambiguous variables to classify.",
            " (factor_binary with desc resolved, double, identifier already clear.)")
    return(invisible(meta_json))
  }

  message("ai_classify_roles: ", nrow(target), " variable(s) to classify.")

  # --- Build label key per variable (non-missing labels, sorted for dedup) ---
  target <- target |>
    dplyr::mutate(
      .lbl_key = purrr::map2_chr(labels, missing_vals, function(lbls, miss) {
        clean <- sort(.normalize_text(lbls[!lbls %in% miss & nzchar(lbls)]))
        paste(clean, collapse = "\x01")
      })
    )

  # --- Deduplicate by label set: classify each unique set once ---
  unique_sets <- target |>
    dplyr::distinct(.lbl_key, .keep_all = TRUE)

  message("  ", nrow(unique_sets), " unique label set(s) after deduplication",
          if (nrow(unique_sets) < nrow(target))
            paste0(" (", nrow(target) - nrow(unique_sets), " duplicate(s) skipped)")
          else ".")

  # --- Build prompt lines: one per unique label set ---
  prompt_lines <- purrr::pmap_chr(
    list(unique_sets$var_name, unique_sets$var_label, unique_sets$r_class,
         unique_sets$n_distinct, unique_sets$labels, unique_sets$missing_vals,
         unique_sets$detected_role, unique_sets$.lbl_key),
    function(var_name, var_label, r_class, n_distinct, labels, missing_vals,
             detected_role, lbl_key) {
      non_miss <- .normalize_text(labels[!labels %in% missing_vals & nzchar(labels)])
      shown    <- head(non_miss, max_labels_sent)
      n_total  <- length(non_miss)
      lbl_str  <- paste0('"', shown, '"', collapse = ", ")
      suffix   <- if (n_total > max_labels_sent)
        paste0(", +", n_total - max_labels_sent, " more") else ""
      # When all labels are missing codes (nd:0, empty non_miss), append them
      # with a "miss:" prefix so the model sees them as context — not as values.
      # This lets it infer role from the variable label (age, count, year…).
      miss_ctx <- if (n_total == 0 && length(missing_vals) > 0) {
        miss_shown <- head(.normalize_text(missing_vals[nzchar(missing_vals)]),
                           max_labels_sent)
        paste0(", miss:", paste0('"', miss_shown, '"', collapse = ","))
      } else ""
      cur_code <- switch(detected_role,
        factor_binary  = "B", factor_nominal = "F", integer = "I", "?")
      sprintf("SET:%s|%s|cur:%s|nd:%d|[%s%s%s]",
              var_name, substr(.normalize_text(var_label), 1, 55),
              cur_code, n_distinct, lbl_str, suffix, miss_ctx)
    }
  )

  # --- Build examples block from role_examples arg ---
  default_examples <- list(
    F = c("Région parisienne / Bassin parisien / Nord / Est / Ouest / Sud-ouest / Centre-est / Méditerranée", 
      "Agriculteurs exploitants / Artisans, commerçants et chefs d'entreprise / Cadres et professions intellectuelles supérieures / Professions intermédiaires / Employés / Ouvriers"),
    O = c("Très satisfait/Satisfait/Peu satisfait/Pas du tout satisfait", 
      "1 enfant / 2 enfants / 3 enfants ou plus", "Moins de 10 / 10 et 49 / 50 à 499 / 500 et plus", 
      "18-24 ans/25-34 ans/35-44 ans/45-54 ans/55-64 ans/65-79 ans", 
      "Moins de 650 €/De 650 à moins de 950 €/De 950 à moins de 1200 €",
      "Aucun / Moins de 6 / De 6 à moins de 12"),
    S = c("Gauche / 2 / 3 / 4 / 5 / 6 / 7 / 8 / 9 / Droite"),
    C = c("1 / 2 / 3 / 4 / 5 / 7 / 10 / 12 / 17 / 20 / 21",
      "1 enfant / 2 enfants / 3 enfants / 4 enfants / 5 enfants / 6 enfants"),
    B = c("Oui / Non", "Choisi/Non choisi"),
    X = c("6-1-4-5-2-3-7 / 6-1-4-5-2-7-3 / 6-1-4-7-3-2-5 / ...")
  )
  ex <- modifyList(default_examples, role_examples)

  fmt_ex <- function(code, lbls) {
    if (is.null(lbls) || length(lbls) == 0) return(NULL)
    paste0("  ", code, ": ", paste0('"', head(lbls, 4), '"', collapse = ", "))
  }
  examples_block <- paste(c(
    fmt_ex("O", ex[["O"]]),
    fmt_ex("S", ex[["S"]]),
    fmt_ex("C", ex[["C"]]),
    fmt_ex("B", ex[["B"]])
  ), collapse = "\n")

  # Concrete nd:0 examples (no regular labels, infer from variable label)
  nd0_examples_block <- paste0(
    "## nd:0 EXAMPLES (all labels are missing codes — classify from variable label)\n",
    "  SET:Q19E_AGE|quel est votre age ?|cur:I|nd:0|[miss:\"NSP\",\"NVPD\"]  → C\n",
    "  SET:Q19E_ANNEE|annee de naissance|cur:I|nd:0|[miss:\"NSP\",\"NVPD\"]  → C\n",
    "  SET:EA3A|combien de freres|cur:I|nd:0|[miss:\"NSP\",\"NVPD\"]          → C\n",
    "  SET:SEX1|combien de relations de couple|cur:I|nd:0|[miss:\"NSP\"]     → C\n",
    "  SET:EMP10|en quelle annee avez-vous commence|cur:I|nd:0|[miss:\"NSP\"] → C"
  )

  # --- System prompt ---
  system_prompt <- paste0(
    "You classify French social survey variables by their value labels.\n\n",
    "## ROLES\n",
    "F  factor_nominal  : unordered categories (regions, professions, parties)\n",
    "O  factor_ordinal  : ordered categories with a meaningful rank\n",
    "   (satisfaction scales, education levels, frequency: jamais→toujours)\n",
    "B  factor_binary   : exactly 2 levels, one positive / one negative\n",
    "   (only sent when the positive level is unknown)\n",
    "S  integer_scale   : numeric integer used as a scale endpoint-labelled\n",
    "   (political left/right 1–7, satisfaction 0–10, agreement 1–5)\n",
    "C  integer_count   : integer counting real-world items\n",
    "   (number of children, rooms, jobs…)\n",
    "Q  double          : reclassify integer → truly continuous numeric\n",
    "X  other           : genuinely unclassifiable (mixed types, free text, answer orders...)\n",
    "?  unclear         : leave unchanged, user will decide\n\n",
    "## EXAMPLES\n",
    examples_block, "\n\n",
    nd0_examples_block, "\n\n",
    "## desc FIELD (add after a TAB, for O and B only)\n",
    "T = descending / positive-first\n",
    "  O: labels go from highest to lowest (Très satisfait … Pas du tout)\n",
    "  B: positive level is listed FIRST in the label set shown\n",
    "F = ascending / positive-second\n",
    "  O: labels go from lowest to highest (Pas du tout … Très satisfait)\n",
    "  B: positive level is listed SECOND\n",
    "? = cannot determine from the labels shown\n\n",
    "## INPUT FORMAT\n",
    "SET:id|variable label|cur:CURRENT_CODE|nd:N_DISTINCT|[\"lv1\", \"lv2\", ...]\n",
    "  nd = number of distinct NON-MISSING values\n",
    "  When nd:0 the bracket may contain miss:\"...\" entries — these are the\n",
    "  ONLY labels present (all are missing codes like NSP, NVPD, 88, 99).\n",
    "  In that case, classify from the variable label alone:\n",
    "  - age, année de naissance, durée, nombre de…, combien → C\n",
    "  - échelle, score, position → S\n",
    "  - nominal list → F\n\n",
    "## OUTPUT FORMAT — one line per SET, no blank lines, no explanations\n",
    "SET:id TAB CODE [TAB desc] [TAB miss:\"label\"]\n",
    "- Omit desc entirely for F, S, C, Q, X, ?\n",
    "- miss: flag only if a shown label is clearly a missing-value code\n",
    "  (NSP, Refus, NR, non-réponse…) that was not already filtered out"
  )

  # --- Split unique_sets into chunks and build one prompt per chunk ---
  chunks  <- split(seq_len(nrow(unique_sets)),
                   ceiling(seq_len(nrow(unique_sets)) / chunk_size))
  prompts <- purrr::map(chunks, function(idx) {
    n_sets <- length(idx)
    paste0(
      "Classify these ", n_sets, " label set(s):\n\n",
      paste(prompt_lines[idx], collapse = "\n")
    )
  })

  # Max tokens: ~20 tokens per output line is ample (id + code + desc + miss)
  max_tok <- max(256L, ceiling(nrow(unique_sets) / length(chunks)) * 20L)

  # --- Dry run: print prompts and exit without calling the API ---
  if (dry_run) {
    message(strrep("=", 60))
    message("DRY RUN — no API call made")
    message(strrep("=", 60))
    message("Variables: ", nrow(target), "  |  Unique sets: ", nrow(unique_sets),
            "  |  Chunks: ", length(prompts),
            "  |  Route: ", if (use_batch) "batch" else "synchronous",
            "  |  max_tokens: ", max_tok)
    message("\n", strrep("-", 60))
    message("SYSTEM PROMPT")
    message(strrep("-", 60))
    cat(system_prompt, "\n")
    purrr::iwalk(prompts, function(p, i) {
      message("\n", strrep("-", 60))
      message("USER MESSAGE ", i, "/", length(prompts))
      message(strrep("-", 60))
      cat(p, "\n")
    })
    message(strrep("=", 60))
    return(invisible(prompts))
  }

  # --- Route sync vs batch ---
  if (!use_batch) {
    message("ai_classify_roles: synchronous (", nrow(unique_sets), " unique set(s), ",
            length(prompts), " chunk(s))")
    results_text <- purrr::imap(prompts, function(p, i) {
      message("  Chunk ", i, "/", length(prompts))
      resp <- ai_call_claude(p, model = model, api_key = api_key,
                             system = system_prompt, max_tokens = max_tok)
      resp$content[[1]]$text
    })
  } else {
    message("ai_classify_roles: batch mode (", nrow(unique_sets), " unique set(s))")
    requests <- purrr::imap(prompts, ~ list(custom_id = paste0("classify_", .y),
                                            prompt     = .x))
    batch    <- ai_batch_submit(requests, model = model, api_key = api_key,
                                system = system_prompt, max_tokens = max_tok)
    message("Batch submitted. ID: ", batch$id)
    raw          <- ai_batch_retrieve(batch$id, api_key = api_key)
    results_text <- purrr::map(purrr::set_names(names(raw)), ~ raw[[.x]])
  }

  raw_text <- paste(unlist(results_text), collapse = "\n")

  # --- Role code → full role name ---
  role_map <- c(
    F = "factor_nominal",
    O = "factor_ordinal",
    B = "factor_binary",
    S = "integer_scale",
    C = "integer_count",
    Q = "double",
    X = "other",
    "?" = "???"  # unclear → keep nominal, user decides
  )

  # Parse response: map set IDs back to full variable lists
  set_roles   <- character(0)   # keyed by var_name of unique set
  set_descs   <- list()
  set_missing <- character(0)

  resp_lines <- stringr::str_split(stringr::str_trim(raw_text), "\n")[[1]]
  resp_lines <- resp_lines[nzchar(resp_lines)]

  for (ln in resp_lines) {
    parts <- stringr::str_split(ln, "\t")[[1]]
    if (length(parts) < 2) next

    set_id_raw <- stringr::str_trim(parts[[1]])
    # Strip "SET:" prefix if echoed back
    set_id <- stringr::str_remove(set_id_raw, "^SET:")
    code   <- stringr::str_trim(parts[[2]])

    if (!set_id %in% unique_sets$var_name) next
    if (!code %in% names(role_map)) next

    set_roles[[set_id]] <- role_map[[code]]

    # desc (for B and O)
    if (code %in% c("B", "O") && length(parts) >= 3) {
      desc_raw <- stringr::str_trim(parts[[3]])
      if (!stringr::str_starts(desc_raw, "miss:") && nzchar(desc_raw)) {
        set_descs[[set_id]] <- switch(desc_raw, T = TRUE, F = FALSE, "?" = NA,
                                      NA)  # unknown token → NA
      }
    }

    # Extra missing
    miss_part <- purrr::keep(parts[-(1:2)], ~ stringr::str_starts(.x, "miss:"))
    if (length(miss_part) > 0) {
      set_missing[[set_id]] <- .normalize_text(
        stringr::str_remove_all(stringr::str_remove(miss_part[[1]], "^miss:"), '"'))
    }
  }

  # --- Propagate set results back to ALL variables with matching label key ---
  key_to_set <- purrr::set_names(unique_sets$.lbl_key, unique_sets$var_name)

  detected_roles <- character(0)
  desc_overrides <- logical(0)
  extra_missing  <- character(0)

  for (vn in target$var_name) {
    key    <- target$.lbl_key[target$var_name == vn]
    set_id <- names(key_to_set)[key_to_set == key]
    if (length(set_id) == 0 || !set_id %in% names(set_roles)) next
    detected_roles[[vn]] <- set_roles[[set_id]]
    if (set_id %in% names(set_descs))   desc_overrides[[vn]] <- set_descs[[set_id]]
    if (set_id %in% names(set_missing)) extra_missing[[vn]]  <- set_missing[[set_id]]
  }

  # --- Write role/desc results directly to meta_json ---
  .backup_meta_json(meta_json, "classify_roles")
  existing <- .read_meta_json(meta_json)

  n_updated <- 0L
  for (vn in names(detected_roles)) {
    if (!is.null(existing$variables[[vn]])) {
      existing$variables[[vn]]$role <- detected_roles[[vn]]
      n_updated <- n_updated + 1L
    }
  }
  for (vn in names(desc_overrides)) {
    if (!is.null(existing$variables[[vn]])) {
      existing$variables[[vn]]$desc <- desc_overrides[[vn]]
    }
  }

  .write_meta_json(existing, meta_json)

  message("\n", strrep("=", 60))
  message("ai_classify_roles: ", n_updated, " variable(s) updated in: ", meta_json)
  message("Review role/desc fields in the JSON, then re-run extract_survey_metadata().")

  if (length(extra_missing) > 0) {
    uniq_miss <- unique(unname(extra_missing))
    message("\n[!] Possible missing labels flagged by AI — add to missing_chr if correct:")
    message("    ", paste0('"', uniq_miss, '"', collapse = ", "))
  }

  message(strrep("=", 60))
  invisible(meta_json)
}


# ============================================================
# 10. ai_suggest_missing()
# ============================================================

#' Use Haiku to suggest missing value candidates from value labels
#'
#' Collects the LAST `max_vals` value labels from each non-identifier variable
#' (missing codes almost always have the highest numeric codes, so they appear
#' at the end of sorted value lists). Deduplicates across variables and sends
#' a single compact list to Haiku, which returns only the labels it considers
#' likely missing values (NSP, Refus, non-réponse, etc.).
#'
#' Primary output: two copy-pasteable vectors printed to the console:
#'   - `missing_chr`  — character vector of label strings (for missing_chr arg)
#'   - `missing_num`  — numeric vector of the numeric codes embedded in those labels
#'
#' These vectors are CANDIDATES to review manually. Paste them into your
#' extract_survey_metadata() call after reviewing.
#'
#' @param metadata      Varmod tibble from extract_survey_metadata().
#' @param examples      Optional character vector of known missing label strings
#'                      from OTHER datasets (for context only — NOT added to
#'                      output automatically). E.g. c("9-NSP", "99-Refus").
#' @param max_vals      Max value labels to keep per variable (last N, sorted).
#'                      Default 10.
#' @param api_key       ANTHROPIC_API_KEY env var by default.
#' @param model         Default: Haiku 4.5.
#' @param max_tokens    Max response tokens. 512 is ample for a label list.
#' @param debug         If TRUE, prints the raw Haiku response to console before
#'                      parsing. Use when labels are being ignored unexpectedly.
#'
#' @return Invisibly returns list(missing_chr = character(), missing_num = numeric()).
#'         Primary output is console print of copy-pasteable vectors.
ai_suggest_missing <- function(
    metadata,
    examples   = NULL,
    max_vals   = 10L,
    api_key    = Sys.getenv("ANTHROPIC_API_KEY"),
    model      = "claude-haiku-4-5",
    max_tokens = 512L,
    debug      = FALSE
) {
  # Drop identifiers — they have no meaningful value labels to inspect
  target <- metadata |>
    dplyr::filter(detected_role != "identifier")

  if (nrow(target) == 0) {
    message("ai_suggest_missing: No non-identifier variables found.")
    return(invisible(list(missing_chr = character(0), missing_num = numeric(0))))
  }

  # Collect the LAST max_vals labels from each variable (missing codes cluster at end)
  all_labels <- purrr::map(target$labels, function(lbls) {
    if (length(lbls) == 0) return(character(0))
    # Labels are stored in value order; take the tail
    tail(lbls, max_vals)
  })

  # Flatten, normalize, deduplicate, drop blanks
  unique_labels <- unique(.normalize_text(unlist(all_labels, use.names = FALSE)))
  unique_labels <- unique_labels[nzchar(unique_labels)]

  if (length(unique_labels) == 0) {
    message("ai_suggest_missing: No value labels found in metadata.")
    return(invisible(list(missing_chr = character(0), missing_num = numeric(0))))
  }

  message("ai_suggest_missing: ", length(unique_labels),
          " unique tail-labels collected from ", nrow(target), " variables.")

  # Build prompt — send labels WITHOUT numbers so Haiku cannot echo them back
  examples_block <- if (!is.null(examples) && length(examples) > 0) {
    ex_norm <- .normalize_text(examples)
    paste0("\nFor reference, labels like these are typically missing in similar surveys:\n",
           paste(paste0("  ", ex_norm), collapse = "\n"), "\n")
  } else ""

  labels_block <- paste(paste0("  ", unique_labels), collapse = "\n")

  prompt <- paste0(
    "You are identifying missing value labels in French social survey data.\n",
    "From the list below, return ONLY the labels that represent missing values:\n",
    "non-response, refusal, 'ne sait pas', 'NSP', 'NRP', 'REFUS', 'NR',\n",
    "or any label that clearly means the respondent did not give a valid answer.\n",
    examples_block,
    "\nLabels to evaluate:\n",
    labels_block,
    "\n\nReply with ONLY the matching labels, one per line, copied EXACTLY as shown.\n",
    "Do NOT add numbers, bullets, or any prefix. No explanations. No extra text."
  )

  resp <- ai_call_claude(prompt, model = model, api_key = api_key,
                         max_tokens = max_tokens)
  raw_text <- resp$content[[1]]$text

  if (debug) {
    message("\n--- ai_suggest_missing DEBUG: raw Haiku response ---")
    message(raw_text)
    message("--- end raw response ---\n")
  }

  # Parse response: one label per line, normalize, trim whitespace
  returned_labels <- .normalize_text(
    stringr::str_trim(stringr::str_split(raw_text, "\n")[[1]]))
  returned_labels <- returned_labels[nzchar(returned_labels)]

  # Validate: keep only labels that actually appear in unique_labels (exact match)
  # Both sides are normalized so encoding differences don't cause false misses
  valid_labels <- returned_labels[returned_labels %in% unique_labels]
  invalid      <- setdiff(returned_labels, unique_labels)
  if (length(invalid) > 0) {
    message("[!] ai_suggest_missing: ", length(invalid),
            " returned label(s) not found in source labels (ignored — use debug = TRUE to inspect):\n  ",
            paste(invalid, collapse = "\n  "))
  }

  if (length(valid_labels) == 0) {
    message("ai_suggest_missing: No missing-value labels identified by AI.")
    return(invisible(list(missing_chr = character(0), missing_num = numeric(0))))
  }

  # Collect the actual numeric VALUES (not just codes embedded in label strings)
  # that correspond to the identified missing labels across all variables.
  # This robustly captures codes like 88, 99, 9999 regardless of label format.
  norm_valid <- valid_labels  # already normalized above
  missing_vals_out <- sort(unique(suppressWarnings(as.numeric(unlist(
    purrr::map2(target$values, target$labels, function(vals, lbls) {
      norm_lbls <- .normalize_text(lbls)
      vals[norm_lbls %in% norm_valid]
    })
  )))))
  missing_vals_out <- missing_vals_out[!is.na(missing_vals_out)]

  # Also extract numeric codes embedded in label strings as a fallback
  # (e.g. "9-NSP" → 9) — may overlap with missing_vals_out, that's fine.
  num_codes <- purrr::map_dbl(valid_labels, function(lbl) {
    m <- regmatches(lbl, regexpr("^([0-9]+)(?=[-. ]|$)", lbl, perl = TRUE))
    if (length(m) == 1 && nzchar(m)) as.numeric(m) else NA_real_
  })
  missing_num_out <- sort(unique(c(missing_vals_out,
                                   num_codes[!is.na(num_codes)])))

  # Print copy-pasteable output
  message("\n", strrep("=", 60))
  message("ai_suggest_missing: ", length(valid_labels), " missing label(s) found.")
  message("Review, then paste into extract_survey_metadata().")
  message(strrep("=", 60), "\n")

  cat("missing_chr <- c(\n")
  cat(paste0('  "', valid_labels, '"', collapse = ",\n"), "\n")
  cat(")\n\n")

  if (length(missing_num_out) > 0) {
    cat("missing_num <- c(", paste(missing_num_out, collapse = ", "), ")\n\n")
  } else {
    cat("# missing_num: no numeric values found for these labels\n\n")
  }

  message("# Then re-run:")
  message("# meta <- extract_survey_metadata(df,")
  message("#   missing_chr = missing_chr,")
  if (length(missing_num_out) > 0) message("#   missing_num = missing_num)")
  message(strrep("=", 60))

  invisible(list(missing_chr = valid_labels, missing_num = missing_num_out))
}


# ============================================================
# 10b. .compute_merge_groups()
# ============================================================

# ---------------------------------------------------------------------------
# Greedy forward scan that groups contiguous non-null ordinal levels into
# merged bins, respecting a minimum percentage and/or count threshold, and
# stopping early at natural breaks in the code sequence.
#
# A "natural break" is a gap between consecutive numeric codes that is
# strictly larger than the median inter-code gap.  E.g. for codes
# 0,1,2,...,11,12,24,36,... the median gap is 1 (months), so the jump
# 12→24 (+12) is a break.  Non-numeric codes never trigger breaks.
#
# Algorithm (greedy, single forward pass):
#   - Accumulate levels into the current group.
#   - Close the group (and start a new one) when EITHER:
#       (a) running pct  >= min_pct  (and min_pct  > 0), OR
#       (b) running n    >= min_n    (and min_n    > 0), OR
#       (c) a natural break falls BETWEEN the just-added level and the next one
#           (break is checked AFTER satisfying a/b, so a group is never split
#           solely by a break when it is still below threshold).
#   - After the forward pass, if the last group is still below threshold,
#     merge it into the preceding group (if one exists).
#
# @param values     Character vector of value codes in ordinal order (non-null
#                   levels only).
# @param counts     Integer vector parallel to values (observed n per level).
# @param freqs      Numeric vector parallel to values (pct 0-100 per level).
# @param min_pct    Close a group when running pct >= min_pct*100.
#                   0 = threshold not used.  Default 0.05 (→ 5 %).
# @param min_n      Close a group when running n   >= min_n.
#                   0 = threshold not used.  Default 0.
#
# @return Integer vector of the same length as values giving the group id
#         (1-based).  All elements in a group share the same id.
.compute_merge_groups <- function(values, counts, freqs,
                                  min_pct = 0.05, min_n = 0L) {
  n <- length(values)
  if (n == 0L) return(integer(0))

  # Both thresholds disabled → every level is its own group
  use_pct <- isTRUE(min_pct > 0)
  use_n   <- isTRUE(min_n   > 0)
  if (!use_pct && !use_n) return(seq_len(n))

  # Convert min_pct from fraction to 0-100 scale to match freqs
  min_pct_100 <- if (use_pct) min_pct * 100 else Inf

  # ---- Natural break detection ---------------------------------------------
  # Try to parse codes as numbers; if any fail, disable break detection.
  num_codes  <- suppressWarnings(as.numeric(values))
  has_breaks <- !any(is.na(num_codes)) && n >= 3L
  is_break_before <- logical(n)   # is_break_before[i] = TRUE means gap(i-1, i) is a break
  if (has_breaks) {
    gaps        <- abs(diff(num_codes))     # absolute distances, direction-agnostic
    median_gap  <- stats::median(gaps)
    if (median_gap > 0) {
      # A break exists between position i and i+1 when gaps[i] > median_gap
      for (i in seq_along(gaps)) {
        if (gaps[i] > median_gap) is_break_before[i + 1L] <- TRUE
      }
    }
  }

  # ---- Greedy forward scan -------------------------------------------------
  groups      <- integer(n)
  gid         <- 1L
  running_pct <- 0
  running_n   <- 0L

  for (i in seq_len(n)) {
    # Accumulate current level
    running_pct <- running_pct + if (!is.na(freqs[i]))  freqs[i]  else 0
    running_n   <- running_n   + if (!is.na(counts[i])) counts[i] else 0L
    groups[i]   <- gid

    # Decide whether to close this group:
    threshold_met <- (use_pct && running_pct >= min_pct_100) ||
                     (use_n   && running_n   >= min_n)

    if (threshold_met && i < n) {
      # Also close on a natural break even if threshold not yet met for the
      # next level — but only when threshold IS already met here.
      # (Always close when threshold met, regardless of break status.)
      gid         <- gid + 1L
      running_pct <- 0
      running_n   <- 0L
    } else if (i < n && is_break_before[i + 1L] && (running_pct > 0 || running_n > 0L)) {
      # Natural break reached before threshold: close the group anyway so we
      # don't cross a semantic boundary.  The resulting group may be below
      # threshold; the post-pass will handle that.
      gid         <- gid + 1L
      running_pct <- 0
      running_n   <- 0L
    }
  }

  # ---- Post-pass: merge last group upward if still below threshold ---------
  if (gid > 1L) {
    last_ids  <- which(groups == gid)
    last_pct  <- sum(freqs[last_ids],  na.rm = TRUE)
    last_n    <- sum(counts[last_ids], na.rm = TRUE)
    still_low <- (use_pct && last_pct < min_pct_100) ||
                 (use_n   && last_n   < min_n)
    if (still_low) {
      groups[last_ids] <- gid - 1L
    }
  }

  groups
}


# ============================================================
# 11. ai_suggest_labels()
# ============================================================

#' Use Haiku to suggest concise French factor level labels and merge ordinal levels
#'
#' Sends factor variables to Claude (as JSON) and asks it to:
#'   - Shorten all factor level labels to <= 30 characters.
#'   - For factor_ordinal only: merge very small contiguous levels
#'     (freq < 5% AND count < 30) by giving them the same new label.
#'
#' Numeric label prefixes ("1-", "01-") are NOT sent to Haiku — they are rebuilt
#' after merging based on the number of remaining distinct levels.
#'
#' ## Ordering sent to Haiku
#'   - factor_binary, desc=FALSE  : positive sent first (swap stored order);
#'     result is un-swapped before saving.
#'   - factor_binary, desc=TRUE or desc=NA : sent in stored order.
#'   - factor_ordinal, ordinal_desc=TRUE : non-NULL labels sent in reversed order;
#'     un-reversed before saving.
#'   - factor_ordinal, ordinal_desc=FALSE (default) : sent in STORED order as-is.
#'   - factor_nominal : always sent in stored order, never reordered.
#'
#' ## max_levels vs use_batch
#'   max_levels controls how many non-null factor levels go into one API request.
#'   This is a better proxy than a fixed variable count because a chunk of 30
#'   binary variables (60 levels) is far lighter than 30 ordinal/nominal variables
#'   (90–300 levels).  Default 150 gives ~75 variables for binary-heavy surveys
#'   and ~12–20 variables for nominal-heavy surveys.  Variables whose level count
#'   alone exceeds max_levels are skipped with a warning (they would cost too much
#'   API credit for no useful output).
#'   use_batch=TRUE submits everything as a single Anthropic Message Batch job
#'   (cheaper, but asynchronous — requires polling to retrieve results and can
#'   take minutes).  Keep use_batch=FALSE (default) for interactive use; set it
#'   to TRUE only for very large surveys (200+ factor variables).
#'
#' ## Output
#'   Results are merged directly into \code{meta_json} (after a backup).
#'   The metadata table is NOT modified here. Reload with
#'   \code{extract_survey_metadata(df, meta_json = meta_json)}.
#'
#' ## Dry run
#'   dry_run=TRUE prints every prompt that would be sent without making any
#'   API call.  Use this to validate prompts before spending tokens.
#'
#' Sends level counts and frequencies to Haiku for ordinal variables if the
#' metadata table already has level_counts/level_freqs columns (added by
#' metadata_add_level_stats()).  If those columns are absent, labels are sent
#' without counts and Haiku still suggests new labels.
#'
#' @param metadata     Varmod tibble.
#' @param vars         Optional character vector of var_name to restrict to.
#' @param ordinal_desc Logical. If TRUE, send factor_ordinal labels to Haiku in
#'                     descending (high->low) order.  Default FALSE = stored order.
#' @param min_pct      For factor_ordinal merging: close a merge group when the
#'                     running percentage reaches this fraction (0–1 scale).
#'                     Default 0.05 (5 %).  Set to 0 to disable the pct threshold.
#' @param min_n        For factor_ordinal merging: close a merge group when the
#'                     running count reaches this value.  Default 0 (disabled).
#'                     Both min_pct and min_n at 0 disables merging entirely.
#' @param max_levels   Maximum total non-null levels per API request.  Default 150.
#'   For ordinal variables, the level count used for chunking is the number of
#'   merge groups (not raw levels), which is typically much smaller.
#'   Variables whose individual level/group count exceeds max_levels are skipped
#'   with a warning.
#' @param chunk_size   Deprecated. Use max_levels instead.  If supplied, sets
#'   max_levels = chunk_size * 5L with a warning.
#' @param use_batch    Logical. Use the Anthropic Message Batch API (cheaper,
#'                     async).  Default FALSE.
#' @param dry_run      If TRUE, print the prompt(s) that would be sent and
#'                     return invisibly without calling the API.  Default FALSE.
#' @param api_key      ANTHROPIC_API_KEY env var by default.
#' @param model        Default: Haiku 4.5.
#'
#' @return Invisibly returns \code{meta_json}.  In dry_run mode:
#'         invisibly returns a list of the prompt strings.
ai_suggest_labels <- function(
    metadata,
    vars          = NULL,
    ordinal_desc  = FALSE,
    min_pct       = 0.05,
    min_n         = 0L,
    meta_json     = NULL,
    max_levels    = 150L,
    chunk_size    = NULL,
    use_batch     = FALSE,
    dry_run       = FALSE,
    api_key       = Sys.getenv("ANTHROPIC_API_KEY"),
    model         = "claude-haiku-4-5"
) {
  if (!is.null(chunk_size)) {
    warning("ai_suggest_labels: 'chunk_size' is deprecated. ",
            "Use 'max_levels' instead. Converting: max_levels = chunk_size * 5L.")
    max_levels <- chunk_size * 5L
  }
  if (is.null(meta_json))
    stop("ai_suggest_labels: meta_json is required. ",
         "Provide the path to your *.survey_meta.json file.")

  # ---------- filter target variables --------------------------------------
  target <- metadata |>
    dplyr::filter(detected_role %in% c("factor_binary", "factor_nominal",
                                        "factor_ordinal"))
  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) {
    message("ai_suggest_labels: No factor variables to process.")
    return(invisible(meta_json))
  }

  # ---------- compute send_order permutation --------------------------------
  # send_order[i] = j means stored position i is sent at position j.
  # NULL-coded levels keep their index so they can be re-inserted exactly.
  # ordinal_desc=FALSE => identity permutation (stored order kept as-is).
  target <- target |>
    dplyr::mutate(
      .send_order = purrr::pmap(
        list(detected_role, desc, new_labels),
        function(role, dsc, nls) {
          idx <- seq_along(nls)
          if (role == "factor_binary" && isFALSE(dsc)) {
            # positive is stored second among non-NULL; swap to first
            non_null <- which(nls != "NULL")
            if (length(non_null) >= 2) {
              tmp              <- idx[non_null[1]]
              idx[non_null[1]] <- idx[non_null[2]]
              idx[non_null[2]] <- tmp
            }
          } else if (role == "factor_ordinal" && isTRUE(ordinal_desc)) {
            non_null      <- which(nls != "NULL")
            idx[non_null] <- rev(idx[non_null])
          }
          idx  # ordinal_desc=FALSE or nominal => untouched identity permutation
        }
      )
    ) |>
    dplyr::mutate(
      # For ordinal variables: compute merge groups using level stats (if
      # available), then use group count for chunking budget.  For binary /
      # nominal: no merging, each non-NULL level is its own "group".
      .merge_groups = purrr::pmap(
        list(detected_role, values, new_labels,
             if ("level_counts" %in% names(target)) level_counts else vector("list", nrow(target)),
             if ("level_freqs"  %in% names(target)) level_freqs  else vector("list", nrow(target)),
             .send_order),
        function(role, vals, nls, counts, freqs, send_ord) {
          n_tot <- length(nls)
          # Default: each position gets its own singleton group id
          groups_full <- seq_len(n_tot)
          if (role == "factor_ordinal" &&
              (isTRUE(min_pct > 0) || isTRUE(min_n > 0)) &&
              length(counts) == n_tot && length(freqs) == n_tot) {
            # Work in the send-order permutation so groups respect ordinal direction
            non_null_idx <- which(nls[send_ord] != "NULL")
            if (length(non_null_idx) >= 2L) {
              vals_s   <- as.character(vals)[send_ord][non_null_idx]
              counts_s <- counts[send_ord][non_null_idx]
              freqs_s  <- freqs[send_ord][non_null_idx]
              grp_s    <- .compute_merge_groups(vals_s, counts_s, freqs_s,
                                                min_pct = min_pct, min_n = min_n)
              # Place groups back into the full-length (send-order) vector
              groups_send <- seq_len(n_tot)
              groups_send[non_null_idx] <- grp_s
              # Un-permute to original stored order
              inv_ord <- order(send_ord)
              groups_full <- groups_send[inv_ord]
            }
          }
          groups_full   # integer vector, length == length(nls), NULLs get their own id
        }
      ),
      .n_levels = purrr::pmap_int(
        list(detected_role, new_labels, .merge_groups),
        function(role, nls, grps) {
          non_null <- nls != "NULL"
          if (role == "factor_ordinal") {
            # Count distinct merge groups among non-null levels
            length(unique(grps[non_null]))
          } else {
            sum(non_null)
          }
        }
      )
    )

  # Skip variables whose level/group count alone exceeds max_levels (would cost
  # too much API credit and produce no useful output).
  oversized <- dplyr::filter(target, .n_levels > max_levels)
  if (nrow(oversized) > 0) {
    warning("ai_suggest_labels: ", nrow(oversized), " variable(s) skipped — ",
            "non-null level/group count exceeds max_levels (", max_levels, "): ",
            paste(oversized$var_name, collapse = ", "))
    target <- dplyr::filter(target, .n_levels <= max_levels)
  }
  if (nrow(target) == 0) {
    message("ai_suggest_labels: No variables remaining after filtering oversized.")
    return(invisible(meta_json))
  }

  # ---------- JSON builder for one variable ---------------------------------
  .build_var_json <- function(var_name, var_label, detected_role, desc,
                               values, labels, new_labels, send_order,
                               merge_groups, level_counts, level_freqs) {
    values_ord      <- as.character(values)[send_order]
    labels_ord      <- labels[send_order]
    new_labels_ord  <- new_labels[send_order]
    groups_ord      <- merge_groups[send_order]
    counts_ord      <- if (length(level_counts) > 0) level_counts[send_order] else integer(0)
    freqs_ord       <- if (length(level_freqs)  > 0) level_freqs[send_order]  else numeric(0)

    keep         <- new_labels_ord != "NULL"
    values_keep  <- values_ord[keep]
    labels_keep  <- labels_ord[keep]
    groups_keep  <- groups_ord[keep]
    counts_keep  <- counts_ord[keep]
    freqs_keep   <- freqs_ord[keep]

    if (length(labels_keep) == 0) return(NULL)

    # Fallback: if values are empty, use positional indices as pseudo-codes
    if (length(values_keep) == 0 || all(nchar(values_keep) == 0L))
      values_keep <- as.character(seq_along(labels_keep))

    type_str <- switch(detected_role,
      factor_binary  = "binary",
      factor_ordinal = "ordinal",
      factor_nominal = "nominal",
      "nominal"
    )

    esc <- function(x) gsub('"', '\\"', x, fixed = TRUE)

    # Strip variable name prefix from var_label before sending to AI
    # (Stata labels often start with "VARNAME. " or "VARNAMEa. " — redundant tokens)
    var_label_clean <- sub("^[A-Za-z][A-Za-z0-9_]*[a-z]?[0-9]*\\.\\s*", "", var_label)

    # ---- For ordinal variables: collapse groups before sending to AI --------
    # Each group is one entry: key = first value code of the group (in send-order),
    # label = original labels joined by " / ", counts/freqs summed.
    # .parse_labels_json_responses() matches AI output by that key and expands
    # the label back to all member levels.
    if (detected_role == "factor_ordinal" && length(unique(groups_keep)) < length(groups_keep)) {
      gids      <- unique(groups_keep)
      n_groups  <- length(gids)

      g_keys    <- character(n_groups)
      g_labels  <- character(n_groups)

      for (gi in seq_along(gids)) {
        gid  <- gids[gi]
        idx  <- which(groups_keep == gid)
        orig_labels <- labels_keep[idx]

        # Group label sent to AI: list of original labels joined by " / "
        g_labels[gi] <- paste(unique(orig_labels), collapse = " / ")
        # Key: first code in the group
        g_keys[gi]   <- values_keep[idx[1]]
      }

      # Build levels JSON using group keys
      kv_pairs    <- paste0('"', esc(g_keys), '":"', esc(g_labels), '"')
      levels_json <- paste0("{", paste(kv_pairs, collapse = ", "), "}")

      obj <- paste0('{"var":"', esc(var_name), '","type":"', type_str,
                    '","desc":"', esc(substr(var_label_clean, 1, 120)), '",',
                    '"levels":', levels_json)

      return(paste0(obj, "}"))
    }

    # ---- Non-ordinal or ordinal with no merging: send raw levels ------------
    kv_pairs    <- paste0('"', esc(values_keep), '":"', esc(labels_keep), '"')
    levels_json <- paste0("{", paste(kv_pairs, collapse = ", "), "}")

    obj <- paste0('{"var":"', esc(var_name), '","type":"', type_str,
                  '","desc":"', esc(substr(var_label_clean, 1, 120)), '",',
                  '"levels":', levels_json)

    paste0(obj, "}")
  }

  # ---------- system prompt (loaded once from .md file) ---------------------
  # Search order: (1) installed package, (2) project root relative to getwd().
  .pkg_name <- utils::packageName()
  .prompt_md_path <- if (!is.null(.pkg_name) && nzchar(.pkg_name)) {
    system.file("instructions/levels_rename_prompt_JSON.md", package = .pkg_name)
  } else {
    ""
  }
  if (!nzchar(.prompt_md_path) || !file.exists(.prompt_md_path)) {
    .prompt_md_path <- file.path(getwd(), "instructions",
                                 "levels_rename_prompt_JSON.md")
  }
  system_prompt <- if (file.exists(.prompt_md_path)) {
    paste(readLines(.prompt_md_path, encoding = "UTF-8", warn = FALSE),
          collapse = "\n")
  } else {
    warning("ai_suggest_labels: instructions/levels_rename_prompt_JSON.md not found; ",
            "falling back to inline rules.")
    paste0(
      "Tu es un assistant de recodage de labels de variables d'enquete en sociologie.\n",
      "Reponds UNIQUEMENT avec un objet JSON dont les valeurs sont des objets ",
      "codes->nouveaux labels : ",
      '{"VARNAME1": {"1": "label A", "2": "label B"}, "VARNAME2": {"1": "label X"}}\n',
      "Aucun commentaire ni markdown."
    )
  }

  # ---------- user message builder for a chunk (data only) ------------------
  build_prompt <- function(chunk_df) {
    json_objects <- purrr::pmap(
      dplyr::select(chunk_df, var_name, var_label, detected_role, desc,
                    values, labels, new_labels, .send_order, .merge_groups,
                    dplyr::any_of(c("level_counts", "level_freqs"))),
      function(var_name, var_label, detected_role, desc,
               values, labels, new_labels, .send_order, .merge_groups,
               level_counts = integer(0), level_freqs = numeric(0)) {
        .build_var_json(var_name, var_label, detected_role, desc,
                        values, labels, new_labels, .send_order, .merge_groups,
                        level_counts, level_freqs)
      }
    ) |> purrr::compact()

    if (length(json_objects) == 0) return(NULL)

    paste0("[\n", paste(json_objects, collapse = ",\n"), "\n]")
  }

  chunks <- local({
    chunk_ids <- integer(nrow(target))
    cid   <- 1L
    cumul <- 0L
    for (i in seq_len(nrow(target))) {
      n <- target$.n_levels[[i]]
      if (cumul + n > max_levels && cumul > 0L) {
        cid   <- cid + 1L
        cumul <- 0L
      }
      chunk_ids[[i]] <- cid
      cumul <- cumul + n
    }
    split(target, chunk_ids)
  })
  prompts <- purrr::map(chunks, build_prompt) |> purrr::compact()

  # ---------- dry run -------------------------------------------------------
  if (dry_run) {
    message(strrep("=", 60))
    message("DRY RUN — no API call made")
    message(strrep("=", 60))
    message("Variables: ", nrow(target), "  |  Chunks: ", length(prompts),
            "  |  Levels budget: ", max_levels, " per chunk",
            "  |  Route: ", if (use_batch) "batch" else "synchronous")
    message("\n", strrep("-", 60))
    message("SYSTEM PROMPT")
    message(strrep("-", 60))
    cat(system_prompt, "\n")
    purrr::iwalk(prompts, function(p, i) {
      message("\n", strrep("-", 60))
      message("USER MESSAGE ", i, "/", length(prompts))
      message(strrep("-", 60))
      cat(p, "\n")
    })
    message(strrep("=", 60))

    # Write stats-only (counts/freqs) so they survive the session.
    stats_map <- .build_stats_only_map(target)
    if (length(stats_map) > 0) {
      .backup_meta_json(meta_json, "labels_dryrun")
      existing <- .read_meta_json(meta_json)
      existing$variables <- .merge_labels_into_meta_vars(existing$variables, stats_map)
      .write_meta_json(existing, meta_json)
      message("ai_suggest_labels dry_run: stats written to: ", meta_json)
    }

    return(invisible(prompts))
  }

  # ---------- API calls -----------------------------------------------------
  if (!use_batch) {
    message("ai_suggest_labels: synchronous (", nrow(target), " vars, ",
            length(prompts), " chunk(s))")
    results_text <- purrr::imap(prompts, function(p, i) {
      message("  Chunk ", i, "/", length(prompts))
      resp <- ai_call_claude(p, model = model, api_key = api_key,
                             system = system_prompt)
      resp$content[[1]]$text
    })
  } else {
    message("ai_suggest_labels: batch mode (", nrow(target), " vars)")
    requests <- purrr::imap(prompts, ~ list(custom_id = paste0("labels_", .y),
                                            prompt     = .x))
    batch    <- ai_batch_submit(requests, model = model, api_key = api_key,
                                system = system_prompt)
    message("Batch submitted. ID: ", batch$id)
    raw      <- ai_batch_retrieve(batch$id, api_key = api_key)
    results_text <- purrr::map(purrr::set_names(names(raw)), ~ raw[[.x]])
  }

  # ---------- parse + write to disk -----------------------------------------
  parsed_map <- .parse_labels_json_responses(results_text, target)

  if (length(parsed_map) == 0) {
    warning("ai_suggest_labels: No valid responses to write.")
    return(invisible(meta_json))
  }

  # Enrich parsed_map with level_counts/level_freqs for ALL factor types
  # (binary and nominal were not sent to Haiku but stats are still useful).
  # Stored in original metadata order (parallel to labels / new_labels).
  parsed_map <- .enrich_labels_map_with_stats(parsed_map, target)

  # Deep-merge new labels into the unified meta_json (backup first)
  .backup_meta_json(meta_json, "labels")
  existing <- .read_meta_json(meta_json)
  existing$variables <- .merge_labels_into_meta_vars(existing$variables, parsed_map)
  .write_meta_json(existing, meta_json)
  message("ai_suggest_labels: ", length(parsed_map), " variable(s) merged into: ", meta_json)
  message("Reload with extract_survey_metadata(df, meta_json = \"", meta_json, "\")")
  invisible(meta_json)
}

# ---------------------------------------------------------------------------
# Write the labels map produced by .build_levels_map() to disk as UTF-8 JSON.
#
# Format: outer structure is pretty-printed (one variable per block), but each
# level entry is collapsed to a single line for human readability, e.g.:
#
#   "prfc1_Q1": {
#     "role": "factor_binary",
#     "levels": {
#       "1": { "label": "Oui, choisie", "new_label": "Choisie", "n": 451, "pct": 61 },
#       "9": { "label": "NSP",          "null_coded": true }
#     }
#   }
#
# Scalars are unboxed (no spurious [brackets]). UTF-8 written with useBytes.
.write_labels_json <- function(labels_map, path) {
  esc <- function(s) gsub("\\", "\\\\", gsub('"', '\\"', as.character(s), fixed = TRUE), fixed = TRUE)
  # Right-pad a string to width w (nchar in Unicode code points).
  rpad <- function(s, w) {
    n <- nchar(s, type = "chars")
    if (n < w) paste0(s, strrep(" ", w - n)) else s
  }

  var_blocks <- purrr::imap_chr(labels_map, function(var_entry, vname) {
    role_str <- esc(as.character(var_entry$role[[1]]))
    levels   <- var_entry$levels
    n_lev    <- length(levels)
    if (n_lev == 0L) {
      return(paste0('  "', esc(vname), '": {\n    "role": "', role_str, '",\n    "levels": {}\n  }'))
    }

    # ---- per-level raw field strings (unpadded) ----------------------------
    has_new_label <- any(purrr::map_lgl(levels, ~ !is.null(.x$new_label)))
    has_null      <- any(purrr::map_lgl(levels, ~ isTRUE(.x$null_coded)))
    has_n         <- any(purrr::map_lgl(levels, ~ !is.null(.x$n)))
    has_pct       <- any(purrr::map_lgl(levels, ~ !is.null(.x$pct)))

    val_keys   <- names(levels)
    f_key      <- paste0('"', purrr::map_chr(val_keys, esc), '"')
    f_label    <- purrr::map_chr(levels, function(lev)
                    paste0('"', esc(lev$label), '"'))
    f_new_lbl  <- if (has_new_label) purrr::map_chr(levels, function(lev)
                    if (!is.null(lev$new_label)) paste0('"', esc(lev$new_label), '"') else '""') else NULL
    f_n        <- if (has_n) purrr::map_chr(levels, function(lev)
                    if (!is.null(lev$n)) as.character(as.integer(lev$n)) else "") else NULL
    f_pct      <- if (has_pct) purrr::map_chr(levels, function(lev)
                    if (!is.null(lev$pct)) as.character(as.integer(lev$pct)) else "") else NULL

    # ---- column widths (max across all levels of this variable) ------------
    w_key   <- max(nchar(f_key,   type = "chars"))
    w_label <- max(nchar(f_label, type = "chars"))
    w_new   <- if (has_new_label) max(nchar(f_new_lbl, type = "chars")) else 0L
    # null_coded is always 4 chars ("true") when present; no variable width needed
    w_n     <- if (has_n)   max(nchar(f_n,   type = "chars")) else 0L
    w_pct   <- if (has_pct) max(nchar(f_pct, type = "chars")) else 0L

    # ---- assemble one line per level ---------------------------------------
    # Each field is padded to the column-max width for this variable.
    # null_coded rows omit n/pct; real rows omit null_coded — no blank gaps.
    level_lines <- character(n_lev)
    for (i in seq_len(n_lev)) {
      lev     <- levels[[i]]
      is_null <- isTRUE(lev$null_coded)

      tokens <- character(0)
      # label — always present, padded to column width
      tokens <- c(tokens, paste0('"label": ', rpad(f_label[[i]], w_label)))
      # new_label — present only when at least one level has it
      if (has_new_label)
        tokens <- c(tokens, paste0('"new_label": ', rpad(f_new_lbl[[i]], w_new)))
      # null_coded — only emitted on null rows (no blank placeholder on real rows,
      # so n/pct stay directly after label on real rows without a gap)
      if (is_null) {
        tokens <- c(tokens, '"null_coded": true')
      } else {
        # n and pct: right-aligned to their column width
        if (has_n && !is.null(lev$n))
          tokens <- c(tokens, paste0('"n": ', formatC(f_n[[i]], width = w_n, flag = " ")))
        if (has_pct && !is.null(lev$pct))
          tokens <- c(tokens, paste0('"pct": ', formatC(f_pct[[i]], width = w_pct, flag = " ")))
      }

      level_lines[[i]] <- paste0(
        '      ', rpad(f_key[[i]], w_key), ': { ',
        paste(tokens, collapse = ", "),
        ' }'
      )
    }

    # trailing comma on all but the last line
    for (i in seq_len(n_lev - 1L))
      level_lines[[i]] <- paste0(level_lines[[i]], ",")

    levels_body <- paste(level_lines, collapse = "\n")

    paste0(
      '  "', esc(vname), '": {\n',
      '    "role": "', role_str, '",\n',
      '    "levels": {\n',
      levels_body, '\n',
      '    }\n',
      '  }'
    )
  })

  json_str <- paste0("{\n", paste(var_blocks, collapse = ",\n"), "\n}\n")
  writeLines(enc2utf8(json_str), con = path, useBytes = TRUE)
}

# ---------------------------------------------------------------------------
# Parse JSON responses from ai_suggest_labels() into a var_name -> labels map.
# Returns a named list suitable for jsonlite::write_json().
# target: filtered tibble with .send_order column.
.parse_labels_json_responses <- function(results_text, target) {
  # Step 1: collect raw AI outputs → flat var_name -> named-list (keyed) or
  # unnamed vector (legacy positional format) map.
  raw_map <- list()
  for (txt in results_text) {
    if (is.null(txt) || !nzchar(txt)) next
    tryCatch({
      # Broad regex: grab the outermost {...} including nested objects.
      json_str <- stringr::str_extract(txt, "(?s)\\{.+\\}")
      if (is.na(json_str)) stop("No JSON object found")
      parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      for (vname in names(parsed)) {
        val <- parsed[[vname]]
        if (is.list(val) && !is.null(names(val))) {
          # New keyed format: {"value_code": "new_label", ...}
          raw_map[[vname]] <- purrr::map_chr(val, as.character)
        } else if (is.character(val) ||
                   (is.list(val) && all(purrr::map_lgl(val, is.character)))) {
          # Legacy positional array format
          raw_map[[vname]] <- unlist(val)
        }
      }
    }, error = function(e) {
      warning("ai_suggest_labels: parse error: ", conditionMessage(e))
    })
  }

  if (length(raw_map) == 0) return(list())

  # Step 2: join AI labels onto original new_labels, expanding merge groups
  # for ordinal variables back to individual level labels, then un-permute.
  out <- purrr::imap(raw_map, function(ai_result, vname) {
    row <- target[target$var_name == vname, ]
    if (nrow(row) == 0) return(NULL)

    orig_new_labels <- row$new_labels[[1]]
    orig_values     <- as.character(row$values[[1]])
    send_order      <- row$.send_order[[1]]
    merge_groups    <- row$.merge_groups[[1]]
    role            <- row$detected_role[[1]]

    # Apply send_order permutation
    nl_ord     <- orig_new_labels[send_order]
    val_ord    <- if (length(orig_values) == length(orig_new_labels))
                    orig_values[send_order]
                  else
                    as.character(seq_along(nl_ord))
    groups_ord <- merge_groups[send_order]
    keep       <- nl_ord != "NULL"

    if (!is.null(names(ai_result))) {
      # ---- Keyed format: join by value code or group key -------------------
      result_ord <- nl_ord   # default: keep existing label; "NULL" preserved

      if (role == "factor_ordinal" &&
          length(unique(groups_ord[keep])) < sum(keep)) {
        # Group-based matching: AI returned one label per group, keyed by the
        # first code of each group.  Expand the group label to all members.
        gids      <- unique(groups_ord[keep])
        n_matched <- 0L
        for (gid in gids) {
          member_idx <- which(keep & groups_ord == gid)
          # Group key = value code of the first member (in send-order)
          group_key  <- val_ord[member_idx[1]]
          ai_lbl     <- ai_result[[group_key]]
          if (!is.null(ai_lbl) && nzchar(ai_lbl)) {
            result_ord[member_idx] <- ai_lbl
            n_matched              <- n_matched + 1L
          }
        }
        n_groups    <- length(gids)
        n_unmatched <- n_groups - n_matched
        if (n_unmatched > 0)
          message("ai_suggest_labels: ", vname, " — ", n_unmatched, "/",
                  n_groups, " group(s) not matched by key in AI response; ",
                  "original label kept.")
      } else {
        # Non-grouped or non-ordinal: standard level-by-level matching
        n_matched <- 0L
        for (j in which(keep)) {
          key    <- val_ord[j]
          ai_lbl <- ai_result[[key]]
          if (!is.null(ai_lbl) && nzchar(ai_lbl)) {
            result_ord[j] <- ai_lbl
            n_matched     <- n_matched + 1L
          }
        }
        n_send      <- sum(keep)
        n_unmatched <- n_send - n_matched
        if (n_unmatched > 0)
          message("ai_suggest_labels: ", vname, " — ", n_unmatched, "/",
                  n_send, " level(s) not matched by value code in AI response; ",
                  "original label kept.")
      }
    } else {
      # ---- Legacy positional fallback --------------------------------------
      n_keep <- sum(keep)
      if (length(ai_result) != n_keep) {
        warning("ai_suggest_labels: length mismatch for ", vname,
                " (expected ", n_keep, ", got ", length(ai_result), "). Skipped.")
        return(NULL)
      }
      result_ord         <- nl_ord
      result_ord[keep]   <- ai_result
    }

    inv_order <- order(send_order)
    as.list(result_ord[inv_order])
  }) |> purrr::compact()

  out
}

# ---------------------------------------------------------------------------
# Build the on-disk JSON structure for one or all variables.
#
# Each variable entry has the form:
#   { "role": "factor_binary",
#     "levels": {
#       "1": { "label": "Oui — choisie",  "new_label": "Choisie", "n": 451, "pct": 61 },
#       "9": { "label": "NSP / NR",       "null_coded": true }
#     }
#   }
#
# `new_label` is omitted when ai_labels is NULL (dry_run / stats-only).
# `n` / `pct`  are omitted when stats are absent.
# Value codes are the keys — stable join key, never touches AI output.
#
# @param target      Filtered metadata tibble (with .send_order, level_counts,
#                    level_freqs columns).
# @param ai_map      Named list varname -> char vector of AI labels in original
#                    metadata order (full length, NULLs kept).  NULL = dry_run.
.build_levels_map <- function(target, ai_map = NULL) {
  has_counts <- "level_counts" %in% names(target)
  has_freqs  <- "level_freqs"  %in% names(target)
  has_values <- "values"       %in% names(target)
  has_labels <- "labels"       %in% names(target)

  purrr::pmap(
    list(
      vname   = target$var_name,
      role    = target$detected_role,
      vals    = if (has_values) target$values     else vector("list", nrow(target)),
      orig_lb = if (has_labels) target$labels     else vector("list", nrow(target)),
      new_lb  = target$new_labels,
      counts  = if (has_counts) target$level_counts else vector("list", nrow(target)),
      freqs   = if (has_freqs)  target$level_freqs  else vector("list", nrow(target))
    ),
    function(vname, role, vals, orig_lb, new_lb, counts, freqs) {
      n_lev  <- length(new_lb)
      # ai_labels in original metadata order (NULL-coded positions kept as "NULL")
      ai_vec <- ai_map[[vname]]   # NULL when dry_run

      levels_obj <- purrr::imap(
        seq_len(n_lev),
        function(i, ...) {
          val_key  <- if (length(vals)   >= i) as.character(vals[[i]])   else as.character(i)
          cur_lbl  <- if (length(orig_lb) >= i) as.character(orig_lb[[i]]) else ""
          is_null  <- identical(new_lb[[i]], "NULL")
          has_n    <- length(counts) >= i && !is.na(counts[[i]])
          has_p    <- length(freqs)  >= i && !is.na(freqs[[i]])

          entry <- list()
          entry[["label"]] <- cur_lbl
          if (!is.null(ai_vec) && !is_null)
            entry[["new_label"]] <- as.character(ai_vec[[i]])
          if (is_null)
            entry[["null_coded"]] <- TRUE
          if (has_n && !is_null) entry[["n"]]   <- as.integer(counts[[i]])
          if (has_p && !is_null) entry[["pct"]] <- as.integer(freqs[[i]])
          entry
        }
      )
      # Name the levels list by value code (the stable join key)
      val_keys <- purrr::map_chr(seq_len(n_lev), function(i)
        if (length(vals) >= i) as.character(vals[[i]]) else as.character(i))
      names(levels_obj) <- val_keys

      list(role = role, levels = levels_obj)
    }
  ) |> purrr::set_names(target$var_name)
}

# ---------------------------------------------------------------------------
# Wrap parsed_map (varname -> char vector in original order) into the
# on-disk rich format by calling .build_levels_map with the AI results.
.enrich_labels_map_with_stats <- function(parsed_map, target) {
  # parsed_map has only successfully parsed variables; restrict target to those
  sub_target <- target[target$var_name %in% names(parsed_map), ]
  .build_levels_map(sub_target, ai_map = parsed_map)
}

# ---------------------------------------------------------------------------
# Stats-only map for dry_run: same structure but no new_label fields.
.build_stats_only_map <- function(target) {
  .build_levels_map(target, ai_map = NULL)
}

# ---------------------------------------------------------------------------
# Merge a labels_map (from .build_levels_map / .enrich_labels_map_with_stats)
# into the $variables section of a unified survey_meta.json list.
# Only updates the "levels" sub-object; preserves var_label, role, desc,
# new_name untouched.
.merge_labels_into_meta_vars <- function(meta_vars, labels_map) {
  for (vname in names(labels_map)) {
    entry  <- labels_map[[vname]]
    levels <- entry$levels
    if (is.null(levels)) next

    if (is.null(meta_vars[[vname]])) {
      # Variable not yet in unified JSON (e.g. ai_suggest_labels was run before
      # extract_survey_metadata wrote the initial JSON). Create stub.
      meta_vars[[vname]] <- list(
        var_label = "",
        role      = as.character(entry$role[[1]]),
        desc      = NULL,
        new_name  = vname,
        levels    = levels
      )
    } else {
      # Merge only levels; preserve all other fields
      existing_levels <- meta_vars[[vname]]$levels
      if (is.null(existing_levels)) existing_levels <- list()

      # For each level in the new map, update new_label, n, pct
      for (val_code in names(levels)) {
        new_lev  <- levels[[val_code]]
        if (is.null(existing_levels[[val_code]])) {
          existing_levels[[val_code]] <- new_lev
        } else {
          # Keep existing label; update new_label, n, pct, null_coded
          if (!is.null(new_lev$new_label))
            existing_levels[[val_code]]$new_label <- new_lev$new_label
          if (!is.null(new_lev$n))
            existing_levels[[val_code]]$n <- new_lev$n
          if (!is.null(new_lev$pct))
            existing_levels[[val_code]]$pct <- new_lev$pct
          if (isTRUE(new_lev$null_coded))
            existing_levels[[val_code]]$null_coded <- TRUE
        }
      }
      meta_vars[[vname]]$levels <- existing_levels

      # Also update role in meta_vars if it was auto-set in initial JSON
      if (!is.null(entry$role))
        meta_vars[[vname]]$role <- as.character(entry$role[[1]])
    }
  }
  meta_vars
}

# ============================================================
# 11. ai_suggest_varnames()
# ============================================================

#' Use Haiku to suggest short UPPER_SNAKE_CASE R variable names
#'
#' Calls the Anthropic API (synchronous or batch) to propose new names for all
#' variables in `metadata`. Results are written directly to meta_json.
#'
#' @param metadata       Varmod tibble from `extract_survey_metadata()`.
#' @param vars           Optional character vector of `var_name` to restrict.
#' @param meta_json      Path to the unified `*.survey_meta.json` file (required).
#' @param chunk_size     Variables per API request. Default 300L (large enough
#'                       to handle most datasets in one call; dedup handles
#'                       cross-chunk name collisions automatically).
#' @param max_new_labels Max number of non-NULL new labels sent per variable to
#'                       help the model understand the content. Default 4L.
#' @param use_batch      If TRUE, use the Anthropic batch API. Default FALSE.
#' @param dry_run        If TRUE, print prompts without calling the API.
#' @param api_key        Anthropic API key. Default: `ANTHROPIC_API_KEY` env var.
#' @param model          Model to use. Default `"claude-haiku-4-5"`.
#'
#' @return Invisibly: `meta_json` (or the list of prompts in dry_run mode).
ai_suggest_varnames <- function(
    metadata,
    vars           = NULL,
    meta_json      = NULL,
    chunk_size     = 300L,
    max_new_labels = 4L,
    max_tokens     = 20000L,
    use_batch      = FALSE,
    dry_run        = FALSE,
    api_key        = Sys.getenv("ANTHROPIC_API_KEY"),
    model          = "claude-haiku-4-5"
) {
  if (is.null(meta_json))
    stop("ai_suggest_varnames: meta_json is required. ",
         "Provide the path to your *.survey_meta.json file.")

  # ---------- filter target variables ---------------------------------------
  target <- metadata
  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) {
    message("ai_suggest_varnames: No variables to process.")
    return(invisible(meta_json))
  }

  # ---------- system prompt -------------------------------------------------
  .pkg_name <- utils::packageName()
  .prompt_md_path <- if (!is.null(.pkg_name) && nzchar(.pkg_name)) {
    system.file("instructions/varnames_prompt.md", package = .pkg_name)
  } else {
    ""
  }
  if (!nzchar(.prompt_md_path) || !file.exists(.prompt_md_path)) {
    .prompt_md_path <- file.path(getwd(), "instructions", "varnames_prompt.md")
  }
  system_prompt <- if (file.exists(.prompt_md_path)) {
    paste(readLines(.prompt_md_path, encoding = "UTF-8", warn = FALSE),
          collapse = "\n")
  } else {
    warning("ai_suggest_varnames: instructions/varnames_prompt.md not found; ",
            "using minimal inline prompt.")
    paste0(
      "Rename French survey variables to UPPER_SNAKE_CASE R names (max 25 chars).\n",
      'Reply ONLY as a flat JSON object: {"ORIG": "NEW", ...}\n',
      "No comments, no markdown."
    )
  }

  # ---------- user message builder for one chunk ----------------------------
  has_new_labels <- "new_labels" %in% names(target)
  has_labels     <- "labels"     %in% names(target)

  build_prompt <- function(chunk_df) {
    esc <- function(x) gsub('"', '\\"', x, fixed = TRUE)

    objs <- purrr::pmap(chunk_df, function(var_name, var_label, ...) {
      dots <- list(...)

      # Prefer new_labels over original labels, limited to max_new_labels non-NULL
      display_labels <- NULL
      if (has_new_labels) {
        nls <- dots[["new_labels"]]
        non_null <- nls[!is.na(nls) & nls != "NULL"]
        if (length(non_null) > 0)
          display_labels <- head(non_null, max_new_labels)
      }
      if (is.null(display_labels) && has_labels) {
        lbs <- dots[["labels"]]
        if (length(lbs) > 0)
          display_labels <- head(lbs[!is.na(lbs)], max_new_labels)
      }

      desc_short <- esc(substr(var_label, 1, 150))
      obj <- paste0('{"var":"', esc(var_name), '","desc":"', desc_short, '"')
      if (!is.null(display_labels) && length(display_labels) > 0) {
        labs_json <- paste0('["', paste(esc(display_labels), collapse = '","'), '"]')
        obj <- paste0(obj, ',"new_labels":', labs_json)
      }
      paste0(obj, "}")
    })

    paste0("[\n", paste(objs, collapse = ",\n"), "\n]")
  }

  chunks  <- split(target, ceiling(seq_len(nrow(target)) / chunk_size))
  prompts <- purrr::map(chunks, build_prompt)

  # ---------- dry run -------------------------------------------------------
  if (dry_run) {
    message(strrep("=", 60))
    message("DRY RUN — no API call made")
    message(strrep("=", 60))
    message("Variables: ", nrow(target), "  |  Chunks: ", length(prompts),
            "  |  Route: ", if (use_batch) "batch" else "synchronous")
    message("\n", strrep("-", 60))
    message("SYSTEM PROMPT")
    message(strrep("-", 60))
    cat(system_prompt, "\n")
    purrr::iwalk(prompts, function(p, i) {
      message("\n", strrep("-", 60))
      message("USER MESSAGE ", i, "/", length(prompts))
      message(strrep("-", 60))
      cat(p, "\n")
    })
    message(strrep("=", 60))

    message("ai_suggest_varnames dry_run: no stub written (edit meta_json directly)")

    return(invisible(prompts))
  }

  # ---------- API calls -----------------------------------------------------
  cache_dir  <- file.path(tempdir(), "varnames_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  if (!use_batch) {
    message("ai_suggest_varnames: synchronous (", nrow(target), " vars, ",
            length(prompts), " chunk(s))")
    results_text <- purrr::imap(prompts, function(p, i) {
      message("  Chunk ", i, "/", length(prompts))
      resp <- ai_call_claude(p, model = model, api_key = api_key,
                             system = system_prompt, max_tokens = max_tokens)
      txt <- resp$content[[1]]$text
      # Cache raw response for debugging
      cache_file <- file.path(cache_dir, paste0("chunk_", i, "_raw.txt"))
      writeLines(enc2utf8(if (is.null(txt)) "" else txt), cache_file, useBytes = TRUE)
      message("  Raw response cached: ", cache_file)
      txt
    })
  } else {
    message("ai_suggest_varnames: batch mode (", nrow(target), " vars)")
    requests <- purrr::imap(prompts, ~ list(custom_id = paste0("varnames_", .y),
                                            prompt     = .x))
    batch <- ai_batch_submit(requests, model = model, api_key = api_key,
                             system = system_prompt, max_tokens = max_tokens)
    message("Batch submitted. ID: ", batch$id)
    raw   <- ai_batch_retrieve(batch$id, api_key = api_key)
    # Cache raw batch responses
    purrr::iwalk(raw, function(txt, nm) {
      cache_file <- file.path(cache_dir, paste0("batch_", nm, "_raw.txt"))
      writeLines(enc2utf8(if (is.null(txt)) "" else txt), cache_file, useBytes = TRUE)
    })
    message("Raw batch responses cached in: ", cache_dir)
    results_text <- purrr::map(purrr::set_names(names(raw)), ~ raw[[.x]])
  }

  # ---------- parse + dedup + write to disk ---------------------------------
  names_map <- .parse_varnames_json_responses(results_text, target$var_name)

  if (length(names_map) == 0) {
    warning("ai_suggest_varnames: No valid responses to write.")
    return(invisible(meta_json))
  }

  # Build structured map enriched with metadata (new_labels, counts, freqs)
  out_map <- .build_varnames_map(target, names_map)

  # Deep-merge new_name into the unified meta_json (backup first)
  .backup_meta_json(meta_json, "varnames")
  existing <- .read_meta_json(meta_json)
  for (vname in names(out_map)) {
    entry <- out_map[[vname]]
    nn    <- entry[["new_name"]]
    if (!is.null(nn) && nzchar(nn) && !is.null(existing$variables[[vname]])) {
      existing$variables[[vname]]$new_name <- nn
    }
  }
  .write_meta_json(existing, meta_json)
  message("ai_suggest_varnames: ", length(out_map), " variable(s) merged into: ", meta_json)
  message("Reload with extract_survey_metadata(df, meta_json = \"", meta_json, "\")")
  invisible(meta_json)
}


# ============================================================
# 11b. ai_suggest_varnames() helpers
# ============================================================

# ---------------------------------------------------------------------------
# Parse flat {"ORIG": "NEW"} JSON from each chunk response.
# Robust to: markdown code fences, leading/trailing prose, truncated responses.
# Strategy: try valid JSON first; on failure, regex-extract all complete
# "key": "value" pairs from the raw text (handles truncation gracefully).
# Merges chunks, then deduplicates by appending _2, _3, ... (in variable order).
.parse_varnames_json_responses <- function(results_text, all_var_names) {
  raw_map <- list()

  # Extract all complete "key": "value" string pairs from arbitrary text.
  # Works on truncated JSON, prose-wrapped JSON, and fenced code blocks.
  .extract_kv_pairs <- function(txt) {
    # Pattern: "key": "value" — both strings, value may contain escaped chars
    # Uses a non-backtracking approach: find all matches of the pattern
    m <- gregexpr(
      '"((?:[^"\\\\]|\\\\.)*)"\\.?:\\s*"((?:[^"\\\\]|\\\\.)*)"',
      txt, perl = TRUE
    )
    if (m[[1]][[1]] == -1L) return(list())

    starts  <- m[[1]]
    lengths <- attr(m[[1]], "match.length")
    pairs   <- list()

    for (i in seq_along(starts)) {
      chunk <- substr(txt, starts[[i]], starts[[i]] + lengths[[i]] - 1L)
      # Split on the first ": " to get key and value
      colon_pos <- regexpr('":\\s*"', chunk, perl = TRUE)
      if (colon_pos == -1L) next
      key_raw <- substr(chunk, 2L, colon_pos - 1L)
      val_raw <- substr(chunk,
                        colon_pos + attr(colon_pos, "match.length"),
                        nchar(chunk) - 1L)
      # Unescape basic JSON escapes
      unescape <- function(s) {
        s <- gsub('\\\\"', '"',  s, fixed = TRUE)
        s <- gsub("\\\\n", "\n", s, fixed = TRUE)
        s <- gsub("\\\\t", "\t", s, fixed = TRUE)
        s <- gsub("\\\\\\\\", "\\", s, fixed = TRUE)
        s
      }
      key <- unescape(key_raw)
      val <- unescape(val_raw)
      if (nzchar(key) && nzchar(trimws(val)))
        pairs[[key]] <- trimws(val)
    }
    pairs
  }

  for (txt in results_text) {
    if (is.null(txt) || !nzchar(txt)) next

    # Detect truncation: response ends without a closing brace anywhere after the
    # opening brace (trailing prose after } is fine — only flag when no } exists).
    first_open  <- regexpr("\\{", txt, perl = TRUE)[[1]]
    has_close   <- grepl("\\}", txt, perl = TRUE)
    is_truncated <- first_open > 0L && !has_close
    if (is_truncated)
      warning("ai_suggest_varnames: response appears truncated (no closing '}').",
              " Only fully parsed pairs will be used. Increase max_tokens or chunk_size.")

    tryCatch({
      # --- Strategy 1: parse as complete valid JSON ---
      stripped <- gsub("```(?:json)?\\s*\\n?|\\n?```", "", txt, perl = TRUE)
      # Find outermost { ... } by position of first { and last }
      first_brace <- regexpr("\\{", stripped, perl = TRUE)[[1]]
      last_brace  <- tail(gregexpr("\\}", stripped, perl = TRUE)[[1]], 1L)

      parsed_ok <- FALSE
      if (first_brace > 0L && last_brace > first_brace) {
        json_str <- substr(stripped, first_brace, last_brace)
        tryCatch({
          parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
          for (vname in names(parsed)) {
            val <- parsed[[vname]]
            v   <- if (is.list(val) && length(val) == 1L) val[[1]] else val
            if (is.character(v) && length(v) == 1L && nzchar(trimws(v)))
              raw_map[[vname]] <- trimws(v)
          }
          parsed_ok <- TRUE
        }, error = function(e) NULL)
      }

      # --- Strategy 2 (fallback / truncation): regex extract all "K":"V" pairs ---
      if (!parsed_ok || is_truncated) {
        pairs <- .extract_kv_pairs(txt)
        for (k in names(pairs)) {
          if (!k %in% names(raw_map))   # don't overwrite clean parse results
            raw_map[[k]] <- pairs[[k]]
        }
        if (!parsed_ok && length(pairs) == 0L)
          stop("No JSON object and no key-value pairs found")
      }
    }, error = function(e) {
      warning("ai_suggest_varnames: parse error: ", conditionMessage(e))
    })
  }

  if (length(raw_map) == 0) return(character(0))

  # Keep only variables that were in the target
  raw_map <- raw_map[names(raw_map) %in% all_var_names]

  # Deduplicate: for any new_name appearing more than once, append _2, _3 ...
  # in the order variables appear in all_var_names (stable ordering)
  ordered_keys <- all_var_names[all_var_names %in% names(raw_map)]
  new_names    <- unlist(raw_map[ordered_keys])  # character vector, named by orig

  dup_vals <- names(which(table(new_names) > 1))
  if (length(dup_vals) > 0) {
    counters <- integer(length(dup_vals))
    names(counters) <- dup_vals
    for (i in seq_along(new_names)) {
      nm <- new_names[[i]]
      if (nm %in% dup_vals) {
        counters[[nm]] <- counters[[nm]] + 1L
        if (counters[[nm]] > 1L)
          new_names[[i]] <- paste0(nm, "_", counters[[nm]])
      }
    }
    warning("ai_suggest_varnames: ", length(dup_vals),
            " duplicate name(s) detected and disambiguated with numeric suffix: ",
            paste(dup_vals, collapse = ", "))
  }

  new_names  # named character vector: orig_name -> new_name
}

# ---------------------------------------------------------------------------
# Build the on-disk map for ai_suggest_varnames(): one entry per variable with
# new_name plus any available metadata (new_labels, level_counts, level_freqs).
# Returns a named list suitable for .write_varnames_json().
#
# @param target    Filtered metadata tibble (rows for variables being renamed).
# @param names_map Named character vector: orig_name -> new_name (from AI).
.build_varnames_map <- function(target, names_map) {
  has_new_labels <- "new_labels"    %in% names(target)
  has_counts     <- "level_counts"  %in% names(target)
  has_freqs      <- "level_freqs"   %in% names(target)

  purrr::imap(names_map, function(new_name, orig_name) {
    row <- target[target$var_name == orig_name, ]

    entry <- list(new_name = unname(new_name))

    if (nrow(row) == 1L) {
      if (has_new_labels) {
        nls <- row$new_labels[[1]]
        if (length(nls) > 0) entry[["new_labels"]] <- as.list(nls)
      }
      if (has_counts) {
        cts <- row$level_counts[[1]]
        if (length(cts) > 0) entry[["level_counts"]] <- as.list(cts)
      }
      if (has_freqs) {
        fqs <- row$level_freqs[[1]]
        if (length(fqs) > 0) entry[["level_freqs"]] <- as.list(fqs)
      }
    }

    entry
  })
}


# Write the varnames map to a pretty-printed JSON file.
# map: named list of lists, each with a $new_name character element and
#      optionally $new_labels, $level_counts, $level_freqs list columns.
#
# Format (one variable per block, arrays on one line):
#   "ORIG_VAR": { "new_name": "NEW_VAR", "new_labels": ["A","B"], "level_counts": [10,20], "level_freqs": [33,67] }
#   "STUB_VAR": {}
.write_varnames_json <- function(map, path) {
  if (length(map) == 0) {
    writeLines("{}", con = path, useBytes = FALSE)
    return(invisible(path))
  }

  esc   <- function(x) gsub("\\", "\\\\", x, fixed = TRUE) |>
    (\(s) gsub('"', '\\"', s, fixed = TRUE))()
  w_key <- max(nchar(names(map))) + 4L

  # Serialize a list/vector of scalars as a compact JSON array
  arr_str <- function(v) {
    if (is.null(v) || length(v) == 0) return("[]")
    elems <- vapply(v, function(x) {
      if (is.na(x))         "null"
      else if (is.numeric(x)) as.character(as.integer(x))
      else                   paste0('"', esc(as.character(x)), '"')
    }, character(1))
    paste0("[", paste(elems, collapse = ", "), "]")
  }

  lines  <- c("{")
  vnames <- names(map)
  for (i in seq_along(vnames)) {
    vn      <- vnames[[i]]
    entry   <- map[[vn]]
    comma   <- if (i < length(vnames)) "," else ""
    key_str <- paste0('"', esc(vn), '"')
    pad     <- strrep(" ", w_key - nchar(key_str))

    nn <- entry[["new_name"]]
    if (!is.null(nn) && nzchar(nn)) {
      fields <- paste0('"new_name": "', esc(nn), '"')
      if (!is.null(entry[["new_labels"]]) && length(entry[["new_labels"]]) > 0)
        fields <- paste0(fields, ', "new_labels": ', arr_str(entry[["new_labels"]]))
      if (!is.null(entry[["level_counts"]]) && length(entry[["level_counts"]]) > 0)
        fields <- paste0(fields, ', "level_counts": ', arr_str(entry[["level_counts"]]))
      if (!is.null(entry[["level_freqs"]]) && length(entry[["level_freqs"]]) > 0)
        fields <- paste0(fields, ', "level_freqs": ', arr_str(entry[["level_freqs"]]))
      val_str <- paste0("{ ", fields, " }")
    } else {
      val_str <- "{}"
    }
    lines <- c(lines, paste0("  ", key_str, ":", pad, val_str, comma))
  }
  lines <- c(lines, "}")

  writeLines(enc2utf8(paste(lines, collapse = "\n")), con = path, useBytes = TRUE)
  invisible(path)
}


# ---------------------------------------------------------------------------
# Sync null_coded flags in the JSON variables section with freshly-computed
# missing_vals from extract_survey_metadata(). Called on every re-run when
# meta_json already exists, so new missing_chr entries take effect in the JSON.
# Returns the updated json_vars list (unchanged object if no differences found).
.update_null_coded_in_meta_json <- function(json_vars, metadata) {
  for (vname in names(json_vars)) {
    row <- metadata[metadata$var_name == vname, ]
    if (nrow(row) == 0) next
    miss_vals <- as.character(row$missing_vals[[1]])
    levels    <- json_vars[[vname]]$levels
    if (is.null(levels) || length(levels) == 0) next

    changed <- FALSE
    for (val_code in names(levels)) {
      should_be_null <- val_code %in% miss_vals
      currently_null <- isTRUE(levels[[val_code]]$null_coded)
      if (should_be_null && !currently_null) {
        json_vars[[vname]]$levels[[val_code]]$null_coded <- TRUE
        changed <- TRUE
      } else if (!should_be_null && currently_null) {
        json_vars[[vname]]$levels[[val_code]]$null_coded <- NULL
        changed <- TRUE
      }
    }
  }
  json_vars
}

# ---------------------------------------------------------------------------
# Apply a unified survey_meta.json variables section to a metadata table.
# Called internally by extract_survey_metadata() when meta_json is supplied.
# json_vars: the $variables list read from .read_meta_json() (already in memory).
# Updates: new_labels (from levels[*].new_label), level_counts/freqs (from n/pct),
#          new_name, detected_role, desc — all matched by variable name + value code.
metadata_apply_meta_json <- function(metadata, json_vars) {
  if (is.null(json_vars) || length(json_vars) == 0) return(metadata)

  update_rows <- purrr::imap(json_vars, function(entry, vname) {
    row <- metadata[metadata$var_name == vname, ]
    if (nrow(row) == 0) return(NULL)

    out <- tibble::tibble(var_name = vname)

    # new_name
    nn <- entry[["new_name"]]
    if (!is.null(nn) && nzchar(nn) && nn != vname)
      out$new_name <- nn

    # new_labels + level stats from levels block (matched by value code)
    levels_obj <- entry[["levels"]]
    if (!is.null(levels_obj) && length(levels_obj) > 0) {
      meta_vals <- as.character(row$values[[1]])

      has_new <- any(purrr::map_lgl(levels_obj, ~ !is.null(.x$new_label)))
      if (has_new) {
        nls <- row$new_labels[[1]]
        for (i in seq_along(meta_vals)) {
          lev <- levels_obj[[meta_vals[[i]]]]
          if (!is.null(lev) && !is.null(lev$new_label))
            nls[[i]] <- as.character(lev$new_label)
        }
        out$new_labels <- list(nls)
      }

      has_stats <- any(purrr::map_lgl(levels_obj, ~ !is.null(.x$n)))
      if (has_stats) {
        counts <- purrr::map_int(meta_vals, function(v) {
          lev <- levels_obj[[v]]
          if (!is.null(lev) && !is.null(lev$n)) as.integer(lev$n) else NA_integer_
        })
        freqs <- purrr::map_dbl(meta_vals, function(v) {
          lev <- levels_obj[[v]]
          if (!is.null(lev) && !is.null(lev$pct)) as.double(lev$pct) else NA_real_
        })
        out$level_counts <- list(counts)
        out$level_freqs  <- list(freqs)
      }
    }

    if (ncol(out) == 1L) return(NULL)  # only var_name, nothing to update
    out
  }) |> purrr::compact() |> dplyr::bind_rows()

  if (nrow(update_rows) == 0) return(metadata)

  # Ensure list columns exist before rows_update
  for (col in c("level_counts", "level_freqs")) {
    if (col %in% names(update_rows) && !col %in% names(metadata)) {
      metadata[[col]] <- vector("list", nrow(metadata))
      metadata[[col]][] <- list(if (col == "level_counts") integer(0) else numeric(0))
    }
  }

  n_updated <- nrow(update_rows)
  has_nl  <- "new_labels"   %in% names(update_rows)
  has_nn  <- "new_name"     %in% names(update_rows)
  has_st  <- "level_counts" %in% names(update_rows)
  message("metadata_apply_meta_json: ", n_updated, " variable(s) updated",
          if (has_nn) paste0(" (new_name: ",     sum(!is.na(update_rows$new_name)),    ")") else "",
          if (has_nl) paste0(" (new_labels: ",   sum(!is.null(update_rows$new_labels)),")")  else "",
          if (has_st) paste0(" (stats: ",        n_updated,                            ")")  else "")

  dplyr::rows_update(metadata, update_rows, by = "var_name", unmatched = "ignore")
}


# ---------------------------------------------------------------------------
# Write an initial survey_meta.json from a freshly-extracted metadata table.
# Called by extract_survey_metadata() when meta_json does not exist yet.
# datapath: optional path to the source dataset (written to config.dataset).
.write_initial_meta_json <- function(metadata, path,
                                     missing_num = NULL, missing_chr = NULL,
                                     yes_labels = NULL, no_labels = NULL,
                                     datapath = NULL) {
  # Config section
  cfg <- list()
  if (!is.null(datapath) && nzchar(datapath))
    cfg$dataset <- basename(datapath)
  if (!is.null(missing_num) && length(missing_num) > 0)
    cfg$missing_num <- as.list(missing_num)
  if (!is.null(missing_chr) && length(missing_chr) > 0)
    cfg$missing_chr <- as.list(missing_chr)
  if (!is.null(yes_labels) && length(yes_labels) > 0)
    cfg$yes_labels <- as.list(yes_labels)
  if (!is.null(no_labels) && length(no_labels) > 0)
    cfg$no_labels <- as.list(no_labels)

  # Variables section: one entry per variable, levels with original labels
  vars_list <- purrr::imap(
    purrr::set_names(seq_len(nrow(metadata)), metadata$var_name),
    function(i, vname) {
      row    <- metadata[i, ]
      role   <- row$detected_role
      desc   <- row$desc
      vals   <- row$values[[1]]
      labs   <- row$labels[[1]]
      mnulls <- row$missing_vals[[1]]

      # Build levels: original labels, null_coded for missing
      levels_list <- if (length(vals) > 0) {
        purrr::set_names(
          purrr::map2(as.character(vals), labs, function(v, l) {
            is_miss <- v %in% as.character(mnulls)
            entry   <- list(label = l)
            if (is_miss) entry$null_coded <- TRUE
            entry
          }),
          as.character(vals)
        )
      } else list()

      list(
        var_label = row$var_label,
        role      = role,
        desc      = desc,
        new_name  = vname,
        levels    = levels_list
      )
    }
  )

  out_dir <- dirname(path)
  if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  .write_meta_json(list(config = cfg, variables = vars_list), path)
}



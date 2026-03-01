# ============================================================
# Survey Formatting Pipeline — data_formatting_pipeline.R  v3
# ============================================================
# Source this file in any _mf.R script:
#   source("data_formatting_pipeline.R")
#
# METADATA SCHEMA — extract_survey_metadata() produces a tibble with:
#   var_name, var_label, r_class, n_distinct, detected_role, desc,
#   values, labels, missing_vals, new_labels, new_name, doc_note
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
#   3. [optional] ai_classify_roles(meta)
#      # Prints detected_roles / desc_overrides vectors — edit, then re-run step 2
#   4. [optional] metadata_apply_codebook(meta, codebook_df, ...)
#   5. meta <- metadata_fix_binary(meta)
#   6. export_metadata_excel(meta, "meta_review.xlsx")
#      # Open Excel, check orange rows (factor_binary/nominal/integer), iterate
#   6b.[optional] meta <- metadata_add_level_stats(meta, df)
#      # Adds level_counts/level_freqs for ordinal merging in ai_suggest_labels()
#   7. [optional] meta <- ai_suggest_labels(meta, df)
#                 meta <- ai_suggest_varnames(meta)
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
# 2. extract_survey_metadata()
# ============================================================

#' Extract variable and value metadata from a labelled tibble
#'
#' Produces a "varmod" tibble. Iterate: run, review console output, adjust
#' arguments (missing_num, missing_chr, detected_roles, desc_overrides), run again.
#' When satisfied, proceed to metadata_fix_binary() and export_metadata_excel().
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
#' @param detected_roles  Named character vector overriding role for specific
#'                        variables. Names = var_name, values = role string.
#'                        Paste output of ai_classify_roles() here after editing.
#' @param desc_overrides  Named logical vector overriding desc for specific vars.
#'                        For factor_binary: TRUE = positive level is first.
#'                        For factor_ordinal: TRUE = descending (high→low).
#'                        Paste output of ai_classify_roles() here after editing.
#' @param labels_json     Optional path to a JSON file produced by
#'                        ai_suggest_labels().  When supplied, new_labels and
#'                        level_counts/level_freqs stored in the JSON are merged
#'                        back into the metadata table via
#'                        metadata_apply_labels_json().  Works for both full API
#'                        results and dry_run stats-only files (which restore
#'                        counts/freqs but leave new_labels unchanged).
#' @param varnames_json   Optional path to a JSON file produced by
#'                        ai_suggest_varnames().  When supplied, new_name (and
#'                        optionally new_labels/level_counts/level_freqs) are
#'                        merged back via metadata_apply_varnames_json().
#'
#' @return A tibble with columns:
#'   var_name, var_label, r_class, n_distinct, detected_role, desc,
#'   values, labels, missing_vals, new_labels, new_name, doc_note
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
                        "Ne sait pas", "Refus"), # "8", "9",
    yes_labels      = NULL,
    no_labels       = NULL,
    max_levels_cat  = 20,
    detected_roles  = NULL,
    desc_overrides  = NULL,
    labels_json     = NULL,
    varnames_json   = NULL
) {
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

    # Override role from user-supplied vector
    if (!is.null(detected_roles) && vname %in% names(detected_roles)) {
      detected_role <- detected_roles[[vname]]
    }
    # Override desc from user-supplied vector
    desc_val <- if (!is.null(desc_overrides) && vname %in% names(desc_overrides)) {
      desc_overrides[[vname]]
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
      new_name      = vname,
      doc_note      = var_lbl
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
    message("    Run ai_classify_roles(meta) to get suggested detected_roles/desc_overrides vectors.")
  }

  if (!is.null(labels_json))   meta <- metadata_apply_labels_json(meta, labels_json)
  if (!is.null(varnames_json)) meta <- metadata_apply_varnames_json(meta, varnames_json)
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
metadata_add_level_stats <- function(metadata, df) {
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

  # Cast relevant columns to character once, stack into a long data.table,
  # then tabulate with a single groupby — O(n_obs * n_factor_vars) but done
  # in one C-level pass by data.table.
  var_names <- fac_meta$var_name
  dt_long   <- data.table::rbindlist(
    lapply(var_names, function(vn) {
      data.table::data.table(
        var_name = vn,
        val      = as.character(df[[vn]])
      )
    })
  )

  # Count NA separately then drop — we only count coded values
  counts_dt <- dt_long[!is.na(val),
                       .(n = .N),
                       by = .(var_name, val)]

  # Build named-vector lookup: var_name -> (value -> count)
  counts_by_var <- split(counts_dt, by = "var_name", keep.by = FALSE)
  lookup <- lapply(counts_by_var, function(d) {
    v <- d$n
    names(v) <- d$val
    v
  })

  # Compute level_counts and level_freqs per factor row
  compute_stats <- function(vname, vals, nls) {
    tab  <- lookup[[vname]]
    cnts <- purrr::map_int(as.character(vals), function(v) {
      if (!is.null(tab) && v %in% names(tab)) tab[[v]] else 0L
    })
    non_null_mask <- nls != "NULL"
    total_valid   <- sum(cnts[non_null_mask])
    freqs         <- rep(NA_real_, length(cnts))
    if (total_valid > 0)
      freqs[non_null_mask] <- round(cnts[non_null_mask] / total_valid * 100, 0)
    list(counts = cnts, freqs = freqs)
  }

  stats_list <- purrr::pmap(
    list(fac_meta$var_name, fac_meta$values, fac_meta$new_labels),
    compute_stats
  )

  # Write back into a full-length result (non-factor rows get empty vectors)
  result_counts <- vector("list", nrow(metadata))
  result_freqs  <- vector("list", nrow(metadata))
  result_counts[] <- list(integer(0))
  result_freqs[]  <- list(numeric(0))

  fac_idx <- match(fac_meta$var_name, metadata$var_name)
  for (i in seq_along(fac_idx)) {
    result_counts[[fac_idx[i]]] <- stats_list[[i]]$counts
    result_freqs[[fac_idx[i]]]  <- stats_list[[i]]$freqs
  }

  metadata$level_counts <- result_counts
  metadata$level_freqs  <- result_freqs
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
        var_label = dplyr::if_else(!is.na(new_var_label), new_var_label, var_label),
        doc_note  = dplyr::if_else(!is.na(new_var_label), new_var_label, doc_note)
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
#'   - Warns and skips. Set desc_overrides in extract_survey_metadata() first.
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
            "\nSet desc_overrides in extract_survey_metadata() first.")
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
#'                        Default hides new_labels, new_name, doc_note (cluttered
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
    hide_cols       = c("labels", "new_name", "doc_note"), # c("new_labels", "new_name", "doc_note"),
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
      new_name, doc_note
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
                  "n_distinct", "missing_vals", "labels", "new_labels", "new_name", "doc_note")
  all_widths <- c(20,          40,           12,        22,              8,
                  10,           30,            45,        45,             20,           40)
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
      lbl        = dplyr::if_else(doc_note != "", doc_note, var_label)
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
    '# STEP 4: Export ----\n',
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
#' @param role_examples    Named list of character vectors: example label sets for
#'                         each role, to guide Haiku. Each element is a small
#'                         vector of example labels for that role (not the actual
#'                         data labels — just illustrative examples the AI can
#'                         use as reference). Names must be role codes: F,O,B,S,C.
#'                         E.g. list(O = c("Très satisfait","Satisfait","Peu satisfait"),
#'                                   S = c("Gauche","--2--","--3--","Droite"),
#'                                   C = c("1 enfant","2 enfants","3 enfants ou plus")).
#' @param api_key          ANTHROPIC_API_KEY env var by default.
#' @param model            Default: Haiku 4.5.
#' @param max_labels_sent  Max non-missing labels per unique label set sent to AI.
#'                         Default 8. Increase for variables with many levels.
#' @param batch_threshold  Above this many unique label sets, use batch API.
#'                         Default 60.
#' @param dry_run          If TRUE, print the system prompt and user prompt that
#'                         would be sent, then return invisibly without making
#'                         any API call. Use to review and validate the prompt
#'                         before spending tokens. Default FALSE.
#'
#' @return Invisibly returns list(detected_roles, desc_overrides, extra_missing).
#'         Primary output: copy-pasteable vectors printed to console.
#'         In dry_run mode: invisibly returns list(system_prompt, user_prompt,
#'         n_vars, n_unique_sets).
ai_classify_roles <- function(
    metadata,
    role_examples    = list(),
    api_key          = Sys.getenv("ANTHROPIC_API_KEY"),
    model            = "claude-haiku-4-5",
    max_labels_sent  = 8L,
    batch_threshold  = 60L,
    dry_run          = FALSE
) {
  # --- Filter to ambiguous variables only ---
  target <- metadata |>
    dplyr::filter(
      detected_role %in% c("factor_nominal", "integer") |
        (detected_role == "factor_binary" & is.na(desc))
    )

  if (nrow(target) == 0) {
    message("ai_classify_roles: No ambiguous variables to classify.",
            " (factor_binary with desc resolved, double, identifier already clear.)")
    return(invisible(list(detected_roles = character(0),
                          desc_overrides = logical(0),
                          extra_missing  = character(0))))
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
      cur_code <- switch(detected_role,
        factor_binary  = "B", factor_nominal = "F", integer = "I", "?")
      sprintf("SET:%s|%s|cur:%s|nd:%d|[%s%s]",
              var_name, substr(.normalize_text(var_label), 1, 55),
              cur_code, n_distinct, lbl_str, suffix)
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
    C = c("1 / 2 / 3 / 4 / 5 / 7 / 10 / 12 / 17 / 20 / 21" ,
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
    "## desc FIELD (add after a TAB, for O and B only)\n",
    "T = descending / positive-first\n",
    "  O: labels go from highest to lowest (Très satisfait … Pas du tout)\n",
    "  B: positive level is listed FIRST in the label set shown\n",
    "F = ascending / positive-second\n",
    "  O: labels go from lowest to highest (Pas du tout … Très satisfait)\n",
    "  B: positive level is listed SECOND\n",
    "? = cannot determine from the labels shown\n\n",
    "## INPUT FORMAT\n",
    "SET:id|variable label|cur:CURRENT_CODE|nd:N_DISTINCT|[\"lv1\", \"lv2\", ...]\n\n",
    "## OUTPUT FORMAT — one line per SET, no blank lines, no explanations\n",
    "SET:id TAB CODE [TAB desc] [TAB miss:\"label\"]\n",
    "- Omit desc entirely for F, S, C, Q, X, ?\n",
    "- miss: flag only if a shown label is clearly a missing-value code\n",
    "  (NSP, Refus, NR, non-réponse…) that was not already filtered out"
  )

  user_prompt <- paste0(
    "Classify these ", nrow(unique_sets), " label set(s):\n\n",
    paste(prompt_lines, collapse = "\n")
  )

  # Max tokens: ~12 tokens per output line is ample (id + code + desc + miss)
  max_tok <- max(256L, nrow(unique_sets) * 20L)

  # --- Dry run: print prompts and exit without calling the API ---
  if (dry_run) {
    message(strrep("=", 60))
    message("DRY RUN — no API call made")
    message(strrep("=", 60))
    message("\nVariables to classify: ", nrow(target),
            "  |  Unique label sets: ", nrow(unique_sets),
            "  |  Route: ", if (nrow(unique_sets) <= batch_threshold) "synchronous" else "batch",
            "  |  max_tokens: ", max_tok)
    message("\n", strrep("-", 60))
    message("SYSTEM PROMPT:")
    message(strrep("-", 60))
    cat(system_prompt, "\n")
    message("\n", strrep("-", 60))
    message("USER PROMPT:")
    message(strrep("-", 60))
    cat(user_prompt, "\n")
    message(strrep("=", 60))
    return(invisible(list(
      system_prompt  = system_prompt,
      user_prompt    = user_prompt,
      n_vars         = nrow(target),
      n_unique_sets  = nrow(unique_sets)
    )))
  }

  # --- Route sync vs batch ---
  if (nrow(unique_sets) <= batch_threshold) {
    message("  Synchronous call (", nrow(unique_sets), " sets)...")
    resp     <- ai_call_claude(user_prompt, model = model, api_key = api_key,
                               system = system_prompt, max_tokens = max_tok)
    raw_text <- resp$content[[1]]$text
  } else {
    message("  Batch mode (", nrow(unique_sets), " sets > threshold ",
            batch_threshold, ")...")
    req_list <- list(list(custom_id = "classify_all", prompt = user_prompt))
    batch    <- ai_batch_submit(req_list, model = model, api_key = api_key,
                                max_tokens = max_tok, system = system_prompt)
    message("  Batch submitted: ", batch$id)
    message("  To recover if session ends: ai_batch_retrieve('", batch$id, "')")
    raw      <- ai_batch_retrieve(batch$id, api_key = api_key)
    raw_text <- raw[["classify_all"]]
  }

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

  # --- Print copy-pasteable output ---
  lbl_lookup <- purrr::set_names(
    purrr::map(target$labels, ~ paste(head(.x[nzchar(.x)], 3), collapse = '", "')),
    target$var_name
  )

  message("\n", strrep("=", 60))
  message("Review and copy-paste into your script,")
  message("then re-run extract_survey_metadata() with these vectors.")
  message(strrep("=", 60), "\n")

  if (length(detected_roles) > 0) {
    lines_out <- purrr::imap_chr(detected_roles, function(role, vname) {
      comment <- if (vname %in% names(lbl_lookup))
        paste0('  # "', lbl_lookup[[vname]], '"') else ""
      sprintf('  %-22s = "%s",%s', vname, role, comment)
    })
    cat("detected_roles <- c(\n", paste(lines_out, collapse = "\n"),
        "\n)\n\n", sep = "")
  } else {
    cat("detected_roles <- c()  # no role changes\n\n")
  }

  if (length(desc_overrides) > 0) {
    desc_lines <- purrr::imap_chr(desc_overrides, function(dv, vname) {
      role_hint <- if (vname %in% names(detected_roles))
        paste0("  # [", detected_roles[[vname]], "]") else ""
      sprintf('  %-22s = %s,%s', vname,
              if (is.na(dv)) "NA" else toupper(as.character(dv)), role_hint)
    })
    cat("desc_overrides <- c(\n", paste(desc_lines, collapse = "\n"),
        "\n)\n\n", sep = "")
  }

  if (length(extra_missing) > 0) {
    uniq_miss <- unique(unname(extra_missing))
    cat('# Possible missing labels flagged by AI — add to missing_chr:\n')
    cat("c(\n", paste(paste0('  "', uniq_miss, '"'), collapse = ",\n"),
        "\n)\n\n", sep = "")
    message("[!] Add the above to missing_chr in extract_survey_metadata() if correct.")
  }

  message("# Re-run with:\n# meta <- extract_survey_metadata(df,\n",
          "#   detected_roles = detected_roles,\n",
          "#   desc_overrides = desc_overrides)")
  message(strrep("=", 60))

  invisible(list(detected_roles = detected_roles,
                 desc_overrides = desc_overrides,
                 extra_missing  = extra_missing))
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

  # Extract numeric codes embedded in labels (e.g. "9-NSP" → 9, "99-Refus" → 99)
  # Pattern: optional leading digits before a hyphen or space
  num_codes <- purrr::map_dbl(valid_labels, function(lbl) {
    m <- regmatches(lbl, regexpr("^([0-9]+)(?=[-. ]|$)", lbl, perl = TRUE))
    if (length(m) == 1 && nzchar(m)) as.numeric(m) else NA_real_
  })
  missing_num_out <- sort(unique(num_codes[!is.na(num_codes)]))

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
    cat("# missing_num: no numeric codes extracted from these labels\n\n")
  }

  message("# Then re-run:")
  message("# meta <- extract_survey_metadata(df,")
  message("#   missing_chr = missing_chr,")
  if (length(missing_num_out) > 0) message("#   missing_num = missing_num)")
  message(strrep("=", 60))

  invisible(list(missing_chr = valid_labels, missing_num = missing_num_out))
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
#' ## chunk_size vs use_batch
#'   chunk_size controls how many variables go into one API request.  Smaller
#'   chunks (default 30) give Haiku more focus and produce better labels, at the
#'   cost of more sequential calls.  Larger values (up to 80) reduce call count.
#'   use_batch=TRUE submits everything as a single Anthropic Message Batch job
#'   (cheaper, but asynchronous — requires polling to retrieve results and can
#'   take minutes).  Keep use_batch=FALSE (default) for interactive use; set it
#'   to TRUE only for very large surveys (200+ factor variables).
#'
#' ## Output
#'   Results are written to a JSON file on disk (default path derived from
#'   the df path).  The metadata table is NOT modified here.  Load the JSON
#'   back via the labels_json argument of extract_survey_metadata().
#'
#' ## Dry run
#'   dry_run=TRUE prints every prompt that would be sent without making any
#'   API call.  Use this to validate prompts before spending tokens.
#'
#' Requires level_counts and level_freqs columns for ordinal merging.  Pass
#' df= to compute them automatically, or call metadata_add_level_stats() first.
#'
#' @param metadata     Varmod tibble.
#' @param df           Optional original survey tibble.  Used for two purposes:
#'                     (1) auto-compute level stats if missing; (2) derive the
#'                     default output directory and stem from the df's file path
#'                     attribute (set by import_survey()).
#' @param vars         Optional character vector of var_name to restrict to.
#' @param ordinal_desc Logical. If TRUE, send factor_ordinal labels to Haiku in
#'                     descending (high->low) order.  Default FALSE = stored order.
#' @param output_path   Where to write the JSON file.  Three accepted forms:
#'                     \itemize{
#'                       \item \code{NULL} (default): use the same directory as
#'                         the source file (from \code{attr(df, "path")}), or
#'                         \code{getwd()} if unavailable.
#'                       \item A **directory path** (no \code{.json} suffix):
#'                         the JSON is named \code{{stem}_labels.json} and
#'                         written into that directory.
#'                       \item A **full file path** ending in \code{.json}:
#'                         used as-is (overrides stem logic entirely).
#'                     }
#'                     The directory is created automatically if it does not
#'                     exist.  Existing files are silently overwritten.
#' @param chunk_size   Variables per API request.  Default 30.
#' @param use_batch    Logical. Use the Anthropic Message Batch API (cheaper,
#'                     async).  Default FALSE.
#' @param dry_run      If TRUE, print the prompt(s) that would be sent and
#'                     return invisibly without calling the API.  Default FALSE.
#' @param api_key      ANTHROPIC_API_KEY env var by default.
#' @param model        Default: Haiku 4.5.
#'
#' @return Invisibly returns the resolved output path.  In dry_run mode:
#'         invisibly returns a list of the prompt strings.
ai_suggest_labels <- function(
    metadata,
    df            = NULL,
    vars          = NULL,
    ordinal_desc  = FALSE,
    output_path    = NULL,
    chunk_size    = 30L,
    use_batch     = FALSE,
    dry_run       = FALSE,
    api_key       = Sys.getenv("ANTHROPIC_API_KEY"),
    model         = "claude-haiku-4-5"
) {
  # ---------- resolve output path -----------------------------------------
  # output_path can be:
  #   NULL              -> same dir + stem as df source file, or getwd()
  #   a data file path  -> treated as NULL: use its dir + stem
  #   a directory path  -> use it; stem from df source file
  #   a .json path      -> used verbatim (full override)
  df_path     <- if (!is.null(df)) attr(df, "path") else NULL
  df_has_path <- !is.null(df_path) && nzchar(df_path)

  # Regex covering common survey data formats plus generic non-json files
  .data_ext_re <- "\\.(dta|sas7bdat|sav|por|xpt|parquet|feather|arrow|csv|tsv|rds|rda|rdata|xlsx|xls|ods)$"

  if (!is.null(output_path) && grepl("\\.json$", output_path, ignore.case = TRUE)) {
    # Full .json path — use verbatim after normalisation.
    output_path <- enc2utf8(normalizePath(output_path, winslash = "/", mustWork = FALSE))
  } else {
    # Determine source for dir + stem:
    #   1. If output_path looks like a data file, use it as the source.
    #   2. If output_path is a plain directory, use it for dir and df for stem.
    #   3. If output_path is NULL, use df path (or getwd()).
    src_path <- if (!is.null(output_path) &&
                    grepl(.data_ext_re, output_path, ignore.case = TRUE)) {
      output_path          # treat data file path exactly like df path
    } else if (df_has_path) {
      df_path
    } else {
      NULL
    }

    out_dir <- if (!is.null(output_path) &&
                   nzchar(output_path) &&
                   !grepl(.data_ext_re, output_path, ignore.case = TRUE)) {
      # Plain directory explicitly supplied
      enc2utf8(normalizePath(output_path, winslash = "/", mustWork = FALSE))
    } else if (!is.null(src_path)) {
      enc2utf8(normalizePath(dirname(src_path), winslash = "/", mustWork = FALSE))
    } else {
      enc2utf8(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    }

    stem <- if (!is.null(src_path)) {
      tools::file_path_sans_ext(basename(src_path))
    } else {
      "survey"
    }

    output_path <- file.path(out_dir, paste0(stem, "_labels.json"))
  }

  # Ensure the output directory exists (handles accented / Unicode paths)
  out_dir_final <- dirname(output_path)
  if (!dir.exists(out_dir_final))
    dir.create(out_dir_final, recursive = TRUE, showWarnings = FALSE)

  # ---------- level stats ---------------------------------------------------
  needs_stats <- !all(c("level_counts", "level_freqs") %in% names(metadata))
  if (needs_stats && !is.null(df)) {
    message("ai_suggest_labels: computing level stats via metadata_add_level_stats().")
    metadata <- metadata_add_level_stats(metadata, df)
  } else if (needs_stats) {
    message("ai_suggest_labels: level_counts/level_freqs missing. ",
            "Ordinal merging disabled. Pass df= to enable.")
  }

  # ---------- filter target variables --------------------------------------
  target <- metadata |>
    dplyr::filter(detected_role %in% c("factor_binary", "factor_nominal",
                                        "factor_ordinal"))
  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) {
    message("ai_suggest_labels: No factor variables to process.")
    return(invisible(output_path))
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
    )

  # ---------- JSON builder for one variable ---------------------------------
  .build_var_json <- function(var_name, var_label, detected_role, desc,
                               labels, new_labels, send_order,
                               level_counts, level_freqs) {
    labels_ord     <- labels[send_order]
    new_labels_ord <- new_labels[send_order]
    counts_ord     <- if (length(level_counts) > 0) level_counts[send_order] else integer(0)
    freqs_ord      <- if (length(level_freqs)  > 0) level_freqs[send_order]  else numeric(0)

    keep        <- new_labels_ord != "NULL"
    labels_send <- labels_ord[keep]
    counts_send <- counts_ord[keep]
    freqs_send  <- freqs_ord[keep]

    if (length(labels_send) == 0) return(NULL)

    type_str <- switch(detected_role,
      factor_binary  = "binary",
      factor_ordinal = "ordinal",
      factor_nominal = "nominal",
      "nominal"
    )

    esc       <- function(x) gsub('"', '\\"', x, fixed = TRUE)
    labs_json <- paste0('["', paste(esc(labels_send), collapse = '", "'), '"]')
    obj       <- paste0('{"var":"', esc(var_name), '","type":"', type_str,
                        '","desc":"', esc(substr(var_label, 1, 120)), '",',
                        '"labels":', labs_json)

    if (detected_role == "factor_ordinal" &&
        length(counts_send) == length(labels_send) &&
        !any(is.na(counts_send))) {
      obj <- paste0(obj,
                    ',"counts":[', paste(counts_send, collapse = ","), ']',
                    ',"freqs":[',  paste(freqs_send,  collapse = ","), ']')
    }
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
      "Reponds UNIQUEMENT avec un objet JSON : ",
      '{"VARNAME1": ["label A", "label B"], "VARNAME2": ["label X"]}\n',
      "Aucun commentaire ni markdown."
    )
  }

  # ---------- user message builder for a chunk (data only) ------------------
  build_prompt <- function(chunk_df) {
    json_objects <- purrr::pmap(
      dplyr::select(chunk_df, var_name, var_label, detected_role, desc,
                    labels, new_labels, .send_order,
                    dplyr::any_of(c("level_counts", "level_freqs"))),
      function(var_name, var_label, detected_role, desc,
               labels, new_labels, .send_order,
               level_counts = integer(0), level_freqs = numeric(0)) {
        .build_var_json(var_name, var_label, detected_role, desc,
                        labels, new_labels, .send_order,
                        level_counts, level_freqs)
      }
    ) |> purrr::compact()

    if (length(json_objects) == 0) return(NULL)

    paste0("[\n", paste(json_objects, collapse = ",\n"), "\n]")
  }

  chunks  <- split(target, ceiling(seq_len(nrow(target)) / chunk_size))
  prompts <- purrr::map(chunks, build_prompt) |> purrr::compact()

  # ---------- dry run -------------------------------------------------------
  if (dry_run) {
    # Replace "_labels.json" -> "_labels_dry_run.json", or fall back generically.
    dry_path <- if (grepl("_labels\\.json$", output_path)) {
      sub("_labels\\.json$", "_labels_dry_run.json", output_path)
    } else {
      sub("\\.json$", "_dry_run.json", output_path)
    }

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

    # Write stats-only JSON (no AI labels) so counts/freqs survive the session.
    stats_map <- .build_stats_only_map(target)
    if (length(stats_map) > 0) {
      .write_labels_json(stats_map, dry_path)
      message("ai_suggest_labels dry_run: ", length(stats_map),
              " variable(s) written to: ", dry_path)
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
    return(invisible(output_path))
  }

  # Enrich parsed_map with level_counts/level_freqs for ALL factor types
  # (binary and nominal were not sent to Haiku but stats are still useful).
  # Stored in original metadata order (parallel to labels / new_labels).
  parsed_map <- .enrich_labels_map_with_stats(parsed_map, target)

  .write_labels_json(parsed_map, output_path)
  message("ai_suggest_labels: ", length(parsed_map), " variable(s) written to: ", output_path)
  message("Load with extract_survey_metadata(..., labels_json = \"", output_path, "\")")

  invisible(output_path)
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
  # Step 1: collect raw AI outputs into a flat var_name -> char-vector map
  raw_map <- list()
  for (txt in results_text) {
    if (is.null(txt) || !nzchar(txt)) next
    tryCatch({
      json_str <- stringr::str_extract(
        txt,
        "(?s)\\{(?:[^{}]|\\[[^\\[\\]]*\\])*\\}"
      )
      if (is.na(json_str)) stop("No JSON object found")
      parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      for (vname in names(parsed)) {
        val <- parsed[[vname]]
        if (is.character(val) ||
            (is.list(val) && all(purrr::map_lgl(val, is.character)))) {
          raw_map[[vname]] <- unlist(val)
        }
      }
    }, error = function(e) {
      warning("ai_suggest_labels: parse error: ", conditionMessage(e))
    })
  }

  if (length(raw_map) == 0) return(list())

  # Step 2: un-permute each variable back to original metadata order,
  # validate length, re-insert NULL placeholders.
  out <- purrr::imap(raw_map, function(ai_labels, vname) {
    row <- target[target$var_name == vname, ]
    if (nrow(row) == 0) return(NULL)

    orig_new_labels <- row$new_labels[[1]]
    send_order      <- row$.send_order[[1]]

    orig_reordered <- orig_new_labels[send_order]
    keep_mask      <- orig_reordered != "NULL"
    n_keep         <- sum(keep_mask)

    if (length(ai_labels) != n_keep) {
      warning("ai_suggest_labels: length mismatch for ", vname,
              " (expected ", n_keep, ", got ", length(ai_labels), "). Skipped.")
      return(NULL)
    }

    result_in_send_order            <- orig_reordered
    result_in_send_order[keep_mask] <- ai_labels

    inv_order <- order(send_order)
    as.list(result_in_send_order[inv_order])  # list so JSON keeps arrays
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
# Apply a saved ai_suggest_labels() JSON file to a metadata table.
# Called internally by extract_survey_metadata() when labels_json != NULL.
# Also exported for direct use.
#
# @param metadata    Varmod tibble.
# @param labels_json Path to a JSON file produced by ai_suggest_labels().
# @return metadata with new_labels updated.
metadata_apply_labels_json <- function(metadata, labels_json) {
  if (!file.exists(labels_json))
    stop("labels_json file not found: ", labels_json)

  saved <- jsonlite::read_json(labels_json, simplifyVector = FALSE)

  # Build one update-row per variable in the JSON.
  # Each row may carry: new_labels (if present), level_counts, level_freqs.
  # Matching is by value code — never by position — so ordering is irrelevant.
  update_rows <- purrr::imap(saved, function(entry, vname) {

    row <- metadata[metadata$var_name == vname, ]
    if (nrow(row) == 0) {
      warning("metadata_apply_labels_json: var '", vname, "' not in metadata. Skipped.")
      return(NULL)
    }

    # ---- detect format -------------------------------------------------------
    if (is.list(entry) && !is.null(entry$levels)) {
      # Rich format: { role, levels: { val_code: { label, new_label?, n?, pct? } } }
      meta_vals  <- as.character(row$values[[1]])
      levels_obj <- entry$levels

      # -- new_labels (omit entirely if none present, i.e. dry_run file) -------
      has_new <- any(purrr::map_lgl(levels_obj, ~ !is.null(.x$new_label)))
      new_labs <- if (has_new) {
        nls <- row$new_labels[[1]]
        for (i in seq_along(meta_vals)) {
          lev <- levels_obj[[meta_vals[[i]]]]
          if (!is.null(lev) && !is.null(lev$new_label))
            nls[[i]] <- as.character(lev$new_label)
        }
        nls
      } else NULL

      # -- level_counts / level_freqs (matched by value code) ------------------
      has_stats <- any(purrr::map_lgl(levels_obj, ~ !is.null(.x$n)))
      counts <- if (has_stats) {
        purrr::map_int(meta_vals, function(v) {
          lev <- levels_obj[[v]]
          if (!is.null(lev) && !is.null(lev$n)) as.integer(lev$n) else NA_integer_
        })
      } else NULL
      freqs <- if (has_stats) {
        purrr::map_dbl(meta_vals, function(v) {
          lev <- levels_obj[[v]]
          if (!is.null(lev) && !is.null(lev$pct)) as.double(lev$pct) else NA_real_
        })
      } else NULL

      if (is.null(new_labs) && is.null(counts)) return(NULL)

      out <- tibble::tibble(var_name = vname)
      if (!is.null(new_labs)) out$new_labels   <- list(new_labs)
      if (!is.null(counts))   out$level_counts  <- list(counts)
      if (!is.null(freqs))    out$level_freqs   <- list(freqs)
      out

    } else {
      # Legacy flat format: entry is a plain list of label strings
      new_labs <- unlist(entry)
      if (length(new_labs) == 0) return(NULL)
      orig <- row$new_labels[[1]]
      if (length(new_labs) != length(orig)) {
        warning("metadata_apply_labels_json: length mismatch for '", vname,
                "' (", length(new_labs), " vs ", length(orig), "). Skipped.")
        return(NULL)
      }
      tibble::tibble(var_name = vname, new_labels = list(new_labs))
    }
  }) |> purrr::compact() |> dplyr::bind_rows()

  if (nrow(update_rows) == 0) {
    message("metadata_apply_labels_json: stats-only (dry_run) JSON — no labels to apply.")
    return(metadata)
  }

  # Ensure metadata has level_counts / level_freqs columns before rows_update,
  # initialising missing ones with empty vectors for non-factor rows.
  if ("level_counts" %in% names(update_rows) &&
      !"level_counts" %in% names(metadata)) {
    metadata$level_counts <- vector("list", nrow(metadata))
    metadata$level_counts[] <- list(integer(0))
  }
  if ("level_freqs" %in% names(update_rows) &&
      !"level_freqs" %in% names(metadata)) {
    metadata$level_freqs <- vector("list", nrow(metadata))
    metadata$level_freqs[] <- list(numeric(0))
  }

  n_labels <- if ("new_labels"   %in% names(update_rows)) nrow(update_rows) else 0L
  n_stats  <- if ("level_counts" %in% names(update_rows)) nrow(update_rows) else 0L
  message("metadata_apply_labels_json: ", nrow(update_rows), " variable(s) — ",
          n_labels, " new_labels, ", n_stats, " with counts/freqs.")

  dplyr::rows_update(metadata, update_rows, by = "var_name", unmatched = "ignore")
}




# ============================================================
# 11. ai_suggest_varnames()
# ============================================================

#' Use Haiku to suggest short UPPER_SNAKE_CASE R variable names
#'
#' Calls the Anthropic API (synchronous or batch) to propose new names for all
#' variables in `metadata`. Results are written to a JSON file; apply with
#' `metadata_apply_varnames_json()`.
#'
#' @param metadata       Varmod tibble from `extract_survey_metadata()`.
#' @param vars           Optional character vector of `var_name` to restrict.
#' @param output_path    Path for the output `.json`. NULL = same dir/stem as
#'                       `attr(metadata, "path")` or `getwd()`.
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
#' @return Invisibly: the `output_path` where results were written (or the list
#'   of prompts in dry_run mode). Apply with `metadata_apply_varnames_json()`.
ai_suggest_varnames <- function(
    metadata,
    vars           = NULL,
    output_path    = NULL,
    chunk_size     = 300L,
    max_new_labels = 4L,
    max_tokens     = 20000L,
    use_batch      = FALSE,
    dry_run        = FALSE,
    api_key        = Sys.getenv("ANTHROPIC_API_KEY"),
    model          = "claude-haiku-4-5"
) {
  # ---------- output path resolution (same logic as ai_suggest_labels) ------
  df_path     <- attr(metadata, "path")
  df_has_path <- !is.null(df_path) && nzchar(df_path)

  .data_ext_re <- "\\.(dta|sas7bdat|sav|por|xpt|parquet|feather|arrow|csv|tsv|rds|rda|rdata|xlsx|xls|ods)$"

  if (!is.null(output_path) && grepl("\\.json$", output_path, ignore.case = TRUE)) {
    output_path <- enc2utf8(normalizePath(output_path, winslash = "/", mustWork = FALSE))
  } else {
    src_path <- if (!is.null(output_path) &&
                    grepl(.data_ext_re, output_path, ignore.case = TRUE)) {
      output_path
    } else if (df_has_path) {
      df_path
    } else {
      NULL
    }

    out_dir <- if (!is.null(output_path) &&
                   nzchar(output_path) &&
                   !grepl(.data_ext_re, output_path, ignore.case = TRUE)) {
      enc2utf8(normalizePath(output_path, winslash = "/", mustWork = FALSE))
    } else if (!is.null(src_path)) {
      enc2utf8(normalizePath(dirname(src_path), winslash = "/", mustWork = FALSE))
    } else {
      enc2utf8(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
    }

    stem <- if (!is.null(src_path)) {
      tools::file_path_sans_ext(basename(src_path))
    } else {
      "survey"
    }

    output_path <- file.path(out_dir, paste0(stem, "_varnames.json"))
  }

  out_dir_final <- dirname(output_path)
  if (!dir.exists(out_dir_final))
    dir.create(out_dir_final, recursive = TRUE, showWarnings = FALSE)

  # ---------- filter target variables ---------------------------------------
  target <- metadata
  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) {
    message("ai_suggest_varnames: No variables to process.")
    return(invisible(output_path))
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
    dry_path <- if (grepl("_varnames\\.json$", output_path)) {
      sub("_varnames\\.json$", "_varnames_dry_run.json", output_path)
    } else {
      sub("\\.json$", "_dry_run.json", output_path)
    }

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

    # Write stub JSON (empty entries, no AI names)
    stub <- purrr::set_names(
      purrr::map(target$var_name, ~ list()),
      target$var_name
    )
    .write_varnames_json(stub, dry_path)
    message("ai_suggest_varnames dry_run: stub written to: ", dry_path)

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
    return(invisible(output_path))
  }

  # Build structured map enriched with metadata (new_labels, counts, freqs)
  out_map <- .build_varnames_map(target, names_map)
  .write_varnames_json(out_map, output_path)
  message("ai_suggest_varnames: ", length(out_map), " variable(s) written to: ",
          output_path)
  message("Apply with metadata_apply_varnames_json(metadata, \"", output_path, "\")")

  invisible(output_path)
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


#' Apply a varnames JSON file to update metadata
#'
#' Reads the JSON produced by `ai_suggest_varnames()` and updates:
#' - `new_name` for all matched variables
#' - `new_labels`, `level_counts`, `level_freqs` when present in the JSON
#'
#' @param metadata  Varmod tibble.
#' @param json_path Path to the `_varnames.json` file.
#'
#' @return Updated metadata tibble.
metadata_apply_varnames_json <- function(metadata, json_path) {
  if (!file.exists(json_path))
    stop("metadata_apply_varnames_json: file not found: ", json_path)

  raw <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

  # --- new_name updates (always) ---
  update_name <- purrr::imap_dfr(raw, function(entry, vname) {
    nn <- entry[["new_name"]]
    if (!is.null(nn) && nzchar(nn))
      tibble::tibble(var_name = vname, new_name = nn)
    else
      NULL
  })

  if (nrow(update_name) == 0) {
    message("metadata_apply_varnames_json: no new_name entries found in JSON.")
    return(metadata)
  }

  update_name <- dplyr::filter(update_name, var_name %in% metadata$var_name)
  message("metadata_apply_varnames_json: updating new_name for ",
          nrow(update_name), " variable(s).")
  metadata <- dplyr::rows_update(metadata, update_name,
                                 by = "var_name", unmatched = "ignore")

  # --- optional: restore new_labels / level_counts / level_freqs ---
  for (col in c("new_labels", "level_counts", "level_freqs")) {
    entries_with_col <- purrr::keep(raw, ~ !is.null(.x[[col]]) && length(.x[[col]]) > 0)
    if (length(entries_with_col) == 0 || !col %in% names(metadata)) next

    for (vname in names(entries_with_col)) {
      if (!vname %in% metadata$var_name) next
      idx <- which(metadata$var_name == vname)
      metadata[[col]][[idx]] <- unlist(entries_with_col[[vname]][[col]])
    }
    message("metadata_apply_varnames_json: restored ", col,
            " for ", length(entries_with_col), " variable(s).")
  }

  metadata
}


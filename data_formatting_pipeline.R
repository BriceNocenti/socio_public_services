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
#   7. [optional] meta <- ai_suggest_labels(meta)
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
#   ai_suggest_labels()         — shorten French factor level labels
#   ai_suggest_varnames()       — propose short R variable names + doc strings
# ============================================================


# ============================================================
# 1. import_survey()
# ============================================================

#' Auto-detect file format and import a survey dataset
#'
#' @param path         Path to the data file.
#' @param format       Optional override: "sas", "dta", "parquet", "sav", "rds".
#' @param catalog_file Optional SAS catalog file (.sas7bcat).
#' @param encoding     Character encoding. NULL = haven default (usually correct).
#'                     Try "latin1" if accents are garbled in SAS files.
#'
#' @return A tibble with preserved labels. No transformation applied.
import_survey <- function(
    path,
    format       = NULL,
    catalog_file = NULL,
    encoding     = NULL
) {
  ext <- if (!is.null(format)) format else tolower(tools::file_ext(path))

  switch(
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
    desc_overrides  = NULL
) {
  default_yes <- c("oui", "choisi", "yes", "vrai", "true",
                   "présent", "actif", "sélectionné", "concerné",
                   "a le", "dispose", "perçoit")
  default_no  <- c("non", "non choisi", "no", "faux", "false",
                   "absent", "inactif", "non sélectionné",
                   "pas ", "n'a pas", "ne dispose", "ne perçoit")

  yes_kw <- if (!is.null(yes_labels)) yes_labels else default_yes
  no_kw  <- if (!is.null(no_labels))  no_labels  else default_no

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
      raw_labels <- names(val_labs)
    } else if (is.factor(col)) {
      raw_values <- levels(col)
      raw_labels <- levels(col)
    } else {
      sorted_vals <- sort(vals_present)
      raw_values  <- as.character(sorted_vals)
      raw_labels  <- as.character(sorted_vals)
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

    # --- For double columns: suppress spurious float labels ---
    if (detected_role == "double") {
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
# Returns TRUE (positive is first), FALSE (positive is second), or NA (no keyword match)
.find_binary_desc <- function(lbls_clean, yes_kw, no_kw) {
  if (length(lbls_clean) != 2) return(NA)

  lbl_lower    <- tolower(lbls_clean)
  # Remove numeric prefix (e.g. "1-") before keyword matching
  lbl_stripped <- stringr::str_remove(lbl_lower, "^[0-9]+-\\s*")
  # Lowercase keywords so matching is case-insensitive
  yes_kw_lc <- tolower(yes_kw)

  yes_match <- purrr::map_lgl(yes_kw_lc,
    ~ any(stringr::str_detect(lbl_stripped, stringr::fixed(.x))))
  has_yes <- any(yes_match)

  if (!has_yes) return(NA)

  # Which of the two labels contains a positive keyword?
  matched_kw <- yes_kw_lc[yes_match]
  yes_idx <- which(purrr::map_lgl(lbl_stripped,
    ~ any(stringr::str_detect(.x, stringr::fixed(matched_kw)))))

  if (length(yes_idx) == 1) return(yes_idx == 1L)  # TRUE if positive is first

  NA
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
      dplyr::filter(!is.na(new_var_label))

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
#'   - desc = FALSE → labels[[2]] is positive, labels[[1]] is negative
#'     (reorders new_labels so positive comes first)
#'   - Renames positive level to "1-<var_label>", negative to "2-Non"
#'   - Any other levels (e.g. missing candidates left in labels) → "NULL"
#'
#' For rows with desc == NA:
#'   - Warns and skips. Set desc_overrides in extract_survey_metadata() first.
#'
#' @param metadata       Varmod tibble.
#' @param use_var_label  If TRUE (default), positive level is renamed to
#'                       paste0("1-", var_label). If FALSE, keeps original label.
#'
#' @return Updated metadata tibble with new_labels modified for factor_binary rows.
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
    dplyr::mutate(new_labels = purrr::pmap(
      list(detected_role, var_label, new_labels, desc),
      function(role, lbl, nls, dsc) {
        if (role != "factor_binary" || is.na(dsc)) return(nls)

        # Determine which index is positive
        pos_idx <- if (isTRUE(dsc)) 1L else 2L
        neg_idx <- if (isTRUE(dsc)) 2L else 1L

        pos_label <- if (use_var_label && lbl != "") paste0("1-", lbl) else nls[[pos_idx]]
        neg_label <- "2-Non"

        # Reorder so positive is first, negative is second; any others → "NULL"
        result <- rep("NULL", length(nls))
        result[[pos_idx]] <- pos_label
        result[[neg_idx]] <- neg_label
        # If desc = FALSE (positive was second), also swap new_labels order for display
        if (!isTRUE(dsc)) {
          result <- c(result[[pos_idx]], result[[neg_idx]],
                      result[seq_along(result)[-c(pos_idx, neg_idx)]])
        }
        result
      }
    ))
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
#'
#' @return Invisibly returns path.
export_metadata_excel <- function(
    metadata,
    path            = "metadata_review.xlsx",
    highlight_roles = c("factor_nominal", "integer"),
    show_missing    = FALSE,
    hide_cols       = c("new_labels", "new_name", "doc_note")
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx is required. Install with: install.packages('openxlsx')")
  }

  # --- Flatten list columns to readable strings ---
  collapse5 <- function(lbls, vals, miss) {
    # Filter missing if show_missing = FALSE
    if (!show_missing && length(miss) > 0) {
      keep <- !(vals %in% miss)
      lbls <- lbls[keep]
    }
    if (length(lbls) == 0) return("")
    paste(head(lbls, 5), collapse = " / ")
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
    paste(head(nls, 5), collapse = " / ")
  }

  df_excel <- metadata |>
    dplyr::mutate(
      missing_vals_str  = purrr::pmap_chr(
        list(missing_vals, labels, values),
        ~ collapse_miss_labels(..1, ..2, ..3)
      ),
      labels_str        = purrr::pmap_chr(
        list(labels, values, missing_vals),
        ~ collapse5(..1, ..2, ..3)
      ),
      new_labels_str    = purrr::map_chr(new_labels, collapse_new_labels),
      desc_str          = dplyr::case_when(
        is.na(desc)       ~ "NA",
        isTRUE(desc)      ~ "TRUE",
        .default          = "FALSE"
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
#'
#' @return Parsed API response with $id = batch_id for ai_batch_retrieve().
ai_batch_submit <- function(
    requests,
    model      = "claude-haiku-4-5", # "claude-haiku-4-5-20251001"
    api_key    = Sys.getenv("ANTHROPIC_API_KEY"),
    max_tokens = 4096
) {
  if (api_key == "") stop("ANTHROPIC_API_KEY not set.")

  batch_requests <- purrr::map(requests, function(req) {
    list(custom_id = req$custom_id,
         params    = list(model = model, max_tokens = max_tokens,
                          messages = list(list(role = "user",
                                               content = req$prompt))))
  })

  httr2::request("https://api.anthropic.com/v1/messages/batches") |>
    httr2::req_headers("x-api-key" = api_key,
                       "anthropic-version" = "2023-06-01",
                       "content-type"      = "application/json") |>
    httr2::req_body_json(list(requests = batch_requests)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
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

  parsed <- purrr::map(raw_lines, jsonlite::fromJSON)
  purrr::set_names(
    purrr::map(parsed, function(r) {
      if (r$result$type == "succeeded") r$result$message$content[[1]]$text
      else list(error = r$result$error)
    }),
    purrr::map_chr(parsed, "custom_id")
  )
}


# ============================================================
# 9. ai_classify_roles()
# ============================================================

#' Classify ambiguous variables with Haiku, print copy-pasteable R vectors
#'
#' Sends variables with detected_role in "factor_nominal", "integer", or
#' "factor_binary" (where desc=NA) to Claude Haiku for finer classification.
#' Returns two named vectors printed to the console in copy-paste format:
#'   - detected_roles  (var_name = role)
#'   - desc_overrides  (var_name = TRUE/FALSE, for binary/ordinal only)
#'
#' Role codes sent to AI:
#'   F = factor_nominal (no change needed)
#'   O = factor_ordinal (ascending or descending order)
#'   B = factor_binary  (already known; sent only when desc=NA)
#'   S = integer_scale  (Likert, left/right scales)
#'   C = integer_count  (1 enfant, 2 enfants…)
#'   Q = double         (reclassify integer as continuous)
#'   ? = unclear
#'
#' After reviewing the output:
#'   1. Copy the two vectors into your _mf.R script
#'   2. Edit as needed
#'   3. Comment out the ai_classify_roles() line
#'   4. Re-run extract_survey_metadata() with the corrected vectors
#'
#' @param metadata         Varmod tibble from extract_survey_metadata().
#' @param api_key          ANTHROPIC_API_KEY env var by default.
#' @param model            Default: Haiku 4.5.
#' @param max_labels_sent  Max labels per variable sent to AI. Default 5.
#'                         Enough for role detection; keeps tokens minimal.
#' @param batch_threshold  Above this n_vars, use batch API. Default 60.
#'
#' @return Invisibly returns a list(detected_roles, desc_overrides,
#'         extra_missing). Primary output is the console print.
ai_classify_roles <- function(
    metadata,
    api_key          = Sys.getenv("ANTHROPIC_API_KEY"),
    model            = "claude-haiku-4-5", # claude-haiku-4-5-20251001
    max_labels_sent  = 5,
    batch_threshold  = 60
) {
  # Target: factor_nominal (all), integer (all), factor_binary with desc=NA
  target <- metadata |>
    dplyr::filter(
      detected_role %in% c("factor_nominal", "integer") |
        (detected_role == "factor_binary" & is.na(desc))
    )

  if (nrow(target) == 0) {
    message("ai_classify_roles: No ambiguous variables found. ",
            "All roles already determined.")
    return(invisible(list(detected_roles  = character(0),
                          desc_overrides  = logical(0),
                          extra_missing   = character(0))))
  }

  message("ai_classify_roles: ", nrow(target), " variable(s) to classify.")

  # Build compact prompt lines — one per variable
  prompt_lines <- purrr::pmap_chr(
    list(target$var_name, target$var_label, target$r_class,
         target$n_distinct, target$labels, target$missing_vals,
         target$detected_role),
    function(var_name, var_label, r_class, n_distinct, labels, missing_vals,
             detected_role) {
      non_miss_lbls <- labels[!(labels %in% missing_vals)]
      first_n <- head(non_miss_lbls, max_labels_sent)
      n_total <- length(non_miss_lbls)
      lbl_str <- paste0('"', first_n, '"', collapse = ",")
      suffix  <- if (n_total > max_labels_sent)
        paste0(",...+", n_total - max_labels_sent, "more") else ""
      # Hint current role so AI knows the starting point
      cur_code <- switch(detected_role,
        factor_binary  = "B",
        factor_nominal = "F",
        integer        = "I",
        "?"
      )
      sprintf('%s|"%s"|%s|cur:%s|nd:%d|[%s%s]',
              var_name,
              substr(var_label, 1, 60),
              r_class, cur_code, n_distinct,
              lbl_str, suffix)
    }
  )

  # System prompt — abbreviated codes; R wrapper converts to full role names
  system_prompt <- paste0(
    "You classify French social survey variables. For each line:\n",
    "  FORMAT: var_name|\"label\"|r_class|cur:CODE|nd:N|[\"lv1\",\"lv2\",...]\n",
    "Reply with ONE line per variable:\n",
    "  var_name TAB CODE [TAB T|F|?] [TAB miss:\"label\"]\n",
    "CODES: F=factor_nominal O=factor_ordinal B=factor_binary ",
    "S=integer_scale C=integer_count Q=double ?=unclear\n",
    "- B: add TAB then T (positive level is first shown), F (second), or ? if unclear\n",
    "- O: add TAB then T (descending: high\u2192low) or F (ascending: low\u2192high), or ?\n",
    "- S or C: no extra field needed\n",
    "- miss: add TAB miss:\"label\" if a label looks like missing (NSP/Refus/etc.) ",
    "but is not already marked as such\n",
    "No explanations. No extra text. One variable per line."
  )

  user_prompt <- paste0(
    "Classify these variables:\n\n",
    paste(prompt_lines, collapse = "\n")
  )

  # Route sync vs batch
  if (nrow(target) <= batch_threshold) {
    message("  Synchronous call...")
    resp     <- ai_call_claude(user_prompt, model = model, api_key = api_key,
                               system = system_prompt)
    raw_text <- resp$content[[1]]$text
  } else {
    message("  Batch mode (", nrow(target), " vars > threshold ",
            batch_threshold, ")...")
    req_list <- list(list(custom_id = "classify_all", prompt = user_prompt))
    batch    <- ai_batch_submit(req_list, model = model, api_key = api_key)
    raw      <- ai_batch_retrieve(batch$id, api_key = api_key)
    raw_text <- raw[["classify_all"]]
  }

  # Role code → full role name
  role_map <- c(
    F = "factor_nominal",
    O = "factor_ordinal",
    B = "factor_binary",
    S = "integer_scale",
    C = "integer_count",
    Q = "double",
    "?" = "factor_nominal"   # unclear → keep nominal, user decides
  )

  detected_roles  <- character(0)
  desc_overrides  <- logical(0)
  extra_missing   <- character(0)

  lines <- stringr::str_split(stringr::str_trim(raw_text), "\n")[[1]]
  lines <- lines[lines != ""]

  # Build lookup: var_name → first 3 labels (for inline comments in output)
  lbl_lookup <- purrr::set_names(
    purrr::map(target$labels, ~ paste(head(.x, 3), collapse = '","')),
    target$var_name
  )

  for (ln in lines) {
    parts <- stringr::str_split(ln, "\t")[[1]]
    if (length(parts) < 2) next

    vname <- stringr::str_trim(parts[[1]])
    code  <- stringr::str_trim(parts[[2]])

    if (!vname %in% target$var_name) next
    if (!code %in% names(role_map)) next

    detected_roles[[vname]] <- role_map[[code]]

    # desc field for B (binary) and O (ordinal): T/F/?
    if (code %in% c("B", "O") && length(parts) >= 3) {
      desc_raw <- stringr::str_trim(parts[[3]])
      if (!stringr::str_starts(desc_raw, "miss:") && desc_raw != "") {
        desc_val <- switch(desc_raw,
          T   = TRUE,
          F   = FALSE,
          "?" = NA
        )
        if (!is.null(desc_val)) {
          desc_overrides[[vname]] <- desc_val
        }
      }
    }

    # Extra missing label
    miss_part <- purrr::keep(parts[-(1:2)], ~ stringr::str_starts(.x, "miss:"))
    if (length(miss_part) > 0) {
      miss_lbl <- stringr::str_remove(miss_part[[1]], "^miss:")
      miss_lbl <- stringr::str_remove_all(miss_lbl, '"')
      extra_missing[[vname]] <- miss_lbl
    }
  }

  # Print copy-pasteable output
  message("\n", strrep("=", 60))
  message("Copy-paste the vectors below into your script,")
  message("edit as needed, then re-run extract_survey_metadata().")
  message(strrep("=", 60), "\n")

  # detected_roles vector
  if (length(detected_roles) > 0) {
    lines_out <- purrr::imap_chr(detected_roles, function(role, vname) {
      comment <- if (vname %in% names(lbl_lookup))
        paste0('  # "', lbl_lookup[[vname]], '"') else ""
      sprintf('  %-20s = "%s",%s', vname, role, comment)
    })
    cat("detected_roles <- c(\n",
        paste(lines_out, collapse = "\n"), "\n)\n\n", sep = "")
  } else {
    cat("detected_roles <- c()  # (no role changes)\n\n")
  }

  # desc_overrides vector (replaces positive_levels)
  if (length(desc_overrides) > 0) {
    desc_lines <- purrr::imap_chr(desc_overrides, function(dv, vname) {
      role <- detected_roles[[vname]]
      role_hint <- if (!is.null(role)) paste0("  # [", role, "]") else ""
      sprintf('  %-20s = %s,%s', vname, toupper(as.character(dv)), role_hint)
    })
    cat("desc_overrides <- c(\n",
        paste(desc_lines, collapse = "\n"), "\n)\n\n", sep = "")
  }

  if (length(extra_missing) > 0) {
    miss_lines <- purrr::imap_chr(extra_missing, function(lbl, vname) {
      sprintf('  "%s"', lbl)
    })
    cat("# Add to missing_chr:\n")
    cat("c(\n", paste(miss_lines, collapse = ",\n"), "\n)\n\n", sep = "")
    message("[!] extra_missing: add these to missing_chr in extract_survey_metadata()")
  }

  message("# Then re-run:")
  message("# meta <- extract_survey_metadata(df,")
  message("#   detected_roles = detected_roles,")
  message("#   desc_overrides = desc_overrides)")
  message(strrep("=", 60))

  invisible(list(detected_roles  = detected_roles,
                 desc_overrides  = desc_overrides,
                 extra_missing   = extra_missing))
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

  # Flatten, deduplicate, drop blanks
  unique_labels <- unique(unlist(all_labels, use.names = FALSE))
  unique_labels <- unique_labels[nzchar(unique_labels)]

  if (length(unique_labels) == 0) {
    message("ai_suggest_missing: No value labels found in metadata.")
    return(invisible(list(missing_chr = character(0), missing_num = numeric(0))))
  }

  message("ai_suggest_missing: ", length(unique_labels),
          " unique tail-labels collected from ", nrow(target), " variables.")

  # Build prompt — send labels WITHOUT numbers so Haiku cannot echo them back
  examples_block <- if (!is.null(examples) && length(examples) > 0) {
    paste0("\nFor reference, labels like these are typically missing in similar surveys:\n",
           paste(paste0("  ", examples), collapse = "\n"), "\n")
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

  # Parse response: one label per line, trim whitespace
  returned_labels <- stringr::str_trim(stringr::str_split(raw_text, "\n")[[1]])
  returned_labels <- returned_labels[nzchar(returned_labels)]

  # Validate: keep only labels that actually appear in unique_labels (exact match)
  # This prevents hallucinated values from entering the output
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

#' Use Haiku to suggest concise French factor level labels
#'
#' Sends factor variables to Claude and asks it to shorten labels to
#' ≤ 25 characters while preserving numeric prefix and meaning.
#' Ordinal variables (detected_role = "factor_ordinal"): labels are shortened but NOT reordered.
#'
#' Routes to synchronous (<= batch_threshold vars) or batch API (cheaper).
#' Output MUST be reviewed before applying — especially ordinal variables.
#'
#' @param metadata         Varmod tibble.
#' @param vars             Optional character vector of var_name to restrict to.
#' @param examples_text    Optional: paste of a previous _recode.R showing style.
#' @param api_key          ANTHROPIC_API_KEY env var by default.
#' @param model            Default: Haiku 4.5.
#' @param chunk_size       Variables per API request. Default 50.
#' @param batch_threshold  Above this n_vars, use batch mode. Default 60.
#'
#' @return metadata with new_labels updated. Review before apply_survey_formats()!
ai_suggest_labels <- function(
    metadata,
    vars            = NULL,
    examples_text   = NULL,
    api_key         = Sys.getenv("ANTHROPIC_API_KEY"),
    model           = "claude-haiku-4-5", # "claude-haiku-4-5-20251001"
    chunk_size      = 50,
    batch_threshold = 60
) {
  target <- metadata |>
    dplyr::filter(detected_role %in% c("factor_binary", "factor_nominal",
                                        "factor_ordinal"))
  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) {
    message("ai_suggest_labels: No factor variables to label.")
    return(metadata)
  }

  chunks     <- split(target, ceiling(seq_len(nrow(target)) / chunk_size))
  ex_section <- if (!is.null(examples_text))
    paste0("\n\nEXAMPLE STYLE:\n```r\n", examples_text, "\n```\n") else ""

  build_prompt <- function(chunk_df) {
    rows <- purrr::pmap_chr(chunk_df, function(var_name, var_label, labels,
                                                detected_role, ...) {
      ord_note <- if (detected_role == "factor_ordinal")
        " [ORDINAL: keep order, do not rearrange]" else ""
      labs_str <- paste0('"', labels, '"', collapse = ", ")
      sprintf('  list(var_name="%s", label="%s"%s, levels=c(%s))',
              var_name, substr(var_label, 1, 60), ord_note, labs_str)
    })
    paste0(
      'You format French social survey data.\n',
      'Shorten each factor level label to ≤25 chars. Keep numeric prefix (e.g. "1-"). ',
      'Do not change meaning. Keep "NULL" as-is. ',
      'For [ORDINAL] variables: only shorten text, never reorder.\n',
      ex_section,
      '\nDATA:\n', paste(rows, collapse = "\n"),
      '\n\nReply ONLY as an R tribble:\n',
      'tribble(\n  ~var_name, ~new_labels,\n',
      '  "DIPL", list(c("1-Aucun", "2-CEP"))\n)\n',
      'No explanation, no markdown.'
    )
  }

  if (nrow(target) <= batch_threshold) {
    message("ai_suggest_labels: synchronous (", nrow(target), " vars)")
    results_text <- purrr::imap(chunks, function(chunk, i) {
      message("  Chunk ", i, "/", length(chunks))
      resp <- ai_call_claude(build_prompt(chunk), model = model, api_key = api_key)
      resp$content[[1]]$text
    })
  } else {
    message("ai_suggest_labels: batch mode (", nrow(target), " vars)")
    requests <- purrr::imap(chunks, function(chunk, i)
      list(custom_id = paste0("labels_", i), prompt = build_prompt(chunk)))
    batch    <- ai_batch_submit(requests, model = model, api_key = api_key)
    message("Batch submitted. ID: ", batch$id)
    raw      <- ai_batch_retrieve(batch$id, api_key = api_key)
    results_text <- purrr::map(purrr::set_names(names(raw)), ~ raw[[.x]])
  }

  metadata <- .parse_and_merge_labels(metadata, results_text)
  message("Review metadata$new_labels before apply_survey_formats().")
  metadata
}

.parse_and_merge_labels <- function(metadata, results_text) {
  parsed_list <- purrr::map(results_text, function(txt) {
    tryCatch({
      tribble_text <- stringr::str_extract(txt, "(?s)tribble\\(.*?\\)")
      if (is.na(tribble_text)) stop("No tribble found")
      eval(parse(text = tribble_text))
    }, error = function(e) {
      warning("Could not parse AI response: ", conditionMessage(e))
      NULL
    })
  }) |> purrr::compact() |> dplyr::bind_rows()

  if (nrow(parsed_list) == 0) {
    warning("No valid AI responses parsed. metadata unchanged.")
    return(metadata)
  }

  parsed_list <- parsed_list |>
    dplyr::inner_join(dplyr::select(metadata, var_name, labels),
                      by = "var_name", suffix = c("", "_orig")) |>
    dplyr::filter(purrr::map2_lgl(new_labels, labels,
                                   ~ length(.x) == length(.y)))

  if (nrow(parsed_list) == 0) {
    warning("All AI responses had mismatched label lengths. metadata unchanged.")
    return(metadata)
  }

  dplyr::rows_update(metadata, dplyr::select(parsed_list, var_name, new_labels),
                     by = "var_name")
}


# ============================================================
# 11. ai_suggest_varnames()
# ============================================================

#' Use Haiku to suggest short R variable names and documentation strings
#'
#' @param metadata         Varmod tibble.
#' @param vars             Optional character vector of var_name to restrict.
#' @param examples_text    Optional: paste of a previous _recode.R for style.
#' @param api_key          ANTHROPIC_API_KEY env var by default.
#' @param model            Default: Haiku 4.5.
#' @param chunk_size       Variables per API request. Default 80.
#' @param batch_threshold  Above this n_vars, use batch mode. Default 60.
#'
#' @return metadata with new_name and doc_note updated. Review before apply!
ai_suggest_varnames <- function(
    metadata,
    vars            = NULL,
    examples_text   = NULL,
    api_key         = Sys.getenv("ANTHROPIC_API_KEY"),
    model           = "claude-haiku-4-5", # "claude-haiku-4-5-20251001"
    chunk_size      = 80,
    batch_threshold = 60
) {
  target <- metadata
  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) { message("No variables to name."); return(metadata) }

  chunks     <- split(target, ceiling(seq_len(nrow(target)) / chunk_size))
  ex_section <- if (!is.null(examples_text))
    paste0("\n\nEXAMPLE NAMING STYLE:\n```r\n", examples_text, "\n```\n") else ""

  build_prompt <- function(chunk_df) {
    rows <- purrr::pmap_chr(chunk_df, function(var_name, var_label,
                                                new_labels, ...) {
      first2 <- paste0('"', head(new_labels, 2), '"', collapse = ", ")
      sprintf('  list(var_name="%s", label="%s", first_levels=c(%s))',
              var_name, substr(var_label, 1, 80), first2)
    })
    paste0(
      'Name R variables for French survey analysis.\n',
      'For each: new_name (UPPERCASE_SNAKE_CASE, ≤20 chars, French abbrevs: ',
      'DIPL, PCS, ETUDE, AGE, SEXE, etc.) and ',
      'doc_note (one-line French desc, ≤80 chars).\n',
      ex_section,
      '\nDATA:\n', paste(rows, collapse = "\n"),
      '\n\nReply ONLY as tribble:\n',
      'tribble(\n  ~var_name, ~new_name, ~doc_note,\n',
      '  "OLD", "NEW", "Description"\n)\nNo explanation.'
    )
  }

  if (nrow(target) <= batch_threshold) {
    message("ai_suggest_varnames: synchronous (", nrow(target), " vars)")
    results_text <- purrr::imap(chunks, function(chunk, i) {
      message("  Chunk ", i, "/", length(chunks))
      resp <- ai_call_claude(build_prompt(chunk), model = model, api_key = api_key)
      resp$content[[1]]$text
    })
  } else {
    message("ai_suggest_varnames: batch mode (", nrow(target), " vars)")
    requests <- purrr::imap(chunks, function(chunk, i)
      list(custom_id = paste0("names_", i), prompt = build_prompt(chunk)))
    batch    <- ai_batch_submit(requests, model = model, api_key = api_key)
    message("Batch submitted. ID: ", batch$id)
    raw      <- ai_batch_retrieve(batch$id, api_key = api_key)
    results_text <- purrr::map(purrr::set_names(names(raw)), ~ raw[[.x]])
  }

  metadata <- .parse_and_merge_varnames(metadata, results_text)

  dups <- metadata |>
    dplyr::filter(new_name != var_name & new_name != "") |>
    dplyr::count(new_name) |>
    dplyr::filter(n > 1)
  if (nrow(dups) > 0) {
    warning("Duplicate new_name values: ", paste(dups$new_name, collapse = ", "),
            "\nFix before apply_survey_formats().")
  }

  message("Review metadata$new_name and $doc_note before applying.")
  metadata
}

.parse_and_merge_varnames <- function(metadata, results_text) {
  parsed_list <- purrr::map(results_text, function(txt) {
    tryCatch({
      tribble_text <- stringr::str_extract(txt, "(?s)tribble\\(.*?\\)")
      if (is.na(tribble_text)) stop("No tribble found")
      eval(parse(text = tribble_text))
    }, error = function(e) {
      warning("Could not parse AI response: ", conditionMessage(e))
      NULL
    })
  }) |> purrr::compact() |> dplyr::bind_rows()

  if (nrow(parsed_list) == 0) {
    warning("No valid AI responses parsed. metadata unchanged.")
    return(metadata)
  }

  update_df <- parsed_list |>
    dplyr::filter(var_name %in% metadata$var_name) |>
    dplyr::select(var_name, new_name, doc_note)

  dplyr::rows_update(metadata, update_df, by = "var_name")
}

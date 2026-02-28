# ============================================================
# Survey Formatting Pipeline — data_formatting_pipeline.R  v2
# ============================================================
# Source this file in any _mf.R script:
#   source("data_formatting_pipeline.R")
#
# USER WORKFLOW (iterate until metadata is good):
#   1. df   <- import_survey("file.sas7bdat")
#   2. meta <- extract_survey_metadata(df)
#      # Console shows binary detections + ambiguous vars count
#   3. [optional] ai_classify_roles(meta)
#      # Prints copy-pasteable detected_roles / positive_levels vectors
#      # Edit them, then re-run step 2 with those args
#   4. [optional] metadata_apply_codebook(meta, codebook_df, ...)
#   5. meta <- metadata_fix_binary(meta)
#   6. export_metadata_excel(meta, "meta_review.xlsx")
#      # Open Excel, check orange rows, iterate if needed
#   7. [optional] meta <- ai_suggest_labels(meta)
#                 meta <- ai_suggest_varnames(meta)
#                 export_metadata_excel(meta, "meta_review2.xlsx")
#   8. df_out <- apply_survey_formats(df, meta)
#      generate_format_script(meta, "mysurvey", "path/to/file")
#
# Functions:
#   import_survey()             — auto-detect format and import
#   extract_survey_metadata()   — build standardised varmod tibble
#   metadata_apply_codebook()   — merge external Excel/CSV codebook
#   metadata_fix_binary()       — standardise binary positive/negative levels
#   export_metadata_excel()     — review file (openxlsx, orange = needs attention)
#   apply_survey_formats()      — apply metadata → factors on real data
#   generate_format_script()    — write readable _recode.R for students
#
# AI helpers (require ANTHROPIC_API_KEY env var):
#   ai_classify_roles()         — Haiku: classify ambiguous vars, print dput vectors
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
#' arguments (missing_num, detected_roles, positive_levels), run again.
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
#'                        values are flagged "numeric_or_cat" (ambiguous).
#' @param detected_roles  Named character vector overriding role for specific
#'                        variables. Names = var_name, values = role string.
#'                        Paste output of ai_classify_roles() here after editing.
#' @param positive_levels Named character vector: for binary vars, which level
#'                        label is the positive/"yes" one.
#'                        Paste output of ai_classify_roles() here.
#'
#' @return A tibble with columns:
#'   var_name, var_label, r_class, n_distinct, detected_role, positive_level,
#'   is_ordinal, values, labels, missing_vals, new_labels, new_name, doc_note
extract_survey_metadata <- function(
    df,
    missing_num     = c(96, 99, 996, 999, 9996, 9999), # 8, 9, 
    missing_chr     = c("-1", "NSP", "NRP", "NR", "REFUS",
                        "Ne sait pas", "Refus"), # "8", "9",
    yes_labels      = NULL,
    no_labels       = NULL,
    max_levels_cat  = 20,
    detected_roles  = NULL,
    positive_levels = NULL
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

    # --- R class (most informative class) ---
    r_class <- class(col)[[1]]

    # --- value labels ---
    val_labs <- labelled::val_labels(col)

    # --- distinct non-NA values ---
    vals_present <- unique(col[!is.na(col)])
    n_dist       <- length(vals_present)

    # --- values and labels vectors (all, including missing candidates) ---
    if (!is.null(val_labs) && length(val_labs) > 0) {
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

    # --- flag candidate missing values (unified: numeric + char + label text) ---
    is_miss <- purrr::map2_lgl(raw_values, raw_labels, function(v, l) {
      v_num <- suppressWarnings(as.numeric(v))
      num_hit <- !is.na(v_num) && v_num %in% missing_num
      chr_hit <- as.character(v) %in% missing_chr
      lbl_hit <- grepl(missing_lbl_pattern, l, perl = TRUE)
      num_hit || chr_hit || lbl_hit
    })
    missing_vals_vec <- raw_values[is_miss]

    # --- non-missing values/labels for role detection ---
    vals_clean <- raw_values[!is_miss]
    lbls_clean <- raw_labels[!is_miss]
    n_clean    <- length(vals_clean)

    # --- detect role ---
    role_out <- .detect_role_v2(
      vname, col, val_labs, n_dist, n_rows, n_clean,
      lbls_clean, yes_kw, no_kw, max_levels_cat,
      detected_roles, r_class
    )
    detected_role  <- role_out$role
    positive_level <- if (!is.null(positive_levels) && vname %in% names(positive_levels)) {
      positive_levels[[vname]]
    } else {
      role_out$positive_level
    }

    # Override role from user-supplied vector
    if (!is.null(detected_roles) && vname %in% names(detected_roles)) {
      detected_role <- detected_roles[[vname]]
    }

    is_ordinal <- detected_role == "categorical_ordinal"

    # --- console output for binary detection ---
    if (detected_role %in% c("binary_oui_non", "binary_candidate")) {
      lv1 <- if (length(lbls_clean) >= 1) lbls_clean[[1]] else "?"
      lv2 <- if (length(lbls_clean) >= 2) lbls_clean[[2]] else "?"
      pos_str <- if (!is.null(positive_level) && !is.na(positive_level) && positive_level != "") {
        paste0(" (positive: \"", positive_level, "\")")
      } else ""
      match_str <- if (detected_role == "binary_oui_non") "keyword \u2713" else "no keyword match"
      tag <- if (detected_role == "binary_oui_non") "[binary]  " else "[binary?] "
      binary_lines <<- c(binary_lines,
        sprintf("%s%-20s: \"%s\" vs \"%s\"%s \u2014 %s",
                tag, vname, lv1, lv2, pos_str, match_str))
    }

    tibble::tibble(
      var_name       = vname,
      var_label      = var_lbl,
      r_class        = r_class,
      n_distinct     = n_dist,
      detected_role  = detected_role,
      positive_level = if (is.null(positive_level)) NA_character_ else positive_level,
      is_ordinal     = is_ordinal,
      values         = list(raw_values),
      labels         = list(raw_labels),
      missing_vals   = list(missing_vals_vec),
      new_labels     = list(raw_labels),
      new_name       = vname,
      doc_note       = var_lbl
    )
  }) |>
    dplyr::bind_rows()

  # --- Console summary ---
  n_binary    <- sum(meta$detected_role %in% c("binary_oui_non", "binary_candidate"))
  n_ambiguous <- sum(meta$detected_role %in% c("categorical", "binary_candidate",
                                                "numeric_or_cat"))
  message("\nextract_survey_metadata: ", nrow(meta), " variables | ",
          nrow(df), " observations")
  message("  Roles: ",
          paste(names(table(meta$detected_role)),
                table(meta$detected_role), sep = "=", collapse = "  "))
  if (length(binary_lines) > 0) {
    message("\nBinary variables detected:")
    purrr::walk(binary_lines, message)
  }
  if (n_ambiguous > 0) {
    message("\n[!] ", n_ambiguous, " ambiguous variable(s) need classification",
            " (categorical / binary_candidate / numeric_or_cat).")
    message("    Run ai_classify_roles(meta) to get suggested detected_roles vector.")
  }

  meta
}


# Internal role detection — not exported
.detect_role_v2 <- function(
    vname, col, val_labs, n_dist, n_rows, n_clean,
    lbls_clean, yes_kw, no_kw, max_levels_cat,
    detected_roles, r_class
) {
  # 1. Identifier: ID name or all values unique
  id_pattern <- "^(IDENT|IDENTIF|IDENTIFIANT|ID|_ID|ID_|NUMEN|NUMIDENT)$"
  if (grepl(id_pattern, vname, ignore.case = TRUE) || n_dist == n_rows) {
    return(list(role = "identifier", positive_level = NA_character_))
  }

  # 2. Has value labels → categorical (regardless of R class); check binary first
  has_val_labs <- !is.null(val_labs) && length(val_labs) > 0

  if (n_clean == 2) {
    # Structural binary: exactly 2 non-missing levels
    pos <- .find_positive_level(lbls_clean, yes_kw, no_kw)
    if (!is.null(pos)) {
      return(list(role = "binary_oui_non", positive_level = pos))
    }
    return(list(role = "binary_candidate", positive_level = NA_character_))
  }

  if (has_val_labs || is.factor(col)) {
    return(list(role = "categorical", positive_level = NA_character_))
  }

  # 3. Numeric/character without labels
  if (is.numeric(col) || r_class %in% c("numeric", "double", "integer")) {
    if (n_dist > max_levels_cat) return(list(role = "numeric", positive_level = NA_character_))
    return(list(role = "numeric_or_cat", positive_level = NA_character_))
  }

  if (is.character(col)) {
    # All values look like numbers → probably numeric codes without labels
    all_numeric_str <- all(grepl("^-?[0-9]+(\\.[0-9]+)?$",
                                  lbls_clean[lbls_clean != ""]))
    if (all_numeric_str && n_dist > max_levels_cat) {
      return(list(role = "numeric", positive_level = NA_character_))
    }
    if (n_dist > 0) return(list(role = "categorical", positive_level = NA_character_))
  }

  list(role = "other", positive_level = NA_character_)
}


# Internal: find positive level by keyword matching (partial, case-insensitive)
# Returns the label string if found, NULL if not
.find_positive_level <- function(lbls_clean, yes_kw, no_kw) {
  if (length(lbls_clean) != 2) return(NULL)

  lbl_lower <- tolower(lbls_clean)
  # Remove numeric prefix (e.g. "1-") before keyword matching
  lbl_stripped <- stringr::str_remove(lbl_lower, "^[0-9]+-")

  yes_match <- purrr::map_lgl(yes_kw, ~ any(stringr::str_detect(lbl_stripped, stringr::fixed(.x))))
  no_match  <- purrr::map_lgl(no_kw,  ~ any(stringr::str_detect(lbl_stripped, stringr::fixed(.x))))

  has_yes <- any(yes_match)
  has_no  <- any(no_match)

  if (!has_yes && !has_no) return(NULL)

  # Determine which of the two labels is positive
  yes_idx <- which(purrr::map_lgl(lbl_stripped,
    ~ any(stringr::str_detect(.x, stringr::fixed(yes_kw[yes_match])))))
  if (length(yes_idx) == 1) return(lbls_clean[[yes_idx]])

  NULL
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

#' Standardise binary variable labels using positive_level column
#'
#' For rows with detected_role == "binary_oui_non":
#'   - Renames the positive level to "1-<var_label>"
#'   - Renames the negative level to "2-Non"
#'   - Any remaining levels → "NULL" (mapped to NA at apply step)
#'
#' For rows with detected_role == "binary_candidate":
#'   - Warns and skips. Resolve these first (set detected_roles override, or
#'     set positive_levels override, then re-run extract_survey_metadata()).
#'
#' @param metadata       Varmod tibble.
#' @param use_var_label  If TRUE (default), positive level is renamed to
#'                       paste0("1-", var_label). If FALSE, keeps original label.
#'
#' @return Updated metadata tibble.
metadata_fix_binary <- function(metadata, use_var_label = TRUE) {
  candidates <- metadata |>
    dplyr::filter(detected_role == "binary_candidate") |>
    dplyr::pull(var_name)

  if (length(candidates) > 0) {
    warning(length(candidates), " binary_candidate variable(s) not yet resolved: ",
            paste(candidates, collapse = ", "),
            "\nSet positive_levels or detected_roles in extract_survey_metadata() first.")
  }

  metadata |>
    dplyr::mutate(new_labels = purrr::pmap(
      list(detected_role, var_label, new_labels, positive_level),
      function(role, lbl, nls, pos_lv) {
        if (role != "binary_oui_non") return(nls)

        purrr::map_chr(nls, function(l) {
          # Strip numeric prefix for comparison
          l_stripped <- stringr::str_remove(tolower(l), "^[0-9]+-")
          pos_stripped <- if (!is.na(pos_lv))
            stringr::str_remove(tolower(pos_lv), "^[0-9]+-")
          else ""

          is_positive <- !is.na(pos_lv) &&
            (l == pos_lv || l_stripped == pos_stripped)

          if (is_positive) {
            if (use_var_label && lbl != "") paste0("1-", lbl) else l
          } else if (grepl("^(non|no|faux|false|absent|pas |non choisi)", l_stripped)) {
            "2-Non"
          } else {
            "NULL"
          }
        })
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
#' Orange rows = variables needing a decision before apply_survey_formats().
#'
#' @param metadata        Varmod tibble.
#' @param path            Output path. Default: "metadata_review.xlsx".
#' @param highlight_roles Character vector of detected_role values to highlight
#'                        orange. Default: binary_candidate and numeric_or_cat.
#'
#' @return Invisibly returns path. Opens file if in interactive session.
export_metadata_excel <- function(
    metadata,
    path            = "metadata_review.xlsx",
    highlight_roles = c("binary_candidate", "numeric_or_cat")
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx is required. Install with: install.packages('openxlsx')")
  }

  # --- Flatten list columns to readable strings ---
  collapse5 <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    paste(head(x, 5), collapse = " / ")
  }
  collapse_miss <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    paste(x, collapse = "; ")
  }

  df_excel <- metadata |>
    dplyr::mutate(
      missing_vals_str = purrr::map_chr(missing_vals, collapse_miss),
      labels_first5    = purrr::map_chr(labels,       collapse5),
      new_labels_first5 = purrr::map_chr(new_labels,  collapse5)
    ) |>
    dplyr::select(
      var_name, var_label, r_class, detected_role, positive_level,
      is_ordinal, n_distinct,
      missing_vals   = missing_vals_str,
      labels         = labels_first5,
      new_labels     = new_labels_first5,
      new_name, doc_note
    )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "metadata")

  # Write data
  openxlsx::writeData(wb, "metadata", df_excel, startRow = 1, startCol = 1,
                      headerStyle = openxlsx::createStyle(
                        fontColour = "#FFFFFF", fgFill = "#2F4F7F",
                        halign = "left", textDecoration = "bold"
                      ))

  # Freeze header row
  openxlsx::freezePane(wb, "metadata", firstRow = TRUE)

  # Auto column widths (approximate)
  col_widths <- c(20, 40, 12, 22, 25, 10, 10, 25, 40, 40, 20, 40)
  purrr::walk2(seq_along(col_widths), col_widths, function(col, w) {
    openxlsx::setColWidths(wb, "metadata", cols = col, widths = w)
  })

  # Orange highlight for rows needing attention
  orange_style <- openxlsx::createStyle(fgFill = "#FFD580")
  orange_rows  <- which(df_excel$detected_role %in% highlight_roles)
  if (length(orange_rows) > 0) {
    openxlsx::addStyle(
      wb, "metadata", style = orange_style,
      rows = orange_rows + 1,  # +1 for header row
      cols = seq_len(ncol(df_excel)), gridExpand = TRUE
    )
  }

  # Alternating row shading for non-highlighted rows
  light_style <- openxlsx::createStyle(fgFill = "#F5F5F5")
  other_rows  <- setdiff(seq_len(nrow(df_excel)), orange_rows)
  even_rows   <- other_rows[other_rows %% 2 == 0]
  if (length(even_rows) > 0) {
    openxlsx::addStyle(
      wb, "metadata", style = light_style,
      rows = even_rows + 1, cols = seq_len(ncol(df_excel)), gridExpand = TRUE
    )
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  message("Metadata written to: ", path,
          " (", sum(df_excel$detected_role %in% highlight_roles),
          " orange rows need attention)")

  invisible(path)
}


# ============================================================
# 6. apply_survey_formats()
# ============================================================

#' Apply metadata to a dataframe: recode to factors, rename, re-label
#'
#' Terminal step. Warns if any binary_candidate variables remain unresolved.
#' Values in missing_vals are mapped to NA. new_labels == "NULL" → NA.
#' Ordinal variables (is_ordinal = TRUE) are NOT relevel-sorted.
#'
#' @param df               Tibble from import_survey().
#' @param metadata         Fully-reviewed varmod tibble.
#' @param uppercase_names  UPPERCASE all output variable names. Default TRUE.
#'
#' @return A tibble with factors applied and variables renamed.
apply_survey_formats <- function(df, metadata, uppercase_names = TRUE) {
  meta <- metadata |> dplyr::filter(var_name %in% names(df))

  candidates <- meta |>
    dplyr::filter(detected_role == "binary_candidate") |>
    dplyr::pull(var_name)
  if (length(candidates) > 0) {
    warning(length(candidates), " binary_candidate variable(s) treated as categorical: ",
            paste(candidates, collapse = ", "))
  }

  for (i in seq_len(nrow(meta))) {
    row       <- meta[i, ]
    vname     <- row$var_name
    vals      <- row$values[[1]]
    nls       <- row$new_labels[[1]]
    miss_vals <- row$missing_vals[[1]]
    role      <- row$detected_role
    is_ord    <- isTRUE(row$is_ordinal)

    if (role %in% c("numeric", "identifier", "other")) next
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
    dplyr::filter(detected_role %in% c("categorical", "categorical_nominal",
                                        "categorical_ordinal", "binary_oui_non",
                                        "binary_candidate"))

  if (nrow(cat_meta) > 0) {
    mutate_lines <- purrr::pmap_chr(cat_meta, function(
      var_name, var_label, new_name, new_labels, values, missing_vals,
      is_ordinal, detected_role, ...
    ) {
      display <- if (!is.null(new_name) && new_name != "" && new_name != var_name)
        new_name else var_name

      recode_lines <- purrr::map2_chr(new_labels, values, function(nl, v) {
        if (nl == "NULL") sprintf('    "NULL"                         = "%s"  # missing',
                                  as.character(v))
        else sprintf('    "%-35s= "%s"', paste0(nl, '"'), as.character(v))
      })

      relevel_line <- if (isTRUE(is_ordinal)) {
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
#' Sends variables with detected_role in "categorical", "binary_candidate",
#' "numeric_or_cat" to Claude Haiku. Returns three named vectors printed to
#' the console in dput/copy-paste format:
#'   - detected_roles    (var_name = role)
#'   - positive_levels   (var_name = positive label, for binary only)
#'   - extra_missing     (var_name = additional missing label flagged by AI)
#'
#' After reviewing the output:
#'   1. Copy the three vectors into your _mf.R script
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
#' @return Invisibly returns a list(detected_roles, positive_levels,
#'         extra_missing). Primary output is the console print.
ai_classify_roles <- function(
    metadata,
    api_key          = Sys.getenv("ANTHROPIC_API_KEY"),
    model            = "claude-haiku-4-5", # claude-haiku-4-5-20251001
    max_labels_sent  = 5,
    batch_threshold  = 60
) {
  target <- metadata |>
    dplyr::filter(detected_role %in% c("categorical", "binary_candidate",
                                        "numeric_or_cat"))

  if (nrow(target) == 0) {
    message("ai_classify_roles: No ambiguous variables found. ",
            "All roles already determined.")
    return(invisible(list(detected_roles  = character(0),
                          positive_levels = character(0),
                          extra_missing   = character(0))))
  }

  message("ai_classify_roles: ", nrow(target), " variable(s) to classify.")

  # Build compact prompt lines — one per variable
  prompt_lines <- purrr::pmap_chr(target, function(
    var_name, var_label, r_class, n_distinct, labels, missing_vals, ...
  ) {
    non_miss_lbls <- labels[!(labels %in% missing_vals)]
    first_n  <- head(non_miss_lbls, max_labels_sent)
    n_total  <- length(non_miss_lbls)
    lbl_str  <- paste0('"', first_n, '"', collapse = ",")
    suffix   <- if (n_total > max_labels_sent)
      paste0(",...+", n_total - max_labels_sent, "more") else ""

    sprintf('%s|"%s"|%s|nd:%d|[%s%s]',
            var_name,
            substr(var_label, 1, 60),
            r_class, n_distinct,
            lbl_str, suffix)
  })

  # System prompt — ask for abbreviated codes, R wrapper converts to full names
  system_prompt <- paste0(
    "You classify French social survey variables. For each line:\n",
    "  FORMAT: var_name|\"label\"|r_class|nd:N|[\"lv1\",\"lv2\",...]\n",
    "Reply with ONE line per variable:\n",
    "  var_name TAB CODE [TAB pos_label] [TAB miss:\"label\"]\n",
    "CODES: N=nominal O=ordinal B=binary Q=numeric ?=unclear\n",
    "- B: add TAB then the positive level label exactly as shown\n",
    "- miss: add TAB miss:\"label\" if a label text looks like missing ",
    "(NSP/Refus/etc.) but is not marked as such\n",
    "No explanations. No extra text. One variable per line."
  )

  user_prompt <- paste0(
    "Classify these variables:\n\n",
    paste(prompt_lines, collapse = "\n")
  )

  # Route sync vs batch
  if (nrow(target) <= batch_threshold) {
    message("  Synchronous call...")
    resp <- ai_call_claude(user_prompt, model = model, api_key = api_key,
                           system = system_prompt)
    raw_text <- resp$content[[1]]$text
  } else {
    message("  Batch mode (", nrow(target), " vars > threshold ", batch_threshold, ")...")
    req_list <- list(list(custom_id = "classify_all", prompt = user_prompt))
    batch    <- ai_batch_submit(req_list, model = model, api_key = api_key)
    raw      <- ai_batch_retrieve(batch$id, api_key = api_key)
    raw_text <- raw[["classify_all"]]
  }

  # Parse AI response
  role_map <- c(N = "categorical_nominal", O = "categorical_ordinal",
                B = "binary_oui_non",      Q = "numeric",
                "?" = "categorical")

  detected_roles  <- character(0)
  positive_levels <- character(0)
  extra_missing   <- character(0)

  lines <- stringr::str_split(stringr::str_trim(raw_text), "\n")[[1]]
  lines <- lines[lines != ""]

  # Build lookup: var_name → original labels (for inline comments in output)
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

    # Positive level for binary
    if (code == "B" && length(parts) >= 3) {
      pos <- stringr::str_trim(parts[[3]])
      if (!stringr::str_starts(pos, "miss:") && pos != "") {
        positive_levels[[vname]] <- pos
        parts <- parts[-3]  # remove so miss: parsing still works
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
    cat("detected_roles <- c()  # (no classifications returned)\n\n")
  }

  if (length(positive_levels) > 0) {
    pos_lines <- purrr::imap_chr(positive_levels, function(lv, vname) {
      sprintf('  %-20s = "%s"', vname, lv)
    })
    cat("positive_levels <- c(\n",
        paste(pos_lines, collapse = ",\n"), "\n)\n\n", sep = "")
  }

  if (length(extra_missing) > 0) {
    miss_lines <- purrr::imap_chr(extra_missing, function(lbl, vname) {
      sprintf('  %-20s = "%s"', vname, lbl)
    })
    cat("extra_missing <- c(\n",
        paste(miss_lines, collapse = ",\n"), "\n)\n\n", sep = "")
    message("[!] extra_missing: add these to missing_chr in extract_survey_metadata()")
  }

  message("# Then re-run:")
  message("# meta <- extract_survey_metadata(df,")
  message("#   detected_roles  = detected_roles,")
  message("#   positive_levels = positive_levels)")
  message(strrep("=", 60))

  invisible(list(detected_roles  = detected_roles,
                 positive_levels = positive_levels,
                 extra_missing   = extra_missing))
}


# ============================================================
# 10. ai_suggest_labels()
# ============================================================

#' Use Haiku to suggest concise French factor level labels
#'
#' Sends categorical variables to Claude and asks it to shorten labels to
#' ≤ 25 characters while preserving numeric prefix and meaning.
#' Ordinal variables (is_ordinal = TRUE): labels are shortened but NOT reordered.
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
    dplyr::filter(detected_role %in% c("categorical", "categorical_nominal",
                                        "categorical_ordinal", "binary_oui_non",
                                        "binary_candidate"))
  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) {
    message("ai_suggest_labels: No categorical variables to label.")
    return(metadata)
  }

  chunks     <- split(target, ceiling(seq_len(nrow(target)) / chunk_size))
  ex_section <- if (!is.null(examples_text))
    paste0("\n\nEXAMPLE STYLE:\n```r\n", examples_text, "\n```\n") else ""

  build_prompt <- function(chunk_df) {
    rows <- purrr::pmap_chr(chunk_df, function(var_name, var_label, labels,
                                                is_ordinal, ...) {
      ord_note <- if (isTRUE(is_ordinal)) " [ORDINAL: keep order, do not rearrange]" else ""
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

# ============================================================
# Survey Formatting Pipeline — data_formatting_pipeline.R
# ============================================================
# Source this file in any _mf.R script:
#   source("data_formatting_pipeline.R")
#
# Functions (in pipeline order):
#   import_survey()             — auto-detect format and import
#   extract_survey_metadata()   — build standardised varmod tibble
#   metadata_apply_codebook()   — merge external Excel/CSV codebook
#   metadata_fix_binary()       — standardise Oui/Non binary variables
#   apply_survey_formats()      — apply metadata → factors on real data
#   generate_format_script()    — write readable _recode.R for students
#
# AI helpers (require ANTHROPIC_API_KEY env var):
#   ai_call_claude()            — synchronous single call (httr2)
#   ai_batch_submit()           — submit Message Batch job (httr2)
#   ai_batch_retrieve()         — poll + retrieve batch results (httr2)
#   ai_suggest_labels()         — shorten French factor level labels
#   ai_suggest_varnames()       — propose short R variable names
# ============================================================


# ============================================================
# 1. import_survey()
# ============================================================

#' Auto-detect file format and import a survey dataset
#'
#' @param path     Path to the data file.
#' @param format   Optional override: "sas", "dta", "parquet", "sav", "rds".
#'                 If NULL, detected from file extension.
#' @param catalog_file  Optional path to a SAS catalog file (.sas7bcat).
#' @param encoding Character encoding for SAS files. Default "UTF-8".
#'                 Try "latin1" if accents appear garbled.
#'
#' @return A tibble. Labels are preserved but no transformation is applied.
import_survey <- function(
    path,
    format       = NULL,
    catalog_file = NULL,
    encoding     = NULL # "UTF-8"
) {
  ext <- if (!is.null(format)) format else {
    tolower(tools::file_ext(path))
  }

  switch(
    ext,
    "sas7bdat" = ,
    "sas"      = haven::read_sas(
      data_file    = path,
      catalog_file = catalog_file,
      encoding     = encoding
    ),
    "dta"      = haven::read_dta(path, encoding = encoding),
    "sav"      = haven::read_sav(path, encoding = encoding),
    "parquet"  = arrow::read_parquet(path),
    "rds"      = readRDS(path),
    stop("Unrecognised file format: '", ext, "'. ",
         "Use format = 'sas'/'dta'/'sav'/'parquet'/'rds'.")
  )
}


# ============================================================
# 2. extract_survey_metadata()
# ============================================================

#' Extract variable and value metadata from a labelled tibble
#'
#' Produces a "varmod" tibble describing every variable: its role, distinct
#' values, labels, and candidate missing values. All columns marked with
#' "placeholder" are intended to be reviewed and modified before passing to
#' apply_survey_formats() or generate_format_script().
#'
#' @param df               A tibble, ideally imported via import_survey().
#' @param missing_patterns Numeric values to flag as candidate missing values.
#'                         Default: c(96, 99, 996, 999, 9996, 9999).
#'                         These are NOT applied automatically — review first.
#'
#' @return A tibble with columns:
#'   var_name, var_label, detected_role, n_distinct,
#'   values, labels, missing_vals, new_labels, new_name, doc_note
extract_survey_metadata <- function(
    df,
    missing_patterns = c(96, 99, 996, 999, 9996, 9999)
) {
  var_labels_list <- labelled::get_variable_labels(df)
  n_rows          <- nrow(df)

  purrr::imap(df, function(col, vname) {

    # --- variable label ---
    var_lbl <- var_labels_list[[vname]]
    if (is.null(var_lbl) || is.na(var_lbl)) var_lbl <- ""

    # --- value labels (from haven/labelled) ---
    val_labs <- labelled::val_labels(col)  # named numeric/char vector or NULL

    # --- distinct non-NA values ---
    vals_present <- unique(col[!is.na(col)])
    n_dist       <- length(vals_present)

    # --- values and labels vectors ---
    if (!is.null(val_labs) && length(val_labs) > 0) {
      raw_values  <- unname(val_labs)          # numeric codes
      raw_labels  <- names(val_labs)           # label strings
    } else if (is.factor(col)) {
      raw_values  <- as.character(levels(col))
      raw_labels  <- levels(col)
    } else {
      raw_values  <- as.character(sort(vals_present))
      raw_labels  <- as.character(sort(vals_present))
    }

    # --- detect role ---
    detected_role <- .detect_role(vname, col, val_labs, n_dist, n_rows)

    # --- candidate missing values ---
    missing_vals <- raw_values[raw_values %in% missing_patterns |
                                 raw_labels  %in% as.character(missing_patterns)]

    tibble::tibble(
      var_name      = vname,
      var_label     = var_lbl,
      detected_role = detected_role,
      n_distinct    = n_dist,
      values        = list(raw_values),
      labels        = list(raw_labels),
      missing_vals  = list(missing_vals),
      # placeholders — modify these before apply:
      new_labels    = list(raw_labels),   # copy of labels to edit
      new_name      = vname,              # rename suggestion goes here
      doc_note      = var_lbl            # one-line doc for generated script
    )
  }) |>
    dplyr::bind_rows()
}

# Internal role heuristic — not exported
.detect_role <- function(vname, col, val_labs, n_dist, n_rows) {
  # Identifier: name matches ID/IDENT pattern, or all values unique
  if (grepl("^(IDENT|ID_|_ID|IDENTIFIANT)$", vname, ignore.case = TRUE) ||
      n_dist == n_rows) {
    return("identifier")
  }

  # Check for Oui/Non binary (with optional numeric prefix)
  if (!is.null(val_labs) && length(val_labs) <= 4) {
    lbl_lower <- tolower(names(val_labs))
    has_oui   <- any(grepl("^(1-)?oui$", lbl_lower))
    has_non   <- any(grepl("^(0|2)-?non$|^non$", lbl_lower))
    if (has_oui && has_non) return("binary_oui_non")
  }
  if (is.factor(col) && nlevels(col) <= 4) {
    lv_lower <- tolower(levels(col))
    has_oui  <- any(grepl("^(1-)?oui$", lv_lower))
    has_non  <- any(grepl("^(0|2)-?non$|^non$", lv_lower))
    if (has_oui && has_non) return("binary_oui_non")
  }

  # Categorical: has value labels or is factor with ≥ 3 levels
  if ((!is.null(val_labs) && length(val_labs) >= 3) ||
      (is.factor(col) && nlevels(col) >= 3)) {
    return("categorical")
  }

  # Numeric: no value labels, not factor
  if (is.numeric(col) && is.null(val_labs)) return("numeric")

  "other"
}


# ============================================================
# 3. metadata_apply_codebook()
# ============================================================

#' Merge an external codebook (Excel/CSV) into a metadata tibble
#'
#' Overwrites var_label, labels, and new_labels columns where the codebook
#' provides information. Leaves other rows/columns intact.
#'
#' @param metadata        A varmod tibble from extract_survey_metadata().
#' @param codebook_df     A data frame with at least one of: variable labels
#'                        or value labels. Typically from readxl::read_excel().
#' @param var_col         Column name in codebook_df containing variable codes
#'                        (matched to metadata$var_name).
#' @param label_col       Column name for variable-level labels. NULL to skip.
#' @param code_col        Column name for value codes. NULL to skip value labels.
#' @param value_label_col Column name for value label strings. NULL to skip.
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
  # --- Update variable-level labels ---
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

  # --- Update value-level labels ---
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
        .groups = "drop"
      )

    metadata <- metadata |>
      dplyr::left_join(val_lbl_map, by = "var_name") |>
      dplyr::mutate(
        values     = purrr::map2(values, cb_values,
                                 ~ if (!is.null(.y)) .y else .x),
        labels     = purrr::map2(labels, cb_labels,
                                 ~ if (!is.null(.y)) .y else .x),
        new_labels = purrr::map2(new_labels, cb_labels,
                                 ~ if (!is.null(.y)) .y else .x)
      ) |>
      dplyr::select(-cb_values, -cb_labels)
  }

  metadata
}


# ============================================================
# 4. metadata_fix_binary()
# ============================================================

#' Standardise binary Oui/Non variables in metadata
#'
#' For all rows with detected_role == "binary_oui_non":
#'   - Replaces the first level ("1-Oui" or "Oui") with "1-<var_label>"
#'   - Ensures the Non level is labelled "2-Non"
#'   - Any additional levels (3+) are set to NA mapping (written as "NULL")
#'
#' Relies on var_label to name the Oui level. Make sure var_label is populated
#' (possibly via metadata_apply_codebook()) before calling this.
#'
#' @param metadata A varmod tibble from extract_survey_metadata().
#'
#' @return Updated metadata tibble with new_labels modified for binary vars.
metadata_fix_binary <- function(metadata) {
  metadata |>
    dplyr::mutate(new_labels = purrr::pmap(
      list(detected_role, var_label, new_labels),
      function(role, lbl, nls) {
        if (role != "binary_oui_non") return(nls)

        purrr::map_chr(nls, function(l) {
          ll <- tolower(l)
          if (grepl("^(1-)?oui$", ll))           paste0("1-", lbl)
          else if (grepl("^(0|2)-?non$|^non$", ll)) "2-Non"
          else                                       "NULL"  # maps to NA at apply step
        })
      }
    ))
}


# ============================================================
# 5. apply_survey_formats()
# ============================================================

#' Apply metadata to a dataframe: rename, recode, and factor
#'
#' This is the terminal step. Reads metadata$new_name, metadata$values,
#' metadata$new_labels, and metadata$missing_vals to transform the actual data.
#'
#' Values listed in missing_vals are mapped to NA (not "NULL" factor level).
#' new_labels entries of "NULL" are also mapped to NA.
#'
#' Wraps and generalises the existing mf_from_varmod() logic from Demarrage.R.
#'
#' @param df               A tibble imported via import_survey().
#' @param metadata         A varmod tibble, fully reviewed and edited.
#' @param uppercase_names  If TRUE (default), UPPERCASE all output variable names.
#' @param relevel_sort     If TRUE (default), call fct_relevel(sort) after recoding
#'                         for categorical variables. Set to FALSE for ordinal vars
#'                         (you must set is_ordinal flag manually in metadata first,
#'                         then filter those out before calling this function).
#'
#' @return A tibble with factors applied and variables renamed.
apply_survey_formats <- function(
    df,
    metadata,
    uppercase_names = TRUE,
    relevel_sort    = TRUE
) {
  # Only process variables that exist in the dataframe
  meta <- metadata |>
    dplyr::filter(var_name %in% names(df))

  # --- Step 1: apply factor recodes ---
  for (i in seq_len(nrow(meta))) {
    row       <- meta[i, ]
    vname     <- row$var_name
    vals      <- row$values[[1]]
    nls       <- row$new_labels[[1]]
    miss_vals <- row$missing_vals[[1]]
    role      <- row$detected_role

    if (role %in% c("numeric", "identifier", "other")) next
    if (length(vals) == 0 || length(nls) == 0) next

    # Build recode vector: new_label = old_code
    # Entries where new_label == "NULL" get mapped to NA
    null_mask  <- nls == "NULL"
    keep_mask  <- !null_mask
    recode_vec <- purrr::set_names(as.character(vals[keep_mask]),
                                   nls[keep_mask])

    df[[vname]] <- df[[vname]] |>
      as.character() |>
      dplyr::recode(!!!recode_vec, .default = NA_character_) |>
      # also NA-ify explicit missing_vals
      (\(x) dplyr::if_else(x %in% as.character(miss_vals) |
                             as.character(df[[vname]]) %in% as.character(miss_vals),
                           NA_character_, x))() |>
      factor()

    if (relevel_sort) {
      df[[vname]] <- forcats::fct_relevel(df[[vname]], sort)
    }
  }

  # --- Step 2: rename variables ---
  rename_map <- meta |>
    dplyr::filter(new_name != var_name, new_name != "") |>
    dplyr::select(var_name, new_name)

  if (nrow(rename_map) > 0) {
    rename_vec <- purrr::set_names(rename_map$var_name, rename_map$new_name)
    df <- dplyr::rename(df, !!!rename_vec)
  }

  # --- Step 3: uppercase names ---
  if (uppercase_names) {
    names(df) <- toupper(names(df))
  }

  # --- Step 4: re-apply variable labels ---
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
# 6. generate_format_script()
# ============================================================

#' Generate a readable _recode.R script for students
#'
#' Produces an R script file that a student can read, modify, and re-run.
#' The script uses base pipe, forcats, and explicit function calls — no
#' dependencies on this pipeline file are needed to run the generated script.
#'
#' @param metadata    A fully-reviewed varmod tibble.
#' @param df_name     Short name for the dataset, used as object name in the
#'                    generated script (e.g. "prfe", "gen1017").
#' @param input_path  Path to the original data file (for documentation).
#' @param output_path Path to write the generated .R file. If NULL, uses
#'                    paste0(df_name, "_recode.R") in current directory.
#' @param n_obs       Number of observations (nrow of original df). Optional.
#'
#' @return Invisibly returns the script text. Also writes the file.
generate_format_script <- function(
    metadata,
    df_name,
    input_path,
    output_path = NULL,
    n_obs       = NULL
) {
  if (is.null(output_path)) output_path <- paste0(df_name, "_recode.R")

  n_vars  <- nrow(metadata)
  n_str   <- if (!is.null(n_obs)) formatC(n_obs, format = "d", big.mark = " ") else "?"
  today   <- format(Sys.Date(), "%Y-%m-%d")

  # --- STEP 2: rename block ---
  rename_rows <- metadata |>
    dplyr::filter(new_name != var_name & new_name != "")

  if (nrow(rename_rows) > 0) {
    rename_lines <- purrr::pmap_chr(rename_rows, function(var_name, new_name, var_label, ...) {
      pad <- strrep(" ", max(0, 20 - nchar(new_name)))
      sprintf('  %s%s= %s,  # "%s"', new_name, pad, var_name,
              substr(var_label, 1, 60))
    })
    rename_block <- paste0(
      df_name, ' <- ', df_name, ' |> dplyr::rename(\n',
      paste(rename_lines, collapse = "\n"),
      '\n)\n'
    )
  } else {
    rename_block <- paste0('# No variables renamed (new_name == var_name for all)\n')
  }

  # --- STEP 3: mutate block ---
  cat_meta <- metadata |>
    dplyr::filter(detected_role %in% c("categorical", "binary_oui_non"))

  if (nrow(cat_meta) > 0) {
    mutate_lines <- purrr::pmap_chr(cat_meta, function(
      var_name, var_label, new_name, new_labels, values, missing_vals, ...
    ) {
      display_name <- if (!is.null(new_name) && new_name != "" &&
                           new_name != var_name) new_name else var_name

      # Build fct_recode lines
      recode_lines <- purrr::map2_chr(new_labels, values, function(nl, v) {
        if (nl == "NULL") {
          sprintf('    "NULL"%-20s= "%s"   # missing', "", as.character(v))
        } else {
          sprintf('    "%-30s= "%s"',
                  paste0(nl, '"'), as.character(v))
        }
      })

      paste0(
        '\n  # ', display_name, ': "', substr(var_label, 1, 70), '"\n',
        '  ', display_name, ' = factor(as.character(', display_name, ')) |>\n',
        '    forcats::fct_recode(\n',
        paste(recode_lines, collapse = ",\n"), '\n',
        '    ) |>\n',
        '    forcats::fct_relevel(sort),'
      )
    })

    mutate_block <- paste0(
      df_name, ' <- ', df_name, ' |> dplyr::mutate(\n',
      paste(mutate_lines, collapse = "\n"),
      '\n)\n'
    )
  } else {
    mutate_block <- "# No categorical variables to format\n"
  }

  # --- Assemble full script ---
  script <- paste0(
    '# === SURVEY FORMATTING SCRIPT: ', toupper(df_name), ' ===\n',
    '# Generated : ', today, '\n',
    '# Source    : ', input_path, '\n',
    '# N = ', n_str, ' | Variables: ', n_vars, '\n',
    '#\n',
    '# HOW TO USE:\n',
    '#   1. Review the rename and mutate sections below.\n',
    '#   2. Edit labels or names if needed.\n',
    '#   3. Run once to produce ', df_name, '.rds\n',
    '#   4. In your analysis script: readRDS("', df_name, '.rds")\n',
    '\n',
    'library(dplyr)\n',
    'library(forcats)\n',
    'library(labelled)\n',
    '\n',
    '# To regenerate the metadata that produced this script:\n',
    '# source("data_formatting_pipeline.R")\n',
    '# ', df_name, '_raw <- import_survey("', input_path, '")\n',
    '# ', df_name, '_meta <- extract_survey_metadata(', df_name, '_raw)\n',
    '\n\n',
    '# STEP 1: Import ----\n',
    df_name, ' <- import_survey("', input_path, '")\n',
    '\n\n',
    '# STEP 2: Rename variables ----\n',
    rename_block,
    '\n\n',
    '# STEP 3: Format categorical variables ----\n',
    mutate_block,
    '\n\n',
    '# STEP 4: Export ----\n',
    df_name, ' |> saveRDS("', df_name, '.rds")\n',
    '\nmessage("Done. ', df_name, '.rds written.")\n'
  )

  writeLines(script, output_path, useBytes = FALSE)
  message("Script written to: ", output_path)
  invisible(script)
}


# ============================================================
# 7. AI helpers — httr2 only, no reticulate
# ============================================================

#' Single synchronous call to Claude API
#'
#' Use for interactive/small datasets (< ~60 variables). For larger datasets
#' use ai_batch_submit() + ai_batch_retrieve() to benefit from 50% cost
#' discount and separate rate limit bucket.
#'
#' @param prompt    Character string to send as user message.
#' @param model     Claude model ID. Default: Haiku 4.5 (fast, cheap).
#'                  Use "claude-sonnet-4-6" for messy codebook parsing.
#' @param api_key   Anthropic API key. Default: ANTHROPIC_API_KEY env var.
#' @param max_tokens Maximum response tokens.
#'
#' @return Parsed JSON response list (same structure as API response).
ai_call_claude <- function(
    prompt,
    model     = "claude-haiku-4-5-20251001",
    api_key   = Sys.getenv("ANTHROPIC_API_KEY"),
    max_tokens = 4096
) {
  if (api_key == "") stop("ANTHROPIC_API_KEY not set. Run: Sys.setenv(ANTHROPIC_API_KEY = 'sk-...')")

  httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    httr2::req_body_json(list(
      model      = model,
      max_tokens = max_tokens,
      messages   = list(list(role = "user", content = prompt))
    )) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}


#' Submit a Message Batch job to the Claude API
#'
#' Batch API: up to 10 000 requests per batch, 50% cheaper than synchronous,
#' separate rate limit bucket (does not consume standard Messages API limits).
#' Results are returned asynchronously — use ai_batch_retrieve() to poll.
#'
#' @param requests     A list of lists, each with:
#'                       $custom_id  — unique string to match results
#'                       $prompt     — character prompt string
#' @param model        Claude model ID. Default: Haiku 4.5.
#' @param api_key      Anthropic API key.
#' @param max_tokens   Max tokens per response.
#'
#' @return Parsed API response, including $id (the batch_id to pass to
#'         ai_batch_retrieve()).
ai_batch_submit <- function(
    requests,
    model      = "claude-haiku-4-5-20251001",
    api_key    = Sys.getenv("ANTHROPIC_API_KEY"),
    max_tokens = 4096
) {
  if (api_key == "") stop("ANTHROPIC_API_KEY not set.")

  batch_requests <- purrr::map(requests, function(req) {
    list(
      custom_id = req$custom_id,
      params    = list(
        model      = model,
        max_tokens = max_tokens,
        messages   = list(list(role = "user", content = req$prompt))
      )
    )
  })

  httr2::request("https://api.anthropic.com/v1/messages/batches") |>
    httr2::req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    httr2::req_body_json(list(requests = batch_requests)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}


#' Poll a Message Batch until complete, then retrieve results
#'
#' Blocks until the batch finishes (status == "ended"). Results are returned
#' as a list of responses, keyed by custom_id. Each element has $content
#' (the text reply) or $error if that request failed.
#'
#' @param batch_id      The $id returned by ai_batch_submit().
#' @param poll_interval Seconds to wait between status checks. Default 30.
#' @param api_key       Anthropic API key.
#'
#' @return A named list: names = custom_id, values = parsed response content.
ai_batch_retrieve <- function(
    batch_id,
    poll_interval = 30,
    api_key       = Sys.getenv("ANTHROPIC_API_KEY")
) {
  if (api_key == "") stop("ANTHROPIC_API_KEY not set.")

  status_url <- paste0("https://api.anthropic.com/v1/messages/batches/", batch_id)

  # --- Poll until done ---
  repeat {
    status_resp <- httr2::request(status_url) |>
      httr2::req_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    proc_status <- status_resp$processing_status
    message("Batch ", batch_id, " — status: ", proc_status)

    if (proc_status == "ended") break

    Sys.sleep(poll_interval)
  }

  # --- Retrieve JSONL results ---
  results_url <- status_resp$results_url

  raw_lines <- httr2::request(results_url) |>
    httr2::req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    stringr::str_split("\n") |>
    purrr::pluck(1) |>
    purrr::discard(~ .x == "")

  parsed <- purrr::map(raw_lines, jsonlite::fromJSON)

  # Return as named list: custom_id → content text
  purrr::set_names(
    purrr::map(parsed, function(r) {
      if (r$result$type == "succeeded") {
        r$result$message$content[[1]]$text
      } else {
        list(error = r$result$error)
      }
    }),
    purrr::map_chr(parsed, "custom_id")
  )
}


# ============================================================
# 8. ai_suggest_labels()
# ============================================================

#' Use Claude to suggest concise French factor level labels
#'
#' Sends variable metadata to Claude and asks it to shorten label strings to
#' ≤ 25 characters while preserving numeric prefix and meaning.
#'
#' Routes to synchronous or batch mode depending on n_vars vs batch_threshold.
#'
#' Output MUST be reviewed before applying, especially for ordinal variables
#' where AI may confuse order.
#'
#' @param metadata         A varmod tibble (from extract_survey_metadata()).
#' @param vars             Optional character vector of var_name to restrict to.
#' @param examples_text    Optional character string: paste of a previous
#'                         *_recode.R to show desired style to the model.
#' @param api_key          Anthropic API key.
#' @param model            Default: Haiku 4.5. Use Sonnet 4.6 if output quality
#'                         is poor after review.
#' @param chunk_size       Variables per API request. Default 50.
#' @param batch_threshold  Above this n_vars, use batch mode. Default 60.
#'
#' @return metadata with new_labels column updated. Review before apply!
ai_suggest_labels <- function(
    metadata,
    vars            = NULL,
    examples_text   = NULL,
    api_key         = Sys.getenv("ANTHROPIC_API_KEY"),
    model           = "claude-haiku-4-5-20251001",
    chunk_size      = 50,
    batch_threshold = 60
) {
  target <- metadata |>
    dplyr::filter(detected_role %in% c("categorical", "binary_oui_non"))

  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) { message("No categorical variables to label."); return(metadata) }

  # Build chunks
  chunks      <- split(target, ceiling(seq_len(nrow(target)) / chunk_size))
  ex_section  <- if (!is.null(examples_text)) {
    paste0("\n\nEXAMPLE OF DESIRED STYLE (from a previous script):\n```r\n",
           examples_text, "\n```\n")
  } else ""

  build_prompt <- function(chunk_df) {
    rows <- purrr::pmap_chr(chunk_df, function(var_name, var_label, labels, ...) {
      labs_str <- paste0('"', labels, '"', collapse = ", ")
      sprintf('  list(var_name = "%s", var_label = "%s", labels = c(%s))',
              var_name, substr(var_label, 1, 80), labs_str)
    })
    paste0(
      'You are formatting French social survey data for student analysis.\n',
      'Shorten each factor level label to ≤25 characters. Keep the numeric prefix ',
      '(e.g. "1-", "2-"). Do not change the meaning. Keep "NULL" as-is.\n',
      ex_section,
      '\nDATA (one row per variable):\n',
      paste(rows, collapse = "\n"),
      '\n\nReply ONLY as an R tribble with exactly these columns:\n',
      'var_name (chr), new_labels (list of chr vectors, same length as input labels).\n',
      'Example:\n',
      'tribble(\n',
      '  ~var_name, ~new_labels,\n',
      '  "DIPL", list(c("1-Aucun", "2-CEP", "3-BEPC"))\n',
      ')\n',
      'Do not include any explanation or markdown.'
    )
  }

  if (nrow(target) <= batch_threshold) {
    # --- Synchronous mode ---
    message("ai_suggest_labels: synchronous mode (", nrow(target), " vars)")
    results_text <- purrr::imap(chunks, function(chunk, i) {
      message("  Chunk ", i, "/", length(chunks), " (", nrow(chunk), " vars)")
      resp <- ai_call_claude(build_prompt(chunk), model = model, api_key = api_key)
      resp$content[[1]]$text
    })
  } else {
    # --- Batch mode ---
    message("ai_suggest_labels: batch mode (", nrow(target), " vars, threshold = ", batch_threshold, ")")
    requests <- purrr::imap(chunks, function(chunk, i) {
      list(custom_id = paste0("labels_chunk_", i), prompt = build_prompt(chunk))
    })
    batch    <- ai_batch_submit(requests, model = model, api_key = api_key)
    message("Batch submitted. ID: ", batch$id, ". Polling for results...")
    raw      <- ai_batch_retrieve(batch$id, api_key = api_key)
    results_text <- purrr::map(purrr::set_names(names(raw)), ~ raw[[.x]])
  }

  # Parse and merge results back
  metadata <- .parse_and_merge_labels(metadata, results_text)
  message("Review metadata$new_labels before calling apply_survey_formats().")
  metadata
}

# Internal: parse tribble text from AI and merge into metadata
.parse_and_merge_labels <- function(metadata, results_text) {
  parsed_list <- purrr::map(results_text, function(txt) {
    tryCatch({
      # Extract tribble call from text
      tribble_text <- stringr::str_extract(txt, "(?s)tribble\\(.*?\\)")
      if (is.na(tribble_text)) stop("No tribble found in response")
      eval(parse(text = tribble_text))
    }, error = function(e) {
      warning("Could not parse AI response: ", conditionMessage(e))
      NULL
    })
  }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  if (nrow(parsed_list) == 0) {
    warning("No valid AI responses could be parsed. metadata unchanged.")
    return(metadata)
  }

  # Validate: check that list lengths match
  parsed_list <- parsed_list |>
    dplyr::inner_join(
      dplyr::select(metadata, var_name, labels),
      by = "var_name",
      suffix = c("", "_orig")
    ) |>
    dplyr::filter(purrr::map2_lgl(
      new_labels, labels,
      ~ length(.x) == length(.y)
    ))

  if (nrow(parsed_list) == 0) {
    warning("All AI responses had mismatched label lengths. metadata unchanged.")
    return(metadata)
  }

  dplyr::rows_update(
    metadata,
    dplyr::select(parsed_list, var_name, new_labels),
    by = "var_name"
  )
}


# ============================================================
# 9. ai_suggest_varnames()
# ============================================================

#' Use Claude to suggest short R variable names and documentation strings
#'
#' Sends var_name, var_label, and first 2 level labels per variable.
#' Claude returns a short uppercase snake_case name and a one-line doc string.
#'
#' Routes to synchronous or batch mode depending on n_vars vs batch_threshold.
#'
#' Output must be reviewed. Duplicates in new_name are detected and flagged.
#'
#' @param metadata         A varmod tibble.
#' @param vars             Optional character vector of var_name to restrict to.
#' @param examples_text    Optional string: paste of a previous *_recode.R.
#' @param api_key          Anthropic API key.
#' @param model            Default: Haiku 4.5.
#' @param chunk_size       Variables per API request. Default 80.
#' @param batch_threshold  Above this n_vars, use batch mode. Default 60.
#'
#' @return metadata with new_name and doc_note columns updated. Review before apply!
ai_suggest_varnames <- function(
    metadata,
    vars            = NULL,
    examples_text   = NULL,
    api_key         = Sys.getenv("ANTHROPIC_API_KEY"),
    model           = "claude-haiku-4-5-20251001",
    chunk_size      = 80,
    batch_threshold = 60
) {
  target <- metadata
  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) { message("No variables to name."); return(metadata) }

  chunks     <- split(target, ceiling(seq_len(nrow(target)) / chunk_size))
  ex_section <- if (!is.null(examples_text)) {
    paste0("\n\nEXAMPLE OF DESIRED NAMING STYLE:\n```r\n", examples_text, "\n```\n")
  } else ""

  build_prompt <- function(chunk_df) {
    rows <- purrr::pmap_chr(chunk_df, function(var_name, var_label, new_labels, ...) {
      first2 <- paste0('"', head(new_labels, 2), '"', collapse = ", ")
      sprintf('  list(var_name = "%s", var_label = "%s", first_levels = c(%s))',
              var_name, substr(var_label, 1, 80), first2)
    })
    paste0(
      'You are naming R variables for French social survey data analysis.\n',
      'For each variable, suggest:\n',
      '  1. new_name: short UPPERCASE_SNAKE_CASE R name, ≤20 chars, use French ',
      'abbreviations when helpful (e.g. DIPL, PCS, ETUDE, AGE, SEXE).\n',
      '  2. doc_note: one-line French description ≤80 chars, precise and clear ',
      'for student readers.\n',
      ex_section,
      '\nDATA:\n',
      paste(rows, collapse = "\n"),
      '\n\nReply ONLY as an R tribble:\n',
      'tribble(\n',
      '  ~var_name, ~new_name, ~doc_note,\n',
      '  "OLD_VAR", "NEW_NAME", "Description courte"\n',
      ')\n',
      'No explanation, no markdown.'
    )
  }

  if (nrow(target) <= batch_threshold) {
    message("ai_suggest_varnames: synchronous mode (", nrow(target), " vars)")
    results_text <- purrr::imap(chunks, function(chunk, i) {
      message("  Chunk ", i, "/", length(chunks))
      resp <- ai_call_claude(build_prompt(chunk), model = model, api_key = api_key)
      resp$content[[1]]$text
    })
  } else {
    message("ai_suggest_varnames: batch mode (", nrow(target), " vars)")
    requests <- purrr::imap(chunks, function(chunk, i) {
      list(custom_id = paste0("names_chunk_", i), prompt = build_prompt(chunk))
    })
    batch    <- ai_batch_submit(requests, model = model, api_key = api_key)
    message("Batch submitted. ID: ", batch$id)
    raw      <- ai_batch_retrieve(batch$id, api_key = api_key)
    results_text <- purrr::map(purrr::set_names(names(raw)), ~ raw[[.x]])
  }

  metadata <- .parse_and_merge_varnames(metadata, results_text)

  # Warn on duplicate new_names
  dups <- metadata |>
    dplyr::filter(new_name != var_name & new_name != "") |>
    dplyr::count(new_name) |>
    dplyr::filter(n > 1)
  if (nrow(dups) > 0) {
    warning("Duplicate new_name values detected: ",
            paste(dups$new_name, collapse = ", "),
            "\nReview and fix before apply_survey_formats().")
  }

  message("Review metadata$new_name and metadata$doc_note before applying.")
  metadata
}

# Internal: parse tribble text and merge new_name / doc_note
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
  }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  if (nrow(parsed_list) == 0) {
    warning("No valid AI responses parsed. metadata unchanged.")
    return(metadata)
  }

  # Only update rows where AI returned a result
  update_df <- parsed_list |>
    dplyr::filter(var_name %in% metadata$var_name) |>
    dplyr::select(var_name, new_name, doc_note)

  dplyr::rows_update(metadata, update_df, by = "var_name")
}

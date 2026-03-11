# ============================================================
# Survey Formatting Pipeline — data_formatting_pipeline.R  v4
# ============================================================
# Source this file in any _mf.R script:
#   source("data_formatting_pipeline.R")
#
# METADATA SCHEMA — extract_survey_metadata() produces a tibble with:
#   var_name, var_label, r_class, n_distinct, detected_role,
#   values, labels, missing_vals, new_labels, new_name, order
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
#   order (integer list-column, parallel to values):
#     factor_binary/ordinal/nominal: integer ≥ 1 = desired position in output factor
#       (levels with same integer are merged before AI label suggestion)
#       binary: order=1 = positive level (Oui, Choisi…), order=2 = negative
#       ordinal: order=1 = first shown level (direction set by ai_classify_roles)
#     missing/suppressed level: NA_integer_ (JSON: "missing": true, no "order" field)
#     other roles: NA_integer_
#
# USER WORKFLOW (iterate until metadata is good):
#   1. df   <- import_survey("file.sas7bdat")
#   2b.[optional] ai_suggest_missing(meta)
#      # Prints missing_chr / missing_num vectors — paste into step 2 args
#   2. meta <- extract_survey_metadata(df,
#                missing_chr = c(...), missing_num = c(...))
#      # Console shows factor_binary detections + n needing AI classification
#   3. [optional] ai_classify_roles(meta, meta_json = meta_json)
#      # Writes "order" integers to meta_json — review, then re-run step 2
#   4. [optional] metadata_apply_codebook(meta, codebook_df, ...)
#   5. meta <- metadata_fix_binary(meta)
#   6. export_metadata_excel(meta, "meta_review.xlsx")
#      # Open Excel, check orange rows (factor_binary/nominal/integer), iterate
#   6b.[optional] meta <- metadata_add_level_stats(meta, df, meta_json = meta_json)
#      # Adds level_counts/level_freqs to metadata + writes n/pct into meta_json
#      # Also sets missing:true for zero-n levels in meta_json
#   6c.[optional] meta <- metadata_merge_ordinal_levels(meta, meta_json = meta_json)
#      # Fast algorithmic pre-pass: merges small ordinal levels by frequency threshold
#   6d.[optional] ai_merge_levels(meta, meta_json = meta_json)
#      # Semantic merge by Haiku: decides group structure for ordinal (+ nominal if nominal=TRUE)
#      # Writes merged "order" integers to meta_json; collapses to factor_binary when 2 groups
#      # Run AFTER ai_classify_roles() and metadata_add_level_stats()
#   7. [optional] ai_suggest_labels(meta, meta_json = meta_json)
#                 ai_suggest_varnames(meta, meta_json = meta_json)
#                 export_metadata_excel(meta, "meta_review2.xlsx")
#   8. generate_format_script(meta, "mysurvey", "path/to/file")
#
# Functions:
#   import_survey()                  — auto-detect format and import
#   extract_survey_metadata()        — build standardised varmod tibble (v4 schema)
#   metadata_apply_codebook()        — merge external Excel/CSV codebook
#   metadata_fix_binary()            — standardise binary positive/negative labels
#   metadata_merge_ordinal_levels()  — fast algorithmic ordinal merge (frequency threshold)
#   ai_merge_levels()                — semantic ordinal/nominal merge via Haiku; writes order to JSON
#   export_metadata_excel()          — review file (openxlsx, orange = needs attention)
#   generate_format_script()         — write readable _recode.R for students
#
# AI helpers (require ANTHROPIC_API_KEY env var):
#   ai_suggest_missing()        — Haiku: identify missing-value label candidates
#   ai_classify_roles()         — Haiku: classify ambiguous vars (nominal/ordinal/scale/count)
#                                  writes "order" integers (direction) to meta_json
#   ai_call_claude()            — synchronous single call (httr2)
#   ai_batch_submit()           — submit Message Batch job (httr2)
#   ai_batch_retrieve()         — poll + retrieve batch results (httr2)
#   metadata_add_level_stats()  — add level_counts/level_freqs to metadata
#   ai_suggest_labels()         — shorten labels using order field for grouping/direction
#   ai_suggest_varnames()       — propose short R variable names + doc strings
#
# Tests scripts: 
#   source("tests/testthat/test-json-roundtrip.R")
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
    ext[1],
    "sas7bdat" = ,
    "sas"      = haven::read_sas(path, catalog_file = catalog_file,
                                 encoding = encoding),
    "dta"      = haven::read_dta(path, encoding = encoding),
    "sav"      = haven::read_sav(path, encoding = encoding),
    "parquet"  = arrow::open_dataset(path, unify_schemas = TRUE) |> collect() |> tibble::as_tibble(), # arrow::read_parquet(path),
    "rds"      = readRDS(path),
    stop("Unrecognised format: '", ext[1], "'. Use sas/dta/sav/parquet/rds.")
  )

  # Replace "" with NA in all character/factor columns (incl. haven_labelled on
  # character base). Done here so downstream counts/freqs are never corrupted by
  # empty-string pseudo-values.
  # NOTE: dplyr::na_if() on haven_labelled triggers vec_cast -> validate_labelled()
  # which fails when val_labels contain duplicates. Bypass by modifying the
  # underlying values directly and restoring attributes.
  .zap_empty_str <- function(col) {
    if (is.factor(col)) {
      if ("" %in% levels(col)) {
        col[col == ""] <- NA
        col <- droplevels(col)
      }
      return(col)
    }
    # character (possibly haven_labelled): preserve all attributes
    attrs <- attributes(col)
    col_raw <- as.character(col)
    col_raw[!is.na(col_raw) & col_raw == ""] <- NA_character_
    attributes(col_raw) <- attrs
    col_raw
  }
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::where(~ is.character(.) || is.factor(.)),
    .zap_empty_str
  ))

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
# 1a-bis. parse_sas_formats() — parse SAS PROC FORMAT text file
# ============================================================

#' Parse a SAS PROC FORMAT text file to extract value labels and variable labels.
#'
#' Reads a SAS format definition file (typically produced by PROC FORMAT or
#' distributed with survey microdata) and returns value-label mappings keyed
#' by variable name.
#'
#' The file may contain two sections:
#' \itemize{
#'   \item \strong{Format definitions}: \code{;value $ FORMATf} blocks with
#'         \code{"code"="label"} pairs.
#'   \item \strong{Variable-to-format mapping}: a \code{data; set; format ...}
#'         block listing \code{VARNAME $FORMATf} associations.
#'   \item \strong{Variable labels} (optional): \code{label VARNAME="text";}
#'         statements.
#' }
#'
#' @param path   Path to the SAS format text file.
#' @param encoding  Character encoding (default \code{"UTF-8"}).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{value_labels}}{Named list: variable name → named character
#'       vector \code{c("Label" = "code", ...)} (same layout as
#'       \code{labelled::val_labels()}).}
#'     \item{\code{var_labels}}{Named character vector: variable name →
#'       variable description (from SAS \code{label} statements, if any).}
#'   }
parse_sas_formats <- function(path, encoding = "UTF-8") {
  lines <- readLines(path, encoding = encoding, warn = FALSE)

  # ------------------------------------------------------------------
  # 1. Parse format definitions:  ;value [$ ]FORMATNAMEf
  #    followed by "code"="label" lines until next ;value or end-of-section

  # ------------------------------------------------------------------
  formats <- list()            # FORMATNAMEf -> c("Label" = "code", ...)
  current_fmt  <- NULL
  current_labs <- character(0) # named: names=labels, values=codes

  for (line in lines) {
    trimmed <- trimws(line)

    # Detect format header:  ;value $ MOISf   or   ;value MOISf
    if (grepl("^;value\\s", trimmed, ignore.case = TRUE)) {
      # Save previous format block (if any)
      if (!is.null(current_fmt) && length(current_labs) > 0) {
        formats[[current_fmt]] <- current_labs
      }
      # Extract format name (after optional $)
      m <- regmatches(trimmed,
        regexec("^;value\\s+(?:\\$\\s*)?(\\S+)", trimmed, perl = TRUE))[[1]]
      if (length(m) >= 2) {
        current_fmt  <- m[2]
        current_labs <- character(0)
      } else {
        current_fmt <- NULL
      }
      next
    }

    # Collect "code"="label" pairs inside a format block
    if (!is.null(current_fmt)) {
      # Match:  "code" = "label"  (tolerant of spaces around =)
      m <- regmatches(trimmed,
        regexec('^"([^"]*)"\\s*=\\s*"([^"]*)"', trimmed, perl = TRUE))[[1]]
      if (length(m) == 3) {
        code  <- m[2]
        label <- m[3]
        current_labs[label] <- code
      }
      # A line starting with ; (but not ;value) or empty block boundary
      # terminates the current block — handled by the ;value detection above.
    }
  }
  # Flush the last format block
  if (!is.null(current_fmt) && length(current_labs) > 0) {
    formats[[current_fmt]] <- current_labs
  }

  # ------------------------------------------------------------------
  # 2. Parse variable-to-format mapping section:
  #    data; set; format  VARNAME $FORMATf  ...  ;  run;
  # ------------------------------------------------------------------
  mapping <- character(0)  # named: names=VARNAME, values=FORMATNAMEf

  # Find the "data;" line that starts the mapping section
  data_idx <- grep("^\\s*data;\\s*$", lines, ignore.case = TRUE)
  if (length(data_idx) > 0) {
    map_start <- data_idx[length(data_idx)]  # use last occurrence
    map_lines <- lines[seq(map_start, length(lines))]
    for (ml in map_lines) {
      mt <- trimws(ml)
      # Match:  VARNAME $FORMATNAMEf  (one or more per line)
      # Pattern: word chars, then $ (possibly with space), then word chars
      matches <- gregexpr(
        "([A-Za-z_][A-Za-z0-9_]*)\\s+\\$([A-Za-z_][A-Za-z0-9_]*)",
        mt, perl = TRUE)
      if (matches[[1]][1] > 0) {
        starts  <- matches[[1]]
        lengths <- attr(matches[[1]], "match.length")
        for (i in seq_along(starts)) {
          piece <- substr(mt, starts[i], starts[i] + lengths[i] - 1L)
          parts <- regmatches(piece,
            regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s+\\$([A-Za-z_][A-Za-z0-9_]*)",
                    piece, perl = TRUE))[[1]]
          if (length(parts) == 3) {
            mapping[parts[2]] <- parts[3]  # VARNAME -> FORMATNAMEf
          }
        }
      }
    }
  }

  # ------------------------------------------------------------------
  # 3. Build value_labels: VARNAME -> c("Label" = "code", ...)
  # ------------------------------------------------------------------
  value_labels <- list()

  if (length(mapping) > 0) {
    # Use mapping to link variables to formats
    for (varname in names(mapping)) {
      fmt_name <- mapping[[varname]]
      if (fmt_name %in% names(formats)) {
        value_labels[[varname]] <- formats[[fmt_name]]
      }
    }
  } else {
    # Fallback: derive variable name by stripping trailing "f" from format name
    for (fmt_name in names(formats)) {
      varname <- sub("f$", "", fmt_name)
      if (nzchar(varname)) {
        value_labels[[toupper(varname)]] <- formats[[fmt_name]]
      }
    }
  }

  # ------------------------------------------------------------------
  # 4. Parse variable labels (optional):  label VARNAME="text";
  # ------------------------------------------------------------------
  var_labels <- character(0)
  label_lines <- grep("^\\s*label\\b", lines, ignore.case = TRUE, value = TRUE)
  for (ll in label_lines) {
    # Match patterns like:  VARNAME = "description"  or  VARNAME="description"
    lm <- gregexpr(
      '([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*"([^"]*)"',
      ll, perl = TRUE)
    if (lm[[1]][1] > 0) {
      for (i in seq_along(lm[[1]])) {
        start <- lm[[1]][i]
        len   <- attr(lm[[1]], "match.length")[i]
        piece <- substr(ll, start, start + len - 1L)
        parts <- regmatches(piece,
          regexec('^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*"([^"]*)"',
                  piece, perl = TRUE))[[1]]
        if (length(parts) == 3) {
          var_labels[toupper(parts[2])] <- parts[3]
        }
      }
    }
  }

  list(value_labels = value_labels, var_labels = var_labels)
}


# ============================================================
# 1a-ter. apply_sas_labels() — apply parsed SAS labels to a tibble
# ============================================================

#' Apply SAS value labels and variable labels to a plain tibble.
#'
#' For each column that has a matching entry in \code{sas_parsed$value_labels},
#' wraps it in \code{haven_labelled} via \code{labelled::labelled()}.
#' Columns that already carry \code{haven_labelled} class are left untouched.
#'
#' Variable labels from \code{sas_parsed$var_labels} are applied only when the
#' column does not already have a \code{label} attribute.
#'
#' @param df          A tibble (plain or partially labelled).
#' @param sas_parsed  Output of \code{parse_sas_formats()}.
#'
#' @return The tibble with \code{haven_labelled} value labels and variable
#'   labels applied where applicable.
apply_sas_labels <- function(df, sas_parsed) {
  val_labs  <- sas_parsed$value_labels
  var_labs  <- sas_parsed$var_labels

  for (vname in names(df)) {
    col <- df[[vname]]

    # --- Value labels ---
    if (vname %in% names(val_labs) && !inherits(col, "haven_labelled")) {
      labs <- val_labs[[vname]]  # c("Label" = "code", ...)

      if (is.numeric(col)) {
        # Coerce string codes to numeric to match column type
        labs_num <- suppressWarnings(as.numeric(labs))
        if (!anyNA(labs_num)) {
          names(labs_num) <- names(labs)
          df[[vname]] <- labelled::labelled(col, labels = labs_num)
        }
      } else {
        # Character column: labels are already character strings
        df[[vname]] <- labelled::labelled(col, labels = labs)
      }
    }

    # --- Variable label ---
    if (vname %in% names(var_labs)) {
      existing_lbl <- attr(df[[vname]], "label", exact = TRUE)
      if (is.null(existing_lbl) || is.na(existing_lbl) || !nzchar(existing_lbl)) {
        labelled::var_label(df[[vname]]) <- var_labs[[vname]]
      }
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
.normalize_text <- function(x, to_guillemets = FALSE, sanitize = FALSE) {
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

  # 6. Sanitize for storage: replace chars that break downstream parsing
  if (sanitize) {
    x <- gsub("|",       " / ", x, fixed = TRUE)  # field separator in SET: lines
    x <- gsub('"',       "'",   x, fixed = TRUE)  # breaks JSON encoding
    x <- gsub("\t",      " ",   x, fixed = TRUE)  # TAB = API output field separator
    x <- gsub("[\r\n]+", " ",   x)                # newlines break line-oriented parsing
    x <- trimws(gsub("\\s{2,}", " ", x))          # collapse double spaces created above
  }

  x
}


# Compact JSON inside ```json ... ``` input example blocks in the system prompt.
# Input blocks are identified by containing both "var": and "levels": fields.
# Output blocks (Sortie) are left pretty-printed.
# The JSON preamble line (first line of each fence body) is preserved verbatim;
# only the array content that follows the first blank line is compacted.
# This ensures Haiku sees the same compact one-liner format in examples as in
# the real user messages it receives.
.compact_example_json_blocks <- function(text) {
  lines  <- strsplit(text, "\n", fixed = TRUE)[[1]]
  result <- character(length(lines))
  in_fence      <- FALSE
  fence_start   <- 0L
  fence_lines   <- character(0)

  flush_fence <- function(fl) {
    # Determine if this is an input block (contains both "var": and "levels":)
    body <- paste(fl, collapse = "\n")
    is_input <- grepl('"var"', body, fixed = TRUE) &&
                grepl('"levels"', body, fixed = TRUE)
    if (!is_input) return(c("```json", fl, "```"))

    # Split into: preamble line(s) before first blank line, then the rest
    blank_idx <- which(nchar(trimws(fl)) == 0L)
    if (length(blank_idx) == 0L || blank_idx[[1L]] <= 1L) {
      # No blank separator — compact everything as one block
      compacted <- .compact_json_string(paste(fl, collapse = ""))
      return(c("```json", compacted, "```"))
    }
    sep       <- blank_idx[[1L]]
    preamble  <- fl[seq_len(sep)]          # includes the blank line
    json_part <- fl[seq.int(sep + 1L, length(fl))]
    compacted <- .compact_json_string(paste(json_part, collapse = ""))
    c("```json", preamble, compacted, "```")
  }

  i <- 1L
  out <- list()
  while (i <= length(lines)) {
    ln <- lines[[i]]
    if (!in_fence && grepl("^```json", ln)) {
      in_fence    <- TRUE
      fence_lines <- character(0)
    } else if (in_fence && ln == "```") {
      in_fence <- FALSE
      out <- c(out, list(flush_fence(fence_lines)))
    } else if (in_fence) {
      fence_lines <- c(fence_lines, ln)
    } else {
      out <- c(out, list(ln))
    }
    i <- i + 1L
  }
  paste(unlist(out), collapse = "\n")
}

# Compact a JSON string: collapse whitespace outside string literals.
.compact_json_string <- function(s) {
  chars   <- strsplit(s, "", fixed = TRUE)[[1L]]
  out     <- character(length(chars))
  j       <- 0L
  in_str  <- FALSE
  escaped <- FALSE
  for (ch in chars) {
    if (escaped) {
      escaped <- FALSE
      j <- j + 1L; out[[j]] <- ch
      next
    }
    if (ch == "\\" && in_str) {
      escaped <- TRUE
      j <- j + 1L; out[[j]] <- ch
      next
    }
    if (ch == '"') {
      in_str <- !in_str
      j <- j + 1L; out[[j]] <- ch
      next
    }
    if (!in_str && ch %in% c(" ", "\t", "\n", "\r")) next
    j <- j + 1L; out[[j]] <- ch
  }
  paste(out[seq_len(j)], collapse = "")
}

.clean_var_label_for_api <- function(var_label, var_name = NULL) {
  # Strip Stata-style variable name prefix — redundant since the variable name
  # is already the JSON key / SET: id field.
  # Examples stripped: "Q19E_age. ", "EMP10_annee. ", "Q2. ", "NET3. "
  # When var_name is supplied, match specifically against it (case-insensitive,
  # allowing a lowercase suffix like "_age" or "_annee" after the base name).
  # This avoids stripping ordinary French words that happen to start a label.
  if (!is.null(var_name) && nzchar(var_name)) {
    pat <- paste0("(?i)^", gsub("([.^$*+?|(){}\\[\\]\\\\])", "\\\\\\1", var_name),
                  "[a-z0-9_]*[.:]?\\s+")
    sub(pat, "", var_label, perl = TRUE)
  } else {
    # Fallback: generic pattern (only when var_name unavailable)
    sub("^[A-Za-z][A-Za-z0-9_]*[0-9]*[a-z]?[0-9]*[._]?\\s+", "", var_label)
  }
}


# Formats one SET: line for the ai_classify_roles() prompt.
# Used for both real input lines and system-prompt examples so their format is identical.
# [] and {} inside labels are preserved; only round parentheses are stripped.
.format_classify_jsonl <- function(var_name, var_label, detected_role,
                                    labels, values, missing_vals,
                                    n_distinct, max_labels = 5L) {
  cur_role <- switch(detected_role,
    factor_binary  = "factor_binary",
    factor_nominal = "factor_nominal",
    integer        = "integer",
    "unknown")

  is_miss_lbl <- as.character(values) %in% as.character(missing_vals)
  non_miss    <- .normalize_text(labels[!is_miss_lbl & nzchar(labels)])

  strip_parens <- function(x, max_chars = 50L) {
    for (i in seq_len(3)) x <- gsub("\\([^()]*\\)", "", x)
    x <- trimws(gsub("\\s{2,}", " ", x))
    substr(x, 1L, max_chars)
  }

  shown   <- strip_parens(head(non_miss, max_labels))
  n_total <- length(non_miss)
  if (n_total > max_labels) shown <- c(shown, paste0("+", n_total - max_labels, " more"))

  obj <- list(
    id     = var_name,
    label  = substr(.clean_var_label_for_api(var_label, var_name = var_name), 1L, 55L),
    cur    = cur_role,
    nd     = as.integer(n_distinct),
    levels = shown
  )

  as.character(jsonlite::toJSON(obj, auto_unbox = TRUE))
}


# ---------------------------------------------------------------------------
# Parse a json_example block from the classify_roles prompt .md file.
# Input: raw JSON text from inside a ```json_example fenced block.
# Returns a list of example objects, one per variable in the JSON.
# Each object has $input_args (for .format_classify_jsonl()) and $expected
# (a JSONL output string like '{"id":"X","role":"factor_ordinal","desc":"high_first"}').
# ---------------------------------------------------------------------------
.parse_json_example_block <- function(json_text, ordinal_desc = TRUE) {
  parsed <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  if (!is.list(parsed) || length(parsed) == 0) return(list())

  # Map cur code (from source JSON) to detected_role for .format_classify_jsonl()
  cur_to_role <- c(
    factor_nominal = "factor_nominal", factor_binary = "factor_binary",
    F = "factor_nominal", B = "factor_binary",
    I = "integer", "?" = "unknown"
  )

  purrr::imap(parsed, function(var_def, var_name) {
    var_label <- var_def$var_label %||% ""
    role      <- var_def$role %||% "factor_nominal"
    cur       <- var_def$cur %||% "factor_nominal"
    levs      <- var_def$levels %||% list()

    # Separate missing from non-missing levels
    lev_keys    <- names(levs)
    is_miss     <- purrr::map_lgl(levs, ~ isTRUE(.x$missing) || isTRUE(.x$null_coded))
    miss_keys   <- lev_keys[is_miss]
    valid_keys  <- lev_keys[!is_miss]
    valid_levs  <- levs[valid_keys]

    # Sort non-missing levels by order field (ascending) if present
    orders <- purrr::map_int(valid_levs, function(lv) {
      as.integer(lv$order %||% NA_integer_)
    })
    if (!all(is.na(orders))) {
      sort_idx   <- order(orders, na.last = TRUE)
      valid_keys <- valid_keys[sort_idx]
      valid_levs <- valid_levs[sort_idx]
      orders     <- orders[sort_idx]
    }

    labels_vec <- unname(purrr::map_chr(valid_levs, ~ .x$label %||% ""))
    n_distinct <- length(valid_keys)

    # Determine direction: explicit "desc" field overrides inference from order
    dir_code <- ""
    explicit_desc <- var_def$desc %||% ""
    if (nzchar(explicit_desc) && explicit_desc %in%
        c("high_first", "low_first", "unknown")) {
      dir_code <- explicit_desc
    } else if (role %in% c("factor_ordinal", "factor_binary") &&
               n_distinct >= 2 && !all(is.na(orders))) {
      first_order <- orders[[1]]
      last_order  <- orders[[n_distinct]]
      if (!is.na(first_order) && !is.na(last_order)) {
        if (first_order < last_order) {
          dir_code <- "high_first"   # order=1 (top rank) is first shown
        } else if (first_order > last_order) {
          dir_code <- "low_first"    # order=N (bottom rank) is first shown
        }
      }
    }

    # Build expected output as JSONL string
    out_obj <- list(id = var_name, role = role)
    if (nzchar(dir_code) && (role == "factor_binary" || ordinal_desc)) {
      out_obj$desc <- dir_code
    }
    expected <- as.character(jsonlite::toJSON(out_obj, auto_unbox = TRUE))

    # Build values vector: use numeric codes if they look numeric, else character
    values_vec <- if (all(grepl("^[0-9]+$", c(valid_keys, miss_keys)))) {
      as.integer(c(valid_keys, miss_keys))
    } else {
      seq_along(c(valid_keys, miss_keys))
    }

    # All labels including missing (for .format_classify_jsonl)
    miss_labels <- unname(purrr::map_chr(levs[miss_keys], ~ .x$label %||% ""))
    all_labels  <- c(labels_vec, miss_labels)
    all_values  <- values_vec

    # Missing vals: use the values corresponding to missing keys
    miss_values <- if (length(miss_keys) > 0) {
      tail(all_values, length(miss_keys))
    } else {
      integer(0)
    }

    # Map cur to detected_role
    detected_role <- unname(cur_to_role[cur])
    if (is.na(detected_role)) detected_role <- "factor_nominal"

    list(
      input_args = list(
        var_name      = var_name,
        var_label     = var_label,
        detected_role = detected_role,
        labels        = all_labels,
        values        = all_values,
        missing_vals  = miss_values,
        n_distinct    = as.integer(n_distinct)
      ),
      expected = expected
    )
  })
}


# ---------------------------------------------------------------------------
# Build the classify_roles system prompt from the external .md file.
# Processes conditional sections (ordinal_desc) and replaces json_example
# blocks with formatted JSONL Input/Output pairs using .format_classify_jsonl().
# ---------------------------------------------------------------------------
.build_classify_system_prompt <- function(prompt_path, ordinal_desc,
                                          max_labels_sent = 5L) {
  md_text <- paste(
    readLines(prompt_path, encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  )

  # --- Process conditional sections ---
  if (ordinal_desc) {
    # Remove "IF NOT ordinal_desc" blocks, keep "IF ordinal_desc" content
    md_text <- gsub(
      "<!-- IF NOT ordinal_desc -->[\\s\\S]*?<!-- ENDIF NOT ordinal_desc -->",
      "", md_text, perl = TRUE
    )
    md_text <- gsub(
      "<!-- IF ordinal_desc -->|<!-- ENDIF ordinal_desc -->",
      "", md_text
    )
  } else {
    # Remove "IF ordinal_desc" blocks, keep "IF NOT ordinal_desc" content
    md_text <- gsub(
      "<!-- IF ordinal_desc -->[\\s\\S]*?<!-- ENDIF ordinal_desc -->",
      "", md_text, perl = TRUE
    )
    md_text <- gsub(
      "<!-- IF NOT ordinal_desc -->|<!-- ENDIF NOT ordinal_desc -->",
      "", md_text
    )
  }

  # --- Replace json blocks (with "var_label") with formatted JSONL Input/Output pairs ---
  block_pattern <- "```json\\s*\\n([\\s\\S]*?)```\\s*\\n?"
  matches <- gregexpr(block_pattern, md_text, perl = TRUE)[[1]]

  if (matches[[1]] != -1L) {
    match_starts  <- as.integer(matches)
    match_lengths <- attr(matches, "match.length")

    # Process in reverse order to preserve positions
    for (i in rev(seq_along(match_starts))) {
      full_match <- substr(md_text, match_starts[[i]],
                           match_starts[[i]] + match_lengths[[i]] - 1L)
      # Extract JSON content
      json_text <- sub(
        "```json\\s*\\n([\\s\\S]*?)```\\s*\\n?", "\\1",
        full_match, perl = TRUE
      )

      # Only process blocks that contain "var_label" (classify example blocks)
      if (!grepl('"var_label"', json_text, fixed = TRUE)) next

      examples <- tryCatch(
        .parse_json_example_block(json_text, ordinal_desc = ordinal_desc),
        error = function(e) {
          warning("Failed to parse json example block: ", e$message)
          list()
        }
      )

      if (length(examples) > 0) {
        formatted <- purrr::map_chr(examples, function(ex) {
          args <- ex$input_args
          input_line <- .format_classify_jsonl(
            var_name      = args$var_name,
            var_label     = args$var_label,
            detected_role = args$detected_role,
            labels        = args$labels,
            values        = args$values,
            missing_vals  = args$missing_vals,
            n_distinct    = args$n_distinct,
            max_labels    = max_labels_sent
          )
          paste0("Input:  ", input_line, "\nOutput: ", ex$expected)
        })
        replacement <- paste0(paste(formatted, collapse = "\n\n"), "\n")

        md_text <- paste0(
          substr(md_text, 1L, match_starts[[i]] - 1L),
          replacement,
          substr(md_text, match_starts[[i]] + match_lengths[[i]], nchar(md_text))
        )
      }
    }
  }

  # Clean up excess blank lines
  md_text <- gsub("\n{3,}", "\n\n", md_text)

  md_text
}


# ============================================================
# 1c. Unified survey_meta.json helpers
# ============================================================

# ---------------------------------------------------------------------------
# Back up the current meta_json before overwriting it.
# Creates .survey_meta/ directory next to path if absent.
# Names: {stem}_{YYYYMMDD_HHMM}_{step}.json  (appends _2, _3 if already exists).
# "_survey_meta" in stem is shortened to "_meta" for cleaner filenames.
.backup_meta_json <- function(path, step) {
  if (!file.exists(path)) return(invisible(NULL))
  dir_path  <- dirname(path)
  stem      <- tools::file_path_sans_ext(basename(path))
  stem      <- sub("_survey_meta$", "_meta", stem)
  backup_dir <- file.path(dir_path, ".survey_meta")
  if (!dir.exists(backup_dir))
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

  date_str  <- format(Sys.time(), "%Y%m%d_%H%M")
  base_name <- paste0(stem, "_", date_str, "_", step, ".json")
  dest      <- file.path(backup_dir, base_name)
  suffix    <- 2L
  while (file.exists(dest)) {
    dest <- file.path(backup_dir,
                      paste0(stem, "_", date_str, "_", step, "_", suffix, ".json"))
    suffix <- suffix + 1L
  }
  file.copy(path, dest, overwrite = FALSE)
  invisible(dest)
}

# ---------------------------------------------------------------------------
# Read the unified survey_meta.json, auto-migrating v3 format (desc/null_coded)
# to v4 format (order/missing) if needed.
# Returns list(config = list(), variables = list()) if file absent or malformed.
.read_meta_json <- function(path) {
  empty <- list(config = list(), variables = list())
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(empty)
  data <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      warning(".read_meta_json: could not parse '", path, "': ", conditionMessage(e))
      empty
    }
  )
  # Auto-migrate v3 → v4 if any variable still uses desc/null_coded
  result <- .migrate_meta_json_v3_to_v4(data, path)
  result
}

# ---------------------------------------------------------------------------
# Migrate v3 JSON format (desc boolean + null_coded) to v4 (order integer + missing).
# Idempotent: if "order" already exists on a level, it is left unchanged.
# Writes the migrated JSON back to disk (with backup) only if changes were made.
.migrate_meta_json_v3_to_v4 <- function(data, path) {
  vars <- data$variables
  if (is.null(vars) || length(vars) == 0) return(data)

  n_migrated <- 0L
  for (vname in names(vars)) {
    entry  <- vars[[vname]]
    levels <- entry$levels
    if (is.null(levels) || length(levels) == 0) next

    has_v3 <- any(purrr::map_lgl(levels, ~ !is.null(.x$null_coded))) ||
              !is.null(entry$desc)
    # Check if any level still lacks "order" and is not missing
    any_needs_order <- any(purrr::map_lgl(levels, function(lev)
      !isTRUE(lev$missing) && is.null(lev$null_coded) && is.null(lev$order)))

    if (!has_v3 && !any_needs_order) next

    # Determine direction from v3 desc field
    desc_val <- entry$desc  # TRUE / FALSE / NULL
    role     <- entry$role

    # Count non-missing levels (those that are neither null_coded nor already missing)
    non_miss_codes <- names(levels)[!purrr::map_lgl(levels, function(lev)
      isTRUE(lev$null_coded) || isTRUE(lev$missing))]
    n_valid <- length(non_miss_codes)

    # Assign sequential order to non-missing levels; apply direction from desc
    seq_orders <- seq_len(n_valid)
    if (isTRUE(desc_val) && identical(role, "factor_ordinal") && n_valid >= 2) {
      # Descending: reverse sequential
      seq_orders <- rev(seq_orders)
    }

    valid_idx <- 0L
    for (val_code in names(levels)) {
      lev <- levels[[val_code]]
      # Migrate null_coded → missing
      if (isTRUE(lev$null_coded)) {
        vars[[vname]]$levels[[val_code]]$null_coded <- NULL
        vars[[vname]]$levels[[val_code]]$missing    <- TRUE
        next
      }
      # Skip levels already flagged as missing (no order assigned to them)
      if (isTRUE(lev$missing)) next
      # Assign order if absent
      if (is.null(lev$order)) {
        valid_idx <- valid_idx + 1L
        ord_val   <- seq_orders[[valid_idx]]
        # For binary: apply desc → swap orders (positive gets 1)
        if (identical(role, "factor_binary") && !is.null(desc_val)) {
          if (isTRUE(desc_val) && valid_idx == 1L) ord_val <- 1L
          if (isTRUE(desc_val) && valid_idx == 2L) ord_val <- 2L
          if (isFALSE(desc_val) && valid_idx == 1L) ord_val <- 2L
          if (isFALSE(desc_val) && valid_idx == 2L) ord_val <- 1L
        }
        vars[[vname]]$levels[[val_code]]$order <- ord_val
      }
    }

    # Remove v3 desc field from variable block
    vars[[vname]]$desc <- NULL
    n_migrated <- n_migrated + 1L
  }

  if (n_migrated == 0L) return(data)

  # Write migrated JSON back with backup
  data$variables <- vars
  .backup_meta_json(path, "migrate_v4")
  .write_meta_json(data, path)
  message("extract_survey_metadata: migrated ", n_migrated,
          " variable(s) from v3 JSON format (null_coded/desc \u2192 order/missing) in ",
          basename(path))
  data
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
  scalar_fields <- c("var_label", "role", "r_class", "new_name")
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

    # -- levels sub-block ------------------------------------------------------
    levels <- entry$levels
    if (is.null(levels) || length(levels) == 0) {
      levels_body <- '      "levels": {}'
    } else {
      n_lev <- length(levels)
      # is_missing: level with missing: true (suppressed/non-response)
      is_missing_vec <- purrr::map_lgl(levels, ~ isTRUE(.x[["missing"]]))
      has_order     <- any(purrr::map_lgl(levels, ~ !is.null(.x[["order"]])))
      has_new_label <- any(purrr::map_lgl(levels, ~ !isTRUE(.x[["missing"]]) && !is.null(.x[["new_label"]])))
      has_n         <- any(purrr::map_lgl(levels, ~ !is.null(.x[["n"]])))
      has_pct       <- any(purrr::map_lgl(levels, ~ !is.null(.x[["pct"]])))

      val_keys  <- names(levels)
      f_key     <- paste0('"', purrr::map_chr(val_keys, esc), '"')
      f_label   <- purrr::map_chr(levels, function(lev)
                     paste0('"', esc(as.character(lev[["label"]])), '"'))
      # order column: integer for valid levels, blank for missing levels
      f_order   <- if (has_order) purrr::map_chr(levels, function(lev)
                     if (!isTRUE(lev[["missing"]]) && !is.null(lev[["order"]]))
                       as.character(as.integer(lev[["order"]]))
                     else "") else NULL
      f_new_lbl <- if (has_new_label) purrr::map_chr(levels, function(lev)
                     if (!isTRUE(lev[["missing"]]) && !is.null(lev[["new_label"]]))
                       paste0('"', esc(as.character(lev[["new_label"]])), '"')
                     else '""') else NULL
      f_n       <- if (has_n) purrr::map_chr(levels, function(lev) {
                     v <- lev[["n"]]
                     if (!is.null(v) && length(v) == 1L && !is.na(v)) as.character(as.integer(v)) else ""
                   }) else NULL
      f_pct     <- if (has_pct) purrr::map_chr(levels, function(lev) {
                     v <- lev[["pct"]]
                     if (!is.null(v) && length(v) == 1L && !is.na(v)) as.character(as.integer(v)) else ""
                   }) else NULL

      w_key   <- max(nchar(f_key,   type = "chars"), na.rm = TRUE)
      # order column width: max of non-empty order strings (right-aligned integers)
      w_order <- if (has_order) {
        non_empty <- f_order[nzchar(f_order)]
        if (length(non_empty) > 0) max(nchar(non_empty, type = "chars")) else 1L
      } else 0L
      w_label <- max(nchar(f_label, type = "chars"), na.rm = TRUE)
      w_new   <- if (has_new_label) max(nchar(f_new_lbl, type = "chars"), na.rm = TRUE) else 0L
      w_n     <- if (has_n) { ne <- f_n[nzchar(f_n)];     if (length(ne) > 0) max(nchar(ne,  type = "chars")) else 1L } else 0L
      w_pct   <- if (has_pct) { ne <- f_pct[nzchar(f_pct)]; if (length(ne) > 0) max(nchar(ne,  type = "chars")) else 1L } else 0L

      level_lines <- character(n_lev)
      for (i in seq_len(n_lev)) {
        lev     <- levels[[i]]
        is_miss <- is_missing_vec[[i]]
        tokens  <- character(0)
        # "order" before "label" — right-aligned integer; only for non-missing levels
        if (has_order && !is_miss && !is.null(lev[["order"]])) {
          ord_str <- formatC(f_order[[i]], width = w_order, flag = " ")
          tokens  <- c(tokens, paste0('"order": ', ord_str))
        }
        # "missing": true before "label" for missing levels
        if (is_miss) tokens <- c(tokens, '"missing": true')
        tokens <- c(tokens, paste0('"label": ', rpad(f_label[[i]], w_label)))
        # new_label: only for non-missing levels
        if (has_new_label && !is_miss)
          tokens <- c(tokens, paste0('"new_label": ', rpad(f_new_lbl[[i]], w_new)))
        # n / pct: only for non-missing levels
        if (!is_miss) {
          if (has_n && !is.null(lev[["n"]]))
            tokens <- c(tokens, paste0('"n": ',   formatC(f_n[[i]],   width = w_n,   flag = " ")))
          if (has_pct && !is.null(lev[["pct"]]))
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
    '      "variables.VAR.var_label"              : "Intitul\u00e9 original de la question dans le questionnaire (peut \u00eatre modifi\u00e9 pour la documentation)",\n',
    '      "variables.VAR.role"                   : "Type de variable : factor_binary = binaire, factor_ordinal = ordinale (ordre significatif), factor_nominal = nominale (cat\u00e9gories sans ordre), integer_scale = \u00e9chelle num\u00e9rique, integer_count = comptage, double = continu, identifier = identifiant, integer = entier, other = autre",\n',
    '      "variables.VAR.new_name"               : "Nom de la variable dans le fichier de donn\u00e9es final \u2014 c\'est ce nom qu\'il faut utiliser pour conseiller les \u00e9tudiant\u00b7es",\n',
    '      "variables.VAR.doc_note"               : "Note de documentation libre sur la variable (ajout\u00e9e manuellement)",\n',
    '      "variables.VAR.levels.CODE.order"      : "Entier \u22651 = position de cette modalit\u00e9 dans le facteur de sortie (ordre croissant). Modalit\u00e9s partageant le m\u00eame entier sont fusionn\u00e9es avant la suggestion de libell\u00e9s (IA). Binaire : 1 = modalit\u00e9 positive (Oui\u2026), 2 = n\u00e9gative. Absent pour les valeurs manquantes.",\n',
    '      "variables.VAR.levels.CODE.missing"    : "true = cette modalit\u00e9 est une non-r\u00e9ponse ou valeur manquante, exclue de l\'analyse. Pas de champ \'order\' dans ce cas.",\n',
    '      "variables.VAR.levels.CODE.label"      : "Libell\u00e9 original de cette modalit\u00e9 de r\u00e9ponse",\n',
    '      "variables.VAR.levels.CODE.new_label"  : "Libell\u00e9 court pour l\'affichage dans les tableaux (sugg\u00e9r\u00e9 par l\'IA, modifiable)",\n',
    '      "variables.VAR.levels.CODE.n"          : "Nombre de r\u00e9pondant\u00b7es ayant choisi cette modalit\u00e9",\n',
    '      "variables.VAR.levels.CODE.pct"        : "Pourcentage de r\u00e9pondant\u00b7es ayant choisi cette modalit\u00e9 (hors valeurs manquantes)"\n',
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
# 1d. Nomenclatures INSEE helpers
# ============================================================

# ---------------------------------------------------------------------------
# Read nomenclatures_INSEE.json.
# Returns list(nomenclatures = list()) if absent or malformed.
.read_nomenclatures_json <- function(path) {
  empty <- list(nomenclatures = list())
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(empty)
  tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      warning(".read_nomenclatures_json: could not parse '", path, "': ",
              conditionMessage(e))
      empty
    }
  )
}

# ---------------------------------------------------------------------------
# Write nomenclatures_INSEE.json with padded alignment.
# nom_list: list with keys "_schema" and "nomenclatures"
# Each nomenclature entry: list(var_label, source, version, levels = list(CODE = list(label)))
.write_nomenclatures_json <- function(nom_list, path) {
  esc <- function(s) {
    s <- as.character(s)
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    gsub('"', '\\"', s, fixed = TRUE)
  }
  rpad <- function(s, w) {
    n <- nchar(s, type = "chars")
    if (n < w) paste0(s, strrep(" ", w - n)) else s
  }

  lines <- character(0)
  lines <- c(lines, "{")

  # _schema block
  schema <- nom_list[["_schema"]]
  if (!is.null(schema)) {
    lines  <- c(lines, '  "_schema": {')
    desc   <- schema[["description"]]
    fields <- schema[["fields"]]
    has_fields <- !is.null(fields) && length(fields) > 0
    if (!is.null(desc)) {
      comma_desc <- if (has_fields) "," else ""
      lines <- c(lines, paste0('    "description": "', esc(desc), '"', comma_desc))
    }
    if (has_fields) {
      field_keys  <- names(fields)
      fw <- max(nchar(field_keys)) + 4L  # +4 for quotes + colon
      field_lines <- vapply(seq_along(fields), function(i) {
        k <- field_keys[[i]]
        v <- fields[[k]]
        comma <- if (i < length(fields)) "," else ""
        paste0('    ', rpad(paste0('"', k, '"'), fw), ': "', esc(v), '"', comma)
      }, character(1))
      lines <- c(lines, '    "fields": {')
      lines <- c(lines, field_lines)
      lines <- c(lines, '    }')
    }
    lines <- c(lines, '  },')
  }

  # nomenclatures block
  nom_entries <- nom_list[["nomenclatures"]]
  if (is.null(nom_entries)) nom_entries <- list()
  nom_names <- names(nom_entries)

  lines <- c(lines, '  "nomenclatures": {')

  for (ni in seq_along(nom_names)) {
    nom_id  <- nom_names[[ni]]
    nom     <- nom_entries[[nom_id]]
    is_last_nom <- (ni == length(nom_names))

    meta_fields <- c("var_label", "source", "version")
    mw <- max(nchar(meta_fields)) + 4L
    meta_lines <- vapply(meta_fields, function(f) {
      v <- nom[[f]]
      if (is.null(v)) return(NULL)
      paste0('      ', rpad(paste0('"', f, '"'), mw), ': "', esc(v), '",')
    }, character(1))
    meta_lines <- meta_lines[!vapply(meta_lines, is.null, logical(1))]

    # levels block
    lvls <- nom[["levels"]]
    if (is.null(lvls)) lvls <- list()
    lvl_codes <- names(lvls)

    # compute padding for level labels
    lbl_width <- if (length(lvl_codes) > 0)
      max(nchar(lvl_codes)) + 4L
    else 10L

    lvl_lines <- character(0)
    for (li in seq_along(lvl_codes)) {
      code     <- lvl_codes[[li]]
      lbl      <- lvls[[code]][["label"]]
      is_last  <- (li == length(lvl_codes))
      comma    <- if (is_last) "" else ","
      lvl_lines <- c(lvl_lines,
        paste0('        ', rpad(paste0('"', esc(code), '"'), lbl_width),
               ': { "label": "', esc(lbl %||% ""), '" }', comma))
    }

    comma_nom <- if (is_last_nom) "" else ","
    lines <- c(lines, paste0('    "', esc(nom_id), '": {'))
    lines <- c(lines, meta_lines)
    lines <- c(lines, '      "levels": {')
    lines <- c(lines, lvl_lines)
    lines <- c(lines, '      }')
    lines <- c(lines, paste0('    }', comma_nom))
  }

  lines <- c(lines, '  }')
  lines <- c(lines, '}')

  json_str <- paste(lines, collapse = "\n")
  json_str <- paste0(json_str, "\n")

  dir_p <- dirname(path)
  if (!dir.exists(dir_p))
    dir.create(dir_p, recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(json_str), con = path, useBytes = TRUE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Parse NAF Rev.2 XLS file and return a named list(CODE = list(label = "...")).
# Only retains sous-classes (pattern "01.11Z") — 732 postes.
# Codes are normalized to EE format: "01.11Z" -> "0111Z".
.parse_naf_rev2 <- function(naf_path) {
  naf <- readxl::read_xls(naf_path, col_names = TRUE)
  # Column 4 = 65-char labels
  col_label <- names(naf)[[4]]
  pattern   <- "^[0-9]{2}[.][0-9]{2}[A-Z]$"
  rows      <- !is.na(naf$Code) & grepl(pattern, naf$Code)
  sub_naf   <- naf[rows, ]
  codes     <- gsub("[.]", "", sub_naf$Code)
  labels    <- .normalize_text(as.character(sub_naf[[col_label]]), sanitize = TRUE)
  stats::setNames(lapply(labels, function(l) list(label = l)), codes)
}

# ---------------------------------------------------------------------------
# Parse PCS 2020 Excel and return a named list for a given level (3 or 4).
# Code transformation: strip last character (trailing "0") to match EE format.
# For level=3: "38A0" -> "38A", "3800" -> "380"
# For level=4: "10A1" -> kept as-is (EE codes are identical to N4 minus nothing)
# Actually N4 codes are already the final digit: "10A1" matches EE "10A1".
# N3 codes end in "0" and are stripped.
.parse_pcs2020 <- function(pcs_path, level) {
  pcs <- readxl::read_xlsx(pcs_path, col_names = TRUE)
  col_niveau <- names(pcs)[[1]]   # "Niveau"
  col_code   <- names(pcs)[[2]]   # "code PCS2020"
  col_label  <- names(pcs)[[3]]   # "Libellé long de la nomenclature"
  rows  <- !is.na(pcs[[col_niveau]]) & pcs[[col_niveau]] == level
  sub   <- pcs[rows, ]
  codes <- as.character(sub[[col_code]])
  if (level == 3) {
    # Strip trailing character (always "0") to get EE code format
    codes <- substr(codes, 1L, nchar(codes) - 1L)
  }
  labels <- .normalize_text(as.character(sub[[col_label]]), sanitize = TRUE)
  stats::setNames(lapply(labels, function(l) list(label = l)), codes)
}

# ---------------------------------------------------------------------------
# Parse FAP 2021 DARES Excel and return levels for a given FAP level (22/86/228/341).
.parse_fap2021 <- function(fap_path, fap_level = 341) {
  fap   <- readxl::read_xlsx(fap_path, sheet = "niveaux_emboités", col_names = TRUE)
  col_code  <- paste0("Code_FAP", fap_level)
  col_label <- paste0("Intitul\u00e9_FAP", fap_level)
  if (!col_code %in% names(fap))
    stop("Column '", col_code, "' not found in FAP file.")
  # Deduplicate (for levels 22/86/228 which repeat across rows)
  codes  <- as.character(fap[[col_code]])
  labels <- .normalize_text(as.character(fap[[col_label]]), sanitize = TRUE)
  df     <- unique(data.frame(code = codes, label = labels, stringsAsFactors = FALSE))
  df     <- df[!is.na(df$code), ]
  stats::setNames(lapply(df$label, function(l) list(label = l)), df$code)
}

# ---------------------------------------------------------------------------
# Create the nomenclatures_INSEE.json from the three Excel source files.
# If the file already exists, it is NEVER overwritten — a message is shown instead.
# naf_path: path to int_courts_naf_rev_2.xls
# fap_path: path to Dares_Arborescence_FAP2021.xlsx
# pcs_path: path to Nomenclature_4Nemboites_PCS2020.xlsx
# path:     output JSON path (default "instructions/nomenclatures_INSEE.json")
create_nomenclatures_json <- function(
    naf_path,
    fap_path,
    pcs_path,
    path = "instructions/nomenclatures_INSEE.json"
) {
  if (file.exists(path)) {
    message("Fichier d\u00e9j\u00e0 existant, non \u00e9cras\u00e9 : ", path,
            "\nUtiliser add_nomenclature_to_json() pour ajouter ou mettre \u00e0 jour une nomenclature.")
    return(invisible(path))
  }

  message("Lecture des fichiers source...")

  nom_list <- list(
    `_schema` = list(
      description = paste0(
        "Nomenclatures de r\u00e9f\u00e9rence INSEE / DARES pour les enqu\u00eates fran\u00e7aises. ",
        "Utilis\u00e9 par apply_nomenclatures() pour enrichir les labels des variables cod\u00e9es. ",
        "Ce fichier peut \u00eatre compl\u00e9t\u00e9 manuellement avec add_nomenclature_to_json()."
      ),
      fields = list(
        `nomenclatures.ID.var_label` = "Intitul\u00e9 complet de la nomenclature",
        `nomenclatures.ID.source`    = "Organisme producteur (INSEE, DARES...)",
        `nomenclatures.ID.version`   = "Version ou mill\u00e9sime de la nomenclature",
        `nomenclatures.ID.levels.CODE.label` = "Libell\u00e9 officiel du code"
      )
    ),
    nomenclatures = list()
  )

  # NAF Rev.2 (732 sous-classes)
  message("  NAF Rev.2 (732 postes)...")
  nom_list$nomenclatures[["NAF_rev2"]] <- list(
    var_label = "Nomenclature d'Activit\u00e9s Fran\u00e7aises R\u00e9v.2 (sous-classes, 732 postes)",
    source    = "INSEE",
    version   = "R\u00e9v.2 (2008)",
    levels    = .parse_naf_rev2(naf_path)
  )
  message("    -> ", length(nom_list$nomenclatures[["NAF_rev2"]]$levels), " codes")

  # PCS 2020 N3
  message("  PCS 2020 niveau 3...")
  nom_list$nomenclatures[["PCS2020_N3"]] <- list(
    var_label = "Professions et Cat\u00e9gories Socioprofessionnelles 2020 \u2013 niveau 3",
    source    = "INSEE",
    version   = "PCS 2020",
    levels    = .parse_pcs2020(pcs_path, level = 3)
  )
  message("    -> ", length(nom_list$nomenclatures[["PCS2020_N3"]]$levels), " codes")

  # PCS 2020 N4
  message("  PCS 2020 niveau 4...")
  nom_list$nomenclatures[["PCS2020_N4"]] <- list(
    var_label = "Professions et Cat\u00e9gories Socioprofessionnelles 2020 \u2013 niveau 4",
    source    = "INSEE",
    version   = "PCS 2020",
    levels    = .parse_pcs2020(pcs_path, level = 4)
  )
  message("    -> ", length(nom_list$nomenclatures[["PCS2020_N4"]]$levels), " codes")

  # FAP 2021 (all 4 levels)
  for (fap_lv in c(341L, 228L, 86L, 22L)) {
    key <- paste0("FAP2021_", fap_lv)
    message("  FAP 2021 niveau ", fap_lv, "...")
    nom_list$nomenclatures[[key]] <- list(
      var_label = paste0("Familles professionnelles FAP-2021 (", fap_lv, " postes)"),
      source    = "DARES",
      version   = "FAP 2021",
      levels    = .parse_fap2021(fap_path, fap_level = fap_lv)
    )
    message("    -> ", length(nom_list$nomenclatures[[key]]$levels), " codes")
  }

  message("  \u00c9criture de ", path, "...")
  .write_nomenclatures_json(nom_list, path)
  message("Fichier cr\u00e9\u00e9 : ", path)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Add (or replace) a nomenclature entry in the JSON file from a data.frame.
# df_codes: data.frame with columns $code and $label.
# If nomenclature_id already exists, a warning is shown and it is replaced.
add_nomenclature_to_json <- function(
    nomenclature_id,
    df_codes,
    path      = "instructions/nomenclatures_INSEE.json",
    var_label = nomenclature_id,
    source    = "INSEE",
    version   = ""
) {
  if (!is.data.frame(df_codes) || !all(c("code", "label") %in% names(df_codes)))
    stop("df_codes must be a data.frame with columns 'code' and 'label'.")

  nom_list <- .read_nomenclatures_json(path)
  if (is.null(nom_list$nomenclatures)) nom_list$nomenclatures <- list()

  if (nomenclature_id %in% names(nom_list$nomenclatures))
    warning("add_nomenclature_to_json: nomenclature '", nomenclature_id,
            "' d\u00e9j\u00e0 pr\u00e9sente, remplac\u00e9e.")

  # Build levels list
  df_codes <- df_codes[!is.na(df_codes$code) & nzchar(df_codes$code), ]
  lvls <- stats::setNames(
    lapply(as.character(df_codes$label), function(l) list(label = l)),
    as.character(df_codes$code)
  )

  nom_list$nomenclatures[[nomenclature_id]] <- list(
    var_label = var_label,
    source    = source,
    version   = version,
    levels    = lvls
  )

  .backup_meta_json(path, step = "add_nomenclature")
  .write_nomenclatures_json(nom_list, path)
  message("Nomenclature '", nomenclature_id, "' ajout\u00e9e dans ", path,
          " (", nrow(df_codes), " codes).")
  invisible(path)
}

# ---------------------------------------------------------------------------
# Detect which variables in metadata are likely encoded with a standard INSEE
# nomenclature, based on regex patterns applied to the values list-column.
# Returns a named list suitable for the `mapping` argument of apply_nomenclatures().
# metadata: tibble returned by extract_survey_metadata()
detect_nomenclature_vars <- function(metadata) {
  # Regex patterns per nomenclature key
  # Tested against the majority of non-NA, non-missing values of each variable
  patterns <- list(
    FAP2021_341 = "^[A-Z][0-9][A-Z][0-9]{2}[a-z]?$",
    NAF_rev2    = "^[0-9]{4}[A-Z]$",
    NAF_129N    = "^[A-Z][0-9]{2}[A-Z]$",
    NAF_38N     = "^[A-Z]{2}$",
    PCS2020_N4  = "^[0-9]{2}[A-Z][0-9]$",
    PCS2020_N3  = "^([0-9]{2}[A-Z]|[0-9]{3})$"
  )

  mapping <- list()

  for (i in seq_len(nrow(metadata))) {
    vname  <- metadata$var_name[[i]]
    vals   <- metadata$values[[i]]
    m_vals <- metadata$missing_vals[[i]]
    if (is.null(vals) || length(vals) == 0) next

    # Exclude missing-coded values
    non_missing <- setdiff(vals, m_vals)
    non_missing <- non_missing[!is.na(non_missing) & nzchar(non_missing)]
    if (length(non_missing) == 0) next

    # Test each pattern: count how many values match
    # Use a sample for speed (max 200 values)
    sample_vals <- if (length(non_missing) > 200)
      non_missing[seq(1, length(non_missing), length.out = 200)]
    else non_missing

    for (nom_key in names(patterns)) {
      frac <- mean(grepl(patterns[[nom_key]], sample_vals))
      if (frac >= 0.8) {
        mapping[[vname]] <- nom_key
        break  # first matching pattern wins (ordered from most to least specific)
      }
    }
  }

  if (length(mapping) == 0)
    message("detect_nomenclature_vars: aucune variable candidate d\u00e9tect\u00e9e.")
  else
    message("detect_nomenclature_vars: ", length(mapping), " variable(s) d\u00e9tect\u00e9e(s) :\n",
            paste0("  ", names(mapping), " -> ", unlist(mapping), collapse = "\n"))

  mapping
}

# ---------------------------------------------------------------------------
# Apply reference nomenclature labels to the metadata table.
# mapping: named list(VAR_NAME = "NOMENCLATURE_ID") — use detect_nomenclature_vars()
#          to generate it automatically.
# nom_json: path to nomenclatures_INSEE.json
# meta_json: if provided, updated labels are written back to the survey_meta.json
# dry_run: if TRUE, print changes but do not write anything
apply_nomenclatures <- function(
    metadata,
    mapping,
    nom_json  = "instructions/nomenclatures_INSEE.json",
    meta_json = NULL,
    dry_run   = FALSE
) {
  nom_list <- .read_nomenclatures_json(nom_json)
  noms     <- nom_list[["nomenclatures"]]

  for (var_name in names(mapping)) {
    nom_key <- mapping[[var_name]]
    row_idx <- which(metadata$var_name == var_name)
    if (length(row_idx) == 0) {
      warning("apply_nomenclatures: variable '", var_name,
              "' absente de metadata, ignor\u00e9e.")
      next
    }
    if (is.null(noms[[nom_key]])) {
      warning("apply_nomenclatures: nomenclature '", nom_key,
              "' absente du JSON ", nom_json, ", ignor\u00e9e.")
      next
    }

    lvls   <- noms[[nom_key]][["levels"]]
    codes  <- metadata$values[[row_idx]]
    if (is.null(codes) || length(codes) == 0) next

    old_labels <- metadata$labels[[row_idx]]
    new_labels <- vapply(codes, function(code) {
      if (code %in% names(lvls)) lvls[[code]][["label"]] else NA_character_
    }, character(1))

    # Merge: keep old label if new one is NA (code not in nomenclature)
    n_found   <- sum(!is.na(new_labels))
    n_missing <- sum(is.na(new_labels))
    if (n_missing > 0)
      warning("apply_nomenclatures: ", n_missing, " code(s) de '", var_name,
              "' absents de '", nom_key, "' \u2014 libell\u00e9s originaux conserv\u00e9s.")

    merged <- ifelse(!is.na(new_labels), new_labels,
                     if (!is.null(old_labels)) old_labels else NA_character_)

    if (dry_run) {
      message("[dry_run] ", var_name, " <- ", nom_key,
              " (", n_found, " codes match\u00e9s sur ", length(codes), ")")
    } else {
      metadata$labels[[row_idx]] <- merged
    }
  }

  # Write updated new_labels back to meta_json if provided
  if (!is.null(meta_json) && !dry_run) {
    survey_meta <- .read_meta_json(meta_json)
    for (var_name in names(mapping)) {
      row_idx <- which(metadata$var_name == var_name)
      if (length(row_idx) == 0) next
      codes  <- metadata$values[[row_idx]]
      labels <- metadata$labels[[row_idx]]
      if (is.null(codes)) next
      var_entry <- survey_meta$variables[[var_name]]
      if (is.null(var_entry)) next
      for (j in seq_along(codes)) {
        code <- codes[[j]]
        lbl  <- labels[[j]]
        if (!is.null(var_entry$levels[[code]]) && !is.na(lbl)) {
          survey_meta$variables[[var_name]]$levels[[code]][["new_label"]] <- lbl
        }
      }
    }
    .backup_meta_json(meta_json, step = "nomenclatures")
    .write_meta_json(survey_meta, meta_json)
    message("apply_nomenclatures: labels \u00e9crits dans ", meta_json)
  }

  invisible(metadata)
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
#'                        the current extraction (with auto-detected roles and
#'                        the config parameters written to the JSON). If the file
#'                        exists, its \code{config} section is used as defaults for
#'                        missing_num/missing_chr/yes_labels/no_labels (R args
#'                        override if explicitly supplied), and its
#'                        \code{variables} section overrides auto-detected role,
#'                        order, new_labels, new_name for each variable.
#'                        Edit the JSON between steps for manual corrections.
#'
#' @return A tibble with columns:
#'   var_name, var_label, r_class, n_distinct, detected_role, order,
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
    df = NULL,
    missing_num     = c(96, 99, 996, 999, 9996, 9999), # 8, 9,
    missing_chr     = c("-1", "NSP", "NRP", "NR", "REFUS",
                        "Ne sait pas", "Refus"), # "8", "9",
    yes_labels      = NULL,
    no_labels       = NULL,
    max_levels_cat  = 20,
    meta_json       = NULL,
    sas_format_file = NULL
) {
  # ---- Apply SAS format labels if provided -----------------------------------
  if (!is.null(df) && !is.null(sas_format_file) && nzchar(sas_format_file)) {
    sas_parsed <- parse_sas_formats(sas_format_file)
    df <- apply_sas_labels(df, sas_parsed)
    n_applied <- sum(names(df) %in% names(sas_parsed$value_labels))
    message("extract_survey_metadata: applied SAS format labels to ",
            n_applied, " variable(s) from ", basename(sas_format_file))
  }

  # ---- Read config/variables from meta_json (if exists) ---------------------
  .meta_json_existed <- !is.null(meta_json) && nzchar(meta_json) && file.exists(meta_json)
  .meta_json_data <- .read_meta_json(meta_json)
  .cfg            <- .meta_json_data$config
  .json_vars      <- .meta_json_data$variables

  # ---- Early return: reconstruct from JSON alone when df is NULL ------------
  if (is.null(df)) {
    if (!.meta_json_existed)
      stop("extract_survey_metadata: df is required when meta_json does not exist yet.")

    meta <- .skeleton_meta_from_json(.json_vars)
    meta <- metadata_apply_meta_json(meta, .json_vars)

    # n_distinct: count non-missing values
    meta$n_distinct <- purrr::map_int(seq_len(nrow(meta)), function(i) {
      as.integer(sum(!meta$values[[i]] %in% meta$missing_vals[[i]]))
    })

    message("extract_survey_metadata: reconstructed ", nrow(meta),
            " variables from JSON (no data frame)")
    return(meta)
  }

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

  # JSON variable-level role overrides (auto-detected < JSON < R args)
  .json_role_overrides <- if (length(.json_vars) > 0) {
    purrr::compact(purrr::imap(.json_vars, ~ {
      r <- .x$role; if (!is.null(r) && nzchar(r)) r else NULL
    }))
  } else list()

  # Role overrides come from JSON variables section only
  .effective_roles <- .json_role_overrides

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

  binary_lines  <- character(0)  # collect console output
  dropped_lines <- character(0)  # val_labs codes not observed in data

  meta <- purrr::imap(df, function(col, vname) {

    # --- variable label ---
    var_lbl <- var_labels_list[[vname]]
    if (is.null(var_lbl) || is.na(var_lbl)) var_lbl <- ""
    # Sanitize and strip Stata-style "VARNAME. " prefix — redundant since the
    # variable name is already the key in the JSON and the tibble var_name column.
    # Parentheses/brackets are kept (useful for relabeling reference).
    var_lbl <- .clean_var_label_for_api(.normalize_text(var_lbl, sanitize = TRUE), var_name = vname)

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
      # Sort by numeric value code — Stata's label-definition order may be
      # alphabetical by label text, but we always want code-ascending order.
      sorted_idx <- order(val_labs)
      all_codes  <- unname(val_labs)[sorted_idx]   # numeric codes
      all_labels <- .normalize_text(names(val_labs)[sorted_idx], sanitize = TRUE)

      # Inner join: keep only codes that are actually observed in the data.
      # Strategy: try numeric-numeric match first (handles double/integer columns
      # where the stored value may be 1.0 but the label code is 1L); fall back to
      # string comparison when either side contains non-numeric codes (e.g. SPSS
      # string variables with text value labels).
      obs_num  <- suppressWarnings(as.numeric(as.character(vals_present)))
      code_num <- suppressWarnings(as.numeric(as.character(all_codes)))
      if (!anyNA(obs_num) && !anyNA(code_num)) {
        keep <- code_num %in% obs_num
      } else {
        obs_chr  <- as.character(vals_present)
        keep     <- as.character(all_codes) %in% obs_chr
      }

      # Collect dropped codes for a single summary message after the loop.
      dropped_codes  <- all_codes[!keep]
      dropped_labels <- all_labels[!keep]
      if (length(dropped_codes) > 0) {
        dropped_lines <<- c(dropped_lines,
          purrr::map_chr(seq_along(dropped_codes), function(di) {
            sprintf("  %-20s  code=%-6s  \"%s\"",
                    vname,
                    as.character(dropped_codes[[di]]),
                    dropped_labels[[di]])
          })
        )
      }

      raw_values <- all_codes[keep]
      raw_labels <- all_labels[keep]
    } else if (is.factor(col)) {
      raw_values <- levels(col)
      raw_labels <- .normalize_text(levels(col), sanitize = TRUE)
    } else {
      sorted_vals <- sort(vals_present)
      raw_values  <- as.character(sorted_vals)
      raw_labels  <- .normalize_text(as.character(sorted_vals), sanitize = TRUE)
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
    pos_idx_auto  <- role_out$pos_idx   # 1L/2L/NA for binary; NA for others

    # Override role: from JSON variables section
    if (vname %in% names(.effective_roles)) {
      detected_role <- .effective_roles[[vname]]
    }

    # --- new_labels: copy of labels with missing candidates pre-marked as "NULL" ---
    new_labels_init <- ifelse(is_miss, "NULL", raw_labels)

    # --- For non-factor roles, labels are not meaningful — clear them ---
    if (detected_role %in% c("double", "integer", "identifier")) {
      raw_values      <- character(0)
      raw_labels      <- character(0)
      new_labels_init <- character(0)
      is_miss         <- logical(0)
    }

    # --- order vector: initial sequential assignment for non-missing levels ---
    # For binary vars with known positive position, assign order=1 to positive.
    # order column is populated from JSON by metadata_apply_meta_json(); here we
    # just create a default sequential vector as the baseline.
    n_lev <- length(raw_values)
    order_init <- if (n_lev > 0) {
      valid_pos <- 0L
      purrr::imap_int(seq_len(n_lev), function(idx, ...) {
        if (length(is_miss) >= idx && is_miss[[idx]]) {
          NA_integer_
        } else {
          valid_pos <<- valid_pos + 1L
          valid_pos
        }
      })
    } else integer(0)

    # For binary with known positive: swap order so positive=1, negative=2
    if (detected_role == "factor_binary" && !is.na(pos_idx_auto) && n_lev >= 2) {
      non_null_idx <- which(!is_miss)
      if (length(non_null_idx) >= 2) {
        pos_in_nonnull <- if (pos_idx_auto == 1L) non_null_idx[1] else non_null_idx[2]
        neg_in_nonnull <- if (pos_idx_auto == 1L) non_null_idx[2] else non_null_idx[1]
        order_init[pos_in_nonnull] <- 1L
        order_init[neg_in_nonnull] <- 2L
      }
    }

    # --- console output for factor_binary detection ---
    if (detected_role == "factor_binary") {
      lv1 <- if (length(lbls_clean) >= 1) lbls_clean[[1]] else "?"
      lv2 <- if (length(lbls_clean) >= 2) lbls_clean[[2]] else "?"
      if (is.na(pos_idx_auto)) {
        tag      <- "[factor_binary?]"
        pos_str  <- "positive unknown — needs ai_classify_roles()"
      } else if (pos_idx_auto == 1L) {
        tag      <- "[factor_binary] "
        pos_str  <- "positive=first \u2713"
      } else {
        tag      <- "[factor_binary] "
        pos_str  <- "positive=second"
      }
      binary_lines <<- c(binary_lines,
        sprintf("%s %-20s: \"%s\" vs \"%s\" \u2014 %s",
                tag, vname, lv1, lv2, pos_str))
    }

    tibble::tibble(
      var_name      = vname,
      var_label     = var_lbl,
      r_class       = r_class,
      n_distinct      = n_dist,
      n_distinct_data = n_dist_total,
      detected_role   = detected_role,
      values        = list(raw_values),
      labels        = list(raw_labels),
      missing_vals  = list(missing_vals_vec),
      new_labels    = list(new_labels_init),
      new_name      = vname,
      order         = list(order_init)
    )
  }) |>
    dplyr::bind_rows()

  # --- Console summary ---
  # Binary vars needing review: factor_binary where no level has order=1
  # (positive not yet resolved — order still at sequential default 1,2 from auto-detect)
  binary_needs_review <- purrr::map_lgl(seq_len(nrow(meta)), function(i) {
    row <- meta[i, ]
    if (row$detected_role != "factor_binary") return(FALSE)
    ord <- row$order[[1]]
    non_miss <- ord[!is.na(ord)]
    # If all orders are sequential (no swap), positive is unknown
    length(non_miss) >= 2 && !any(non_miss == 1L & non_miss != seq_along(non_miss[!is.na(ord)]))
  })
  n_needs_ai <- sum(meta$detected_role %in% c("factor_nominal", "integer") |
                    binary_needs_review)
  message("\nextract_survey_metadata: ", nrow(meta), " variables | ",
          nrow(df), " observations")
  message("  Roles: ",
          paste(names(table(meta$detected_role)),
                table(meta$detected_role), sep = "=", collapse = "  "))
  if (length(dropped_lines) > 0) {
    hdr <- paste0("\nextract_survey_metadata: ",
                  length(dropped_lines),
                  " value label(s) dropped (code not observed in data):")
    if (length(dropped_lines) <= 900L) {
      message(hdr, "\n", paste(dropped_lines, collapse = "\n"))
    } else {
      tmp <- tempfile(fileext = ".txt")
      writeLines(c(sub("^\n", "", hdr), dropped_lines), tmp)
      message(hdr, "\n  (output too long — opening in editor: ", tmp, ")")
      tryCatch(
        rstudioapi::navigateToFile(tmp),
        error = function(e) utils::file.edit(tmp)
      )
    }
  }
  if (length(binary_lines) > 0) {
    message("\nBinary variables (factor_binary) detected:")
    purrr::walk(binary_lines, message)
  }
  if (n_needs_ai > 0) {
    message("\n[!] ", n_needs_ai, " variable(s) may need role refinement",
            " (factor_nominal / integer / factor_binary with unknown positive level).")
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
      # On re-run: sync missing flags and optionally update config
      .updated_vars <- .update_missing_in_meta_json(.meta_json_data$variables, meta)
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
        message("extract_survey_metadata: updated missing flags/config in ", meta_json)
      }
    }
  }

  meta
}


# Internal role detection v4 — not exported
# Returns list(role, pos_idx) where pos_idx is:
#   - For factor_binary: 1L if positive label is first, 2L if second, NA_integer_ if unknown
#   - For all other roles: NA_integer_
.detect_role_v3 <- function(
    vname, col, has_val_labs, n_dist_total, n_rows,
    n_clean, vals_clean, lbls_clean, yes_kw, no_kw, r_class
) {
  # 1. Identifier: ID name or all (total) values unique
  id_pattern <- "^(IDENT|IDENTIF|IDENTIFIANT|ID|_ID|ID_|NUMEN|NUMIDENT)$|^IDENT"
  if (grepl(id_pattern, vname, ignore.case = TRUE) || n_dist_total == n_rows) {
    return(list(role = "identifier", pos_idx = NA_integer_))
  }

  # 2. Labelled column → factor_binary (2 clean levels) or factor_nominal (>=3)
  #    When all labels are missing (n_clean == 0), fall through to
  #    numeric/character detection below instead of returning factor_nominal.
  if (has_val_labs && n_clean > 0) {
    if (n_clean == 2) {
      pos_idx <- .find_binary_desc(lbls_clean, yes_kw, no_kw)
      return(list(role = "factor_binary", pos_idx = pos_idx))
    }
    return(list(role = "factor_nominal", pos_idx = NA_integer_))
  }

  # 3. Factor column (no val_labs but is.factor)
  if (is.factor(col)) {
    if (n_clean == 2) {
      pos_idx <- .find_binary_desc(lbls_clean, yes_kw, no_kw)
      return(list(role = "factor_binary", pos_idx = pos_idx))
    }
    return(list(role = "factor_nominal", pos_idx = NA_integer_))
  }

  # 4. Unlabelled numeric: distinguish double (any non-whole value) from integer
  if (r_class %in% c("double", "numeric", "integer") || is.numeric(col)) {
    vals_num <- suppressWarnings(as.numeric(vals_clean))
    vals_num <- vals_num[!is.na(vals_num)]
    if (length(vals_num) > 0 && any(vals_num != floor(vals_num))) {
      return(list(role = "double", pos_idx = NA_integer_))
    }
    return(list(role = "integer", pos_idx = NA_integer_))
  }

  # 5. Character column without labels
  if (is.character(col)) {
    all_int_str <- length(vals_clean) > 0 &&
      all(grepl("^-?[0-9]+$", vals_clean[vals_clean != ""]))
    if (all_int_str) return(list(role = "integer", pos_idx = NA_integer_))
    return(list(role = "factor_nominal", pos_idx = NA_integer_))
  }

  list(role = "other", pos_idx = NA_integer_)
}


# Internal: determine the position index of the positive label for a 2-level binary variable.
# Returns 1L if the positive label is first, 2L if second, NA_integer_ if unknown.
# Used by extract_survey_metadata() to write initial "order" integers to JSON:
#   pos_idx=1 → order: [1, 2]; pos_idx=2 → order: [2, 1] (positive always gets order=1).
.find_binary_desc <- function(lbls_clean, yes_kw, no_kw) {
  if (length(lbls_clean) != 2) return(NA_integer_)

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

  # Exactly one label matches yes and not no → return its position index (1 or 2)
  yes_only <- is_yes & !is_no
  if (sum(yes_only) == 1) return(as.integer(which(yes_only)))

  # If both labels claim yes (shouldn't happen with good kw lists), use first match
  if (sum(is_yes) == 1) return(as.integer(which(is_yes)))

  NA_integer_
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
# 2c. metadata_merge_ordinal_levels()
# ============================================================

#' Compute and write merge groups for ordinal factor levels
#'
#' Reads the \code{order} integers already stored in \code{metadata$order}
#' (set by \code{ai_classify_roles()}), groups contiguous non-missing levels
#' that fall below the frequency/count thresholds, and re-emits consecutive
#' group integers so that merged levels share the same \code{order} value.
#' The result is written back to \code{metadata$order} and, if \code{meta_json}
#' is supplied, persisted to the JSON file (with a backup).
#'
#' Run between \code{metadata_add_level_stats()} and \code{ai_suggest_labels()}.
#' After this step, review the JSON manually and adjust \code{order} integers
#' as needed before calling \code{ai_suggest_labels()}.
#'
#' @param metadata  Varmod tibble with an \code{order} list-column.
#' @param vars      Character vector of variable names to process.
#'                  \code{NULL} (default) = all \code{factor_ordinal} variables.
#' @param min_pct   Merge threshold as a fraction (e.g. \code{0.05} = 5 \%).
#'                  Contiguous levels whose cumulative percentage is below this
#'                  threshold are merged. \code{0} disables percentage threshold.
#' @param min_n     Merge threshold as an absolute count. \code{0} (default)
#'                  disables count threshold.
#' @param meta_json Path to \code{*.survey_meta.json}. When provided, the
#'                  updated \code{order} integers are written to the file
#'                  (after a backup). Default \code{NULL}.
#'
#' @return Modified metadata tibble (visibly). Variables where at least one
#'   merge group contains more than one level are reported to the console.
metadata_merge_ordinal_levels <- function(
    metadata,
    vars      = NULL,
    min_pct   = 0.05,
    min_n     = 0L,
    meta_json = NULL
) {
  # Target: factor_ordinal rows, optionally filtered by vars
  target_mask <- metadata$detected_role == "factor_ordinal"
  if (!is.null(vars)) target_mask <- target_mask & metadata$var_name %in% vars
  target_idx  <- which(target_mask)

  if (length(target_idx) == 0L) {
    message("metadata_merge_ordinal_levels: No factor_ordinal variables found.")
    return(metadata)
  }

  if (!"order" %in% names(metadata)) {
    warning("metadata_merge_ordinal_levels: metadata$order column absent. ",
            "Re-run extract_survey_metadata() with meta_json first.")
    return(metadata)
  }

  merged_vars <- character(0)   # variables where at least one merge happened

  for (i in target_idx) {
    ord_ints  <- metadata$order[[i]]        # integer vector, NA = missing level
    nls       <- metadata$new_labels[[1L + (i - 1L)]]  # parallel labels
    # Safer: access list-column element by index
    nls       <- metadata$new_labels[[i]]

    if (length(ord_ints) == 0L || all(is.na(ord_ints))) next

    # Non-missing level positions (in stored vector order)
    valid_pos <- which(!is.na(ord_ints))
    if (length(valid_pos) < 2L) next

    # Sort valid positions by their current order integer (encodes direction)
    sorted_pos <- valid_pos[order(ord_ints[valid_pos])]

    # Retrieve level stats for merge threshold computation
    cnts <- metadata$level_counts[[i]]
    frqs <- metadata$level_freqs[[i]]

    # Build parallel vectors for non-missing levels in sorted order
    val_codes  <- as.character(metadata$values[[i]])[sorted_pos]
    counts_s   <- if (!is.null(cnts) && length(cnts) == length(ord_ints))
                    cnts[sorted_pos] else rep(NA_integer_, length(sorted_pos))
    freqs_s    <- if (!is.null(frqs) && length(frqs) == length(ord_ints))
                    frqs[sorted_pos] else rep(NA_real_,    length(sorted_pos))

    # Run merge-group algorithm
    grp_ids <- .compute_merge_groups(val_codes, counts_s, freqs_s,
                                     min_pct = min_pct, min_n = min_n)

    # Check if any merge actually happened (group spans > 1 level)
    grp_tbl <- table(grp_ids)
    if (any(grp_tbl > 1L)) merged_vars <- c(merged_vars, metadata$var_name[[i]])

    # Re-emit consecutive group integers (1 … n_groups) preserving order.
    # Levels in the same group get the same integer.
    n_groups    <- max(grp_ids)
    new_ord_sorted <- grp_ids  # already consecutive 1…n_groups from .compute_merge_groups

    # Map back to original vector positions
    new_ord <- ord_ints
    for (j in seq_along(sorted_pos)) {
      new_ord[sorted_pos[[j]]] <- new_ord_sorted[[j]]
    }
    metadata$order[[i]] <- new_ord
  }

  # --- Write updated order integers to JSON ---
  if (!is.null(meta_json)) {
    .backup_meta_json(meta_json, "merge_ordinal_levels")
    existing <- .read_meta_json(meta_json)

    for (i in target_idx) {
      vn       <- metadata$var_name[[i]]
      if (is.null(existing$variables[[vn]])) next
      ord_ints <- metadata$order[[i]]
      vals     <- metadata$values[[i]]
      levs     <- existing$variables[[vn]]$levels
      if (is.null(levs)) next

      for (j in seq_along(vals)) {
        key <- as.character(vals[[j]])
        if (!key %in% names(levs)) next
        if (isTRUE(levs[[key]]$missing)) next   # skip missing levels
        if (is.na(ord_ints[[j]])) next
        existing$variables[[vn]]$levels[[key]]$order <- ord_ints[[j]]
      }
    }
    .write_meta_json(existing, meta_json)
    message("metadata_merge_ordinal_levels: order integers written to ",
            basename(meta_json))
  }

  # Console report: which variables had at least one merge group
  if (length(merged_vars) > 0L) {
    quoted  <- paste0('"', merged_vars, '"', collapse = ',\n      ')
    message("metadata_merge_ordinal_levels: merges applied to ",
            length(merged_vars), " variable(s):\n\n",
            "  meta |>\n",
            "    filter(\n",
            "      var_name %in% c(\n",
            "        ", quoted, ") ) |>\n",
            "    export_metadata_excel(path = metadata_review_path)\n")
  } else {
    message("metadata_merge_ordinal_levels: no levels merged (all above threshold).")
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

#' Standardise binary variable labels using the order column
#'
#' For rows with detected_role == "factor_binary" and a valid order vector:
#'   - The level with order=1 is the positive level (Oui, Choisi…)
#'   - The level with order=2 is the negative level (Non, Non choisi…)
#'   - Positive level → "1-<var_label>" (or original label if use_var_label=FALSE)
#'   - Negative level → "2-Non"
#'   - Missing levels (order=NA) keep "NULL" sentinel
#'
#' For binary rows where positive is not yet resolved (no level with order=1):
#'   - Warns and skips. Run ai_classify_roles() first.
#'
#' @param metadata       Varmod tibble (must have order list-column).
#' @param use_var_label  If TRUE (default), positive level → "1-<var_label>".
#'                       If FALSE, uses original positive label text (still adds "1-" prefix).
#'
#' @return Updated metadata tibble with new_labels set for binary variables.
metadata_fix_binary <- function(metadata, use_var_label = TRUE) {
  if (!"order" %in% names(metadata)) {
    warning("metadata_fix_binary: 'order' column missing. Run extract_survey_metadata() first.")
    return(metadata)
  }

  # Identify binary rows where positive is not yet resolved (no level with order=1)
  unresolved <- purrr::map_lgl(seq_len(nrow(metadata)), function(i) {
    row <- metadata[i, ]
    if (row$detected_role != "factor_binary") return(FALSE)
    ord      <- row$order[[1]]
    non_miss <- ord[!is.na(ord)]
    length(non_miss) < 2 || !1L %in% non_miss
  })

  if (any(unresolved)) {
    warning(sum(unresolved), " factor_binary variable(s) with unresolved positive level skipped: ",
            paste(metadata$var_name[unresolved], collapse = ", "),
            "\nRun ai_classify_roles(meta, meta_json = meta_json) to assign order.")
  }

  metadata |>
    dplyr::mutate(
      new_labels = purrr::pmap(
        list(detected_role, var_label, new_labels,
             if ("order" %in% names(metadata)) order else vector("list", nrow(metadata))),
        function(role, lbl, nls, ord) {
          if (role != "factor_binary") return(nls)
          non_miss <- which(!is.na(ord))
          if (length(non_miss) < 2) return(nls)
          # Find the level with order=1 (positive) and order=2 (negative)
          pos_idx <- non_miss[which(ord[non_miss] == 1L)]
          neg_idx <- non_miss[which(ord[non_miss] == 2L)]
          if (length(pos_idx) == 0 || length(neg_idx) == 0) return(nls)
          pos_idx <- pos_idx[[1]]; neg_idx <- neg_idx[[1]]

          pos_orig  <- nls[[pos_idx]]
          pos_label <- if (use_var_label && lbl != "") paste0("1-", lbl) else paste0("1-", pos_orig)
          neg_label <- "2-Non"

          result              <- rep("NULL", length(nls))
          result[[pos_idx]]   <- pos_label
          result[[neg_idx]]   <- neg_label
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
#'                        Default: factor_nominal, factor_binary (order unresolved), integer.
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
      new_labels_str    = purrr::map_chr(new_labels, collapse_new_labels)
    ) |>
    dplyr::select(
      var_name, var_label, r_class, detected_role, n_distinct,
      missing_vals = missing_vals_str,
      labels       = labels_str,
      new_labels   = new_labels_str,
      new_name
    )

  # Highlight factor_binary rows where positive level is not yet resolved
  # (no level with order=1 among non-missing levels)
  highlight_rows_extra <- if ("factor_binary" %in% metadata$detected_role &&
                               "order" %in% names(metadata)) {
    which(purrr::map_lgl(seq_len(nrow(metadata)), function(i) {
      row <- metadata[i, ]
      if (row$detected_role != "factor_binary") return(FALSE)
      ord      <- row$order[[1]]
      non_miss <- ord[!is.na(ord)]
      length(non_miss) < 2 || !1L %in% non_miss
    }))
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
  all_cols   <- c("var_name", "var_label", "r_class", "detected_role",
                  "n_distinct", "missing_vals", "labels", "new_labels", "new_name")
  all_widths <- c(20,          40,           12,        22,
                  10,           30,            45,        45,             20)
  shown_cols <- intersect(names(df_excel), all_cols)
  shown_w    <- all_widths[match(shown_cols, all_cols)]
  purrr::walk2(seq_along(shown_w), shown_w, function(col, w) {
    openxlsx::setColWidths(wb, "metadata", cols = col, widths = w)
  })

  # Orange rows = highlight_roles + factor_binary with unresolved positive level
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
# 6. AI helpers — httr2 only, no reticulate
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

  # When system is a list (content blocks with cache_control), add the
  # prompt-caching beta header required by Anthropic.
  use_cache <- is.list(system) && !is.null(system)
  headers <- list("x-api-key"         = api_key,
                  "anthropic-version" = "2023-06-01",
                  "content-type"      = "application/json")
  if (use_cache)
    headers[["anthropic-beta"]] <- "prompt-caching-2024-07-31"

  do.call(httr2::req_headers, c(list(httr2::request("https://api.anthropic.com/v1/messages")),
                                 headers)) |>
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

  use_cache <- is.list(system) && !is.null(system)
  batch_headers <- list("x-api-key"         = api_key,
                        "anthropic-version" = "2023-06-01",
                        "content-type"      = "application/json")
  if (use_cache)
    batch_headers[["anthropic-beta"]] <- "prompt-caching-2024-07-31"

  resp <- do.call(httr2::req_headers,
                  c(list(httr2::request("https://api.anthropic.com/v1/messages/batches")),
                    batch_headers)) |>
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
# 7. ai_classify_roles()
# ============================================================

#' Classify ambiguous variables with Haiku, print copy-pasteable R vectors
#'
#' Only sends variables that genuinely need refinement:
#'   - factor_nominal  (all: may be ordinal)
#'   - integer         (all: may be integer_scale or integer_count)
#'   - factor_binary with no order=1 level (needs positive level confirmed)
#' NOT sent: identifier, double, factor_binary with order resolved,
#'           factor_ordinal/integer_scale/integer_count (already refined).
#'
#' Deduplicates by unique label set before sending — if 50 variables share
#' the same value labels, Haiku classifies the label set once, not 50 times.
#'
#' Role codes:
#'   F = factor_nominal  — unordered categories (professions, régions…)
#'   O = factor_ordinal  — ordered categories (niveau de diplôme, satisfaction…)
#'   B = factor_binary   — 2-level yes/no variable (positive level unknown only)
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
#'   → Translated to "order" integers in the JSON:
#'     O+T: reverse sequential (n, n-1, …, 1)  O+F: sequential (1, 2, …, n)
#'     B+T: order=1 for first label, order=2 for second
#'     B+F: order=2 for first label, order=1 for second
#'     O+?: assumes ascending data order; reverses when ordinal_desc=TRUE
#'
#' @param metadata         Varmod tibble from extract_survey_metadata().
#' @param meta_json        Path to the unified \code{*.survey_meta.json} file
#'                         (required). Results (role, order integers) are written
#'                         directly to this file after a backup. Re-run
#'                         extract_survey_metadata() to reload.
#' @param ordinal_desc     Logical. Controls the display order for ordinal variables.
#'                         \code{TRUE} = highest category first (order=1); when
#'                         Haiku returns "?" the fallback assumes labels in the data
#'                         are stored in ascending order (the norm) and reverses them.
#'                         \code{FALSE} = lowest category first; "?" fallback keeps
#'                         the data order as-is. Default \code{FALSE}.
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
#'                         Default 10.
#'
#' @return Invisibly returns \code{meta_json}.
#'         In dry_run mode: invisibly returns the list of user prompt strings.
ai_classify_roles <- function(
    metadata,
    meta_json        = NULL,
    ordinal_desc     = FALSE,
    chunk_size       = 400L,
    use_batch        = FALSE,
    dry_run          = FALSE,
    api_key          = Sys.getenv("ANTHROPIC_API_KEY"),
    model            = "claude-haiku-4-5",
    max_labels_sent  = 10L,
    log_raw_answer   = FALSE
) {
  if (is.null(meta_json))
    stop("ai_classify_roles: meta_json is required. ",
         "Provide the path to your *.survey_meta.json file.")
  # --- Filter to ambiguous variables only ---
  # Binary variables are included only when positive level not yet resolved
  # (no level has order = 1L).
  bin_unresolved <- if ("order" %in% names(metadata)) {
    purrr::map_lgl(metadata$order, function(ord) !any(ord == 1L, na.rm = TRUE))
  } else {
    rep(TRUE, nrow(metadata))
  }

  target <- metadata |>
    dplyr::filter(
      detected_role %in% c("factor_nominal", "integer") |
        (detected_role == "factor_binary" & bin_unresolved)
    )

  if (nrow(target) == 0) {
    message("ai_classify_roles: No ambiguous variables to classify.",
            " (factor_binary with order resolved, double, identifier already clear.)")
    return(invisible(meta_json))
  }

  # --- Auto-classify nd:0 (all missing) and nd:1 (single non-missing value) ---
  auto_nd0 <- target |> dplyr::filter(n_distinct == 0L)
  auto_nd1 <- target |> dplyr::filter(n_distinct == 1L)
  n_auto   <- nrow(auto_nd0) + nrow(auto_nd1)

  if (n_auto > 0L) {
    .backup_meta_json(meta_json, "classify_roles_auto")
    existing_auto <- .read_meta_json(meta_json)

    # Threshold: if the actual data has >= 5 distinct values but only 1 non-missing
    # label, it is a continuous integer variable (age, year, score…), not a factor.
    .nd_cont_threshold <- 5L

    # French survey open-text field patterns: "noter en clair", "précisez", etc.
    .opentext_re <- paste0(
      "(?i)(noter?\\s+en\\s+clair|notez\\s+en\\s+clair|",
      "\\bpr\u00e9cisez\\b|\\bprecisez\\b|texte\\s+libre|r\u00e9ponse\\s+libre|",
      "reponse\\s+libre|champ\\s+texte|libre\\s+r\u00e9ponse|libre\\s+reponse)"
    )

    if (nrow(auto_nd0) > 0L) {
      message("  ", nrow(auto_nd0),
              " variable(s) with nd=0 (all missing codes) — auto-classified, not sent to AI.")
      for (vn in auto_nd0$var_name) {
        if (is.null(existing_auto$variables[[vn]])) next
        row <- auto_nd0[auto_nd0$var_name == vn, ]
        dr  <- row$detected_role
        ndd <- if ("n_distinct_data" %in% names(row)) row$n_distinct_data else 0L
        if (is.na(ndd)) ndd <- 0L
        if (dr == "integer" || ndd >= .nd_cont_threshold) {
          existing_auto$variables[[vn]]$role <- "integer_count"
        }
        # else: leave existing JSON role unchanged; variable not sent to AI
      }
    }

    if (nrow(auto_nd1) > 0L) {
      message("  ", nrow(auto_nd1),
              " variable(s) with 1 non-missing value — disambiguating pre-AI.")
      for (vn in auto_nd1$var_name) {
        if (is.null(existing_auto$variables[[vn]])) next
        row  <- auto_nd1[auto_nd1$var_name == vn, ]
        dr   <- row$detected_role
        ndd  <- if ("n_distinct_data" %in% names(row)) row$n_distinct_data else 0L
        if (is.na(ndd)) ndd <- 0L
        lbl1 <- if (length(row$labels[[1]]) > 0) row$labels[[1]][[1]] else ""
        vlbl <- row$var_label

        if (dr == "integer" || ndd >= .nd_cont_threshold) {
          # Many distinct data values despite 1 label = continuous integer
          existing_auto$variables[[vn]]$role <- "integer_count"
        } else if (grepl(.opentext_re, lbl1, perl = TRUE) ||
                   grepl(.opentext_re, vlbl, perl = TRUE)) {
          # Open-text / free-text field ("noter en clair", "précisez"…)
          existing_auto$variables[[vn]]$role <- "other"
        } else {
          # True single-category factor (e.g. question filtered to one answer)
          existing_auto$variables[[vn]]$role <- "factor_unique_value"
        }
      }
    }

    .write_meta_json(existing_auto, meta_json)
    target <- target |> dplyr::filter(n_distinct > 1L)
  }

  if (nrow(target) == 0) {
    message("ai_classify_roles: All variables auto-classified. No API call needed.")
    return(invisible(meta_json))
  }

  message("ai_classify_roles: ", nrow(target), " variable(s) to classify.")

  # --- Build label key per variable (non-missing labels, sorted for dedup) ---
  # Use value codes (not label strings) to identify missing levels correctly.
  target <- target |>
    dplyr::mutate(
      .lbl_key = purrr::pmap_chr(list(labels, values, missing_vals),
        function(lbls, vals, miss) {
          is_miss_lbl <- as.character(vals) %in% as.character(miss)
          clean <- sort(tolower(.normalize_text(lbls[!is_miss_lbl & nzchar(lbls)])))
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
         unique_sets$n_distinct, unique_sets$labels, unique_sets$values,
         unique_sets$missing_vals, unique_sets$detected_role, unique_sets$.lbl_key),
    function(var_name, var_label, r_class, n_distinct, labels, values,
             missing_vals, detected_role, lbl_key) {
      .format_classify_jsonl(var_name, var_label, detected_role,
                             labels, values, missing_vals,
                             n_distinct, max_labels = max_labels_sent)
    }
  )

  # --- Load system prompt from external .md file ---
  .prompt_path <- file.path(getwd(), "instructions", "classify_roles_prompt.md")
  if (!file.exists(.prompt_path)) {
    .pkg_name <- utils::packageName()
    if (!is.null(.pkg_name) && nzchar(.pkg_name)) {
      pkg_path <- system.file("instructions/classify_roles_prompt.md",
                              package = .pkg_name)
      if (nzchar(pkg_path) && file.exists(pkg_path)) .prompt_path <- pkg_path
    }
  }
  if (!file.exists(.prompt_path))
    stop("ai_classify_roles: instructions/classify_roles_prompt.md not found.")

  system_prompt <- .build_classify_system_prompt(
    .prompt_path, ordinal_desc, max_labels_sent
  )
  system_prompt_cached <- list(
    list(type = "text", text = system_prompt,
         cache_control = list(type = "ephemeral"))
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
                             system = system_prompt_cached,
                             max_tokens = max_tok)
      resp$content[[1]]$text
    })
  } else {
    message("ai_classify_roles: batch mode (", nrow(unique_sets), " unique set(s))")
    requests <- purrr::imap(prompts, ~ list(custom_id = paste0("classify_", .y),
                                            prompt     = .x))
    batch    <- ai_batch_submit(requests, model = model, api_key = api_key,
                                system = system_prompt_cached,
                                max_tokens = max_tok)
    message("Batch submitted. ID: ", batch$id)
    raw          <- ai_batch_retrieve(batch$id, api_key = api_key)
    results_text <- purrr::map(purrr::set_names(names(raw)), ~ raw[[.x]])
  }

  raw_text <- paste(unlist(results_text), collapse = "\n")

  if (log_raw_answer) {
    message(strrep("-", 60))
    message("RAW HAIKU ANSWER")
    message(strrep("-", 60))
    message(raw_text)
    message(strrep("-", 60))
  }

  # --- Valid roles and desc codes ---
  valid_roles <- c("factor_nominal", "factor_ordinal", "factor_binary",
                   "integer_scale", "integer_count", "double", "other", "unclear")
  valid_descs <- c("high_first", "low_first", "unknown")

  # Parse JSONL response: one JSON object per line
  set_roles   <- character(0)   # keyed by var_name of unique set
  set_dirs    <- character(0)   # "high_first", "low_first", or "unknown" per set id
  set_missing <- character(0)

  resp_lines <- strsplit(trimws(raw_text), "\n")[[1]]
  resp_lines <- resp_lines[nzchar(resp_lines)]

  for (ln in resp_lines) {
    parsed <- tryCatch(jsonlite::fromJSON(ln), error = function(e) NULL)
    if (is.null(parsed)) {
      warning("ai_classify_roles: skipping malformed JSONL line: ",
              substr(ln, 1, 80))
      next
    }

    set_id <- parsed$id
    role   <- parsed$role
    if (is.null(set_id) || is.null(role)) next
    if (!set_id %in% unique_sets$var_name) next
    if (!role %in% valid_roles) next

    # Map "unclear" to internal "???" sentinel
    set_roles[[set_id]] <- if (role == "unclear") "???" else role

    # desc (direction) for binary and ordinal
    desc <- parsed$desc
    should_parse_dir <- role == "factor_binary" ||
      (role == "factor_ordinal" && ordinal_desc)
    if (should_parse_dir && !is.null(desc) && desc %in% valid_descs) {
      set_dirs[[set_id]] <- desc
    }

    # Extra missing-value label
    if (!is.null(parsed$miss)) {
      set_missing[[set_id]] <- .normalize_text(parsed$miss)
    }
  }

  # --- Propagate set results back to ALL variables with matching label key ---
  key_to_set <- purrr::set_names(unique_sets$.lbl_key, unique_sets$var_name)

  detected_roles <- character(0)
  dir_overrides  <- character(0)   # "T", "F", or "?" per variable
  extra_missing  <- character(0)

  for (vn in target$var_name) {
    key    <- target$.lbl_key[target$var_name == vn]
    set_id <- names(key_to_set)[key_to_set == key]
    if (length(set_id) == 0 || !set_id %in% names(set_roles)) next
    detected_roles[[vn]] <- set_roles[[set_id]]
    if (set_id %in% names(set_dirs))    dir_overrides[[vn]]  <- set_dirs[[set_id]]
    if (set_id %in% names(set_missing)) extra_missing[[vn]]  <- set_missing[[set_id]]
  }

  # --- Write role + order integers to meta_json ---
  .backup_meta_json(meta_json, "classify_roles")
  existing <- .read_meta_json(meta_json)

  n_updated <- 0L
  for (vn in names(detected_roles)) {
    if (is.null(existing$variables[[vn]])) next
    new_role <- detected_roles[[vn]]
    existing$variables[[vn]]$role <- new_role
    n_updated <- n_updated + 1L

    # Write "order" integers based on direction for ordinal and binary
    dir_code <- if (vn %in% names(dir_overrides)) dir_overrides[[vn]] else "unknown"
    levs <- existing$variables[[vn]]$levels
    if (is.null(levs) || length(levs) == 0) next

    # Identify non-missing levels (those without missing: true)
    non_miss_keys <- purrr::keep(names(levs), ~ !isTRUE(levs[[.x]]$missing))
    n_valid <- length(non_miss_keys)
    if (n_valid == 0) next

    if (new_role == "factor_ordinal") {
      # ordinal_desc=FALSE → leave existing order field unchanged
      if (!ordinal_desc) next
      # "unknown" fallback: assume labels in data are ascending (the norm for French
      # survey data — youngest ages first, lowest diploma first).
      # When ordinal_desc=TRUE we want highest-first display, so invert ascending data
      # (same logic as an explicit "low_first" reply from Haiku).
      desc_flag <- switch(dir_code,
        high_first = TRUE, low_first = FALSE, !ordinal_desc)
      if (desc_flag) {
        # Descending: first shown level is highest → order 1
        new_orders <- seq_len(n_valid)
      } else {
        # Ascending: last shown level is highest → order 1
        new_orders <- seq(n_valid, 1L)
      }
      for (j in seq_along(non_miss_keys)) {
        existing$variables[[vn]]$levels[[non_miss_keys[[j]]]]$order <- new_orders[[j]]
      }

      # Move "Autre" catch-all to last position if present
      autre_pat <- "(?i)^autre(\\b|$)"
      for (j in seq_along(non_miss_keys)) {
        lbl_j <- levs[[non_miss_keys[[j]]]]$label %||% ""
        if (grepl(autre_pat, lbl_j, perl = TRUE)) {
          cur_order <- existing$variables[[vn]]$levels[[non_miss_keys[[j]]]]$order
          if (!is.null(cur_order) && cur_order != n_valid) {
            # Shift levels that were after "Autre" position back by 1
            for (k in seq_along(non_miss_keys)) {
              ok <- existing$variables[[vn]]$levels[[non_miss_keys[[k]]]]$order
              if (!is.null(ok) && ok > cur_order && ok <= n_valid) {
                existing$variables[[vn]]$levels[[non_miss_keys[[k]]]]$order <- ok - 1L
              }
            }
            existing$variables[[vn]]$levels[[non_miss_keys[[j]]]]$order <- n_valid
          }
          break
        }
      }

    } else if (new_role == "factor_binary" && n_valid == 2L) {
      # For binary: determine which label is positive using .find_binary_desc()
      yes_kw <- existing$config$yes_labels %||% character(0)
      no_kw  <- existing$config$no_labels  %||% character(0)
      lv1_lbl <- levs[[non_miss_keys[[1]]]]$label %||% ""
      lv2_lbl <- levs[[non_miss_keys[[2]]]]$label %||% ""
      shown_lbls <- c(lv1_lbl, lv2_lbl)
      pos_idx <- .find_binary_desc(shown_lbls, yes_kw, no_kw)

      if (!is.na(pos_idx) && dir_code != "unknown") {
        # high_first = positive is first shown; low_first = positive is second
        pos_in_shown <- switch(dir_code,
          high_first = 1L, low_first = 2L, pos_idx)
      } else {
        pos_in_shown <- if (!is.na(pos_idx)) pos_idx else NA_integer_
      }

      if (!is.na(pos_in_shown)) {
        neg_in_shown <- if (pos_in_shown == 1L) 2L else 1L
        existing$variables[[vn]]$levels[[non_miss_keys[[pos_in_shown]]]]$order <- 1L
        existing$variables[[vn]]$levels[[non_miss_keys[[neg_in_shown]]]]$order <- 2L
      }
    }
  }

  .write_meta_json(existing, meta_json)

  message("\n", strrep("=", 60))
  message("ai_classify_roles: ", n_updated, " variable(s) updated in: ", meta_json)
  message("Review role/order fields in the JSON, then re-run extract_survey_metadata().")

  if (length(extra_missing) > 0) {
    uniq_miss <- unique(unname(extra_missing))
    message("\n[!] Possible missing labels flagged by AI — add to missing_chr if correct:")
    message("    ", paste0('"', uniq_miss, '"', collapse = ", "))
  }

  message(strrep("=", 60))
  invisible(meta_json)
}


# ============================================================
# 9b. invert_ordinal_order()
# ============================================================

#' Invert the "order" field for all factor_ordinal variables in a meta JSON
#'
#' Reads the unified survey_meta.json, finds every variable with
#' `role = "factor_ordinal"`, and reverses the numeric `order` values of its
#' non-missing levels (e.g. [1,2,3,4] → [4,3,2,1]).  The actual level order in
#' the JSON is never changed — only the `order` integers are updated.
#'
#' Use this when all ordinal `order` fields are currently ascending and you want
#' descending (positive/best level = order 1) without calling the API again.
#'
#' @param meta_json Path to the `.survey_meta.json` file.
#'
#' @return `meta_json` invisibly.
#' @export
invert_ordinal_order <- function(meta_json) {
  existing <- .read_meta_json(meta_json)
  n_updated <- 0L

  for (vn in names(existing$variables)) {
    var <- existing$variables[[vn]]
    if (!identical(var$role, "factor_ordinal")) next

    levs <- var$levels
    if (is.null(levs) || length(levs) == 0L) next

    non_miss_keys <- purrr::keep(names(levs), ~ !isTRUE(levs[[.x]]$missing))
    if (length(non_miss_keys) == 0L) next

    orders <- purrr::map_int(non_miss_keys, ~ {
      o <- levs[[.x]]$order
      if (is.null(o)) NA_integer_ else as.integer(o)
    })

    if (anyNA(orders)) next

    rev_orders <- (max(orders) + 1L) - orders
    for (j in seq_along(non_miss_keys)) {
      existing$variables[[vn]]$levels[[non_miss_keys[[j]]]]$order <- rev_orders[[j]]
    }
    n_updated <- n_updated + 1L
  }

  .backup_meta_json(meta_json, "invert_ordinal_order")
  .write_meta_json(existing, meta_json)
  message("invert_ordinal_order: ", n_updated, " variable(s) updated in: ", meta_json)
  invisible(meta_json)
}


# ============================================================
# 8. ai_suggest_missing()
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
      gid <- gid - 1L
    }
  }

  # ---- Post-pass: merge first group downward if still below threshold ------
  # Symmetric guard for the first group (endpoint on the low end).
  if (gid > 1L) {
    first_ids  <- which(groups == 1L)
    first_pct  <- sum(freqs[first_ids],  na.rm = TRUE)
    first_n    <- sum(counts[first_ids], na.rm = TRUE)
    still_low  <- (use_pct && first_pct < min_pct_100) ||
                  (use_n   && first_n   < min_n)
    if (still_low) {
      # Merge first group into the second group by renaming group 2 → 1
      # and shifting all higher group ids down by 1.
      groups[groups == 2L] <- 1L
      groups[groups >  2L] <- groups[groups > 2L] - 1L
      gid <- gid - 1L
    }
  }

  # ---- Two-level failsafe: ensure at least 2 distinct groups ---------------
  # If all levels ended up in one group, split at the midpoint so the caller
  # always has at least 2 distinct order values to work with.
  if (length(unique(groups)) == 1L && n >= 2L) {
    mid         <- ceiling(n / 2)
    groups[seq(mid + 1L, n)] <- 2L
  }

  groups
}


# ============================================================
# 9. ai_merge_levels()
# ============================================================

#' Use Haiku to semantically merge factor level groups
#'
#' Sends ordinal (and optionally nominal) variables to Claude Haiku and asks it
#' to decide which adjacent categories should be merged based on frequencies,
#' semantic logic, and sociological conventions.  The function writes new
#' \code{order} integers into \code{meta_json} only — label renaming is left
#' for \code{ai_suggest_labels()}, which already reads the \code{order} field
#' and proposes group names.
#'
#' ## Workflow position
#'   Run \emph{after} \code{ai_classify_roles()} (which sets roles) and
#'   \emph{after} \code{metadata_add_level_stats()} (which provides n/pct,
#'   and applies the zero-n → missing rule).
#'   Run \emph{before} \code{ai_suggest_labels()}.
#'
#' ## Arguments
#' @param metadata     Varmod tibble with \code{detected_role}, \code{order},
#'                     \code{level_counts}, and \code{level_freqs} columns.
#' @param meta_json    Path to \code{*.survey_meta.json} (required).
#' @param vars         Optional character vector of var_name to restrict to.
#' @param exclude      Character vector of var_names to skip entirely.
#' @param nominal      If TRUE, also process \code{factor_nominal} variables
#'                     (default FALSE — ordinal only).
#' @param optimal_levels Integer vector of acceptable group counts.  Default
#'   \code{2:5}.  Passed as context to Haiku; used as guidance, not hard rule.
#' @param min_pct      Minimum percentage per group (integer, 0–100 scale).
#'   Default 5.  Passed as context to Haiku; Haiku may deviate with justification.
#' @param max_levels   Maximum total non-missing level count across all variables
#'   in one API chunk.  Default 250.  Controls chunking like \code{ai_suggest_labels()}.
#'   With average 3–5 levels per variable this gives 50–80 variables per chunk,
#'   enough for Haiku to detect batteries (R7).  Lower if JSON parse errors occur.
#' @param max_levels_in_single_var  Variables with more non-missing levels than
#'   this threshold are skipped (too granular for Haiku).  Default 30.
#' @param use_batch    Logical.  Use Anthropic Message Batch API (cheaper, async).
#'   Default FALSE.
#' @param dry_run      If TRUE, print prompts without calling the API.
#' @param api_key      ANTHROPIC_API_KEY env var by default.
#' @param model        Default: Haiku 4.5.
#'
#' @return Invisibly returns \code{meta_json}.  In dry_run mode returns the
#'         prompt list invisibly.
ai_merge_levels <- function(
    metadata,
    meta_json                = NULL,
    vars                     = NULL,
    exclude                  = character(),
    nominal                  = FALSE,
    optimal_levels           = 2:5,
    min_pct                  = 5L,
    max_levels               = 250L,
    max_levels_in_single_var = 30L,
    use_batch                = FALSE,
    dry_run                  = FALSE,
    api_key                  = Sys.getenv("ANTHROPIC_API_KEY"),
    model                    = "claude-haiku-4-5"
) {
  if (is.null(meta_json))
    stop("ai_merge_levels: meta_json is required. ",
         "Provide the path to your *.survey_meta.json file.")

  if (!"level_counts" %in% names(metadata))
    stop("ai_merge_levels: metadata must have level_counts column. ",
         "Run metadata_add_level_stats() first.")

  roles_to_process <- if (nominal) c("factor_ordinal", "factor_nominal")
                      else "factor_ordinal"

  target <- metadata |>
    dplyr::filter(detected_role %in% roles_to_process) |>
    dplyr::filter(!var_name %in% exclude)

  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)

  if (nrow(target) == 0L) {
    message("ai_merge_levels: No variables to process.")
    return(invisible(meta_json))
  }

  # Read meta JSON once — used to get freshest missing status per level
  existing_for_filter <- .read_meta_json(meta_json)

  # Count non-missing levels for each variable using the JSON (freshest status)
  target <- target |>
    dplyr::mutate(
      .n_valid_levels = purrr::map_int(var_name, function(vn) {
        levs <- existing_for_filter$variables[[vn]]$levels
        if (is.null(levs)) return(0L)
        sum(!purrr::map_lgl(levs, ~ isTRUE(.x$missing)))
      })
    )

  # Skip vars with too many levels
  oversized <- dplyr::filter(target, .n_valid_levels > max_levels_in_single_var)
  if (nrow(oversized) > 0L) {
    message("ai_merge_levels: ", nrow(oversized), " variable(s) skipped (> ",
            max_levels_in_single_var, " non-missing levels): ",
            paste(oversized$var_name, collapse = ", "))
    target <- dplyr::filter(target, .n_valid_levels <= max_levels_in_single_var)
  }

  # Skip vars with < 2 valid levels (nothing to merge)
  target <- dplyr::filter(target, .n_valid_levels >= 2L)

  if (nrow(target) == 0L) {
    message("ai_merge_levels: No variables remaining after filtering.")
    return(invisible(meta_json))
  }

  message("ai_merge_levels: ", nrow(target), " variable(s) to process.")

  # Build per-variable JSON input block
  .build_var_json_merge <- function(vn, var_label, detected_role,
                                    values, order_ints, level_counts, level_freqs,
                                    existing_vars) {
    levs_json <- existing_vars[[vn]]$levels
    if (is.null(levs_json)) return(NULL)

    # Order non-missing levels by order integer
    non_miss_keys <- purrr::keep(names(levs_json), ~ !isTRUE(levs_json[[.x]]$missing))
    if (length(non_miss_keys) < 2L) return(NULL)

    # Get order integers from JSON (most up-to-date after ai_classify_roles)
    ord_vals <- purrr::map_int(non_miss_keys, function(k) {
      o <- levs_json[[k]]$order
      if (is.null(o)) NA_integer_ else as.integer(o)
    })
    sorted_idx <- order(ord_vals, na.last = TRUE)
    sorted_keys <- non_miss_keys[sorted_idx]

    type_str <- if (detected_role == "factor_ordinal") "ordinal" else "nominal"
    desc_clean <- .clean_var_label_for_api(var_label, var_name = vn)
    esc <- function(x) gsub('"', '\\"', x, fixed = TRUE)

    level_entries <- purrr::map_chr(sorted_keys, function(k) {
      lev  <- levs_json[[k]]
      lbl  <- lev$label %||% ""
      n_v  <- lev$n
      pct_v <- lev$pct
      n_str   <- if (!is.null(n_v))   paste0('"n":', as.integer(n_v)) else NULL
      pct_str <- if (!is.null(pct_v)) paste0('"pct":', as.integer(pct_v)) else NULL
      fields  <- c(paste0('"key":"', esc(k), '"'),
                   paste0('"label":"', esc(lbl), '"'),
                   n_str, pct_str)
      paste0("{", paste(fields[!is.null(fields)], collapse = ","), "}")
    })

    paste0('{"var":"', esc(vn),
           '","type":"', type_str,
           '","desc":"', esc(substr(desc_clean, 1L, 120L)), '",',
           '"levels":[', paste(level_entries, collapse = ","), "]}",
           collapse = "")
  }

  var_jsons <- purrr::pmap(
    dplyr::select(target, var_name, var_label, detected_role,
                  values, dplyr::any_of(c("order", "level_counts", "level_freqs"))),
    function(var_name, var_label, detected_role, values,
             order = NULL, level_counts = integer(0), level_freqs = numeric(0)) {
      .build_var_json_merge(var_name, var_label, detected_role,
                            values, order, level_counts, level_freqs,
                            existing_for_filter$variables)
    }
  ) |> purrr::compact()

  # Attach computed json strings back to target for chunking
  var_json_map <- purrr::set_names(var_jsons, target$var_name[seq_along(var_jsons)])

  # Chunk by max_levels budget
  chunks <- local({
    chunk_ids <- integer(nrow(target))
    cid <- 1L; cumul <- 0L
    for (i in seq_len(nrow(target))) {
      n <- target$.n_valid_levels[[i]]
      if (cumul + n > max_levels && cumul > 0L) { cid <- cid + 1L; cumul <- 0L }
      chunk_ids[[i]] <- cid
      cumul <- cumul + n
    }
    split(target$var_name, chunk_ids)
  })

  build_prompt <- function(vnames) {
    jsons <- purrr::map_chr(vnames, ~ var_json_map[[.x]] %||% "")
    jsons <- jsons[nzchar(jsons)]
    if (length(jsons) == 0L) return(NULL)
    paste0('{"optimal_levels":[', min(optimal_levels), ',', max(optimal_levels),
           '],"min_pct":', min_pct, '}\n\n',
           "[\n", paste(jsons, collapse = ",\n"), "\n]")
  }
  prompts <- purrr::map(chunks, build_prompt) |> purrr::compact()

  if (length(prompts) == 0L) {
    message("ai_merge_levels: No valid prompts to send.")
    return(invisible(meta_json))
  }

  # Load system prompt from file; pass with cache_control for Haiku caching
  # (Haiku 4.5 minimum cacheable block: 2048 tokens)
  prompt_path <- file.path(getwd(), "instructions", "merge_levels_prompt.md")
  if (!file.exists(prompt_path)) {
    pkg_path <- system.file("instructions/merge_levels_prompt.md",
                            package = utils::packageName() %||% "")
    if (nzchar(pkg_path) && file.exists(pkg_path)) prompt_path <- pkg_path
  }
  if (!file.exists(prompt_path))
    stop("ai_merge_levels: instructions/merge_levels_prompt.md not found.")

  system_prompt_text <- paste(
    readLines(prompt_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  # Strip nominal-only sections when nominal = FALSE to avoid sending unused rules to the API.
  if (!nominal) {
    system_prompt_text <- gsub(
      "<!-- BEGIN_NOMINAL_ONLY -->[\\s\\S]*?<!-- END_NOMINAL_ONLY -->",
      "",
      system_prompt_text,
      perl = TRUE
    )
    system_prompt_text <- gsub("\n{3,}", "\n\n", system_prompt_text, perl = TRUE)
    system_prompt_text <- trimws(system_prompt_text)
  }
  # Compact input example JSON blocks so Haiku sees the same compact format in
  # examples as in the real user messages it receives (output blocks kept pretty).
  system_prompt_text <- .compact_example_json_blocks(system_prompt_text)

  # System prompt as cacheable content block list (Anthropic extended-cache beta)
  system_prompt_cached <- list(
    list(type = "text", text = system_prompt_text,
         cache_control = list(type = "ephemeral"))
  )

  # Each level needs ~20 tokens in output (key + order integer in JSON).
  # Add 20% headroom. Floor at 1024 to handle fixed overhead.
  max_tok <- max(1024L, ceiling(max_levels * 20L * 1.5))

  # Dry run
  if (dry_run) {
    message(strrep("=", 60))
    message("DRY RUN — no API call made")
    message(strrep("=", 60))
    message("Variables: ", nrow(target), "  |  Chunks: ", length(prompts),
            "  |  Levels budget: ", max_levels, " per chunk",
            "  |  Route: ", if (use_batch) "batch" else "synchronous",
            "  |  max_tokens: ", max_tok)
    message("\n", strrep("-", 60))
    message("SYSTEM PROMPT")
    message(strrep("-", 60))
    cat(system_prompt_text, "\n")
    purrr::iwalk(prompts, function(p, i) {
      message("\n", strrep("-", 60))
      message("USER MESSAGE ", i, "/", length(prompts))
      message(strrep("-", 60))
      cat(p, "\n")
    })
    message(strrep("=", 60))
    return(invisible(prompts))
  }

  # API route
  if (!use_batch) {
    message("ai_merge_levels: synchronous (", nrow(target), " var(s), ",
            length(prompts), " chunk(s))")
    results_text <- purrr::imap(prompts, function(p, i) {
      message("  Chunk ", i, "/", length(prompts))
      resp <- ai_call_claude(p, model = model, api_key = api_key,
                             system = system_prompt_cached,
                             max_tokens = max_tok)
      resp$content[[1]]$text
    })
  } else {
    message("ai_merge_levels: batch mode (", nrow(target), " var(s))")
    requests <- purrr::imap(prompts, ~ list(custom_id = paste0("merge_", .y),
                                            prompt     = .x))
    batch <- ai_batch_submit(requests, model = model, api_key = api_key,
                             system = system_prompt_cached,
                             max_tokens = max_tok)
    message("Batch submitted. ID: ", batch$id)
    raw <- ai_batch_retrieve(batch$id, api_key = api_key)
    results_text <- purrr::map(purrr::set_names(names(raw)), ~ raw[[.x]])
  }

  # Parse response: each chunk may be wrapped in markdown fences; extract the
  # outermost {...} from each response individually, then merge all results.
  parsed <- list()
  for (txt in results_text) {
    if (is.null(txt) || !nzchar(txt)) next
    tryCatch({
      json_str <- stringr::str_extract(txt, "(?s)\\{.+\\}")
      if (is.na(json_str)) stop("No JSON object found")
      chunk_parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      parsed <- c(parsed, chunk_parsed)
    }, error = function(e) {
      warning("ai_merge_levels: JSON parse error: ", conditionMessage(e))
    })
  }

  if (length(parsed) == 0L) {
    warning("ai_merge_levels: No valid JSON response to apply.")
    return(invisible(meta_json))
  }

  # Apply merged order integers to JSON and metadata
  .backup_meta_json(meta_json, "merge_levels")
  existing <- .read_meta_json(meta_json)

  n_updated  <- 0L
  n_binary   <- 0L
  bad_vars   <- character(0)

  for (vn in names(parsed)) {
    if (is.null(existing$variables[[vn]])) next
    groups_raw <- parsed[[vn]]   # list of {order, keys}

    # Validate: must be a list of groups each with order + keys
    if (!is.list(groups_raw) || length(groups_raw) == 0L) {
      bad_vars <- c(bad_vars, vn); next
    }
    valid_groups <- purrr::keep(groups_raw, function(g)
      !is.null(g$order) && !is.null(g$keys) && length(g$keys) > 0L)
    if (length(valid_groups) == 0L) { bad_vars <- c(bad_vars, vn); next }

    # Build key → new_order_int map
    key_order_map <- character(0)
    for (g in valid_groups) {
      ord_int <- as.integer(g$order)
      for (k in g$keys) key_order_map[as.character(k)] <- ord_int
    }

    # Get non-missing level keys currently in JSON
    levs <- existing$variables[[vn]]$levels
    non_miss_keys <- purrr::keep(names(levs), ~ !isTRUE(levs[[.x]]$missing))

    # Validate coverage: every non-missing input key must appear in response
    missing_from_response <- setdiff(non_miss_keys, names(key_order_map))
    extra_in_response     <- setdiff(names(key_order_map), non_miss_keys)
    if (length(missing_from_response) > 0L || length(extra_in_response) > 0L) {
      warning("ai_merge_levels: key mismatch for '", vn, "' — skipping. ",
              if (length(missing_from_response) > 0L)
                paste0("Missing from response: ", paste(missing_from_response, collapse = ","), ". "),
              if (length(extra_in_response) > 0L)
                paste0("Extra in response: ", paste(extra_in_response, collapse = ","), "."))
      next
    }

    # Write new order integers into JSON
    for (k in non_miss_keys) {
      new_ord <- key_order_map[k]
      if (!is.na(new_ord))
        existing$variables[[vn]]$levels[[k]]$order <- as.integer(new_ord)
    }

    # Detect binary collapse: if Haiku returned exactly 2 distinct order values
    new_orders_distinct <- length(unique(as.integer(key_order_map[non_miss_keys])))
    meta_idx <- which(metadata$var_name == vn)
    if (new_orders_distinct == 2L &&
        length(meta_idx) == 1L &&
        metadata$detected_role[[meta_idx]] == "factor_ordinal") {
      existing$variables[[vn]]$role <- "factor_binary"
      metadata$detected_role[[meta_idx]] <- "factor_binary"
      n_binary <- n_binary + 1L
    }

    n_updated <- n_updated + 1L
  }

  .write_meta_json(existing, meta_json)

  message("\n", strrep("=", 60))
  message("ai_merge_levels: ", n_updated, " variable(s) updated in: ", meta_json)
  if (n_binary > 0L)
    message("  ", n_binary, " variable(s) recoded to factor_binary (collapsed to 2 groups).")
  if (length(bad_vars) > 0L)
    message("  [!] Malformed response for: ", paste(bad_vars, collapse = ", "))
  message("Review order fields in the JSON, then re-run extract_survey_metadata().")
  message("Next step: ai_suggest_labels() to name the merged groups.")
  message(strrep("=", 60))

  invisible(meta_json)
}


# ============================================================
# 10. ai_suggest_labels()
# ============================================================

#' Use Haiku to suggest concise French factor level labels
#'
#' Sends factor variables to Claude (as JSON) and asks it to shorten all factor
#' level labels to <= 30 characters.
#'
#' Numeric label prefixes ("1-", "01-") are NOT sent to Haiku — they are rebuilt
#' after merging based on the number of remaining distinct levels.
#'
#' ## Ordering and merging
#'   Levels are sorted by the \code{order} integer stored in
#'   \code{metadata$order} (set by \code{ai_classify_roles()} and optionally
#'   refined by \code{metadata_merge_ordinal_levels()}).  Levels sharing the
#'   same \code{order} integer are collapsed into one entry sent to Haiku
#'   (label = original labels joined by " / "). Run
#'   \code{metadata_merge_ordinal_levels()} before this function to pre-compute
#'   merge groups. factor_nominal levels are sorted alphabetically if no
#'   \code{order} is available.
#'
#' ## max_levels vs use_batch
#'   max_levels controls how many non-null factor levels go into one API request.
#'   For ordinal/binary variables the count used is the number of distinct
#'   \code{order} integers (= entries sent to Haiku), which is typically smaller
#'   than the raw level count when merges have been applied.
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
#' @param metadata     Varmod tibble with an \code{order} list-column.
#' @param vars         Optional character vector of var_name to restrict to.
#' @param meta_json    Path to \code{*.survey_meta.json} (required).
#' @param max_levels   Maximum total non-null level entries per API request.
#'   Default 150. Variables whose individual entry count exceeds max_levels are
#'   skipped with a warning.
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
    meta_json     = NULL,
    max_levels               = 150L,
    max_levels_in_single_var = 30L,
    replace_existing_new_labels = FALSE,
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

  has_order_col <- "order" %in% names(metadata)

  # ---------- filter target variables --------------------------------------
  target <- metadata |>
    dplyr::filter(detected_role %in% c("factor_binary", "factor_nominal",
                                        "factor_ordinal"))
  if (!is.null(vars)) target <- dplyr::filter(target, var_name %in% vars)
  if (nrow(target) == 0) {
    message("ai_suggest_labels: No factor variables to process.")
    return(invisible(meta_json))
  }

  # ---------- build send permutation and group IDs from order col -----------
  # For ordinal/binary: sort non-missing levels by order integer ascending.
  # Levels sharing the same order integer = one merged entry sent to Haiku.
  # For nominal (or when order col absent): identity permutation.
  #
  # .send_order: integer vector (length = n levels), gives the position in the
  #   sorted sequence for each stored level. NA-coded levels keep their index.
  # .merge_groups: integer vector parallel to new_labels; levels with the same
  #   integer belong to the same merge group. NULL-coded levels get a unique id.
  target <- target |>
    dplyr::mutate(
      .ord_for_send = if (has_order_col) order else vector("list", dplyr::n()),
      .send_order = purrr::pmap(
        list(detected_role, new_labels, .ord_for_send),
        function(role, nls, ord_ints) {
          idx <- seq_along(nls)
          if ((role %in% c("factor_ordinal", "factor_binary")) &&
              length(ord_ints) == length(nls)) {
            # Sort non-missing (non-NULL) levels by order integer
            non_null <- which(nls != "NULL")
            if (length(non_null) >= 2L && !all(is.na(ord_ints[non_null]))) {
              sorted_pos        <- non_null[order(ord_ints[non_null], na.last = TRUE)]
              idx[non_null]     <- sorted_pos
            }
          }
          idx
        }
      ),
      .merge_groups = purrr::pmap(
        list(detected_role, new_labels, .ord_for_send, .send_order),
        function(role, nls, ord_ints, send_ord) {
          n_tot       <- length(nls)
          groups_full <- seq_len(n_tot)   # default: each level is its own group

          if ((role %in% c("factor_ordinal", "factor_binary")) &&
              length(ord_ints) == n_tot) {
            # Map order integers to group IDs in send order
            non_null_send <- which(nls[send_ord] != "NULL")
            if (length(non_null_send) >= 2L) {
              ords_send <- ord_ints[send_ord][non_null_send]
              if (!all(is.na(ords_send))) {
                # Convert unique order integers to consecutive group IDs
                uniq_ords <- unique(ords_send[!is.na(ords_send)])
                grp_map   <- purrr::set_names(seq_along(uniq_ords), uniq_ords)
                grp_ids   <- grp_map[as.character(ords_send)]
                grp_ids[is.na(ords_send)] <- max(uniq_ords) + seq_len(sum(is.na(ords_send)))
                groups_send <- seq_len(n_tot)
                groups_send[non_null_send] <- grp_ids
                # Un-permute back to stored order
                inv_ord     <- order(send_ord)
                groups_full <- groups_send[inv_ord]
              }
            }
          }
          groups_full
        }
      ),
      .n_levels = purrr::pmap_int(
        list(detected_role, new_labels, .merge_groups),
        function(role, nls, grps) {
          non_null <- nls != "NULL"
          if (role %in% c("factor_ordinal", "factor_binary")) {
            length(unique(grps[non_null]))
          } else {
            sum(non_null)
          }
        }
      )
    )

  # Skip vars with too many levels in a single variable (cost-saving guard)
  oversized_single <- dplyr::filter(target, .n_levels > max_levels_in_single_var)
  if (nrow(oversized_single) > 0L) {
    message("ai_suggest_labels: ", nrow(oversized_single), " variable(s) skipped (> ",
            max_levels_in_single_var, " non-null level/group count): ",
            paste(oversized_single$var_name, collapse = ", "))
    target <- dplyr::filter(target, .n_levels <= max_levels_in_single_var)
  }
  if (nrow(target) == 0L) {
    message("ai_suggest_labels: No variables remaining after filtering by max_levels_in_single_var.")
    return(invisible(meta_json))
  }

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

  # ---------- skip fully-labeled variables (unless replace_existing_new_labels) --
  if (!replace_existing_new_labels && !is.null(meta_json) && file.exists(meta_json)) {
    existing_vars <- .read_meta_json(meta_json)$variables
    fully_labeled <- names(Filter(function(v) {
      levs <- v$levels
      if (is.null(levs) || length(levs) == 0L) return(FALSE)
      non_miss <- Filter(function(l) !isTRUE(l$missing), levs)
      length(non_miss) > 0L &&
        all(purrr::map_lgl(non_miss, ~ !is.null(.x$new_label)))
    }, existing_vars))
    if (length(fully_labeled) > 0L) {
      n_skip <- sum(target$var_name %in% fully_labeled)
      if (n_skip > 0L)
        message("ai_suggest_labels: ", n_skip,
                " variable(s) already fully labeled — skipped. ",
                "Use replace_existing_new_labels = TRUE to reprocess.")
      target <- dplyr::filter(target, !var_name %in% fully_labeled)
    }
    if (nrow(target) == 0L) {
      message("ai_suggest_labels: All factor variables already labeled.")
      return(invisible(meta_json))
    }
  }

  # ---------- JSON builder for one variable ---------------------------------
  .build_var_json <- function(var_name, var_label, detected_role,
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
    var_label_clean <- .clean_var_label_for_api(var_label, var_name = var_name)

    # ---- For ordinal/binary: collapse groups before sending to AI -----------
    # Each group is one entry: key = first value code of the group (in send-order),
    # label = original labels joined by " / ", counts/freqs summed.
    # .parse_labels_json_responses() matches AI output by that key and expands
    # the label back to all member levels.
    if (detected_role %in% c("factor_ordinal", "factor_binary") &&
        length(unique(groups_keep)) < length(groups_keep)) {
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
                    # '","desc":"', esc(substr(var_label_clean, 1, 120)), '",', # Trim at 120 chars
                    '","desc":"', esc(var_label_clean), '",',
                    '"levels":', levels_json)

      return(paste0(obj, "}"))
    }

    # ---- Non-ordinal or ordinal with no merging: send raw levels ------------
    kv_pairs    <- paste0('"', esc(values_keep), '":"', esc(labels_keep), '"')
    levels_json <- paste0("{", paste(kv_pairs, collapse = ", "), "}")

    obj <- paste0('{"var":"', esc(var_name), '","type":"', type_str,
                  # '","desc":"', esc(substr(var_label_clean, 1, 120)), '",', # Trim at 120 chars
                  '","desc":"', esc(var_label_clean), '",',
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
      dplyr::select(chunk_df, var_name, var_label, detected_role,
                    values, labels, new_labels, .send_order, .merge_groups,
                    dplyr::any_of(c("level_counts", "level_freqs"))),
      function(var_name, var_label, detected_role,
               values, labels, new_labels, .send_order, .merge_groups,
               level_counts = integer(0), level_freqs = numeric(0)) {
        .build_var_json(var_name, var_label, detected_role,
                        values, labels, new_labels, .send_order, .merge_groups,
                        level_counts, level_freqs)
      }
    ) |> purrr::compact()

    if (length(json_objects) == 0) return(NULL)

    paste0("[\n", paste(json_objects, collapse = ",\n"), "\n]")
  }

  # Warn about variables that will be silently dropped (empty/all-NULL new_labels)
  zero_level_vars <- target[target$.n_levels == 0L, ]
  if (nrow(zero_level_vars) > 0L) {
    sample_vars <- head(zero_level_vars$var_name, 5L)
    message("ai_suggest_labels: ", nrow(zero_level_vars),
            " variable(s) have 0 sendable levels (new_labels empty or all NULL) — ",
            "they will be absent from the prompt. ",
            "Sample: ", paste(sample_vars, collapse = ", "),
            if (nrow(zero_level_vars) > 5L) paste0(" ... (", nrow(zero_level_vars) - 5L, " more)") else "",
            ". Check new_labels column in your metadata tibble for these variables.")
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
            "  |  Max levels/var: ", max_levels_in_single_var,
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
#       "1": { "order": 1, "label": "Oui, choisie", "new_label": "Choisie", "n": 451, "pct": 61 },
#       "9": { "missing": true, "label": "NSP" }
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
    is_missing_vec <- purrr::map_lgl(levels, ~ isTRUE(.x[["missing"]]))
    has_order     <- any(purrr::map_lgl(levels, ~ !is.null(.x[["order"]])))
    has_new_label <- any(purrr::map_lgl(levels, ~ !isTRUE(.x[["missing"]]) && !is.null(.x[["new_label"]])))
    has_n         <- any(purrr::map_lgl(levels, ~ !is.null(.x[["n"]])))
    has_pct       <- any(purrr::map_lgl(levels, ~ !is.null(.x[["pct"]])))

    val_keys   <- names(levels)
    f_key      <- paste0('"', purrr::map_chr(val_keys, esc), '"')
    f_label    <- purrr::map_chr(levels, function(lev)
                    paste0('"', esc(lev[["label"]]), '"'))
    f_order    <- if (has_order) purrr::map_chr(levels, function(lev)
                    if (!isTRUE(lev[["missing"]]) && !is.null(lev[["order"]]))
                      as.character(as.integer(lev[["order"]]))
                    else "") else NULL
    f_new_lbl  <- if (has_new_label) purrr::map_chr(levels, function(lev)
                    if (!isTRUE(lev[["missing"]]) && !is.null(lev[["new_label"]]))
                      paste0('"', esc(lev[["new_label"]]), '"') else '""') else NULL
    f_n        <- if (has_n) purrr::map_chr(levels, function(lev) {
                    v <- lev[["n"]]
                    if (!is.null(v) && length(v) == 1L && !is.na(v)) as.character(as.integer(v)) else ""
                  }) else NULL
    f_pct      <- if (has_pct) purrr::map_chr(levels, function(lev) {
                    v <- lev[["pct"]]
                    if (!is.null(v) && length(v) == 1L && !is.na(v)) as.character(as.integer(v)) else ""
                  }) else NULL

    # ---- column widths (max across all levels of this variable) ------------
    w_key   <- max(nchar(f_key,   type = "chars"))
    w_order <- if (has_order) {
      non_empty <- f_order[nzchar(f_order)]
      if (length(non_empty) > 0) max(nchar(non_empty, type = "chars")) else 1L
    } else 0L
    w_label <- max(nchar(f_label, type = "chars"))
    w_new   <- if (has_new_label) max(nchar(f_new_lbl, type = "chars")) else 0L
    w_n     <- if (has_n) { ne <- f_n[nzchar(f_n)];     if (length(ne) > 0) max(nchar(ne,  type = "chars")) else 1L } else 0L
    w_pct   <- if (has_pct) { ne <- f_pct[nzchar(f_pct)]; if (length(ne) > 0) max(nchar(ne,  type = "chars")) else 1L } else 0L

    # ---- assemble one line per level ---------------------------------------
    level_lines <- character(n_lev)
    for (i in seq_len(n_lev)) {
      lev     <- levels[[i]]
      is_miss <- is_missing_vec[[i]]

      tokens <- character(0)
      if (has_order && !is_miss && !is.null(lev[["order"]])) {
        ord_str <- formatC(f_order[[i]], width = w_order, flag = " ")
        tokens  <- c(tokens, paste0('"order": ', ord_str))
      }
      if (is_miss) tokens <- c(tokens, '"missing": true')
      tokens <- c(tokens, paste0('"label": ', rpad(f_label[[i]], w_label)))
      if (has_new_label && !is_miss)
        tokens <- c(tokens, paste0('"new_label": ', rpad(f_new_lbl[[i]], w_new)))
      if (!is_miss) {
        # n and pct: right-aligned to their column width
        if (has_n && !is.null(lev[["n"]]))
          tokens <- c(tokens, paste0('"n": ', formatC(f_n[[i]], width = w_n, flag = " ")))
        if (has_pct && !is.null(lev[["pct"]]))
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

  # Warn about variables that were sent but absent from AI response
  missing_from_resp <- setdiff(unique(target$var_name), names(raw_map))
  if (length(missing_from_resp) > 0)
    message("ai_suggest_labels: ", length(missing_from_resp),
            " variable(s) absent from AI response (no label written): ",
            paste(missing_from_resp, collapse = ", "))

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

      if (role %in% c("factor_ordinal", "factor_binary") &&
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
#       "1": { "order": 1, "label": "Oui — choisie",  "new_label": "Choisie", "n": 451, "pct": 61 },
#       "9": { "missing": true, "label": "NSP / NR" }
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
            entry[["missing"]] <- TRUE
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
# Only updates the "levels" sub-object; preserves var_label, role,
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
          # Keep existing label; update new_label, n, pct, missing
          if (!is.null(new_lev$new_label))
            existing_levels[[val_code]]$new_label <- new_lev$new_label
          if (!is.null(new_lev$n))
            existing_levels[[val_code]]$n <- new_lev$n
          if (!is.null(new_lev$pct))
            existing_levels[[val_code]]$pct <- new_lev$pct
          if (isTRUE(new_lev$missing))
            existing_levels[[val_code]]$missing <- TRUE
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
# Sync "missing": true flags in the JSON variables section with freshly-computed
# missing_vals from extract_survey_metadata(). Called on every re-run when
# meta_json already exists, so new missing_chr entries take effect in the JSON.
# Returns the updated json_vars list (unchanged object if no differences found).
.update_missing_in_meta_json <- function(json_vars, metadata) {
  for (vname in names(json_vars)) {
    row <- metadata[metadata$var_name == vname, ]
    if (nrow(row) == 0) next
    miss_vals <- as.character(row$missing_vals[[1]])
    levels    <- json_vars[[vname]]$levels
    if (is.null(levels) || length(levels) == 0) next

    for (val_code in names(levels)) {
      should_be_miss <- val_code %in% miss_vals
      currently_miss <- isTRUE(levels[[val_code]]$missing)
      if (should_be_miss && !currently_miss) {
        json_vars[[vname]]$levels[[val_code]]$missing <- TRUE
        # Remove "order" from newly-missing levels
        json_vars[[vname]]$levels[[val_code]]$order <- NULL
      } else if (!should_be_miss && currently_miss) {
        json_vars[[vname]]$levels[[val_code]]$missing <- NULL
        # Assign a sequential order if not already set
        if (is.null(json_vars[[vname]]$levels[[val_code]]$order)) {
          non_miss_codes <- names(levels)[!purrr::map_lgl(levels, ~ isTRUE(.x$missing))]
          next_ord <- length(non_miss_codes) + 1L
          json_vars[[vname]]$levels[[val_code]]$order <- next_ord
        }
      }
    }
  }
  json_vars
}

# ---------------------------------------------------------------------------
# Build an empty metadata tibble with correct column types from JSON variables.
# Used by extract_survey_metadata() when df is NULL (reconstruction from JSON).
.skeleton_meta_from_json <- function(json_vars) {
  n <- length(json_vars)
  tibble::tibble(
    var_name        = names(json_vars),
    var_label       = rep("", n),
    r_class         = rep(NA_character_, n),
    n_distinct      = rep(NA_integer_, n),
    n_distinct_data = rep(NA_integer_, n),
    detected_role   = rep("", n),
    values          = replicate(n, character(0), simplify = FALSE),
    labels          = replicate(n, character(0), simplify = FALSE),
    missing_vals    = replicate(n, character(0), simplify = FALSE),
    new_labels      = replicate(n, character(0), simplify = FALSE),
    new_name        = names(json_vars),
    order           = replicate(n, integer(0), simplify = FALSE)
  )
}


# ---------------------------------------------------------------------------
# Apply a unified survey_meta.json variables section to a metadata table.
# Called internally by extract_survey_metadata() when meta_json is supplied.
# json_vars: the $variables list read from .read_meta_json() (already in memory).
# Updates: var_label, detected_role, r_class, new_name, values, labels, missing_vals,
#          new_labels (from levels[*].new_label / missing), order (from levels[*].order),
#          level_counts/freqs (from n/pct) — all matched by variable name + value code.
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

    # var_label
    vl <- entry[["var_label"]]
    if (!is.null(vl) && nzchar(vl))
      out$var_label <- vl

    # detected_role (from JSON "role")
    role_val <- entry[["role"]]
    if (!is.null(role_val) && nzchar(role_val))
      out$detected_role <- role_val

    # r_class (absent in old JSONs → no override)
    rc <- entry[["r_class"]]
    if (!is.null(rc) && nzchar(rc))
      out$r_class <- rc

    # new_labels, order, level stats from levels block (matched by value code)
    levels_obj <- entry[["levels"]]
    if (!is.null(levels_obj) && length(levels_obj) > 0) {
      meta_vals <- as.character(row$values[[1]])

      # Skeleton row: when values is empty, derive from JSON keys
      if (length(meta_vals) == 0) {
        meta_vals <- names(levels_obj)
        out$values       <- list(meta_vals)
        out$missing_vals <- list(meta_vals[purrr::map_lgl(levels_obj, ~ isTRUE(.x$missing))])
        out$labels       <- list(unname(purrr::map_chr(levels_obj, ~ as.character(.x$label %||% ""))))
      }

      # labels: restore original human-readable labels from JSON
      has_label <- any(purrr::map_lgl(levels_obj, ~ !is.null(.x$label)))
      if (has_label && !"labels" %in% names(out)) {
        labs <- purrr::map_chr(meta_vals, function(v) {
          lev <- levels_obj[[v]]
          if (!is.null(lev) && !is.null(lev$label)) as.character(lev$label) else ""
        })
        out$labels <- list(labs)
      }

      # new_labels: use new_label from JSON; missing:true levels → "NULL" sentinel
      has_new  <- any(purrr::map_lgl(levels_obj, ~ !is.null(.x$new_label)))
      has_miss <- any(purrr::map_lgl(levels_obj, ~ isTRUE(.x$missing)))
      if (has_new || has_miss) {
        nls <- row$new_labels[[1]]
        for (i in seq_along(meta_vals)) {
          lev <- levels_obj[[meta_vals[[i]]]]
          if (!is.null(lev)) {
            if (isTRUE(lev$missing))
              nls[[i]] <- "NULL"
            else if (!is.null(lev$new_label))
              nls[[i]] <- as.character(lev$new_label)
          }
        }
        out$new_labels <- list(nls)
      }

      # order: read per-level "order" integer; missing levels → NA_integer_
      has_order <- any(purrr::map_lgl(levels_obj, ~ !is.null(.x$order)))
      if (has_order || has_miss) {
        ord <- purrr::map_int(meta_vals, function(v) {
          lev <- levels_obj[[v]]
          if (is.null(lev) || isTRUE(lev$missing)) return(NA_integer_)
          if (!is.null(lev$order)) as.integer(lev$order) else NA_integer_
        })
        out$order <- list(ord)
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
  }) |> purrr::compact()

  if (length(update_rows) == 0) return(metadata)

  # Apply updates column-by-column to avoid bind_rows() padding missing list-columns
  # with list(NULL), which would silently overwrite existing values (e.g. new_labels).
  # Each column is updated only for the rows that actually have that column set.
  all_cols <- c("var_label", "detected_role", "r_class",
                "new_name", "values", "labels", "missing_vals",
                "new_labels", "order", "level_counts", "level_freqs")
  n_updated <- length(update_rows)

  has_nl  <- FALSE; has_nn <- FALSE; has_st <- FALSE; has_ord <- FALSE

  for (col in all_cols) {
    rows_with_col <- purrr::keep(update_rows, ~ col %in% names(.x))
    if (length(rows_with_col) == 0) next

    # Build a two-column tibble: var_name + this column only.
    # Use select() on each row-tibble to avoid bind_rows() padding other columns.
    col_df <- dplyr::bind_rows(purrr::map(rows_with_col,
                                           ~ dplyr::select(.x, dplyr::all_of(c("var_name", col)))))

    # Ensure list columns exist in metadata before rows_update
    if (col %in% c("level_counts", "level_freqs", "order",
                    "new_labels", "values", "labels", "missing_vals") &&
        !col %in% names(metadata)) {
      metadata[[col]] <- vector("list", nrow(metadata))
      metadata[[col]][] <- list(switch(col,
        level_counts = integer(0), level_freqs = numeric(0),
        order = integer(0), new_labels = character(0),
        values = character(0), labels = character(0),
        missing_vals = character(0)))
    }

    metadata <- dplyr::rows_update(metadata, col_df, by = "var_name",
                                   unmatched = "ignore")

    if (col == "new_labels")    has_nl  <- TRUE
    if (col == "new_name")      has_nn  <- TRUE
    if (col == "level_counts")  has_st  <- TRUE
    if (col == "order")         has_ord <- TRUE
  }

  message("metadata_apply_meta_json: ", n_updated, " variable(s) updated",
          if (has_nn)  paste0(" (new_name: ",  n_updated, ")")  else "",
          if (has_nl)  paste0(" (new_labels: ", n_updated, ")")  else "",
          if (has_ord) paste0(" (order: ",      n_updated, ")")  else "",
          if (has_st)  paste0(" (stats: ",      n_updated, ")")  else "")

  metadata
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
      vals   <- row$values[[1]]
      labs   <- row$labels[[1]]
      mnulls <- row$missing_vals[[1]]
      # order list-col: use existing if present, otherwise NULL (sequential default)
      ord_vec <- if ("order" %in% names(row)) row$order[[1]] else NULL

      # Build levels: sequential "order" for non-missing, "missing": true for missing
      levels_list <- if (length(vals) > 0) {
        vals_chr    <- as.character(vals)
        valid_pos   <- 0L  # counter for sequential order among non-missing levels
        purrr::set_names(
          purrr::imap(vals_chr, function(v, idx) {
            is_miss <- v %in% as.character(mnulls)
            entry   <- list(label = labs[[idx]])
            if (is_miss) {
              entry$missing <- TRUE
            } else {
              # Use existing order value if available, else sequential
              ord_val <- if (!is.null(ord_vec) && idx <= length(ord_vec) && !is.na(ord_vec[[idx]]))
                as.integer(ord_vec[[idx]])
              else {
                valid_pos <<- valid_pos + 1L
                valid_pos
              }
              entry$order <- ord_val
            }
            entry
          }),
          vals_chr
        )
      } else list()

      list(
        var_label = row$var_label,
        role      = role,
        r_class   = row$r_class,
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



# ============================================================
# 12. generate_format_script()
# ============================================================

# --- Internal helpers (prefixed .gfs_) ---

#' Zero-padded numeric prefix for factor level ordering.
#' Ensures alphanumeric sort via sort() matches the intended order.
#'
#' @param order_val  Integer: the order position (1, 2, 3…).
#' @param max_order  Integer: maximum order value among non-missing levels.
#' @return Character: "1-" (max≤9), "01-" (max≤99), "001-" (max≤999), etc.
#'
#' Examples:
#'   .gfs_numeric_prefix(3, 7)   => "3-"
#'   .gfs_numeric_prefix(3, 12)  => "03-"
#'   .gfs_numeric_prefix(3, 150) => "003-"
.gfs_numeric_prefix <- function(order_val, max_order) {
  width <- nchar(as.character(max_order))
  paste0(formatC(order_val, width = width, flag = "0"), "-")
}


#' Compute summary statistics for a numeric column, excluding missing codes.
#'
#' @param col          A vector (possibly haven_labelled).
#' @param missing_codes Character vector of value codes marked as missing in JSON.
#' @return Named list (min, max, mean, sd, q1, median, q3) or NULL if all NA.
.gfs_compute_numeric_stats <- function(col, missing_codes) {
  x <- suppressWarnings(as.numeric(as.character(col)))
  # Remove values matching missing codes (as numeric)
  miss_num <- suppressWarnings(as.numeric(missing_codes))
  miss_num <- miss_num[!is.na(miss_num)]
  if (length(miss_num) > 0) x[x %in% miss_num] <- NA
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NULL)
  qs <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  list(
    min    = min(x),
    max    = max(x),
    mean   = mean(x),
    sd     = sd(x),
    q1     = unname(qs[1]),
    median = unname(qs[2]),
    q3     = unname(qs[3])
  )
}


#' Build a normalized list of variable entries from JSON + metadata.
#'
#' Returns a list in the original variable order (metadata row order), where
#' each element is a list with fields: orig_name, new_name, var_label, role,
#' r_class, levels_sorted (non-missing, by order ascending), missing_levels,
#' n_non_missing, max_order.
#'
#' Each level entry has: code, order, display_label, orig_label, n, pct.
#'
#' @param json_vars  Named list from JSON$variables.
#' @param metadata   Metadata tibble with var_name, r_class, detected_role, etc.
#' @return List of normalized entry lists.
.gfs_build_entries <- function(json_vars, metadata) {
  entries <- list()
  for (i in seq_len(nrow(metadata))) {
    vname <- metadata$var_name[i]
    jv    <- json_vars[[vname]]
    if (is.null(jv)) next

    role     <- jv$role %||% metadata$detected_role[i]
    new_name <- jv$new_name %||% vname
    var_label <- jv$var_label %||% ""
    r_class  <- metadata$r_class[i]

    # Parse levels
    lvls <- jv$levels
    non_missing <- list()
    missing_lvls <- list()

    if (length(lvls) > 0) {
      for (code in names(lvls)) {
        lv <- lvls[[code]]
        if (isTRUE(lv$missing)) {
          missing_lvls[[length(missing_lvls) + 1]] <- list(
            code       = code,
            orig_label = lv$label %||% ""
          )
        } else {
          display <- lv$new_label %||% lv$label %||% code
          non_missing[[length(non_missing) + 1]] <- list(
            code          = code,
            order         = lv$order %||% NA_integer_,
            display_label = display,
            orig_label    = lv$label %||% "",
            n             = lv$n,
            pct           = lv$pct
          )
        }
      }
    }

    # Sort non-missing by order ascending
    if (length(non_missing) > 0) {
      orders <- sapply(non_missing, function(x) x$order)
      non_missing <- non_missing[order(orders)]
    }

    max_order <- if (length(non_missing) > 0) {
      max(sapply(non_missing, function(x) x$order), na.rm = TRUE)
    } else {
      0L
    }

    entries[[length(entries) + 1]] <- list(
      orig_name      = vname,
      new_name       = new_name,
      var_label      = var_label,
      role           = role,
      r_class        = r_class,
      levels_sorted  = non_missing,
      missing_levels = missing_lvls,
      n_non_missing  = length(non_missing),
      max_order      = as.integer(max_order)
    )
  }
  entries
}


#' Right-pad a string with spaces to a given width.
#' @param s Character string.
#' @param w Target width.
#' @return Padded string (unchanged if already >= w).
.gfs_rpad <- function(s, w) {
  n <- nchar(s)
  ifelse(n < w, paste0(s, strrep(" ", w - n)), s)
}


#' Generate the codebook section (Part 1: variable list).
#'
#' Produces a character vector of R code lines forming the c(...) block.
#' See plan for per-role formatting rules.
#'
#' @param entries   List from .gfs_build_entries().
#' @param stats     Named list of numeric stats (keyed by orig_name), or NULL.
#' @return Character vector of lines.
.gfs_codebook_lines <- function(entries, stats = NULL) {
  if (length(entries) == 0) return(character(0))

  # --- compute global padding width for variable name column ---
  # Width = max of '  "NEW_NAME",' across all entries
  name_widths <- sapply(entries, function(e) nchar(paste0('  "', e$new_name, '",')))
  w_name <- max(name_widths)

  # --- for binary factors: compute padding for first level display ---
  # Collect first-level display strings for consecutive binary alignment
  # (We'll compute these per-entry below)

  lines <- character(0)
  n_entries <- length(entries)

  for (idx in seq_len(n_entries)) {
    e <- entries[[idx]]
    is_last <- (idx == n_entries)

    # Name column: with or without trailing comma
    # Always pad to w_name so the # comment aligns across all entries
    if (is_last) {
      name_str <- paste0('  "', e$new_name, '"')
      name_str <- .gfs_rpad(name_str, w_name)
    } else {
      name_str <- paste0('  "', e$new_name, '",')
      name_str <- .gfs_rpad(name_str, w_name)
    }

    # Build prefixed labels for this variable (reused in codebook + formatting)
    prefixed <- character(0)
    if (e$n_non_missing > 0) {
      prefixed <- sapply(e$levels_sorted, function(lv) {
        paste0(.gfs_numeric_prefix(lv$order, e$max_order), lv$display_label)
      })
    }

    # Orig name suffix (omit if same as new_name)
    orig_suffix <- if (e$orig_name != e$new_name) paste0(" ", e$orig_name) else ""

    # Missing labels
    miss_str <- ""
    if (length(e$missing_levels) > 0) {
      miss_labels <- sapply(e$missing_levels, function(m) paste0('"', m$orig_label, '"'))
      miss_str <- paste0(", missing: ", paste(miss_labels, collapse = ","))
    }

    is_factor <- grepl("^factor_", e$role)

    if (is_factor && e$role != "factor_binary" && e$n_non_missing > 0) {
      # --- ordinal / nominal / unique_value : 2 lines ---
      # Line 1: var_label, orig_name, missing
      comment1 <- paste0('"', e$var_label, '"', orig_suffix, miss_str)
      line1 <- paste0(name_str, " # ", comment1)

      # Line 2: continuation with prefixed levels
      cont_prefix <- .gfs_rpad("  #", w_name)
      level_parts <- sapply(seq_along(prefixed), function(li) {
        lv <- e$levels_sorted[[li]]
        n_str <- if (!is.null(lv$n)) paste0(" n=", lv$n) else ""
        p_str <- if (!is.null(lv$pct)) paste0(" ", lv$pct, "%") else ""
        paste0('"', prefixed[li], '"', n_str, p_str)
      })
      line2 <- paste0(cont_prefix, " # ", paste(level_parts, collapse = ", "))
      lines <- c(lines, line1, line2)

    } else if (is_factor && e$role == "factor_binary" && e$n_non_missing > 0) {
      # --- binary : 1 line ---
      # Show first level (order=1) prominently, then second, then var_label
      lv1 <- e$levels_sorted[[1]]
      lv2 <- if (e$n_non_missing >= 2) e$levels_sorted[[2]] else NULL

      lv1_str <- paste0('"', prefixed[1], '"')
      n1_str <- if (!is.null(lv1$n)) paste0(" n=", lv1$n) else ""
      p1_str <- if (!is.null(lv1$pct)) paste0(" ", lv1$pct, "%") else ""

      lv2_part <- ""
      if (!is.null(lv2)) {
        n2_str <- if (!is.null(lv2$n)) paste0(" n=", lv2$n) else ""
        p2_str <- if (!is.null(lv2$pct)) paste0(" ", lv2$pct, "%") else ""
        lv2_part <- paste0(', "', prefixed[2], '"', n2_str, p2_str)
      }

      comment <- paste0(
        lv1_str, n1_str, p1_str, " ",
        '"', e$var_label, '"',
        lv2_part,
        orig_suffix, miss_str
      )
      line1 <- paste0(name_str, " # ", comment)
      lines <- c(lines, line1)

    } else if (e$role %in% c("integer_count", "integer_scale", "double")) {
      # --- numeric : 1 line with optional stats ---
      st <- stats[[e$orig_name]]
      if (!is.null(st)) {
        # Determine rounding: integers get round(1), doubles get round(1)
        stats_str <- paste0(
          round(st$min, 1), "-", round(st$max, 1),
          " mean ", round(st$mean, 1),
          " \u03c3", round(st$sd, 1),
          ", median ", round(st$median, 1)
        )
        comment <- paste0(stats_str, ', "', e$var_label, '"', orig_suffix)
      } else {
        comment <- paste0(e$role, ' "', e$var_label, '"', orig_suffix)
      }
      line1 <- paste0(name_str, " # ", comment)
      lines <- c(lines, line1)

    } else {
      # --- identifier / other / factor with no levels ---
      comment <- paste0(e$role, ' "', e$var_label, '"', orig_suffix)
      line1 <- paste0(name_str, " # ", comment)
      lines <- c(lines, line1)
    }
  }

  # Wrap in var_list <- c(...)
  c("var_list <- c(", lines, ")")
}


#' Generate formatting code blocks (Part 2).
#'
#' Produces a character vector of R code lines for rename + per-variable blocks.
#'
#' For factor roles: fct_recode() with numeric-prefixed labels, fct_relevel(sort),
#'   as.ordered() for ordinal.
#' For integer/double: conversion + NA assignment for missing codes.
#' For identifier/other: comment-only.
#'
#' @param entries   List from .gfs_build_entries().
#' @param df_name   Character: name of the data frame variable in generated script.
#' @param stats     Named list of numeric stats (keyed by orig_name), or NULL.
#' @return Character vector of lines.
.gfs_format_blocks <- function(entries, df_name, stats = NULL) {
  lines <- character(0)

  # --- Step 1: Rename block ---
  renames <- list()
  for (e in entries) {
    if (e$new_name != e$orig_name) {
      renames[[length(renames) + 1]] <- e
    }
  }

  if (length(renames) > 0) {
    lines <- c(lines, "",
      "## Rename variables",
      paste0(df_name, " <- dplyr::rename(", df_name, ","))

    # Padding for rename lines
    w_new <- max(sapply(renames, function(r) nchar(r$new_name)))
    for (ri in seq_along(renames)) {
      r <- renames[[ri]]
      comma <- if (ri < length(renames)) "," else ""
      line <- paste0("  ", .gfs_rpad(r$new_name, w_new), " = ", r$orig_name, comma)
      lines <- c(lines, line)
    }
    lines <- c(lines, ")")
  }

  # --- Step 2: Per-variable formatting blocks ---
  lines <- c(lines, "", "## Format variables")

  for (e in entries) {
    # Orig name suffix for comment header
    orig_suffix <- if (e$orig_name != e$new_name) paste0(" ", e$orig_name) else ""
    # Short role label for comment
    role_short <- sub("^factor_", "", e$role)
    var_expr <- paste0(df_name, "$", e$new_name)

    is_factor <- grepl("^factor_", e$role)

    if (is_factor && e$n_non_missing > 0) {
      # --- Factor formatting block ---
      lines <- c(lines, "",
        paste0("# ", e$new_name, " ", role_short, " \"", e$var_label, "\"", orig_suffix))

      # Build all recode lines (non-missing + missing)
      recode_entries <- list()

      # Non-missing levels (sorted by order)
      for (lv in e$levels_sorted) {
        prefix <- .gfs_numeric_prefix(lv$order, e$max_order)
        new_lbl <- paste0(prefix, lv$display_label)
        recode_entries[[length(recode_entries) + 1]] <- list(
          new_label  = paste0('"', new_lbl, '"'),
          code       = paste0('"', lv$code, '"'),
          pct        = lv$pct,
          n          = lv$n,
          orig_label = lv$orig_label,
          is_missing = FALSE
        )
      }

      # Missing levels (at the end, recoded to NULL)
      for (ml in e$missing_levels) {
        recode_entries[[length(recode_entries) + 1]] <- list(
          new_label  = "NULL",
          code       = paste0('"', ml$code, '"'),
          pct        = NULL,
          n          = NULL,
          orig_label = ml$orig_label,
          is_missing = TRUE
        )
      }

      # Compute padding widths within fct_recode
      w_lbl  <- max(sapply(recode_entries, function(x) nchar(x$new_label)))
      w_code <- max(sapply(recode_entries, function(x) nchar(x$code)))

      # Compute pct+n strings for alignment
      pct_n_strs <- sapply(recode_entries, function(x) {
        if (x$is_missing) return("")
        p_str <- if (!is.null(x$pct)) paste0(x$pct, "%") else ""
        n_str <- if (!is.null(x$n)) paste0(" n=", formatC(x$n, big.mark = "")) else ""
        paste0(p_str, n_str)
      })
      w_pct_n <- max(nchar(pct_n_strs))

      # First line: assignment with fct_recode(factor(as.character(...)),
      # Always use factor(as.character()) for safety against haven_labelled
      lines <- c(lines,
        paste0(var_expr, " <- fct_recode(factor(as.character(", var_expr, ')), # "new" = "old"'))

      # Recode lines
      for (ri in seq_along(recode_entries)) {
        re <- recode_entries[[ri]]
        lbl_pad  <- .gfs_rpad(re$new_label, w_lbl)
        code_pad <- .gfs_rpad(re$code, w_code)
        pct_pad  <- .gfs_rpad(pct_n_strs[ri], w_pct_n)

        # Trailing comma: always (fct_recode tolerates it)
        rline <- paste0("  ", lbl_pad, " = ", code_pad, ",  # ",
                         pct_pad, "    # \"", re$orig_label, '"')
        lines <- c(lines, rline)
      }

      # Closing: ) |> fct_relevel(sort) [|> as.ordered()]
      close <- ") |> fct_relevel(sort)"
      if (e$role == "factor_ordinal") close <- paste0(close, " |> as.ordered()")
      lines <- c(lines, close)

    } else if (e$role %in% c("integer_count", "integer_scale")) {
      # --- Integer formatting block ---
      lines <- c(lines, "",
        paste0("# ", e$new_name, " ", e$role, " \"", e$var_label, "\"", orig_suffix))

      # Stats comment line (if available)
      st <- stats[[e$orig_name]]
      if (!is.null(st)) {
        lines <- c(lines,
          paste0("# range: ", round(st$min, 1), "-", round(st$max, 1),
                 ", mean ", round(st$mean, 1), " \u03c3", round(st$sd, 1),
                 ", Q1=", round(st$q1, 0),
                 " median=", round(st$median, 0),
                 " Q3=", round(st$q3, 0)))
      }

      # Conversion: always as.integer(as.character()) for safety
      lines <- c(lines,
        paste0(var_expr, " <- as.integer(as.character(", var_expr, "))"))

      # Missing codes from JSON levels
      miss_codes <- sapply(e$missing_levels, function(m) m$code)
      if (length(miss_codes) > 0) {
        miss_nums <- suppressWarnings(as.integer(miss_codes))
        miss_nums <- miss_nums[!is.na(miss_nums)]
        if (length(miss_nums) > 0) {
          miss_str <- paste0(miss_nums, "L", collapse = ", ")
          lines <- c(lines,
            paste0(var_expr, "[", var_expr, " %in% c(", miss_str, ")] <- NA_integer_"))
        }
      }

      # Show non-missing value labels as comments (if any exist for an integer var)
      if (e$n_non_missing > 0) {
        val_comments <- sapply(e$levels_sorted, function(lv) {
          paste0('"', lv$code, '"="', lv$orig_label, '"')
        })
        lines <- c(lines,
          paste0("# Values: ", paste(val_comments, collapse = ", ")))
      }

    } else if (e$role == "double") {
      # --- Double formatting block ---
      lines <- c(lines, "",
        paste0("# ", e$new_name, " ", e$role, " \"", e$var_label, "\"", orig_suffix))

      # Stats comment line
      st <- stats[[e$orig_name]]
      if (!is.null(st)) {
        lines <- c(lines,
          paste0("# range: ", round(st$min, 1), "-", round(st$max, 1),
                 ", mean ", round(st$mean, 1), " \u03c3", round(st$sd, 1),
                 ", Q1=", round(st$q1, 1),
                 " median=", round(st$median, 1),
                 " Q3=", round(st$q3, 1)))
      }

      # Conversion: always as.double(as.character()) for safety
      lines <- c(lines,
        paste0(var_expr, " <- as.double(as.character(", var_expr, "))"))

      # Missing codes
      miss_codes <- sapply(e$missing_levels, function(m) m$code)
      if (length(miss_codes) > 0) {
        miss_nums <- suppressWarnings(as.numeric(miss_codes))
        miss_nums <- miss_nums[!is.na(miss_nums)]
        if (length(miss_nums) > 0) {
          miss_str <- paste(miss_nums, collapse = ", ")
          lines <- c(lines,
            paste0(var_expr, "[", var_expr, " %in% c(", miss_str, ")] <- NA_real_"))
        }
      }

    } else {
      # --- identifier / other / factor with 0 levels ---
      lines <- c(lines, "",
        paste0("# ", e$new_name, " ", e$role, " \"", e$var_label, "\"", orig_suffix))
    }
  }

  lines
}


#' Generate a standalone, human-readable R formatting script.
#'
#' Reads the unified .survey_meta.json and the metadata tibble to produce
#' a self-contained R script that formats a raw dataset. The generated script
#' depends only on haven (for import), dplyr, and forcats — no dependency on
#' data_formatting_pipeline.R.
#'
#' @param metadata     Metadata tibble from extract_survey_metadata().
#' @param meta_json    Path to the unified .survey_meta.json file.
#' @param df           Optional: the raw data frame (for computing numeric
#'                     summary statistics in codebook and comments).
#' @param df_name      Character: name of the data frame variable in the
#'                     generated script (default: "data"). Used in all
#'                     fct_recode(), rename(), and assignment calls.
#' @param output_path  Path for the output .R file. Default: derived from
#'                     meta_json as {stem}_format.R in the same directory.
#'
#' @return The output_path, invisibly.
#'
#' @examples
#' \dontrun{
#'   meta <- extract_survey_metadata(df, meta_json = "virage.survey_meta.json")
#'   generate_format_script(meta, "virage.survey_meta.json", df = df)
#' }
generate_format_script <- function(metadata,
                                   meta_json,
                                   df          = NULL,
                                   df_name     = "data",
                                   output_path = NULL) {
  # --- Input validation ---
  stopifnot(is.data.frame(metadata))
  stopifnot("var_name" %in% names(metadata))
  stopifnot("r_class" %in% names(metadata))
  stopifnot(file.exists(meta_json))
  stopifnot(is.character(df_name), nchar(df_name) > 0)

  # --- Read JSON ---
  json_data <- .read_meta_json(meta_json)
  json_vars <- json_data$variables
  config    <- json_data$config

  # --- Derive output path ---
  if (is.null(output_path)) {
    output_path <- sub("\\.survey_meta\\.json$", "_format.R", meta_json)
    if (output_path == meta_json) {
      output_path <- paste0(tools::file_path_sans_ext(meta_json), "_format.R")
    }
  }

  # --- Build normalized entries ---
  entries <- .gfs_build_entries(json_vars, metadata)

  # --- Compute numeric stats (if df provided) ---
  num_stats <- list()
  if (!is.null(df)) {
    for (e in entries) {
      if (e$role %in% c("integer_count", "integer_scale", "double")) {
        col <- df[[e$orig_name]]
        if (!is.null(col)) {
          miss_codes <- sapply(e$missing_levels, function(m) m$code)
          num_stats[[e$orig_name]] <- .gfs_compute_numeric_stats(col, miss_codes)
        }
      }
    }
  }

  # --- Build script sections ---

  # Header
  dataset_name <- config$dataset %||% basename(meta_json)
  header <- c(
    "# ============================================================",
    paste0("# Formatting script: ", dataset_name),
    paste0("# Generated: ", Sys.Date(), " from ", basename(meta_json)),
    "# Dependencies: haven, dplyr, forcats",
    "# ============================================================",
    "#",
    '# Usage:',
    '#   source("this_script.R")',
    "#",
    "# The variable list below can be used to select variables:",
    paste0("#   ", df_name, " <- dplyr::select(", df_name, ", dplyr::all_of(var_list))"),
    "",
    "library(haven)",
    "library(dplyr)",
    "library(forcats)",
    "" #,
    # "## Import data",
    # paste0(df_name, ' <- haven::read_dta("', dataset_name, '")'),
    # ""
  )

  # Part 1: Codebook
  codebook_header <- c("## Variable list (codebook)")
  codebook <- .gfs_codebook_lines(entries, stats = num_stats)

  # Part 2: Formatting
  formatting <- .gfs_format_blocks(entries, df_name, stats = num_stats)

  # Part 3: Variable labels (reapply after transformations)
  label_lines <- c("", "", "## Variable labels",
    "# Reapply variable labels (lost during class conversions / fct_recode)")
  for (e in entries) {
    if (nchar(e$var_label) > 0) {
      # Escape any quotes in the label
      escaped <- gsub('"', '\\\\"', e$var_label)
      label_lines <- c(label_lines,
        paste0('attr(', df_name, '$', e$new_name, ', "label") <- "', escaped, '"'))
    }
  }

  # Footer
  footer <- c("",
    "# Select and reorder variables",
    paste0("# ", df_name, " <- dplyr::select(", df_name, ", dplyr::all_of(var_list))"),
    ""
  )

  # --- Assemble and write ---
  all_lines <- c(header, codebook_header, codebook, formatting, label_lines, footer)

  writeLines(all_lines, output_path, useBytes = TRUE)
  n_vars <- length(entries)
  message(sprintf("Format script written to %s (%d variables)", output_path, n_vars))
  invisible(output_path)
}


# ============================================================
# 10. make_dummy_tibble() — create minimal test dataframe
# ============================================================

#' Create a minimal dummy tibble from a survey dataframe
#'
#' Extracts unique values from each column to build a small representative
#' tibble suitable for unit tests. Preserves all column attributes (haven
#' labels, factor levels, Date class, etc.). Prints \code{dput()} output
#' that can be pasted directly into test files.
#'
#' @param df         A data frame or tibble (typically imported via haven).
#' @param cols       Character vector of column names to include. NULL = all.
#' @param max_unique Maximum number of unique non-NA values per column (default 30).
#'                   Columns exceeding this are randomly sampled.
#' @param na_ratio   Proportion of padding positions that remain NA (default 1/3).
#' @param seed       Optional integer seed for reproducibility.
#' @param clipboard  If TRUE, copy the dput output to the clipboard (Windows).
#'
#' @return The dummy tibble (invisibly). The \code{dput()} representation is
#'   printed to the console via \code{cat()}.
make_dummy_tibble <- function(df,
                              cols       = NULL,
                              max_unique = 30L,
                              na_ratio   = 1/3,
                              seed       = NULL,
                              clipboard  = FALSE) {

  stopifnot(is.data.frame(df))
  max_unique <- as.integer(max_unique)

  if (!is.null(seed)) set.seed(seed)

  # Column selection
  if (is.null(cols)) {
    cols <- names(df)
  } else {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      warning("Columns not found in df: ", paste(missing_cols, collapse = ", "))
    }
    cols <- intersect(cols, names(df))
    if (length(cols) == 0) stop("No valid columns selected.")
  }

  df <- df[, cols, drop = FALSE]

  # Edge case: 0-row input
  if (nrow(df) == 0) {
    dput_str <- paste(utils::capture.output(dput(df)), collapse = "\n")
    cat(dput_str, "\n")
    return(invisible(df))
  }

  # Per-column: extract unique non-NA values and sample if needed
  col_info <- lapply(cols, function(nm) {
    col <- df[[nm]]
    saved_attrs <- attributes(col)
    unique_vals <- unique(col[!is.na(col)])
    # Convert factors to character now so c() doesn't coerce to integer later
    if (is.factor(unique_vals)) unique_vals <- as.character(unique_vals)
    k <- length(unique_vals)

    if (k > max_unique) {
      idx <- sample.int(k, max_unique)
      unique_vals <- unique_vals[idx]
      k <- max_unique
    }

    list(
      unique_vals = unique_vals,
      k           = k,
      saved_attrs = saved_attrs,
      original    = col
    )
  })
  names(col_info) <- cols

  # Row count = max unique count across columns (already capped by max_unique)
  n_rows <- max(vapply(col_info, function(x) x$k, integer(1)), 1L)

  # Per-column: pad, fill, shuffle, restore attributes
  rebuilt <- lapply(col_info, function(info) {
    .rebuild_dummy_col(info$original, info$unique_vals, info$k,
                       n_rows, na_ratio, info$saved_attrs)
  })

  result <- tibble::new_tibble(setNames(rebuilt, cols), nrow = n_rows)

  # Output dput
  dput_str <- paste(utils::capture.output(dput(result)), collapse = "\n")
  cat(dput_str, "\n")

  if (isTRUE(clipboard)) {
    tryCatch({
      utils::writeClipboard(dput_str)
      message("dput output copied to clipboard.")
    }, error = function(e) {
      message("Could not copy to clipboard: ", e$message)
    })
  }

  invisible(result)
}

# Helper: rebuild a single column with padding, fill, shuffle, and attributes
.rebuild_dummy_col <- function(original, unique_vals, k, n_rows,
                               na_ratio, saved_attrs) {
  n_pad <- n_rows - k

  if (n_pad > 0 && k > 0) {
    n_keep_na <- max(1L, floor(n_pad * na_ratio))
    n_fill    <- n_pad - n_keep_na
    fill_vals <- if (n_fill > 0) {
      unique_vals[sample.int(k, n_fill, replace = TRUE)]
    } else {
      unique_vals[integer(0)]
    }
    col_vec <- c(unique_vals, fill_vals, rep(NA, n_keep_na))
  } else if (n_pad > 0 && k == 0) {
    # All-NA column
    col_vec <- rep(NA, n_rows)
  } else {
    col_vec <- unique_vals
  }

  # Shuffle row order
  col_vec <- col_vec[sample.int(length(col_vec))]

  # Restore attributes by column type
  if (is.factor(original)) {
    col_vec <- factor(as.character(col_vec), levels = levels(original),
                      ordered = is.ordered(original))
    lbl <- saved_attrs[["label"]]
    if (!is.null(lbl)) attr(col_vec, "label") <- lbl

  } else if ("haven_labelled" %in% (saved_attrs[["class"]] %||% character(0))) {
    # Determine base type from original class vector
    base_classes <- setdiff(saved_attrs[["class"]],
                            c("haven_labelled", "vctrs_vctr", "haven_labelled_spss"))
    base_type <- base_classes[1] %||% "double"

    # unclass() first: c() on haven_labelled keeps vctrs class, blocking as.double()
    col_vec <- unclass(col_vec)
    col_vec <- switch(base_type,
      "double"    = as.double(col_vec),
      "integer"   = as.integer(col_vec),
      "character" = as.character(col_vec),
      as.double(col_vec)
    )
    attrs_to_set <- saved_attrs
    attrs_to_set$names <- NULL
    attributes(col_vec) <- attrs_to_set

  } else if (inherits(original, "POSIXct")) {
    col_vec <- as.double(col_vec)
    attrs_to_set <- saved_attrs
    attrs_to_set$names <- NULL
    attributes(col_vec) <- attrs_to_set

  } else if (inherits(original, "Date")) {
    col_vec <- as.double(col_vec)
    attrs_to_set <- saved_attrs
    attrs_to_set$names <- NULL
    attributes(col_vec) <- attrs_to_set

  } else {
    # Plain numeric/character/logical — just restore label if present
    lbl <- saved_attrs[["label"]]
    if (!is.null(lbl)) attr(col_vec, "label") <- lbl
  }

  col_vec
}

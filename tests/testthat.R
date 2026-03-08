library(testthat)

# Detect current project root from the location of this testthat.R file
.proj_root <- normalizePath(file.path(dirname(testthat::test_path()), ".."), winslash = "/")
#.proj_root <- "~/github/socio_public_services"

source(file.path(.proj_root, "R", "data_formatting_pipeline.R"), encoding = "UTF-8")


# ---------------------------------------------------------------------------
# Shared helpers (available to all test files)
# ---------------------------------------------------------------------------

# Project root for tests that need instructions/*.md files
.test_proj_root <- .proj_root

tmp_json <- function() tempfile(fileext = ".json")

# Minimal meta_list suitable for .write_meta_json() / .read_meta_json()
make_meta_list <- function(vars) {
  list(
    config = list(
      dataset     = "test.dta",
      missing_num = list(-1L, 99L),
      missing_chr = list("NSP"),
      yes_labels  = list("Oui"),
      no_labels   = list("Non")
    ),
    variables = vars
  )
}

# Minimal metadata tibble row for ai_classify_roles().
# detected_role = "factor_nominal" so the variable enters the target filter.
make_classify_meta <- function(var_name, var_label,
                               labels_vec, values_vec,
                               missing_vals    = integer(0),
                               detected_role   = "factor_nominal",
                               n_distinct_data = as.integer(length(labels_vec))) {
  tibble::tibble(
    var_name        = var_name,
    var_label       = var_label,
    r_class         = "integer",
    n_distinct      = as.integer(length(labels_vec) - length(missing_vals)),
    n_distinct_data = n_distinct_data,
    labels          = list(labels_vec),
    values          = list(values_vec),
    missing_vals    = list(missing_vals),
    detected_role   = detected_role
  )
}

# Wrap a fake Haiku reply text into the list structure ai_call_claude() returns.
mock_ai <- function(text) {
  function(...) list(content = list(list(text = text)))
}




# ---------------------------------------------------------------------------
# Run tests scripts
# ---------------------------------------------------------------------------
test_dir(file.path(.proj_root, "tests", "testthat"))
# test_check("package_name")



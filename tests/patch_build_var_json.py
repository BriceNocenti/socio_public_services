"""Patch .build_var_json in ai_suggest_labels and .build_prompt to use new $levels structure."""
import re

path = 'R/data_formatting_pipeline.R'
with open(path, encoding='utf-8') as f:
    content = f.read()

# ---------- 1. Replace .build_var_json function body ----------
old_build_var = r"""  \.build_var_json <- function\(var_name, var_label, detected_role,
                               values, labels, new_labels, send_order,
                               merge_groups, level_counts, level_freqs\) \{.*?paste0\(obj, "\}"\)
  \}"""

new_build_var = '''  # Works directly from the $levels list (named list keyed by value code).
  .build_var_json <- function(var_name, var_label, detected_role,
                               levels, send_order, merge_groups) {
    lvl_names  <- names(levels)
    names_ord  <- lvl_names[send_order]
    lvls_ord   <- levels[send_order]
    groups_ord <- merge_groups[send_order]
    keep       <- !purrr::map_lgl(lvls_ord, ~ isTRUE(.x$missing))

    names_keep  <- names_ord[keep]
    lvls_keep   <- lvls_ord[keep]
    groups_keep <- groups_ord[keep]

    if (length(lvls_keep) == 0) return(NULL)

    if (length(names_keep) == 0 || all(nchar(names_keep) == 0L))
      names_keep <- as.character(seq_along(lvls_keep))

    type_str <- switch(detected_role,
      factor_binary  = "binary",
      factor_ordinal = "ordinal",
      factor_nominal = "nominal",
      "nominal"
    )

    esc <- function(x) gsub(\'"\', \'\\\\"\', x, fixed = TRUE)
    var_label_clean <- .clean_var_label_for_api(var_label, var_name = var_name)

    # ---- For ordinal/binary: collapse groups before sending to AI -----------
    if (detected_role %in% c("factor_ordinal", "factor_binary") &&
        length(unique(groups_keep)) < length(groups_keep)) {
      gids     <- unique(groups_keep)
      g_keys   <- character(length(gids))
      g_labels <- character(length(gids))
      for (gi in seq_along(gids)) {
        idx <- which(groups_keep == gids[gi])
        orig_labels <- purrr::map_chr(lvls_keep[idx], ~ as.character(.x$label %||% ""))
        g_labels[gi] <- paste(unique(orig_labels), collapse = " / ")
        g_keys[gi]   <- names_keep[idx[1]]
      }
      kv_pairs    <- paste0(\'"\', esc(g_keys), \'":"\', esc(g_labels), \'"\')
      levels_json <- paste0("{", paste(kv_pairs, collapse = ", "), "}")
      return(paste0(\'{"var":"\', esc(var_name), \'","type":"\', type_str,
                    \'","desc":"\', esc(var_label_clean), \'","levels":\', levels_json, "}"))
    }

    # ---- Non-ordinal or ordinal with no merging: send raw levels ------------
    labels_keep <- purrr::map_chr(lvls_keep, ~ as.character(.x$label %||% ""))
    kv_pairs    <- paste0(\'"\', esc(names_keep), \'":"\', esc(labels_keep), \'"\')
    levels_json <- paste0("{", paste(kv_pairs, collapse = ", "), "}")
    paste0(\'{"var":"\', esc(var_name), \'","type":"\', type_str,
           \'","desc":"\', esc(var_label_clean), \'","levels":\', levels_json, "}")
  }'''

m = re.search(old_build_var, content, re.DOTALL)
if m:
    content = content[:m.start()] + new_build_var + content[m.end():]
    print("Replaced .build_var_json")
else:
    print("ERROR: .build_var_json pattern not found")

# ---------- 2. Replace build_prompt to use $levels ----------
old_build_prompt = r"""  build_prompt <- function\(chunk_df\) \{
    json_objects <- purrr::pmap\(
      dplyr::select\(chunk_df, var_name, var_label, detected_role,
                    values, labels, new_labels, \.send_order, \.merge_groups,
                    dplyr::any_of\(c\("level_counts", "level_freqs"\)\)\),
      function\(var_name, var_label, detected_role,
               values, labels, new_labels, \.send_order, \.merge_groups,
               level_counts = integer\(0\), level_freqs = numeric\(0\)\) \{
        \.build_var_json\(var_name, var_label, detected_role,
                        values, labels, new_labels, \.send_order, \.merge_groups,
                        level_counts, level_freqs\)
      \}
    \) \|> purrr::compact\(\)

    if \(length\(json_objects\) == 0\) return\(NULL\)

    paste0\("\[\n", paste\(json_objects, collapse = ",\n"\), "\n\]"\)
  \}"""

new_build_prompt = '''  build_prompt <- function(chunk_df) {
    json_objects <- purrr::pmap(
      list(var_name = chunk_df$var_name, var_label = chunk_df$var_label,
           detected_role = chunk_df$detected_role, levels = chunk_df$levels,
           send_order = chunk_df$.send_order, merge_groups = chunk_df$.merge_groups),
      function(var_name, var_label, detected_role, levels, send_order, merge_groups) {
        .build_var_json(var_name, var_label, detected_role, levels, send_order, merge_groups)
      }
    ) |> purrr::compact()

    if (length(json_objects) == 0) return(NULL)

    paste0("[\\n", paste(json_objects, collapse = ",\\n"), "\\n]")
  }'''

m2 = re.search(old_build_prompt, content, re.DOTALL)
if m2:
    content = content[:m2.start()] + new_build_prompt + content[m2.end():]
    print("Replaced build_prompt")
else:
    print("ERROR: build_prompt pattern not found")

# ---------- 3. Replace .build_stats_only_map / .enrich_labels_map_with_stats ----------
# These use target's old list-columns to build a map; now $levels already has all data
# We replace the zero_level_vars warning that still checks old column
old_zero = r'zero_level_vars <- target\[target\$\.n_levels == 0L, \]\n  if \(nrow\(zero_level_vars\) > 0L\) \{\n    sample_vars <- head\(zero_level_vars\$var_name, 5L\)\n    message\("ai_suggest_labels: ", nrow\(zero_level_vars\),.*?\}\n  \}'
m3 = re.search(old_zero, content, re.DOTALL)
if m3:
    print("Found zero_level_vars block")
else:
    print("zero_level_vars block not found (may be ok)")

with open(path, 'w', encoding='utf-8') as f:
    f.write(content)
print("Done writing file.")

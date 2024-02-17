# # Gestion des packages
# 
# # Renv : gestion de packages au sein d'un Projet R
# #   Première installation de renv
# utils::install.packages("renv", dependencies = TRUE) #
# # renv::init()
# # renv::equip() # install external dependencies for renv on Windows ; turn off Antivirus if needed
# 
# 
# #   Installation des packages
# #    in R Project with renv:: : possibly need to turn off firewall
# install.packages(c(
#   "tidyverse", # "dplyr", "stringr", "forcats", "ggplot2", "tibble", "tidyr", "rlang", "purrr", "vctrs",
#   "knitr", "rmarkdown", "kableExtra", "devtools", "ragg",
#   "tabxplor", "FactoMineR", "ggfacto", "openxlsx",
#   "tidymodels", "nnet", "mice", #, "srvyr",
#   "TraMineR", "TraMineRextras", "WeightedCluster", "seqhandbook",
#   "ggiraph", "ggpattern", "ggnewscale", "widgetframe", # "oce",
#   "DescTools",  "dineq", "gtsummary", # "finalfit",
#   "fastcluster", "Rfast", "arrow", "duckdb", "tictoc"
# ))
# # # # To try in case it's impossible to install any package (first turn off firewall)
# # # getOption("repos") ; getOption("download.file.method") ; renv:::renv_download_method()
# # # Sys.setenv(RENV_DOWNLOAD_METHOD = "libcurl")
# # # options(RENV_DOWNLOAD_OVERRIDE = utils::download.file) # options(renv.download.override = utils::download.file)
# # # Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT=TRUE)
# # # # options(RENV_CURL_EXECUTABLE = "C:\\Windows\\SYSTEM32\\curl.exe")

# renv::intall("BriceNocenti/pcspp") # github packages

# # Installer de nouveau packages, avec utils::install.packages si erreur
# renv::install("openxlsx")
# install.packages("openxlsx")
# utils::install.packages("openxlsx")
#
# #   Mise à jour des packages via renv
# renv::update()
# 
# #   Sauvegarder packages actuels dans la configuration renv::
# renv::snapshot()
# 
## #   Installer les packages de la dernière sauvegarde renv::
## renv::restore()





# To start by default in the right directory on Windows 10
#   System Properties > Advanced, add a SystemEnvironment Variable : 
#              name: R_USER     ; path: D:\Statistiques      # then test Sys.getenv("HOME")
#   (In \My Documents, move R and .R directories to D:\Statistiques)






# Start--------------------------------------------------------------------------------------


suppressMessages({
  #library(lme4)
  #library(magrittr) 
  library(rlang)
  library(tictoc)
  #library(pillar)
  #library(vctrs)
  #library(crayon)
  #library(janitor)
  #library(labelled)
  #library(devtools)
  #library(furrr)
  
  library(haven)
  library(readr)
  library(readxl)
  #library(openxlsx) 
  library(knitr)
  #library(svglite)
  #library(dineq)  #: use                 dineq::ntiles.wtd()
  library(FactoMineR)
  #library(Factoshiny)
  #library(factoextra)
  #library(GDAtools) #Pour faire une ACM spécifique / spécifique de classe 
  #library(finalfit) #Regressions logistiques
  #library(explor)
  #library(plotly, exclude = c("mutate", "filter"))
  #library(ggiraph)
  #library(ggsci)
  #library(htmlwidgets)
  #library(DescTools)
  
  #library(pollster)
  # library(tictoc)
  #library(widgetframe)
  
  #library(gtsummary)
  library(tidymodels)
  library(tidyverse)
  library(ggpattern)
   #devtools::install_github("hadley/lineprof")
  #library(lineprof)
  
  #library(learnr)
  #library(shinyShortcut) #To run a shiny app as application. Try with learnr !
  #library(rsconnect)
  
  
  #library(kableExtra)
  #library(ggThemeAssist)
  #library(formattable)
  #library(questionr)
  #library(viridis)
})



# devtools::install_github("BriceNocenti/tabxplor")

if("tabxplor" %in% (.packages())) detach("package:tabxplor", unload = TRUE) 
library(tabxplor)

if("ggfacto" %in% (.packages())) detach("package:ggfacto", unload = TRUE)
library(ggfacto)

options(scipen = 999) #Enlever la notation scientifique
# options(max.print = "500")
ggplot2::theme_set(ggplot2::theme_minimal())
#options(tabxplor.print = "kable")

#  couleur des tableaux tabxplor::, de base
c(
  "#1AE6D6", "#00bcd4", "#1e88e5", "#0019ff",
  "#ffb300", "#FF8138", "#ff3d00", "#cb0000" 
)



## Path and imports ----

# Nomenclatures des corps et des grades
# nomenc_CORPS_3FP_2010_2019_V1         <- readRDS("~\\Data\\CASD SIASP FGE/nomenc_CORPS_3FP_2010_2019_V1.rds")
nomenc_CORPS_GRADE_EF_final           <- readRDS("~\\Data\\CASD SIASP FGE/nomenc_CORPS_GRADE_EF_final.rds")
nomenc_final_with_CONT                <- readRDS("~\\Data\\CASD SIASP FGE/nomenc_final_with_CONT.rds")

# nomenc_CORPS_GRADE_EF_final <- openxlsx::read.xlsx("~\\Data\\CASD SIASP FGE\\Nomenclature des corps et grades\\nomenc_CORPS_GRADE_EF_final.xlsx") |> 
#   as_tibble() |> 
#   #select(-any_of(c("CSK", "EMP_MIN"))) |>
#   mutate(across(where(is.character), as.factor))
# ID_DUPS <- as.character(nomenc_CORPS_GRADE_EF_final$GRADE[!is.na(nomenc_CORPS_GRADE_EF_final$GRADE) & duplicated(nomenc_CORPS_GRADE_EF_final$GRADE)]) ; ID_DUPS
# if (length(ID_DUPS) > 0) stop("duplicated nomenc_CORPS")



## Fonts management ----

#windowsFonts(sans = windowsFont("DejaVu Sans Condensed")) #Replace "TT Arial"
windowsFonts(sans = windowsFont("DejaVu Sans"))
windowsFonts(mono = windowsFont("DejaVu Sans Mono"))
#windowsFonts(arial = windowsFont("TT Arial"))

euros <- stringi::stri_unescape_unicode("\\u20ac")
up    <- stringi::stri_unescape_unicode("\\u2191")
dw    <- stringi::stri_unescape_unicode("\\u2193")


# #Avec le package extrafont
# #install.packages('extrafont')
# extrafont::font_import(pattern = "DejaVu")
# extrafont::loadfonts()
# #extrafont::fonts()
# extrafont::fonttable()
# #"DejaVu Sans Condensed"




#Code de David Gohel :
# library(systemfonts)
# condensed <- 
#   system_fonts() %>% 
#   filter(
#     family %in% "DejaVu Sans", 
#     grepl("condensed", style, ignore.case = TRUE))
# 
# register_font(
#   "DejaVu Sans Condensed", 
#   plain = list(filter(condensed, !weight %in% "bold", italic) %>% pull(path), 0), 
#   bold = list(filter(condensed, weight %in% "bold", !italic) %>% pull(path), 0), 
#   italic = list(filter(condensed, !weight %in% "bold", !italic) %>% pull(path), 0),
#   bolditalic = list(filter(condensed, weight %in% "bold", italic) %>% pull(path), 0)
# )
# dat <- mtcars
# dat$carname <- row.names(dat)
# gg <- ggplot(dat, aes(drat, carname)) + 
#   geom_point() + 
#   geom_text(data = sample_n(dat, 3), aes(label = "coco"), family = "Arial")+
#   theme_minimal(base_family = "DejaVu Sans Condensed") + 
#   theme(axis.title = element_text(face = "bold"),
#         axis.text.x = element_text(face = "italic"),
#         axis.text.y = element_text(face = "bold")
#   ) 
# ggiraph(ggobj = gg) %>% print()
# 
# # However, you should also declare/create new font faces to embed the font 
# # "DejaVu Sans Condensed" in the web page. 
# # For example, the font is not found on Rstudio Viewer








# #Garbage collector with summary printing of memory usage
# gc2 <- function(msg = TRUE) {
#   g <- base::gc()
#   if (msg) message(str_c("Total memory used ", round(g[2, 4] + g[1, 4]), " Mb"))
# } 
# 
# multi_gc <- function() {
#   gc()
#   gc()
#   gc()
#   gc()
#   gc()
#   gc()
#   gc()
#   gc()
#   gc()
#   gc()
#   gc2()
# }






# New tab functions --------------------------------------------------
tab_transpose <- function(tabs, name = "variables") {
  row_var <- tab_get_vars(tabs, "row_var")$row_var
  totrow_names <- filter(tabs, is_totrow(tabs)) |> pull(1) |> as.character()
  if (length(totrow_names) >= 2) stop("not working for now with many total rows")
  totcol_name <- is_totcol(tabs) ; totcol_name <- names(totcol_name[totcol_name])
  if (length(totcol_name) >= 2) stop("not working for now with many total columns")
  
  tabs |>
    pivot_longer(cols = -1, names_to = name, values_to = "value") |> 
    pivot_wider(names_from = all_of(row_var), values_from = value, names_sort = TRUE) |> 
    mutate(across(where(is.character), as_factor)) |>
    mutate(across(where(is_fmt), ~ set_type(., "col"))) |> 
    mutate(across(where(is_fmt), ~ as_totcol(., FALSE))) |> 
    mutate(across(any_of(totrow_names), ~ as_totrow(as_totcol(.), FALSE))) |>
    mutate(across(where(is_fmt), ~ if_else(!!sym(name) == totcol_name, 
                                           as_totrow(.), 
                                           as_totrow(., FALSE)))) |> 
    new_tab()
}




summarise_factors <- function(data, group_var, name, vars_levels_list) {
  
  data <- data |> select(all_of(c(group_var, names(vars_levels_list))))
  
  unfound_lvs <- vars_levels_list |> imap(~ .x[!.x %in% levels(data[[.y]])] )
  unfound_lvs <- unfound_lvs[map_lgl(unfound_lvs, ~ length(.) != 0)]
  if (length(unfound_lvs) != 0) {
    unfound_text <- unfound_lvs |>
      imap(~ paste0("; ", .y, ": ",
                    paste0("'", .x, "'", collapse = ", ")) ) |> 
      flatten_chr() |> 
      paste0(collapse = "")
    
    warning(paste0("levels not found in data:", unfound_text))
    
    vars_levels_list <- vars_levels_list |> 
      imap(~ levels(data[[.y]])[levels(data[[.y]]) %in% .x] )
  }
  
  # #On passe en NA les modalités non selectionnées
  # rmlvs <- vars_levels_list |> 
  #   imap(~ levels(data[[.y]])[!levels(data[[.y]]) %in% .x] ) |>
  #   map(~ set_names(., rep("NULL", length(.))))
  # 
  # data <- data |> mutate(across(
  #   all_of(names(vars_levels_list)), 
  #   ~ fct_recode(., splice(rmlvs[[cur_column()]]))  
  # ))
  
  
  #On transforme chaque factor en autant de variables logiques qu'il a de levels
  #  sum() sur un vecteur logique compte les TRUE
  rename_variable_with_level <- function(group, varname, level) {
    str_c(group, "_", varname, "_",
          str_remove(level, tabxplor:::cleannames_condition()) |>
            str_replace_all(" +", "_")
    )
  }
  
  # data <- data |> 
  #   mutate(across(
  #     all_of(names(vars_levels_list)), 
  #     function(var) as_tibble(set_names(
  #       map(levels(var), function(.lv) var == .lv ), 
  #       rename_variable_with_level(name, cur_column(), levels(var))
  #     ))
  #   )) 
  # 
  # gc()
  # data <- data |> unnest(all_of(names(vars_levels_list)))
  
  grouping_var <- pull(data, !!sym(group_var))
  
  data <- 
    imap_dfc(vars_levels_list, 
             function(.lvs, .var_names) as_tibble(set_names(
               map(.lvs, function(.lv) data[[.var_names]] == .lv ), 
               rename_variable_with_level(name, .var_names, .lvs)
             ))
             
    )
  
  data <- data |> add_column(!!sym(group_var) := grouping_var, .before = 1)
  
  data |> 
    group_by(!!sym(group_var)) |>
    summarise(across(everything(), 
                     ~ sum(., na.rm = TRUE) / n() ), 
              .groups = "drop") 
}
# test <- 
# tibble(a = rep(c("h", "b"), 10000000), 
#        b = tibble(b1 = rep(c("k", "k"), 10000000),
#                   b2 = rep(c("c", "b"), 10000000) )
#        )
# 
# system.time(bind_cols(select(test, -b), test$b))
# system.time(test |> mutate(b1 = b$b1, b2 = b$b2) |> select(-b))
# system.time(unnest(test, b))




summarise_factor_nest <- function(data, group, var) {
  group <- rlang::enquo(group)
  var   <- rlang::ensym(var)
  
  group <- tidyselect::eval_select(group, data)
  var   <- tidyselect::eval_select(var  , data)
  
  data <- data[c(group, var)]
  
  group <- names(group)
  var   <- names(var)
  
  data <- data |> rename(variable = !!rlang::sym(var))
  
  data <- data |> dplyr::group_by(!!!rlang::syms(group))
  
  data <- data |>
    arrange(.by_group = TRUE) |>
    mutate(group_ID = cur_group_id()) |>
    ungroup() |>
    nest(variable = c(group_ID, variable) ) 
  
  # 1) bind, 2) calculs au niveau global, 3() re split 
  data$variable <- 
    bind_rows(data$variable) |> 
    group_by(group_ID, variable) |> 
    mutate(!!rlang::sym(paste0("n_", var)) := n()) |>
    distinct() |>
    group_by(group_ID) |> 
    arrange(-!!rlang::sym(paste0("n_", var)), .by_group = TRUE) |>
    mutate(!!rlang::sym(paste0("freq_", var)) := 
             !!rlang::sym(paste0("n_", var)) / sum(!!rlang::sym(paste0("n_", var))), 
           #first_CORPS = row_number() == 1
    ) |> 
    rename(!!rlang::sym(var) := variable) |>
    group_split(.keep = FALSE)
  
  data <- data |> rename(!!rlang::sym(var) := variable)
  
  data
}



# tabs <- gradesHC_ppp1
# rename_field <- 14
# str_pad <- TRUE
# comment_var = "comment"
tbl_to_tribble_code <- function(tabs, rename_field = 0, digits = 0, 
                                show = rlang::is_interactive(), 
                                comment_var = character()
) {
  
  tabs <- tabs |> 
    mutate(across(where(~ is.character(.) | is.factor(.)), 
                  ~ format(paste0("\"", ., "\"")) 
    )) |>
    mutate(across(where(is.numeric), ~ format(round(., digits)))) |>
    mutate(across(where(is_fmt), 
                  ~ format(round(get_num(.),  #* if_else(tabxplor:::get_display(.) == "pct", 100, 1)
                                 get_digits(.) + if_else(tabxplor:::get_display(.) == "pct", 2, 0)))
    ))
  
  if (rename_field > 0) {
    tabs <- tabs |> 
      mutate(new_names = str_pad("\"\"", rename_field, "right")) |>
      select(new_names, everything())
  }
  
  if (length(comment_var) != 0) {
    tabs <- tabs |> 
      mutate(across(1, ~ if_else(!!sym(comment_var), paste0("#", .), .))) |>
      select(-all_of(comment_var))
  }
  
  headers <- set_names(paste0("~", names(tabs)), names(tabs))
  
  tabs <- tabs |> 
    add_row(!!!headers, .before = 1) |> 
    mutate(across(everything(), ~ str_pad(., max(str_length(.)), "right") )) 
  
  tabs <- unite(tabs, "text", -any_of(comment_var), sep = ", ") |> pull(text)
  
  out <- paste0("tibble::tribble(\n", paste0(tabs, collapse = ",\n"), "\n)")
  
  path <- tempfile("cat", fileext = ".R")
  writeLines(out, path, useBytes = TRUE)
  if (show) file.show(path)
}





# data <- siasp16_cumul_final[500000:500499,] |>
#   select(IND_CORPS) |> #CORPS, 
#   filter(!is.na(IND_CORPS))
# var <- expr(IND_CORPS)

nested_char_split <- function(data, var, nb = 5, lvs) {
  var = ensym(var)
  new_vars <- paste0(rlang::as_name(var), 1:nb)
  
  data <- data |>
    separate(!!var, sep = "\\|", into = new_vars, 
             extra = "drop", fill = "right") 
  
  data <- 
    reduce(new_vars, .init = data, 
           ~ separate(.x, col = !!sym(.y), sep = "§", 
                      into = c(.y, paste0(.y, "_H")), convert = TRUE))
  
  if (!missing(lvs)) {
    data |> mutate(across(all_of(new_vars), ~ fct_relevel(., lvs) ))
    
  } else {
    data |> mutate(across(all_of(new_vars), as.factor))
  }
}



# digits <- c(0, 0, 0, 0, 2)
# ntiles = 5
# type <- "range"

tab_ntiles <- function(data, num_vars, ntiles = 5, digits = 2, 
                       type = c("range", "min")) {
  num_vars <- names(tidyselect::eval_select(rlang::enquo(num_vars), data))
  digits <- vctrs::vec_recycle(digits, length(num_vars))
  ntiles <- purrr::set_names(vctrs::vec_recycle(ntiles, length(num_vars)),
                             num_vars)
  
  type_func <- 
    switch(
      type[1],
      "range" = function(tile, num, digits) as.factor(str_c(
        tile, "-", 
        round(min(num, na.rm = TRUE), digits),
        "–", 
        round(max(num, na.rm = TRUE), digits)
      )), 
      
      "min"   = function(tile, num, digits) as.factor(str_c(
        tile, "-", 
        round(min(num, na.rm = TRUE), digits),
        " +"))
    )
  
  
  data <- data |> dplyr::mutate(across(
    all_of(num_vars), 
    ~ as.factor(ntile(., ntiles[[cur_column()]])), 
    .names = "{.col}_tiles"
  ))
  
  suppressWarnings(
    purrr::reduce2(num_vars, digits, .init = data, 
                   ~ dplyr::group_by(..1, rlang::eval_tidy(sym(paste0(..2, "_tiles")))) |> 
                     dplyr::mutate(across(
                       all_of(paste0(..2, "_tiles")),
                       ~  type_func(., rlang::eval_tidy(sym(str_remove(cur_column(), "_tiles$"))), ..3)
                       
                     )) |>
                     dplyr::ungroup()
    ) |> 
      dplyr::select(-all_of(num_vars)) |> 
      dplyr::rename(all_of(purrr::set_names(paste0(num_vars, "_tiles"), num_vars)))
  )
}





# À ajouter :
#   Pour le montant maximal d'un individu (<85% de total), il faudrait créer un fichier à part.
#   Pour les totaux : erreur s'ils ne contiennent pas l'intitulé "Total|Ensemble" (s'il a été modifié) ;
#       erreur si "Total|Ensemble" hors d'un total.
#   Calculer les totaux par formules ?
#   Un seul tableau si ce n'est pas pondéré (all(wn == n)).
#   Les paramètres à utiliser dans tab_pct / tab_ci
#   Un fichier par tableau ?

#Règles CASD :
# Entreprises : aucun total ne doit être inférieur à 3.
# Individus ménages : Aucune case ne doit être égale au total. À 98 % du total ? 95 % ?
# DADS/SIASP/FGE : les tableaux au lieu de résidence / au lieu de travail ont des critères spécifiques
# DGFiP données fiscales : aucune case ne doit comporter moins de 11 individus.
# DARES : minimum 5 individus/ménages ; minimum 3 entreprises (max < 85%)




# data <- pan |> as_tibble() |> mutate(ANNEE = as.factor(ANNEE))
# row_var = expr(GRADE)
# col_var = expr(STATUT)

tab_majo <- function(data, row_var, col_var, seuil = 0.95) {
  # data <- siasp_EF
  # row_var <- expr(GRAD)
  # col_var <- expr(EMP_MIN)
  # seuil <- 0.95
  
  row_var <- ensym(row_var)
  col_var <- ensym(col_var)
  
  lvs <- levels(pull(data, !!col_var))
  
  data <- select(data, !!row_var, !!col_var) #|>
  #nest(!!col_var := !!col_var)
  
  row_var <- as.character(row_var)
  col_var <- as.character(col_var)
  both_vars <- c(row_var, col_var)
  
  
  data <- data |> as.data.table() |> na.omit()
  
  data <- data[, .(N = .N), keyby = both_vars]
  setorderv(data, cols = c(row_var, "N"), order = c(1, -1))
  data[, pct := N/sum(N), keyby = row_var]
  data <- data[, map(.SD, first),
               keyby = row_var, .SDcols = c(col_var, "pct")]
  
  data <- data[, c(row_var, col_var, "pct"), with = FALSE] |> as_tibble() 
  
  data <- data |> 
    mutate(!!sym(col_var) := if_else(pct >= seuil, 
                                     true  = fct_expand(!!sym(col_var), "Multiple"), 
                                     false = factor("Multiple"))
    ) |> 
    select(-pct)
  
  # data[, c(row_var, map(.SD, ~ ) ), 
  #      .SDcols = col_var,
  # ]  data[, c(row_var, map(.SD, ~ if_else(pct >= seuil, 
  #                                         true  = fct_expand(., "Multiple"), 
  #                                         false = factor("Multiple"))) ), 
  #         .SDcols = col_var,
  # ]
  data
}









# A fastest function for simple tables with data.tables, for huge databases
# (work with both tibbles and lazy tibbles)
tab_dt <- function(data, groups, num_vars = NULL, vars_levels_list = NULL, 
                   n_min = 5, na.rm = FALSE, wt = NULL, names_to_stack = FALSE, 
                   condition) {
  
  if (length(num_vars) > 0) {
    num_vars[!num_vars %in% groups]
    if(all(is.na(num_vars))) num_vars <- NULL
  }
  
  if ("tbl_lazy" %in% class(data)) {
    DATA <- data
    
    if (!missing(condition)) {
      condition <- enquo(condition)
      data <- DATA |> 
        mutate(champ = !!condition) |>
        select(all_of(c(groups, num_vars, names(vars_levels_list))), 
               champ, any_of(c(wt)) ) |>
        filter(champ) |> 
        collect() |> 
        select(-champ) |> 
        as.data.table()
      
    } else {
      data <- DATA |> 
        select(all_of(c(groups, num_vars, names(vars_levels_list))),
               any_of(c(wt)) ) |>
        collect() |> 
        as.data.table()
    }
    
    vars_to_factor <- map_lgl(data, is.character) ; vars_to_factor <- names(vars_to_factor)[vars_to_factor]
    if (length(vars_to_factor) > 0) {
      data[, eval(vars_to_factor) := map(.SD, as.factor), .SDcols = vars_to_factor]
    }
    
  }
  
  
  if(length(num_vars) > 0) {
    num_vars_sd <- c(num_vars, paste0(num_vars, "_sd"))
  } else {
    num_vars_sd <- NULL
  }
  
  if (length(wt) > 0) data$wt <- pull(data, wt)
  
  if (length(vars_levels_list) > 0 ) {
    vars_levels_list_flat <-  vars_levels_list |>
      imap_dfr(~ tibble(var = .y, cat = .x)) |>
      mutate(vect = set_names(cat, var)) |> pull(vect)
    
    var_names <- paste0(
      str_remove_all(vars_levels_list_flat, tabxplor:::cleannames_condition()), 
      " [", names(vars_levels_list_flat), "]"
    ) |> set_names(names(vars_levels_list_flat))
    # var_names_noSX <- var_names[!names(var_names) == "SEXE"]
    
    
    for(i in 1:length(vars_levels_list_flat)) {
      var <- names(vars_levels_list_flat)[i]
      cat <- vars_levels_list_flat[i]
      var_name <- var_names[i]
      
      data[, eval(var_name) := as.integer(eval(str2expression(var)) == cat) ]
    }
    
  } else {
    var_names <- NULL
  }
  
  if (length(wt) == 0) {
    suppressWarnings({
      res <- data[
        , 
        .(n  = .N, 
          map(.SD, ~ mean(., na.rm = TRUE)),
          map(.SD[, ..num_vars], ~ sd(., na.rm = TRUE)) |> 
            set_names(paste0(num_vars, "_sd"))
        ) |> flatten(), 
        .SDcols = c(num_vars, var_names),
        keyby = groups
      ][,
        c(num_vars_sd, var_names) := map(.SD, ~ ifelse(is.nan(.), NA, .)),
        .SDcols = c(num_vars_sd, var_names)
      ][, 
        c("n", num_vars_sd, var_names) := map(.SD, ~ ifelse(n < n_min, NA, .)),
        .SDcols = c("n", num_vars_sd, var_names)
      ]
    })
    
  } else {
    suppressWarnings({
      res <-  data[
        , 
        .(n  = .N, 
          wn = sum(wt, na.rm = TRUE), 
          map(.SD, ~ weighted.mean(., wt, na.rm = TRUE)),
          map(.SD[, ..num_vars], ~ tabxplor:::weighted.var(., wt, na.rm = TRUE)) |> 
            set_names(paste0(num_vars, "_sd"))
        ) |> flatten(), 
        .SDcols = c(num_vars, var_names),
        keyby = groups
      ][,
        c(num_vars_sd, var_names) := map(.SD, ~ ifelse(is.nan(.), NA, .)),
        .SDcols = c(num_vars_sd, var_names)
      ][, 
        c("n", "wn", num_vars_sd, var_names) := map(.SD, ~ ifelse(n < n_min, NA, .)),
        .SDcols = c("n", "wn", num_vars_sd, var_names)
      ]
    })
    
  }
  
  if (na.rm) {
    for(gr in groups) {
      res <- res[!is.na(eval(str2expression(gr))), ]
    }
    
  } else {
    setorderv(res, cols = groups, na.last = TRUE)
  }
  
  if (names_to_stack) {
    groups_no_year <- groups[groups != "ANNEE"]
    
    res[, eval(groups_no_year) := map(.SD, as.factor), .SDcols = groups_no_year]
    
    var_names <- set_names(groups_no_year,
                           paste0("var_", 1:length(groups_no_year)) )
    res <- res |> as_tibble() |>
      rename(all_of(var_names)) |> 
      mutate(variables = list(groups_no_year), .before = 1)
    
    res <- res |> select(variables, starts_with("var_"), everything())
  }
  
  res
}






tab_dt_to_tab <- function(data, n_var, group, comp = "tab", totrow_condition, 
                          digits_mean = 2L) {
  
  pct_vars <- names(data)[str_detect(names(data), "\\[") & map_lgl(data, is.numeric)] 
  
  mean_vars <- names(data)[!str_detect(names(data), "\\[") & map_lgl(data, is.double) &
                             !str_detect(names(data), "_sd$")]
  
  # pct_vars <- 
  # tibble(pct_vars = names(data)[str_detect(names(data), "\\[")]) |> 
  #   mutate(col_var = str_extract(pct_vars, "\\[.+\\]") |> str_remove_all("\\[|\\]") #, 
  #          # new_name = str_remove_all(pct_vars, "\\[.*\\]") |> 
  #          #   str_replace("\\.", "") |> str_squish()
  #   )
  # 
  # mean_vars <- 
  #   tibble(
  #     mean_vars = names(data)[!str_detect(names(data), "\\[") & map_lgl(data, is.double) &
  #                               !str_detect(names(data), "_sd$")], 
  #     sd_vars   = names(data)[!str_detect(names(data), "\\[") & map_lgl(data, is.double) &
  #                               str_detect(names(data), "_sd$")]
  #   )
  
  
  with_totrows <- ! missing(totrow_condition) # with_totrows <- TRUE
  if (with_totrows) {
    totrow_condition <- enquo(totrow_condition) # totrow_condition <- expr(clust == as.character(small_clust))
  } else {
    totrow_condition <- expr(FALSE)
  }
  
  data <- data |> 
    mutate(
      across(all_of(pct_vars), 
             ~ fmt(n = as.integer(round(!!sym(n_var) * .)), 
                   type = "row", 
                   pct  = ., 
                   in_totrow = !!totrow_condition, 
                   col_var = str_extract(cur_column(), "\\[.+\\]") |> str_remove_all("\\[|\\]")
             )
      ), 
      
      across(all_of(mean_vars), 
             ~ fmt(n = !!sym(n_var), 
                   type = "mean", 
                   mean = ., 
                   digits = digits_mean, 
                   in_totrow = !!totrow_condition,
                   var  = eval_tidy(sym(paste0(cur_column(), "_sd"))) ^ 2, 
                   col_var = cur_column()
             )
      )
    ) |> 
    select(-ends_with("_sd") ) # -n_clust
  
  if(!missing(group) & comp == "tab") {
    data <- data |> group_by(!!sym(group))
  }
  
  if (with_totrows | comp == "all") {
   if (!missing(group)) {
     data <- data |> arrange(!!sym(group), is_totrow(!!sym(pct_vars[1])))
   } else {
     data <- data |> arrange(is_totrow(!!sym(pct_vars[1])))
   }

    
    if (comp == "all") {
      general_total_row <- data |> 
        filter(!is_totrow(!!sym(pct_vars[1]))) |> 
        summarise(
          across(where(~is_fmt(.) & get_type(.) != "mean"), 
                 ~ fmt(
                   n         = sum(.$n, na.rm = TRUE), 
                   type      = "row", 
                   pct       = weighted.mean(.$pct, .$n, na.rm = TRUE),
                   in_tottab = TRUE, 
                   in_totrow = TRUE,
                   col_var   =  get_col_var(.)
                 )
          ), 
          
          across(where(~is_fmt(.) & get_type(.) == "mean"), 
                 ~ fmt(n         = sum(.$n, na.rm = TRUE), 
                       type      = "mean", 
                       mean      = weighted.mean(.$mean, .$n, na.rm = TRUE),
                       digits    = digits_mean, 
                       in_tottab = TRUE, 
                       in_totrow = TRUE,
                       var       = agregate_sd(.$mean, 
                                               sd = sqrt(.$var), 
                                               wt = .$n) ^ 2,
                       col_var   = get_col_var(.)
                 )
          ), 
          
        )
      
      data <- bind_rows(data, general_total_row)
      data <- data |> 
        mutate(across(where(is.factor), fct_drop), 
               
               across(where(~is.factor(.) & !nlevels(.) <= 1), 
                      ~ if_else(is_tottab(!!sym(pct_vars[1])) & is_totrow(!!sym(pct_vars[1])), 
                                true  = factor("Total", levels = c(levels(.), "Total")), 
                                false = .)), 
               
               across(where(~is.factor(.) & nlevels(.) <= 1), 
                      ~ if_else(is_tottab(!!sym(pct_vars[1])) & is_totrow(!!sym(pct_vars[1])), 
                                true  = factor(levels(.)[1], levels = levels(.)), 
                                false = .)), 
               
        )
      
    }
    
    data <- data |> 
        mutate(
          across(all_of(pct_vars) , ~ mutate(., diff = pct  - last(pct))  |> 
                   set_diff_type("tot") |> set_color("diff") ), 
          
          across(all_of(mean_vars), ~ mutate(., diff = mean / last(mean)) |> 
                   set_diff_type("tot") |> set_color("diff") ), 
        )
    
  } else {
    data <- data |> 
      # tab_tot(totcol = "last") |>
      mutate(
        # across(where(is_totcol), ~ mutate(., pct = 1)), 
        # across(all_of(pct_vars), ~ if_else(
        #   is_totrow(.),
        #   true  = mutate(., 
        #                  n         = sum(n, na.rm = TRUE),
        #                  pct       = weighted.mean(pct, n, na.rm = TRUE), 
        #                  in_refrow = TRUE
        #   ), 
        #   false = .)
        # ), 
        # across(all_of(pct_vars) , ~ mutate(., diff = pct  - last(pct))  |> 
        #          set_diff_type("tot") |> set_color("diff") ), 
        # across(all_of(mean_vars), ~ mutate(., diff = mean / last(mean)) |> 
        #          set_diff_type("tot") |> set_color("diff") ), 
        
        across(all_of(pct_vars) , ~ mutate(., diff = pct  - weighted.mean(pct, n, na.rm = TRUE))  |> 
                 set_diff_type("tot") |> set_color("diff") ), 
        
        across(all_of(mean_vars), ~ mutate(., diff = mean / weighted.mean(mean, n, na.rm = TRUE)) |> 
                 set_diff_type("tot") |> set_color("diff") ), 
      )
    
    # data <- data |> select(-Total)
    # totrows <- is_totrow(data)
    # data <- data |> filter(!totrows)
    
  }
  
  
  if(!missing(group) & comp == "tab") {
    data <- data |> ungroup()
  }

  
  data |> new_tab()
  
  # data |>
  #   rename_with(~ str_remove_all(., "\\[.*\\]") |> str_replace("\\.", "") |> str_squish()) #|>
  #   pull(mob_DEPT)  |> vctrs::vec_data()
  
}













# Utils functions ----

# Function to calculate a score with the first levels of many variables
# Ajout par rapport au départ : NA = +0, on ne compte pas la première modalité
score_from_lv1 <- function (data, name, vars_list) {
  name <- rlang::ensym(name)
  data <- data |> mutate(!!sym(name) := 0L)
  
  reduce(
    vars_list, 
    .init = mutate(data, across(all_of(vars_list), ~ fct_explicit_na(., "NA"))), 
    
    .f = ~ mutate(.x, !!name := if_else(
      condition = !!sym(.y) == levels(as.factor(!!sym(.y)))[1],
      true  = !!name + 1L, 
      false = !!name
    )
    )
  ) 
}

png600_save <- function(plot = last_plot(), filename, device = "png",
                        width = 17, height = 13.5, scale = 1.5, ...) {
  ggsave(plot = plot, filename = paste0(filename, ".png"), 
         path = "Plots", device = device, dpi = 600, units = "cm", 
         width  = width, height = height, scale = scale, ...
  )
  invisible(plot)
}

samp <- function(df, n = 100) {
  df |> slice_sample(n = n) |> print(n = n)
  message(paste0("sampling ", n, "rows over ", nrow(df)))
  invisible(df)
}

cd <- function(pattern, data = nomenc_CORPS_GRADE_EF_final) {
  levels(data$CORPS)[str_detect(levels(data$CORPS), pattern)]
}




# chr <- cor_apparie_NNE$lib_corps
str_to_min_sans_accent <- function(chr) {
  chr |> 
    stringi::stri_trans_general(id = "Latin-ASCII") |>
    str_to_sentence() |>
    str_replace_all("\\.", " ") |>
    str_replace_all("\"", "'") |>
    str_replace_all("[:punct:]", " ") |>
    str_squish()
}





lv <- function(data, ..., lines = TRUE) {
  vars <- rlang::ensyms(...)
  if (lines == TRUE ){
    line_end <- ",\n "
    at_the_start <- " == c(\n"
    at_the_end <- "\n)"
  } else {
    line_end <- ", "
    at_the_start <- " == c( "
    at_the_end <- " )"
  }
  
  lvls <- dplyr::select(data, !!!vars) %>% dplyr::mutate_all(as.factor) %>% 
    purrr::map( function(.x) stringr::str_c('"', levels(.x), '"') %>% stringr::str_c(collapse = line_end) %>% 
                  purrr::prepend(at_the_start) %>% append(at_the_end) %>% str_c(collapse = ""))
  
  purrr::map2_chr(lvls, vars, ~ stringr::str_c(.y, .x)) %>% 
    stringr::str_c(collapse = "\n\n") %>% cat()
}

# repex <- function(formula, pattern_replacement_named_vector, exec = TRUE) {
#   formula_text <- formula %>% rlang::f_text(.)
#   
#   if (!missing(pattern_replacement_named_vector)) {
#     formula_text <- purrr::reduce2(pattern_replacement_named_vector,
#                                    names(pattern_replacement_named_vector),
#                                    .init = formula_text, .f = ~ stringr::str_replace_all(..1, ..2, ..3))
#     
#     formula_text <- formula_text %>% str2lang()
#     
#     formula <- formula %>% rlang::`f_rhs<-`(formula_text)
#   }
#   
#   if (exec == TRUE) {
#     formula %>% rlang::as_function() %>% rlang::exec()
#   } else {
#     formula
#   }
# }







# #Furrr to use map functions with multicore (future_map) :
# if (future::availableCores() > 1) { nb_cores <- future::availableCores() - 1 } else { nb_cores <- 1 }
# future::plan(multisession, workers = nb_cores) # multiprocess, because multicore don't work on Windows ?
# options(future.globals.maxSize = 48*1024^3) # Nécessaire mais mange trop de RAM ?

#Pour tester ce qui prend du temps avec lineprof : 
# copier fonction dans nouveau script + source()
# tab <- tabw(data, PR3, EMP_CJUR, wt = EXTRI, show_na = FALSE)
# timeprof <- lineprof(tabw(data, var))
# timeprof %>% shine() #Navigate through links 
#Pour accélérer le code : 
# - Regrouper les group_by identiques qui sont très couteux ! 
# - Fonctions vectorisées : rowSums(), colSums(), rowMeans(), colMeans(), cumsum(),  diff()
# - Pour vecteur ET df : x[is.na(x)] <- 0 will replace all missing values with 0.
# - If you can figure out a way to use matrix algebra, you'll often get a very fast solution



# #Remove objects in memory, except the selected ones (not working...)
# clear_memory_and_reload <- function(keep) {
#   objects_list <- ls(name = rlang::global_env(), all.names = TRUE)
#   objects_list <- objects_list[!objects_list %in% keep]
#   rm(list = objects_list, pos = rlang::global_env())
#   gc()
#   source("Demarrage.R", encoding = "UTF-8")
# }




# wt_panel(n, before02 = FALSE)
unwt_panel <- function(wn, before02 = TRUE) {
    case_when(
      before02 & wn >= 144 ~ as.integer(wn %/% 24),
      before02 & wn >= 136 ~ 5L,
      before02 & wn >= 128 ~ 4L,
      before02 & wn >= 120 ~ 3L,
      before02 & wn >= 100 ~ 2L,
      
      wn >= 72 ~ as.integer(wn %/% 12),
      wn >= 68 ~ 5L,
      wn >= 64 ~ 4L,
      wn >= 60 ~ 3L,
      wn >= 50 ~ 2L,
    )

  ## was on chronogramme() code
  #case_when(
  #  wn >= 144  ~ as.integer(wn %/% 24),
  #  wn >= 136  ~ 5L,
  #  wn >= 128  ~ 4L,
  #  wn >= 120  ~ 3L,
  #  wn >= 112  ~ 2L, 
  #)
  
  # if (before02) {
  #   case_when(
  #     wn >= 144 ~ as.integer(wn %/% 24),
  #     wn >= 136 ~ 5L,
  #     wn >= 128 ~ 4L,
  #     wn >= 120 ~ 3L,
  #     wn >= 100 ~ 2L,
  #   )
  #   
  # } else {
  #   case_when(
  #     wn >= 72 ~ as.integer(wn %/% 12),
  #     wn >= 68 ~ 5L,
  #     wn >= 64 ~ 4L,
  #     wn >= 60 ~ 3L,
  #     wn >= 50 ~ 2L,
  #   )
  # }
  }



# chrono_tabs_sort |> 
#   pull(data) |> nth(1) |> print(n = 400)
# 
# chrono_tabs_sort |> 
#   pull(data) |> nth(5) |> print(n = 400)
# 
# 
# chrono_tabs_sort |> 
#   filter(before02) |> 
#   unnest(data) |> 
#   mutate(wn = trunc(wn) ) |>
#   group_by(wn) |>
#   summarise(nb = n()) |>
#   ggplot(aes(x= wn, y = nb)) + geom_bar(stat = "identity") + xlim(c(100, 168)) + 
#   geom_text(aes(label = wn), nudge_y = 100, size = 2)
# 
# chrono_tabs_sort |> 
#   filter(before02) |> 
#   unnest(data) |> 
#   mutate(wn = trunc(wn) ) |>
#   filter(wn <= 144) |>
#   group_by(wn) |>
#   summarise(nb = n()) |>
#   print(n = 100)
# 
# 
# chrono_tabs_sort |> 
#   filter(!before02) |> 
#   unnest(data) |> 
#   mutate(wn = trunc(wn) ) |>
#   ggplot(aes(x= wn)) + geom_histogram(binwidth = 1) + xlim(c(45, 100))
# 
# chrono_tabs_sort |> 
#   filter(!before02) |> 
#   unnest(data) |> 
#   mutate(wn = trunc(wn) ) |>
#   filter(wn <= 72) |>
#   group_by(wn) |>
#   summarise(nb = n()) |>
#   print(n = 100)
# 
# 
# 
# chrono_tabs |> ggplot(aes(x = wn)) + geom_histogram(binwidth = 1) + xlim(c(0, 200))
# chrono_tabs_sort$data[[8]] |> 
#   ggplot(aes(x = n)) + geom_histogram(binwidth = 1) + xlim(c(0, 10))
# 
# chrono_tabs |>
#   group_nest(description, field, time_var, analysis_group, keep = TRUE) |> 
#   mutate(before02 = map_lgl(data, ~ min(.$wn, na.rm = FALSE) < 95 )) |> 
#   pull(data) |> nth(5) |> 
#   ggplot(aes(x= wn)) + geom_histogram(binwidth = 1) + xlim(c(0, 200))
# 
# 
# chrono_tabs |>
#   group_nest(description, field, time_var, analysis_group, keep = TRUE) |> 
#   pull(data) |> nth(7) |> filter(clust == 1) |> print(n = 100)
# 
# chrono_tabs |>
#   group_nest(description, field, time_var, analysis_group, keep = TRUE) |> 
#   pull(data) |> nth(10) |> filter(clust == 1) |> print(n = 100)


sort_factor_as_integer <- function(.fct) {
  fct_drop(.fct) |> fct_relevel( ~ as.character(sort(as.integer(.))) )
  
}



# breaks <- range(chrono_data$time)
break_uniform_integer <- function(range) {
  
  if (range[2] >= 20) {
    return( c(range[1], seq(10, range[2], by = 10)) |> unique() )
    #return( c(seq(range[1], range[2], by = 10), range[2]) |> unique() )
  }
  
  nb_breaks <- 
    list(
      "1" = ggplot2:::breaks(range, equal = "numbers", n = 1),
      "2" = ggplot2:::breaks(range, equal = "numbers", n = 2),
      "3" = ggplot2:::breaks(range, equal = "numbers", n = 3),
      "4" = ggplot2:::breaks(range, equal = "numbers", n = 4) #,
      #"5" = ggplot2:::breaks(range, equal = "numbers", n = 5)
    ) |> 
    map(unname)
  
  whole_number_test <- nb_breaks |> 
    map_lgl(~ all(round(. - round(.), 2) == 0))
  
  if (any(whole_number_test)) {
   nb_breaks[[last(names(whole_number_test)[whole_number_test])]]
  } else {
   nb_breaks[["1"]]
  }
  
}




# ggplot themes ----

pyramid_options <- function() {}
pyramid_options <- list(
  ggplot2::theme(legend.position = "bottom", 
                 plot.background  = ggplot2::element_rect(color = "white", fill = "white"), 
                 panel.background = ggplot2::element_rect(color = "white", fill = "white"), 
                 panel.border     = ggplot2::element_rect(color = "black", fill = NA), 
                 panel.grid.major = ggplot2::element_line(color = "grey45", linewidth = 0.4, 
                                                          linetype = "dashed"), 
                 panel.grid.minor.x = ggplot2::element_line(color = "grey80", linewidth = 0.4, 
                                                            linetype = "dotdash"), 
                 strip.background = ggplot2::element_rect(color = "black"), 
                 strip.text       = ggplot2::element_text(face = "bold"),
                 
                 #legend.box.margin = margin(),
                 legend.box.spacing = ggplot2::unit(0, units = "cm"),
                 legend.text      = ggplot2::element_text(size = 7)
  )
)











# Parquet to R -------------------------------------------------------


# tblname <- names(parquet)
# create_type = TRUE
# writedb = FALSE
# maxlv = 5000

# Very long with writedb = TRUE ; high memory use or OK ? Limit number of levels ?
dbDictionaryToFactors <- function(db, tblname, arrow_parquet, maxlv = 500, out = character(),
                                  see_errors = FALSE, writedb = FALSE) {
  
  # If levels are to be translated to ASCII, it must be done before saving parquet
  #   stringi::stri_trans_general(factors_levels[[3]], "latin-ascii")
  
  #Reading the parquet file informations in arrow :
  is_dictio <- rep(FALSE, length(arrow_parquet$columns))
  for (i in 1:length(arrow_parquet$columns)) {
    is_dictio[i] <- arrow_parquet$schema[[i]]$type$name == "dictionary"
  } 
  
  #Only keep factor/dictionary if the number of levels in less than 10000
  factors_levels <- arrow_parquet$columns[is_dictio] |> #missing factors not in row group 1 ?
    map(~ as.character(.$chunks[[1]]$dictionary() ) ) |> 
    map(~ discard(., . == "NULL" )) |>
    set_names(names(arrow_parquet)[is_dictio])
  
  if (length(out) != 0) {
    factors_levels <- discard(factors_levels, names(factors_levels) %in% out)
  }
  
  
  if (length(factors_levels) == 0) return(character())
  
  factors_levels <- factors_levels[map_lgl(factors_levels, ~ length(.) < maxlv
                                           #~ length(.) >= 2000 & length(.) < 10000
  )]
  
  factors_levels <- map(factors_levels, sort)
  
  factors_create_type <-
    imap(factors_levels,
         ~ paste0("CREATE TYPE ", paste0(.y, "_T"), 
                  " AS ENUM (", 
                  paste0("'", .x, "'", collapse = ", "),
                  ");")
    )
  
  # dbSendQuery(db, factors_create_type[[10]]) #Ok, but too long somehow ?
  
  # dbSendQuery(db, factors_create_type[[17]])
  # dbSendQuery(db, factors_create_type[[24]])
  
  # Retrieve informations about columns in duckdb
  #   factors are sorted in alphabetical order :
  # factors_columns <- as_tibble(dbGetQuery(db, "SELECT * FROM duckdb_columns();")) |> 
  #   filter(table_name == "ee" & data_type == "VARCHAR") |>
  #   pull(column_name)
  #   
  # # Too long (~14s) : 
  # factors_levels <- factors_columns |>
  #   map(~ dbGetQuery(db, paste0("SELECT DISTINCT ", ., 
  #                       " FROM ", tblname, " ORDER BY ", ., " ;"))) |> 
  #   set_names(factors_columns) |> 
  #   map(deframe) |> 
  #   map(~ discard(., is.na(.)))
  
  dbSendQuery_no_error <- safely(dbSendQuery, quiet = see_errors)
  walk(factors_create_type, ~ dbSendQuery_no_error(db, .))
  
  if (writedb) { # With CREATE VIEW : Error: Can only update base table!
    factors_update <- 
      map(names(factors_levels),
          ~ paste0("UPDATE ", tblname, " SET ", ., " = CAST(", ., " AS ", ., "_T);")
      ) |>
      set_names(names(factors_levels)) #useless ?
    
    iwalk(factors_update, ~ { # Error: Could not convert string 'NULL' to UINT8
      message(paste0("writing ", .y))
      dbSendStatement(db, .x)
    }
    )
  }
  
  
  
  
  #dbSendStatement(db, factors_update[[1]])
  ## paste0(
  ##   "UPDATE ", tblname, " SET ", 
  ##   map_chr(names(factors_levels), ~ paste0(., " = CAST(", ., " AS ", ., "_T)")) |> paste0(collapse = ", "),
  ##   ";"
  ## )
  
  # factors_select <- 
  #   paste0(
  #     "SELECT ",  
  #     map_chr(names(factors_levels), ~ paste0("CAST(", ., " AS ", ., "_T) AS ", .)) |> paste0(collapse = ", "),
  #     "\nFROM ", tblname, ";"
  #   )
  
  # rs    <- dbSendQuery(db, factors_select)
  # eefct <- as_tibble(dbFetch(rs))
  # 
  # #In chunks 
  # rs    <- dbSendQuery(db, factors_select)
  # eefct <- as_tibble(dbFetch(rs, n = 100))
  
  
  # Individual tests :
  # dbSendQuery(db, paste0("CREATE TYPE ", paste0(factors_columns[[3]], "_T"), 
  #                        " AS ENUM (", 
  #                        paste0("'", factors_levels[[3]], "'", collapse = ", "),
  #                        ");"))
  # 
  # dbGetQuery(db, paste0("SELECT CAST(", factors_columns[[1]], " AS ", 
  #                       paste0(factors_columns[[1]], "_T"), 
  #                       ") AS ", factors_columns[[1]], " FROM ", tblname, ";")) |>
  #   as_tibble()
  # 
  # # # not working
  # # # style : dbGetQuery(db, "SELECT CAST(city AS city_levels) AS city FROM weather")
  # # dbSendQuery(db, paste0("UPDATE ", tblname, 
  # #   " SET ", factors_columns[[1]], " = CAST(", factors_columns[[1]], 
  # #   " AS ", paste0(factors_columns[[1]], "_T"), ")"))
  
  # eefct <- as_tibble(dbGetQuery(db, factors_select))
  # eefct
  # # Memory usage divided by 2 with factors, compared to strings
  # #     print(object.size(eefct), units = "Mb") #1127Mb
  # #   eetbl <- tbl(db, "ee") |> select(all_of(names(factors_levels))) |> collect()
  # #     print(object.size(eetbl), units = "Mb") #2253.9 Mb
  
  return(names(factors_levels))
}

# EE |> 
#   select(NAFall) |>
#   mutate(NAFall = CAST(NAFall %AS% NAFall_T)) |> #show_query()
#   collect()

# dbplyr::translate_sql(mutate(EE, SEXE = CAST(SEXE %AS% SEXE_T)), con = db)


dbDictionaryToFactors_cat_mutate <- function(factor_vars, tblname, dbname = "db") {
  if (missing(tblname)) tblname <- character()
  
  starting <- 
    paste0(stringr::str_to_upper(tblname), " <- tbl(", dbname, ', "', tblname, '")')
  
  if (missing(factor_vars)) {
    writeLines(starting)
    return(invisible(starting))
  } 
  if (length(factor_vars) == 0) {
    writeLines(starting)
    return(invisible(starting))
  } 
  
  max_length <- max(stringr::str_length(factor_vars))
  res <- paste0(starting, " |> ",
                "\n  mutate(",
                map_chr(factor_vars,
                        ~ paste0( "\n    ",
                                  stringr::str_pad(., max_length, "right"),
                                  " = CAST(", stringr::str_pad(., max_length, "right"),
                                  " %AS% ", paste0(., "_T)"))) |>
                  paste0(collapse = ", "),
                "\n  )"
  )
  writeLines(res)
  return(invisible(res))
}
# dbDictionaryToFactors_cat_mutate("SEXE2", "sexe")
# dbDictionaryToFactors_cat_mutate()























# Formats and nomenclatures------------------------------------------------------
cser_relabel_short <- function(var) {
  case_when(
    str_detect(var, "^1|^2") ~ "1-Indép",
    str_detect(var, "^3")    ~ "3-CPIS" , 
    str_detect(var, "^4")    ~ "4-PI"   ,
    TRUE                     ~ var
  )
}

# ppp1_relabel_short <- function(var) {
#   #var <- str_remove(var, "\\([^\\)]+\\)") %>% str_squish()
#   case_when(
#     str_detect(var, "^1")    ~ "1-Patrons",
#     str_detect(var, "^2")    ~ "2-Cadres",
#     str_detect(var, "^3")    ~ "3-CBI" , 
#     str_detect(var, "^4")    ~ "4-Professions",
#     str_detect(var, "^5")    ~ "5-Employées",
#     str_detect(var, "^6")    ~ "6-Ouvriers",
#     str_detect(var, "^7")    ~ "7-Agri"
#   )
# }

PCS_create <- function(.data, var = "PCS", EMP_ADM_ENT,
                       res = c("PCS", "CSER", "CSE", 
                               "PPP1", "FAPPP", "PPP2", "PPP3", "PPP4"), 
                       ...) {
  var <- rlang::ensym(var)
  
  .data <- .data |> 
    dplyr::mutate(
      !!var := forcats::fct_relabel(!!var, ~ str_to_upper(str_sub(., 1L, 4L))) |>
        fct_relevel(sort)
    )
  
  if ("CSER" %in% res) {
    .data <- .data |> 
      dplyr::mutate(CSER = fct_relabel(!!var, ~ str_sub(., 1L, 1L)) |> 
                      fct_recode(
                        "NULL"="0",
                        "NULL"="Z",
                        "1-Agriculteurs exploitants"="1",
                        "2-Artisans commerçants patrons"="2",
                        "3-Cadres et PIS"="3",
                        "4-Professions intermédiaires"="4",
                        "5-Employés"="5",
                        "6-Ouvriers"="6",
                      ) |>
                      fct_relevel(sort))
  }
  
  if ("CSE" %in% res) {
    .data <- .data |> 
      dplyr::mutate(CSE = fct_relabel(!!var, ~ str_sub(., 1L, 2L)) |> 
                      fct_recode(
                        "NULL" = "0",
                        "NULL" = "ZZ",
                        "11-Agriculteurs petite exploitation"="11",
                        "12-Agriculteurs moyenne exploitation"="12",
                        "13-Agriculteurs grande exploitation"="13",
                        "21-Artisans"="21",
                        "22-Commerçants et assimilés"="22",
                        "23-Chefs d'entreprise"="23",
                        "31-Professions libérales"="31",
                        "33-Cadres de la fonction publique"="33",
                        "34-Professeurs, scientifiques"="34",
                        "35-Professions info arts spectacles"="35",
                        "37-Cadres admin comm entreprise"="37",
                        "38-Ingénieurs"="38",
                        "42-Professeurs des écoles"="42",
                        "43-PI santé travail social"="43",
                        "44-Clergé"="44",
                        "45-PI administratives FP"="45",
                        "46-PI admin comm entreprise"="46",
                        "47-Techniciens"="47",
                        "48-Agents de maîtrise"="48",
                        "52-Employés FP"="52",
                        "53-Policiers et militaires"="53",
                        "54-Employés admin entreprise"="54",
                        "55-Employés commerce"="55",
                        "56-Employés services aux particuliers"="56",
                        "62-Ouvriers qualifiés industriels"="62",
                        "63-Ouvriers qualifiés artisanaux"="63",
                        "64-Chauffeurs"="64",
                        "65-Ouvriers qualifiés logistique"="65",
                        "67-Ouvriers non qualifiés industriels"="67",
                        "68-Ouvriers non qualifiés artisanaux"="68",
                        "69-Ouvriers agricoles"="69",
                      ) |>
                      fct_relevel(sort))
  }
  
  if(any(c("PPP1", "FAPPP", "PPP2", "PPP3", "PPP4") %in% res)) {
    source("Scripts importés/pcspp.R", encoding = "UTF-8")
    
    missingEMP_ADM_ENT <- missing(EMP_ADM_ENT)
    if (!missingEMP_ADM_ENT) {
      EMP_ADM_ENT <- rlang::ensym(EMP_ADM_ENT)
      
    } else {
      .data <- .data |> 
        dplyr::mutate(EMP_ADM_ENT = factor("1-Admin", levels = c("1-Admin", "2-Entreprises")))
      EMP_ADM_ENT <- rlang::expr(EMP_ADM_ENT)
    }
    
    
    
    .data <- .data |>
      pcspp(profession = !!var, admin_ent = EMP_ADM_ENT, gender_sign = FALSE,
            ...)
    
    .data <- .data |> 
      mutate(PPP1 = fct_relabel(PPP1, ~ case_when(
        str_detect(., "^1")    ~ "1-Patrons",
        str_detect(., "^2")    ~ "2-Cadres",
        str_detect(., "^3")    ~ "3-CBI" , 
        str_detect(., "^4")    ~ "4-Professions",
        str_detect(., "^5")    ~ "5-Employées",
        str_detect(., "^6")    ~ "6-Ouvriers",
        str_detect(., "^7")    ~ "6-Ouvriers" # "7-Filière agri"
      ))
      )
    
    .data <- .data |> 
      mutate(across(all_of(c("PPP2", "PPP3", "PPP4")), ~ fct_relabel(
        .,
        ~ str_replace_all(., stringi::stri_unescape_unicode("\\u2640"), "\\\\u2640") |>
          str_replace_all(stringi::stri_unescape_unicode("\\u2642"), "\\\\u2642" )
      ) 
      )) 
    
    var_not_in_res <- c("PPP1", "FAPPP", "PPP2", "PPP3", "PPP4")
    var_not_in_res <- var_not_in_res[!var_not_in_res %in% res]
    
    if (missingEMP_ADM_ENT) .data$EMP_ADM_ENT <- NULL
    
    .data <- .data |> select(-any_of(c(var_not_in_res)))
  }
  
  if ("PCS" %in% res) {
    .data <- .data |> 
      mutate(!!var := forcats::fct_recode(
        !!var,
        "111A-Agriculteurs sur petite exploitation de céréales-grandes cultures"="111A",
        "111B-Maraîchers, horticulteurs sur petite exploitation"="111B",
        "111C-Viticulteurs, arboriculteurs fruitiers, sur petite exploitation"="111C",
        "111D-Éleveurs d'herbivores, sur petite exploitation"="111D",
        "111E-Éleveurs de granivores et éleveurs mixtes, sur petite exploitation"="111E",
        "111F-Agriculteurs sur petite exploitation sans orientation dominante"="111F",
        "121A-Agriculteurs sur moyenne exploitation de céréales-grandes cultures"="121A",
        "121B-Maraîchers, horticulteurs sur moyenne exploitation"="121B",
        "121C-Viticulteurs, arboriculteurs fruitiers, sur moyenne exploitation"="121C",
        "121D-Éleveurs d'herbivores sur moyenne exploitation"="121D",
        "121E-Éleveurs de granivores et éleveurs mixtes, sur moyenne exploitation"="121E",
        "121F-Agriculteurs sur moyenne exploitation sans orientation dominante"="121F",
        "122A-Entrepreneurs de travaux agricoles à façon, de 0 à 9 salariés"="122A",
        "122B-Exploitants forestiers indépendants, de 0 à 9 salariés"="122B",
        "122C-Patrons pêcheurs et aquaculteurs, de 0 à 9 salariés"="122C",
        "131A-Agriculteurs sur grande exploitation de céréales-grandes cultures"="131A",
        "131B-Maraîchers, horticulteurs, sur grande exploitation"="131B",
        "131C-Viticulteurs, arboriculteurs fruitiers, sur grande exploitation"="131C",
        "131D-Éleveurs d'herbivores, sur grande exploitation"="131D",
        "131E-Éleveurs de granivores et éleveurs mixtes, sur grande exploitation"="131E",
        "131F-Agriculteurs sur grande exploitation sans orientation dominante"="131F",
        "211A-Artisans maçons"="211A",
        "211B-Artisans menuisiers du bâtiment, charpentiers en bois"="211B",
        "211C-Artisans couvreurs"="211C",
        "211D-Artisans plombiers, chauffagistes"="211D",
        "211E-Artisans électriciens du bâtiment"="211E",
        "211F-Artisans de la peinture et des finitions du bâtiment"="211F",
        "211G-Artisans serruriers, métalliers"="211G",
        "211H-Artisans en terrassement, travaux publics"="211H",
        "211J-Entrepreneurs en parcs et jardins, paysagistes"="211J",
        "212A-Artisans mécaniciens en machines agricoles"="212A",
        "212B-Artisans chaudronniers"="212B",
        "212C-Artisans en mécanique générale, fabrication et travail des métaux (hors horlogerie et matériel de précision)"="212C",
        "212D-Artisans divers de fabrication de machines"="212D",
        "213A-Artisans de l'habillement, du textile et du cuir"="213A",
        "214A-Artisans de l'ameublement"="214A",
        "214B-Artisans du travail mécanique du bois"="214B",
        "214C-Artisans du papier, de l'imprimerie et de la reproduction"="214C",
        "214D-Artisans de fabrication en matériaux de construction (hors artisanat d'art)"="214D",
        "214E-Artisans d'art"="214E",
        "214F-Autres artisans de fabrication (y.c. horlogers, matériel de précision)"="214F",
        "215A-Artisans boulangers, pâtissiers, de 0 à 9 salariés"="215A",
        "215B-Artisans bouchers, de 0 à 9 salariés"="215B",
        "215C-Artisans charcutiers, de 0 à 9 salariés"="215C",
        "215D-Autres artisans de l'alimentation, de 0 à 9 salariés"="215D",
        "216A-Artisans mécaniciens réparateurs d'automobiles"="216A",
        "216B-Artisans tôliers-carrossiers d'automobiles"="216B",
        "216C-Artisans réparateurs divers"="216C",
        "217A-Conducteurs de taxis, ambulanciers et autres artisans du transport, de 0 à 9 salariés"="217A",
        "217B-Artisans déménageurs, de 0 à 9 salariés"="217B",
        "217C-Artisans coiffeurs, manucures, esthéticiens, de 0 à 9 salariés"="217C",
        "217D-Artisans teinturiers, blanchisseurs, de 0 à 9 salariés"="217D",
        "217E-Artisans des services divers, de 0 à 9 salariés"="217E",
        "218A-Transporteurs indépendants routiers et fluviaux, de 0 à 9 salariés"="218A",
        "219A-Aides familiaux non salariés ou associés d'artisans, effectuant un travail administratif ou commercial"="219A",
        "221A-Petits et moyens grossistes en alimentation, de 0 à 9 salariés"="221A",
        "221B-Petits et moyens grossistes en produits non alimentaires, de 0 à 9 salariés"="221B",
        "222A-Petits et moyens détaillants en alimentation spécialisée, de 0 à 9 salariés"="222A",
        "222B-Petits et moyens détaillants en alimentation générale, de 0 à 9 salariés"="222B",
        "223A-Détaillants en ameublement, décor, équipement du foyer, de 0 à 9 salariés"="223A",
        "223B-Détaillants en droguerie, bazar, quincaillerie, bricolage, de 0 à 9 salariés"="223B",
        "223C-Fleuristes, de 0 à 9 salariés"="223C",
        "223D-Détaillants en habillement et articles de sport, de 0 à 9 salariés"="223D",
        "223E-Détaillants en produits de beauté, de luxe (hors biens culturels), de 0 à 9 salariés"="223E",
        "223F-Détaillants en biens culturels (livres, disques, multimédia, objets d'art), de 0 à 9 salariés"="223F",
        "223G-Détaillants en tabac, presse et articles divers, de 0 à 9 salariés"="223G",
        "223H-Exploitants et gérants libres de station-service, de 0 à 9 salariés"="223H",
        "224A-Exploitants de petit restaurant, café-restaurant, de 0 à 2 salariés"="224A",
        "224B-Exploitants de petit café, débit de boisson, associé ou non à une autre activité hors restauration, de 0 à 2 salariés"="224B",
        "224C-Exploitants de petit hôtel, hôtel-restaurant, de 0 à 2 salariés"="224C",
        "224D-Exploitants de café, restaurant, hôtel, de 3 à 9 salariés"="224D",
        "225A-Intermédiaires indépendants du commerce, de 0 à 9 salariés"="225A",
        "226A-Agents généraux et courtiers d'assurance indépendants, de 0 à 9 salariés"="226A",
        "226B-Agents de voyage et auxiliaires de transports indépendants, de 0 à 9 salariés"="226B",
        "226C-Agents immobiliers indépendants, de 0 à 9 salariés"="226C",
        "227A-Indépendants gestionnaires de spectacle ou de service récréatif, de 0 à 9 salariés"="227A",
        "227B-Indépendants gestionnaires d'établissements privés (enseignement, santé, social), de 0 à 9 salariés"="227B",
        "227C-Astrologues, professionnels de la parapsychologie, guérisseurs, de 0 à 9 salariés"="227C",
        "227D-Autres indépendants divers prestataires de services, de 0 à 9 salariés"="227D",
        "231A-Chefs de grande entreprise de 500 salariés et plus"="231A",
        "232A-Chefs de moyenne entreprise, de 50 à 499 salariés"="232A",
        "233A-Chefs d'entreprise du bâtiment et des travaux publics, de 10 à 49 salariés"="233A",
        "233B-Chefs d'entreprise de l'industrie ou des transports, de 10 à 49 salariés"="233B",
        "233C-Chefs d'entreprise commerciale, de 10 à 49 salariés"="233C",
        "233D-Chefs d'entreprise de services, de 10 à 49 salariés"="233D",
        "311A-Médecins libéraux spécialistes"="311A",
        "311B-Médecins libéraux généralistes"="311B",
        "311C-Chirurgiens dentistes (libéraux ou salariés)"="311C",
        "311D-Psychologues, psychanalystes, psychothérapeutes (non médecins)"="311D",
        "311E-Vétérinaires (libéraux ou salariés)"="311E",
        "311F-Pharmaciens libéraux"="311F",
        "312A-Avocats"="312A",
        "312B-Notaires"="312B",
        "312C-Experts comptables, comptables agréés, libéraux"="312C",
        "312D-Conseils et experts libéraux en études économiques, organisation et recrutement, gestion et fiscalité"="312D",
        "312E-Ingénieurs conseils libéraux en études techniques"="312E",
        "312F-Architectes libéraux"="312F",
        "312G-Géomètres-experts, huissiers de justice, officiers ministériels, professions libérales diverses"="312G",
        "313A-Aides familiaux non salariés de professions libérales effectuant un travail administratif"="313A",
        "331A-Personnels de direction de la fonction publique (État, collectivités locales, hôpitaux)"="331A",
        "332A-Ingénieurs de l'État (y.c. ingénieurs militaires) et assimilés"="332A",
        "332B-Ingénieurs des collectivités locales et des hôpitaux"="332B",
        "333A-Magistrats"="333A",
        "333B-Inspecteurs et autres personnels de catégorie A des Impôts, du Trésor et des Douanes"="333B",
        "333C-Cadres de la Poste"="333C",
        "333D-Cadres administratifs de France Télécom (statut public)"="333D",
        "333E-Autres personnels administratifs de catégorie A de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="333E",
        "333F-Personnels administratifs de catégorie A des collectivités locales et hôpitaux publics (hors Enseignement, Patrimoine)"="333F",
        "334A-Officiers des Armées et de la Gendarmerie (sauf officiers généraux)"="334A",
        "335A-Personnes exerçant un mandat politique ou syndical"="335A",
        "341A-Professeurs agrégés et certifiés de l'enseignement secondaire"="341A",
        "341B-Chefs d'établissement de l'enseignement secondaire et inspecteurs"="341B",
        "342A-Enseignants de l'enseignement supérieur"="342A",
        "342E-Chercheurs de la recherche publique"="342E",
        "343A-Psychologues spécialistes de l'orientation scolaire et professionnelle"="343A",
        "344A-Médecins hospitaliers sans activité libérale"="344A",
        "344B-Médecins salariés non hospitaliers"="344B",
        "344C-Internes en médecine, odontologie et pharmacie"="344C",
        "344D-Pharmaciens salariés"="344D",
        "351A-Bibliothécaires, archivistes, conservateurs et autres cadres du patrimoine (fonction publique)"="351A",
        "352A-Journalistes (y. c. rédacteurs en chef)"="352A",
        "352B-Auteurs littéraires, scénaristes, dialoguistes"="352B",
        "353A-Directeurs de journaux, administrateurs de presse, directeurs d'éditions (littéraire, musicale, audiovisuelle et multimédia)"="353A",
        "353B-Directeurs, responsables de programmation et de production de l'audiovisuel et des spectacles"="353B",
        "353C-Cadres artistiques et technico-artistiques de la réalisation de l'audiovisuel et des spectacles"="353C",
        "354A-Artistes plasticiens"="354A",
        "354B-Artistes+B493 de la musique et du chant"="354B",
        "354C-Artistes dramatiques"="354C",
        "354D-Artistes de la danse, du cirque et des spectacles divers"="354D",
        "354G-Professeurs d'art (hors établissements scolaires)"="354G",
        "371A-Cadres d'état-major administratifs, financiers, commerciaux des grandes entreprises"="371A",
        "372A-Cadres chargés d'études économiques, financières, commerciales"="372A",
        "372B-Cadres de l'organisation ou du contrôle des services administratifs et financiers"="372B",
        "372C-Cadres spécialistes des ressources humaines et du recrutement"="372C",
        "372D-Cadres spécialistes de la formation"="372D",
        "372E-Juristes"="372E",
        "372F-Cadres de la documentation, de l'archivage (hors fonction publique)"="372F",
        "373A-Cadres des services financiers ou comptables des grandes entreprises"="373A",
        "373B-Cadres des autres services administratifs des grandes entreprises"="373B",
        "373C-Cadres des services financiers ou comptables des petites et moyennes entreprises"="373C",
        "373D-Cadres des autres services administratifs des petites et moyennes entreprises"="373D",
        "374A-Cadres de l'exploitation des magasins de vente du commerce de détail"="374A",
        "374B-Chefs de produits, acheteurs du commerce et autres cadres de la mercatique"="374B",
        "374C-Cadres commerciaux des grandes entreprises (hors commerce de détail)"="374C",
        "374D-Cadres commerciaux des petites et moyennes entreprises (hors commerce de détail)"="374D",
        "375A-Cadres de la publicité"="375A",
        "375B-Cadres des relations publiques et de la communication"="375B",
        "376A-Cadres des marchés financiers"="376A",
        "376B-Cadres des opérations bancaires"="376B",
        "376C-Cadres commerciaux de la banque"="376C",
        "376D-Chefs d'établissements et responsables de l'exploitation bancaire"="376D",
        "376E-Cadres des services techniques des assurances"="376E",
        "376F-Cadres des services techniques des organismes de sécurité sociale et assimilés"="376F",
        "376G-Cadres de l'immobilier"="376G",
        "377A-Cadres de l'hôtellerie et de la restauration"="377A",
        "380A-Directeurs techniques des grandes entreprises"="380A",
        "381A-Ingénieurs et cadres d'étude et d'exploitation de l'agriculture, la pêche, les eaux et forêts"="381A",
        "382A-Ingénieurs et cadres d'étude du bâtiment et des travaux publics"="382A",
        "382B-Architectes salariés"="382B",
        "382C-Ingénieurs, cadres de chantier et conducteurs de travaux (cadres) du bâtiment et des travaux publics"="382C",
        "382D-Ingénieurs et cadres technico-commerciaux en bâtiment, travaux publics"="382D",
        "383A-Ingénieurs et cadres d'étude, recherche et développement en électricité, électronique"="383A",
        "383B-Ingénieurs et cadres de fabrication en matériel électrique, électronique"="383B",
        "383C-Ingénieurs et cadres technico-commerciaux en matériel électrique ou électronique professionnel"="383C",
        "384A-Ingénieurs et cadres d'étude, recherche et développement en mécanique et travail des métaux"="384A",
        "384B-Ingénieurs et cadres de fabrication en mécanique et travail des métaux"="384B",
        "384C-Ingénieurs et cadres technico-commerciaux en matériel mécanique professionnel"="384C",
        "385A-Ingénieurs et cadres d'étude, recherche et développement des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385A",
        "385B-Ingénieurs et cadres de fabrication des industries de transformation (agroalimentaire, chimie, métallurgie, matériaux lourds)"="385B",
        "385C-Ingénieurs et cadres technico-commerciaux des industries de transformations (biens intermédiaires)"="385C",
        "386A-Ingénieurs et cadres d'étude, recherche et développement des autres industries (imprimerie, matériaux souples, ameublement et bois, énergie, eau)"="386A",
        "386D-Ingénieurs et cadres de la production et de la distribution d'énergie, eau"="386D",
        "386E-Ingénieurs et cadres de fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="386E",
        "387A-Ingénieurs et cadres des achats et approvisionnements industriels"="387A",
        "387B-Ingénieurs et cadres de la logistique, du planning et de l'ordonnancement"="387B",
        "387C-Ingénieurs et cadres des méthodes de production"="387C",
        "387D-Ingénieurs et cadres du contrôle-qualité"="387D",
        "387E-Ingénieurs et cadres de la maintenance, de l'entretien et des travaux neufs"="387E",
        "387F-Ingénieurs et cadres techniques de l'environnement"="387F",
        "388A-Ingénieurs et cadres d'étude, recherche et développement en informatique"="388A",
        "388B-Ingénieurs et cadres d'administration, maintenance, support et services aux utilisateurs en informatique"="388B",
        "388C-Chefs de projets informatiques, responsables informatiques"="388C",
        "388D-Ingénieurs et cadres technico-commerciaux en informatique et télécommunications"="388D",
        "388E-Ingénieurs et cadres spécialistes des télécommunications"="388E",
        "389A-Ingénieurs et cadres techniques de l'exploitation des transports"="389A",
        "389B-Officiers et cadres navigants techniques et commerciaux de l'aviation civile"="389B",
        "389C-Officiers et cadres navigants techniques de la marine marchande"="389C",
        "421A-Instituteurs"="421A",
        "421B-Professeurs des écoles"="421B",
        "422A-Professeurs d'enseignement général des collèges"="422A",
        "422B-Professeurs de lycée professionnel"="422B",
        "422C-Maîtres auxiliaires et professeurs contractuels de l'enseignement secondaire"="422C",
        "422D-Conseillers principaux d'éducation"="422D",
        "422E-Surveillants et aides-éducateurs des établissements d'enseignement"="422E",
        "423A-Moniteurs d'école de conduite"="423A",
        "423B-Formateurs et animateurs de formation continue"="423B",
        "424A-Moniteurs et éducateurs sportifs, sportifs professionnels"="424A",
        "425A-Sous-bibliothécaires, cadres intermédiaires du patrimoine"="425A",
        "431A-Cadres infirmiers et assimilés"="431A",
        "431B-Infirmiers psychiatriques"="431B",
        "431C-Puéricultrices"="431C",
        "431D-Infirmiers spécialisés (autres qu'infirmiers psychiatriques et puéricultrices)"="431D",
        "431E-Sages-femmes (libérales ou salariées)"="431E",
        "431F-Infirmiers en soins généraux, salariés"="431F",
        "431G-Infirmiers libéraux"="431G",
        "432A-Masseurs-kinésithérapeutes rééducateurs, libéraux"="432A",
        "432B-Masseurs-kinésithérapeutes rééducateurs, salariés"="432B",
        "432C-Autres spécialistes de la rééducation, libéraux"="432C",
        "432D-Autres spécialistes de la rééducation, salariés"="432D",
        "433A-Techniciens médicaux"="433A",
        "433B-Opticiens lunetiers et audioprothésistes (indépendants et salariés)"="433B",
        "433C-Autres spécialistes de l'appareillage médical (indépendants et salariés)"="433C",
        "433D-Préparateurs en pharmacie"="433D",
        "434A-Cadres de l'intervention socio-éducative"="434A",
        "434B-Assistants de service social"="434B",
        "434C-Conseillers en économie sociale familiale"="434C",
        "434D-Éducateurs spécialisés"="434D",
        "434E-Moniteurs éducateurs"="434E",
        "434F-Éducateurs techniques spécialisés, moniteurs d'atelier"="434F",
        "434G-Éducateurs de jeunes enfants"="434G",
        "435A-Directeurs de centres socioculturels et de loisirs"="435A",
        "435B-Animateurs socioculturels et de loisirs"="435B",
        "441A-Clergé séculier"="441A",
        "441B-Clergé régulier"="441B",
        "451A-Professions intermédiaires de la Poste"="451A",
        "451B-Professions intermédiaires administratives de France Télécom (statut public)"="451B",
        "451C-Contrôleurs des Impôts, du Trésor, des Douanes et assimilés"="451C",
        "451D-Ingénieurs du contrôle de la navigation aérienne"="451D",
        "451E-Autres personnels administratifs de catégorie B de l'État (hors Enseignement, Patrimoine, Impôts, Trésor, Douanes)"="451E",
        "451F-Personnels administratifs de catégorie B des collectivités locales et des hôpitaux (hors Enseignement, Patrimoine)"="451F",
        "452A-Inspecteurs et officiers de police"="452A",
        "452B-Adjudants-chefs, adjudants et sous-officiers de rang supérieur de l'Armée et de la Gendarmerie"="452B",
        "461A-Personnel de secrétariat de niveau supérieur, secrétaires de direction (non cadres)"="461A",
        "461D-Maîtrise et techniciens des services financiers ou comptables"="461D",
        "461E-Maîtrise et techniciens administratifs des services juridiques ou du personnel"="461E",
        "461F-Maîtrise et techniciens administratifs des autres services administratifs"="461F",
        "462A-Chefs de petites surfaces de vente (salariés ou mandataires)"="462A",
        "462B-Maîtrise de l'exploitation des magasins de vente"="462B",
        "462C-Acheteurs non classés cadres, aides-acheteurs"="462C",
        "462D-Animateurs commerciaux des magasins de vente, marchandiseurs (non cadres)"="462D",
        "462E-Autres professions intermédiaires commerciales (sauf techniciens des forces de vente)"="462E",
        "463A-Techniciens commerciaux et technico-commerciaux, représentants en informatique"="463A",
        "463B-Techniciens commerciaux et technico-commerciaux, représentants en biens d'équipement, en biens intermédiaires, commerce interindustriel (hors informatique)"="463B",
        "463C-Techniciens commerciaux et technico-commerciaux, représentants en biens de consommation auprès d'entreprises"="463C",
        "463D-Techniciens commerciaux et technico-commerciaux, représentants en services auprès d'entreprises ou de professionnels (hors banque, assurance, informatique)"="463D",
        "463E-Techniciens commerciaux et technico-commerciaux, représentants auprès de particuliers (hors banque, assurance, informatique)"="463E",
        "464A-Assistants de la publicité, des relations publiques (indépendants ou salariés)"="464A",
        "464B-Interprètes, traducteurs (indépendants ou salariés)"="464B",
        "465A-Concepteurs et assistants techniques des arts graphiques, de la mode et de la décoration (indépendants et salariés)"="465A",
        "465B-Assistants techniques de la réalisation des spectacles vivants et audiovisuels (indépendants ou salariés)"="465B",
        "465C-Photographes (indépendants et salariés)"="465C",
        "466A-Responsables commerciaux et administratifs des transports de voyageurs et du tourisme (non cadres)"="466A",
        "466B-Responsables commerciaux et administratifs des transports de marchandises (non cadres)"="466B",
        "466C-Responsables d'exploitation des transports de voyageurs et de marchandises (non cadres)"="466C",
        "467A-Chargés de clientèle bancaire"="467A",
        "467B-Techniciens des opérations bancaires"="467B",
        "467C-Professions intermédiaires techniques et commerciales des assurances"="467C",
        "467D-Professions intermédiaires techniques des organismes de sécurité sociale"="467D",
        "468A-Maîtrise de restauration : salle et service"="468A",
        "468B-Maîtrise de l'hébergement : hall et étages"="468B",
        "471A-Techniciens d'étude et de conseil en agriculture, eaux et forêt"="471A",
        "471B-Techniciens d'exploitation et de contrôle de la production en agriculture, eaux et forêt"="471B",
        "472A-Dessinateurs en bâtiment, travaux publics"="472A",
        "472B-Géomètres, topographes"="472B",
        "472C-Métreurs et techniciens divers du bâtiment et des travaux publics"="472C",
        "472D-Techniciens des travaux publics de l'État et des collectivités locales"="472D",
        "473A-Dessinateurs en électricité, électromécanique et électronique"="473A",
        "473B-Techniciens de recherche-développement et des méthodes de fabrication en électricité, électromécanique et électronique"="473B",
        "473C-Techniciens de fabrication et de contrôle-qualité en électricité, électromécanique et électronique"="473C",
        "474A-Dessinateurs en construction mécanique et travail des métaux"="474A",
        "474B-Techniciens de recherche-développement et des méthodes de fabrication en construction mécanique et travail des métaux"="474B",
        "474C-Techniciens de fabrication et de contrôle-qualité en construction mécanique et travail des métaux"="474C",
        "475A-Techniciens de recherche-développement et des méthodes de production des industries de transformation"="475A",
        "475B-Techniciens de production et de contrôle-qualité des industries de transformation"="475B",
        "476A-Assistants techniques, techniciens de l'imprimerie et de l'édition"="476A",
        "476B-Techniciens de l'industrie des matériaux souples, de l'ameublement et du bois"="476B",
        "477A-Techniciens de la logistique, du planning et de l'ordonnancement"="477A",
        "477B-Techniciens d'installation et de maintenance des équipements industriels (électriques, électromécaniques, mécaniques, hors informatique)"="477B",
        "477C-Techniciens d'installation et de maintenance des équipements non industriels (hors informatique et télécommunications)"="477C",
        "477D-Techniciens de l'environnement et du traitement des pollutions"="477D",
        "478A-Techniciens d'étude et de développement en informatique"="478A",
        "478B-Techniciens de production, d'exploitation en informatique"="478B",
        "478C-Techniciens d'installation, de maintenance, support et services aux utilisateurs en informatique"="478C",
        "478D-Techniciens des télécommunications et de l'informatique des réseaux"="478D",
        "479A-Techniciens des laboratoires de recherche publique ou de l'enseignement"="479A",
        "479B-Experts salariés ou indépendants de niveau technicien, techniciens divers"="479B",
        "480A-Contremaîtres et agents d'encadrement (non cadres) en agriculture, sylviculture"="480A",
        "480B-Maîtres d'équipage de la marine marchande et de la pêche"="480B",
        "481A-Conducteurs de travaux (non cadres)"="481A",
        "481B-Chefs de chantier (non cadres)"="481B",
        "482A-Agents de maîtrise en fabrication de matériel électrique, électronique"="482A",
        "483A-Agents de maîtrise en construction mécanique, travail des métaux"="483A",
        "484A-Agents de maîtrise en fabrication : agroalimentaire, chimie, plasturgie, pharmacie."="484A",
        "484B-Agents de maîtrise en fabrication : métallurgie, matériaux lourds et autres industries de transformation"="484B",
        "485A-Agents de maîtrise et techniciens en production et distribution d'énergie, eau, chauffage"="485A",
        "485B-Agents de maîtrise en fabrication des autres industries (imprimerie, matériaux souples, ameublement et bois)"="485B",
        "486A-Agents de maîtrise en maintenance, installation en électricité, électromécanique et électronique"="486A",
        "486D-Agents de maîtrise en maintenance, installation en mécanique"="486D",
        "486E-Agents de maîtrise en entretien général, installation, travaux neufs (hors mécanique, électromécanique, électronique)"="486E",
        "487A-Responsables d'entrepôt, de magasinage"="487A",
        "487B-Responsables du tri, de l'emballage, de l'expédition et autres responsables de la manutention"="487B",
        "488A-Maîtrise de restauration : cuisine/production"="488A",
        "488B-Maîtrise de restauration : gestion d'établissement"="488B",
        "521A-Employés de la Poste"="521A",
        "521B-Employés de France Télécom (statut public)"="521B",
        "522A-Agents de constatation ou de recouvrement des Impôts, du Trésor, des Douanes"="522A",
        "523A-Adjoints administratifs de la fonction publique (y.c. enseignement)"="523A",
        "524A-Agents administratifs de la fonction publique (y.c. enseignement)"="524A",
        "525A-Agents de service des établissements primaires"="525A",
        "525B-Agents de service des autres établissements d'enseignement"="525B",
        "525C-Agents de service de la fonction publique (sauf écoles, hôpitaux)"="525C",
        "525D-Agents de service hospitaliers (de la fonction publique ou du secteur privé)"="525D",
        "526A-Aides-soignants (de la fonction publique ou du secteur privé)"="526A",
        "526B-Assistants dentaires, médicaux et vétérinaires, aides de techniciens médicaux"="526B",
        "526C-Auxiliaires de puériculture"="526C",
        "526D-Aides médico-psychologiques"="526D",
        "526E-Ambulanciers salariés (du secteur public ou du secteur privé)"="526E",
        "531A-Agents de police de l'État"="531A",
        "531B-Agents des polices municipales"="531B",
        "531C-Surveillants de l'administration pénitentiaire"="531C",
        "532A-Gendarmes (de grade inférieur à adjudant)"="532A",
        "532B-Sergents et sous-officiers de grade équivalent des Armées (sauf pompiers militaires)"="532B",
        "532C-Hommes du rang (sauf pompiers militaires)"="532C",
        "533A-Pompiers (y.c. pompiers militaires)"="533A",
        "533B-Agents techniques forestiers, gardes des espaces naturels"="533B",
        "533C-Agents de surveillance du patrimoine et des administrations"="533C",
        "534A-Agents civils de sécurité et de surveillance"="534A",
        "534B-Convoyeurs de fonds, gardes du corps, enquêteurs privés et métiers assimilés (salariés)"="534B",
        "541A-Agents et hôtesses d'accueil et d'information (hors hôtellerie)"="541A",
        "541D-Standardistes, téléphonistes"="541D",
        "542A-Secrétaires"="542A",
        "542B-Dactylos, sténodactylos (sans secrétariat), opérateurs de traitement de texte"="542B",
        "543A-Employés des services comptables ou financiers"="543A",
        "543D-Employés administratifs divers d'entreprises"="543D",
        "544A-Employés et opérateurs d'exploitation en informatique"="544A",
        "545A-Employés administratifs des services techniques de la banque"="545A",
        "545B-Employés des services commerciaux de la banque"="545B",
        "545C-Employés des services techniques des assurances"="545C",
        "545D-Employés des services techniques des organismes de sécurité sociale et assimilés"="545D",
        "546A-Contrôleurs des transports (personnels roulants)"="546A",
        "546B-Agents des services commerciaux des transports de voyageurs et du tourisme"="546B",
        "546C-Employés administratifs d'exploitation des transports de marchandises"="546C",
        "546D-Hôtesses de l'air et stewards"="546D",
        "546E-Autres agents et hôtesses d'accompagnement (transports, tourisme)"="546E",
        "551A-Employés de libre service du commerce et magasiniers"="551A",
        "552A-Caissiers de magasin"="552A",
        "553A-Vendeurs non spécialisés"="553A",
        "554A-Vendeurs en alimentation"="554A",
        "554B-Vendeurs en ameublement, décor, équipement du foyer"="554B",
        "554C-Vendeurs en droguerie, bazar, quincaillerie, bricolage"="554C",
        "554D-Vendeurs du commerce de fleurs"="554D",
        "554E-Vendeurs en habillement et articles de sport"="554E",
        "554F-Vendeurs en produits de beauté, de luxe (hors biens culturels) et optique"="554F",
        "554G-Vendeurs de biens culturels (livres, disques, multimédia, objets d'art)"="554G",
        "554H-Vendeurs de tabac, presse et articles divers"="554H",
        "554J-Pompistes et gérants de station-service (salariés ou mandataires)"="554J",
        "555A-Vendeurs par correspondance, télévendeurs"="555A",
        "556A-Vendeurs en gros de biens d'équipement, biens intermédiaires"="556A",
        "561A-Serveurs, commis de restaurant, garçons (bar, brasserie, café ou restaurant)"="561A",
        "561D-Aides de cuisine, apprentis de cuisine et employés polyvalents de la restauration"="561D",
        "561E-Employés de l'hôtellerie : réception et hall"="561E",
        "561F-Employés d'étage et employés polyvalents de l'hôtellerie"="561F",
        "562A-Manucures, esthéticiens (salariés)"="562A",
        "562B-Coiffeurs salariés"="562B",
        "563A-Assistantes maternelles, gardiennes d'enfants, familles d'accueil"="563A",
        "563B-Aides à domicile, aides ménagères, travailleuses familiales"="563B",
        "563C-Employés de maison et personnels de ménage chez des particuliers"="563C",
        "564A-Concierges, gardiens d'immeubles"="564A",
        "564B-Employés des services divers"="564B",
        "621A-Chefs d'équipe du gros œuvre et des travaux publics"="621A",
        "621B-Ouvriers qualifiés du travail du béton"="621B",
        "621C-Conducteurs qualifiés d'engins de chantiers du bâtiment et des travaux publics"="621C",
        "621D-Ouvriers des travaux publics en installations électriques et de télécommunications"="621D",
        "621E-Autres ouvriers qualifiés des travaux publics"="621E",
        "621F-Ouvriers qualifiés des travaux publics (salariés de l'État et des collectivités locales)"="621F",
        "621G-Mineurs de fond qualifiés et autres ouvriers qualifiés des industries d'extraction (carrières, pétrole, gaz...)"="621G",
        "622A-Opérateurs qualifiés sur machines automatiques en production électrique ou électronique"="622A",
        "622B-Câbleurs qualifiés, bobiniers qualifiés"="622B",
        "622G-Plateformistes, contrôleurs qualifiés de matériel électrique ou électronique"="622G",
        "623A-Chaudronniers-tôliers industriels, opérateurs qualifiés du travail en forge, conducteurs qualifiés d'équipement de formage, traceurs qualifiés"="623A",
        "623B-Tuyauteurs industriels qualifiés"="623B",
        "623C-Soudeurs qualifiés sur métaux"="623C",
        "623F-Opérateurs qualifiés d'usinage des métaux travaillant à l'unité ou en petite série, moulistes qualifiés"="623F",
        "623G-Opérateurs qualifiés d'usinage des métaux sur autres machines (sauf moulistes)"="623G",
        "624A-Monteurs qualifiés d'ensembles mécaniques"="624A",
        "624D-Monteurs qualifiés en structures métalliques"="624D",
        "624E-Ouvriers qualifiés de contrôle et d'essais en mécanique"="624E",
        "624F-Ouvriers qualifiés des traitements thermiques et de surface sur métaux"="624F",
        "624G-Autres mécaniciens ou ajusteurs qualifiés (ou spécialité non reconnue)"="624G",
        "625A-Pilotes d'installation lourde des industries de transformation : agroalimentaire, chimie, plasturgie, énergie"="625A",
        "625B-Ouvriers qualifiés et agents qualifiés de laboratoire : agroalimentaire, chimie, biologie, pharmacie"="625B",
        "625C-Autres opérateurs et ouvriers qualifiés de la chimie (y.c. pharmacie) et de la plasturgie"="625C",
        "625D-Opérateurs de la transformation des viandes"="625D",
        "625E-Autres opérateurs et ouvriers qualifiés de l'industrie agricole et alimentaire (hors transformation des viandes)"="625E",
        "625H-Ouvriers qualifiés des autres industries (eau, gaz, énergie, chauffage)"="625H",
        "626A-Pilotes d'installation lourde des industries de transformation : métallurgie, production verrière, matériaux de construction"="626A",
        "626B-Autres opérateurs et ouvriers qualifiés : métallurgie, production verrière, matériaux de construction"="626B",
        "626C-Opérateurs et ouvriers qualifiés des industries lourdes du bois et de la fabrication du papier-carton"="626C",
        "627A-Opérateurs qualifiés du textile et de la mégisserie"="627A",
        "627B-Ouvriers qualifiés de la coupe des vêtements et de l'habillement, autres opérateurs de confection qualifiés"="627B",
        "627C-Ouvriers qualifiés du travail industriel du cuir"="627C",
        "627D-Ouvriers qualifiés de scierie, de la menuiserie industrielle et de l'ameublement"="627D",
        "627E-Ouvriers de la photogravure et des laboratoires photographiques et cinématographiques"="627E",
        "627F-Ouvriers de la composition et de l'impression, ouvriers qualifiés de la brochure, de la reliure et du façonnage du papier-carton"="627F",
        "628A-Mécaniciens qualifiés de maintenance, entretien : équipements industriels"="628A",
        "628B-Électromécaniciens, électriciens qualifiés d'entretien : équipements industriels"="628B",
        "628C-Régleurs qualifiés d'équipements de fabrication (travail des métaux, mécanique)"="628C",
        "628D-Régleurs qualifiés d'équipements de fabrication (hors travail des métaux et mécanique)"="628D",
        "628E-Ouvriers qualifiés de l'assainissement et du traitement des déchets"="628E",
        "628F-Agents qualifiés de laboratoire (sauf chimie, santé)"="628F",
        "628G-Ouvriers qualifiés divers de type industriel"="628G",
        "631A-Jardiniers"="631A",
        "632A-Maçons qualifiés"="632A",
        "632B-Ouvriers qualifiés du travail de la pierre"="632B",
        "632C-Charpentiers en bois qualifiés"="632C",
        "632D-Menuisiers qualifiés du bâtiment"="632D",
        "632E-Couvreurs qualifiés"="632E",
        "632F-Plombiers et chauffagistes qualifiés"="632F",
        "632G-Peintres et ouvriers qualifiés de pose de revêtements sur supports verticaux"="632G",
        "632H-Soliers moquetteurs et ouvriers qualifiés de pose de revêtements souples sur supports horizontaux"="632H",
        "632J-Monteurs qualifiés en agencement, isolation"="632J",
        "632K-Ouvriers qualifiés d'entretien général des bâtiments"="632K",
        "633A-Électriciens qualifiés de type artisanal (y.c. bâtiment)"="633A",
        "633B-Dépanneurs qualifiés en radiotélévision, électroménager, matériel électronique (salariés)"="633B",
        "633C-Électriciens, électroniciens qualifiés en maintenance entretien, réparation : automobile"="633C",
        "633D-Électriciens, électroniciens qualifiés en maintenance, entretien : équipements non industriels"="633D",
        "634A-Carrossiers d'automobiles qualifiés"="634A",
        "634B-Métalliers, serruriers qualifiés"="634B",
        "634C-Mécaniciens qualifiés en maintenance, entretien, réparation : automobile"="634C",
        "634D-Mécaniciens qualifiés de maintenance, entretien : équipements non industriels"="634D",
        "635A-Tailleurs et couturières qualifiés, ouvriers qualifiés du travail des étoffes (sauf fabrication de vêtements), ouvriers qualifiés de type artisanal du travail du cuir"="635A",
        "636A-Bouchers (sauf industrie de la viande)"="636A",
        "636B-Charcutiers (sauf industrie de la viande)"="636B",
        "636C-Boulangers, pâtissiers (sauf activité industrielle)"="636C",
        "636D-Cuisiniers et commis de cuisine"="636D",
        "637A-Modeleurs (sauf modeleurs de métal), mouleurs-noyauteurs à la main, ouvriers qualifiés du travail du verre ou de la céramique à la main"="637A",
        "637B-Ouvriers d'art"="637B",
        "637C-Ouvriers et techniciens des spectacles vivants et audiovisuels"="637C",
        "637D-Ouvriers qualifiés divers de type artisanal"="637D",
        "641A-Conducteurs routiers et grands routiers (salariés)"="641A",
        "641B-Conducteurs de véhicule routier de transport en commun (salariés)"="641B",
        "642A-Conducteurs de taxi (salariés)"="642A",
        "642B-Conducteurs de voiture particulière (salariés)"="642B",
        "643A-Conducteurs livreurs, coursiers (salariés)"="643A",
        "644A-Conducteurs de véhicule de ramassage des ordures ménagères"="644A",
        "651A-Conducteurs d'engin lourd de levage"="651A",
        "651B-Conducteurs d'engin lourd de manœuvre"="651B",
        "652A-Ouvriers qualifiés de la manutention, conducteurs de chariots élévateurs, caristes"="652A",
        "652B-Dockers"="652B",
        "653A-Magasiniers qualifiés"="653A",
        "654A-Conducteurs qualifiés d'engins de transport guidés"="654A",
        "655A-Autres agents et ouvriers qualifiés (sédentaires) des services d'exploitation des transports"="655A",
        "656A-Matelots de la marine marchande, capitaines et matelots timoniers de la navigation fluviale (salariés)"="656A",
        "671A-Ouvriers non qualifiés des travaux publics de l'État et des collectivités locales"="671A",
        "671B-Ouvriers non qualifiés des travaux publics, du travail du béton et de l'extraction, hors État et collectivités locales"="671B",
        "672A-Ouvriers non qualifiés de l'électricité et de l'électronique"="672A",
        "673A-Ouvriers de production non qualifiés travaillant par enlèvement de métal"="673A",
        "673B-Ouvriers de production non qualifiés travaillant par formage de métal"="673B",
        "673C-Ouvriers non qualifiés de montage, contrôle en mécanique et travail des métaux"="673C",
        "674A-Ouvriers de production non qualifiés : chimie, pharmacie, plasturgie"="674A",
        "674B-Ouvriers de production non qualifiés de la transformation des viandes"="674B",
        "674C-Autres ouvriers de production non qualifiés : industrie agro-alimentaire"="674C",
        "674D-Ouvriers de production non qualifiés : métallurgie, production verrière, céramique, matériaux de construction"="674D",
        "674E-Ouvriers de production non qualifiés : industrie lourde du bois, fabrication des papiers et cartons"="674E",
        "675A-Ouvriers de production non qualifiés du textile et de la confection, de la tannerie-mégisserie et du travail du cuir"="675A",
        "675B-Ouvriers de production non qualifiés du travail du bois et de l'ameublement"="675B",
        "675C-Ouvriers de production non qualifiés de l'imprimerie, presse, édition"="675C",
        "676A-Manutentionnaires non qualifiés"="676A",
        "676B-Déménageurs (hors chauffeurs-déménageurs), non qualifiés"="676B",
        "676C-Ouvriers du tri, de l'emballage, de l'expédition, non qualifiés"="676C",
        "676D-Agents non qualifiés des services d'exploitation des transports"="676D",
        "676E-Ouvriers non qualifiés divers de type industriel"="676E",
        "681A-Ouvriers non qualifiés du gros œuvre du bâtiment"="681A",
        "681B-Ouvriers non qualifiés du second œuvre du bâtiment"="681B",
        "682A-Métalliers, serruriers, réparateurs en mécanique non qualifiés"="682A",
        "683A-Apprentis boulangers, bouchers, charcutiers"="683A",
        "684A-Nettoyeurs"="684A",
        "684B-Ouvriers non qualifiés de l'assainissement et du traitement des déchets"="684B",
        "685A-Ouvriers non qualifiés divers de type artisanal"="685A",
        "691A-Conducteurs d'engin agricole ou forestier"="691A",
        "691B-Ouvriers de l'élevage"="691B",
        "691C-Ouvriers du maraîchage ou de l'horticulture"="691C",
        "691D-Ouvriers de la viticulture ou de l'arboriculture fruitière"="691D",
        "691E-Ouvriers agricoles sans spécialisation particulière"="691E",
        "691F-Ouvriers de l'exploitation forestière ou de la sylviculture"="691F",
        "692A-Marins-pêcheurs et ouvriers de l'aquaculture"="692A",
        "7100-Anciens agriculteurs exploitants"="7100",
        "7200-Anciens artisans, commerçants, chefs d'entreprise"="7200",
        "7400-Anciens cadres"="7400",
        "7500-Anciennes professions intermédiaires"="7500",
        "7700-Anciens employés"="7700",
        "7800-Anciens ouvriers"="7800",
        "8100-Chômeurs n'ayant jamais travaillé"="8100",
        "8300-Militaires du contingent"="8300",
        "8400-Élèves, étudiants"="8400",
        "8500-Personnes diverses sans activité professionnelle de moins de 60 ans (sauf retraites)"="8500",
        "8600-Personnes diverses sans activité professionnelle de 60 ans et plus (sauf retraites)"="8600",
        
        #Catégories seulement disponibles avec PCS-ESE
        "100X-Agriculteurs et éleveurs, salariés de leur exploitation"                                                                              = "100X", 
        "210X-Artisans salariés de leur entreprise"                                                                                                 = "210X", 
        "220X-Commerçants et assimilés, salariés de leur entreprise"                                                                                = "220X", 
        "342B-Professeurs et maîtres de conférences"                                                                                                = "342B", 
        "342C-Professeurs agrégés et certifiés en fonction dans l'enseignement supérieur"                                                           = "342C", 
        "342D-Personnel enseignant temporaire de l'enseignement supérieur"                                                                          = "342D", 
        "342F-Directeurs et chargés de recherche de la recherche publique"                                                                          = "342F", 
        "342G-Ingénieurs d'étude et de recherche de la recherche publique"                                                                          = "342G", 
        "342H-Allocataires de la recherche publique"                                                                                                = "342H", 
        "381B-Ingénieurs et cadres d'étude et développement de l'agriculture, la pêche, les eaux et forêts"                                         = "381B", 
        "381C-Ingénieurs et cadres de production et d'exploitation de l'agriculture, la pêche, les eaux et forêts"                                  = "381C", 
        "386B-Ingénieurs et cadres d'étude, recherche et développement de la distribution d'énergie, eau"                                           = "386B", 
        "386C-Ingénieurs et cadres d'étude, recherche et développement des autres industries (imprimerie, matériaux souples, ameublement et bois)"  = "386C", 
        "451G-Professions intermédiaires administratives des collectivités locales"                                                                 = "451G", 
        "451H-Professions intermédiaires administratives des hôpitaux"                                                                              = "451H", 
        "461B-Secrétaires de direction, assistants de direction (non cadres)"                                                                       = "461B", 
        "461C-Secrétaires de niveau supérieur (non cadres, hors secrétaires de direction)"                                                          = "461C", 
        "486B-Agents de maîtrise en maintenance, installation en électricité et électronique"                                                       = "486B", 
        "486C-Agents de maîtrise en maintenance, installation en électromécanique"                                                                  = "486C", 
        "523B-Adjoints administratifs de l'Etat et assimilés (sauf Poste, France Télécom)"                                                          = "523B", 
        "523C-Adjoints administratifs des collectivités locales"                                                                                    = "523C", 
        "523D-Adjoints administratifs des hôpitaux publics"                                                                                         = "523D", 
        "524B-Agents administratifs de l'Etat et assimilés (sauf Poste, France Télécom)"                                                            = "524B", 
        "524C-Agents administratifs des collectivités locales"                                                                                      = "524C", 
        "524D-Agents administratifs des hôpitaux publics"                                                                                           = "524D", 
        "541B-Agents d'accueil qualifiés, hôtesses d'accueil et d'information"                                                                      = "541B", 
        "541C-Agents d'accueil non qualifiés"                                                                                                       = "541C", 
        "543B-Employés qualifiés des services comptables ou financiers"                                                                             = "543B", 
        "543C-Employés non qualifiés des services comptables ou financiers"                                                                         = "543C", 
        "543E-Employés qualifiés des services du personnel et des services juridiques"                                                              = "543E", 
        "543F-Employés qualifiés des services commerciaux des entreprises (hors vente)"                                                             = "543F", 
        "543G-Employés administratifs qualifiés des autres services des entreprises"                                                                = "543G", 
        "543H-Employés administratifs non qualifiés"                                                                                                = "543H", 
        "553B-Vendeurs polyvalents des grands magasins"                                                                                             = "553B", 
        "553C-Autres vendeurs non spécialisés"                                                                                                      = "553C", 
        "561B-Serveurs, commis de restaurant, garçons qualifiés"                                                                                    = "561B", 
        "561C-Serveurs, commis de restaurant, garçons non qualifiés"                                                                                = "561C", 
        "622C-Monteurs câbleurs qualifiés en électricité"                                                                                           = "622C", 
        "622E-Autres monteurs câbleurs en électronique"                                                                                             = "622E", 
        "623E-Soudeurs manuels"                                                                                                                     = "623E", 
        "624B-Monteurs, metteurs au point très qualifiés d'ensembles mécaniques travaillant à l'unité ou en petite série"                           = "624B", 
        "624C-Monteurs qualifiés d'ensembles mécaniques travaillant en moyenne ou en grande série"                                                  = "624C", 
        "625F-Autres opérateurs travaillant sur installations ou machines : industrie agroalimentaire (hors transformation des viandes)"            = "625F", 
        "625G-Autres ouvriers de production qualifiés ne travaillant pas sur machine : industrie agroalimentaire (hors transformation des viandes)" = "625G", 
        "654B-Conducteurs qualifiés d'engins de transport guidés (sauf remontées mécaniques)"                                                       = "654B", 
        "654C-Conducteurs qualifiés de systèmes de remontées mécaniques"                                                                            = "654C", 
        "656B-Matelots de la marine marchande"                                                                                                      = "656B", 
        "656C-Capitaines et matelots timoniers de la navigation fluviale"                                                                           = "656C", 
        "671C-Ouvriers non qualifiés des travaux publics et du travail du béton"                                                                    = "671C" 
      ))
  }
  
  .data
}


EPN_recode <- function(var) {
  fct_recode(
    var, 
    "003-ACADEMIES HORS INSTITUT.FR"               ="003",
    "004-ACOSS"                                    ="004",
    "006-AGENCES DE L’EAU"                         ="006",
    "007-AG.NAT.AMELIORAT.COND.TRAV"               ="007",
    "008-AG.NAT.AMELIORATION HABITAT"              ="008",
    "009-AGENCE NAT. POUR L’EMPLOI"                ="009",
    "010-AG.NAT.INDEMNISAT.FRANC.O.M"              ="010",
    "015-BIBLIOTHEQ.NATION.DE FRANCE"              ="015",
    "016-BIBLIOTHEQ.NATION.STRASBOURG"             ="016",
    "020-CAISSE DES DEPOTS ET CONSIGN"             ="020",
    "021-CAIS.NAT. ALLOCATIONS FAMIL"              ="021",
    "022-CAISSE NATIONALE ASSURANCE MALADIE"       ="022",
    "023-CAISSE NATIONALE ASSURANCE VIEILLESSE"    ="023",
    "027-CAIS.NAT.MILIT.SECU.SOCIALE"              ="027",
    "028-CENTRE MONUMENTS NATIONAUX"               ="028",
    "031-CENTRE D’ETUDES SUP. DE S.S"              ="031",
    "033-CENTR.N.ART CULTUR.POMPIDOU"              ="033",
    "035-CENTR.N.CINEMATOGRAPH.FRANC"              ="035",
    "036-CENTR.N.DOCUMENTATION PEDAG"              ="036",
    "038-INST.RECH.SCIENCES TECHNO. ENVIR.AGRI."   ="038",
    "041-CENTRE NATIONAL DU LIVRE"                 ="041",
    "042-CENTR.N.OEUVR.UNIV.SCOLAIRE"              ="042",
    "043-CENTR.N.OPHTALM.QUINZ-VINGT"              ="043",
    "045-CENTR.NAT.RECHERCH.SCIENTIF"              ="045",
    "046-CENTR.REG.EDUC.POP ET SPORTS"             ="046",
    "048-CENTR.REG.OEUV.UNIV.SCOLAIR"              ="048",
    "049-CENTR.REG. PROPRIETE FOREST"              ="049",
    "050-CENT.LIAISON SS EUROP,INTER"              ="050",
    "052-CHANCELLERIES D’UNIVERSITES"              ="052",
    "053-COLLEGE DE FRANCE"                        ="053",
    "055-AUTORITE MARCHES FINANCIERS"              ="055",
    "056-CONSEIL SUPERIEUR PECHE"                  ="056",
    "057-CONSERVAT. ESPACE LITTORAL"               ="057",
    "058-CONSERVAT.NAT. ARTS METIERS"              ="058",
    "059-CONSERV.NAT MUSIQ,ART DRAMA"              ="059",
    "061-GDS ETABLISMTS A L’ETRANGER"              ="061",
    "062-ECOLE NAT. D’ADMINISTRATION"              ="062",
    "063-ECOLE NAT. AVIATION CIVILE"               ="063",
    "064-ECOLE NAT. DES CHARTES"                   ="064",
    "065-ECOLE NAT. D’EQUITATION"                  ="065",
    "069-EC.NAT.INGENIEURS(MIN EDUC)"              ="069",
    "070-EC.NAT.INGENIEURS(MIN AGRI)"              ="070",
    "071-ECOLE NAT. MAGISTRATURE"                  ="071",
    "072-ECOLES NAT.MARINE MARCHANDE"              ="072",
    "073-ECOLE HAUTES ETUDES EN SANTE PUBLIQUE"    ="073",
    "074-ECOLE NAT. SKI ALPINISME"                 ="074",
    "075-CENTRE INTER.ETUDES SUP. SCIENCES AGRO."  ="075",
    "076-ECOLE NAT. SUP. ARTS DECO"                ="076",
    "077-ECOLE NAT. SUP. BEAUX-ARTS"               ="077",
    "078-ECOLE NAT. SUP. BIBLIOTHEQ"               ="078",
    "079-ECOLE NAT. SUP. PAYSAGE"                  ="079",
    "080-ECOLES NATIONAL. VETERINAIR"              ="080",
    "081-ECOLE NATIONALE DE VOILE"                 ="081",
    "082-ECOLES NORMALES SUPERIEURES"              ="082",
    "083-ECOLE POLYTECHNIQUE"                      ="083",
    "087-ETABL.NAT. INVALIDES MARINE"              ="087",
    "088-FONDATIONS CARNEGIE,SINGER"               ="088",
    "089-ASCE AGENC.NAT.COHES.SOC.EGALITE"         ="089",
    "096-INSTITUT DE FRANCE"                       ="096",
    "097-INSTITUT GEOGRAPHIQUE NAT"                ="097",
    "100-I.NAT.APPELLATIONS D’ORIGIN"              ="100",
    "102-INST.NAT.JEUN ET EDUC.POPUL"              ="102",
    "103-INSTIT.NAT.ETUDES DEMOGRAPH"              ="103",
    "104-I.NAT.JEUNES SOURDS,J.AVEUG"              ="104",
    "106-INSTITUTS NAT.POLYTECHNIQUE"              ="106",
    "108-INSTITUT NAT. PROPRIETE INDUSTRIELLE"     ="108",
    "109-INSTIT.NAT.RECHERCH.AGRONOM"              ="109",
    "111-CENTRE INTER ETUDES PEDAGOGIQUES (CIEP)"  ="111",
    "112-INST.NAT.SANTE RECH.MEDICAL"              ="112",
    "113-INSTIT.NAT.SCIENCES APPLIQU"              ="113",
    "114-INSTITUT NATIONAL DU SPORT"               ="114",
    "115-INSTIT.RECH. INFO. ET AUTOMA"             ="115",
    "117-INSTIT.REG.D’ADMINISTRATION"              ="117",
    "119-LYCEES AGRICOLES ET L.E.P.A"              ="119",
    "120-MUSEE DE L’ARMEE"                         ="120",
    "121-MUSEES NATIONAUX (CULTURE)"               ="121",
    "122-MUSEE DE LA MARINE"                       ="122",
    "125-MUSEUM NAT.HISTOIRE NATUREL"              ="125",
    "126-OBSERVATOIRE DE PARIS"                    ="126",
    "128-OFF.FR.PROTEC.REFUGIES,APAT"              ="128",
    "129-OFFICE NAT. ANCIENS COMBATT"              ="129",
    "130-OFFICE NATIONAL DE LA CHASS"              ="130",
    "132-OFFICE NATIONAL DES FORETS"               ="132",
    "134-OFF.NAT.INFORM.ENSEIGN.PROF"              ="134",
    "140-IRP INST.RECHECH.DEVELOP"                 ="140",
    "143-PARCS NATIONAUX"                          ="143",
    "150-UNIVERSITES"                              ="150",
    "154-INST.N.LANGUES-CIVIL.ORIENT"              ="154",
    "155-ECOL.PRATIQUE H.ETUD, EHESS"              ="155",
    "157-CENT.N.ENSEIGNEM.A DISTANCE"              ="157",
    "160-ECOLES D ARCHITECTURE"                    ="160",
    "167-FONDS FINANCEMENT CMU"                    ="167",
    "174-INST.FORM.PERS.MIN.AGRICULT"              ="174",
    "175-CENTRE NAT. ARTS PLASTIQUES"              ="175",
    "176-CENT.N.ETUD.AGRON.REG.CHAUD"              ="176",
    "177-ECOLE NAT.FORMATION AGRONOM"              ="177",
    "180-FONDS DE SOLIDARITE"                      ="180",
    "181-FONDS N.ORGA.PROF.ARTISANAT"              ="181",
    "182-ETAB.HOSPITAL.PUBL. FRESNES"              ="182",
    "197-CENT.ETUD.REC.EMPL-QUALIFIC"              ="197",
    "198-CENTRE ETUDES DE L EMPLOI"                ="198",
    "199-ECOLE NAT. SUP. DE POLICE"                ="199",
    "250-ETAB.ENSEIG.2DEGRE EDUC.NAT"              ="250",
    "251-AGENC.ENSEIG.FRANC.ETRANGER"              ="251",
    "252-INSTITUT NAT. DU PATRIMOINE"              ="252",
    "254-LYC.ENS.MARITIME ET AQUACOLE"             ="254",
    "255-INSTITUT ETUDES POLITIQUES"               ="255",
    "258-INSTITUTION NAT.INVALIDES"                ="258",
    "261-E.N.S. DES MINES"                         ="261",
    "262-E.N.TECHNIQ.INDUSTRIE-MINES"              ="262",
    "263-MUSEE DU LOUVRE"                          ="263",
    "264-METEO-FRANCE"                             ="264",
    "265-AG.FR.SECU.SAN.PRODUI.SANTE"              ="265",
    "266-ECOLE DES PONTS PARIS TECH"               ="266",
    "271-MUSEE DE L AIR"                           ="271",
    "275-AG.BIBLIO,CENT.LIVR.ENS.SUP"              ="275",
    "276-CAIS.AMORTISS.DETTE SOCIALE"              ="276",
    "278-ECOL.NAT.SUP.INGEN. MIN.DEF"              ="278",
    "279-AGENCE DE LA BIOMEDECINE"                 ="279",
    "280-MUSEE ET DOMAINE VERSAILLES"              ="280",
    "281-ECOLE NAT SUP OFFICIERS POMPIERS"         ="281",
    "283-HAUTE AUTORITE DE SANTE"                  ="283",
    "284-AGENCE NAT.DES FREQUENCES"                ="284",
    "286-EP.MAITRIS.OUVRAG.TRAV.CULT"              ="286",
    "287-GRPE DES ECOLES DES TELECOM"              ="287",
    "288-INST.N.HTES ETUD.DEFENS.NAT"              ="288",
    "289-LABO.CENTR.PONTS-CHAUSSEES"               ="289",
    "290-MASSE DES DOUANES"                        ="290",
    "292-ETABLISSEMENT FRANCAIS DU SANG"           ="292",
    "293-INSTIT. DE VEILLE SANITAIRE"              ="293",
    "294-ENS METIERS IMAGE-SON,PHOTO"              ="294",
    "295-MUSEE DU QUAI BRANLY"                     ="295",
    "298-INST.GESTION SOCIALE ARMEES"              ="298",
    "299-INST.RECHERCHES ST-LOUIS"                 ="299",
    "300-GIP ENSEIGNEMT SUP-RECHERCH"              ="300",
    "301-GIP SANTE, RECH.MEDICALE"                 ="301",
    "302-GIP ACTION SOCIALE"                       ="302",
    "303-GIP EMPLOI, FORM.PROF"                    ="303",
    "304-GIP ECONOMIE, INDUSTRIE"                  ="304",
    "305-GIP AGRICULTURE"                          ="305",
    "306-GIP CULTURE, COMMUNICATION"               ="306",
    "307-GIP SPORTS,LOISIRS"                       ="307",
    "308-GIP EQUIPEMENT-LOGEMENT"                  ="308",
    "309-GIP ENVIRONNEMENT"                        ="309",
    "310-GIP JUSTICE(ACCES AU DROIT)"              ="310",
    "312-GIP RELATIONS EXTERIEURES"                ="312",
    "313-GIP DIVERS"                               ="313",
    "314-AG.MAITRIS.OUVR.TRAV.JUSTIC"              ="314",
    "315-AG.MISE EN VALEUR ZON.50PAS"              ="315",
    "316-AG.SECURIT.SANIT.ENVIRONNEMENT"           ="316",
    "317-AG.TECHN.INFORMATION HOSPIT"              ="317",
    "318-CAIS.GARANT.LOG.LOCATIF SOC"              ="318",
    "320-ECOL.NAT.ADMINISTR.PENITENT"              ="320",
    "321-ECOL.NAT.SUP.D ARTS"                      ="321",
    "322-ET.COMMUN.PROD.AUDIOV.DEFEN"              ="322",
    "324-CAIS.NAT.SOLIDARITE AUTONOMIE"            ="324",
    "325-INST.NAT.HISTOIRE DE L ART"               ="325",
    "326-INST.NAT.PREV.EDUCAT.SANTE"               ="326",
    "327-INST.NAT RECH.ARCHEOLOG."                 ="327",
    "328-OF.NAT.INDEMNIS.ACCID.MEDIC"              ="328",
    "330-AG.FINANC.INFRASTRUCT.TRANS"              ="330",
    "331-AG.N.GARANTI.DROITS MINEURS"              ="331",
    "338-FONDS POUR LES RETRAITES"                 ="338",
    "341-AG.NAT.SERVICES@LA PERSONNE"              ="341",
    "343-ETAB.PUB.INSERTION DEFENSE"               ="343",
    "346-CENTR.NAT.DEVELOPPEMENENT SPORT"          ="346",
    "347-AGENCE NATIONALE RECHERCHE"               ="347",
    "348-AGENCE FRANCAISE LUTTTE CONTRE LE DOPAGE" ="348",
    "349-EP CITE N. HIST IMMIGRATION"              ="349",
    "350-EP DE SECURITE FERROVIAIRE"               ="350",
    "352-INSTIT NAT POLICE SCIENTIF"               ="352",
    "353-I.N.SUP.FORM.RECH.HANDICAP"               ="353",
    "356-ECOLE NATIONALE DES TPE"                  ="356",
    "357-AG.NAT.TITRES SECURISES"                  ="357",
    "358-C.N.GEST.PRAT.HOSP & DIR.HOP"             ="358",
    "359-INST.HAUT.ETUD.SCIENT-TECHN"              ="359",
    "360-SERV.HYDROL-OCEANOGR.MARINE"              ="360",
    "361-AGENCE NAT EVALUATION SERVICES SOCIAUX"   ="361",
    "362-POLE RECHERCHE - ENSEIGNEMENT SUPERIEUR"  ="362",
    "364-EXPLOITATION LIVRE FONCIER"               ="364",
    "365-MEDIATEUR NATIONAL ENERGIE"               ="365",
    "367-HAUT CONSEIL DU COMMISARIAT AUX COMPTES"  ="367",
    "400-CERCLES ET FOYERS DES ARMEES"             ="400",
    "401-FRANCE AGRIMER"                           ="401",
    "402-AGENCE DE SERVICES ET DE PAIEMENT ASP"    ="402",
    "403-OFFICE FRANCAIS IMMIGRATION INTEGRA OFII" ="403",
    "404-ETABL RETRAITE ADDIT FONCT PUBLIQU ERAFP" ="404",
    "405-ACADEMIE DES TECHNOLOGIES"                ="405",
    "406-CENTRE INFORMAT NAT ENSEIGN SUP CINES"    ="406",
    "407-AGRO CAMPUS OUEST"                        ="407",
    "408-CERCLES DE GENDARMERIE"                   ="408",
    "409-AGENCE REGIONALE DE SANTE"                ="409",
    "410-HADOPI-AUTOR DIFF PROTEC DROITS INTERNET" ="410",
    "411-AUTORITE REGULAT ACTIVITES FERROVIAIRES"  ="411",
    "413-CHANCELLERIES LEGION HONNEUR-LIBERATION"  ="413",
    "414-PORTS AUTONOMES"                          ="414",
    "415-ASSOCIATION FONCIERE DE REMEMBREMENT"     ="415",
    "416-ETAB. PUB. DES CULTES D’ALSACE-LORRAINE"  ="416",
    "417-ORGANISMES CONSULAIRES"                   ="417",
    "418-GRPT DE COOPERATION SANITAIRE"            ="418",
    "419-ASSOCIATION SYNDICALE AUTORISEE"          ="419",
    "420-COMITE DE PROTECTION DES PERSONNES"       ="420",
    "421-GRP EUROPEEN DE COOPERATION TERRITORIALE" ="421",
    "422-ASSOCIATION FONCIERE URBAINE"             ="422",
    "423-AUTORITE DE REGUL. DISTRIB. PRESSE"       ="423",
    "424-AGRASC"                                   ="424",
    "425-VOIES NAVIGABLES DE FRANCE"               ="425",
    "426-CENT NAT DES ACTIVITES PRIVEES SECURITE"  ="426",
    "427-CEREMA"                                   ="427",
    "428-ENSAPC"                                   ="428",
    "429-ANCOLS"                                   ="429",
    "430-AGENCE NATIONALE DE SANTE PUBLIQUE"       ="430",
    "431-AGENCE DE L’OUTRE-MER POUR LA MOBILITE"   ="431",
    "432-SPIC DE STATIONNEMENT"                    ="432",
  )
}
# siasp16 %>% tab(EMP_EPN) %>% arrange(-n) %>% print(n = 500)

#Piste de recherche :
# - Il y a les effectifs des écoles, et les élèves-stagiaires semblent parfois 
# comptés dedans. Donc, en retraçant les parcours, si on distingue un passage 
# dans les écoles dans la bonne CSP/le bon corps, on sait que la personne est 
# passée par là.




# EMP_MIN_recode <- function(var) {
#   fct_recode(var,
#              '01-Affaires étrangères' = '01',
#              
#              '02-Culture' = '02',
#              
#              '03-Agriculture' = '03',
#              
#              '06-Éducation nationale ESR' = '06',
#              '06-Éducation nationale ESR' = '38',
#              
#              '07-Économie et finances' = '07',
#              '07-Économie et finances' = '21',
#              '07-Économie et finances' = '57',
#              '07-Économie et finances' = '58', #Fonction publique
#              
#              '09-Intérieur' = '09',
#              '09-Intérieur' = '59', #intégration
#              '09-Intérieur' = '44', #Outre-Mer
#              '09-Intérieur' = '45', #Cohésion des territoires
#              
#              
#              '10-Justice' = '10',
#              
#              '12-Premier ministre' = '12',
#              
#              '23-Écologie et territoire' = '23',
#              '23-Écologie et territoire' = '39', #Ville
#              
#              '36-Travail' = '36',
#              
#              '56-Santé' = '56',
#              '56-Santé' = '35',
#              '56-Santé' = '52', #Ville, jeunesse et sports
#              
#              '70-Défense' = '70'
#   ) #|>
#   #fct_relevel(sort)
# }





NH_code <- function(var) {
  
  if (!is.factor(var)) var <- as.factor(var)
  
  recode_levels <-  c("A0-Emploi de direction"            = "10", # 10	A-Dir	Emplois de direction (1) 
                      "A1-Niveau administrateur"          = "11", # 11	A-I	Emplois de niveau administrateur
                      "A2-Niveau attaché principal"       = "12", # 12	A-II	Emplois de niveau attaché (AC) ou inspecteur (SD) principal (2)
                      "A3-Niveau attaché"                 = "13", # 13	A-III	Emplois de niveau attaché (AC) ou inspecteur (SD)
                      "A4-Emploi à carrière courte"       = "14", # 14	A-IV	Emplois à carrière courte (3)
                      "A9-Niveau A autres"                = "19", # 19	A-Sai	Emplois niveau SAI et officiers (4)
                      "B1-Niveau secrétaire CE"           = "21", # 21	B-I	Emplois niveau secrétaire administratif de classe exceptionnelle ou contrôleur principal ; instituteur  (5)
                      "B2-Niveau secrétaire"              = "22", # 22	B-II	Emplois niveau secrétaire administratif ou contrôleur (6)
                      "B9-Niveau B autres"                = "29", # 29	B-Sai	Emplois niveau SAI et sous-officiers
                      "C1-Niveau adjoint principal 1C E6" = "31", # 31	C	Emplois supérieurs (nouvel espace indiciaire ou non standard)
                      "C2-Niveau adjoint principal 2C E5" = "32", # 32	C	Echelle 5
                      "C3-Niveau adjoint 1C E4"           = "33", # 33	C	Echelle 4
                      "C4-Niveau adjoint 2C E3"           = "34", # 34	C	Echelle 3
                      "C5-Assistante maternelle ou E2"    = "35", # 35	C	Echelle 2
                      "C6-C E1"                           = "36", # 36	C	à partir de 1998 : Echelle 1 et anciens emplois de catégorie D  (7)
                      "C9-Niveau C autres"                = "39", # 39	C	SAI et hommes du rang
                      "D2-Niveau D"                       = "42", # 42	D 	avant 1998 : Echelle 1 et tous autres emplois de niveau D  (7)
                      "ZZ-Niveau indéterminé"             = "99" # 99	Indéterminé     )
  )
  
  recode_levels <- recode_levels[recode_levels %in% levels(var)]
  
  if (length(recode_levels) != 0) {
    fct_recode(var, !!!recode_levels)
  } else {
    warning("no corresponding level to recode")
    var
  }
}


NH_recode <- function(var) {
  fct_recode(
    var,
    "A0-Emploi de direction"            = "A0-Emploi de direction"           ,
    "A1-Niveau administrateur"          = "A1-Niveau administrateur"         ,
    "A2-Niveau attaché"                 = "A2-Niveau attaché principal"      ,
    "A2-Niveau attaché"                 = "A3-Niveau attaché"                ,
    "A4-Emploi à carrière courte"       = "A4-Emploi à carrière courte"      ,
    "A9-Niveau A autres"                = "A9-Niveau A autres"               ,
    "B1-Niveau B"                       = "B1-Niveau secrétaire CE"          ,
    "B1-Niveau B"                       = "B2-Niveau secrétaire"             ,
    "B1-Niveau B"                       = "B9-Niveau B autres"               ,
    "C1-Niveau adjoint"                 = "C1-Niveau adjoint principal 1C E6",
    "C1-Niveau adjoint"                 = "C2-Niveau adjoint principal 2C E5",
    "C1-Niveau adjoint"                 = "C3-Niveau adjoint 1C E4"          ,
    "C1-Niveau adjoint"                 = "C4-Niveau adjoint 2C E3"          ,
    "C5-Assistante maternelle ou E2"    = "C5-Assistante maternelle ou E2"   ,
    "C9-Niveau C autres"                = "C9-Niveau C autres"               ,
    "ZZ-Niveau indéterminé"             = "ZZ-Niveau indéterminé"            
  )
}






















## Catégorie juridique SIRENE
CJ_create <- function(data, var = "CJ", res = c("CJ1", "CJ2", "CJ")) {
  
  if (!var %in% names(data)) stop(paste0("no variable named ", var, " in data"))
  
  var <- rlang::ensym(var)
  
  # if (any(!is.na(pull(data, !!sym(var))) & str_detect(pull(data, !!sym(var)), "-"))) {
  #   data <- mutate(data, var := fct_relabel(!!sym(var), ~ str_extract(., "^[^-]+")))
  # }
  
  data <- data |> 
    mutate(!!var := fct_recode(!!var, "NULL" = "NULL"))
  
  
  if ("CJ1" %in% res) {
    CJ1_recode <-
      c("1-Entrepreneur individuel" = "1",
        "2-Groupement de droit privé non doté de la personnalité morale" = "2",
        "3-Personne morale de droit étranger" = "3",
        "4-Personne morale de droit public soumise au droit commercial" = "4",
        "5-Société commerciale" = "5",
        "6-Autre personne morale immatriculée au RCS" = "6",
        "7-Personne morale et organisme soumis au droit administratif" = "7",
        "8-Organisme privé spécialisé" = "8",
        "9-Groupement de droit privé" = "9")
    
    CJ1_recode <- set_names(names(CJ1_recode), CJ1_recode)
    data <- data |> 
      mutate(!!sym(paste0(var, "1")) := 
               fct_relabel(!!sym(var), ~ CJ1_recode[str_sub(., 1, 1)] ) |>
               fct_relevel(sort))
  }
  
  if ("CJ2" %in% res) {
    CJ2_recode <-
      c("11-Artisan-commerçant" = "11",
        "12-Commerçant" = "12",
        "13-Artisan" = "13",
        "14-Officier public ou ministériel" = "14",
        "15-Profession libérale" = "15",
        "16-Exploitant agricole" = "16",
        "17-Agent commercial" = "17",
        "18-Associé gérant de Société" = "18",
        "19-(Autre) Personne physique" = "19",
        "21-Indivision" = "21",
        "22-Société créée de fait" = "22",
        "23-Société en participation" = "23",
        "24-Fiducie" = "24",
        "27-Paroisse hors zone concordataire" = "27",
        "29-Autre groupement de droit privé non doté de la personnalité morale" = "29",
        "31-Personne morale de droit étranger, immatriculée au RCS (registre du commerce et des sociétés)" = "31",
        "32-Personne morale de droit étranger, non immatriculée au RCS" = "32",
        "41-Etablissement public ou régie à caractère industriel ou commercial" = "41",
        "51-Société coopérative commerciale particulière" = "51",
        "52-Société en nom collectif" = "52",
        "53-Société en commandite" = "53",
        "54-Société à responsabilité limitée (SARL)" = "54",
        "55-Société anonyme à conseil d'administration" = "55",
        "56-Société anonyme à directoire" = "56",
        "57-Société par actions simplifiée" = "57",
        "58-Société européenne" = "58",
        "61-Caisse d'épargne et de prévoyance" = "61",
        "62-Groupement d'intérêt économique" = "62",
        "63-Société coopérative agricole" = "63",
        "64-Société d'assurance mutuelle" = "64",
        "65-Société civile" = "65",
        "69-Autre personne morale de droit privé inscrite au registre du commerce et des sociétés" = "69",
        "71-Administration de l'état" = "71",
        "72-Collectivité territoriale" = "72",
        "73-Etablissement public administratif" = "73",
        "74-Autre personne morale de droit public administratif" = "74",
        "81-Organisme gérant un régime de protection sociale à adhésion obligatoire" = "81",
        "82-Organisme mutualiste" = "82",
        "83-Comité d'entreprise" = "83",
        "84-Organisme professionnel" = "84",
        "85-Organisme de retraite à adhésion non obligatoire" = "85",
        "91-Syndicat de propriétaires" = "91",
        "92-Association loi 1901 ou assimilé" = "92",
        "93-Fondation" = "93",
        "99-Autre personne morale de droit privé" = "99")
    
    CJ2_recode <- set_names(names(CJ2_recode), CJ2_recode)
    data <- data |> 
      mutate(!!sym(paste0(var, "2")) := 
               fct_relabel(!!sym(var), ~ CJ2_recode[str_sub(., 1, 2)] ) |>
               fct_relevel(sort))
  }
  
  if ("CJ" %in% res) {
    
    CJ4_recode <-
      c("1100-Artisan-commerçant" = "1100",
        "1200-Commerçant" = "1200",
        "1300-Artisan" = "1300",
        "1400-Officier public ou ministériel" = "1400",
        "1500-Profession libérale" = "1500",
        "1600-Exploitant agricole" = "1600",
        "1700-Agent commercial" = "1700",
        "1800-Associé gérant de Société" = "1800",
        "1900-(Autre) Personne physique" = "1900",
        "2110-Indivision entre personnes physiques" = "2110",
        "2120-Indivision avec personne morale" = "2120",
        "2210-Société créée de fait entre personnes physiques" = "2210",
        "2220-Société créée de fait avec personne morale" = "2220",
        "2310-Société en participation entre personnes physiques" = "2310",
        "2320-Société en participation avec personne morale" = "2320",
        "2385-Société en participation de professions libérales" = "2385",
        "2400-Fiducie" = "2400",
        "2700-Paroisse hors zone concordataire" = "2700",
        "2900-Autre groupement de droit privé non doté de la personnalité morale" = "2900",
        "3110-Représentation ou agence commerciale d'état ou organisme public étranger immatriculé au RCS" = "3110",
        "3120-Société commerciale étrangère immatriculée au RCS" = "3120",
        "3205-Organisation internationale" = "3205",
        "3210-État, collectivité ou établissement public étranger" = "3210",
        "3220-Société étrangère non immatriculée au RCS" = "3220",
        "3290-Autre personne morale de droit étranger" = "3290",
        "4110-Établissement public national à caractère industriel ou commercial doté d'un comptable public" = "4110",
        "4120-Établissement public national à caractère industriel ou commercial non doté d'un comptable public" = "4120",
        "4130-Exploitant public" = "4130",
        "4140-Établissement public local à caractère industriel ou commercial" = "4140",
        "4150-Régie d'une collectivité locale à caractère industriel ou commercial" = "4150",
        "4160-Institution Banque de France" = "4160",
        "5191-Société de caution mutuelle" = "5191",
        "5192-Société coopérative de banque populaire" = "5192",
        "5193-Caisse de crédit maritime mutuel" = "5193",
        "5194-Caisse (fédérale) de crédit mutuel" = "5194",
        "5195-Association coopérative inscrite (droit local Alsace Moselle)" = "5195",
        "5196-Caisse d'épargne et de prévoyance à forme coopérative" = "5196",
        "5202-Société en nom collectif" = "5202",
        "5203-Société en nom collectif coopérative" = "5203",
        "5306-Société en commandite simple" = "5306",
        "5307-Société en commandite simple coopérative" = "5307",
        "5308-Société en commandite par actions" = "5308",
        "5309-Société en commandite par actions coopérative" = "5309",
        "5370-Société de Participations Financières de Profession Libérale Société en commandite par actions (SPFPL SCA)" = "5370",
        "5385-Société d'exercice libéral en commandite par actions" = "5385",
        "5410-SARL nationale" = "5410",
        "5415-SARL d'économie mixte" = "5415",
        "5422-SARL immobilière pour le commerce et l'industrie (SICOMI)" = "5422",
        "5426-SARL immobilière de gestion" = "5426",
        "5430-SARL d'aménagement foncier et d'équipement rural (SAFER)" = "5430",
        "5431-SARL mixte d'intérêt agricole (SMIA)" = "5431",
        "5432-SARL d'intérêt collectif agricole (SICA)" = "5432",
        "5442-SARL d'attribution" = "5442",
        "5443-SARL coopérative de construction" = "5443",
        "5451-SARL coopérative de consommation" = "5451",
        "5453-SARL coopérative artisanale" = "5453",
        "5454-SARL coopérative d'intérêt maritime" = "5454",
        "5455-SARL coopérative de transport" = "5455",
        "5458-SARL coopérative ouvrière de production (SCOP)" = "5458",
        "5459-SARL union de sociétés coopératives" = "5459",
        "5460-Autre SARL coopérative" = "5460",
        "5470-Société de Participations Financières de Profession Libérale Société à responsabilité limitée (SPFPL SARL)" = "5470",
        "5485-Société d'exercice libéral à responsabilité limitée" = "5485",
        "5498-SARL unipersonnelle" = "5498",
        "5499-Société à responsabilité limitée (sans autre indication)" = "5499",
        "5505-SA à participation ouvrière à conseil d'administration" = "5505",
        "5510-SA nationale à conseil d'administration" = "5510",
        "5515-SA d'économie mixte à conseil d'administration" = "5515",
        "5520-Fonds à forme sociétale à conseil d'administration" = "5520",
        "5522-SA immobilière pour le commerce et l'industrie (SICOMI) à conseil d'administration" = "5522",
        "5525-SA immobilière d'investissement à conseil d'administration" = "5525",
        "5530-SA d'aménagement foncier et d'équipement rural (SAFER) à conseil d'administration" = "5530",
        "5531-Société anonyme mixte d'intérêt agricole (SMIA) à conseil d'administration" = "5531",
        "5532-SA d'intérêt collectif agricole (SICA) à conseil d'administration" = "5532",
        "5542-SA d'attribution à conseil d'administration" = "5542",
        "5543-SA coopérative de construction à conseil d'administration" = "5543",
        "5546-SA de HLM à conseil d'administration" = "5546",
        "5547-SA coopérative de production de HLM à conseil d'administration" = "5547",
        "5548-SA de crédit immobilier à conseil d'administration" = "5548",
        "5551-SA coopérative de consommation à conseil d'administration" = "5551",
        "5552-SA coopérative de commerçants-détaillants à conseil d'administration" = "5552",
        "5553-SA coopérative artisanale à conseil d'administration" = "5553",
        "5554-SA coopérative (d'intérêt) maritime à conseil d'administration" = "5554",
        "5555-SA coopérative de transport à conseil d'administration" = "5555",
        "5558-SA coopérative ouvrière de production (SCOP) à conseil d'administration" = "5558",
        "5559-SA union de sociétés coopératives à conseil d'administration" = "5559",
        "5560-Autre SA coopérative à conseil d'administration" = "5560",
        "5470-Société de Participations Financières de Profession Libérale Société anonyme à conseil d'administration" = "5470",
        "5585-Société d'exercice libéral à forme anonyme à conseil d'administration" = "5585",
        "5599-SA à conseil d'administration (s.a.i.)" = "5599",
        "5605-SA à participation ouvrière à directoire" = "5605",
        "5610-SA nationale à directoire" = "5610",
        "5615-SA d'économie mixte à directoire" = "5615",
        "5620-Fonds à forme sociétale à directoire" = "5620",
        "5622-SA immobilière pour le commerce et l'industrie (SICOMI) à directoire" = "5622",
        "5625-SA immobilière d'investissement à directoire" = "5625",
        "5630-Safer anonyme à directoire" = "5630",
        "5631-SA mixte d'intérêt agricole (SMIA)" = "5631",
        "5632-SA d'intérêt collectif agricole (SICA)" = "5632",
        "5642-SA d'attribution à directoire" = "5642",
        "5643-SA coopérative de construction à directoire" = "5643",
        "5646-SA de HLM à directoire" = "5646",
        "5647-Société coopérative de production de HLM anonyme à directoire" = "5647",
        "5648-SA de crédit immobilier à directoire" = "5648",
        "5651-SA coopérative de consommation à directoire" = "5651",
        "5652-SA coopérative de commerçants-détaillants à directoire" = "5652",
        "5653-SA coopérative artisanale à directoire" = "5653",
        "5654-SA coopérative d'intérêt maritime à directoire" = "5654",
        "5655-SA coopérative de transport à directoire" = "5655",
        "5658-SA coopérative ouvrière de production (SCOP) à directoire" = "5658",
        "5659-SA union de sociétés coopératives à directoire" = "5659",
        "5660-Autre SA coopérative à directoire" = "5660",
        "5670-Société de Participations Financières de Profession Libérale Société anonyme à Directoire (SPFPL SA à directoire)" = "5670",
        "5685-Société d'exercice libéral à forme anonyme à directoire" = "5685",
        "5699-SA à directoire (s.a.i.)" = "5699",
        "5710-SAS, société par actions simplifiée" = "5710",
        "5720-Société par actions simplifiée à associé unique ou société par actions simplifiée unipersonnelle" = "5720",
        "5770-Société de Participations Financières de Profession Libérale Société par actions simplifiée (SPFPL SAS)" = "5770",
        "5785-Société d'exercice libéral par action simplifiée" = "5785",
        "5800-Société européenne" = "5800",
        "6100-Caisse d'Épargne et de Prévoyance" = "6100",
        "6210-Groupement européen d'intérêt économique (GEIE)" = "6210",
        "6220-Groupement d'intérêt économique (GIE)" = "6220",
        "6316-Coopérative d'utilisation de matériel agricole en commun (CUMA)" = "6316",
        "6317-Société coopérative agricole" = "6317",
        "6318-Union de sociétés coopératives agricoles" = "6318",
        "6411-Société d'assurance à forme mutuelle" = "6411",
        "6511-Sociétés Interprofessionnelles de Soins Ambulatoires " = "6511",
        "6521-Société civile de placement collectif immobilier (SCPI)" = "6521",
        "6532-Société civile d'intérêt collectif agricole (SICA)" = "6532",
        "6533-Groupement agricole d'exploitation en commun (GAEC)" = "6533",
        "6534-Groupement foncier agricole" = "6534",
        "6535-Groupement agricole foncier" = "6535",
        "6536-Groupement forestier" = "6536",
        "6537-Groupement pastoral" = "6537",
        "6538-Groupement foncier et rural" = "6538",
        "6539-Société civile foncière" = "6539",
        "6540-Société civile immobilière" = "6540",
        "6541-Société civile immobilière de construction-vente" = "6541",
        "6542-Société civile d'attribution" = "6542",
        "6543-Société civile coopérative de construction" = "6543",
        "6544-Société civile immobilière d' accession progressive à la propriété" = "6544",
        "6551-Société civile coopérative de consommation" = "6551",
        "6554-Société civile coopérative d'intérêt maritime" = "6554",
        "6558-Société civile coopérative entre médecins" = "6558",
        "6560-Autre société civile coopérative" = "6560",
        "6561-SCP d'avocats" = "6561",
        "6562-SCP d'avocats aux conseils" = "6562",
        "6563-SCP d'avoués d'appel" = "6563",
        "6564-SCP d'huissiers" = "6564",
        "6565-SCP de notaires" = "6565",
        "6566-SCP de commissaires-priseurs" = "6566",
        "6567-SCP de greffiers de tribunal de commerce" = "6567",
        "6568-SCP de conseils juridiques" = "6568",
        "6569-SCP de commissaires aux comptes" = "6569",
        "6571-SCP de médecins" = "6571",
        "6572-SCP de dentistes" = "6572",
        "6573-SCP d'infirmiers" = "6573",
        "6574-SCP de masseurs-kinésithérapeutes" = "6574",
        "6575-SCP de directeurs de laboratoire d'analyse médicale" = "6575",
        "6576-SCP de vétérinaires" = "6576",
        "6577-SCP de géomètres experts" = "6577",
        "6578-SCP d'architectes" = "6578",
        "6585-Autre société civile professionnelle" = "6585",
        "6588-Société civile laitière" = "6588",
        "6589-Société civile de moyens" = "6589",
        "6595-Caisse locale de crédit mutuel" = "6595",
        "6596-Caisse de crédit agricole mutuel" = "6596",
        "6597-Société civile d'exploitation agricole" = "6597",
        "6598-Exploitation agricole à responsabilité limitée" = "6598",
        "6599-Autre société civile" = "6599",
        "6901-Autre personne de droit privé inscrite au registre du commerce et des sociétés" = "6901",
        "7111-Autorité constitutionnelle" = "7111",
        "7112-Autorité administrative ou publique indépendante" = "7112",
        "7113-Ministère" = "7113",
        "7120-Service central d'un ministère" = "7120",
        "7150-Service du ministère de la Défense" = "7150",
        "7160-Service déconcentré à compétence nationale d'un ministère (hors Défense)" = "7160",
        "7171-Service déconcentré de l'État à compétence (inter) régionale" = "7171",
        "7172-Service déconcentré de l'État à compétence (inter) départementale" = "7172",
        "7179-(Autre) Service déconcentré de l'État à compétence territoriale" = "7179",
        "7190-Ecole nationale non dotée de la personnalité morale" = "7190",
        "7210-Commune et commune nouvelle" = "7210",
        "7220-Département" = "7220",
        "7225-Collectivité et territoire d'Outre Mer" = "7225",
        "7229-(Autre) Collectivité territoriale" = "7229",
        "7230-Région" = "7230",
        "7312-Commune associée et commune déléguée" = "7312",
        "7313-Section de commune" = "7313",
        "7314-Ensemble urbain" = "7314",
        "7321-Association syndicale autorisée" = "7321",
        "7322-Association foncière urbaine" = "7322",
        "7323-Association foncière de remembrement" = "7323",
        "7331-Établissement public local d'enseignement" = "7331",
        "7340-Pôle métropolitain" = "7340",
        "7341-Secteur de commune" = "7341",
        "7342-District urbain" = "7342",
        "7343-Communauté urbaine" = "7343",
        "7344-Métropole" = "7344",
        "7345-Syndicat intercommunal à vocation multiple (SIVOM)" = "7345",
        "7346-Communauté de communes" = "7346",
        "7347-Communauté de villes" = "7347",
        "7348-Communauté d'agglomération" = "7348",
        "7349-Autre établissement public local de coopération non spécialisé ou entente" = "7349",
        "7351-Institution interdépartementale ou entente" = "7351",
        "7352-Institution interrégionale ou entente" = "7352",
        "7353-Syndicat intercommunal à vocation unique (SIVU)" = "7353",
        "7354-Syndicat mixte fermé" = "7354",
        "7355-Syndicat mixte ouvert" = "7355",
        "7356-Commission syndicale pour la gestion des biens indivis des communes" = "7356",
        "7357-Pôle d'équilibre territorial et rural (PETR)" = "7357",
        "7361-Centre communal d'action sociale" = "7361",
        "7362-Caisse des écoles" = "7362",
        "7363-Caisse de crédit municipal" = "7363",
        "7364-Établissement d'hospitalisation" = "7364",
        "7365-Syndicat inter hospitalier" = "7365",
        "7366-Établissement public local social et médico-social" = "7366",
        "7367-Centre Intercommunal d'action sociale (CIAS)" = "7367",
        "7371-Office public d'habitation à loyer modéré (OPHLM)" = "7371",
        "7372-Service départemental d'incendie et de secours (SDIS)" = "7372",
        "7373-Établissement public local culturel" = "7373",
        "7378-Régie d'une collectivité locale à caractère administratif" = "7378",
        "7379-(Autre) Établissement public administratif local" = "7379",
        "7381-Organisme consulaire" = "7381",
        "7382-Établissement public national ayant fonction d'administration centrale" = "7382",
        "7383-Établissement public national à caractère scientifique culturel et professionnel" = "7383",
        "7384-Autre établissement public national d'enseignement" = "7384",
        "7385-Autre établissement public national administratif à compétence territoriale limitée" = "7385",
        "7389-Établissement public national à caractère administratif" = "7389",
        "7410-Groupement d'intérêt public (GIP)" = "7410",
        "7430-Établissement public des cultes d'Alsace-Lorraine" = "7430",
        "7450-Etablissement public administratif, cercle et foyer dans les armées" = "7450",
        "7470-Groupement de coopération sanitaire à gestion publique" = "7470",
        "7490-Autre personne morale de droit administratif" = "7490",
        "8110-Régime général de la Sécurité Sociale" = "8110",
        "8120-Régime spécial de Sécurité Sociale" = "8120",
        "8130-Institution de retraite complémentaire" = "8130",
        "8140-Mutualité sociale agricole" = "8140",
        "8150-Régime maladie des non-salariés non agricoles" = "8150",
        "8160-Régime vieillesse ne dépendant pas du régime général de la Sécurité Sociale" = "8160",
        "8170-Régime d'assurance chômage" = "8170",
        "8190-Autre régime de prévoyance sociale" = "8190",
        "8210-Mutuelle" = "8210",
        "8250-Assurance mutuelle agricole" = "8250",
        "8290-Autre organisme mutualiste" = "8290",
        "8310-Comité central d'entreprise" = "8310",
        "8311-Comité d'établissement" = "8311",
        "8410-Syndicat de salariés" = "8410",
        "8420-Syndicat patronal" = "8420",
        "8450-Ordre professionnel ou assimilé" = "8450",
        "8470-Centre technique industriel ou comité professionnel du développement économique" = "8470",
        "8490-Autre organisme professionnel" = "8490",
        "8510-Institution de prévoyance" = "8510",
        "8520-Institution de retraite supplémentaire" = "8520",
        "9110-Syndicat de copropriété" = "9110",
        "9150-Association syndicale libre" = "9150",
        "9210-Association non déclarée" = "9210",
        "9220-Association déclarée" = "9220",
        "9221-Association déclarée d'insertion par l'économique" = "9221",
        "9222-Association intermédiaire" = "9222",
        "9223-Groupement d'employeurs" = "9223",
        "9224-Association d'avocats à responsabilité professionnelle individuelle" = "9224",
        "9230-Association déclarée, reconnue d'utilité publique" = "9230",
        "9240-Congrégation" = "9240",
        "9260-Association de droit local (Bas-Rhin, Haut-Rhin et Moselle)" = "9260",
        "9300-Fondation" = "9300",
        "9900-Autre personne morale de droit privé" = "9900",
        "9970-Groupement de coopération sanitaire à gestion privée" = "9970")
    
    CJ4_recode <- set_names(names(CJ4_recode), CJ4_recode)
    data <- data |> 
      mutate(!!sym(var) := fct_relabel(!!sym(var), ~ CJ4_recode[str_sub(., 1, 4)] ) |>
               fct_relevel(sort))
  }
  
  data
}

point_indice <- function() {}
point_indice <- #Traitement indiciaire brut annuel, en moyenne annuelle
  #tibble::tribble(~ANNEE, ~point100,
  c(
    "1970"  = 896.06  / 100,
    "1971"  = 955.78  / 100,
    "1972"  = 1034.03 / 100,
    "1973"  = 1129.47 / 100,
    "1974"  = 1286.60 / 100,
    "1975"  = 1456.77 / 100,
    "1976"  = 1615.09 / 100,
    "1977"  = 1772.64 / 100,
    "1978"  = 1957.78 / 100,
    "1979"  = 2169.19 / 100,
    "1980"  = 2481.37 / 100,
    "1981"  = 2827.51 / 100,
    "1982"  = 3187.58 / 100,
    "1983"  = 3539.26 / 100,
    "1984"  = 3783.68 / 100,
    "1985"  = 3946.99 / 100,
    "1986"  = 4028.92 / 100,
    "1987"  = 4084.34 / 100,
    "1988"  = 4161.55 / 100,
    "1989"  = 4247.61 / 100,
    "1990"  = 4352.92 / 100,
    "1991"  = 4433.49 / 100,
    "1992"  = 4551.24 / 100,
    "1993"  = 4675.24 / 100,
    "1994"  = 4741.67 / 100,
    "1995"  = 4849.46 / 100,
    "1996"  = 4915.57 / 100,
    "1997"  = 4942.20 / 100,
    "1998"  = 4998.77 / 100,
    "1999"  = 5051.51 / 100,
    "2000"  = 5096.82 / 100,
    "2001"  = 5143.22 / 100,
    "2002"  = 5210.70 / 100,
    "2003"  = 5249.33 / 100,
    "2004"  = 5275.59 / 100,
    "2005"  = 5320.12 / 100,
    "2006"  = 5384.53 / 100,
    "2007"  = 5437.53 / 100,
    "2008"  = 5467.91 / 100,
    "2009"  = 5502.60 / 100,
    "2010"  = 5542.53 / 100,
    "2011"  = 5556.35 / 100,
    "2012"  = 5556.35 / 100,
    "2013"  = 5556.35 / 100,
    "2014"  = 5556.35 / 100,
    "2015"  = 5556.35 / 100,
    "2016"  = 5573.02 / 100,
    "2017"  = 5620.44 / 100,
    "2018"  = 5623.23 / 100,
    "2019"  = 5623.23 / 100,
    "2020"  = 5623.23 / 100
  )










NAF_recode <- function(var) {
  suppressWarnings(fct_recode(var,
                              'A-AGRICULTURE, SYLVICULTURE ET PÊCHE' = 'A',
                              
                              '01-Cult & prod animale, chasse & sce ann' = '01',
                              
                              '011-Cultures non permanentes' = '011',
                              '0111-Cult céréale, légumineuse, graine oléag' = '0111',
                              '0111Z-Cult céréale, légumineuse, graine oléag' = '0111Z',
                              '0112-Culture du riz' = '0112',
                              '0112Z-Culture du riz' = '0112Z',
                              '0113-Cult légume, melon, racine & tubercule' = '0113',
                              '0113Z-Cult légume, melon, racine & tubercule' = '0113Z',
                              '0114-Culture de la canne à sucre' = '0114',
                              '0114Z-Culture de la canne à sucre' = '0114Z',
                              '0115-Culture du tabac' = '0115',
                              '0115Z-Culture du tabac' = '0115Z',
                              '0116-Culture de plantes à fibres' = '0116',
                              '0116Z-Culture de plantes à fibres' = '0116Z',
                              '0119-Autres cultures non permanentes' = '0119',
                              '0119Z-Autres cultures non permanentes' = '0119Z',
                              
                              '012-Cultures permanentes' = '012',
                              '0121-Culture de la vigne' = '0121',
                              '0121Z-Culture de la vigne' = '0121Z',
                              '0122-Culture fruits tropicaux et subtropicaux' = '0122',
                              '0122Z-Culture fruits tropicaux et subtropicaux' = '0122Z',
                              '0123-Culture agrumes' = '0123',
                              '0123Z-Culture agrumes' = '0123Z',
                              '0124-Culture de fruits à pépins et à noyau' = '0124',
                              '0124Z-Culture de fruits à pépins et à noyau' = '0124Z',
                              '0125-Cult aut fruits & de fruits à coque' = '0125',
                              '0125Z-Cult aut fruits & de fruits à coque' = '0125Z',
                              '0126-Culture de fruits oléagineux' = '0126',
                              '0126Z-Culture de fruits oléagineux' = '0126Z',
                              '0127-Culture de plantes à boissons' = '0127',
                              '0127Z-Culture de plantes à boissons' = '0127Z',
                              '0128-Cult plante aromatiq médicin pharma' = '0128',
                              '0128Z-Cult plante aromatiq médicin pharma' = '0128Z',
                              '0129-Autres cultures permanentes' = '0129',
                              '0129Z-Autres cultures permanentes' = '0129Z',
                              
                              '013-Reproduction de plantes' = '013',
                              '0130-Reproduction de plantes' = '0130',
                              '0130Z-Reproduction de plantes' = '0130Z',
                              
                              '014-Production animale' = '014',
                              '0141-Élevage de vaches laitières' = '0141',
                              '0141Z-Élevage de vaches laitières' = '0141Z',
                              '0142-Élevage autres bovins et de buffles' = '0142',
                              '0142Z-Élevage autres bovins et de buffles' = '0142Z',
                              '0143-Élevage de chevaux et autres équidés' = '0143',
                              '0143Z-Élevage de chevaux et autres équidés' = '0143Z',
                              '0144-Élevage de chameaux & autres camélidés' = '0144',
                              '0144Z-Élevage de chameaux & autres camélidés' = '0144Z',
                              '0145-Élevage ovins et de caprins' = '0145',
                              '0145Z-Élevage ovins et de caprins' = '0145Z',
                              '0146-Élevage de porcins' = '0146',
                              '0146Z-Élevage de porcins' = '0146Z',
                              '0147-Élevage de volailles' = '0147',
                              '0147Z-Élevage de volailles' = '0147Z',
                              '0149-Élevage autres animaux' = '0149',
                              '0149Z-Élevage autres animaux' = '0149Z',
                              
                              '015-Culture et élevage associés' = '015',
                              '0150-Culture et élevage associés' = '0150',
                              '0150Z-Culture et élevage associés' = '0150Z',
                              
                              '016-Act soutien agr & trait prim récolt' = '016',
                              '0161-Activités de soutien aux cultures' = '0161',
                              '0161Z-Activités de soutien aux cultures' = '0161Z',
                              '0162-Activités de soutien à la prod animale' = '0162',
                              '0162Z-Activités de soutien à la prod animale' = '0162Z',
                              '0163-Traitement primaire des récoltes' = '0163',
                              '0163Z-Traitement primaire des récoltes' = '0163Z',
                              '0164-Traitement des semences' = '0164',
                              '0164Z-Traitement des semences' = '0164Z',
                              
                              '017-Chasse, piégeage et services annexes' = '017',
                              '0170-Chasse, piégeage et services annexes' = '0170',
                              '0170Z-Chasse, piégeage et services annexes' = '0170Z',
                              
                              '02-Sylviculture et exploitation forestière' = '02',
                              
                              '021-Sylviculture & autres act forestières' = '021',
                              '0210-Sylviculture & autres act forestières' = '0210',
                              '0210Z-Sylviculture & autres act forestières' = '0210Z',
                              
                              '022-Exploitation forestière' = '022',
                              '0220-Exploitation forestière' = '0220',
                              '0220Z-Exploitation forestière' = '0220Z',
                              
                              '023-Récolte prodts forestiers non ligneux' = '023',
                              '0230-Récolte prodts forestiers non ligneux' = '0230',
                              '0230Z-Récolte prodts forestiers non ligneux' = '0230Z',
                              
                              '024-Services de soutien à expl forestière' = '024',
                              '0240-Services de soutien à expl forestière' = '0240',
                              '0240Z-Services de soutien à expl forestière' = '0240Z',
                              
                              '03-Pêche et aquaculture' = '03',
                              
                              '031-Pêche' = '031',
                              '0311-Pêche en mer' = '0311',
                              '0311Z-Pêche en mer' = '0311Z',
                              '0312-Pêche en eau douce' = '0312',
                              '0312Z-Pêche en eau douce' = '0312Z',
                              
                              '032-Aquaculture' = '032',
                              '0321-Aquaculture en mer' = '0321',
                              '0321Z-Aquaculture en mer' = '0321Z',
                              '0322-Aquaculture en eau douce' = '0322',
                              '0322Z-Aquaculture en eau douce' = '0322Z',
                              
                              'B-INDUSTRIES EXTRACTIVES' = 'B',
                              
                              '05-Extraction de houille et de lignite' = '05',
                              
                              '051-Extraction de houille' = '051',
                              '0510-Extraction de houille' = '0510',
                              '0510Z-Extraction de houille' = '0510Z',
                              
                              '052-Extraction de lignite' = '052',
                              '0520-Extraction de lignite' = '0520',
                              '0520Z-Extraction de lignite' = '0520Z',
                              
                              '06-Extraction hydrocarbures' = '06',
                              
                              '061-Extraction de pétrole brut' = '061',
                              '0610-Extraction de pétrole brut' = '0610',
                              '0610Z-Extraction de pétrole brut' = '0610Z',
                              
                              '062-Extraction de gaz naturel' = '062',
                              '0620-Extraction de gaz naturel' = '0620',
                              '0620Z-Extraction de gaz naturel' = '0620Z',
                              
                              '07-Extraction de minerais métalliques' = '07',
                              
                              '071-Extraction de minerais de fer' = '071',
                              '0710-Extraction de minerais de fer' = '0710',
                              '0710Z-Extraction de minerais de fer' = '0710Z',
                              
                              '072-Extr de minerais de métaux non ferreux' = '072',
                              '0721-Extr de minerais uranium & de thorium' = '0721',
                              '0721Z-Extr de minerais uranium & de thorium' = '0721Z',
                              '0729-Extr aut minerai de métaux non ferreux' = '0729',
                              '0729Z-Extr aut minerai de métaux non ferreux' = '0729Z',
                              
                              '08-Autres industries extractives' = '08',
                              
                              '081-Extr de pierres, de sables et argiles' = '081',
                              '0811-Extr pierre ornement & construct etc' = '0811',
                              '0811Z-Extr pierre ornement & construct etc' = '0811Z',
                              '0812-Exploit gravière & sabl, extr argile' = '0812',
                              '0812Z-Exploit gravière & sabl, extr argile' = '0812Z',
                              
                              '089-Activités extractives nca' = '089',
                              '0891-Extr minéraux chimiq & engrais min' = '0891',
                              '0891Z-Extr minéraux chimiq & engrais min' = '0891Z',
                              '0892-Extraction de tourbe' = '0892',
                              '0892Z-Extraction de tourbe' = '0892Z',
                              '0893-Production de sel' = '0893',
                              '0893Z-Production de sel' = '0893Z',
                              '0899-Autres activités extractives nca' = '0899',
                              '0899Z-Autres activités extractives nca' = '0899Z',
                              
                              '09-Sces de soutien aux indust extractives' = '09',
                              
                              '091-Act de soutien à extr hydrocarbures' = '091',
                              '0910-Act de soutien à extr hydrocarbures' = '0910',
                              '0910Z-Act de soutien à extr hydrocarbures' = '0910Z',
                              
                              '099-Act de soutien aut indus extractives' = '099',
                              '0990-Act de soutien aut indus extractives' = '0990',
                              '0990Z-Act de soutien aut indus extractives' = '0990Z',
                              
                              'C-INDUSTRIE MANUFACTURIÈRE' = 'C',
                              
                              '10-Industries alimentaires' = '10',
                              
                              '101-Transf & conserv viande & prép viande' = '101',
                              '1011-Transf & conserv  viande de boucherie' = '1011',
                              '1011Z-Transf & conserv  viande de boucherie' = '1011Z',
                              '1012-Transf & conserv de viande de volaille' = '1012',
                              '1012Z-Transf & conserv de viande de volaille' = '1012Z',
                              '1013-Préparation de produits à base de viande' = '1013',
                              '1013A-Prépa indust produits à base de viande' = '1013A',
                              '1013B-Charcuterie' = '1013B',
                              
                              '102-Transf & conserv poisson, crust, etc' = '102',
                              '1020-Transf & conserv poisson, crust, etc' = '1020',
                              '1020Z-Transf & conserv poisson, crust, etc' = '1020Z',
                              
                              '103-Transf et conserv de fruits et légumes' = '103',
                              '1031-Transf et conserv de pommes de terre' = '1031',
                              '1031Z-Transf et conserv de pommes de terre' = '1031Z',
                              '1032-Préparation de jus de fruits et légumes' = '1032',
                              '1032Z-Préparation de jus de fruits et légumes' = '1032Z',
                              '1039-Aut transf & cons de fruit et légume' = '1039',
                              '1039A-Autre transf et conserv de légumes' = '1039A',
                              '1039B-Transformation et conservation de fruits' = '1039B',
                              
                              '104-Fab huile et graisse végétale & animale' = '104',
                              '1041-Fabrication huiles et graisses' = '1041',
                              '1041A-Fabrication huiles et graisses brutes' = '1041A',
                              '1041B-Fab huiles et graisses raffinées' = '1041B',
                              '1042-Fab de margarine & graisses similaires' = '1042',
                              '1042Z-Fab de margarine & graisses similaires' = '1042Z',
                              
                              '105-Fabrication de produits laitiers' = '105',
                              '1051-Expl de laiteries et fabric de fromage' = '1051',
                              '1051A-Fab de lait liquide & de produits frais' = '1051A',
                              '1051B-Fabrication de beurre' = '1051B',
                              '1051C-Fabrication de fromage' = '1051C',
                              '1051D-Fabrication autres produits laitiers' = '1051D',
                              '1052-Fabrication de glaces et sorbets' = '1052',
                              '1052Z-Fabrication de glaces et sorbets' = '1052Z',
                              
                              '106-Travail des grains ; fab prod amylacé' = '106',
                              '1061-Travail des grains' = '1061',
                              '1061A-Meunerie' = '1061A',
                              '1061B-Autres activités du travail des grains' = '1061B',
                              '1062-Fabrication de produits amylacés' = '1062',
                              '1062Z-Fabrication de produits amylacés' = '1062Z',
                              
                              '107-Fab prod boulangerie-pâtis & pâtes ' = '107',
                              '1071-Fab de pain et de pâtisserie fraîche' = '1071',
                              '1071A-Fab indus de pain & pâtisserie fraîche' = '1071A',
                              '1071B-Cuisson de produits de boulangerie' = '1071B',
                              '1071C-Boulangerie et boulangerie-pâtisserie' = '1071C',
                              '1071D-Pâtisserie' = '1071D',
                              '1072-Fab pain, biscuit & pâtiss de conserv' = '1072',
                              '1072Z-Fab pain, biscuit & pâtiss de conserv' = '1072Z',
                              '1073-Fabrication de pâtes alimentaires' = '1073',
                              '1073Z-Fabrication de pâtes alimentaires' = '1073Z',
                              
                              '108-Fabric autres produits alimentaires' = '108',
                              '1081-Fabrication de sucre' = '1081',
                              '1081Z-Fabrication de sucre' = '1081Z',
                              '1082-Fabric de cacao, chocolat & confiseries' = '1082',
                              '1082Z-Fabric de cacao, chocolat & confiseries' = '1082Z',
                              '1083-Transformation du thé et du café' = '1083',
                              '1083Z-Transformation du thé et du café' = '1083Z',
                              '1084-Fabric de condiments et assaisonnements' = '1084',
                              '1084Z-Fabric de condiments et assaisonnements' = '1084Z',
                              '1085-Fabrication de plats préparés' = '1085',
                              '1085Z-Fabrication de plats préparés' = '1085Z',
                              '1086-Fab aliment homogénéisé & diététique' = '1086',
                              '1086Z-Fab aliment homogénéisé & diététique' = '1086Z',
                              '1089-Fab autres prod alimentaires nca' = '1089',
                              '1089Z-Fab autres prod alimentaires nca' = '1089Z',
                              
                              '109-Fabrication aliments pour animaux' = '109',
                              '1091-Fabric aliments pour animaux de ferme' = '1091',
                              '1091Z-Fabric aliments pour animaux de ferme' = '1091Z',
                              '1092-Fab aliments pour animaux de compagnie' = '1092',
                              '1092Z-Fab aliments pour animaux de compagnie' = '1092Z',
                              
                              '11-Fabrication de boissons' = '11',
                              
                              '110-Fabrication de boissons' = '110',
                              '1101-Prod de boissons alcooliques distillées' = '1101',
                              '1101Z-Prod de boissons alcooliques distillées' = '1101Z',
                              '1102-Production de vin (de raisin)' = '1102',
                              '1102A-Fabrication de vins effervescents' = '1102A',
                              '1102B-Vinification' = '1102B',
                              '1103-Fabrication de cidre & de vins de fruits' = '1103',
                              '1103Z-Fabrication de cidre & de vins de fruits' = '1103Z',
                              '1104-Prod aut boisson fermentée non distil' = '1104',
                              '1104Z-Prod aut boisson fermentée non distil' = '1104Z',
                              '1105-Fabrication de bière' = '1105',
                              '1105Z-Fabrication de bière' = '1105Z',
                              '1106-Fabrication de malt' = '1106',
                              '1106Z-Fabrication de malt' = '1106Z',
                              '1107-Ind eaux & boissons rafraîchissantes' = '1107',
                              '1107A-Industrie des eaux de table' = '1107A',
                              '1107B-Production de boissons rafraîchissantes' = '1107B',
                              
                              '12-Fabrication de produits à base de tabac' = '12',
                              
                              '120-Fabrication de produits à base de tabac' = '120',
                              '1200-Fabrication de produits à base de tabac' = '1200',
                              '1200Z-Fabrication de produits à base de tabac' = '1200Z',
                              
                              '13-Fabrication de textiles' = '13',
                              
                              '131-Prépa de fibres textiles et filature' = '131',
                              '1310-Prépa de fibres textiles et filature' = '1310',
                              '1310Z-Prépa de fibres textiles et filature' = '1310Z',
                              
                              '132-Tissage' = '132',
                              '1320-Tissage' = '1320',
                              '1320Z-Tissage' = '1320Z',
                              
                              '133-Ennoblissement textile' = '133',
                              '1330-Ennoblissement textile' = '1330',
                              '1330Z-Ennoblissement textile' = '1330Z',
                              
                              '139-Fabrication autres textiles' = '139',
                              '1391-Fabrication étoffes à mailles' = '1391',
                              '1391Z-Fabrication étoffes à mailles' = '1391Z',
                              '1392-Fab article textile, sauf habillement' = '1392',
                              '1392Z-Fab article textile, sauf habillement' = '1392Z',
                              '1393-Fabrication de tapis et moquettes' = '1393',
                              '1393Z-Fabrication de tapis et moquettes' = '1393Z',
                              '1394-Fabric de ficelles, cordes et filets' = '1394',
                              '1394Z-Fabric de ficelles, cordes et filets' = '1394Z',
                              '1395-Fabric de non-tissés, sauf habillement' = '1395',
                              '1395Z-Fabric de non-tissés, sauf habillement' = '1395Z',
                              '1396-Fab autre textile techniq & industriel' = '1396',
                              '1396Z-Fab autre textile techniq & industriel' = '1396Z',
                              '1399-Fabrication autres textiles nca' = '1399',
                              '1399Z-Fabrication autres textiles nca' = '1399Z',
                              
                              '14-Industrie de habillement' = '14',
                              
                              '141-Fab de vêtements, autres qu en fourrure' = '141',
                              '1411-Fabrication de vêtements en cuir' = '1411',
                              '1411Z-Fabrication de vêtements en cuir' = '1411Z',
                              '1412-Fabrication de vêtements de travail' = '1412',
                              '1412Z-Fabrication de vêtements de travail' = '1412Z',
                              '1413-Fabrication de vêtements de dessus' = '1413',
                              '1413Z-Fabrication de vêtements de dessus' = '1413Z',
                              '1414-Fabrication de vêtements de dessous' = '1414',
                              '1414Z-Fabrication de vêtements de dessous' = '1414Z',
                              '1419-Fabric autres vêtements et accessoires' = '1419',
                              '1419Z-Fabric autres vêtements et accessoires' = '1419Z',
                              
                              '142-Fabrication articles en fourrure' = '142',
                              '1420-Fabrication articles en fourrure' = '1420',
                              '1420Z-Fabrication articles en fourrure' = '1420Z',
                              
                              '143-Fabrication articles à mailles' = '143',
                              '1431-Fabric articles chaussants à mailles' = '1431',
                              '1431Z-Fabric articles chaussants à mailles' = '1431Z',
                              '1439-Fabrication autres articles à mailles' = '1439',
                              '1439Z-Fabrication autres articles à mailles' = '1439Z',
                              
                              '15-Industrie du cuir et de la chaussure' = '15',
                              
                              '151-Indust cuir & fourrure; maroq & selle' = '151',
                              '1511-Prépa cuirs; prép & teinture fourrures' = '1511',
                              '1511Z-Prépa cuirs; prép & teinture fourrures' = '1511Z',
                              '1512-Fab art voyage, maroquin, & sellerie' = '1512',
                              '1512Z-Fab art voyage, maroquin, & sellerie' = '1512Z',
                              
                              '152-Fabrication de chaussures' = '152',
                              '1520-Fabrication de chaussures' = '1520',
                              '1520Z-Fabrication de chaussures' = '1520Z',
                              
                              '16-Trav bois; fab article bois, vannerie' = '16',
                              
                              '161-Sciage et rabotage du bois' = '161',
                              '1610-Sciage et rabotage du bois' = '1610',
                              '1610A-Sciage & rabotage bois, sf imprégnation' = '1610A',
                              '1610B-Imprégnation du bois' = '1610B',
                              
                              '162-Fab article bois, liège, vannerie, etc' = '162',
                              '1621-Fabric  placage et panneaux de bois' = '1621',
                              '1621Z-Fabric  placage et panneaux de bois' = '1621Z',
                              '1622-Fabrication de parquets assemblés' = '1622',
                              '1622Z-Fabrication de parquets assemblés' = '1622Z',
                              '1623-Fab charpentes et autres menuiseries' = '1623',
                              '1623Z-Fab charpentes et autres menuiseries' = '1623Z',
                              '1624-Fabrication emballages en bois' = '1624',
                              '1624Z-Fabrication emballages en bois' = '1624Z',
                              '1629-Fab objet div bois, liège, vann, etc' = '1629',
                              '1629Z-Fab objet div bois, liège, vann, etc' = '1629Z',
                              
                              '17-Industrie du papier et du carton' = '17',
                              
                              '171-Fab de pâte à papier, papier et carton' = '171',
                              '1711-Fabrication de pâte à papier' = '1711',
                              '1711Z-Fabrication de pâte à papier' = '1711Z',
                              '1712-Fabrication de papier et de carton' = '1712',
                              '1712Z-Fabrication de papier et de carton' = '1712Z',
                              
                              '172-Fab articles en papier ou en carton' = '172',
                              '1721-Fab papier & carton & emball pap-car' = '1721',
                              '1721A-Fabrication de carton ondulé' = '1721A',
                              '1721B-Fabrication de cartonnages' = '1721B',
                              '1721C-Fabrication emballages en papier' = '1721C',
                              '1722-Fab article papier sanit ou domestique' = '1722',
                              '1722Z-Fab article papier sanit ou domestique' = '1722Z',
                              '1723-Fabrication articles de papeterie' = '1723',
                              '1723Z-Fabrication articles de papeterie' = '1723Z',
                              '1724-Fabrication de papiers peints' = '1724',
                              '1724Z-Fabrication de papiers peints' = '1724Z',
                              '1729-Fab aut article en papier ou en carton' = '1729',
                              '1729Z-Fab aut article en papier ou en carton' = '1729Z',
                              
                              '18-Imprimerie & reprod enregistrements' = '18',
                              
                              '181-Imprimerie et services annexes' = '181',
                              '1811-Imprimerie de journaux' = '1811',
                              '1811Z-Imprimerie de journaux' = '1811Z',
                              '1812-Autre imprimerie (labeur)' = '1812',
                              '1812Z-Autre imprimerie (labeur)' = '1812Z',
                              '1813-Activités de pré-presse' = '1813',
                              '1813Z-Activités de pré-presse' = '1813Z',
                              '1814-Reliure et activités connexes' = '1814',
                              '1814Z-Reliure et activités connexes' = '1814Z',
                              
                              '182-Reproduction enregistrements' = '182',
                              '1820-Reproduction enregistrements' = '1820',
                              '1820Z-Reproduction enregistrements' = '1820Z',
                              
                              '19-Cokéfaction et raffinage' = '19',
                              
                              '191-Cokéfaction' = '191',
                              '1910-Cokéfaction' = '1910',
                              '1910Z-Cokéfaction' = '1910Z',
                              
                              '192-Raffinage du pétrole' = '192',
                              '1920-Raffinage du pétrole' = '1920',
                              '1920Z-Raffinage du pétrole' = '1920Z',
                              
                              '20-Industrie chimique' = '20',
                              
                              '201-Fab prod chimi, plast & caout synt' = '201',
                              '2011-Fabrication de gaz industriels' = '2011',
                              '2011Z-Fabrication de gaz industriels' = '2011Z',
                              '2012-Fabrication de colorants et de pigments' = '2012',
                              '2012Z-Fabrication de colorants et de pigments' = '2012Z',
                              '2013-Fab aut prod chimique inorg de base' = '2013',
                              '2013A-Enrichissment & retrait mat nucléaire' = '2013A',
                              '2013B-Fab aut prod chim inorg base nca' = '2013B',
                              '2014-Fab aut prod chimique org de base ' = '2014',
                              '2014Z-Fab aut prod chimique org de base ' = '2014Z',
                              '2015-Fabric de produits azotés et engrais' = '2015',
                              '2015Z-Fabric de produits azotés et engrais' = '2015Z',
                              '2016-Fabric de matières plastiques de base' = '2016',
                              '2016Z-Fabric de matières plastiques de base' = '2016Z',
                              '2017-Fabrication de caoutchouc synthétique' = '2017',
                              '2017Z-Fabrication de caoutchouc synthétique' = '2017Z',
                              
                              '202-Fab pesticide & aut prod agrochimique' = '202',
                              '2020-Fab pesticide & aut prod agrochimique' = '2020',
                              '2020Z-Fab pesticide & aut prod agrochimique' = '2020Z',
                              
                              '203-Fab de peinture, vernis, encre & mastic' = '203',
                              '2030-Fab de peinture, vernis, encre & mastic' = '2030',
                              '2030Z-Fab de peinture, vernis, encre & mastic' = '2030Z',
                              
                              '204-Fab savon, prod entretien & parfum' = '204',
                              '2041-Fab savon, détergent & prod entretien' = '2041',
                              '2041Z-Fab savon, détergent & prod entretien' = '2041Z',
                              '2042-Fab parfum & produit pour la toilette' = '2042',
                              '2042Z-Fab parfum & produit pour la toilette' = '2042Z',
                              
                              '205-Fabrication autres produits chimiques' = '205',
                              '2051-Fabrication de produits explosifs' = '2051',
                              '2051Z-Fabrication de produits explosifs' = '2051Z',
                              '2052-Fabrication de colles' = '2052',
                              '2052Z-Fabrication de colles' = '2052Z',
                              '2053-Fabrication huiles essentielles' = '2053',
                              '2053Z-Fabrication huiles essentielles' = '2053Z',
                              '2059-Fabric autres produits chimiques nca' = '2059',
                              '2059Z-Fabric autres produits chimiques nca' = '2059Z',
                              
                              '206-Fab  fibre artificielle ou synthétique' = '206',
                              '2060-Fab  fibre artificielle ou synthétique' = '2060',
                              '2060Z-Fab  fibre artificielle ou synthétique' = '2060Z',
                              
                              '21-Industrie pharmaceutique' = '21',
                              
                              '211-Fab de produits pharmaceutiques de base' = '211',
                              '2110-Fab de produits pharmaceutiques de base' = '2110',
                              '2110Z-Fab de produits pharmaceutiques de base' = '2110Z',
                              
                              '212-Fabric de préparations pharmaceutiques' = '212',
                              '2120-Fabric de préparations pharmaceutiques' = '2120',
                              '2120Z-Fabric de préparations pharmaceutiques' = '2120Z',
                              
                              '22-Fab prod en caoutchouc & en plastique' = '22',
                              
                              '221-Fabrication de produits en caoutchouc' = '221',
                              '2211-Fabrication et rechapage de pneumatiques' = '2211',
                              '2211Z-Fabrication et rechapage de pneumatiques' = '2211Z',
                              '2219-Fabric autres articles en caoutchouc' = '2219',
                              '2219Z-Fabric autres articles en caoutchouc' = '2219Z',
                              
                              '222-Fabrication  de produits en plastique' = '222',
                              '2221-Fab plaque, feuille, tube,  etc plast' = '2221',
                              '2221Z-Fab plaque, feuille, tube,  etc plast' = '2221Z',
                              '2222-Fab emballage en matière plastique' = '2222',
                              '2222Z-Fab emballage en matière plastique' = '2222Z',
                              '2223-Fab élément mat plastiq pr construct' = '2223',
                              '2223Z-Fab élément mat plastiq pr construct' = '2223Z',
                              '2229-Fab autre article en matière plastique' = '2229',
                              '2229A-Fab pièce techniq base mat plastiq' = '2229A',
                              '2229B-Fab prod conso courante en plastique' = '2229B',
                              
                              '23-Fab aut prod minéraux non métalliques' = '23',
                              
                              '231-Fabric de verre et articles en verre' = '231',
                              '2311-Fabrication de verre plat' = '2311',
                              '2311Z-Fabrication de verre plat' = '2311Z',
                              '2312-Façonnage & transformation du verre plat' = '2312',
                              '2312Z-Façonnage & transformation du verre plat' = '2312Z',
                              '2313-Fabrication de verre creux' = '2313',
                              '2313Z-Fabrication de verre creux' = '2313Z',
                              '2314-Fabrication de fibres de verre' = '2314',
                              '2314Z-Fabrication de fibres de verre' = '2314Z',
                              '2319-Fab & façonnage aut article en verre' = '2319',
                              '2319Z-Fab & façonnage aut article en verre' = '2319Z',
                              
                              '232-Fabrication de produits réfractaires' = '232',
                              '2320-Fabrication de produits réfractaires' = '2320',
                              '2320Z-Fabrication de produits réfractaires' = '2320Z',
                              
                              '233-Fab matériaux de constr en terre cuite' = '233',
                              '2331-Fabrication de carreaux en céramique' = '2331',
                              '2331Z-Fabrication de carreaux en céramique' = '2331Z',
                              '2332-Fab produit  construct en terre cuite' = '2332',
                              '2332Z-Fab produit  construct en terre cuite' = '2332Z',
                              
                              '234-Fab aut prod en céramiq & porcelaine' = '234',
                              '2341-Fab art céramiq usage domest & déco' = '2341',
                              '2341Z-Fab art céramiq usage domest & déco' = '2341Z',
                              '2342-Fab appareil sanitaire en céramique' = '2342',
                              '2342Z-Fab appareil sanitaire en céramique' = '2342Z',
                              '2343-Fab isolateur & pièce isolante céramiq' = '2343',
                              '2343Z-Fab isolateur & pièce isolante céramiq' = '2343Z',
                              '2344-Fab aut prod céram à usage technique' = '2344',
                              '2344Z-Fab aut prod céram à usage technique' = '2344Z',
                              '2349-Fabrication autres produits céramiques' = '2349',
                              '2349Z-Fabrication autres produits céramiques' = '2349Z',
                              
                              '235-Fabrication de ciment, chaux et plâtre' = '235',
                              '2351-Fabrication de ciment' = '2351',
                              '2351Z-Fabrication de ciment' = '2351Z',
                              '2352-Fabrication de chaux et plâtre' = '2352',
                              '2352Z-Fabrication de chaux et plâtre' = '2352Z',
                              
                              '236-Fabric ouvrage en béton, ciment, plâtre' = '236',
                              '2361-Fab élément en béton pour la construct' = '2361',
                              '2361Z-Fab élément en béton pour la construct' = '2361Z',
                              '2362-Fab élément en plâtre pour la construc' = '2362',
                              '2362Z-Fab élément en plâtre pour la construc' = '2362Z',
                              '2363-Fabrication de béton prêt à emploi' = '2363',
                              '2363Z-Fabrication de béton prêt à emploi' = '2363Z',
                              '2364-Fabrication de mortiers et bétons secs' = '2364',
                              '2364Z-Fabrication de mortiers et bétons secs' = '2364Z',
                              '2365-Fabrication ouvrages en fibre-ciment' = '2365',
                              '2365Z-Fabrication ouvrages en fibre-ciment' = '2365Z',
                              '2369-Fab aut ouvrage béton, ciment, plâtre' = '2369',
                              '2369Z-Fab aut ouvrage béton, ciment, plâtre' = '2369Z',
                              
                              '237-Taille, façonnage & finissage de pierres' = '237',
                              '2370-Taille, façonnage & finissage de pierres' = '2370',
                              '2370Z-Taille, façonnage & finissage de pierres' = '2370Z',
                              
                              '239-Fab pdt abrasif & minér non métal nca' = '239',
                              '2391-Fabrication de produits abrasifs' = '2391',
                              '2391Z-Fabrication de produits abrasifs' = '2391Z',
                              '2399-Fab aut prod minéraux non métal nca' = '2399',
                              '2399Z-Fab aut prod minéraux non métal nca' = '2399Z',
                              
                              '24-Métallurgie' = '24',
                              
                              '241-Sidérurgie' = '241',
                              '2410-Sidérurgie' = '2410',
                              '2410Z-Sidérurgie' = '2410Z',
                              
                              '242-Fab tube, profilé creux etc en acier' = '242',
                              '2420-Fab tube, profilé creux etc en acier' = '2420',
                              '2420Z-Fab tube, profilé creux etc en acier' = '2420Z',
                              
                              '243-Fab aut prod de 1ère transform acier' = '243',
                              '2431-Étirage à froid de barres' = '2431',
                              '2431Z-Étirage à froid de barres' = '2431Z',
                              '2432-Laminage à froid de feuillards' = '2432',
                              '2432Z-Laminage à froid de feuillards' = '2432Z',
                              '2433-Profilage à froid par formage ou pliage' = '2433',
                              '2433Z-Profilage à froid par formage ou pliage' = '2433Z',
                              '2434-Tréfilage à froid' = '2434',
                              '2434Z-Tréfilage à froid' = '2434Z',
                              
                              '244-Prod métaux précieux & aut m non fer' = '244',
                              '2441-Production de métaux précieux' = '2441',
                              '2441Z-Production de métaux précieux' = '2441Z',
                              '2442-Métallurgie de aluminium' = '2442',
                              '2442Z-Métallurgie de aluminium' = '2442Z',
                              '2443-Métallurgie du Pb, du Zn ou du Sn' = '2443',
                              '2443Z-Métallurgie du Pb, du Zn ou du Sn' = '2443Z',
                              '2444-Métallurgie du cuivre' = '2444',
                              '2444Z-Métallurgie du cuivre' = '2444Z',
                              '2445-Métallurgie autres métaux non ferreux' = '2445',
                              '2445Z-Métallurgie autres métaux non ferreux' = '2445Z',
                              '2446-Élaboration et transform mat nucléaire' = '2446',
                              '2446Z-Élaboration et transform mat nucléaire' = '2446Z',
                              
                              '245-Fonderie' = '245',
                              '2451-Fonderie de fonte' = '2451',
                              '2451Z-Fonderie de fonte' = '2451Z',
                              '2452-Fonderie acier' = '2452',
                              '2452Z-Fonderie acier' = '2452Z',
                              '2453-Fonderie de métaux légers' = '2453',
                              '2453Z-Fonderie de métaux légers' = '2453Z',
                              '2454-Fonderie autres métaux non ferreux' = '2454',
                              '2454Z-Fonderie autres métaux non ferreux' = '2454Z',
                              
                              '25-Fab prod métalliq sf machine & équipt' = '25',
                              
                              '251-Fab élément en métal pour la construct' = '251',
                              '2511-Fab structure métal & partie structure' = '2511',
                              '2511Z-Fab structure métal & partie structure' = '2511Z',
                              '2512-Fabric de portes et fenêtres en métal' = '2512',
                              '2512Z-Fabric de portes et fenêtres en métal' = '2512Z',
                              
                              '252-Fab réservr, citerne & conteneur métal' = '252',
                              '2521-Fab radiat & chaudière pr chauf ctral' = '2521',
                              '2521Z-Fab radiat & chaudière pr chauf ctral' = '2521Z',
                              '2529-Fab aut réservr, citerne, etc métal' = '2529',
                              '2529Z-Fab aut réservr, citerne, etc métal' = '2529Z',
                              
                              '253-Fab générat vapeur sf pr chauff ctral' = '253',
                              '2530-Fab générat vapeur sf pr chauff ctral' = '2530',
                              '2530Z-Fab générat vapeur sf pr chauff ctral' = '2530Z',
                              
                              '254-Fabrication armes et de munitions' = '254',
                              '2540-Fabrication armes et de munitions' = '2540',
                              '2540Z-Fabrication armes et de munitions' = '2540Z',
                              
                              '255-Forge, etc; métallurgie des poudres' = '255',
                              '2550-Forge, etc; métallurgie des poudres' = '2550',
                              '2550A-Forge; métallurgie des poudres' = '2550A',
                              '2550B-Découpage, emboutissage' = '2550B',
                              
                              '256-Traitmnt & revêtmnt des métaux; usinage' = '256',
                              '2561-Traitement et revêtement des métaux' = '2561',
                              '2561Z-Traitement et revêtement des métaux' = '2561Z',
                              '2562-Usinage' = '2562',
                              '2562A-Décolletage' = '2562A',
                              '2562B-Mécanique industrielle' = '2562B',
                              
                              '257-Fab coutellerie, outillage & quincaill' = '257',
                              '2571-Fabrication de coutellerie' = '2571',
                              '2571Z-Fabrication de coutellerie' = '2571Z',
                              '2572-Fabrication de serrures et de ferrures' = '2572',
                              '2572Z-Fabrication de serrures et de ferrures' = '2572Z',
                              '2573-Fabrication outillage' = '2573',
                              '2573A-Fabrication de moules et modèles' = '2573A',
                              '2573B-Fabrication autres outillages' = '2573B',
                              
                              '259-Fabrication autres ouvrages en métaux' = '259',
                              '2591-Fab fût & emballage métalliq similaire' = '2591',
                              '2591Z-Fab fût & emballage métalliq similaire' = '2591Z',
                              '2592-Fabric emballages métalliques légers' = '2592',
                              '2592Z-Fabric emballages métalliques légers' = '2592Z',
                              '2593-Fab art  fil métal, chaîne & ressort' = '2593',
                              '2593Z-Fab art  fil métal, chaîne & ressort' = '2593Z',
                              '2594-Fabrication de vis et de boulons' = '2594',
                              '2594Z-Fabrication de vis et de boulons' = '2594Z',
                              '2599-Fabric autres prod métalliques nca' = '2599',
                              '2599A-Fabric articles métalliques ménagers' = '2599A',
                              '2599B-Fabric autres articles métalliques' = '2599B',
                              
                              '26-Fab prod informat, électroniq & opt' = '26',
                              
                              '261-Fab de composants & cartes électroniq' = '261',
                              '2611-Fabrication de composants électroniques' = '2611',
                              '2611Z-Fabrication de composants électroniques' = '2611Z',
                              '2612-Fab de cartes électroniques assemblées' = '2612',
                              '2612Z-Fab de cartes électroniques assemblées' = '2612Z',
                              
                              '262-Fab ordinateur & équipement périphériq' = '262',
                              '2620-Fab ordinateur & équipement périphériq' = '2620',
                              '2620Z-Fab ordinateur & équipement périphériq' = '2620Z',
                              
                              '263-Fabric équipements de communication' = '263',
                              '2630-Fabric équipements de communication' = '2630',
                              '2630Z-Fabric équipements de communication' = '2630Z',
                              
                              '264-Fab produit électronique grand public' = '264',
                              '2640-Fab produit électronique grand public' = '2640',
                              '2640Z-Fab produit électronique grand public' = '2640Z',
                              
                              '265-Fab instr mesure, navigat; horlogerie' = '265',
                              '2651-Fab instrum de mesure & de navigation' = '2651',
                              '2651A-Fab équipement aide à la navigation' = '2651A',
                              '2651B-Fab instrumentation scientifiq & tech' = '2651B',
                              '2652-Horlogerie' = '2652',
                              '2652Z-Horlogerie' = '2652Z',
                              
                              '266-Fab éqpt irrad médic & électromedic' = '266',
                              '2660-Fab éqpt irrad médic & électromedic' = '2660',
                              '2660Z-Fab éqpt irrad médic & électromedic' = '2660Z',
                              
                              '267-Fab matériel optique et photographique' = '267',
                              '2670-Fab matériel optique et photographique' = '2670',
                              '2670Z-Fab matériel optique et photographique' = '2670Z',
                              
                              '268-Fab de supports magnétiques et optiques' = '268',
                              '2680-Fab de supports magnétiques et optiques' = '2680',
                              '2680Z-Fab de supports magnétiques et optiques' = '2680Z',
                              
                              '27-Fabrication équipements électriques' = '27',
                              
                              '271-Fab moteur génér transfo & mat élec' = '271',
                              '2711-Fab moteur génér transfo & mat élec' = '2711',
                              '2711Z-Fab moteur génér transfo & mat élec' = '2711Z',
                              '2712-Fab mat de distrib & de cde électri' = '2712',
                              '2712Z-Fab mat de distrib & de cde électri' = '2712Z',
                              
                              '272-Fabric pile & accumulateur électrique' = '272',
                              '2720-Fabric pile & accumulateur électrique' = '2720',
                              '2720Z-Fabric pile & accumulateur électrique' = '2720Z',
                              
                              '273-Fab fil câble & mat install électriq' = '273',
                              '2731-Fabrication de câbles de fibres optiques' = '2731',
                              '2731Z-Fabrication de câbles de fibres optiques' = '2731Z',
                              '2732-Fab aut fil & câble éltron ou éltriq' = '2732',
                              '2732Z-Fab aut fil & câble éltron ou éltriq' = '2732Z',
                              '2733-Fabric matériel installation électrique' = '2733',
                              '2733Z-Fabric matériel installation électrique' = '2733Z',
                              
                              '274-Fabric appareils éclairage électrique' = '274',
                              '2740-Fabric appareils éclairage électrique' = '2740',
                              '2740Z-Fabric appareils éclairage électrique' = '2740Z',
                              
                              '275-Fabrication appareils ménagers' = '275',
                              '2751-Fabrication appareils électroménagers' = '2751',
                              '2751Z-Fabrication appareils électroménagers' = '2751Z',
                              '2752-Fab appareils ménagers non électriques' = '2752',
                              '2752Z-Fab appareils ménagers non électriques' = '2752Z',
                              
                              '279-Fabric autres matériels électriques' = '279',
                              '2790-Fabric autres matériels électriques' = '2790',
                              '2790Z-Fabric autres matériels électriques' = '2790Z',
                              
                              '28-Fabric de machines & équipements nca' = '28',
                              
                              '281-Fabrication de machines usage général' = '281',
                              '2811-Fab moteur & turb sf pr avion & véhic' = '2811',
                              '2811Z-Fab moteur & turb sf pr avion & véhic' = '2811Z',
                              '2812-Fab équipement hydraulique & pneumatiq' = '2812',
                              '2812Z-Fab équipement hydraulique & pneumatiq' = '2812Z',
                              '2813-Fabric autres pompes et compresseurs' = '2813',
                              '2813Z-Fabric autres pompes et compresseurs' = '2813Z',
                              '2814-Fabric autres articles de robinetterie' = '2814',
                              '2814Z-Fabric autres articles de robinetterie' = '2814Z',
                              '2815-Fab engrenage & organe méca transmis' = '2815',
                              '2815Z-Fab engrenage & organe méca transmis' = '2815Z',
                              
                              '282-Fabric autres machines usage général' = '282',
                              '2821-Fabrication de fours et brûleurs' = '2821',
                              '2821Z-Fabrication de fours et brûleurs' = '2821Z',
                              '2822-Fab matériel de levage & de manutention' = '2822',
                              '2822Z-Fab matériel de levage & de manutention' = '2822Z',
                              '2823-Fab machine équipt bureau (sf ordinat)' = '2823',
                              '2823Z-Fab machine équipt bureau (sf ordinat)' = '2823Z',
                              '2824-Fab outillage portatif à moteur incorp' = '2824',
                              '2824Z-Fab outillage portatif à moteur incorp' = '2824Z',
                              '2825-Fab équipt aérauliq & frigorifiq ind' = '2825',
                              '2825Z-Fab équipt aérauliq & frigorifiq ind' = '2825Z',
                              '2829-Fab machines diverses usage général' = '2829',
                              '2829A-Fab éqpt emballage condition & pesage' = '2829A',
                              '2829B-Fab autres machines usage général' = '2829B',
                              
                              '283-Fab machines agricoles et forestières' = '283',
                              '2830-Fab machines agricoles et forestières' = '2830',
                              '2830Z-Fab machines agricoles et forestières' = '2830Z',
                              
                              '284-Fab mach formage métaux & mach-outil' = '284',
                              '2841-Fab de machines de formage des métaux' = '2841',
                              '2841Z-Fab de machines de formage des métaux' = '2841Z',
                              '2849-Fabrication autres machines-outils' = '2849',
                              '2849Z-Fabrication autres machines-outils' = '2849Z',
                              
                              '289-Fabric autre machine usage spécifique' = '289',
                              '2891-Fabric de machines pour la métallurgie' = '2891',
                              '2891Z-Fabric de machines pour la métallurgie' = '2891Z',
                              '2892-Fab machine pour extraction ou constr' = '2892',
                              '2892Z-Fab machine pour extraction ou constr' = '2892Z',
                              '2893-Fab machine pour indus agro-aliment' = '2893',
                              '2893Z-Fab machine pour indus agro-aliment' = '2893Z',
                              '2894-Fab machine pour industries textiles' = '2894',
                              '2894Z-Fab machine pour industries textiles' = '2894Z',
                              '2895-Fab machine pr indus papier & carton' = '2895',
                              '2895Z-Fab machine pr indus papier & carton' = '2895Z',
                              '2896-Fab machine pr trav du caoutch, plast' = '2896',
                              '2896Z-Fab machine pr trav du caoutch, plast' = '2896Z',
                              '2899-Fab autre machine usage spécifi nca' = '2899',
                              '2899A-Fabrication de machines imprimerie' = '2899A',
                              '2899B-Fabric autres machines spécialisées' = '2899B',
                              
                              '29-Industrie automobile' = '29',
                              
                              '291-Construction de véhicules automobiles' = '291',
                              '2910-Construction de véhicules automobiles' = '2910',
                              '2910Z-Construction de véhicules automobiles' = '2910Z',
                              
                              '292-Fabrication de carrosseries et remorques' = '292',
                              '2920-Fabrication de carrosseries et remorques' = '2920',
                              '2920Z-Fabrication de carrosseries et remorques' = '2920Z',
                              
                              '293-Fabrication équipements automobiles' = '293',
                              '2931-Fab équipt électriq & électron auto' = '2931',
                              '2931Z-Fab équipt électriq & électron auto' = '2931Z',
                              '2932-Fabric autres équipements automobiles' = '2932',
                              '2932Z-Fabric autres équipements automobiles' = '2932Z',
                              
                              '30-Fabric autres matériels de transport' = '30',
                              
                              '301-Construction navale' = '301',
                              '3011-Construct navires & structure flottante' = '3011',
                              '3011Z-Construct navires & structure flottante' = '3011Z',
                              '3012-Construction de bateaux de plaisance' = '3012',
                              '3012Z-Construction de bateaux de plaisance' = '3012Z',
                              
                              '302-Const loco & autre mat ferro roulant' = '302',
                              '3020-Const loco & autre mat ferro roulant' = '3020',
                              '3020Z-Const loco & autre mat ferro roulant' = '3020Z',
                              
                              '303-Construction aéronautique et spatiale' = '303',
                              '3030-Construction aéronautique et spatiale' = '3030',
                              '3030Z-Construction aéronautique et spatiale' = '3030Z',
                              
                              '304-Constr véhicules militaires de combat' = '304',
                              '3040-Constr véhicules militaires de combat' = '3040',
                              '3040Z-Constr véhicules militaires de combat' = '3040Z',
                              
                              '309-Fabric de matériels de transport nca' = '309',
                              '3091-Fabrication de motocycles' = '3091',
                              '3091Z-Fabrication de motocycles' = '3091Z',
                              '3092-Fab bicyclette & véhic pour invalides' = '3092',
                              '3092Z-Fab bicyclette & véhic pour invalides' = '3092Z',
                              '3099-Fab aut équipement de transport nca' = '3099',
                              '3099Z-Fab aut équipement de transport nca' = '3099Z',
                              
                              '31-Fabrication de meubles' = '31',
                              
                              '310-Fabrication de meubles' = '310',
                              '3101-Fab de meubles de bureau et de magasin' = '3101',
                              '3101Z-Fab de meubles de bureau et de magasin' = '3101Z',
                              '3102-Fabrication de meubles de cuisine' = '3102',
                              '3102Z-Fabrication de meubles de cuisine' = '3102Z',
                              '3103-Fabrication de matelas' = '3103',
                              '3103Z-Fabrication de matelas' = '3103Z',
                              '3109-Fabrication autres meubles' = '3109',
                              '3109A-Fabric sièges ameublement intérieur' = '3109A',
                              '3109B-Fab aut meub & ind connexe ameublmnt' = '3109B',
                              
                              '32-Autres industries manufacturières' = '32',
                              
                              '321-Fab artic joaillerie, bijout & simil' = '321',
                              '3211-Frappe de monnaie' = '3211',
                              '3211Z-Frappe de monnaie' = '3211Z',
                              '3212-Fab article de joaillerie et bijouterie' = '3212',
                              '3212Z-Fab article de joaillerie et bijouterie' = '3212Z',
                              '3213-Fab art bijout fantaisie & similaire' = '3213',
                              '3213Z-Fab art bijout fantaisie & similaire' = '3213Z',
                              
                              '322-Fabrication instruments de musique' = '322',
                              '3220-Fabrication instruments de musique' = '3220',
                              '3220Z-Fabrication instruments de musique' = '3220Z',
                              
                              '323-Fabrication articles de sport' = '323',
                              '3230-Fabrication articles de sport' = '3230',
                              '3230Z-Fabrication articles de sport' = '3230Z',
                              
                              '324-Fabrication de jeux et jouets' = '324',
                              '3240-Fabrication de jeux et jouets' = '3240',
                              '3240Z-Fabrication de jeux et jouets' = '3240Z',
                              
                              '325-Fab inst & fournit usage méd & dent' = '325',
                              '3250-Fab inst & fournit usage méd & dent' = '3250',
                              '3250A-Fab matériel médico-chirurg & dentaire' = '3250A',
                              '3250B-Fabrication de lunettes' = '3250B',
                              
                              '329-Activités manufacturières nca' = '329',
                              '3291-Fabrication d’articles de brosserie' = '3291',
                              '3291Z-Fabrication d’articles de brosserie' = '3291Z',
                              '3299-Autres activités manufacturières nca' = '3299',
                              '3299Z-Autres activités manufacturières nca' = '3299Z',
                              
                              '33-Réparation & install machine & équipt' = '33',
                              
                              '331-Répar ouvrage en métaux, mach & équipt' = '331',
                              '3311-Réparation ouvrages en métaux' = '3311',
                              '3311Z-Réparation ouvrages en métaux' = '3311Z',
                              '3312-Répar machine & équipement mécaniques' = '3312',
                              '3312Z-Répar machine & équipement mécaniques' = '3312Z',
                              '3313-Répar matériel électronique & optique' = '3313',
                              '3313Z-Répar matériel électronique & optique' = '3313Z',
                              '3314-Réparation équipements électriques' = '3314',
                              '3314Z-Réparation équipements électriques' = '3314Z',
                              '3315-Réparation et maintenance navale' = '3315',
                              '3315Z-Réparation et maintenance navale' = '3315Z',
                              '3316-Répar & maint aéronef & eng spatiaux' = '3316',
                              '3316Z-Répar & maint aéronef & eng spatiaux' = '3316Z',
                              '3317-Répar & maint aut équipt transport' = '3317',
                              '3317Z-Répar & maint aut équipt transport' = '3317Z',
                              '3319-Réparation autres équipements' = '3319',
                              '3319Z-Réparation autres équipements' = '3319Z',
                              
                              '332-Install de machines & équipt industriel' = '332',
                              '3320-Install de machines & équipt industriel' = '3320',
                              '3320A-Instal struct métal, chaudr & tuyau' = '3320A',
                              '3320B-Instal machines & équipement mécanique' = '3320B',
                              '3320C-Instal éqpts ctrle des processus indus' = '3320C',
                              '3320D-Inst éqpt élec électro optiq ou aut' = '3320D',
                              
                              'D-Prdn & distr élec gaz vap & air cond' = 'D',
                              
                              '35-Prdn & distr élec gaz vap & air cond' = '35',
                              
                              '351-Prod, transport & distrib électricité' = '351',
                              '3511-Production électricité' = '3511',
                              '3511Z-Production électricité' = '3511Z',
                              '3512-Transport électricité' = '3512',
                              '3512Z-Transport électricité' = '3512Z',
                              '3513-Distribution électricité' = '3513',
                              '3513Z-Distribution électricité' = '3513Z',
                              '3514-Commerce électricité' = '3514',
                              '3514Z-Commerce électricité' = '3514Z',
                              
                              '352-Prod & distrib de combustibles gazeux' = '352',
                              '3521-Production de combustibles gazeux' = '3521',
                              '3521Z-Production de combustibles gazeux' = '3521Z',
                              '3522-Distrib combustible gazeux pr conduites' = '3522',
                              '3522Z-Distrib combustible gazeux pr conduites' = '3522Z',
                              '3523-Commerce combustible gazeux par conduite' = '3523',
                              '3523Z-Commerce combustible gazeux par conduite' = '3523Z',
                              
                              '353-Prod & distrib vapeur et air condit' = '353',
                              '3530-Prod & distrib vapeur et air condit' = '3530',
                              '3530Z-Prod & distrib vapeur et air condit' = '3530Z',
                              
                              'E-Gestion eau, déchets & dépollution' = 'E',
                              
                              '36-Captage, traitement & distribution eau' = '36',
                              
                              '360-Captage, traitement & distribution eau' = '360',
                              '3600-Captage, traitement & distribution eau' = '3600',
                              '3600Z-Captage, traitement & distribution eau' = '3600Z',
                              
                              '37-Collecte et traitement des eaux usées' = '37',
                              
                              '370-Collecte et traitement des eaux usées' = '370',
                              '3700-Collecte et traitement des eaux usées' = '3700',
                              '3700Z-Collecte et traitement des eaux usées' = '3700Z',
                              
                              '38-Collecte, gestion déchets ; récupération' = '38',
                              
                              '381-Collecte des déchets' = '381',
                              '3811-Collecte des déchets non dangereux' = '3811',
                              '3811Z-Collecte des déchets non dangereux' = '3811Z',
                              '3812-Collecte des déchets dangereux' = '3812',
                              '3812Z-Collecte des déchets dangereux' = '3812Z',
                              
                              '382-Traitement et élimination des déchets' = '382',
                              '3821-Traitmnt & élimin déchets non dangereux' = '3821',
                              '3821Z-Traitmnt & élimin déchets non dangereux' = '3821Z',
                              '3822-Traitmnt & élimination déchets dangereux' = '3822',
                              '3822Z-Traitmnt & élimination déchets dangereux' = '3822Z',
                              
                              '383-Récupération' = '383',
                              '3831-Démantèlement épaves' = '3831',
                              '3831Z-Démantèlement épaves' = '3831Z',
                              '3832-Récupération de déchets triés' = '3832',
                              '3832Z-Récupération de déchets triés' = '3832Z',
                              
                              '39-Dépollution & autre sces gestion déchets' = '39',
                              
                              '390-Dépollution & autre sces gestion déchets' = '390',
                              '3900-Dépollution & autre sces gestion déchets' = '3900',
                              '3900Z-Dépollution & autre sces gestion déchets' = '3900Z',
                              
                              'F-CONSTRUCTION' = 'F',
                              
                              '41-Construction de bâtiments' = '41',
                              
                              '411-Promotion immobilière' = '411',
                              '4110-Promotion immobilière' = '4110',
                              '4110A-Promotion immobilière de logements' = '4110A',
                              '4110B-Promotion immobilière de bureaux' = '4110B',
                              '4110C-Promotion immobilière autres bâtiments' = '4110C',
                              '4110D-Supports juridiques de programmes' = '4110D',
                              
                              '412-Construc bâtimnt résid & non résident' = '412',
                              '4120-Construc bâtimnt résid & non résident' = '4120',
                              '4120A-Construction de maisons individuelles' = '4120A',
                              '4120B-Construction autres bâtiments' = '4120B',
                              
                              '42-Génie civil' = '42',
                              
                              '421-Construction de route & de voies ferrées' = '421',
                              '4211-Construction de routes et autoroutes' = '4211',
                              '4211Z-Construction de routes et autoroutes' = '4211Z',
                              '4212-Const voie ferrée surface & souterraine' = '4212',
                              '4212Z-Const voie ferrée surface & souterraine' = '4212Z',
                              '4213-Construction de ponts et tunnels' = '4213',
                              '4213A-Construction ouvrages art' = '4213A',
                              '4213B-Construction et entretien de tunnels' = '4213B',
                              
                              '422-Construction de réseaux et de lignes' = '422',
                              '4221-Construction de réseaux pour fluides' = '4221',
                              '4221Z-Construction de réseaux pour fluides' = '4221Z',
                              '4222-Const réseaux électriq & de télécom' = '4222',
                              '4222Z-Const réseaux électriq & de télécom' = '4222Z',
                              
                              '429-Construc autres ouvrages de génie civil' = '429',
                              '4291-Construc ouvrages maritimes et fluviaux' = '4291',
                              '4291Z-Construc ouvrages maritimes et fluviaux' = '4291Z',
                              '4299-Constr aut ouvrage de génie civil nca' = '4299',
                              '4299Z-Constr aut ouvrage de génie civil nca' = '4299Z',
                              
                              '43-Travaux de construction spécialisés' = '43',
                              
                              '431-Démolition et préparation des sites' = '431',
                              '4311-Travaux de démolition' = '4311',
                              '4311Z-Travaux de démolition' = '4311Z',
                              '4312-Travaux de préparation des sites' = '4312',
                              '4312A-Travaux de terrassement courants' = '4312A',
                              '4312B-Travaux de terrassement spécialisés' = '4312B',
                              '4313-Forages et sondages' = '4313',
                              '4313Z-Forages et sondages' = '4313Z',
                              
                              '432-Travaux install élect plomberie & aut' = '432',
                              '4321-Installation électrique' = '4321',
                              '4321A-Travaux instal électriq ds tous locaux' = '4321A',
                              '4321B-Travaux instal électriq sr voie publi' = '4321B',
                              '4322-Trav plomberie chauffage & condit air' = '4322',
                              '4322A-Travaux instal eau & gaz en tous locaux' = '4322A',
                              '4322B-Travaux instal équipt thermique & clim' = '4322B',
                              '4329-Autres travaux installation' = '4329',
                              '4329A-Travaux isolation' = '4329A',
                              '4329B-Autres travaux installation nca' = '4329B',
                              
                              '433-Travaux de finition' = '433',
                              '4331-Travaux de plâtrerie' = '4331',
                              '4331Z-Travaux de plâtrerie' = '4331Z',
                              '4332-Travaux de menuiserie' = '4332',
                              '4332A-Travaux de menuiserie bois et PVC' = '4332A',
                              '4332B-Travaux menuiserie métal & serrurerie' = '4332B',
                              '4332C-Agencement de lieux de vente' = '4332C',
                              '4333-Travaux revêtement des sols et des murs' = '4333',
                              '4333Z-Travaux revêtement des sols et des murs' = '4333Z',
                              '4334-Travaux de peinture et vitrerie' = '4334',
                              '4334Z-Travaux de peinture et vitrerie' = '4334Z',
                              '4339-Autres travaux de finition' = '4339',
                              '4339Z-Autres travaux de finition' = '4339Z',
                              
                              '439-Autres travaux construction spécialisés' = '439',
                              '4391-Travaux de couverture' = '4391',
                              '4391A-Travaux de charpente' = '4391A',
                              '4391B-Travaux de couverture par éléments' = '4391B',
                              '4399-Aut travaux construc spécialisés nca' = '4399',
                              '4399A-Travaux étanchéification' = '4399A',
                              '4399B-Travaux montage de structure métallique' = '4399B',
                              '4399C-Trav maçon gle & gros oeuvre bâtiment' = '4399C',
                              '4399D-Aut travaux spécialisés de construction' = '4399D',
                              '4399E-Location avec opérateur mat de constr' = '4399E',
                              
                              'G-COMMERCE ; RÉPAR AUTOMOBILE & MOTOCYCLE' = 'G',
                              
                              '45-Commerce & répar automobile & motocycle' = '45',
                              
                              '451-Commerce de véhicules automobiles' = '451',
                              '4511-Comm de voiture & véhicule auto léger' = '4511',
                              '4511Z-Comm de voiture & véhicule auto léger' = '4511Z',
                              '4519-Commerce autres véhicules automobiles' = '4519',
                              '4519Z-Commerce autres véhicules automobiles' = '4519Z',
                              
                              '452-Entretien & répar de véhicule auto' = '452',
                              '4520-Entretien & répar de véhicule auto' = '4520',
                              '4520A-Entretien & répar véhicule auto léger' = '4520A',
                              '4520B-Entretien & répar autre véhicule auto' = '4520B',
                              
                              '453-Commerce équipements automobiles' = '453',
                              '4531-Commerce de gros équipement automobile' = '4531',
                              '4531Z-Commerce de gros équipement automobile' = '4531Z',
                              '4532-Commerce de détail équipement automobile' = '4532',
                              '4532Z-Commerce de détail équipement automobile' = '4532Z',
                              
                              '454-Commerce et réparation de motocycles' = '454',
                              '4540-Commerce et réparation de motocycles' = '4540',
                              '4540Z-Commerce et réparation de motocycles' = '4540Z',
                              
                              '46-Commerce gros hors auto & motocycle' = '46',
                              
                              '461-Intermédiaires du commerce de gros' = '461',
                              '4611-Interm du comm en produits agricoles' = '4611',
                              '4611Z-Interm du comm en produits agricoles' = '4611Z',
                              '4612-Int comm comb mét minér & pd chim' = '4612',
                              '4612A-Centrales achat de carburant' = '4612A',
                              '4612B-Aut ic comb mét minér & prod chim' = '4612B',
                              '4613-Interm comm bois & matériaux construc' = '4613',
                              '4613Z-Interm comm bois & matériaux construc' = '4613Z',
                              '4614-Int comm équipt indus, navire & avion' = '4614',
                              '4614Z-Int comm équipt indus, navire & avion' = '4614Z',
                              '4615-Int comm meuble, art ménage & quinc' = '4615',
                              '4615Z-Int comm meuble, art ménage & quinc' = '4615Z',
                              '4616-Int comm textile, habillt & assimil' = '4616',
                              '4616Z-Int comm textile, habillt & assimil' = '4616Z',
                              '4617-Int comm en denrées, boissons & tabac' = '4617',
                              '4617A-Centrales achat alimentaires' = '4617A',
                              '4617B-Autre ic en denrées, boissons et tabac' = '4617B',
                              '4618-Int spécialis comm aut prod spécif' = '4618',
                              '4618Z-Int spécialis comm aut prod spécif' = '4618Z',
                              '4619-Interm du commerce en produits divers' = '4619',
                              '4619A-Centrales achat non alimentaires' = '4619A',
                              '4619B-Autre interm commerce en prodts divers' = '4619B',
                              
                              '462-Com gros prod agric brut & anim viv' = '462',
                              '4621-Com gros céréal tab brt & alim bétail' = '4621',
                              '4621Z-Com gros céréal tab brt & alim bétail' = '4621Z',
                              '4622-Commerce de gros de fleurs et plantes' = '4622',
                              '4622Z-Commerce de gros de fleurs et plantes' = '4622Z',
                              '4623-Commerce de gros animaux vivants' = '4623',
                              '4623Z-Commerce de gros animaux vivants' = '4623Z',
                              '4624-Commerce de gros de cuirs et peaux' = '4624',
                              '4624Z-Commerce de gros de cuirs et peaux' = '4624Z',
                              
                              '463-Comm gros prod alim boisson & tabac' = '463',
                              '4631-Commerce de gros de fruits et légumes' = '4631',
                              '4631Z-Commerce de gros de fruits et légumes' = '4631Z',
                              '4632-Comm gros viande & prod à base  viande' = '4632',
                              '4632A-Commerce de gros de viandes de boucherie' = '4632A',
                              '4632B-Comm gros de produits à base de viande' = '4632B',
                              '4632C-Commerce de gros de volailles et gibier' = '4632C',
                              '4633-Com gros prod laitier oeuf & mat grse' = '4633',
                              '4633Z-Com gros prod laitier oeuf & mat grse' = '4633Z',
                              '4634-Commerce de gros de boissons' = '4634',
                              '4634Z-Commerce de gros  de boissons' = '4634Z',
                              '4635-Comm gros de produits à base de tabac' = '4635',
                              '4635Z-Comm gros de produits à base de tabac' = '4635Z',
                              '4636-Com gros de sucre chocolat & confiserie' = '4636',
                              '4636Z-Com gros de sucre chocolat & confiserie' = '4636Z',
                              '4637-Comm gros de café, thé, cacao et épices' = '4637',
                              '4637Z-Comm gros de café, thé, cacao et épices' = '4637Z',
                              '4638-Com gros aut alim yc poisson crustacé' = '4638',
                              '4638A-Com gros aut alim yc poisson crustacé' = '4638A',
                              '4638B-Comm gros alimentaire spécialisé divers' = '4638B',
                              '4639-Com gros non spéc denrée boisson tabac' = '4639',
                              '4639A-Commerce de gros de produits surgelés' = '4639A',
                              '4639B-Comm de gros alimentaire non spécialisé' = '4639B',
                              
                              '464-Commerce de gros de biens domestiques' = '464',
                              '4641-Commerce de gros de textiles' = '4641',
                              '4641Z-Commerce de gros de textiles' = '4641Z',
                              '4642-Commerce gros habillement & chaussures' = '4642',
                              '4642Z-Commerce gros habillement & chaussures' = '4642Z',
                              '4643-Commerce de gros appareil électroménager' = '4643',
                              '4643Z-Commerce de gros appareil électroménager' = '4643Z',
                              '4644-Com gros vaisselle verrerie prod entr' = '4644',
                              '4644Z-Com gros vaisselle verrerie prod entr' = '4644Z',
                              '4645-Com gros parfumerie & produit de beauté' = '4645',
                              '4645Z-Com gros parfumerie & produit de beauté' = '4645Z',
                              '4646-Comm gros de produits pharmaceutiques' = '4646',
                              '4646Z-Comm gros de produits pharmaceutiques' = '4646Z',
                              '4647-Com gros meuble tapis appareil éclaira' = '4647',
                              '4647Z-Com gros meuble tapis appareil éclaira' = '4647Z',
                              '4648-Com gros artic horlogerie & bijouterie' = '4648',
                              '4648Z-Com gros artic horlogerie & bijouterie' = '4648Z',
                              '4649-Commerce gros autres biens domestiques' = '4649',
                              '4649Z-Commerce gros autres biens domestiques' = '4649Z',
                              
                              '465-Comm gros équipt information & com' = '465',
                              '4651-Comm gros ordi éqpt périph & logiciel' = '4651',
                              '4651Z-Comm gros ordi éqpt périph & logiciel' = '4651Z',
                              '4652-Cg éqpt & composant électron & télécom' = '4652',
                              '4652Z-Cg éqpt & composant électron & télécom' = '4652Z',
                              
                              '466-Commerce de gros autres équipts indust' = '466',
                              '4661-Commerce de gros de matériel agricole' = '4661',
                              '4661Z-Commerce de gros de matériel agricole' = '4661Z',
                              '4662-Commerce de gros de machines-outils' = '4662',
                              '4662Z-Commerce de gros de machines-outils' = '4662Z',
                              '4663-Com gros machine pr extrac, constr GC' = '4663',
                              '4663Z-Com gros machine pr extrac, constr GC' = '4663Z',
                              '4664-Com gros machine pr ind text & habil' = '4664',
                              '4664Z-Com gros machine pr ind text & habil' = '4664Z',
                              '4665-Commerce de gros de mobilier de bureau' = '4665',
                              '4665Z-Commerce de gros de mobilier de bureau' = '4665Z',
                              '4666-Com gros autre machine & équipt bureau' = '4666',
                              '4666Z-Com gros autre machine & équipt bureau' = '4666Z',
                              '4669-Commerce de gros autre machine & équipt' = '4669',
                              '4669A-Commerce de gros de matériel électrique' = '4669A',
                              '4669B-Com gros fourniture & équipt ind div' = '4669B',
                              '4669C-Cg fournit & équipt div pr com & sces' = '4669C',
                              
                              '467-Autres commerces de gros spécialisés' = '467',
                              '4671-Com gros combustible & produits annexes' = '4671',
                              '4671Z-Com gros combustible & produits annexes' = '4671Z',
                              '4672-Commerce de gros de minerais et métaux' = '4672',
                              '4672Z-Commerce de gros de minerais et métaux' = '4672Z',
                              '4673-Cg bois, mat construc & app sanitaire' = '4673',
                              '4673A-Com gros bois & matériaux construction' = '4673A',
                              '4673B-Cg appareil sanitaire & prod décoration' = '4673B',
                              '4674-Cg quincail & fournit plombr & chauf' = '4674',
                              '4674A-Commerce de gros de quincaillerie' = '4674A',
                              '4674B-Cg fourniture pour plomberie & chauffage' = '4674B',
                              '4675-Commerce de gros de produits chimiques' = '4675',
                              '4675Z-Commerce de gros de produits chimiques' = '4675Z',
                              '4676-Commerce gros aut prod intermédiaire' = '4676',
                              '4676Z-Commerce gros aut prod intermédiaire' = '4676Z',
                              '4677-Commerce de gros de déchets et débris' = '4677',
                              '4677Z-Commerce de gros de déchets et débris' = '4677Z',
                              
                              '469-Commerce de gros non spécialisé' = '469',
                              '4690-Commerce de gros non spécialisé' = '4690',
                              '4690Z-Commerce de gros non spécialisé' = '4690Z',
                              
                              '47-Com détail, sf automobiles & motocycles' = '47',
                              
                              '471-Comm détail en magasin non spécialisé' = '471',
                              '4711-Comm dét mag non spéc à prédo alim' = '4711',
                              '4711A-Commerce de détail de produits surgelés' = '4711A',
                              '4711B-Commerce alimentation générale' = '4711B',
                              '4711C-Supérettes' = '4711C',
                              '4711D-Supermarchés' = '4711D',
                              '4711E-Magasins multi-commerces' = '4711E',
                              '4711F-Hypermarchés' = '4711F',
                              '4719-Autre comm détail en magasin non spéc' = '4719',
                              '4719A-Grands magasins' = '4719A',
                              '4719B-Autres comm détail en magasin non spéc' = '4719B',
                              
                              '472-Com dét aliment en magasin spécialisé' = '472',
                              '4721-Com détail fruit & légume en mag spéc' = '4721',
                              '4721Z-Com détail fruit & légume en mag spéc' = '4721Z',
                              '4722-Com dét viande & prdt avec viande (ms)' = '4722',
                              '4722Z-Com dét viande & prdt avec viande (ms)' = '4722Z',
                              '4723-Comm détail poisson crustacé etc (ms)' = '4723',
                              '4723Z-Comm détail poisson crustacé etc (ms)' = '4723Z',
                              '4724-Comm dét pain pâtiss & confiser (ms)' = '4724',
                              '4724Z-Comm dét pain pâtiss & confiser (ms)' = '4724Z',
                              '4725-Com détail boisson en magasin spéciali' = '4725',
                              '4725Z-Com détail boisson en magasin spéciali' = '4725Z',
                              '4726-Comm dét produit à base de tabac (ms)' = '4726',
                              '4726Z-Comm dét produit à base de tabac (ms)' = '4726Z',
                              '4729-Aut com détail alim en mag spéciali' = '4729',
                              '4729Z-Aut com détail alim en mag spéciali' = '4729Z',
                              
                              '473-Comm détail carburant en mag spéciali' = '473',
                              '4730-Comm détail carburant en mag spéciali' = '4730',
                              '4730Z-Comm détail carburant en mag spéciali' = '4730Z',
                              
                              '474-Com dét équipt information & com (ms)' = '474',
                              '4741-Com dét ordi un périph & logicl (ms)' = '4741',
                              '4741Z-Com dét ordi un périph & logicl (ms)' = '4741Z',
                              '4742-Comm dét matériel télécom (ms)' = '4742',
                              '4742Z-Comm dét matériel télécom (ms)' = '4742Z',
                              '4743-Comm dét matériels audio/vidéo (ms)   ' = '4743',
                              '4743Z-Comm dét matériels audio/vidéo (ms)   ' = '4743Z',
                              
                              '475-Com dét aut éqpt du foyer (mag spé)' = '475',
                              '4751-Com dét textiles en magasin spécialisé' = '4751',
                              '4751Z-Com dét textiles en magasin spécialisé' = '4751Z',
                              '4752-Comm dét quinca peinture & verre (ms)' = '4752',
                              '4752A-Com dét quinc pein etc (mag<400m2)' = '4752A',
                              '4752B-Com dét quinc pein etc (mag>400m2)' = '4752B',
                              '4753-Cd tapis moquette & revêt mur sol (ms)' = '4753',
                              '4753Z-Cd tapis moquette & revêt mur sol (ms)' = '4753Z',
                              '4754-Comm dét  appareil électroménager (ms)' = '4754',
                              '4754Z-Comm dét  appareil électroménager (ms)' = '4754Z',
                              '4759-Com dét meub éclair & aut art mén' = '4759',
                              '4759A-Commerce de détail de meubles' = '4759A',
                              '4759B-Comm détail autres équipements du foyer' = '4759B',
                              
                              '476-Comm dét biens culturels & loisir (ms)' = '476',
                              '4761-Comm dét livres en magasin spécialisé' = '4761',
                              '4761Z-Comm dét livres en magasin spécialisé' = '4761Z',
                              '4762-Comm détail journaux & papeterie (ms)' = '4762',
                              '4762Z-Comm détail journaux & papeterie (ms)' = '4762Z',
                              '4763-Comm dét enreg musicaux & vidéo (ms)' = '4763',
                              '4763Z-Com dét enreg musicaux & vidéo (ms)' = '4763Z',
                              '4764-Com dét articles de sport en mag spé' = '4764',
                              '4764Z-Com dét articles de sport en mag spé' = '4764Z',
                              '4765-Com dét jeux & jouets en mag spécial' = '4765',
                              '4765Z-Com dét jeux & jouets en mag spécial' = '4765Z',
                              
                              '477-Autre comm détail en magasin spécialisé' = '477',
                              '4771-Com dét habillement en mag spécialisé' = '4771',
                              '4771Z-Com dét habillement en mag spécialisé' = '4771Z',
                              '4772-Comm dét chaussure & article cuir (ms)' = '4772',
                              '4772A-Commerce de détail de la chaussure' = '4772A',
                              '4772B-Com dét maroquinerie & article  voyage' = '4772B',
                              '4773-Comm dét produits pharmaceutiques (ms)' = '4773',
                              '4773Z-Comm dét produits pharmaceutiques (ms)' = '4773Z',
                              '4774-Com dét art médicaux & orthopéd (ms)' = '4774',
                              '4774Z-Com dét art médicaux & orthopéd (ms)' = '4774Z',
                              '4775-Com dét parfumerie & prodt beauté (ms)' = '4775',
                              '4775Z-Com dét parfumerie & prodt beauté (ms)' = '4775Z',
                              '4776-Com dét fleur plante anim cie + alim' = '4776',
                              '4776Z-Com dét fleur plante anim cie + alim' = '4776Z',
                              '4777-Com dét art horlogerie & bijout (ms)' = '4777',
                              '4777Z-Com dét art horlogerie & bijout (ms)' = '4777Z',
                              '4778-Aut comm dét biens neufs en mag spé' = '4778',
                              '4778A-Commerces de détail optique' = '4778A',
                              '4778B-Comm détail de charbons & combustibles' = '4778B',
                              '4778C-Autre commerce détail spécialisé divers' = '4778C',
                              '4779-Comm détail biens occasion en magasin' = '4779',
                              '4779Z-Comm détail biens occasion en magasin' = '4779Z',
                              
                              '478-Commerce détail sur éventaires & marchés' = '478',
                              '4781-Cd alimentaire sur éventaire & marché' = '4781',
                              '4781Z-Cd alimentaire sur éventaire & marché' = '4781Z',
                              '4782-Cd textiles habillt & chauss s/marchés' = '4782',
                              '4782Z-Cd textiles habillt & chauss s/marchés' = '4782Z',
                              '4789-Aut com dét sur éventaires & marchés' = '4789',
                              '4789Z-Aut com dét sur éventaires & marchés' = '4789Z',
                              
                              '479-Com dét hors mag éventair ou marchés' = '479',
                              '4791-Vente à distance' = '4791',
                              '4791A-Vente à distance sur catalogue général' = '4791A',
                              '4791B-Vente à distance sur catalogue spécialis' = '4791B',
                              '4799-Aut comm dét hors mag évent, marché' = '4799',
                              '4799A-Vente à domicile' = '4799A',
                              '4799B-Vente par automate, aut cd hors magasin' = '4799B',
                              
                              'H-TRANSPORTS ET ENTREPOSAGE' = 'H',
                              
                              '49-Transport terrest & trans par conduite' = '49',
                              
                              '491-Transport ferrov interurbain voyageur' = '491',
                              '4910-Transport ferrov interurbain voyageur' = '4910',
                              '4910Z-Transport ferrov interurbain voyageur' = '4910Z',
                              
                              '492-Transports ferroviaires de fret' = '492',
                              '4920-Transports ferroviaires de fret' = '4920',
                              '4920Z-Transports ferroviaires de fret' = '4920Z',
                              
                              '493-Autres transports terrestres de voyageur' = '493',
                              '4931-Transport urbain & suburbain de voyageur' = '4931',
                              '4931Z-Transport urbain & suburbain de voyageur' = '4931Z',
                              '4932-Transports de voyageurs par taxis' = '4932',
                              '4932Z-Transports de voyageurs par taxis' = '4932Z',
                              '4939-Aut transport terrestre voyageurs nca' = '4939',
                              '4939A-Transport routier régulier de voyageurs' = '4939A',
                              '4939B-Autres transports routiers de voyageurs' = '4939B',
                              '4939C-Téléphériques et remontées mécaniques' = '4939C',
                              
                              '494-Transport routier de fret & sce déménagt' = '494',
                              '4941-Transports routiers de fret' = '4941',
                              '4941A-Transports routiers de fret interurbains' = '4941A',
                              '4941B-Transports routiers de fret de proximité' = '4941B',
                              '4941C-Location de camions avec chauffeur' = '4941C',
                              '4942-Services de déménagement' = '4942',
                              '4942Z-Services de déménagement' = '4942Z',
                              
                              '495-Transports par conduites' = '495',
                              '4950-Transports par conduites' = '4950',
                              '4950Z-Transports par conduites' = '4950Z',
                              
                              '50-Transports par eau' = '50',
                              
                              '501-Transport maritime & côtier de passagers' = '501',
                              '5010-Transport maritime & côtier de passagers' = '5010',
                              '5010Z-Transport maritime & côtier de passagers' = '5010Z',
                              
                              '502-Transports maritimes et côtiers de fret' = '502',
                              '5020-Transports maritimes et côtiers de fret' = '5020',
                              '5020Z-Transports maritimes et côtiers de fret' = '5020Z',
                              
                              '503-Transports fluviaux de passagers' = '503',
                              '5030-Transports fluviaux de passagers' = '5030',
                              '5030Z-Transports fluviaux de passagers' = '5030Z',
                              
                              '504-Transports fluviaux de fret' = '504',
                              '5040-Transports fluviaux de fret' = '5040',
                              '5040Z-Transports fluviaux de fret' = '5040Z',
                              
                              '51-Transports aériens' = '51',
                              
                              '511-Transports aériens de passagers' = '511',
                              '5110-Transports aériens de passagers' = '5110',
                              '5110Z-Transports aériens de passagers' = '5110Z',
                              
                              '512-Transp aérien de fret & trans spatiaux' = '512',
                              '5121-Transports aériens de fret' = '5121',
                              '5121Z-Transports aériens de fret' = '5121Z',
                              '5122-Transports spatiaux' = '5122',
                              '5122Z-Transports spatiaux' = '5122Z',
                              
                              '52-Entreposage & sce auxiliaire des transp' = '52',
                              
                              '521-Entreposage et stockage' = '521',
                              '5210-Entreposage et stockage' = '5210',
                              '5210A-Entreposage et stockage frigorifique' = '5210A',
                              '5210B-Entreposage et stockage non frigorifique' = '5210B',
                              
                              '522-Services auxiliaires des transports' = '522',
                              '5221-Sces auxiliaires de transport terrestre' = '5221',
                              '5221Z-Sces auxiliaires de transport terrestre' = '5221Z',
                              '5222-Sces auxiliaires des transports par eau' = '5222',
                              '5222Z-Sces auxiliaires des transports par eau' = '5222Z',
                              '5223-Sces auxiliaires des transports aériens' = '5223',
                              '5223Z-Sces auxiliaires des transports aériens' = '5223Z',
                              '5224-Manutention' = '5224',
                              '5224A-Manutention portuaire' = '5224A',
                              '5224B-Manutention non portuaire' = '5224B',
                              '5229-Autre service auxiliaires des transports' = '5229',
                              '5229A-Messagerie, fret express' = '5229A',
                              '5229B-Affrètement & organisation des transp' = '5229B',
                              
                              '53-Activités de poste et de courrier' = '53',
                              
                              '531-Activ poste (obligation sce universel)' = '531',
                              '5310-Activ poste (obligation sce universel)' = '5310',
                              '5310Z-Activ poste (obligation sce universel)' = '5310Z',
                              
                              '532-Autres activités de poste et de courrier' = '532',
                              '5320-Autres activités de poste et de courrier' = '5320',
                              '5320Z-Autres activités de poste et de courrier' = '5320Z',
                              
                              'I-HÉBERGEMENT ET RESTAURATION' = 'I',
                              
                              '55-Hébergement' = '55',
                              
                              '551-Hôtels et hébergement similaire' = '551',
                              '5510-Hôtels et hébergement similaire' = '5510',
                              '5510Z-Hôtels et hébergement similaire' = '5510Z',
                              
                              '552-Hébergt tourist & aut hbt courte durée' = '552',
                              '5520-Hébergt tourist & aut hbt courte durée' = '5520',
                              '5520Z-Hébergt tourist & aut hbt courte durée' = '5520Z',
                              
                              '553-Terrain camping & parc pr caravane etc' = '553',
                              '5530-Terrain camping & parc pr caravane etc' = '5530',
                              '5530Z-Terrain camping & parc pr caravane etc' = '5530Z',
                              
                              '559-Autres hébergements' = '559',
                              '5590-Autres hébergements' = '5590',
                              '5590Z-Autres hébergements' = '5590Z',
                              
                              '56-Restauration' = '56',
                              
                              '561-Restaurant & serv de restaurat mobile' = '561',
                              '5610-Restaurant & serv de restaurat mobile' = '5610',
                              '5610A-Restauration traditionnelle' = '5610A',
                              '5610B-Cafétérias et autres libres-services' = '5610B',
                              '5610C-Restauration de type rapide' = '5610C',
                              
                              '562-Traiteur & autre service de restauration' = '562',
                              '5621-Services des traiteurs' = '5621',
                              '5621Z-Services des traiteurs' = '5621Z',
                              '5629-Autres services de restauration' = '5629',
                              '5629A-Restauration collective sous contrat' = '5629A',
                              '5629B-Autres services de restauration nca' = '5629B',
                              
                              '563-Débits de boissons' = '563',
                              '5630-Débits de boissons' = '5630',
                              '5630Z-Débits de boissons' = '5630Z',
                              
                              'J-INFORMATION ET COMMUNICATION' = 'J',
                              
                              '58-Édition' = '58',
                              
                              '581-Édition livre & périodiq & aut édition' = '581',
                              '5811-Édition de livres' = '5811',
                              '5811Z-Édition de livres' = '5811Z',
                              '5812-Édition répertoires & fichiers adresse' = '5812',
                              '5812Z-Édition répertoires & fichiers adresse' = '5812Z',
                              '5813-Édition de journaux' = '5813',
                              '5813Z-Édition de journaux' = '5813Z',
                              '5814-Édition de revues et périodiques' = '5814',
                              '5814Z-Édition de revues et périodiques' = '5814Z',
                              '5819-Autres activités édition' = '5819',
                              '5819Z-Autres activités édition' = '5819Z',
                              
                              '582-Édition de logiciels' = '582',
                              '5821-Édition de jeux électroniques' = '5821',
                              '5821Z-Édition de jeux électroniques' = '5821Z',
                              '5829-Édition autres logiciels' = '5829',
                              '5829A-Édition de logiciel système et de réseau' = '5829A',
                              '5829B-Edit logiciel outil dévelop & langage' = '5829B',
                              '5829C-Edition de logiciels applicatifs' = '5829C',
                              
                              '59-Prod films ; enr sonore & éd musicale' = '59',
                              
                              '591-Activ cinématogra, vidéo & télévision' = '591',
                              '5911-Prod film cinémato vidéo & pgm télé' = '5911',
                              '5911A-Prod film & progm pour la télévision' = '5911A',
                              '5911B-Prod film institutionnel & publicitaire' = '5911B',
                              '5911C-Production de films pour le cinéma' = '5911C',
                              '5912-Post-production film & prog télévision' = '5912',
                              '5912Z-Post-production film & prog télévision' = '5912Z',
                              '5913-Distribution film vidéo & pgm télé' = '5913',
                              '5913A-Distribution de films cinématographiques' = '5913A',
                              '5913B-Edition et distribution vidéo' = '5913B',
                              '5914-Projection de films cinématographiques' = '5914',
                              '5914Z-Projection de films cinématographiques' = '5914Z',
                              
                              '592-Enregistrement sonore & édition musicale' = '592',
                              '5920-Enregistrement sonore & édition musicale' = '5920',
                              '5920Z-Enregistrement sonore & édition musicale' = '5920Z',
                              
                              '60-Programmation et diffusion' = '60',
                              
                              '601-Édition et diffusion de programmes radio' = '601',
                              '6010-Édition et diffusion de programmes radio' = '6010',
                              '6010Z-Édition et diffusion de programmes radio' = '6010Z',
                              
                              '602-Programmation de télévision & télédiff' = '602',
                              '6020-Programmation de télévision & télédiff' = '6020',
                              '6020A-Edition de chaînes généralistes' = '6020A',
                              '6020B-Edition de chaînes thématiques' = '6020B',
                              
                              '61-Télécommunications' = '61',
                              
                              '611-Télécommunications filaires' = '611',
                              '6110-Télécommunications filaires' = '6110',
                              '6110Z-Télécommunications filaires' = '6110Z',
                              
                              '612-Télécommunications sans fil' = '612',
                              '6120-Télécommunications sans fil' = '6120',
                              '6120Z-Télécommunications sans fil' = '6120Z',
                              
                              '613-Télécommunications par satellite' = '613',
                              '6130-Télécommunications par satellite' = '6130',
                              '6130Z-Télécommunications par satellite' = '6130Z',
                              
                              '619-Autres activités de télécommunication' = '619',
                              '6190-Autres activités de télécommunication' = '6190',
                              '6190Z-Autres activités de télécommunication' = '6190Z',
                              
                              '62-Pgmtion conseil & aut act informatique' = '62',
                              
                              '620-Pgmtion conseil & aut act informatique' = '620',
                              '6201-Programmation informatique' = '6201',
                              '6201Z-Programmation informatique' = '6201Z',
                              '6202-Conseil informatique' = '6202',
                              '6202A-Conseil en système & logiciel informati' = '6202A',
                              '6202B-Tierce mainten syst & appli nformati' = '6202B',
                              '6203-Gestion installations informatiques' = '6203',
                              '6203Z-Gestion installations informatiques' = '6203Z',
                              '6209-Autres activités informatiques' = '6209',
                              '6209Z-Autres activités informatiques' = '6209Z',
                              
                              '63-Services information' = '63',
                              
                              '631-Trt donnée, hébrgt etc; portail Internt' = '631',
                              '6311-Traitt donnée, hébergt & activ connexe' = '6311',
                              '6311Z-Traitt donnée, hébergt & activ connexe' = '6311Z',
                              '6312-Portails Internet' = '6312',
                              '6312Z-Portails Internet' = '6312Z',
                              
                              '639-Autres services information' = '639',
                              '6391-Activités des agences de presse' = '6391',
                              '6391Z-Activités des agences de presse' = '6391Z',
                              '6399-Autres services information nca' = '6399',
                              '6399Z-Autres services information nca' = '6399Z',
                              
                              'K-ACTIVITÉS FINANCIÈRES ET ASSURANCE' = 'K',
                              
                              '64-Act financ hs assur & cais retraite' = '64',
                              
                              '641-Intermédiation monétaire' = '641',
                              '6411-Activités de banque centrale' = '6411',
                              '6411Z-Activités de banque centrale' = '6411Z',
                              '6419-Autres intermédiations monétaires' = '6419',
                              '6419Z-Autres intermédiations monétaires' = '6419Z',
                              
                              '642-Activités des sociétés holding' = '642',
                              '6420-Activités des sociétés holding' = '6420',
                              '6420Z-Activités des sociétés holding' = '6420Z',
                              
                              '643-Fonds placement & entité financ simil' = '643',
                              '6430-Fonds placement & entité financ simil' = '6430',
                              '6430Z-Fonds placement & entité financ simil' = '6430Z',
                              
                              '649-Aut act finan hs assur & c retrait' = '649',
                              '6491-Crédit-bail' = '6491',
                              '6491Z-Crédit-bail' = '6491Z',
                              '6492-Autre distribution de crédit' = '6492',
                              '6492Z-Autre distribution de crédit' = '6492Z',
                              '6499-Aut act finan hs as & c retra nca' = '6499',
                              '6499Z-Aut act finan hs as & c retra nca' = '6499Z',
                              
                              '65-Assurance' = '65',
                              
                              '651-Assurance' = '651',
                              '6511-Assurance vie' = '6511',
                              '6511Z-Assurance vie' = '6511Z',
                              '6512-Autres assurances' = '6512',
                              '6512Z-Autres assurances' = '6512Z',
                              
                              '652-Réassurance' = '652',
                              '6520-Réassurance' = '6520',
                              '6520Z-Réassurance' = '6520Z',
                              
                              '653-Caisses de retraite' = '653',
                              '6530-Caisses de retraite' = '6530',
                              '6530Z-Caisses de retraite' = '6530Z',
                              
                              '66-Act auxiliaire sces financ & assur' = '66',
                              
                              '661-Act aux sce financ hs ass & retraite' = '661',
                              '6611-Administration de marchés financiers' = '6611',
                              '6611Z-Administration de marchés financiers' = '6611Z',
                              '6612-Courtage valeur mobilière & marchandise' = '6612',
                              '6612Z-Courtage valeur mobilière & marchandise' = '6612Z',
                              '6619-Aut act aux sce fin, hs ass & retr' = '6619',
                              '6619A-Support juridiq gest patrimoine mobil' = '6619A',
                              '6619B-Aut aux sce financ hs ass retr nca' = '6619B',
                              
                              '662-Act auxil assurance & caisse retraite' = '662',
                              '6621-Évaluation des risques et dommages' = '6621',
                              '6621Z-Évaluation des risques et dommages' = '6621Z',
                              '6622-Act des agents & courtiers assurances' = '6622',
                              '6622Z-Act des agents & courtiers assurances' = '6622Z',
                              '6629-Aut act aux assur & caisse retraite' = '6629',
                              '6629Z-Aut act aux assur & caisse retraite' = '6629Z',
                              
                              '663-Gestion de fonds' = '663',
                              '6630-Gestion de fonds' = '6630',
                              '6630Z-Gestion de fonds' = '6630Z',
                              
                              'L-ACTIVITÉS IMMOBILIÈRES' = 'L',
                              
                              '68-Activités immobilières' = '68',
                              
                              '681-Activité marchands de biens immobiliers' = '681',
                              '6810-Activité marchands de biens immobiliers' = '6810',
                              '6810Z-Activité marchands de biens immobiliers' = '6810Z',
                              
                              '682-Loc & exploi bien immo propre ou loué' = '682',
                              '6820-Loc & exploi bien immo propre ou loué' = '6820',
                              '6820A-Location de logements' = '6820A',
                              '6820B-Location terrain & autre bien immobilier' = '6820B',
                              
                              '683-Activité immobilière pour cpte de tiers' = '683',
                              '6831-Agences immobilières' = '6831',
                              '6831Z-Agences immobilières' = '6831Z',
                              '6832-Administration de biens immobiliers' = '6832',
                              '6832A-Administrat immeuble & autre bien immo' = '6832A',
                              '6832B-Support juridi gestion patrimoine immo' = '6832B',
                              
                              'M-ACT SPÉCIALISÉE, SCIENTIFIQ & TECHNIQ' = 'M',
                              
                              '69-Activités juridiques et comptables' = '69',
                              
                              '691-Activités juridiques' = '691',
                              '6910-Activités juridiques' = '6910',
                              '6910Z-Activités juridiques' = '6910Z',
                              
                              '692-Activités comptables' = '692',
                              '6920-Activités comptables' = '6920',
                              '6920Z-Activités comptables' = '6920Z',
                              
                              '70-Act sièges sociaux ; conseil de gestion' = '70',
                              
                              '701-Activités des sièges sociaux' = '701',
                              '7010-Activités des sièges sociaux' = '7010',
                              '7010Z-Activités des sièges sociaux' = '7010Z',
                              
                              '702-Conseil de gestion' = '702',
                              '7021-Conseil en relation publique & communic' = '7021',
                              '7021Z-Conseil en relation publique & communic' = '7021Z',
                              '7022-Conseil pr affaire & aut cons gestion' = '7022',
                              '7022Z-Conseil pr affaire & aut cons gestion' = '7022Z',
                              
                              '71-Architec & ingénierie; ctrle ana tech' = '71',
                              
                              '711-Activités architecture et ingénierie' = '711',
                              '7111-Activités architecture' = '7111',
                              '7111Z-Activités architecture' = '7111Z',
                              '7112-Activités ingénierie' = '7112',
                              '7112A-Activité des géomètres' = '7112A',
                              '7112B-Ingénierie, études techniques' = '7112B',
                              
                              '712-Activité de contrôle & analyse technique' = '712',
                              '7120-Activité de contrôle & analyse technique' = '7120',
                              '7120A-Contrôle technique automobile' = '7120A',
                              '7120B-Analyses, essais & inspection technique' = '7120B',
                              
                              '72-Recherche-développement scientifique' = '72',
                              
                              '721-R&D en sciences physiques et naturelles' = '721',
                              '7211-Recherche-développemnt en biotechnologie' = '7211',
                              '7211Z-Recherche-développemnt en biotechnologie' = '7211Z',
                              '7219-R&D : aut sciences physique & naturelle' = '7219',
                              '7219Z-R&D : aut sciences physique & naturelle' = '7219Z',
                              
                              '722-R&D en sciences humaines et sociales' = '722',
                              '7220-R&D en sciences humaines et sociales' = '7220',
                              '7220Z-R&D en sciences humaines et sociales' = '7220Z',
                              
                              '73-Publicité et études de marché' = '73',
                              
                              '731-Publicité' = '731',
                              '7311-Activités des agences de publicité' = '7311',
                              '7311Z-Activités des agences de publicité' = '7311Z',
                              '7312-Régie publicitaire de médias' = '7312',
                              '7312Z-Régie publicitaire de médias' = '7312Z',
                              
                              '732-Études de marché et sondages' = '732',
                              '7320-Études de marché et sondages' = '7320',
                              '7320Z-Études de marché et sondages' = '7320Z',
                              
                              '74-Aut act spécial scientifique & techn' = '74',
                              
                              '741-Activités spécialisées de design' = '741',
                              '7410-Activités spécialisées de design' = '7410',
                              '7410Z-Activités spécialisées de design' = '7410Z',
                              
                              '742-Activités photographiques' = '742',
                              '7420-Activités photographiques' = '7420',
                              '7420Z-Activités photographiques' = '7420Z',
                              
                              '743-Traduction et interprétation' = '743',
                              '7430-Traduction et interprétation' = '7430',
                              '7430Z-Traduction et interprétation' = '7430Z',
                              
                              '749-Aut act spéc scientif & techn nca' = '749',
                              '7490-Aut act spéc scientif & techn nca' = '7490',
                              '7490A-Activ des économistes de la construction' = '7490A',
                              '7490B-Act spéc scientif & techniq diverses' = '7490B',
                              
                              '75-Activités vétérinaires' = '75',
                              
                              '750-Activités vétérinaires' = '750',
                              '7500-Activités vétérinaires' = '7500',
                              '7500Z-Activités vétérinaires' = '7500Z',
                              
                              'N-ACTIVITÉS DE SCE ADMINISTR & DE SOUTIEN' = 'N',
                              
                              '77-Activités de location et location-bail' = '77',
                              
                              '771-Loc & loc-bail de véhicule automobile' = '771',
                              '7711-Loc & loc-bail voit & v auto léger' = '7711',
                              '7711A-Loc courte durée voit & v auto léger' = '7711A',
                              '7711B-Loc longue durée voit & v auto léger' = '7711B',
                              '7712-Location et location-bail de camions' = '7712',
                              '7712Z-Location et location-bail de camions' = '7712Z',
                              
                              '772-Loc & loc-bail bien perso & domestiq' = '772',
                              '7721-Loc & loc-bail article loisir & sport' = '7721',
                              '7721Z-Loc & loc-bail article loisir & sport' = '7721Z',
                              '7722-Location de vidéocassette & disque vidéo' = '7722',
                              '7722Z-Location de vidéocassette & disque vidéo' = '7722Z',
                              '7729-Loc & loc-bail aut bien perso & dom' = '7729',
                              '7729Z-Loc & loc-bail aut bien perso & dom' = '7729Z',
                              
                              '773-Loc & loc-bail aut mach, éqpt & bien' = '773',
                              '7731-Loc & loc-bail machine & éqpt agricole' = '7731',
                              '7731Z-Loc & loc-bail machine & éqpt agricole' = '7731Z',
                              '7732-Loc & loc-bail mach & éqpt pr constr' = '7732',
                              '7732Z-Loc & loc-bail mach & éqpt pr constr' = '7732Z',
                              '7733-Loc & loc-bail mach bur & mat info' = '7733',
                              '7733Z-Loc & loc-bail mach bur & mat info' = '7733Z',
                              '7734-Loc & loc-bail mat transport par eau' = '7734',
                              '7734Z-Loc & loc-bail mat transport par eau' = '7734Z',
                              '7735-Loc & loc-bail mat transport aérien' = '7735',
                              '7735Z-Loc & loc-bail mat transport aérien' = '7735Z',
                              '7739-Loc & loc-bail machi éqpt & bien div' = '7739',
                              '7739Z-Loc & loc-bail mach, éqpt & bien div' = '7739Z',
                              
                              '774-Loc-bail propr intel, sf oeuvre avec ©' = '774',
                              '7740-Loc-bail propr intel, sf oeuvre avec ©' = '7740',
                              '7740Z-Loc-bail propr intel, sf oeuvre avec ©' = '7740Z',
                              
                              '78-Activités liées à emploi' = '78',
                              
                              '781-Activ agence placement de main-d oeuvre' = '781',
                              '7810-Activ agence placement de main-d oeuvre' = '7810',
                              '7810Z-Activ agence placement de main-d oeuvre' = '7810Z',
                              
                              '782-Activ des agences de travail temporaire' = '782',
                              '7820-Activ des agences de travail temporaire' = '7820',
                              '7820Z-Activ des agences de travail temporaire' = '7820Z',
                              
                              '783-Aut mise à dispo de ressource humaine' = '783',
                              '7830-Aut mise à dispo de ressource humaine' = '7830',
                              '7830Z-Aut mise à dispo de ressource humaine' = '7830Z',
                              
                              '79-Act ag voyage voyagiste sv résa etc' = '79',
                              
                              '791-Activités agences de voyage & voyagistes' = '791',
                              '7911-Activités des agences de voyage' = '7911',
                              '7911Z-Activités des agences de voyage' = '7911Z',
                              '7912-Activités des voyagistes' = '7912',
                              '7912Z-Activités des voyagistes' = '7912Z',
                              
                              '799-Autre serv réservation & activ connexe' = '799',
                              '7990-Autre serv réservation & activ connexe' = '7990',
                              '7990Z-Autre serv réservation & activ connexe' = '7990Z',
                              
                              '80-Enquêtes et sécurité' = '80',
                              
                              '801-Activités de sécurité privée' = '801',
                              '8010-Activités de sécurité privée' = '8010',
                              '8010Z-Activités de sécurité privée' = '8010Z',
                              
                              '802-Activités liées aux systèmes de sécurité' = '802',
                              '8020-Activités liées aux systèmes de sécurité' = '8020',
                              '8020Z-Activités liées aux systèmes de sécurité' = '8020Z',
                              
                              '803-Activités enquête' = '803',
                              '8030-Activités enquête' = '8030',
                              '8030Z-Activités enquête' = '8030Z',
                              
                              '81-Sces relatifs bâtimnt & aménagt paysager' = '81',
                              
                              '811-Act combinée soutien lié aux bâtiments' = '811',
                              '8110-Act combinée soutien lié aux bâtiments' = '8110',
                              '8110Z-Act combinée soutien lié aux bâtiments' = '8110Z',
                              
                              '812-Activités de nettoyage' = '812',
                              '8121-Nettoyage courant des bâtiments' = '8121',
                              '8121Z-Nettoyage courant des bâtiments' = '8121Z',
                              '8122-Aut act nettoyage bâtim & nett ind' = '8122',
                              '8122Z-Aut act nettoyage bâtim & nett ind' = '8122Z',
                              '8129-Autres activités de nettoyage' = '8129',
                              '8129A-Désinfection désinsectisatn dératisation' = '8129A',
                              '8129B-Autres activités de nettoyage nca' = '8129B',
                              
                              '813-Services aménagement paysager' = '813',
                              '8130-Services aménagement paysager' = '8130',
                              '8130Z-Services aménagement paysager' = '8130Z',
                              
                              '82-Act admin & aut act soutien aux ent' = '82',
                              
                              '821-Activités administratives' = '821',
                              '8211-Services admin combinés de bureau' = '8211',
                              '8211Z-Services admin combinés de bureau' = '8211Z',
                              '8219-Photocopie & aut act spé sout bureau' = '8219',
                              '8219Z-Photocopie & aut act spé sout bureau' = '8219Z',
                              
                              '822-Activités de centres appels' = '822',
                              '8220-Activités de centres appels' = '8220',
                              '8220Z-Activités de centres appels' = '8220Z',
                              
                              '823-Organisation salon profession & congrès' = '823',
                              '8230-Organisation salon profession & congrès' = '8230',
                              '8230Z-Organisation salon profession & congrès' = '8230Z',
                              
                              '829-Activités soutien aux entreprises nca' = '829',
                              '8291-Act recouv fac & info fin s/client' = '8291',
                              '8291Z-Act recouv fac & info fin s/client' = '8291Z',
                              '8292-Activités de conditionnement' = '8292',
                              '8292Z-Activités de conditionnement' = '8292Z',
                              '8299-Autre activité de soutien aux entr nca' = '8299',
                              '8299Z-Autre activité de soutien aux entr nca' = '8299Z',
                              
                              'O-ADMINISTRATION PUBLIQUE' = 'O',
                              
                              '84-Admin publi & défense; séc soc obli' = '84',
                              
                              '841-Admin générale, économique & sociale' = '841',
                              '8411-Administration publique générale' = '8411',
                              '8411Z-Administration publique générale' = '8411Z',
                              '8412-A p santé form cult & soc (sf sécu)' = '8412',
                              '8412Z-A p santé form cult & soc (sf sécu)' = '8412Z',
                              '8413-Adm publique des activités économiques' = '8413',
                              '8413Z-Adm publique des activités économiques' = '8413Z',
                              
                              '842-Services de prérogative publique' = '842',
                              '8421-Affaires étrangères' = '8421',
                              '8421Z-Affaires étrangères' = '8421Z',
                              '8422-Défense' = '8422',
                              '8422Z-Défense' = '8422Z',
                              '8423-Justice' = '8423',
                              '8423Z-Justice' = '8423Z',
                              '8424-Activités d’ordre public et de sécurité' = '8424',
                              '8424Z-Activités d’ordre public et de sécurité' = '8424Z',
                              '8425-Services du feu et de secours' = '8425',
                              '8425Z-Services du feu et de secours' = '8425Z',
                              
                              '843-Sécurité sociale obligatoire' = '843',
                              '8430-Sécurité sociale obligatoire' = '8430',
                              '8430A-Activités générales de sécurité sociale' = '8430A',
                              '8430B-Gestion des retraites complémentaires' = '8430B',
                              '8430C-Distribution sociale de revenus' = '8430C',
                              
                              'P-ENSEIGNEMENT' = 'P',
                              
                              '85-Enseignement' = '85',
                              
                              '851-Enseignement pré-primaire' = '851',
                              '8510-Enseignement pré-primaire' = '8510',
                              '8510Z-Enseignement pré-primaire' = '8510Z',
                              
                              '852-Enseignement primaire' = '852',
                              '8520-Enseignement primaire' = '8520',
                              '8520Z-Enseignement primaire' = '8520Z',
                              
                              '853-Enseignement secondaire' = '853',
                              '8531-Enseignement secondaire général' = '8531',
                              '8531Z-Enseignement secondaire général' = '8531Z',
                              '8532-Enseignemt secondaire techn ou profess' = '8532',
                              '8532Z-Enseignemt secondaire techn ou profess' = '8532Z',
                              
                              '854-Enseigmnt sup & post-second non sup' = '854',
                              '8541-Enseignement post-secondaire non sup' = '8541',
                              '8541Z-Enseignement post-secondaire non sup' = '8541Z',
                              '8542-Enseignement supérieur' = '8542',
                              '8542Z-Enseignement supérieur' = '8542Z',
                              
                              '855-Autres activités enseignement' = '855',
                              '8551-Enseigmnt discipl sport & act loisir' = '8551',
                              '8551Z-Enseigmnt discipl sport & act loisir' = '8551Z',
                              '8552-Enseignement culturel' = '8552',
                              '8552Z-Enseignement culturel' = '8552Z',
                              '8553-Enseignement de la conduite' = '8553',
                              '8553Z-Enseignement de la conduite' = '8553Z',
                              '8559-Enseignements divers' = '8559',
                              '8559A-Formation continue adultes' = '8559A',
                              '8559B-Autres enseignements' = '8559B',
                              
                              '856-Activités de soutien à enseignement' = '856',
                              '8560-Activités de soutien à enseignement' = '8560',
                              '8560Z-Activités de soutien à enseignement' = '8560Z',
                              
                              'Q-SANTÉ HUMAINE ET ACTION SOCIALE' = 'Q',
                              
                              '86-Activités pour la santé humaine' = '86',
                              
                              '861-Activités hospitalières' = '861',
                              '8610-Activités hospitalières' = '8610',
                              '8610Z-Activités hospitalières' = '8610Z',
                              
                              '862-Activité des médecins et des dentistes' = '862',
                              '8621-Activité des médecins généralistes' = '8621',
                              '8621Z-Activité des médecins généralistes' = '8621Z',
                              '8622-Activité des médecins spécialistes' = '8622',
                              '8622A-Act radiodiagnostic et de radiothérapie' = '8622A',
                              '8622B-Activités chirurgicales' = '8622B',
                              '8622C-Autre activité des médecins spécialistes' = '8622C',
                              '8623-Pratique dentaire' = '8623',
                              '8623Z-Pratique dentaire' = '8623Z',
                              
                              '869-Autres activités pour la santé humaine' = '869',
                              '8690-Autres activités pour la santé humaine' = '8690',
                              '8690A-Ambulances' = '8690A',
                              '8690B-Laboratoires analyses médicales' = '8690B',
                              '8690C-Centres de collecte et banques organes' = '8690C',
                              '8690D-Act des infirmiers et des sages-femmes' = '8690D',
                              '8690E-Act rééduc appareillag & pédic-podo' = '8690E',
                              '8690F-Activités de santé humaine nca' = '8690F',
                              
                              '87-Hébergement médico-social et social' = '87',
                              
                              '871-Hébergement médicalisé' = '871',
                              '8710-Hébergement médicalisé' = '8710',
                              '8710A-Hébergt médicalisé pour personnes âgées' = '8710A',
                              '8710B-Hébergt médicalisé pr enfants handicapés' = '8710B',
                              '8710C-Hébrgt médic adul hand & aut ht méd' = '8710C',
                              
                              '872-Hébrgt soc hand, mal mental & toxico' = '872',
                              '8720-Hébrgt soc hand, mal mental & toxico' = '8720',
                              '8720A-Hébrgt soc hand mental & malade mental' = '8720A',
                              '8720B-Hébergement social pour toxicomanes' = '8720B',
                              
                              '873-Hébgt soc perso âgées, hand physiques' = '873',
                              '8730-Hébgt soc perso âgées, hand physiques' = '8730',
                              '8730A-Hébergement social pour personnes âgées' = '8730A',
                              '8730B-Hébergt social pour handicapés physiques' = '8730B',
                              
                              '879-Autres activités d’hébergement social' = '879',
                              '8790-Autres activités d’hébergement social' = '8790',
                              '8790A-Hébergt social pr enfants en difficultés' = '8790A',
                              '8790B-Hébgt soc adult, famille en difficulté' = '8790B',
                              
                              '88-Action sociale sans hébergement' = '88',
                              
                              '881-Action soc sans hbgt, per âgée & hand' = '881',
                              '8810-Action soc sans hbgt, per âgée & hand' = '8810',
                              '8810A-Aide à domicile' = '8810A',
                              '8810B-Accueil ss hbgt adult hand, pers âgée' = '8810B',
                              '8810C-Aide par le travail' = '8810C',
                              
                              '889-Autre action sociale sans hébergement' = '889',
                              '8891-Action soc sans hbrgt pr jeunes enfants' = '8891',
                              '8891A-Accueil de jeunes enfants' = '8891A',
                              '8891B-Accueil sans hébergt enfant handicap' = '8891B',
                              '8899-Autre action sociale sans hébrgemnt nca' = '8899',
                              '8899A-Aut accueil sans hébrgt enfants & ado' = '8899A',
                              '8899B-Action sociale sans hébergement nca' = '8899B',
                              
                              'R-ARTS, SPECTACLES & ACTIVITÉS RÉCRÉATIVES' = 'R',
                              
                              '90-Act créativ artistiques & de spectacle' = '90',
                              
                              '900-Act créativ artistiques & de spectacle' = '900',
                              '9001-Arts du spectacle vivant' = '9001',
                              '9001Z-Arts du spectacle vivant' = '9001Z',
                              '9002-Activités de soutien au spectacle vivant' = '9002',
                              '9002Z-Activités de soutien au spectacle vivant' = '9002Z',
                              '9003-Création artistique' = '9003',
                              '9003A-Création artistique (arts plastiques)' = '9003A',
                              '9003B-Autre création artistique' = '9003B',
                              '9004-Gestion de salles de spectacles' = '9004',
                              '9004Z-Gestion de salles de spectacles' = '9004Z',
                              
                              '91-Biblioth archive musée & aut act cul' = '91',
                              
                              '910-Biblioth archive musée & aut act cul' = '910',
                              '9101-Gestion des bibliothèques & des archives' = '9101',
                              '9101Z-Gestion des bibliothèques & des archives' = '9101Z',
                              '9102-Gestion des musées' = '9102',
                              '9102Z-Gestion des musées' = '9102Z',
                              '9103-Gestion site histor & attraction simil' = '9103',
                              '9103Z-Gestion site histor & attraction simil' = '9103Z',
                              '9104-Gest jardin bota & zoo & réserv nat' = '9104',
                              '9104Z-Gest jardin bota & zoo & réserv nat' = '9104Z',
                              
                              '92-Organisation jeux de hasard & argent' = '92',
                              
                              '920-Organisation jeux de hasard & argent' = '920',
                              '9200-Organisation jeux de hasard & argent' = '9200',
                              '9200Z-Organisation jeux de hasard & argent' = '9200Z',
                              
                              '93-Act sportiv, récréatives & de loisirs' = '93',
                              
                              '931-Activités liées au sport' = '931',
                              '9311-Gestion installations sportives' = '9311',
                              '9311Z-Gestion installations sportives' = '9311Z',
                              '9312-Activités de clubs de sports' = '9312',
                              '9312Z-Activités de clubs de sports' = '9312Z',
                              '9313-Activité des centres de culture physique' = '9313',
                              '9313Z-Activité des centres de culture physique' = '9313Z',
                              '9319-Autres activités liées au sport' = '9319',
                              '9319Z-Autres activités liées au sport' = '9319Z',
                              
                              '932-Activités récréatives et de loisirs' = '932',
                              '9321-Act parcs attractions & parcs à thèmes' = '9321',
                              '9321Z-Act parcs attractions & parcs à thèmes' = '9321Z',
                              '9329-Autres activités récréative & de loisirs' = '9329',
                              '9329Z-Autres activités récréative & de loisirs' = '9329Z',
                              
                              'S-AUTRES ACTIVITÉS DE SERVICES' = 'S',
                              
                              '94-Activités des organisations associatives' = '94',
                              
                              '941-Act organisations écon patron & prof' = '941',
                              '9411-Act organisations patronale & consul' = '9411',
                              '9411Z-Act organisations patronale & consul' = '9411Z',
                              '9412-Act des organisations professionnelles' = '9412',
                              '9412Z-Act des organisations professionnelles' = '9412Z',
                              
                              '942-Activités des syndicats de salariés' = '942',
                              '9420-Activités des syndicats de salariés' = '9420',
                              '9420Z-Activités des syndicats de salariés' = '9420Z',
                              
                              '949-Act autres organisations associatives' = '949',
                              '9491-Activités des organisations religieuses' = '9491',
                              '9491Z-Activités des organisations religieuses' = '9491Z',
                              '9492-Activités des organisations politiques' = '9492',
                              '9492Z-Activités des organisations politiques' = '9492Z',
                              '9499-Act organisations associatives nca' = '9499',
                              '9499Z-Aut org fonctionnant par adhé volont' = '9499Z',
                              
                              '95-Répar ordi & bien perso & domestique' = '95',
                              
                              '951-Répar ordinateur & équipt communication' = '951',
                              '9511-Répar ordinateur & équipt périphérique' = '9511',
                              '9511Z-Répar ordinateur & équipt périphérique' = '9511Z',
                              '9512-Réparation équipements de communication' = '9512',
                              '9512Z-Réparation équipements de communication' = '9512Z',
                              
                              '952-Réparation  biens personnel & domestique' = '952',
                              '9521-Réparation prdts électroniq grd public' = '9521',
                              '9521Z-Réparation prdts électroniq grd public' = '9521Z',
                              '9522-Répar électromén & éqpt maison & jard' = '9522',
                              '9522Z-Répar électromén & éqpt maison & jard' = '9522Z',
                              '9523-Réparation chaussures & articles en cuir' = '9523',
                              '9523Z-Réparation chaussures & articles en cuir' = '9523Z',
                              '9524-Réparation meubles & équipt du foyer' = '9524',
                              '9524Z-Réparation meubles & équipt du foyer' = '9524Z',
                              '9525-Répararticles horlogerie & bijouterie' = '9525',
                              '9525Z-Répararticles horlogerie & bijouterie' = '9525Z',
                              '9529-Répar aut biens personnel & domestique' = '9529',
                              '9529Z-Répar aut biens personnel & domestique' = '9529Z',
                              
                              '96-Autres services personnels' = '96',
                              
                              '960-Autres services personnels' = '960',
                              '9601-Blanchisserie-teinturerie' = '9601',
                              '9601A-Blanchisserie-teinturerie de gros' = '9601A',
                              '9601B-Blanchisserie-teinturerie de détail' = '9601B',
                              '9602-Coiffure et soins de beauté' = '9602',
                              '9602A-Coiffure' = '9602A',
                              '9602B-Soins de beauté' = '9602B',
                              '9603-Services funéraires' = '9603',
                              '9603Z-Services funéraires' = '9603Z',
                              '9604-Entretien corporel' = '9604',
                              '9604Z-Entretien corporel' = '9604Z',
                              '9609-Autres services personnels nca' = '9609',
                              '9609Z-Autres services personnels nca' = '9609Z',
                              
                              'T-Act ménages: empl, prod pr us propre' = 'T',
                              
                              '97-Act ménage: empl de person domestique' = '97',
                              
                              '970-Act ménage: empl de person domestique' = '970',
                              '9700-Act ménage: empl de person domestique' = '9700',
                              '9700Z-Act ménage: empl de person domestique' = '9700Z',
                              
                              '98-Act ménage : prod b&s (usage propre)' = '98',
                              
                              '981-Act ménage : prod biens (usage propre)' = '981',
                              '9810-Act ménage : prod biens (usage propre)' = '9810',
                              '9810Z-Act ménage : prod biens (usage propre)' = '9810Z',
                              
                              '982-Act ménage : prod serv (usage propre)' = '982',
                              '9820-Act ménage : prod serv (usage propre)' = '9820',
                              '9820Z-Act ménage : prod serv (usage propre)' = '9820Z',
                              
                              'U-ACTIVITÉS EXTRA-TERRITORIALES' = 'U',
                              
                              '99-Act organisations extraterritoriales' = '99',
                              
                              '990-Act organisations extraterritoriales' = '990',
                              '9900-Act organisations extraterritoriales' = '9900',
                              '9900Z-Act organisations extraterritoriales' = '9900Z'
  ))
}

SMIC_NET_06_21 <- function() {} # just to appear in RStudio outline

SMIC_NET_06_21 <- tibble::tribble( ~ANNEE, ~SMIC_NET,
                   "2006", 970.33,
                   "2007", 995.24,
                   "2008", 1025.22,
                   "2009", 1044.08,
                   "2010", 1056.24,
                   "2011", 1073.96,
                   "2012", 1107.35,
                   "2013", 1120.43,
                   "2014", 1128.70,
                   "2015", 1135.99,
                   "2016", 1141.61,
                   "2017", 1151.51,
                   "2018", 1177.16,
                   "2019", 1204.19,
                   "2020", 1218.60,
                   "2021", 1283.42,
  )







## Indice des prix à la consommation
# INSEE : IPC Base 2015 Variables annuelles - Ensemble des Ménages - 
#   France - Ensemble  En %

IPC_variation_anuelle <- function() {}
IPC_variation_anuelle <- 
  c(
    "2020" = 0.5,         # 0.0, #0.5,
    "2019" = 1.1,         # 1.1,
    "2018" = 1.8,         # 1.8,
    "2017" = 1.0,         # 1.0,
    "2016" = 0.2,         # 0.2,
    "2015" = 0.0,         # 0.0,
    "2014" = 0.5,         # 0.5,
    "2013" = 0.9,         # 0.9,
    "2012" = 2.0,         # 2.0,
    "2011" = 2.1,         # 2.1,
    "2010" = 1.5,         # 1.5,
    "2009" = 0.1,         # 0.1,
    "2008" = 2.8,         # 2.8,
    "2007" = 1.5,         # 1.5,
    "2006" = 1.7,         # 1.7,
    "2005" = 1.7,         # 1.7,
    "2004" = 2.1,         # 2.1,
    "2003" = 2.1,         # 2.1,
    "2002" = 1.9,         # 1.9,
    "2001" = 1.6,         # 1.6,
    "2000" = 1.7,         # 1.7,
    "1999" = 0.5,         # 0.5,
    "1998" = 0.6,         # 0.6,
    "1997" = 1.2,         # 1.2,
    "1996" = 2.0,         # 2.0,
    "1995" = 1.8,         # 1.8,
    "1994" = 1.6,         # 1.6,
    "1993" = 2.1,         # 2.1,
    "1992" = 2.4,         # 2.4,
    "1991" = 3.2,         # 3.2 #,
    
    "1990" = 3.434459, # cf. IPC80 ci-dessous
    "1989" = 3.311650,
    "1988" = 2.360775,
    "1987" = 2.992519,
    "1986" = 4.223522,
    "1985" = 6.505190,
    "1984" = 8.974359,
    "1983" = 9.586777,
    "1982" = 13.828786,
    "1981" = 12.845011
  )

#     On décale tout de 1 pour faire le calcul à l'envers.
IPC_variation_anuelle <- 
  c("2020" = 0,
    IPC_variation_anuelle |> 
      set_names(lead(names(IPC_variation_anuelle), default = "1980"))
  )

# IPC80 <- (c( 
#   "1990" = 180.7,
#   "1989" = 174.7,
#   "1988" = 169.1,
#   "1987" = 165.2,
#   "1986" = 160.4,
#   "1985" = 153.9,
#   "1984" = 144.5,
#   "1983" = 132.6,
#   "1982" = 121.0,
#   "1981" = 106.3,
#   "1980" = 94.2 # Juillet 1980
# ) / 180.7) 
# 
# (lag(IPC80)/IPC80 - 1 )* 100

euros_2020 <- function() {}
euros_2020 <- 
  tibble(year = names(IPC_variation_anuelle), 
         IPC_variation_anuelle = IPC_variation_anuelle) |>
  mutate(pct = (100 + IPC_variation_anuelle)/100,
         euros_2020 = set_names(cumprod(pct), year)) |> #print(n = 100)
  pull(euros_2020)









## Nomenc corps finale ----

# nomenc_CORPS_GRADE_EF_final # imported at start


CORPS_plus_recode_vect <- c(
  "A2I00a1-Magistrat judiciaire", "A2I00b1-Chg formation ENM"     ,
  "A3F00a1-Inspecteur FiP"      , "A2F00a1-Administrateur FiP"    ,
  "A3F20a2-Inspecteur Douanes"  , "A2F00b2-Administrateur Douanes",
  #"APK10d2-MCF Agri"            , "APK01a2-Professeur sup Agri"   ,
  #"APK10f2-MCF ENSA"            , "APK01e2-Professeur ENSA"       ,
  #"APK32g7-Assistant ingé IRD"  , "APK32k9-Assistant ingé INRIA"  ,
  #"APK40a3-Directeur rech CNRS" , "APK40k9-Dir recherche Ponts"   ,
  #"APK50a3-CR CNRS"             , "APK50f9-CR Ponts"              ,
  #"APK50b4-CR INSERM"           , "APK40b4-Dir recherche INSERM"  ,
  #"APL01a1-Professeur agrégé"   , "A3L00a1-Inspecteur académie"   ,
  "APL01a1-Professeur agrégé"   , "APL00a1-Professeur CS CPGE"    ,
  "APM00a1-Conservateur biblio" , "A2M00a1-Conservateur G bilio"  ,
  "APM00b2-Conserv patrimoine"  , "A2M00b2-Conservateur G patri"  #,
  #"B1E10a4-Contrôleur travail"  , "A3E20a4-Inspecteur travail"    ,
  #"B1E10d9-Inspecteur du permis", "A3E81a9-Délég perm conduire"   ,
  #"B1F10a2-Contrôleur Douanes"  , "A3F21a2-Naviguant Douanes"     ,
  #"B1G41ed-Tech admin Pénit"    , "A3G61ed-Directeur tech Pénit"  ,
  #"BPI20a4-Éducateur PJJ"       , "A3I20a4-Chef de service PJJ"   ,
  #"BPL00a3-Assistant social"    , "APL70b3-Cons tech social"      ,
  #"BPM10b3-Technicien d'Art"    , "APM30a3-Chef travaux d'art"    ,
  #"CZF00a1-Agent admin FiP"     , "B1F00a1-Contrôleur FiP"        ,
  #"CZF10a2-Agent des Douanes"   , "B1F10a2-Contrôleur Douanes"    ,
  #"CZG10gd-Adjoint tech Pénit"  , "B1G41ed-Tech admin Pénit"      ,
  #"CZH10a2-Surv Pénitentiaire"  , "A3H30a2-Capitaine Cmd Pénit"   ,
  #"CZK00a1-Adjoint tech rech"   , "B1G40b8-Tech labo MEFI"        ,
  #"CZK00a1-Adjoint tech rech"   , "B1G41dc-Tech police scienti"   ,
  #
  # "APIc1a-Maitre de conférence"= "APIa1a-Professeur des univ"   ,
)

CORPS_plus_EF_recode_vect  <- c(
  "A2E20a2-Magistrat CRC"        = "A1DY8a2-Dir CRC EF"            ,
  "A2E20b2-Magistrat TA"         = "A1DY8b2-Dir TA CAA EF"         ,
  "A2E41a7-Conseiller MAE"       = "A1DY2a1-Direction AE EF"       ,
  #"A2G03a7-Ingénieur Mines"     =, "A1DY5e1-Chef srv rég PTT EF"   ,
  "A2H10a1-Commissaire PN"       = "A1DY7e1-Insp serv act PN EF"   ,
  "A2H10a1-Commissaire PN"       = "A2HY0a1-Commissaire div PN EF" ,
  "A2I00a1-Magistrat judiciaire" = "A1DY9a1-Inspection srv jud EF" ,
  "A2I00a1-Magistrat judiciaire" = "A1DY9c1-Dir cours appel EF"    ,
  "A2I00a1-Magistrat judiciaire" = "A1DY9d1-Dir cours TGI EF"      ,
  "A2I00a1-Magistrat judiciaire" = "A1DY9b1-Dir cassation EF"      ,
  "A3E10a3-Attaché admin État"   = "A2EY0a3-Conseiller admin EF"   ,
  "A3E10a3-Attaché admin État"   = "A2EY1a3-Administrateur EN EF"  ,
  "A3E10a3-Attaché admin État"   = "A3EY0a3-Intendant univ EF"     ,
  "A3E30a5-Inspecteur CCRF"      = "A2EY2b5-Directeur CCRF EF"     ,
  "A3E30a5-Inspecteur CCRF"      = "A2EY2c5-Insp expert CCRF EF"   ,
  "A3E60a8-Inspecteur des PTT"   = "A2EY2h8-Chef srv dép PTT EF"   ,
  "A3E60a8-Inspecteur des PTT"   = "A3EY0b8-Chef centre PTT EF"    ,
  "A3F00a1-Inspecteur FiP"       = "A2FY0a1-Chef comptable FiP EF" ,
  "A3F00a1-Inspecteur FiP"       = "A3FY0a1-Insp spécial FiP EF"   ,
  "A3G00a1-Ingénieur des TPE"    = "A2GY0a1-Ingénieur chef TPE EF" ,
  "A3G60a3-Ingé travaux météo"   = "A2GY2a3-Chef unité Météo EF"   ,
  "A3H10a1-Capitaine Cmd Police" = "A3HY0a1-Commandant police EF"  ,
  "A3H20a2-Directeur serv Pénit" = "A2HY0a2-Directeur Pénit EF"    ,
  "A3I00a2-Directeur greffier"   = "A2IY0a2-Dir greffier EF"       ,
  "A3I01a3-Dir insertion prob"   = "A2IY1a3-Dir inser prob EF"     ,
  "A3I20a4-Chef de service PJJ"  = "A2IY2a4-Dir PJJ EF"            ,
  "APJ30a3-Médecin ÉducNat"      = "APJY0a3-Méd cons tech EN EF"   ,
  #"APK00a1-Professeur des univ"  = "A1DY5b1-Recteur"               ,
  "APL90b4-Psychologue ÉducNat"  = "APLY1a4-Dir ctr orientation EF",
  "B1G00a1-Technicien sup DD"    = "A3GY0a1-Chef subdiv DD EF"     ,
  "BPI00a2-Greffier"             = "A3IY0a2-Greffier EF"           ,
  "CZH00a1-Policier"             = "A3HY1a1-Resp unité PN EF"      ,
  
  "A2E30a1-Sous-préfet"          = "A2EY2f1-Sous préfet HC EF"
)

# # Croissance du salaire moyen des cadres de l'État
# # Pour distinguer cadres A+ / cadres A, on utilise le seuil de 5000 € en 2020
# #  (cf plus bas), rapporté aux autres années en calculant 
# #  la différence de salaire moyen des cadres par rapport à 2019
# pan |> 
# select(ANNEE, CORPS1,  PSMC_SM_NETR) |> 
# filter(!ANNEE %in% c(1987, 2009) & 
#          CORPS1 %in% c("A2-Cadres supérieurs", "A3-Cadres intermédiaires")) |>
# as_tibble() |> 
# tab(ANNEE, PSMC_SM_NETR) |>
# filter(!is_totrow(PSMC_SM_NETR)) |> 
# mutate(ecart19 = mutate(PSMC_SM_NETR, 
#                         pct = (last(mean) - mean)/mean, 
#                         display = "pct"), 
#        sal5000 = 5000 * (1 - ecart19$pct), 
#        vect    = set_names(sal5000, ANNEE),
#        cat_vect = paste0("\"", ANNEE, "\" = \"", sal5000, "\",\n")
# ) |>
# #pull(vect)
# pull(cat_vect) |> cat()

sal5000_cadres_Etat <- c(
  "1980" = "4355.02295638317",
  "1982" = "4328.86905315602",
  "1983" = "3955.56125428223",
  "1984" = "4097.81723862174",
  "1985" = "3726.12485535989",
  "1986" = "3939.41691723885",
  "1988" = "3806.78064635575",
  "1989" = "3952.00976637744",
  "1991" = "4146.14872151293",
  "1992" = "4357.38778345702",
  "1993" = "4269.31014616441",
  "1994" = "4244.31829699528",
  "1995" = "4472.23439552878",
  "1996" = "4687.15255347203",
  "1997" = "4733.28296025848",
  "1998" = "4847.37434921699",
  "1999" = "4903.93816466439",
  "2000" = "4961.72093872399",
  "2001" = "5061.01080893288",
  "2002" = "5172.53106293947",
  "2003" = "5156.53879429737",
  "2004" = "5083.59007354972",
  "2005" = "5049.81355469277",
  "2006" = "5051.08484097423",
  "2007" = "5102.65493543854",
  "2008" = "5083.637805781",
  "2010" = "5158.34180492285",
  "2011" = "5169.92376938063",
  "2012" = "5124.34638173433",
  "2013" = "5102.03550796741",
  "2014" = "5098.72624483357",
  "2015" = "5124.28419731342",
  "2016" = "5103.38976934309",
  "2017" = "5112.55891240809",
  "2018" = "5036.34010914691",
  "2019" = "5000" # ,
)


corresp_CORPS_ecole <- c(
  #ec_ENA   = "A2E50a3-Élève ENA"            ,
  #ec_ENS   = "APL80b4-Élève ENS"             #,
  
  ec_PEF   = "A2G00a1-Ingénieur Ponts"      ,
  ec_Mines = "A2G03a7-Ingénieur Mines"      ,
  ec_ENSAE = "A2E40a6-Administrateur INSEE" ,
  ec_ENM   = "A2I00a1-Magistrat judiciaire" ,
  ec_IRA   = "A3E10a3-Attaché admin État"   ,
  ec_TPE   = "A3G00a1-Ingénieur des TPE"    ,
  ec_ENAC  = "A3G20c2-Ingé expl Aviation C" ,
  ec_AgEnv = "A3G30a6-Ingénieur agri envir" ,
  ec_Ind   = "A3G40a7-Ingénieur industrie"  ,
  ec_Météo = "A3G60a3-Ingé travaux météo"   ,
  ec_IGN   = "A3G60b5-Ingénieur IGN"        ,
  ec_ENSAI = "A3E40a6-Attaché INSEE"        #,
)

# nomenc_CORPS_GRADE_EF_final |>
#   filter(type == "CORPS" & !niv_lab %in% c("CN", "Élève")) |>
#   arrange(GRAD1) |>
#   group_by(CORPS) |> slice(n()) |> ungroup() |>
#   select(CORPS, GRAD1) |> #print(n = 300)
#   tbl_to_tribble_code()


# nomenc_CORPS_GRADE_EF_final |>
#   filter(type == "CORPS" & niv_lab != "CN") |>
#   arrange(GRAD_4) |>
#   group_by(CORPS4) |> slice(1) |> ungroup() |>
#   select(CORPS4, GRAD_4) |>
#   tbl_to_tribble_code()
# #print(n = 300)

last_GRAD_4_in_CORPS <- c(
  "A1D10-IG Finances"               , "A1D10a1A12-Insp finances Général"        ,
  "A1D11-Cour des comptes"          , "A1D11a2A12-Conseiller maître CC"         ,
  "A1D12-Conseil d'État"            , "A1D12a2A12-Conseiller d'État"            ,
  "A1D13-Préfet"                    , "A1D13a1A12-Préfet HC"                    ,
  "A1D20-Inspection générale"       , "A1D20A12-Inspection générale 1C"         ,
  "A2E00-Administrateur civil"      , "A2E00a3A13-Administrateur Général"       ,
  "A2E20-Magistrat administratif"   , "A2E20A13-Prés section CRC"               ,
  "A2E30-Sous-préfet"               , "A2E30a1A22-Sous préfet HC"               ,
  "A2E40-Administrateur INSEE"      , "A2E40a6A22-Admin INSEE HC"               ,
  "A2E41-Conseiller MAE"            , "A2E41a7A22-Conseiller MAE HC"            ,
  "A2E42-Administrateur autre"      , "A2E42A13-Administrateur autre HC"        ,
  "A2E50-Élève ENA"                 , "A2E50a3A29-Élève ENA"                    ,
  "A2F00-Administrateur FiP"        , "A2F00a1A12-Administrateur FiP G 1C+"     ,
  "A2F01-Administrateur DDI"        , "A2F01a2A12-Administrateur sup DDI"       ,
  "A2G00-Ingénieur des Ponts"       , "A2G00a1A12-Ingé PEF G CE"                ,
  "A2G01-Architecte urbaniste"      , "A2G01a1A13-Archi urbaniste Général"      ,
  "A2G02-Administrateur Mer"        , "A2G02a4A23-Admin Mer Chef"               ,
  "A2G03-Ingénieur des Mines"       , "A2G03a7A13-Ingé Mines Général"           ,
  "A2H10-Commissaire de police"     , "A2H10a1A13-Commissaire PN Général"       ,
  "A2I00-Magistrat judiciaire"      , "A2I00A22-Magistrat judiciaire 1C"        ,
  "A3E10-Attaché admin État"        , "A3E10a3A32-Attaché HC/dir srv"           ,
  "A3E20-Inspecteur du travail"     , "A3E20a4A23-Directeur travail"            ,
  "A3E30-Inspecteur CCRF"           , "A3E30a5A13-Inspecteur CCRF Général"      ,
  "A3E40-Attaché INSEE"             , "A3E40a6A32-Attaché statisticien HC"      ,
  "A3E60-Inspecteur des PTT"        , "A3E60a8A33-Inspecteur PTT Princ"         ,
  "A3E61-Receveur des PTT"          , "A3E61a8A32-Receveur PTT CE"              ,
  "A3E70-Inspecteur ASS"            , "A3E70a9A32-Inspecteur ASS HC/CE"         ,
  "A3E80-Secrétaire trad AE"        , "A3E80A33-Secrétaire trad AE Princ"       ,
  "A3E81-Cadre admin A autre"       , "A3E81A32-Cadre admin A autre HC +"       ,
  "A3F00-Inspecteur FiP"            , "A3F00a1A23-Admin Fip adjoint"            ,
  "A3F20-Inspecteur Douanes"        , "A3F20a2A23-Dir adj Douanes"              ,
  "A3F21-Naviguant Douanes"         , "A3F21a2A33-Navig Douanes Cat1"           ,
  "A3G00-Ingénieur des TPE"         , "A3G00a1A33-Ingénieur TPE Div"            ,
  "A3G20-Ingé Aviation Civile"      , "A3G20A23-Ingé Aviation Civile Chef"      ,
  "A3G30-Ingénieur agri envir"      , "A3G30a6A32-Ingé agri envir HC"           ,
  "A3G40-Ingénieur industrie"       , "A3G40a7A23-Ingé industrie HC"            ,
  "A3G50-Ingénieur travaux PTT"     , "A3G50A33-Ingénieur travaux PTT Princ"    ,
  "A3G60-Ingé Écologie autre"       , "A3G60A23-Ingé IGN Chef"                  ,
  "A3G61-Ingénieur Int AE Pénit"    , "A3G61A32-Ingénieur Int AE Pénit HC"      ,
  "A3G62-Ingénieur A autre"         , "A3G62A23-Ingé A autre Chef/CE"           ,
  "A3H10-Capitaine Cmd Police"      , "A3H10a1A32-Commandant police"            ,
  "A3H20-Directeur pénitentiaire"   , "A3H20a2A23-Directeur serv Pénit HC"      ,
  "A3H30-Capitaine Cmd Pénit"       , "A3H30a2A43-Commandant pénit"             ,
  "A3I00-Directeur greffier"        , "A3I00a2A33-Dir Greffier Princ"           ,
  "A3I01-Dir insertion prob"        , "A3I01a3A33-Dir insertion prob HC"        ,
  "A3I20-Chef de service PJJ"       , "A3I20a4A32-Dir srv PJJ HC"               ,
  "A3L00-Inspecteur EN/IA-IPR"      , "A3L00A21-Inspecteur IA-IPR HC"           ,
  "A3L10-Chef d'étab scolaire"      , "A3L10A22-Chef d'étab scolaire HC"        ,
  "APJ00-Inspecteur vété/SP"        , "APJ00A12-Inspecteur vété/SP G CE"        ,
  "APJ10-PU/MCF prat hospitalier"   , "APJ10A21-PU prat hospitalier CE"         ,
  "APJ30-Médecin ÉducNat"           , "APJ30a3A23-Médecin EN 1C"                ,
  "APJ40-Ingénieur sanitaire"       , "APJ40A32-Ingénieur sanitaire Princ"      ,
  "APK00-Professeur des universités", "APK00A21-Professeur des universités CE"  ,
  "APK01-Professeur grandes écoles" , "APK01A21-Professeur grandes écoles CE"   ,
  "APK10-Maitre de conférence"      , "APK10A23-Maitre de conférence HC"        ,
  "APK20-Assistant ESR"             , "APK20A23-Assistant ESR 1C"               ,
  "APK30-Ingénieur de recherche"    , "APK30A22-Ingénieur de recherche HC"      ,
  "APK31-Ingénieur d'études"        , "APK31A33-Ingénieur d'études HC"          ,
  "APK32-Ingé d'études assistant"   , "APK32A43-Ingé d'études assistant 1C"     ,
  "APK40-Directeur de recherche"    , "APK40A21-Directeur de recherche CE"      ,
  "APK50-Chargé de recherche"       , "APK50A22-Chargé de recherche HC"         ,
  "APL01-Professeur agrégé"         , "APL01a1A22-Professeur agrégé CE"         ,
  "APL10-Professeur certifié"       , "APL10a1A32-Professeur certifié CE"       ,
  "APL11-Professeur de sport"       , "APL11A32-Professeur de sport CE"         ,
  "APL12-Prof certifié Agri"        , "APL12a2A32-Prof certifié Agri CE"        ,
  "APL20-Professeur des écoles"     , "APL20a1A32-Professeur écoles CE"         ,
  "APL30-Prof lycée pro"            , "APL30A32-Prof lycée pro CE"              ,
  "APL40-Prof ens gal tech adj"     , "APL40A33-Prof ens gal tech adj CE"       ,
  "APL50-Conseiller princ éducation", "APL50A32-Conseiller princ éducation CE"  ,
  "APL70-Conseiller social educ pop", "APL70A33-Conseiller social educ pop HC"  ,
  "APL80-Élève ENS"                 , "APL80A49-Élève ENS Élève"                ,
  "APL90-Psychologue"               , "APL90A32-Psychologue HC"                 ,
  "APM00-Conservateur/ICC"          , "APM00A23-Conservateur/ICC Chef"          ,
  "APM10-Bibliothécaire chg docu"   , "APM10A42-Bibliothécaire chg docu 1C"     ,
  "APM30-Ingé patrimoine"           , "APM30A32-Ingé patrimoine HC"             ,
  "B1E00-Secrétaire administratif"  , "B1E00B12-Secrétaire administratif CE"    ,
  "B1E01-Secrétaire admin EN"       , "B1E01a3B13-Secrétaire admin EN CE"       ,
  "B1E02-Secrétaire adm Int"        , "B1E02a3B12-Secrétaire adm Int CE"        ,
  "B1E03-Secrétaire adm DD"         , "B1E03B13-Secrétaire adm DD CE"           ,
  "B1E10-Contrôleur travail"        , "B1E10a4B13-Contrôleur travail CE"        ,
  "B1E11-Contrôleur CCRF"           , "B1E11a5B13-Contrôleur CCRF Princ"        ,
  "B1E12-Contrôleur INSEE"          , "B1E12a6B13-Contrôleur INSEE Princ"       ,
  "B1E13-Inspecteur permis"         , "B1E13a9B13-Inspecteur permis 1C"         ,
  "B1E20-Contrôleur PTT et ass"     , "B1E20B13-Contrôleur PTT et ass CE"       ,
  "B1F00-Contrôleur FiP"            , "B1F00a1B13-Contrôleur FiP Princ"         ,
  "B1F10-Contrôleur des Douanes"    , "B1F10a2B13-Contrôleur Douanes Princ"     ,
  "B1G00-Technicien sup DD"         , "B1G00a1B13-Technicien sup DD Chef"       ,
  "B1G10-Technicien sup Aviat"      , "B1G10a2B13-Tech sup Aviation CE"         ,
  "B1G11-Technicien sup Météo"      , "B1G11a3B13-Tech sup Météo Chef"          ,
  "B1G12-Technicien DD autre"       , "B1G12B13-Technicien DD autre Princ"      ,
  "B1G13-Tech sup industrie"        , "B1G13a7B13-Tech sup indus Chef"          ,
  "B1G20-Technicien sup Agri"       , "B1G20a6B13-Tech sup Agri Chef"           ,
  "B1G30-Technicien des PTT"        , "B1G30B13-Technicien des PTT Chef"        ,
  "B1G40-Technicien EcoFi"          , "B1G40B13-Technicien EcoFi CE"            ,
  "B1G41-Tech Int AE Pénit"         , "B1G41B13-Tech Int AE Pénit CE"           ,
  "BPI00-Greffier"                  , "BPI00a2B13-Greffier Princ"               ,
  "BPI10-Conseiller de probation"   , "BPI10a3A43-Conseiller probation HC"      ,
  "BPI20-Éducateur PJJ"             , "BPI20a4B13-Éducateur PJJ 1C"             ,
  "BPJ00-Infirmier État ÉducNat"    , "BPJ00a3A43-Infirmier de l’État HC"       ,
  "BPJ10-Tech sécu sanitaire"       , "BPJ10a4B13-Tech sécu sanitaire Chef"     ,
  "BPK00-Tech recherche formation"  , "BPK00B13-Tech recherche formation CE"    ,
  "BPL00-Assistant social"          , "BPL00B13-Assistant social Princ"         ,
  "BPM00-Biblio assistant docu B"   , "BPM00B13-Biblio assistant docu B CE"     ,
  "BPM10-Technicien Art BF"         , "BPM10B13-Technicien Art BF CE"           ,
  "CZE00-Adjoint administratif"     , "CZE00C00-Adjoint administratif Princ 1C" ,
  "CZE10-Préposé des PTT"           , "CZE10a8C02-Préposé des PTT Chef"         ,
  "CZF00-Agent admin FiP"           , "CZF00a1C01-Agent admin FiP Princ 1C"     ,
  "CZF10-Agent des Douanes"         , "CZF10a2C01-Agent Douanes Princ 1C"       ,
  "CZG00-Ouvrier des TPE"           , "CZG00a1C02-Chef d’équipe TPE Princ"      ,
  "CZG10-Adjoint tech État"         , "CZG10C01-Adjoint tech État Princ"        ,
  "CZG20-Agent d'exploitation PTT"  , "CZG20C02-Agent d'exploitation PTT Princ" ,
  "CZG30-Ouvrier professionnel"     , "CZG30afC02-Maitre ouvrier Princ"         ,
  "CZG40-Ouvrier d'entretien EN"    , "CZG40afC00-Ouvrier entretien EN Chef"    ,
  "CZH00-Policier"                  , "CZH00a1B12-Major de police"              ,
  "CZH10-Surv Pénitentiaire"        , "CZH10a2C00-Major pénitentiaire"          ,
  "CZK00-Adjoint tech recherche"    , "CZK00C00-Adjoint tech recherche Princ 1C",
  "CZM00-Magasinier biblio"         , "CZM00a1C01-Magasinier biblio Princ 1C"   ,
  "CZM01-Magasinier Culture"        , "CZM01a2C01-Magasinier Culture Princ 1C"  #,
)

# second_GRAD1_in_CORPS <- 
# c( 
#   "A1D10a1A13-Inspecteur finances 1C"      = "A1D10a1-IG Finances"            , 
#   "A1D11a2A13-Cons référendaire CC"        = "A1D11a2-Cour des comptes"       , 
#   "A1D12a2A13-Maître des requêtes CE"      = "A1D12a2-Conseil d'État"         , 
#   "A1D13a1A12-Préfet HC"                   = "A1D13a1-Préfet"                 , 
#   "A1D20b1A12-IG AENR 1C"                  = "A1D20b1-IG AENR"                , 
#   "A1D20c1A12-CG ÉcoFi 1C"                 = "A1D20c1-CG ÉcoFi"               , 
#   "A2E00a3A22-Admin civil HC"              = "A2E00a3-Administrateur civil"   , 
#   "A2E20a2A22-1er conseiller CRC"          = "A2E20a2-Magistrat CRC"          , 
#   "A2E20b2A22-1er conseiller TA CAA"       = "A2E20b2-Magistrat TA"           , 
#   "A2E30a1A22-Sous préfet HC"              = "A2E30a1-Sous-préfet"            , 
#   "A2E40a6A22-Admin INSEE HC"              = "A2E40a6-Administrateur INSEE"   , 
#   "A2E41a7A22-Conseiller MAE HC"           = "A2E41a7-Conseiller MAE"         , 
#   "A2E42a8A22-Admin PTT 1C"                = "A2E42a8-Administrateur PTT"     , 
#   "A2E42b3A22-Cadre sup adm autre HC"      = "A2E42b3-Cadre sup adm autre"    , 
#   "A2F00a1A13-Administrateur FiP G"        = "A2F00a1-Administrateur FiP"     , 
#   "A2F00b2A22-Dir srv douaniers 1C"        = "A2F00b2-Administrateur Douanes" , 
#   "A2G00a1A22-Ingé PEF Chef"               = "A2G00a1-Ingénieur Ponts"        , 
#   "A2G01a1A22-Archi urbaniste Chef"        = "A2G01a1-Archi urbaniste"        , 
#   "A2G02a4A42-Admin Mer 1C"                = "A2G02a4-Administrateur Mer"     , 
#   "A2G03a7A22-Ingé Mines Chef"             = "A2G03a7-Ingénieur Mines"        , 
#   "A2H10a1A23-Commissaire PN Div"          = "A2H10a1-Commissaire PN"         , 
#   "A2I00a1A22-Magistrat tribunaux G1"      = "A2I00a1-Magistrat judiciaire"   , 
#   "A3E10a3A33-Attaché admin Princ"         = "A3E10a3-Attaché admin État"     , 
#   "A3E20a4A33-Dir adj travail"             = "A3E20a4-Inspecteur travail"     , 
#   "A3E30a5A33-Inspecteur CCRF Princ"       = "A3E30a5-Inspecteur CCRF"        , 
#   "A3E40a6A33-Attaché statisticien Princ"  = "A3E40a6-Attaché INSEE"          , 
#   "A3E60a8A33-Inspecteur PTT Princ"        = "A3E60a8-Inspecteur des PTT"     , 
#   "A3E61a8A43-Receveur PTT 1C"             = "A3E61a8-Receveur des PTT"       , 
#   "A3E70a9A33-Inspecteur ASS Princ"        = "A3E70a9-Inspecteur ASS"         , 
#   "A3E80a7A33-Secrétaire AE Princ"         = "A3E80a7-Secrétaires AE"         , 
#   "A3E80b7A33-Traducteur AE Princ"         = "A3E80b7-Traducteur AE"          , 
#   "A3E81a9A33-Délég perm conduire Princ"   = "A3E81a9-Délég perm conduire"    , 
#   "A3E81b9A33-Inspecteur JS Princ"         = "A3E81b9-Inspecteur JS"          , 
#   "A3E81c9A33-Attaché éco Princ"           = "A3E81c9-Attaché cons éco"       , 
#   "A3E81d9A33-Officier OFPRA Princ"        = "A3E81d9-Officier OFPRA"         , 
#   "A3F00a1A42-Inspecteur FiP Div"          = "A3F00a1-Inspecteur FiP"         , 
#   "A3F20a2A33-Inspecteur Douanes Rég"      = "A3F20a2-Inspecteur Douanes"     , 
#   "A3F21a2A33-Navig Douanes Cat1"          = "A3F21a2-Naviguant Douanes"      , 
#   "A3G00a1A33-Ingénieur TPE Div"           = "A3G00a1-Ingénieur des TPE"      , 
#   "A3G20a2A33-Ingé nav aérienne Princ"     = "A3G20a2-Ingé nav aérienne"      , 
#   "A3G20b2A33-Ing el sécu aerienne Princ"  = "A3G20b2-Ing el sécu aerienne"   , 
#   "A3G20c2A33-Ingé expl Aviation Princ"    = "A3G20c2-Ingé expl Aviation C"   , 
#   "A3G30a6A33-Ingé agri envir Div"         = "A3G30a6-Ingénieur agri envir"   , 
#   "A3G40a7A33-Ingé industrie Div"          = "A3G40a7-Ingénieur industrie"    , 
#   "A3G50aeA33-Réviseur trav PTT Princ"     = "A3G50ae-Réviseur trav PTT"      , 
#   "A3G50beA33-Chef trav auto PTT 1C"       = "A3G50be-Chef trav auto PTT"     , 
#   "A3G60a3A33-Ingé trav météo Div"         = "A3G60a3-Ingé travaux météo"     , 
#   "A3G60b5A33-Ingé IGN Div"                = "A3G60b5-Ingénieur IGN"          , 
#   "A3G60c4A33-Capitaine de port 1C"        = "A3G60c4-Officier de port"       , 
#   "A3G61a9A33-Ingé SIC Int Princ"          = "A3G61a9-Ingé SIC Intérieur"     , 
#   "A3G61baA32-Ingé serv tech Int Princ"    = "A3G61ba-Ingé serv tech Int"     , 
#   "A3G61cbA33-Attaché SI AE Princ"         = "A3G61cb-Chiffreur chef AE"      , 
#   "A3G61dcA33-Ingé police scienti Princ"   = "A3G61dc-Ingé police scienti"    , 
#   "A3G61edA33-Directeur tech Pénit 1C"     = "A3G61ed-Directeur tech Pénit"   , 
#   "A3G62a8A33-Ingé méca élec MEFI CE"      = "A3G62a8-Ingé méca élec MEFI"    , 
#   "A3G62b8A33-Ingé éco MEFI CS"            = "A3G62b8-Ingénieur econ MEFI"    , 
#   "A3G62c8A32-Ingé labo MEFI Chef"         = "A3G62c8-Pers scien labo MEFI"   , 
#   "A3H10a1A33-Capitaine police"            = "A3H10a1-Capitaine Cmd Police"   , 
#   "A3H20a2A23-Directeur serv Pénit HC"     = "A3H20a2-Directeur serv Pénit"   , 
#   "A3H30a2B12-Capitaine pénit"             = "A3H30a2-Capitaine Cmd Pénit"    , 
#   "A3I00a2A33-Dir Greffier Princ"          = "A3I00a2-Directeur greffier"     , 
#   "A3I01a3A33-Dir insertion prob HC"       = "A3I01a3-Dir insertion prob"     , 
#   "A3I20a4A33-Dir srv PJJ Princ"           = "A3I20a4-Chef de service PJJ"    , 
#   "A3L00a1A22-Insp académie HC"            = "A3L00a1-Inspecteur académie"    , 
#   "A3L00b1A23-Inspecteur EN HC"            = "A3L00b1-Inspecteur ÉducNat"     , 
#   "A3L10a1A22-Chef d'étab scolaire EN HC"  = "A3L10a1-Chef d'étab scolaire EN", 
#   "A3L10b2A23-Chef d'étab scol Agri 1C"    = "A3L10b2-Chef d'étab scol Agri"  , 
#   "APJ00a1A22-Insp vétérinaire Chef"       = "APJ00a1-Insp vétérinaire"       , 
#   "APJ00b1A22-Médecin insp sp Chef"        = "APJ00b1-Insp santé publique"    , 
#   "APJ00c1A22-Pharma santé pub Chef"       = "APJ00c1-Pharma insp santé"      , 
#   "APJ10a2A22-PU praticien hosp 1C"        = "APJ10a2-PU praticien hosp"      , 
#   "APJ10b2A32-MCF praticien hosp 1C"       = "APJ10b2-MCF praticien hosp"     , 
#   "APJ30a3A23-Médecin EN 1C"               = "APJ30a3-Médecin ÉducNat"        , 
#   "APJ40a4A23-Ingé génie sanitaire Chef"   = "APJ40a4-Ingé génie sanitaire"   , 
#   "APJ40b4A32-Ingé étud sanitaires Princ"  = "APJ40b4-Ingé étud sanitaires"   , 
#   "APK00a1A22-Professeur univ 1C"          = "APK00a1-Professeur des univ"    , 
#   "APK00c1A22-PU médecine g 1C"            = "APK00c1-PU médecine générale"   , 
#   "APK00d1A22-Dir études EHESS 1C"         = "APK00d1-Dir études EHESS"       , 
#   "APK00e1A22-Dir étu EHPE ENC EO 1C"      = "APK00e1-Dir étu EHPE ENC EO"    , 
#   "APK01a2A22-Professeur sup Agri 1C"      = "APK01a2-Professeur sup Agri"    , 
#   "APK01b2A22-Professeur MNHN 1C"          = "APK01b2-Professeur MNHN"        , 
#   "APK01c2A22-Professeur Mines 1C"         = "APK01c2-Professeur Mines"       , 
#   "APK01e2A22-Professeur ENSA 1C"          = "APK01e2-Professeur ENSA"        , 
#   "APK01g2A23-Professeur ENSAM HC"         = "APK01g2-Professeur ENSAM"       , 
#   "APK01h2A23-Professeur d'Art 1C"         = "APK01h2-Professeur d'Art"       , 
#   "APK01i2A22-Prof polytechnique"          = "APK01i2-Prof polytechnique"     , 
#   "APK10a1A23-Maitre de conférences HC"    = "APK10a1-Maitre de conférence"   , 
#   "APK10b1A23-MCF EHESS HC"                = "APK10b1-MCF EHESS"              , 
#   "APK10c1A23-MCF EPEH ENC EO HC"          = "APK10c1-MCF EPEH ENC EO"        , 
#   "APK10d2A23-MCF Agri HC"                 = "APK10d2-MCF Agri"               , 
#   "APK10e2A23-Maître assistant Mines HC"   = "APK10e2-MCF Mines"              , 
#   "APK10f2A32-MCF ENSA 1C"                 = "APK10f2-MCF ENSA"               , 
#   "APK10g2A23-MCF MNHN HC"                 = "APK10g2-MCF MNHN"               , 
#   "APK20b1A23-Chargé mission ensei 1C"     = "APK20b1-Chargé mission ensei"   , 
#   "APK30a1A23-Ingé recherche EN 1C"        = "APK30a1-Ingé recherche EN"      , 
#   "APK30b3A23-Ingé recherche CNRS 1C"      = "APK30b3-Ingé recherche CNRS"    , 
#   "APK30c3A23-Ingénieur nucléaire Cat2"    = "APK30c3-Ingénieur nucléaire"    , 
#   "APK30d4A23-Ingé rech INSERM 1C"         = "APK30d4-Ingé rech INSERM"       , 
#   "APK30e5A32-Ingé recherche INRA 1C"      = "APK30e5-Ingé recherche INRA"    , 
#   "APK30f6A23-Ingé recherche Agri 1C"      = "APK30f6-Ingé recherche Agri"    , 
#   "APK30g7A23-Ingé recherche IRD 1C"       = "APK30g7-Ingé recherche IRD"     , 
#   "APK30h8A23-Ingé recherche Cult 1C"      = "APK30h8-Ingé recherche Cult"    , 
#   "APK30i9A23-Ingé rech CEMAGREF 1C"       = "APK30i9-Ingé rech CEMAGREF"     , 
#   "APK30k9A23-Ingé rech INRETS 1C"         = "APK30k9-Ingé rech INRETS"       , 
#   "APK30l9A23-Ingé rech INRIA 1C"          = "APK30l9-Ingé rech INRIA"        , 
#   "APK31a1A33-Ingé études EN HC"           = "APK31a1-Ingé études EN"         , 
#   "APK31b3A43-Ingé études CNRS 1C"         = "APK31b3-Ingé études CNRS"       , 
#   "APK31c4A42-Ingé études INSERM 1C"       = "APK31c4-Ingé études INSERM"     , 
#   "APK31d5A42-Ingé études INRA 1C"         = "APK31d5-Ingé études INRA"       , 
#   "APK31e6A43-Ingé études Agri 1C"         = "APK31e6-Ingé études Agri"       , 
#   "APK31f7A42-Ingé études IRD 1C"          = "APK31f7-Ingé études IRD"        , 
#   "APK31h9A42-Ingé études INRIA 1C"        = "APK31h9-Ingé études INRIA"      , 
#   "APK31i9A42-Ingé études INRETS 1C"       = "APK31i9-Ingé études INRETS"     , 
#   "APK31j9A42-Ingé études IFREMER 1C"      = "APK31j9-Ingé etudes IFREMER"    , 
#   "APK32c3A43-Tech nucléaire CNRS 1C"      = "APK32c3-Tech nucléaire CNRS"    , 
#   "APK32h8A43-Ingé études Culture 1C"      = "APK32h8-Ingé etudes Culture"    , 
#   "APK40a3A22-Directeur rech CNRS 1C"      = "APK40a3-Directeur rech CNRS"    , 
#   "APK40b4A22-Dir recherche INSERM 1C"     = "APK40b4-Dir recherche INSERM"   , 
#   "APK40c5A22-Dir recherche INRA 1C"       = "APK40c5-Dir recherche INRA"     , 
#   "APK40e7A22-Dir recherche IRD 1C"        = "APK40e7-Dir recherche IRD"      , 
#   "APK40f9A22-Dir recherche INRIA 1C"      = "APK40f9-Dir recherche INRIA"    , 
#   "APK40h9A22-Dir recherche CEMAGREF 1C"   = "APK40h9-Dir recherche CEMAGREF" , 
#   "APK40i9A22-Astronome physicien 1C"      = "APK40i9-Astronome physicien"    , 
#   "APK40j9A22-Dir recherche INRETS 1C"     = "APK40j9-Dir recherche INRETS"   , 
#   "APK40k9A22-Dir recherche Ponts 1C"      = "APK40k9-Dir recherche Ponts"    , 
#   "APK50a3A22-CR CNRS HC"                  = "APK50a3-CR CNRS"                , 
#   "APK50b4A32-CR INSERM 1C"                = "APK50b4-CR INSERM"              , 
#   "APK50c5A32-CR INRA 1C"                  = "APK50c5-CR INRA"                , 
#   "APK50d6A32-CR vétérinaire 1C"           = "APK50d6-CR vétérinaire"         , 
#   "APK50e7A32-CR IRD 1C"                   = "APK50e7-CR IRD"                 , 
#   "APK50f9A32-CR Ponts 1C"                 = "APK50f9-CR Ponts"               , 
#   "APK50g9A32-CR CEMAGREF 1C"              = "APK50g9-CR CEMAGREF"            , 
#   "APK50h9A32-CR INRIA 1C"                 = "APK50h9-CR INRIA"               , 
#   "APK50i9A32-CR INRETS 1C"                = "APK50i9-CR INRETS"              , 
#   "APK50j9A23-Astronome phy adj HC"        = "APK50j9-Astronome phy adj"      , 
#   "APK50k9A23-Chef labo AFSS 1C"           = "APK50k9-Pers scien lab AFSSS"   , 
#   "APL01a1A23-Professeur agrégé HC"        = "APL01a1-Professeur agrégé"      , 
#   "APL10a1A33-Professeur certifié HC"      = "APL10a1-Professeur certifié"    , 
#   "APL11a1A33-Professeur EPS HC"           = "APL11a1-Professeur EPS"         , 
#   "APL11b1A33-Professeur sport HC"         = "APL11b1-Professeur sport"       , 
#   "APL12a2A33-Prof certifié Agri HC"       = "APL12a2-Prof certifié Agri"     , 
#   "APL20a1A33-Professeur écoles HC"        = "APL20a1-Professeur écoles"      , 
#   "APL30a1A33-Prof lycée pro HC"           = "APL30a1-Prof lycée pro"         , 
#   "APL30b2A33-Prof lycée pro Agri HC"      = "APL30b2-Prof lycée pro Agri"    , 
#   "APL40a1A42-Chargé d'EPS HC"             = "APL40a1-Chargé d'EPS"           , 
#   "APL40b1A33-Prof ens gal collège HC"     = "APL40b1-Prof ens gal college"   , 
#   "APL40c1A33-Prof ens gal INJS HC"        = "APL40c1-Prof ens gal INJS"      , 
#   "APL40e4A33-Prof tech PJJ HC"            = "APL40e4-Professeur tech PJJ"    , 
#   "APL40g4A33-Prof tech ens marit HC"      = "APL40g4-Prof tech ens marit"    , 
#   "APL50a1A33-CPE HC"                      = "APL50a1-CPE"                    , 
#   "APL50b2A33-CPE Agri HC"                 = "APL50b2-CPE Agri"               , 
#   "APL70c3A42-Cons Éduc Pop HC"            = "APL70c3-Conseiller Éduc Pop"    , 
#   "APL70d3A33-Cons pédago JS HC"           = "APL70d3-Conseiller pédago JS"   , 
#   "APL90a4A33-Psychologue PJJ HC"          = "APL90a4-Psychologue PJJ"        , 
#   "APL90b4A33-Psychologue EN HC"           = "APL90b4-Psychologue ÉducNat"    , 
#   "APM00a1A23-Conserv biblio Chef"         = "APM00a1-Conservateur biblio"    , 
#   "APM00b2A23-Conserv patrimoine Chef"     = "APM00b2-Conserv patrimoine"     , 
#   "APM00c2A22-Insp créa culturelle HC"     = "APM00c2-Insp créa culturelle"   , 
#   "APM10b2A42-Chargé docu 1C"              = "APM10b2-Chargé documentation"   , 
#   "APM30a3A33-Chef travaux d'art 1C"       = "APM30a3-Chef travaux d'art"     , 
#   "APM30b3A33-Ingé patrimoine Princ"       = "APM30b3-Ingénieur patrimoine"   , 
#   "B1E00a3B22-Secrétaire admin CS"         = "B1E00a3-Secrétaire admin"       , 
#   "B1E00b3B22-Secrétaire adm Agri CS"      = "B1E00b3-Secrétaire adm Agri"    , 
#   "B1E00c3B22-Secrétaire adm Cult CS"      = "B1E00c3-Secrétaire adm Cult"    , 
#   "B1E00d7B22-Secrét chancellerie CS"      = "B1E00d7-Secrét chancellerie"    , 
#   "B1E01a3B22-Secrétaire admin EN 1C"      = "B1E01a3-Secrétaire admin EN"    , 
#   "B1E02a3B22-Secrétaire adm Int CS"       = "B1E02a3-Secrétaire adm Int"     , 
#   "B1E03a3B22-Secrétaire adm DD CS"        = "B1E03a3-Secrétaire adm DD"      , 
#   "B1E03b3B22-Assist adm Aviation CS"      = "B1E03b3-Secrétaire adm Aviat"   , 
#   "B1E10a4B22-Contrôleur travail CS"       = "B1E10a4-Contrôleur travail"     , 
#   "B1E10b5B22-Contrôleur CCRF 1C"          = "B1E10b5-Contrôleur CCRF"        , 
#   "B1E10c6B22-Contrôleur INSEE 1C"         = "B1E10c6-Contrôleur INSEE"       , 
#   "B1E10d9B22-Inspecteur permis 2C"        = "B1E10d9-Inspecteur du permis"   , 
#   "B1E20a8B22-Contrôleur PTT 1C"           = "B1E20a8-Contrôleur des PTT"     , 
#   "B1E20d8B13-Vérificateur PTT Princ"      = "B1E20d8-Vérificateur PTT"       , 
#   "B1F00a1B22-Contrôleur FiP 1C"           = "B1F00a1-Contrôleur FiP"         , 
#   "B1F10a2B22-Contrôleur Douanes 1C"       = "B1F10a2-Contrôleur Douanes"     , 
#   "B1G00a1B22-Technicien sup DD Princ"     = "B1G00a1-Technicien sup DD"      , 
#   "B1G10a2B22-Tech sup Aviation Princ"     = "B1G10a2-Technicien sup Aviat"   , 
#   "B1G11a3B22-Tech sup Météo 1C"           = "B1G11a3-Technicien sup Météo"   , 
#   "B1G12a4B22-Lieutenant de port 1C"       = "B1G12a4-Officier de port adj"   , 
#   "B1G12b5B13-Géomètre IGN Princ"          = "B1G12b5-Géomètre IGN"           , 
#   "B1G13a7B22-Tech sup indus Princ"        = "B1G13a7-Tech sup industrie"     , 
#   "B1G20a6B22-Tech sup Agri Princ"         = "B1G20a6-Technicien sup Agri"    , 
#   "B1G30beB22-Dessinateur PTT Princ"       = "B1G30be-Dessinateur PTT"        , 
#   "B1G30ceB22-Chef sect lignes PTT CS"     = "B1G30ce-Chef sect lignes PTT"   , 
#   "B1G30deC02-Contremaître PTT Princ"      = "B1G30de-Contremaître PTT"       , 
#   "B1G30ieB22-Tech inst Télécom CS"        = "B1G30ie-Tech inst Télécom"      , 
#   "B1G40a8B13-Géomètre cadastre Princ"     = "B1G40a8-Géomètre FiP"           , 
#   "B1G40b8B22-Tech labo MEFI CS"           = "B1G40b8-Tech labo MEFI"         , 
#   "B1G40c8B22-Tech labo Mines CS"          = "B1G40c8-Tech labo Mines"        , 
#   "B1G40e8B13-Dessinateur MEFI 1C"         = "B1G40e8-Dessinateur  MEFI"      , 
#   "B1G41a9B22-Contrôleur SIC Int CS"       = "B1G41a9-Contrôleur SIC Int"     , 
#   "B1G41baB22-Contrôleur tech Int CS"      = "B1G41ba-Contrôleur tech Int"    , 
#   "B1G41cbB22-Secrétaire SI AE 1C"         = "B1G41cb-Secrétaire SI AE"       , 
#   "B1G41dcB22-Tech police scienti Princ"   = "B1G41dc-Tech police scienti"    , 
#   "BPI00a2B13-Greffier Princ"              = "BPI00a2-Greffier"               , 
#   "BPI10a3B12-Conseiller probation 1C"     = "BPI10a3-Conseiller probation"   , 
#   "BPI20a4B13-Éducateur PJJ 1C"            = "BPI20a4-Éducateur PJJ"          , 
#   "BPJ00a3B22-Infirmier de l’État CS"      = "BPJ00a3-Infirmier État ÉducNat" , 
#   "BPJ10a4B22-Tech sécu sanitaire Princ"   = "BPJ10a4-Tech sécu sanitaire"    , 
#   "BPK00a1B22-Tech rech form EN CS"        = "BPK00a1-Tech rech form EN"      , 
#   "BPK00b3B22-Tech recherche CNRS CS"      = "BPK00b3-Tech recherche CNRS"    , 
#   "BPK00c4B22-Tech rech INSERM CS"         = "BPK00c4-Tech rech INSERM"       , 
#   "BPK00d5B22-Tech recherche INRA CS"      = "BPK00d5-Tech recherche INRA"    , 
#   "BPK00e6B22-Tech rech form Agri CS"      = "BPK00e6-Tech rech form Agri"    , 
#   "BPK00f7B22-Tech recherche IRD CS"       = "BPK00f7-Tech recherche IRD"     , 
#   "BPK00g8B22-Tech recherche Cult CS"      = "BPK00g8-Tech recherche Cult"    , 
#   "BPK00h9B22-Tech rech CEMAGREF CS"       = "BPK00h9-Tech rech CEMAGREF"     , 
#   "BPK00i9B22-Tech recherch INRIA CS"      = "BPK00i9-Tech recherch INRIA"    , 
#   "BPL00a3B13-Assistant social Princ"      = "BPL00a3-Assistant social"       , 
#   "BPL00b4B13-Educ spé INJS 1C"            = "BPL00b4-Educ spé INJS"          , 
#   "BPM00a1B22-Biblio assist spec 1C"       = "BPM00a1-Biblio assist spec"     , 
#   "BPM00b2B22-Secrétaire docu Cult CS"     = "BPM00b2-Secrétaire docu Cult"   , 
#   "BPM10a3B22-Technicien SC BF CS"         = "BPM10a3-Technicien SC BF"       , 
#   "BPM10b3B22-Technicien d'Art CS"         = "BPM10b3-Technicien d'Art"       , 
#   "CZE00a3C02-Adjoint admin État 1C"       = "CZE00a3-Adjoint admin État"     , 
#   "CZE00b5C02-Adj contrôle CCRF 1C"        = "CZE00b5-Adj contrôle CCRF"      , 
#   "CZE00c9C02-Adjoint sanitaire Princ"     = "CZE00c9-Adjoint sanitaire"      , 
#   "CZE10a8C02-Préposé des PTT Chef"        = "CZE10a8-Préposé des PTT"        , 
#   "CZF00a1C02-Agent admin FiP Princ 2C"    = "CZF00a1-Agent admin FiP"        , 
#   "CZF10a2C02-Agent Douanes Princ 2C"      = "CZF10a2-Agent des Douanes"      , 
#   "CZG00a1C02-Chef d’équipe TPE Princ"     = "CZG00a1-Ouvrier des TPE"        , 
#   "CZG10a1C02-Dessinateur Équip Chef"      = "CZG10a1-Dessinateur Équip"      , 
#   "CZG10b1C02-Expert tech Équip Princ"     = "CZG10b1-Expert tech Équip"      , 
#   "CZG10c1C02-Agent tech envir Princ"      = "CZG10c1-Agent tech envir"       , 
#   "CZG10d4C02-Syndic mer Princ 2C"         = "CZG10d4-Syndic gens de mer"     , 
#   "CZG10e7C02-Expert technique Princ"      = "CZG10e7-Expert tech Indus"      , 
#   "CZG10f9C02-Agent SIC Int Cl2"           = "CZG10f9-Agent SIC Int"          , 
#   "CZG10gdC02-Adjoint tech Pénit 1C"       = "CZG10gd-Adjoint tech Pénit"     , 
#   "CZG10hfC02-Adjoint tech État 1C"        = "CZG10hf-Adjoint tech État"      , 
#   "CZG10ifC02-Conducteur auto HC"          = "CZG10if-Conducteur auto"        , 
#   "CZG20aeC02-Agent PTT tech"              = "CZG20ae-Agent d'exploit PTT"    , 
#   "CZG20beC02-Ouvrier PTT Princ"           = "CZG20be-Ouvrier PTT"            , 
#   "CZG30afC02-Maitre ouvrier Princ"        = "CZG30af-Ouvrier professionnel"  , 
#   "CZG40afC02-Ouvrier entretien EN 2C"     = "CZG40af-Ouvrier entretien EN"   , 
#   "CZH00a1B22-Brigadier police"            = "CZH00a1-Policier"               , 
#   "CZH10a2C02-Surv brigadier"              = "CZH10a2-Surv Pénitentiaire"     , 
#   "CZK00a1C02-Adjoint tech rech 1C"        = "CZK00a1-Adjoint tech rech"      , 
#   "CZK00b3C02-Adjoint tech CNRS Princ"     = "CZK00b3-Adjoint tech CNRS"      , 
#   "CZK00c4C02-Adjoint tech INSERM Princ"   = "CZK00c4-Adjoint tech INSERM"    , 
#   "CZK00d5C02-Adjoint tech INRA Princ"     = "CZK00d5-Adjoint tech INRA"      , 
#   "CZK00e7C02-Adjoint tech IRD Princ"      = "CZK00e7-Adjoint tech IRD"       , 
#   "CZK00f9C02-Adjoint tech INRIA Princ"    = "CZK00f9-Adjoint tech INRIA"     , 
#   "CZK00g9C02-Adjoint tec CEMAGREF Princ"  = "CZK00g9-Adjoint tec CEMAGREF"   , 
#   "CZK00h9C02-Adjoint tech IFREMER 1C"     = "CZK00h9-Adjoint tech IFREMER"   , 
#   "CZM00a1C02-Magasinier biblio Princ 2C"  = "CZM00a1-Magasinier biblio"      , 
#   "CZM01a2C02-Magasinier Culture Princ 2C" = "CZM01a2-Magasinier Culture"     #, 
# )


# last_GRAD1_in_CORPS <- c(
#   "A1D10a1A12-Insp finances Général"       =  "A1D10a1-IG Finances"            ,
#   "A1D11a2A12-Conseiller maître CC"        =  "A1D11a2-Cour des comptes"       ,
#   "A1D12a2A12-Conseiller d'État"           =  "A1D12a2-Conseil d'État"         ,
#   "A1D13a1A12-Préfet HC"                   =  "A1D13a1-Préfet"                 ,
#   #"A1D20b1A12-IG AENR 1C"                  =  "A1D20b1-IG AENR"                ,
#   #"A1D20c1A12-CG ÉcoFi 1C"                 =  "A1D20c1-CG ÉcoFi"               ,
#   "A2E00a3A13-Administrateur Général"      =  "A2E00a3-Administrateur civil"   ,
#   "A2E20a2A13-Prés section CRC"            =  "A2E20a2-Magistrat CRC"          ,
#   "A2E20b2A22-1er conseiller TA CAA"       =  "A2E20b2-Magistrat TA"           ,
#   "A2E30a1A22-Sous préfet HC"              =  "A2E30a1-Sous-préfet"            ,
#   "A2E40a6A22-Admin INSEE HC"              =  "A2E40a6-Administrateur INSEE"   ,
#   "A2E41a7A22-Conseiller MAE HC"           =  "A2E41a7-Conseiller MAE"         ,
#   "A2E42a8A13-Admin PTT HC"                =  "A2E42a8-Administrateur PTT"     ,
#   #"A2E42b3A22-Cadre sup adm autre HC"      =  "A2E42b3-Cadre sup adm autre"    ,
#   #"A2E50a3A29-Élève ENA"                   =  "A2E50a3-Élève ENA"              ,
#   "A2F00a1A12-Administrateur FiP G 1C+"    =  "A2F00a1-Administrateur FiP"     ,
#   "A2F00b2A12-Administrateur sup DDI"      =  "A2F00b2-Administrateur Douanes" ,
#   "A2G00a1A12-Ingé PEF G CE"               =  "A2G00a1-Ingénieur Ponts"        ,
#   "A2G01a1A13-Archi urbaniste Général"     =  "A2G01a1-Archi urbaniste"        ,
#   "A2G02a4A23-Admin Mer Chef"              =  "A2G02a4-Administrateur Mer"     ,
#   "A2G03a7A13-Ingé Mines Général"          =  "A2G03a7-Ingénieur Mines"        ,
#   "A2H10a1A13-Commissaire PN Général"      =  "A2H10a1-Commissaire PN"         ,
#   "A2I00a1A22-Magistrat tribunaux G1"      =  "A2I00a1-Magistrat judiciaire"   ,
#   "A3E10a3A32-Attaché HC/dir srv"          =  "A3E10a3-Attaché admin État"     ,
#   "A3E20a4A23-Directeur travail"           =  "A3E20a4-Inspecteur travail"     ,
#   "A3E30a5A13-Inspecteur CCRF Général"     =  "A3E30a5-Inspecteur CCRF"        ,
#   "A3E40a6A32-Attaché statisticien HC"     =  "A3E40a6-Attaché INSEE"          ,
#   "A3E60a8A33-Inspecteur PTT Princ"        =  "A3E60a8-Inspecteur des PTT"     ,
#   "A3E61a8A32-Receveur PTT CE"             =  "A3E61a8-Receveur des PTT"       ,
#   "A3E70a9A32-Inspecteur ASS HC/CE"        =  "A3E70a9-Inspecteur ASS"         ,
#   "A3E80a7A33-Secrétaire AE Princ"         =  "A3E80a7-Secrétaires AE"         ,
#   "A3E80b7A33-Traducteur AE Princ"         =  "A3E80b7-Traducteur AE"          ,
#   "A3E81a9A33-Délég perm conduire Princ"   =  "A3E81a9-Délég perm conduire"    ,
#   "A3E81b9A32-Inspecteur JS HC"            =  "A3E81b9-Inspecteur JS"          ,
#   "A3E81c9A13-Conseiller éco CE"           =  "A3E81c9-Attaché cons éco"       ,
#   "A3E81d9A33-Officier OFPRA Princ"        =  "A3E81d9-Officier OFPRA"         ,
#   "A3F00a1A23-Admin Fip adjoint"           =  "A3F00a1-Inspecteur FiP"         ,
#   "A3F20a2A23-Dir adj Douanes"             =  "A3F20a2-Inspecteur Douanes"     ,
#   "A3F21a2A33-Navig Douanes Cat1"          =  "A3F21a2-Naviguant Douanes"      ,
#   "A3G00a1A33-Ingénieur TPE Div"           =  "A3G00a1-Ingénieur des TPE"      ,
#   "A3G20a2A23-Ingé nav aérienne Chef"      =  "A3G20a2-Ingé nav aérienne"      ,
#   "A3G20b2A23-Ing el sécu aerienne Chef"   =  "A3G20b2-Ing el sécu aerienne"   ,
#   "A3G20c2A33-Ingé expl Aviation Princ"    =  "A3G20c2-Ingé expl Aviation C"   ,
#   "A3G30a6A32-Ingé agri envir HC"          =  "A3G30a6-Ingénieur agri envir"   ,
#   "A3G40a7A23-Ingé industrie HC"           =  "A3G40a7-Ingénieur industrie"    ,
#   "A3G50aeA33-Réviseur trav PTT Princ"     =  "A3G50ae-Réviseur trav PTT"      ,
#   "A3G50beA33-Chef trav auto PTT 1C"       =  "A3G50be-Chef trav auto PTT"     ,
#   "A3G60a3A33-Ingé trav météo Div"         =  "A3G60a3-Ingé travaux météo"     ,
#   "A3G60b5A23-Ingé IGN Chef"               =  "A3G60b5-Ingénieur IGN"          ,
#   "A3G60c4A33-Capitaine de port 1C"        =  "A3G60c4-Officier de port"       ,
#   "A3G61a9A32-Ingé SIC Int HC"             =  "A3G61a9-Ingé SIC Intérieur"     ,
#   "A3G61baA32-Ingé serv tech Int Princ"    =  "A3G61ba-Ingé serv tech Int"     ,
#   "A3G61cbA33-Attaché SI AE Princ"         =  "A3G61cb-Chiffreur chef AE"      ,
#   "A3G61dcA23-Ingé police scienti Chef"    =  "A3G61dc-Ingé police scienti"    ,
#   "A3G61edA33-Directeur tech Pénit 1C"     =  "A3G61ed-Directeur tech Pénit"   ,
#   "A3G62a8A33-Ingé méca élec MEFI CE"      =  "A3G62a8-Ingé méca élec MEFI"    ,
#   "A3G62b8A33-Ingé éco MEFI CS"            =  "A3G62b8-Ingénieur econ MEFI"    ,
#   "A3G62c8A22-Dir labo MEFI CE"            =  "A3G62c8-Pers scien labo MEFI"   ,
#   "A3H10a1A32-Commandant police"           =  "A3H10a1-Capitaine Cmd Police"   ,
#   "A3H20a2A23-Directeur serv Pénit HC"     =  "A3H20a2-Directeur serv Pénit"   ,
#   "A3H30a2A43-Commandant pénit"            =  "A3H30a2-Capitaine Cmd Pénit"    ,
#   "A3I00a2A33-Dir Greffier Princ"          =  "A3I00a2-Directeur greffier"     ,
#   "A3I01a3A33-Dir insertion prob HC"       =  "A3I01a3-Dir insertion prob"     ,
#   "A3I20a4A32-Dir srv PJJ HC"              =  "A3I20a4-Chef de service PJJ"    ,
#   "A3L00a1A22-Insp académie HC"            =  "A3L00a1-Inspecteur académie"    ,
#   "A3L00b1A23-Inspecteur EN HC"            =  "A3L00b1-Inspecteur ÉducNat"     ,
#   "A3L10a1A22-Chef d'étab scolaire EN HC"  =  "A3L10a1-Chef d'étab scolaire EN",
#   "A3L10b2A22-Chef d'étab scol Agri HC"    =  "A3L10b2-Chef d'étab scol Agri"  ,
#   "APJ00a1A12-Insp vétérinaire G CE"       =  "APJ00a1-Insp vétérinaire"       ,
#   "APJ00b1A13-Médecin santé pub Général"   =  "APJ00b1-Insp santé publique"    ,
#   "APJ00c1A13-Pharma santé pub Général"    =  "APJ00c1-Pharma insp santé"      ,
#   "APJ10a2A21-PU praticien hosp CE"        =  "APJ10a2-PU praticien hosp"      ,
#   "APJ10b2A32-MCF praticien hosp 1C"       =  "APJ10b2-MCF praticien hosp"     ,
#   "APJ30a3A23-Médecin EN 1C"               =  "APJ30a3-Médecin ÉducNat"        ,
#   "APJ40a4A22-Ingé génie sanitaire HC"     =  "APJ40a4-Ingé génie sanitaire"   ,
#   "APJ40b4A32-Ingé étud sanitaires Princ"  =  "APJ40b4-Ingé étud sanitaires"   ,
#   "APK00a1A21-Professeur univ CE"          =  "APK00a1-Professeur des univ"    ,
#   "APK00c1A21-PU médecine g CE"            =  "APK00c1-PU médecine générale"   ,
#   "APK00d1A21-Dir études EHESS CE"         =  "APK00d1-Dir études EHESS"       ,
#   "APK00e1A21-Dir étu EHPE ENC EO CE"      =  "APK00e1-Dir étu EHPE ENC EO"    ,
#   "APK01a2A21-Professeur sup Agri CE"      =  "APK01a2-Professeur sup Agri"    ,
#   "APK01b2A21-Professeur MNHN CE"          =  "APK01b2-Professeur MNHN"        ,
#   "APK01c2A21-Professeur Mines CE"         =  "APK01c2-Professeur Mines"       ,
#   "APK01e2A21-Professeur ENSA CE"          =  "APK01e2-Professeur ENSA"        ,
#   "APK01g2A23-Professeur ENSAM HC"         =  "APK01g2-Professeur ENSAM"       ,
#   "APK01h2A23-Professeur d'Art 1C"         =  "APK01h2-Professeur d'Art"       ,
#   "APK01i2A22-Prof polytechnique"          =  "APK01i2-Prof polytechnique"     ,
#   "APK10a1A23-Maitre de conférences HC"    =  "APK10a1-Maitre de conférence"   ,
#   "APK10b1A23-MCF EHESS HC"                =  "APK10b1-MCF EHESS"              ,
#   "APK10c1A23-MCF EPEH ENC EO HC"          =  "APK10c1-MCF EPEH ENC EO"        ,
#   "APK10d2A23-MCF Agri HC"                 =  "APK10d2-MCF Agri"               ,
#   "APK10e2A23-Maître assistant Mines HC"   =  "APK10e2-MCF Mines"              ,
#   "APK10f2A23-MCF ENSA HC"                 =  "APK10f2-MCF ENSA"               ,
#   "APK10g2A23-MCF MNHN HC"                 =  "APK10g2-MCF MNHN"               ,
#   "APK20b1A22-Ingé recherche CNRS HC"      =  "APK20b1-Chargé mission ensei"   ,
#   "APK30a1A22-Ingé recherche EN HC"        =  "APK30a1-Ingé recherche EN"      ,
#   "APK30b3A22-Ingé recherche CNRS HC"      =  "APK30b3-Ingé recherche CNRS"    ,
#   "APK30c3A22-Ingénieur nucléaire Cat1"    =  "APK30c3-Ingénieur nucléaire"    ,
#   "APK30d4A22-Ingé rech INSERM HC"         =  "APK30d4-Ingé rech INSERM"       ,
#   "APK30e5A23-Ingé recherche INRA HC"      =  "APK30e5-Ingé recherche INRA"    ,
#   "APK30f6A22-Ingé recherche Agri HC"      =  "APK30f6-Ingé recherche Agri"    ,
#   "APK30g7A22-Ingé recherche IRD HC"       =  "APK30g7-Ingé recherche IRD"     ,
#   "APK30h8A22-Ingé recherche Cult HC"      =  "APK30h8-Ingé recherche Cult"    ,
#   "APK30i9A22-Ingé rech CEMAGREF HC"       =  "APK30i9-Ingé rech CEMAGREF"     ,
#   "APK30k9A22-Ingé rech INRETS HC"         =  "APK30k9-Ingé rech INRETS"       ,
#   "APK30l9A22-Ingé rech INRIA HC"          =  "APK30l9-Ingé rech INRIA"        ,
#   "APK31a1A33-Ingé études EN HC"           =  "APK31a1-Ingé études EN"         ,
#   "APK31b3A23-Ingé études CNRS HC"         =  "APK31b3-Ingé études CNRS"       ,
#   "APK31c4A33-Ingé études INSERM HC"       =  "APK31c4-Ingé études INSERM"     ,
#   "APK31d5A33-Ingé études INRA HC"         =  "APK31d5-Ingé études INRA"       ,
#   "APK31e6A32-Ingé études Agri HC"         =  "APK31e6-Ingé études Agri"       ,
#   "APK31f7A42-Ingé études IRD 1C"          =  "APK31f7-Ingé études IRD"        ,
#   "APK31h9A42-Ingé études INRIA 1C"        =  "APK31h9-Ingé études INRIA"      ,
#   "APK31i9A42-Ingé études INRETS 1C"       =  "APK31i9-Ingé études INRETS"     ,
#   "APK31j9A42-Ingé études IFREMER 1C"      =  "APK31j9-Ingé etudes IFREMER"    ,
#   "APK32c3A33-Tech nucléaire CNRS Princ"   =  "APK32c3-Tech nucléaire CNRS"    ,
#   "APK32h8A23-Ingé études Culture HC"      =  "APK32h8-Ingé etudes Culture"    ,
#   "APK40a3A21-Directeur rech CNRS CE"      =  "APK40a3-Directeur rech CNRS"    ,
#   "APK40b4A21-Dir recherche INSERM CE"     =  "APK40b4-Dir recherche INSERM"   ,
#   "APK40c5A21-Dir recherche INRA CE"       =  "APK40c5-Dir recherche INRA"     ,
#   "APK40e7A21-Dir recherche IRD CE"        =  "APK40e7-Dir recherche IRD"      ,
#   "APK40f9A21-Dir recherche IRD CE"        =  "APK40f9-Dir recherche INRIA"    ,
#   "APK40h9A21-Dir recherche CEMAGREF CE"   =  "APK40h9-Dir recherche CEMAGREF" ,
#   "APK40i9A21-Astronome physicien CE"      =  "APK40i9-Astronome physicien"    ,
#   "APK40j9A21-Dir recherche INRETS CE"     =  "APK40j9-Dir recherche INRETS"   ,
#   "APK40k9A22-Dir recherche Ponts 1C"      =  "APK40k9-Dir recherche Ponts"    ,
#   "APK50a3A22-CR CNRS HC"                  =  "APK50a3-CR CNRS"                ,
#   "APK50b4A32-CR INSERM 1C"                =  "APK50b4-CR INSERM"              ,
#   "APK50c5A32-CR INRA 1C"                  =  "APK50c5-CR INRA"                ,
#   "APK50d6A32-CR vétérinaire 1C"           =  "APK50d6-CR vétérinaire"         ,
#   "APK50e7A32-CR IRD 1C"                   =  "APK50e7-CR IRD"                 ,
#   "APK50f9A32-CR Ponts 1C"                 =  "APK50f9-CR Ponts"               ,
#   "APK50g9A32-CR CEMAGREF 1C"              =  "APK50g9-CR CEMAGREF"            ,
#   "APK50h9A32-CR INRIA 1C"                 =  "APK50h9-CR INRIA"               ,
#   "APK50i9A32-CR INRETS 1C"                =  "APK50i9-CR INRETS"              ,
#   "APK50j9A23-Astronome phy adj HC"        =  "APK50j9-Astronome phy adj"      ,
#   "APK50k9A22-Dir labo AFSS HC"            =  "APK50k9-Pers scien lab AFSSS"   ,
#   "APL01a1A22-Professeur agrégé CE"        =  "APL01a1-Professeur agrégé"      ,
#   "APL10a1A32-Professeur certifié CE"      =  "APL10a1-Professeur certifié"    ,
#   "APL11a1A32-Professeur EPS CE"           =  "APL11a1-Professeur EPS"         ,
#   "APL11b1A32-Professeur sport CE"         =  "APL11b1-Professeur sport"       ,
#   "APL12a2A32-Prof certifié Agri CE"       =  "APL12a2-Prof certifié Agri"     ,
#   "APL20a1A32-Professeur écoles CE"        =  "APL20a1-Professeur écoles"      ,
#   "APL30a1A32-Prof lycée pro CE"           =  "APL30a1-Prof lycée pro"         ,
#   "APL30b2A32-Prof lycée pro Agri CE"      =  "APL30b2-Prof lycée pro Agri"    ,
#   "APL40a1A33-Chargé d'EPS CE"             =  "APL40a1-Chargé d'EPS"           ,
#   "APL40b1A32-Prof ens gal collège CE"     =  "APL40b1-Prof ens gal college"   ,
#   "APL40c1A33-Prof ens gal INJS HC"        =  "APL40c1-Prof ens gal INJS"      ,
#   "APL40e4A33-Prof tech PJJ HC"            =  "APL40e4-Professeur tech PJJ"    ,
#   "APL40g4A33-Prof tech ens marit HC"      =  "APL40g4-Prof tech ens marit"    ,
#   "APL50a1A32-CPE CE"                      =  "APL50a1-CPE"                    ,
#   "APL50b2A33-CPE Agri HC"                 =  "APL50b2-CPE Agri"               ,
#   "APL70c3A33-Cons Éduc Pop CE"            =  "APL70c3-Conseiller Éduc Pop"    ,
#   "APL70d3A33-Cons pédago JS HC"           =  "APL70d3-Conseiller pédago JS"   ,
#   #"APL80a4A49-Élève EN Chartes"            =  "APL80a4-Élève EN Chartes"       ,
#   #"APL80b4A49-Élève ENS"                   =  "APL80b4-Élève ENS"              ,
#   "APL90a4A33-Psychologue PJJ HC"          =  "APL90a4-Psychologue PJJ"        ,
#   "APL90b4A32-Psychologue EN CE"           =  "APL90b4-Psychologue ÉducNat"    ,
#   "APM00a1A23-Conserv biblio Chef"         =  "APM00a1-Conservateur biblio"    ,
#   "APM00b2A23-Conserv patrimoine Chef"     =  "APM00b2-Conserv patrimoine"     ,
#   "APM00c2A13-Insp créa culturelle Général"=  "APM00c2-Insp créa culturelle"   ,
#   "APM10b2A33-Chargé docu Princ"           =  "APM10b2-Chargé documentation"   ,
#   "APM30a3A33-Chef travaux d'art 1C"       =  "APM30a3-Chef travaux d'art"     ,
#   "APM30b3A32-Ingé patrimoine CS"          =  "APM30b3-Ingénieur patrimoine"   ,
#   "B1E00a3B12-Secrétaire admin CE"         =  "B1E00a3-Secrétaire admin"       ,
#   "B1E00b3B13-Secrétaire adm Agri CE"      =  "B1E00b3-Secrétaire adm Agri"    ,
#   "B1E00c3B13-Secrétaire adm Cult CE"      =  "B1E00c3-Secrétaire adm Cult"    ,
#   "B1E00d7B13-Secrét chancellerie CE"      =  "B1E00d7-Secrét chancellerie"    ,
#   "B1E01a3B13-Secrétaire admin EN CE"      =  "B1E01a3-Secrétaire admin EN"    ,
#   "B1E02a3B12-Secrétaire adm Int CE"       =  "B1E02a3-Secrétaire adm Int"     ,
#   "B1E03a3B13-Secrétaire adm DD CE"        =  "B1E03a3-Secrétaire adm DD"      ,
#   "B1E03b3B13-Assist adm Aviation CE"      =  "B1E03b3-Secrétaire adm Aviat"   ,
#   "B1E10a4B13-Contrôleur travail CE"       =  "B1E10a4-Contrôleur travail"     ,
#   "B1E10b5B13-Contrôleur CCRF Princ"       =  "B1E10b5-Contrôleur CCRF"        ,
#   "B1E10c6B13-Contrôleur INSEE Princ"      =  "B1E10c6-Contrôleur INSEE"       ,
#   "B1E10d9B13-Inspecteur permis 1C"        =  "B1E10d9-Inspecteur du permis"   ,
#   "B1E20a8B13-Contrôleur PTT Div"          =  "B1E20a8-Contrôleur des PTT"     ,
#   "B1E20d8B12-Vérificateur PTT CE"         =  "B1E20d8-Vérificateur PTT"       ,
#   "B1F00a1B13-Contrôleur FiP Princ"        =  "B1F00a1-Contrôleur FiP"         ,
#   "B1F10a2B13-Contrôleur Douanes Princ"    =  "B1F10a2-Contrôleur Douanes"     ,
#   "B1G00a1B13-Technicien sup DD Chef"      =  "B1G00a1-Technicien sup DD"      ,
#   "B1G10a2B13-Tech sup Aviation CE"        =  "B1G10a2-Technicien sup Aviat"   ,
#   "B1G11a3B13-Tech sup Météo Chef"         =  "B1G11a3-Technicien sup Météo"   ,
#   "B1G12a4B22-Lieutenant de port 1C"       =  "B1G12a4-Officier de port adj"   ,
#   "B1G12b5B13-Géomètre IGN Princ"          =  "B1G12b5-Géomètre IGN"           ,
#   "B1G13a7B13-Tech sup indus Chef"         =  "B1G13a7-Tech sup industrie"     ,
#   "B1G20a6B13-Tech sup Agri Chef"          =  "B1G20a6-Technicien sup Agri"    ,
#   "B1G30beB13-Dessinateur PTT Chef"        =  "B1G30be-Dessinateur PTT"        ,
#   "B1G30ceB13-Chef sect lignes PTT Chef"   =  "B1G30ce-Chef sect lignes PTT"   ,
#   "B1G30deC02-Contremaître PTT Princ"      =  "B1G30de-Contremaître PTT"       ,
#   "B1G30ieB13-Tech inst Télécom Chef"      =  "B1G30ie-Tech inst Télécom"      ,
#   "B1G40a8B13-Géomètre cadastre Princ"     =  "B1G40a8-Géomètre FiP"           ,
#   "B1G40b8B13-Tech labo MEFI CE"           =  "B1G40b8-Tech labo MEFI"         ,
#   "B1G40c8B13-Tech labo Mines CE"          =  "B1G40c8-Tech labo Mines"        ,
#   "B1G40e8B13-Dessinateur MEFI 1C"         =  "B1G40e8-Dessinateur  MEFI"      ,
#   "B1G41a9B13-Contrôleur SIC Int CE"       =  "B1G41a9-Contrôleur SIC Int"     ,
#   "B1G41baB13-Contrôleur tech Int CE"      =  "B1G41ba-Contrôleur tech Int"    ,
#   "B1G41cbB13-Secrétaire SI AE HC"         =  "B1G41cb-Secrétaire SI AE"       ,
#   "B1G41dcB13-Tech police scienti Chef"    =  "B1G41dc-Tech police scienti"    ,
#   "BPI00a2B13-Greffier Princ"              =  "BPI00a2-Greffier"               ,
#   "BPI10a3A43-Conseiller probation HC"     =  "BPI10a3-Conseiller probation"   ,
#   "BPI20a4B13-Éducateur PJJ 1C"            =  "BPI20a4-Éducateur PJJ"          ,
#   "BPJ00a3A43-Infirmier de l’État HC"      =  "BPJ00a3-Infirmier État ÉducNat" ,
#   "BPJ10a4B13-Tech sécu sanitaire Chef"    =  "BPJ10a4-Tech sécu sanitaire"    ,
#   "BPK00a1B13-Tech rech form EN CE"        =  "BPK00a1-Tech rech form EN"      ,
#   "BPK00b3B13-Tech recherche CNRS CE"      =  "BPK00b3-Tech recherche CNRS"    ,
#   "BPK00c4B13-Tech rech INSERM CE"         =  "BPK00c4-Tech rech INSERM"       ,
#   "BPK00d5B13-Tech recherche INRA CE"      =  "BPK00d5-Tech recherche INRA"    ,
#   "BPK00e6B13-Tech rech form Agri CE"      =  "BPK00e6-Tech rech form Agri"    ,
#   "BPK00f7B13-Tech recherche IRD CE"       =  "BPK00f7-Tech recherche IRD"     ,
#   "BPK00g8B13-Tech recherche Cult CE"      =  "BPK00g8-Tech recherche Cult"    ,
#   "BPK00h9B13-Tech rech CEMAGREF CE"       =  "BPK00h9-Tech rech CEMAGREF"     ,
#   "BPK00i9B13-Tech recherch INRIA CE"      =  "BPK00i9-Tech recherch INRIA"    ,
#   "BPL00a3B13-Assistant social Princ"      =  "BPL00a3-Assistant social"       ,
#   "BPL00b4B13-Educ spé INJS 1C"            =  "BPL00b4-Educ spé INJS"          ,
#   "BPM00a1B12-Biblio assist spec HC"       =  "BPM00a1-Biblio assist spec"     ,
#   "BPM00b2B13-Secrétaire docu Cult CE"     =  "BPM00b2-Secrétaire docu Cult"   ,
#   "BPM10a3B13-Technicien SC BF CE"         =  "BPM10a3-Technicien SC BF"       ,
#   "BPM10b3B13-Technicien d'Art Chef"       =  "BPM10b3-Technicien d'Art"       ,
#   "CZE00a3C00-Adjoint admin État Princ 1C" =  "CZE00a3-Adjoint admin État"     ,
#   "CZE00b5C00-Adj contrôle CCRF Princ 1C"  =  "CZE00b5-Adj contrôle CCRF"      ,
#   "CZE00c9C02-Adjoint sanitaire Princ"     =  "CZE00c9-Adjoint sanitaire"      ,
#   "CZE10a8C02-Préposé des PTT Chef"        =  "CZE10a8-Préposé des PTT"        ,
#   "CZF00a1C01-Agent admin FiP Princ 1C"    =  "CZF00a1-Agent admin FiP"        ,
#   "CZF10a2C01-Agent Douanes Princ 1C"      =  "CZF10a2-Agent des Douanes"      ,
#   "CZG00a1C02-Chef d’équipe TPE Princ"     =  "CZG00a1-Ouvrier des TPE"        ,
#   "CZG10a1C02-Dessinateur Équip Chef"      =  "CZG10a1-Dessinateur Équip"      ,
#   "CZG10b1C02-Expert tech Équip Princ"     =  "CZG10b1-Expert tech Équip"      ,
#   "CZG10c1C02-Agent tech envir Princ"      =  "CZG10c1-Agent tech envir"       ,
#   "CZG10d4C01-Syndic mer Princ 1C"         =  "CZG10d4-Syndic gens de mer"     ,
#   "CZG10e7C02-Expert technique Princ"      =  "CZG10e7-Expert tech Indus"      ,
#   "CZG10f9C01-Agent SIC Int Cl1"           =  "CZG10f9-Agent SIC Int"          ,
#   "CZG10gdB23-Adjoint tech Pénit Chef"     =  "CZG10gd-Adjoint tech Pénit"     ,
#   "CZG10hfC01-Adjoint tech État Princ"     =  "CZG10hf-Adjoint tech État"      ,
#   "CZG10ifC02-Conducteur auto HC"          =  "CZG10if-Conducteur auto"        ,
#   "CZG20aeC02-Agent PTT service Princ"     =  "CZG20ae-Agent d'exploit PTT"    ,
#   "CZG20beC02-Ouvrier PTT Princ"           =  "CZG20be-Ouvrier PTT"            ,
#   "CZG30afC02-Maitre ouvrier Princ"        =  "CZG30af-Ouvrier professionnel"  ,
#   "CZG40afC00-Ouvrier entretien EN Chef"   =  "CZG40af-Ouvrier entretien EN"   ,
#   "CZH00a1B12-Major de police"             =  "CZH00a1-Policier"               ,
#   "CZH10a2C00-Major pénitentiaire"         =  "CZH10a2-Surv Pénitentiaire"     ,
#   "CZK00a1C00-Adjoint tech rech Princ 1C"  =  "CZK00a1-Adjoint tech rech"      ,
#   "CZK00b3C02-Adjoint tech CNRS Princ"     =  "CZK00b3-Adjoint tech CNRS"      ,
#   "CZK00c4C02-Adjoint tech INSERM Princ"   =  "CZK00c4-Adjoint tech INSERM"    ,
#   "CZK00d5C02-Adjoint tech INRA Princ"     =  "CZK00d5-Adjoint tech INRA"      ,
#   "CZK00e7C02-Adjoint tech IRD Princ"      =  "CZK00e7-Adjoint tech IRD"       ,
#   "CZK00f9C02-Adjoint tech INRIA Princ"    =  "CZK00f9-Adjoint tech INRIA"     ,
#   "CZK00g9C02-Adjoint tec CEMAGREF Princ"  =  "CZK00g9-Adjoint tec CEMAGREF"   ,
#   "CZK00h9C01-Adjoint tech IFREMER Princ"  =  "CZK00h9-Adjoint tech IFREMER"   ,
#   "CZM00a1C01-Magasinier biblio Princ 1C"  =  "CZM00a1-Magasinier biblio"      ,
#   "CZM01a2C01-Magasinier Culture Princ 1C" =  "CZM01a2-Magasinier Culture"     #,
# )
# #last_GRAD1_in_CORPS <- last_GRAD1_in_CORPS[!names(last_GRAD1_in_CORPS) %in% names(second_GRAD1_in_CORPS)]
# 












# Geometrical data analysis ------------------------------------------------------------------
burt_table <- function(data, vars, wt, pct = c("all", "row", "col"), 
                       excl, cleannames = TRUE, na = "drop") {
  vars <- names(tidyselect::eval_select(rlang::enquo(vars), data))
  wt <- if (tabxplor:::quo_miss_na_null_empty_no(rlang::enquo(wt))) {character()} else {rlang::ensym(wt)}
  
  first_lvs <- dplyr::select(data, tidyselect::all_of(vars)) |>
    purrr::map_chr(~ dplyr::if_else(nlevels(.) == 2L, "first", "all"))
  
  if (!missing(excl)) data <- levels_to_na(data, tidyselect::all_of(vars), excl = excl)
  
  burt <- data |> 
    tabxplor::tab_many(tidyselect::all_of(vars), 
                       tidyselect::all_of(vars), 
                       wt         = !!wt, 
                       pct        = pct[1], 
                       color      = "diff", 
                       levels     = first_lvs,
                       cleannames = cleannames, 
                       na         = na
    )
  
  burt <- purrr::map(burt, ~dplyr::rename_with(., ~ "row_var", 1) |> 
                       dplyr::rename_with(~ dplyr::if_else(stringr::str_detect(., "Total_"), "Total", .)) |>
                       dplyr::mutate(vars = factor(names(.)[1]), .before = 1)
  ) |> 
    purrr::map_if(1:length(burt) != length(burt),
                  ~ dplyr::filter(., !tabxplor::is_totrow(Total)), 
                  .else = ~ dplyr::mutate(., vars = dplyr::if_else(row_var == "Total",
                                                                   factor("All", c(levels(.data$vars), "All")),
                                                                   .data$vars))) 
  
  burt <- purrr::map_if(burt, purrr::map_lgl(burt, ~ nrow(.) == 2), ~ dplyr::slice(., 1)) |>
    dplyr::bind_rows() |>
    dplyr::group_by(vars)
  
  burt
}


levels_to_na <- function(data, vars, excl) {
  if (length(excl) == 0) return(data)
  
  vars <- names(tidyselect::eval_select(rlang::enquo(vars), data))
  excl <- paste0(excl, collapse = "|")
  
  data <- data |> 
    dplyr::mutate(dplyr::across(
      all_of(vars) & where(~ any(stringr::str_detect(levels(.), excl))), 
      ~ forcats::fct_recode(., purrr::set_names(
        levels(.)[stringr::str_detect(levels(.), excl)],
        "NULL" 
      ))
    ))
  
  data
}


mca_var_table <- function(res.mca, field, cleannames = TRUE) {
  active_variables <- stringr::str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])
  excl <- names(res.mca$call$Xtot)[res.mca$call$excl]
  
  active_var_levels <-
    purrr::map(active_variables, ~ dplyr::pull(res.mca$call$X, .) %>%
                 as.factor() %>% levels()) %>%
    purrr::set_names(active_variables) %>%
    purrr::imap_dfr(~ tibble::tibble(vars = factor(.y), lvs = .x))
  
  freqs    <- tibble::enframe(res.mca$call$marge.col, "lvs", "freq")
  field_data   <- tibble::as_tibble(res.mca$var[[field]], rownames = "lvs")
  
  #dims <- names(field)[stringr::str_detect(names(field), "Dim ")]
  
  active_vars_data <- active_var_levels %>%
    dplyr::left_join(field_data, by = "lvs") %>%
    dplyr::filter(!is.na(.data$`Dim 1`)) %>% 
    dplyr::mutate(across(where(is.numeric), ~ . / if_else(field == "cos2", 1, 100)))
  
  coords <- active_var_levels %>% 
    dplyr::left_join(tibble::as_tibble(res.mca$var$coord, rownames = "lvs"), by = "lvs") %>% 
    dplyr::filter(!is.na(.data$`Dim 1`)) 
  
  if (field == "cos2") {
    totcol <- rowSums(active_vars_data[stringr::str_detect(names(active_vars_data), "Dim ")])
    active_vars_data <- active_vars_data %>% dplyr::mutate(Total = totcol)
    coords           <- coords           %>% dplyr::mutate(Total = 0)
  } 
  
  active_vars_data <- dplyr::bind_cols(select(active_vars_data, where(~ !is.numeric(.))), 
                                       purrr::map2(select(active_vars_data, where(is.numeric)), 
                                                   select(coords, where(is.numeric)),
                                                   ~ tabxplor:::new_fmt(type    = if_else(field == "cos2", "row", "col"), 
                                                                        n       = rep(0L, length(.x)),
                                                                        display = rep("pct", length(.x)),
                                                                        pct     = .x, 
                                                                        diff    = .x * sign(.y), 
                                                                        color   = "diff")
                                       )
  ) 
  
  if (field == "cos2") active_vars_data <- active_vars_data %>% dplyr::mutate(Total = as_totcol(Total))
  
  if (field != "cos2") {
    totrows <- active_vars_data %>% 
      dplyr::summarise(vars = factor("Total"), lvs = factor("Total"),
                       across(where(is_fmt), 
                              ~ tabxplor:::new_fmt(type      = if_else(field == "cos2", "row", "col"),  
                                                   pct       = sum(.$pct, na.rm = TRUE), 
                                                   n         = 0L, 
                                                   display   = "pct",
                                                   diff      = 0, 
                                                   color     = tabxplor::get_color(.), 
                                                   in_totrow = TRUE
                              )
                       ))
    active_vars_data <- active_vars_data %>% bind_rows(totrows)
  }
  
  active_vars_data <- active_vars_data %>% 
    dplyr::left_join(freqs, by = "lvs") %>%
    dplyr::group_by(.data$vars) %>%
    dplyr::mutate(freq = round(.data$freq/sum(.data$freq) * 100, 0)) %>%
    dplyr::ungroup()
  
  active_vars_data <- active_vars_data %>%
    dplyr::mutate(lvs = stringr::str_remove(.data$lvs, stringr::str_c("^", .data$vars, "_")))
  
  if (cleannames == TRUE) active_vars_data <- active_vars_data %>%
    dplyr::mutate(lvs = forcats::fct_relabel(.data$lvs, ~ stringr::str_remove_all(., tabxplor:::cleannames_condition())))
  
  active_vars_data <- active_vars_data |> rename_with(~ str_replace(., "Dim ", "Axe"))
  
  tabxplor::new_tab(active_vars_data) #|> group_by(vars)
}
#mca_var_table(res.mca, "cos2")
#mca_var_table(res.mca, "contrib")


HCPC_parangons <- function(HCPC, abrevs) {
  parangons <-  purrr::map(1:length(HCPC$desc.ind$para), 
                           ~ as.integer(names(HCPC$desc.ind$para[[.]]))) |> 
    purrr::flatten_int()
  
  res <- 
    as_tibble(HCPC$data.clust[parangons, ]) |> 
    mutate(clust = as.factor(clust)) |> 
    mutate(across(
      everything(), 
      ~ fct_relabel(., ~ str_remove(., paste0(cur_column(), "_")) |> 
                      str_remove_all(tabxplor:::cleannames_condition()) )
    )) |>
    select(clust, everything()) 
  
  
  
  if (!missing(abrevs)) {
    res <- res |> 
      mutate(across(
        where(~ any(str_detect(levels(.), paste0(abrev, collapse = "|")))), 
        ~  fct_relabel(.,  ~ reduce2(abrev, names(abrev), .init = ., 
                                     ~ if_else(str_detect(..1, ..2), ..3, ..1) ) )
      ))
  }
  
  new_tab(res) |>  group_by(clust)
}
#data_parangons <- HCPC_parangons(cah_orga2_2) |> ungroup() |> select(-clust) 
#tabxplor:::fct_recode_helper(data_parangons, everything())


# wt = "pondqaa"
# vars <- "OBJtous"
HCPC_tab_active <- function(clust, data, wt, vars = character(), 
                            excl = character(), recode_helper = TRUE, 
                            color = "diff", ...) {
  #active <- names(CAH$data.clust)[names(CAH$data.clust) != "clust"]
  
  data <- data |> select(all_of(c(wt, vars))) |> 
    levels_to_na(all_of(vars), excl = excl)
  
  data <- data |> tibble::add_column(clust = clust)
  
  first_lvs <- select(data, all_of(c(vars))) |> 
    map_chr(~ if_else(nlevels(.) == 2L, "first", "all"))
  
  if(recode_helper) tabxplor:::fct_recode_helper(data, "clust")
  
  tab_many(data, clust, all_of(c( vars)), pct = "row", wt = !!sym(wt),
           na = "drop", cleannames = TRUE, color = color, levels = first_lvs, ...) |>
    rename_with(~ if_else(str_detect(., "Total_", ), "Total", .)) |>
    mutate(Total = mutate(Total, pct = wn / last(wn)) )
}


#data <- ct[salariat & `2013`, ]
#Provide either CAH or clust
HCPC_add_clust <- function(data, filter, CAH, clust, recode) {
  if (!missing(CAH)) {
    name <- as_name(rlang::enquo(CAH))
    clust <- CAH$data.clust$clust
  } else if (!missing(clust)) {
    name <-  as_name(rlang::enquo(clust))
    clust <- clust
  }
  
  if (!missing(recode))  clust <- fct_recode(clust, !!!recode)
  
  data <- mutate(data, !!sym(name) := factor(NA, levels(clust)))
  
  data[filter, ] <- mutate(data[filter, ], !!sym(name) := clust)
  
  data
}


#Don't work with k-means pre-clusters
HCPC_cut_tree <- function(CAH, nb.clust) {
  new_clust <- cutree(CAH$call$t$tree, nb.clust)
  new_clust <- as.factor(unname(new_clust[order(as.integer(names(new_clust)))]))
  
  tibble::tibble(old_clust = CAH$data.clust$clust, new_clust = new_clust) |>
    dplyr::distinct() |>
    dplyr::arrange(new_clust) |> 
    tidyr::unite("old >> new", everything(), sep = "  >>  ") |>
    tibble::deframe() |>
    writeLines()
  
  new_clust 
} 

# #Test that groups are right with `nb.clust = 3`
# new_clust <- cutree(cah_orga2_3$call$t$tree, 3)
# identical(as.factor(unname(new_clust[order(as.integer(names(new_clust)))])),
#           cah_orga2_3$data.clust$clust)

# identical(HCPC_cut_tree(cah_orga2_3, 3), cah_orga2_3$data.clust$clust)



#axes.weight <- "eigen"
#axes <- 1:3

# Clusters over profiles of answers instead of individuals
# Weights are taken into account at two moments : 
# https://stat.ethz.ch/pipermail/r-help/2004-January/044769.html
#   1) when building the matrix distance, consdering that "if two pairs of observations 
#   are of equal euclidian distance, the pair with a higher weight should be considered of
#   greater distance".
#   2) when clustering (calculation of the ward criterion with weighted sum of observations)
HCPCprofiles <- function(res.mca, axes = 1:res.mca$call$ncp, nb.clust = -1, profiles = TRUE,
                         axes.weight = NULL, min = 3, max = NULL, square = TRUE,
                         mode = c("weight_all", "weight_clust", "no_weight"), 
                         print_tree = FALSE) {
  
  if (profiles) {
    ID_profiles <- bind_cols(res.mca$call$X, row.w = res.mca$call$row.w) |>
      mutate(ID = row_number()) |>
      group_by(across(-c(ID, row.w))) |> 
      mutate(ID1 = first(ID), row.w = sum(row.w, na.rm = TRUE)) |> 
      ungroup() |>
      select(ID1, row.w)
    
    X <- as_tibble(res.mca$ind$coord)[axes] |> 
      bind_cols(ID_profiles) |> 
      distinct(ID1, .keep_all = TRUE)
    # mutate(ok = ID == ID1) |> pull(ok) |> all()
    
    ID1    <- X$ID1
    row.w <- X$row.w
    X     <- dplyr::select(X, -any_of(c("ID1", "row.w")))
    
  } else {
    X     <- as_tibble(res.mca$ind$coord)[axes]
    row.w <- res.mca$call$row.w
    ID_profiles <- NULL
    ID1 <- NULL
  }
  
  if (is.null(max)) max <- min(10, round(nrow(X)/2))
  max <- min(max, nrow(X) - 1)
  
  if (length(axes.weight) != 0) {
    if (length(axes.weight) == 1 & any(axes.weight == "eigen")) axes.weight <- 
        res.mca$eig$`percentage of variance`[axes]/mean(res.mca$eig$`percentage of variance`[axes])
    axes.weight <- vctrs::vec_recycle(axes.weight, length(axes))
    X <- mutate(X, across(everything(), ~ . * axes.weight[which(names(X) == cur_column())]))
  }
  message(paste("clustering on", paste(names(X), collapse = ", "), "; over", nrow(X), 
                if_else(profiles, "profiles", "individuals")  ))
  
  # Higher weight -> greater distance, as in FactoMineR::HCPC (ex 9.33 sec)
  if (mode[1] == "weight_all") {
    do    <- Rfast::Dist(X, method = "euclidean", square = square) #dist(X, method = "euclidean")^2
    eff   <- outer(row.w, row.w, FUN = function(x,y,n) {x*y/n/(x+y)}, n = sum(row.w))
    dissi <- suppressWarnings(as.dist(do*eff[lower.tri(eff)]))
    hc   <- flashClust::hclust(dissi, method = "ward", members = row.w)  
    
    # Weight in hclust only (ex 5.99 sec)
  } else if (mode[1] == "weight_clust" ) {
    dissi <- as.dist(Rfast::Dist(X, method = "euclidean", square = square)) #idem ^2 but faster
    hc <- flashClust::hclust(dissi, method = "ward", members = row.w)
    
    # No weights in hclust
  } else if (mode[1] == "no_weight") {
    dissi <- as.dist(Rfast::Dist(X, method = "euclidean", square = square)) #idem ^2 but faster
    hc <- flashClust::hclust(dissi, method = "ward")
    
  } else {
    stop("mode must be 'weight_all', 'weight_clust' or 'no_weight'")
  }
  
  if (print_tree) {
    oldpar <- par() 
    par(mfrow = c(1, 2))
    plot(as.dendrogram(hc) , leaflab = "none", main = "")   
    inertia_drop <- sort(hc$height, decreasing = TRUE)[1:20]
    plot(inertia_drop, type = "s")
    suppressWarnings(par(oldpar))
  }
  
  if (nb.clust < 1) {
    inert.gain <- rev(hc$height)
    #if (!is.null(cla)) inert.gain <- c(inert.gain,cla$tot.withinss/sum(cla$size))
    intra <- rev(cumsum(rev(inert.gain)))
    quot = intra[min:(max)]/intra[(min - 1):(max - 1)] 
    nb.clust = which.min(quot) + min - 1
  }
  
  clust <- as.factor(cutree(hc, nb.clust))
  
  if (profiles) {
    clust <- tibble(clust, ID1)
    clust <- select(ID_profiles, ID1) |> 
      left_join(clust, by = "ID1") |> 
      pull(clust)
  }
  
  return(list(clust = clust, tree = hc, ID_profiles = ID_profiles, ID1 = ID1, X = X))
}

consolidation = function(res, iter.max = 10, X, clust, ...) {
  if(!missing(res)) {
    X     <- res$X
    clust <- res$clust
  }
  
  centers = NULL
  centers = by(X, clust, colMeans)
  centers = matrix(unlist(centers), ncol = ncol(X), byrow = TRUE)
  km = kmeans(X, centers = centers, iter.max = iter.max, ...)
  return(as.factor(km$cluster))
}


HCPC_recut <- function(tree, nb.clust, consol = FALSE) {
  clust <- as.factor(cutree(tree$tree, nb.clust))
  
  if (consol) clust <- consolidation(clust = clust, X = tree$X)
  
  if(! is.null(tree$ID1)) {
    clust <- tibble(clust = clust, ID1 = tree$ID1)
    clust <- tree$ID_profiles |> 
      left_join(clust, by = "ID1") |> 
      pull(clust)
  }
  
  clust
}



# # plotmath 
# plotmath_test <- tibble(label = c('x["x subscript i"]',
#                                   'x^superscript',
#                                   'plain("draw x in normal font")',
#                                   'bold("draw x in bold font")',
#                                   'italic("draw x in italic font")',
#                                   'bolditalic("draw x in bolditalic font ")',
#                                   'hat("x with a circumflex")',
#                                   'tilde("x with a tilde")',
#                                   'dot("x with a dot")',
#                                   'ring("x with a ring")',
#                                   'bar("xy with bar")',
#                                   'widehat("xy with a wide circumflex")',
#                                   'widetilde("xy with a wide tilde")',
#                                   '"x" %<->% "y"',
#                                   '"x" %->% "y"',
#                                   '"x" %<-% "y"',
#                                   '"x" %up% "y"',
#                                   '"x" %down% "y"',
#                                   'displaystyle("draw x in normal size (extra spacing)")',
#                                   'textstyle("draw x in normal size")',
#                                   'scriptstyle("draw x in small size")',
#                                   'scriptscriptstyle("draw x in very small size")',
#                                   'underline("draw x underlined")'
# ), 
# y = length(label):1
# )
# 
# ggplot(plotmath_test, aes(x = 1, y = y, label = label)) + geom_text(parse = TRUE)










# Regressions and logit --------------------------------------------

lm_plots <- function(model) {
  par(mfrow=c(2,2))
  plot(model)
  par(mfrow=c(1,1))
}



# multiplicator = c("DPO" = 10, "DPO" = 50)
# ci <- FALSE
# conf.level <- 0.95
# format_OR <- TRUE
# n = 1

readable_OR <- 
  function(model, multiplicator = double(), n = 1L, digits_with_n = 2L,
           ci = FALSE, conf.level = 0.95, format = TRUE) {
    format_OR <- format
    
    n <- vctrs::vec_cast(n, integer())
    digits_with_n <- vctrs::vec_cast(digits_with_n, integer())
    stopifnot(n >= 1, digits_with_n >= 0)
    
    OR <- broom::tidy(model, exponentiate = TRUE, conf.int = ci, conf.level = conf.level) |> 
      rename(parameter = term, Odds_ratio = estimate, z = statistic, 
             any_of(c(OR_inf = "conf.low", OR_sup = "conf.high"))) |>
      mutate(parameter = if_else(row_number() == 1, "Intercept", parameter))
    # broom.helpers::tidy_plus_plus(model, exponentiate = TRUE)
    
    
    if (length(multiplicator) != 0) {
      missing_names <-  names(multiplicator) |>
        map_lgl(~ length(which(OR$parameter == .)) == 0) |>
        set_names(names(multiplicator) )
      
      if (any(missing_names)) {
        stop(paste0("following variables not found among the model parameters: ",
                    paste0(unique(names(missing_names)[missing_names]), collapse = ", ")))
      }
      
      ref_multiplicator    <- names(multiplicator) |> map(~ which(OR$parameter == .))
      
      
      names(multiplicator) <- paste0(names(multiplicator), "_", multiplicator)
      mult <- list(names(multiplicator), ref_multiplicator, multiplicator) |> transpose()
      
      OR <- 
        reduce(mult, .init = OR, 
               ~ bind_rows(..1, 
                           summarise(..1, 
                                     parameter       = ..2[[1]], 
                                     across(any_of(c("Odds_ratio", "OR_inf", "OR_sup")), 
                                            ~ nth(., ..2[[2]])^..2[[3]]),
                                     std.error  = abs(nth(std.error , ..2[[2]])*..2[[3]]), 
                                     p.value    = nth(p.value   , ..2[[2]]),
                           ))
        )
    }
    
    OR <- OR |>
      mutate(
        signif  = case_when(p.value < 0.01 ~ "***", 
                            p.value < 0.05 ~ "** ", 
                            p.value < 0.10 ~ "*  ", 
                            TRUE           ~ "   " ),
        Odds    = if_else(row_number() != 1, Odds_ratio * first(Odds_ratio), Odds_ratio), 
        prob    = if(n <= 1) {
          fmt(0, type = "row", pct = Odds/(1 + Odds))
        } else {
          fmt(0, type = "mean", mean = Odds/(1 + Odds) * n, digits = digits_with_n)
        }, 
        
        marginal_effect = set_digits( set_num(prob, get_num(prob) - get_num(first(prob)) ), 
                                      if_else(n <= 1, 1L, digits_with_n) ), 
      )
    
    
    if (ci) {
      OR <- OR |>
        mutate(
          Odds_inf = if_else(row_number() != 1, OR_inf * first(OR_inf), OR_inf),
          Odds_sup = if_else(row_number() != 1, OR_sup * first(OR_sup), OR_sup),
          p_inf    = if (n <= 1) {
            fmt(0, type = "row", pct = Odds_inf/(1 + Odds_inf))
          } else {
            fmt(0, type = "mean", mean = Odds_inf/(1 + Odds_inf) * n, digits = digits_with_n)
          }, 
          
          p_sup    = if (n <= 1) {
            fmt(0, type = "row", pct = Odds_sup/(1 + Odds_sup))
          } else {
            fmt(0, type = "mean", mean = Odds_sup/(1 + Odds_sup) * n, digits = digits_with_n)
          }, 
          
          me_inf   = set_digits(set_num(p_inf, get_num(p_inf) - get_num(first(p_inf)) ),
                                if_else(n <= 1, 1L, digits_with_n) ), 
          me_sup   = set_digits(set_num(p_sup, get_num(p_sup) - get_num(first(p_sup)) ),
                                if_else(n <= 1, 1L, digits_with_n) ) 
        )
    }
    
    prob_column <- c("marginal_effect" = "prob", "me_inf" = "p_inf", "me_sup" = "p_sup")
    
    if (format_OR) {
      OR <- OR |>
        mutate(
          across(any_of(c("Odds_ratio", "OR_inf", "OR_sup", "Odds", "Odds_inf", "Odds_sup")),
                 ~ if_else(condition = . < 1, 
                           true      = paste0("1/", format(1/., digits = 3)), 
                           false     = paste0("   ", format(., digits = 2)))   ), 
          
          across(any_of(c("prob", "marginal_effect", "p_inf", "p_sup", "me_inf", "me_sup")), 
                 ~ str_pad(format(.), max(str_length(format(.))))
          ), 
          
          across(any_of(c("Odds_ratio", "marginal_effect")), ~ paste0(., signif)),
          
          across(any_of(c("marginal_effect", "me_inf", "me_sup")), 
                 ~ if_else(row_number() == 1, 
                           true  = paste0("Ref:", rlang::eval_tidy(sym(prob_column[cur_column()] ))) |> 
                             str_remove_all("\\*") |> str_squish(), 
                           false = .)),
          
          p.value = format(round(p.value, 3), digits = 3)
        ) |>
        select(-any_of(c("signif", "p.value")))
    }
    
    if (n > 1) {
      OR <- OR |> rename("prob × n" = "prob")
    }
    
    return(OR)
  }






# #' Odd ratios plot 
# # Inspired from `finalfit`, thanks to Ewen M Harrison.
# # Licence MIT : https://finalfit.org/LICENSE-text.html
# #'
# #' @param model A `glm` model.
# #' @param column_space 	Adjust table column spacing.
# #' @param dependent_label Main label for plot.
# #' @param table_text_size Alter font size of table text.
# # @param title_text_size Alter font size of title text.
# #' @param plot_opts A list of arguments to be appended to the ggplot call by \code{"+"}.
# #' @param table_opts A list of arguments to be appended to the ggplot table call by
# #'  \code{"+"}.
# #' @param return_df To return the dataframe.
# #' @param ... Other parameters.
# 
# #' @return The odd ratios plot as a \code{ggplot2} object.
# #' @export
# #'
# # @examples
pers_or_plot <-
  function (model, column_space = c(-0.5, 0, 0.22, 0.4), 
            #prefix = "", suffix = ": OR (95% CI, p-value)",
            table_text_size = 5, title_text_size = 18, plot_opts = NULL,
            table_opts = NULL, return_df = FALSE, ...) {
    requireNamespace("ggplot2")
    
    # sansF <- grDevices::windowsFonts("sans")
    # grDevices::windowsFonts(sans = windowsFont("TT Arial"))
    # grDevices::windowsFonts() %>% print()
    
    model_data <- broom.helpers::tidy_plus_plus(model, exponentiate = TRUE, intercept = TRUE) 
    
    model_data <- model_data |> 
      select(label = var_label, levels = label, reference_row, Odds_ratio = estimate, 
             z = statistic, p.value, OR_inf = conf.low, OR_sup = conf.high, Total = n_obs) |>
      mutate(across(any_of(c("parameter", "label")), 
                    ~ if_else(row_number() == 1, 
                              true  = paste0("~underline(\"(Constante: toute variable = Ref)\")"),
                              false = paste0("~underline(\"", ., "\")"))), 
             
             across(any_of(c("levels")), 
                    ~ if_else(row_number() == 1, "", .)), 
             
      ) |> 
      new_tab()
    
    ubs <- stringi::stri_unescape_unicode("\\u202f")
    
    model_data <- model_data |>
      mutate(
        signif  = case_when(p.value < 0.01 ~ paste0("***"), 
                            p.value < 0.05 ~ paste0("**", strrep(ubs, 2)), 
                            p.value < 0.10 ~ paste0("*", strrep(ubs, 5)), 
                            TRUE           ~ paste0("", strrep(ubs, 6)) ),
        
        Odds    = if_else(row_number() != 1, Odds_ratio * first(Odds_ratio), Odds_ratio), 
        #Odds_inf = if_else(row_number() != 1, OR_inf * first(OR_inf), OR_inf),
        #Odds_sup = if_else(row_number() != 1, OR_sup * first(OR_sup), OR_sup),
        
        prob    = fmt(0, type = "row", pct = Odds/(1 + Odds)), 
        #p_inf    = fmt(0, type = "row", pct = Odds_inf/(1 + Odds_inf)), 
        #p_sup    = fmt(0, type = "row", pct = Odds_sup/(1 + Odds_sup)), 
        
        marginal_effect = set_digits(prob - first(prob), 1L ), 
        #me_inf   = set_digits(p_inf - first(p_inf), 1L ), 
        #me_sup   = set_digits(p_sup - first(p_sup), 1L ) 
        
        #color = as.factor(dplyr::if_else(reference_row, "Reference", "Autre")),
        nb = n() - row_number(),
        
        reference_row = dplyr::if_else(row_number() == 1, FALSE, reference_row)
      ) |> 
      select(-Odds, -z)
    
    model_data <- model_data |>
      mutate(
        label = dplyr::if_else(row_number() == 1 | reference_row, label, ""), 
        
        OR_text = if_else(condition = Odds_ratio < 1,
                          true      = paste0("1/", format(1/Odds_ratio, digits = 3)),
                          false     = paste0("  ", format(Odds_ratio, digits =3))   
        ), 
        
        me_text = dplyr::if_else(condition = marginal_effect > 0, 
                                 true      = paste0("+", format(marginal_effect)), 
                                 false     = format(marginal_effect) 
        ), 
        
        me_text = str_pad(me_text, max(str_length(me_text))), 
        
        across(any_of(c("OR_text", "me_text")), ~ paste0(., signif)),
        
        OR_text = if_else(condition = reference_row, "Référence", OR_text), 
        
        me_text = if_else(
          condition = row_number() == 1,
          true      = paste0("Ref:", prob) |> str_remove_all("\\*") |> str_squish(),
          false     = me_text
        ),
        
        me_text = if_else(condition = reference_row, "", me_text), 
      )
    
    if (return_df) return(model_data)
    
    
    log_range <- max(as.numeric(model_data$Odds_ratio[-1])) + 
      max(1/as.numeric(model_data$Odds_ratio[-1]))
    break_scale <- dplyr::case_when(
      log_range < 4/8  ~ 16,
      log_range < 4/4  ~ 8,
      log_range < 4/2  ~ 4,
      log_range < 4    ~ 2,
      log_range < 4*2  ~ 1,
      log_range < 4*4  ~ 1/2,
      log_range < 4*8  ~ 1/4,
      log_range < 4*16 ~ 1/8,
      TRUE             ~ 1/16)
    
    inverse_breaks <-
      sort((1:max(round(1/as.numeric(model_data$Odds_ratio[-1], 0))*2*break_scale)),
           decreasing = T)/break_scale
    legend_ticks_breaks <- c(1/inverse_breaks,
                             1:(max(round(as.numeric(model_data$Odds_ratio[-1]), 0)*2*break_scale))/break_scale, 1) %>%
      unique() %>% sort()
    legend_ticks_labels <- ifelse(legend_ticks_breaks < 1,
                                  yes = stringr::str_c("1/", inverse_breaks),
                                  no = stringr::str_remove_all(as.character(
                                    legend_ticks_breaks), "0+$|\\.$"))
    
    first_row <- model_data[1,] %>%
      tibble::add_row(nb = model_data$nb[1] + 1, label = "Variable", Total = 0, #index = 0,
                      levels = stringi::stri_unescape_unicode("Modalit\\u00e9"),
                      OR_text = "Odds ratio", # stringi::stri_unescape_unicode("Odds ratio (IC \\u00e0 95%, \\u00e9chelle logarithmique)")
                      me_text = "Effet marginal",
                      .before = 1) %>%
      dplyr::slice(1)
    
    g1 = model_data |> 
      mutate(across(all_of(c("Odds_ratio", "OR_inf", "OR_sup")), 
                    ~ dplyr::if_else(row_number() == 1, NA_real_, .))) |>
      ggplot2::ggplot(ggplot2::aes(x = as.numeric(.data$Odds_ratio),
                                   xmin = as.numeric(.data$OR_inf),
                                   xmax = as.numeric(.data$OR_sup),
                                   y = .data$nb)) +
      ggplot2::geom_point(ggplot2::aes(size = .data$Total, fill = .data$reference_row),
                          shape = 22, na.rm = TRUE) + #"darkblue"
      ggplot2::geom_vline(xintercept = 1, linetype = "longdash",
                          colour = "black") +
      ggplot2::geom_point(data = dplyr::slice(dplyr::select(model_data, .data$nb), 1),
                          ggplot2::aes(x = 1), y = first_row$nb,
                          shape = 15, color = "white", size = 22,
                          inherit.aes = FALSE, na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(size = .data$Total, fill = .data$reference_row),
                          shape = 22, na.rm = TRUE) + #"darkblue"
      ggplot2::geom_errorbarh(height = 0.2, na.rm = TRUE) +
      ggplot2::annotate("text", x = 0, y = first_row$nb[1],
                        label = stringi::stri_unescape_unicode(" (IC \\u00e0 95%, \\u00e9chelle logarithmique)"), 
                        hjust = 0,
                        size = table_text_size, fontface = "bold", na.rm = TRUE) +
      ggplot2::scale_x_continuous(trans = "log10", breaks = legend_ticks_breaks,
                                  labels = legend_ticks_labels) +
      ggplot2::scale_fill_manual(values = c(`FALSE` = "#333333", `TRUE` = "#999999")) +
      ggplot2::theme_classic(14) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), #element_text(),
                     axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                     axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                     legend.position = "none", plot.margin = ggplot2::unit(c(0.25,0.25,0.25,-0.275), "cm"))
    
    
    t1 = 
      ggplot2::ggplot(model_data, ggplot2::aes(x = as.numeric(.data$Odds_ratio),
                                               y = .data$nb)) +
      ggplot2::annotate("text", x = column_space[1], y = model_data$nb, parse = TRUE,
                        label = model_data$label, hjust = 0, size = table_text_size, na.rm = TRUE) +
      ggplot2::annotate("text", x = column_space[2], y = model_data$nb,
                        label = model_data$levels, hjust = 1, size = table_text_size, na.rm = TRUE) +
      ggplot2::annotate("text", x = column_space[3], y = model_data$nb,
                        label = model_data$me_text, hjust = 1, size = table_text_size, na.rm = TRUE) +
      ggplot2::annotate("text", x = column_space[4], y = model_data$nb,
                        label = model_data$OR_text, hjust = 1, size = table_text_size, na.rm = TRUE) +
      
      
      ggplot2::annotate("text", x = column_space[1], y = first_row$nb,
                        label = first_row$label, hjust = 0, size = table_text_size,
                        fontface = "bold", na.rm = TRUE) +
      ggplot2::annotate("text", x = column_space[2], y = first_row$nb,
                        label = first_row$levels, hjust = 1, size = table_text_size,
                        fontface = "bold", na.rm = TRUE) +
      ggplot2::annotate("text", x = column_space[3], y = first_row$nb,
                        label = first_row$me_text, hjust = 1, size = table_text_size,
                        fontface = "bold", na.rm = TRUE) +
      ggplot2::annotate("text", x = column_space[4], y = first_row$nb,
                        label = first_row$OR_text, hjust = 1, size = table_text_size,
                        fontface = "bold.italic", na.rm = TRUE) +
      
      ggplot2::theme_classic(14) +
      ggplot2::theme(
        #text = ggplot2::element_text(family = "mono"), #if ("arial" %in% names(grDevices::windowsFonts())) { "arial" } else { "sans" }),
        axis.title.x = ggplot2::element_blank(), #element_text(colour = "white"),
        axis.text.x = ggplot2::element_text(colour = "white"), axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
        line = ggplot2::element_blank(), plot.margin = ggplot2::unit(c(0.25,-0.275, 0.25,0.25), "cm"))
    
    
    g1 = g1 + plot_opts
    t1 = t1 + table_opts
    
    
    gridExtra::grid.arrange(t1, g1, ncol = 2, widths = c(3, 1.5)#,
                            # top = grid::textGrob(title, x = 0.02, y = 0.2, gp = grid::gpar(fontsize = title_text_size),
                            #                      just = "left")
    )
    
  }










# # For a single score
# glm_wflow <-
#   workflow() %>%
#   add_model(glm_model) |>
#   add_variables(outcome = score_risques_phy_prob, predictors = all_of(vars_orga))
# 
# lm_fit <- fit(glm_wflow, ct13_reg)
# 
# lm_fit %>%
#   extract_fit_parsnip() |>
#   readable_OR(n = length(vars_risques_phy)) |> 
#   new_tab()
# # => Ok, same result as before.




# multi_logit(ct13_reg, vars_scores_RPS13[1],
#             predictors_sequence = scores_RPS_predictors, marginal_effects = TRUE,
#             nb_questions = vars_scores_RPS13[1] |>
#               str_replace("score_", "vars_") |> 
#               syms() |> map_int(~eval_tidy(.) |> length())
# )
# 
# multi_logit(ct13_reg, "DPO",
#             predictors_sequence = scores_RPS_predictors, marginal_effects = TRUE,
# )
# 
# ct13_reg |> 
#   mutate(DPO01 = if_else(DPO == "DPO", 1L, 0L)) |>
#   multi_logit("DPO01",
#               predictors_sequence = scores_RPS_predictors, marginal_effects = TRUE,
#   )

# data <- ct13_reg 
# outcomes <- "DPO" #vars_scores_RPS13
# predictors_sequence <- scores_RPS_predictors["complet"]
# marginal_effects <- TRUE
# 
# nb_questions <- 1 #vars_scores_RPS13 |>
# #str_replace("score_", "vars_") |> 
# #syms() |> map_int(~eval_tidy(.) |> length())


## multi_logit, with new parsnip engine survey weights ----
# Adding new engine in poisson_reg
svglm2 <- function(formula, family = gaussian(), data, weights, ...) {
  data    <- add_column(data, wt = weights)
  outcome <- as.character(formula)[2]
  
  formula <- paste0(outcome, " ~ ", 
                    paste0(names(data)[!names(data) %in% c(outcome, "wt")], 
                           collapse = " + ")
  ) |>
    as.formula()
  
  design <- survey::svydesign(ids = ~ 1, data = data, weights = ~ wt)
  
  survey::svyglm(formula = as.formula(formula), design = design, family = family, ...)
}

# #svglm2(score_intensite ~ SEXE + AGE4 + DIPLOME4 + PPP1 + ENCADR + EMP4reg + NBSALA2 + cah_ORGA, 
# #       family = quasibinomial(), data = data, weights = as.double(data$pondqaa))
# #
# #svglm2(score_intensite ~ cah_ORGA, 
# #       family = quasibinomial(), data = data, weights = as.double(data$pondqaa))
# 
# svglm2(score_intensite ~ ., 
#        family = quasibinomial(), data = select(data, -pondqaa), 
#        weights = as.double(data$pondqaa))
# 
# svglm2(score_intensite ~ ., 
#        family = quasibinomial(), data = select(data, -pondqaa), 
#        weights = as.double(data$pondqaa))



#https://rdrr.io/github/tidymodels/poissonreg/src/R/poisson_reg_data.R
parsnip::set_model_engine("poisson_reg", "regression", "svglm2")
parsnip::set_dependency("poisson_reg", "svglm2", "stats")
parsnip::set_dependency("poisson_reg", "svglm2", "survey")
parsnip::set_dependency("poisson_reg", "svglm2", "poissonreg")

parsnip::set_fit(
  model = "poisson_reg",
  eng = "svglm2",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(fun = "svglm2"), #c(pkg = "stats", fun = "svglm2"),
    defaults = list(family = expr(stats::quasibinomial))
  )
)

parsnip::set_encoding(
  model = "poisson_reg",
  eng = "svglm2",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

parsnip::set_pred(
  model = "poisson_reg",
  eng = "svglm2",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "response"
      )
  )
)

parsnip::set_pred(
  model = "poisson_reg",
  eng = "svglm2",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = expr(object$fit), newdata = expr(new_data))
  )
)

# https://github.com/cran/parsnip/blob/master/R/logistic_reg_data.R
set_model_engine("logistic_reg", "classification", "svglm2")
set_dependency("logistic_reg", "svglm2", "stats")

set_fit(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(fun = "svglm2"), #c(pkg = "stats", fun = "svglm2"),
    defaults = list(family = expr(stats::quasibinomial))
  )
)

set_encoding(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = parsnip:::prob_to_class_2,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

set_pred(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      x <- tibble(v1 = 1 - x, v2 = x)
      colnames(x) <- object$lvl
      x
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

set_pred(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
  )
)

set_pred(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = parsnip:::logistic_lp_to_conf_int,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        se.fit = TRUE,
        type = "link"
      )
  )
)



# Bug correction :
#  - Pass characters in factors

# Enhancements : 
#  - One line for each reference row

multi_logit <- function(data, outcomes, predictors_sequence, split_var = NULL, wt = NULL,
                        nb_questions, 
                        odds_ratios = TRUE, marginal_effects = FALSE, signif = TRUE,
                        inverse_two_level_factors = TRUE, 
                        subtext = "") {
  variables_list <- c(outcomes, 
                 purrr::flatten_chr(predictors_sequence), 
                 split_var, wt
                 )

  data <- dplyr::select(data, all_of(variables_list))

  outcomes_class <- map_chr(data[outcomes], class)
  outcomes_min   <- map_if(data[outcomes], outcomes_class %in% c("numeric", "integer"),
                           ~ min(., na.rm = TRUE), .else = ~ 0L) |> purrr::flatten_int()
  outcomes_max   <- map_if(data[outcomes], outcomes_class %in% c("numeric", "integer"),
                           ~ max(., na.rm = TRUE), .else = ~ 1L) |> purrr::flatten_int()
  outcomes_int   <- map_lgl(data[outcomes], ~rlang::is_integerish(.) & !is.factor(.))
  outcomes_nlv   <- map_if(data[outcomes], outcomes_class == "factor", 
                           ~nlevels(as.factor(.)), .else = ~ 0L) |> flatten_int()
  
  
  factors_2lv <- outcomes_class %in% c("factor", "character") & outcomes_nlv == 2 | 
    (outcomes_int & outcomes_min == 0 & outcomes_max == 1)
  
  factors_3lv <- outcomes_class == "factor" & outcomes_nlv >= 3
  if (any(factors_3lv)) stop(paste0("some factors have more than 2 levels: ", 
                                    paste0(names(factors_3lv)[factors_3lv], 
                                           collapse = ", ")))
  
  if (any(factors_2lv & outcomes_int)) {
    data <- data |> mutate(across(all_of(outcomes[factors_2lv & outcomes_int]), 
                                  ~ as.factor(.) |> 
                                    `levels<-`(c(paste0("Pas ", dplyr::cur_column()), 
                                               dplyr::cur_column() ) )) )   
  }
  
  if(any(factors_2lv & !outcomes_int & inverse_two_level_factors)) {
    data <- data |> mutate(across(all_of(outcomes[factors_2lv & !outcomes_int]), 
                                  ~ fct_rev(as.factor(.)) ))  
  }
  
  
  
  integer3 <- !factors_2lv & outcomes_int
  
  if (any(integer3)) requireNamespace("poissonreg", quietly = TRUE)
  
  # double_01 <- !factors_2lv &!outcomes_int & outcomes_class == "numeric" & 
  #   outcomes_min >= 0 & outcomes_max <= 1
 
  if (missing(nb_questions)) {
    nb_questions <- if_else(integer3, as.integer(outcomes_max), 1L)
    
  } else {
    nb_questions <- vctrs::vec_recycle(vctrs::vec_cast(nb_questions, integer()), 
                                       length(outcomes))
  }
  nb_questions <- nb_questions |> purrr::set_names(outcomes)
  
  if (any(integer3)) {
    data <- data |> 
      mutate(across(all_of(outcomes[integer3]), ~ ./nb_questions[cur_column()] ))  
  }

  
  reference_population <- 
    paste0( #"Population de réference (modèle complet): ",
      paste0(
        unique(purrr::flatten_chr(predictors_sequence)) %>%
        purrr::keep(purrr::map_lgl(., ~ dplyr::pull(data, .) |> is.factor() )) |>
        purrr::map_chr(~ dplyr::pull(data, .) |> levels() |> dplyr::first()), 
             collapse = ", "), 
      " (modèle complet)"
    )
  
  predictors <- predictors_sequence |> purrr::flatten_chr() |> unique()
  
  data <- data |> dplyr::mutate(NA_pred = if_any(.cols = all_of(predictors), is.na)) 
  message(paste0(sum(data$NA_pred), " rows with NA in at least one predictor were removed"))
  data <- data |> dplyr::filter(!NA_pred) |> dplyr::select(-NA_pred)
  
  if (!is.null(wt)) {
    data <- data |> dplyr::mutate(!!sym(wt) := hardhat::importance_weights(!!sym(wt)))
  }
  
  
  # Specify the models
  if (is.null(split_var)) {
    models_vars <- tidyr::crossing(outcome    = forcats::as_factor(outcomes), 
                                   predictors = predictors_sequence,
                                   data        = list(data)
                                   ) |> 
      dplyr::mutate(split_var = "")
    
  } else {
    data_split <- data |> 
      dplyr::rename(any_of(c("split_var" = split_var))) |>
      dplyr::group_by(split_var) |> group_nest() |> 
      dplyr::filter(if_any(1, ~!is.na(.))) |> 
      dplyr::rename(data = data)
    
    models_vars <- tidyr::crossing(outcome    = forcats::as_factor(outcomes), 
                                   predictors = predictors_sequence,
                                   split_var  = data_split$split_var) |> 
      dplyr::left_join(data_split, by = "split_var")
  }
  
  if (is.null(wt)) {
    glm_model <- 
      parsnip::poisson_reg() |> # since logistic_reg() means binary outcome (2 lv factor)
      parsnip::set_engine("glm", family = stats::quasibinomial(link = "logit") )
    
    binary_model <- 
      parsnip::logistic_reg() |> 
      parsnip::set_engine("glm", family = stats::binomial(link = "logit") )
    
    
    models_vars <- models_vars |> 
    dplyr::mutate(model_type = dplyr::if_else(factors_2lv[outcome], 
                                              list("bin" = binary_model), 
                                              list("glm" = glm_model)), 
                  
                  nb_questions = nb_questions[outcome],
                  
                  # wflow_var = list(workflows::workflow_variables(
                  #   outcomes    = tidyselect::matches(outcome),
                  #   predictors  = tidyselect::all_of(predictors)
                  # )),
                  
                  
                  # recipe = pmap(list(data, outcome, predictors), 
                  #               function(.dat, .outcome, .pred)
                  #               ~ recipe(.dat) |> 
                  #                 update_role(all_of(.outcome), new_role = "outcome") |>
                  #                 update_role(all_of(.pred), new_role = "predictor")
                  #               ),
                  #               
                  #wflow = purrr::map2(recipe, model_type, ~ workflows::workflow(.x, .y))
                  
                  wflow = pmap(list(data, outcome, predictors, model_type), 
                               function(.dat, .outcome, .pred, .model)
                                 workflow() |> 
                                 add_model(.model) |> 
                                 add_variables(outcome    = all_of(.outcome), 
                                               predictors = all_of(.pred))
                  ), 
                  
                  
    )
    
  } else { # with survey weights
    # dw <- survey::svydesign(ids = ~ 1, data = data, weights = ~ pondqaa)
    
    glm_model <- 
      parsnip::poisson_reg() |> # since logistic_reg() means binary outcome (2 lv factor)
      parsnip::set_engine("svglm2", family = stats::quasibinomial(link = "logit") #, 
                          #survey_weights = wt
      )
    
    binary_model <- 
       parsnip::logistic_reg() |> 
       parsnip::set_engine("svglm2", family = stats::quasibinomial(link = "logit")#, 
                           #weights = as.formula(paste0("~", wt ))
       )
     
     models_vars <- models_vars |> 
       dplyr::mutate(model_type = dplyr::if_else(factors_2lv[outcome], 
                                                 list("bin" = binary_model), 
                                                 list("glm" = glm_model)), 
                     nb_questions = nb_questions[outcome],
                    
                    wflow = pmap(list(data, outcome, predictors, model_type), 
                                 function(.dat, .outcome, .pred, .model)
                                   workflow() |> 
                                   add_model(.model) |> 
                                   add_variables(outcome    = all_of(.outcome), 
                                                 predictors = all_of(.pred)    ) |>
                                   add_case_weights(wt)
                    ), 
                    
                    
      )
  }
  


  
  
  #With repice ?
  # logit_recipe <-
  #   recipe() %>%
  #   step_dummy(all_nominal_predictors())
  

  # Run the models
  models <- models_vars |> 
    dplyr::mutate(fit = purrr::map2(wflow, data, ~ fit(.x, data = .y)))
  
  
  
  # Retrieve coefficients, calculate Odds, probabilities and marginal effects
  models <- models |> 
    dplyr::mutate(OR_table = purrr::map2(fit, nb_questions, 
                                         ~ readable_OR(.x, n = .y, format = FALSE)))
  
  models <- models |> 
    dplyr::mutate(
      OR = purrr::map(OR_table, 
                      ~ dplyr::select(., parameter, OR = Odds_ratio, signif)), 
      ME = purrr::map(OR_table, ~ dplyr::select(., parameter, ME = marginal_effect, 
                                                OR = Odds_ratio, 
                                                tidyselect::any_of(c("p" = "prob × n",
                                                                     "p" = "prob")), 
                                                signif)), 
      
      model_name = paste0(names(predictors), ":by:", split_var) #|>
      #stringr::str_remove(":by:$")
    )
  
  
  # # Nested models comparison: drop-in-deviance test
  # models <- models |> 
  #   mutate(n = dplyr::row_number()) |> 
  #   group_by(outcome, split_var) |>
  #   mutate(
  #     nested_with = map2_int(
  #       predictors, list(predictors), 
  #       ~ map_lgl(.y, function(.pred) !identical(.x, .pred) & all(.pred  %in% .x)) |>
  #         which() |> dplyr::last() #|> tidyr::replace_na("")
  #     ), 
  #     
  #     nested_with = n[nested_with]
  #   ) |> 
  #   dplyr::ungroup() |>
  #   select(-n) |> 
  #   mutate(drop_in_dev_test = NA_real_, 
  #          fit_nested_with = fit[nested_with])
  # 
  # models[!is.na(models$nested_with),] <- models[!is.na(models$nested_with),] |> 
  #   mutate(drop_in_dev_test = purrr::map2_dbl(fit, fit_nested_with,
  #                                         ~ anova(extract_fit_engine(.x),
  #                                                 extract_fit_engine(.y), 
  #                                                 test = "Chisq") |>
  #                                           pull(`Pr(>Chi)`) |> dplyr::nth(2)
  #   ))
  # # anova(extract_fit_engine(models$fit[[1]]),  
  # #       extract_fit_engine(models$fit[[3]]), 
  # #       test = "Chisq")
  # 
  # models <- models |> 
  #   mutate(drop_in_dev_test = stringr::str_c(
  #     "/ ", stringr::str_replace(model_name[nested_with], ":by:", " "), ", ",
  #     if_else(drop_in_dev_test < 0.05, 
  #             true  = "signif", #"signif drop in deviance", 
  #             false = "not signif"), # "no signif drop in deviance"), 
  #     " (p=", round(drop_in_dev_test, digits = 3), ")" ) 
  #   ) |> 
  #   select(-nested_with, -fit_nested_with)
  # 
  # # TO DO : notes dans les noms des modèles + renvoie au subtext ? NON
  
  
  
  # Odds-ratio tables
  if (odds_ratios) {
    OR <- models |> 
      dplyr::select(outcome, model_name, OR) |> 
      dplyr::mutate(
        OR = purrr::map2(
          OR, model_name, 
          ~ dplyr::rename(
            .x, tidyselect::all_of(c(purrr::set_names("OR", paste0("OR_", .y)), 
                                     purrr::set_names("signif", paste0("s_", .y)))) 
          )
        ) ) |> 
      dplyr::group_by(outcome) |> 
      dplyr::group_split()
    
    OR <- OR |>
      purrr::set_names(purrr::map_chr(OR, ~ as.character(.$outcome[1]))) |> 
      purrr::map(~ purrr::reduce(.$OR, ~ dplyr::full_join(.x, .y, by = "parameter")))
    
    
    OR <- OR |> 
      purrr::map(~ dplyr::mutate(
        ., 
        var = purrr::map(parameter,
                         function(.param) purrr::map_lgl(predictors, 
                                                         function(.pred) str_detect(.param,
                                                                                    paste0("^", .pred))
                         ) ) |> 
          purrr::map(~ predictors[which(.)]) |> 
          purrr::map(~ if (length(.) == 0) {"Constante (population de référence)"} else {.}) |>
          purrr::flatten_chr() |> 
          as_factor() |> forcats::fct_relevel(c("Constante (population de référence)", 
                                                predictors)), 
        
        levels = as_factor(dplyr::if_else(
          condition = str_detect(parameter, "Intercept"), 
          true      = reference_population,  
          false     = str_remove(parameter, as.character(var))
        ))
      ) |> 
        dplyr::select(var, levels, tidyselect::everything(), -parameter)
      )
    
    OR <-  OR |> 
      purrr::map(~  dplyr::mutate(
        ., 
        dplyr::across(where(is.double), 
                      ~ fmt(
                        0, 
                        type      = "mean", 
                        digits    = 1L, 
                        mean      = ., 
                        diff      = dplyr::if_else(
                          !str_detect(replace_na(eval_tidy(sym(str_replace(cur_column(),
                                                                           "^OR_", "s_"))), ""),
                                      "/*" # in gray when not in 90% ci
                          ) | var == "Constante (population de référence)", 
                          true  = 0, 
                          false = .), 
                        
                        diff_type = "1", 
                        color     = "diff", 
                        #in_totrow = row_number() == 1,
                        in_refrow = dplyr::row_number() == 1, 
                        col_var = dplyr::cur_column() |> stringr::str_remove("^OR_") |>
                          stringr::str_remove(":by:.*$"), 
                        comp_all = FALSE, 
                      ))
      ) |> 
        dplyr::rename_with(~ stringr::str_replace(., ":by:", " ") |> 
                             stringr::str_replace("^OR_", "OR ") ) #|>
        #dplyr::rename_with(~ str_remove(., "^OR_")) |>
        #dplyr::arrange(var) |>
        #new_tab()
      )
    
    if (signif) {
      OR <- OR |>purrr::map(
        ~ dplyr::rename_with(., 
                             .cols = starts_with("s_"), 
                             .fn = ~ paste0("s", seq_along(.) ))
      )
    } else {
      OR <- OR |> 
        map(~ dplyr::select(., -starts_with("s_")))
    }
  }
  
  
  # Marginal effects tables
  if (marginal_effects) {
    ME <- models |> 
      dplyr::select(outcome, model_name, ME) |> 
      dplyr::mutate(ME = purrr::map2(ME, model_name, 
                                     ~ dplyr::rename(.x, all_of(c(purrr::set_names("ME", paste0("ME_", .y)),
                                                                  purrr::set_names("OR", paste0("OR_", .y)),
                                                                  purrr::set_names("p" , paste0("p_", .y)),
                                                                  purrr::set_names("signif", paste0("s_", .y)))) ) |>
                                       dplyr::mutate(dplyr::across(starts_with(c("ME_", "p_")), get_num))
      ) ) |> 
      dplyr::group_by(outcome) |> 
      dplyr::group_split()
    
    ME <- ME |>
      purrr::set_names(purrr::map_chr(ME, ~ as.character(.$outcome[1]))) |> 
      purrr::map(~ purrr::reduce(.$ME, ~ dplyr::full_join(.x, .y, by = "parameter")))
    
    ME <- ME |> 
      purrr::map(~ dplyr::mutate(
        ., 
        var = purrr::map(
          parameter, 
          function(.param) purrr::map_lgl(predictors, 
                                          function(.pred) stringr::str_detect(.param,
                                                                              paste0("^", .pred))
          ) ) |> 
          purrr::map(~ predictors[which(.)]) |> 
          purrr::map(~ if (length(.) == 0) {"Constante (population de référence)"} else {.}) |>
          purrr::flatten_chr() |> 
          as_factor() |> forcats::fct_relevel(c("Constante (population de référence)", 
                                                predictors)), 
        
        levels = as_factor(dplyr::if_else(
          condition = stringr::str_detect(parameter, "Intercept"), 
          true      = reference_population,  
          false     = stringr::str_remove(parameter, as.character(var))
        ))
      ) |> 
        dplyr::select(var, levels, everything(), -parameter)
      )
    
    ME <- ME |>
      purrr::map(~  dplyr::mutate(., dplyr::across(
        where(is.double) & starts_with("ME_"), 
        ~ fmt(
          0, 
          type      = "mean", 
          digits    = 2L, 
          mean      = dplyr::if_else(
            var == "Constante (population de référence)", 
            true  = replace_na(eval_tidy(sym(str_replace(cur_column(),
                                                         "^ME_", "p_"))), 0), 
            false = .), 
          
          diff      = dplyr::if_else(
            !str_detect(replace_na(eval_tidy(sym(str_replace(cur_column(),
                                                             "^ME_", "s_"))), ""),
                        "/*" # in gray when not in 90% ci
            ) | var == "Constante (population de référence)", 
            true  = 0, 
            false = replace_na(eval_tidy(sym(str_replace(cur_column(),
                                                         "^ME_", "OR_"))), 1) ), 
          
          diff_type = "1", 
          color     = "diff", 
          #in_totrow = row_number() == 1,
          in_refrow = dplyr::row_number() == 1, 
          col_var = dplyr::cur_column() |> str_remove("^ME_") |>
            stringr::str_remove(":by:.*$"), 
          comp_all = FALSE, 
        ))
      ) |> 
        dplyr::select(-starts_with(c("s_", "OR_", "p_"))) |>
        dplyr::rename_with(~ stringr::str_replace(., ":by:", " ") |> 
                             stringr::str_replace("^ME_", "ME ") ) #|>
        #dplyr::rename_with(~ str_remove(., "^ME_")) |>
        #dplyr::arrange(var) |>
        #new_tab()
      )
  }
  
  if (marginal_effects & odds_ratios) {
    res <- purrr::map2(OR, ME, ~dplyr::left_join(dplyr::mutate(.x, ` ` = ""), .y,
                                                 by = c("var", "levels")) )
  } else if (marginal_effects) {
    res <- ME
  } else if (odds_ratios) {
    res <- OR
  } else{
    return(models)
  }
  
  res_names <- paste0(names(res), dplyr::if_else(nb_questions > 1, 
                                                 true  = paste0(" (n=", nb_questions, ")"), 
                                                 false = ""))
  
    purrr::set_names(res, res_names) |> 
    purrr::map(~ tabxplor::new_tab(., subtext = subtext) |> dplyr::group_by(var))
}









## multi_logit save ----

#predictors_sequence <- list(
#  "sociodemo" = vars_sociodemo,
#  "metier"    = vars_metier,
#  "employeur" = vars_emp,
#  #"fonction"  = vars_fonction,
#  "sauf_orga" = c(vars_sociodemo, vars_metier, vars_emp),
#  "orga"      = "cah_ORGA",
#  "complet"   = c(vars_sociodemo, vars_metier, vars_emp, "cah_ORGA")
#)
#
# glm_model <- poisson_reg() |> # since logistic_reg() means binary outcome (2 lv factor)
#   set_engine("glm", family = quasibinomial(link = "logit") )
# 
# 
# reference_population <- 
#   paste0( #"Population de réference (modèle complet): ",
#     paste0(map_chr(predictors_sequence$complet, ~ pull(ct13_reg, .) |> levels() |> first()), 
#            collapse = ", "), 
#     " (modèle complet)"
#   )
# predictors <- predictors_sequence |> flatten_chr() |> unique()
# 
# scoreRPS_models_vars <- 
#   purrr::map(
#     vars_scores_RPS13_prob, 
#     function(.scores) purrr::map(predictors_sequence, 
#                                  function(.pred) workflow_variables(
#                                    outcomes    = matches(.scores),
#                                    predictors  = all_of(.pred)
#                                  )
#     )
#   ) |> 
#   set_names(str_remove_all(vars_scores_RPS13_prob, "_prob$"))
# 
# scoreRPS_models_vars <- scoreRPS_models_vars |> 
#   flatten() |> 
#   set_names(scoreRPS_models_vars |> imap(~ paste0(.y, " ", names(.x)) ) |> flatten_chr())
# # scoreRPS_models_vars <- workflow_variables(
# #   outcomes    = vars_scores_RPS13_prob[[1]],
# #   predictors  = all_of(vars_orga)
# # )
# 
# # preproc : a list of workflows or a list of recipes
# scoreRPS_models <- workflow_set(preproc = scoreRPS_models_vars, 
#                                 models = list(glm = glm_model) )
# # scoreRPS_models$info[[1]]
# # extract_workflow(scoreRPS_models, id = "")
# 
# scoreRPS_models <- scoreRPS_models %>%
#   mutate(fit = map(info, ~ fit(.x$workflow[[1]], ct13_reg)))
# # workflow_map("tune_grid", ) # ???
# # scoreRPS_models$fit[[1]]
# 
# # saveRDS(scoreRPS_models, file = "scoreRPS_models1.rds") # too long, wrong object to save
# # # scoreRPS_models <- readRDS( "scoreRPS_models1.rds")
# 
# 
# scoreRPS_models <- scoreRPS_models |> 
#   mutate(nb_questions = str_remove(wflow_id, " .+$") |> 
#            str_replace("score_", "vars_") |> syms() |>
#            map_int(~eval_tidy(.) |> length())
#   ) |> 
#   mutate(OR_table = map2(fit, nb_questions, ~ readable_OR(.x, n = .y, format = FALSE)))
# 
# scoreRPS_models <- scoreRPS_models |> 
#   mutate(outcome = as_factor(str_remove(wflow_id, " .*$")),
#          predictors = as_factor(str_remove(wflow_id, "^[^ ]+ ") |> str_remove("_glm$")),
#          OR = map(OR_table, ~ select(., parameter, OR = Odds_ratio, signif)), 
#          ME = map(OR_table, ~ select(., parameter, ME = marginal_effect, OR = Odds_ratio, 
#                                      p = `prob × n`, signif)), 
#   ) #|> 
# #select(-wflow_id, -info, -option, -result) 
# 
# 
# # Tableaux des Odds-ratio
# scoreRPS_OR <- scoreRPS_models |> 
#   select(outcome, predictors, OR) |> 
#   mutate(OR = map2(OR, predictors, 
#                    ~ rename(.x, all_of(c(set_names("OR", paste0("OR_", .y)), 
#                                          set_names("signif", paste0("s_", .y)))) )
#   ) ) |> 
#   group_by(outcome) |> 
#   group_split()
# 
# scoreRPS_OR <- scoreRPS_OR |>
#   set_names(map_chr(scoreRPS_OR, ~ as.character(.$outcome[1]))) |> 
#   map(~ reduce(.$OR, ~ full_join(.x, .y, by = "parameter")))
# 
# 
# scoreRPS_OR <- scoreRPS_OR |> 
#   map(~ mutate(., 
#                var = map(parameter, 
#                          function(.param) map_lgl(predictors, 
#                                                   function(.pred) str_detect(.param, paste0("^", .pred))
#                          ) ) |> 
#                  map(~ predictors[which(.)]) |> 
#                  map(~ if (length(.) == 0) {"Constante (population de référence)"} else {.}) |>
#                  flatten_chr() |> 
#                  as_factor() |> fct_relevel(c("Constante (population de référence)", 
#                                               predictors)), 
#                
#                levels = as_factor(if_else(
#                  condition = str_detect(parameter, "Intercept"), 
#                  true      = reference_population,  
#                  false     = str_remove(parameter, as.character(var))
#                ))
#   ) |> 
#     select(var, levels, everything(), -parameter)
#   )
# 
# scoreRPS_OR <-  scoreRPS_OR |> 
#   map(~  mutate(., 
#                 across(where(is.double), 
#                        ~ fmt(
#                          0, 
#                          type      = "mean", 
#                          digits    = 1L, 
#                          mean      = ., 
#                          diff      = if_else(
#                            !str_detect(replace_na(eval_tidy(sym(str_replace(cur_column(),
#                                                                             "^OR_", "s_"))), ""),
#                                        "/*" # in gray when not in 90% ci
#                            ) | var == "Constante (population de référence)", 
#                            true  = 0, 
#                            false = .), 
#                          
#                          diff_type = "1", 
#                          color     = "diff", 
#                          #in_totrow = row_number() == 1,
#                          in_refrow = row_number() == 1, 
#                          col_var = cur_column() |> str_remove("^OR_"), 
#                          comp_all = FALSE, 
#                        ))
#   ) |> 
#     select(-starts_with("s_")) |>
#     #rename_with(~ str_remove(., "^OR_")) |>
#     arrange(var) |>
#     new_tab()
#   )
# 
# 
# # Tableaux des effets marginaux
# scoreRPS_ME <- scoreRPS_models |> 
#   select(outcome, predictors, ME) |> 
#   mutate(ME = map2(ME, predictors, 
#                    ~ rename(.x, all_of(c(set_names("ME", paste0("ME_", .y)),
#                                          set_names("OR", paste0("OR_", .y)),
#                                          set_names("p" , paste0("p_", .y)),
#                                          set_names("signif", paste0("s_", .y)))) ) |>
#                      mutate(across(starts_with(c("ME_", "p_")), get_num))
#   ) ) |> 
#   group_by(outcome) |> 
#   group_split()
# 
# scoreRPS_ME <- scoreRPS_ME |>
#   set_names(map_chr(scoreRPS_ME, ~ as.character(.$outcome[1]))) |> 
#   map(~ reduce(.$ME, ~ full_join(.x, .y, by = "parameter")))
# 
# scoreRPS_ME <- scoreRPS_ME |> 
#   map(~ mutate(., 
#                var = map(parameter, 
#                          function(.param) map_lgl(predictors, 
#                                                   function(.pred) str_detect(.param, paste0("^", .pred))
#                          ) ) |> 
#                  map(~ predictors[which(.)]) |> 
#                  map(~ if (length(.) == 0) {"Constante (population de référence)"} else {.}) |>
#                  flatten_chr() |> 
#                  as_factor() |> fct_relevel(c("Constante (population de référence)", 
#                                               predictors)), 
#                
#                levels = as_factor(if_else(
#                  condition = str_detect(parameter, "Intercept"), 
#                  true      = reference_population,  
#                  false     = str_remove(parameter, as.character(var))
#                ))
#   ) |> 
#     select(var, levels, everything(), -parameter)
#   )
# 
# scoreRPS_ME <-  
#   scoreRPS_ME |>
#   map(~  mutate(., 
#                 across(where(is.double) & starts_with("ME_"), 
#                        ~ fmt(
#                          0, 
#                          type      = "mean", 
#                          digits    = 2L, 
#                          mean      = if_else(
#                            var == "Constante (population de référence)", 
#                            true  = replace_na(eval_tidy(sym(str_replace(cur_column(),
#                                                                         "^ME_", "p_"))), 0), 
#                            false = .), 
#                          
#                          diff      = if_else(
#                            !str_detect(replace_na(eval_tidy(sym(str_replace(cur_column(),
#                                                                             "^ME_", "s_"))), ""),
#                                        "/*" # in gray when not in 90% ci
#                            ) | var == "Constante (population de référence)", 
#                            true  = 0, 
#                            false = replace_na(eval_tidy(sym(str_replace(cur_column(),
#                                                                         "^ME_", "OR_"))), 1) ), 
#                          
#                          diff_type = "1", 
#                          color     = "diff", 
#                          #in_totrow = row_number() == 1,
#                          in_refrow = row_number() == 1, 
#                          col_var = cur_column() |> str_remove("^ME_"), 
#                          comp_all = FALSE, 
#                        ))
#   ) |> 
#     select(-starts_with(c("s_", "OR_", "p_"))) |>
#     #rename_with(~ str_remove(., "^ME_")) |>
#     #arrange(var) |>
#     new_tab()
#   )
# 
# # Export Excel
# map2(scoreRPS_OR, scoreRPS_ME, ~ left_join(mutate(.x, `  ` = ""), .y, 
#                                            by = c("var", "levels"))) |>
# tab_xl(colwidth = 8,  titles = names(scoreRPS_OR) )















# Sequence analysis  --------------------------------------------------


traj_df <- function(siasp_lg, var, filter_var = NULL, level_prefix, id = NULL,
                    n_sample = Inf, cleannames = FALSE) {
  
  siasp_lg <- siasp_lg |> select(starts_with(var), any_of(c(filter_var, id)))
  
  
  if (length(filter_var) != 0) siasp_lg <- siasp_lg |> 
      filter(!!sym(filter_var)) |> select(-any_of(filter_var))
  
  siasp_lg <- siasp_lg |> 
    select(starts_with(var), any_of(id)) |>
    rename_with(~str_extract(., "....$"), .cols = -any_of(id)) |>
    filter(!if_all(-any_of(id), is.na))
  
  if (n_sample < nrow(siasp_lg))  siasp_lg <- siasp_lg |> slice_sample(n = n_sample)
  
  
  if(!missing(level_prefix)) {
    siasp_lg <- siasp_lg |>
      mutate(across(-any_of(id),
                    ~ if_else(str_detect(., level_prefix), ., factor(NA_character_) )
      ))
  }
  
  if (cleannames) {
    siasp_lg <- siasp_lg |>
      mutate(across(-any_of(id),
                    ~ fct_relabel(., ~ str_remove_all(., tabxplor:::cleannames_condition()))
      ))
  }
  
  siasp_lg <- siasp_lg %>% select(sort(colnames(.)))
  
  if(length(id) != 0) siasp_lg <- siasp_lg |> column_to_rownames(var = id)
  
  siasp_lg
}




# simplify_corps(pan_any_in_CORPS$data[[1]], 
#                var = "CORPS_CONT_4_simp", name = as.character(pan_any_in_CORPS$CORPS4_any[[1]]), 
#                nomenc_var = "CORPS1", nomenc_by = "CORPS4"
#                ) |>
#   as_tibble() |> 
#   tab(CORPS_CONT_4_simp) |> arrange(-n)

# data <- pan_any_in_CORPS$data[[12]]
# nm   <- as.character(pan_any_in_CORPS$CORPS4_any[[12]])
# var <- "CORPS_CONT_4_simp"

simplify_corps <- function(data, var, pattern_keep, 
                           mode = c("niv_plus", "niv" ), etc = TRUE,
                           plus_threshold = 0.05, 
                           other = "WZDZZ-INDT", other_threshold = 0.01,
                           keep = c("A2E50-Élève ENA", "APL80-Élève ENS"),
                           nomenc = nomenc_CORPS_GRADE_EF_final, 
                           nomenc_var = "CORPS1", nomenc_by = "CORPS4") {
  #nm <- name
  VAR <- str2expression(var)
  nomenc_VAR <- str2expression(nomenc_var)
  
  simp <- setDT(data)
  simp <- simp[, .(n = .N), keyby = var]
  simp[, pct := n/sum(n) ] 
  simp <- simp |>
    left_join(distinct(nomenc, 
                       !!sym(nomenc_var), !!sym(nomenc_by)),
              by = set_names(nomenc_by, var) )
  setDT(simp)
  
  switch(
    mode[1], 
    
    "niv_plus" = {
      simp[, eval(nomenc_var) := if_else(
        str_detect(eval(VAR), pattern_keep) | eval(VAR) %in% keep | pct >= plus_threshold,
        true  = NA_character_,
        false = as.character(eval(nomenc_VAR))
      )]
    }, 
    
    "niv" = {
      simp[, eval(nomenc_var) := if_else(
        str_detect(eval(VAR), pattern_keep) | eval(VAR) %in% keep,
        true  = NA_character_,
        false = as.character(eval(nomenc_VAR))
      )]
      
    }
  )
  # print("here")
  
  simp[, pct_1 := sum(pct), by = eval(nomenc_var)]
  simp[, eval(nomenc_var) := case_when(
    eval(nomenc_VAR) == "A1-Cadres dirigeants" & pct < 0.005 ~ "A2-Cadres supérieurs",
    TRUE ~ eval(nomenc_VAR)
  )]
  setorderv(simp, cols = c(eval(nomenc_var), "n"), order = c(1, -1) )
  simp[, nomenc_var2 := eval(nomenc_VAR)]
  
  if (etc) {
    simp[, res := if_else(
      !is.na(nomenc_var2), 
      paste0(str_remove(eval(nomenc_VAR), "-.+$"), "-",
             str_remove(first(eval(VAR)), "^[^-]+-"), 
             " etc."),
      as.character(eval(VAR))
    ), 
    by = eval(nomenc_var)]
    
  } else {
    simp[, res := if_else(
      !is.na(nomenc_var2), 
      as.character(eval(nomenc_VAR)),
      as.character(eval(VAR))
    ), 
    by = eval(nomenc_var)]
  }
  
  simp[, nomenc_var2 := NULL]
  simp[, `:=`(n_res = sum(n),  pct_res = sum(pct)), by = "res"]
  simp[, res := if_else((pct_res < other_threshold) | #n_res < 50 | # is.na(eval(nomenc_VAR))
                          eval(VAR) %in% c(other), 
                        true  = "ZZZZZ-Autres", 
                        false = res)
  ]
  
  # # simp |> as_tibble() |> print(n = 100)
  # simp[, .(n = sum(n), pct = sum(pct)), keyby = "res"] |>
  #   #arrange(-n) |>  
  #   as_tibble() |> print(n = 150)
  
  recode_vect <- simp[, set_names(as.character(eval(VAR)), res)]
  
  data |> 
    mutate(!!sym(var) := fct_recode(!!sym(var), !!!recode_vect) |>
             fct_drop() |> fct_relevel(sort)
    )
  
}

# simp[, res := case_when( # quels seuils adopter ? n ou pct ?
#   CORPS_CONT_4_simp == names(pan_any_in_CORPS$data)[[4]]
#   ~ as.character(CORPS_CONT_4_simp),
#   
#   CORPS_CONT_4_simp == "WZDZZ-INDT" ~ NA_character_, 
#   
#   (pct < 5) & !is.na(CORPS1) ~ as.character(CORPS1), # n < 100 & 
#   TRUE                       ~ as.character(CORPS_CONT_4_simp)
# )
# ]
# simp[, `:=`(n_res = sum(n), pct_res = sum(pct)), by = "res"]
# simp[, res := case_when(
#   CORPS_CONT_4_simp == names(pan_any_in_CORPS$data)[[4]]
#   ~ as.character(CORPS_CONT_4_simp),
#   
#   pct_res < 3.5 & !is.na(CORPS1) ~ "Corps autres niveaux", # n_res < 80 &
#   is.na(CORPS1) & pct < 2        ~ "Autres",
#   TRUE                           ~ res
# )
# ]
# simp[, set_names(as.character(CORPS_CONT_4_simp), res)]


max_drop_between <- function(agnes, n_min = 5L, n_max = 12L, mode = c("last_above_mean", "max"), 
                             verbose = TRUE) {
  
  if (mode[1] == "max") {
    
    res <- tibble(height = sort(agnes$height, decreasing = TRUE)) |>
      mutate(diff = lag(height, default = height[1]) - height)
    
    if (verbose) print(res, n = n_max) 
    
    res |>
      filter(row_number() >= n_min & row_number() <= n_max) |>
      mutate(nclust = diff == max(diff)) |>
      filter(nclust) |>
      pull(n) |> last()
    
  } else if (mode[1] == "last_above_mean") {
    res <- tibble(height = sort(agnes$height, decreasing = TRUE)[1:(n_max)]) |>
      mutate(n = row_number(), 
             diff = lag(height, default = height[1]) - height, 
             diff2 = if_else(n < n_min, NA_real_, diff), 
             mean = mean(diff2, na.rm = TRUE), #pct_diff = diff/sum(diff)*100, 
             above_mean1 = n == max(which(diff >= mean))
      )  |>
      select(n, height, diff, diff2 , mean, above_mean1)
    
    if (verbose) print(res, n = n_max) 
    
    res |> 
      filter(above_mean1) |>
      select(n, height, diff, diff2, mean, above_mean1) |> 
      pull(n)
  } 
  
  
  
}

# n_clust <- 12L

all_clust <- function(agnes, n_clust = 12L, n_min = 5L, size_min = 1,
                      partition = c("no", "last_above_mean", "max")) {
  
  clusts <- map_dfc(set_names(1:n_clust, 1:n_clust), ~ cutree(agnes, .)) |>
    as.data.table()
  
  min_count <- map_int(set_names(names(clusts), names(clusts)),
                       ~ clusts[, .(n = .N), by = .][, min(n)]
  )
  
  if (partition[1] %in% c("last_above_mean", "max")) {
    n_clust <- max_drop_between(agnes, n_min = n_min, n_max = n_clust, mode = partition, 
                                verbose = FALSE)
    if ( min_count[n_clust] < size_min) {
      rev_min <- rev(min_count)
      n_clust <- names(rev_min)[min(which(rev_min >= size_min))]
      if (n_clust < n_min) n_clust <- n_min
    }
    clusts <- clusts |> select(all_of(c(as.character(1:n_clust))))
  }
  
  count_clust <- clusts[, .(n = .N), keyby = eval(as.character(n_clust))]
  
  all_clust <- distinct(clusts) |> mutate(rn1 = row_number())
  
  
  names_without_rn1 <- names(all_clust)[!names(all_clust) == "rn1"]
  first_cols  <- names_without_rn1[-length(names_without_rn1)]
  second_cols <- names_without_rn1[-1]
  res <- reduce2(first_cols, second_cols, .init = all_clust, 
                 .f = ~ nest(..1, data = -all_of(c(..2, ..3))) |>
                   mutate(rn = row_number()) |> 
                   group_by(!!sym(..2)) |> 
                   mutate(n = n(), res = nth(rn, 2)) |> 
                   ungroup() |> 
                   mutate(!!sym(..2) := replace_na(rn == res, FALSE)) |>
                   select(-rn, -n, -res) |> 
                   unnest(data)
  ) |> 
    arrange(rn1)
  
  heights <- tibble(height = sort(agnes$height, decreasing = TRUE)[1:n_clust] ) |>
    mutate(n = row_number(), 
           diff = lag(height, default = height[1]) - height
    )
  
  
  new_clust <- res |> 
    select(!!!syms(names_without_rn1)) %>%
    rename_with(~ c(.[-1], .[1])) |>
    select(-`1`) |> 
    mutate(across(everything(), ~ if_else(., cur_column(), ""))) |>
    mutate(new_clust = str_c(!!!syms(second_cols), sep = " ") |>
             str_squish() |> str_replace_all(" ", ", ")
           , 
           last = as.integer(str_extract(new_clust, "[^ ]+$")) |> replace_na(1L)
    ) |> 
    left_join(heights, by = c("last" = "n")) |>
    select(new_clust, last, last_height = height, last_diff = diff)
  
  
  clust_hierarchy <- bind_cols(left_join(all_clust, count_clust,
                                         by = as.character(n_clust)), 
                               new_clust) |> arrange(last) |>
    select(-rn1)
  
  clust_hierarchy[, pct := n/sum(n)][, n := NULL]
  clust_hierarchy[, n_clust := n_clust]
  
  clust_hierarchy
  
}


# chrono_data (from sequence analysis)
chronogramme <- function(data, seq_var, groups = NULL, name,
                         analysis_group, time_var = "time_var",
                         min_in_clust = 10, description, time_max = NULL, 
                         entpan = "ENTPAN2") {
  #table(pan_any_in_CORPS$part[[1]])
  chrono_data <- 
    select(data, ID_PAN, any_of(c(entpan = entpan)), all_of(groups), starts_with(seq_var)) |>
    as_tibble() |> 
    group_by(!!!syms(groups)) |> 
    mutate(n_clust = n()) |>
    ungroup() |>
    filter(n_clust >= min_in_clust) |>
    select(-n_clust) |> 
    pivot_longer(cols = starts_with(seq_var),
                 names_to = "time", names_prefix = paste0(seq_var, "_"), 
                 values_to = "state_in_seq")
  
  setDT(chrono_data)
  
  if (length(time_max) > 0) {
    chrono_data <- chrono_data[time <= time_max, ]
  }
  
  if (length(groups) >= 2) {
    #chrono_data[, clust := fct_cross(!!!syms(groups), sep = " ")]
    chrono_data[, eval(groups) := map(.SD, as.factor), .SDcols = groups]
    chrono_data <- chrono_data |> mutate(clust = fct_cross(!!!syms(groups), sep = " "))
  } else if (length(groups) == 1)  {
    chrono_data <- chrono_data |> rename(any_of(set_names(groups, "clust")))
    chrono_data[, clust := as.factor(clust)]
  } else {
    chrono_data[, clust := factor("all")]
  }
  
  chrono_data[, time := as.integer(time)]
  chrono_data <- chrono_data[, .(n = .N),
                             keyby = c("clust", "time", "state_in_seq")] 
  
  chrono_data[, wn := wt_panel(n, ENTPAN = entpan)] # before02 = TRUE
  chrono_data[, n := NULL]
  
  chrono_data[, field := name] 
  if (!missing(analysis_group)) chrono_data[, analysis_group := analysis_group] 
  chrono_data <- chrono_data[!is.na(wn),]
  chrono_data[, clust := as.factor(clust)]
  #if (length(groups) > 0) chrono_data[, eval(groups) := map(.SD, as.factor), .SDcols = eval(groups)]
  #chrono_data[, state_var := seq_var]
  chrono_data[, description := description]
  chrono_data[, time_var := eval(time_var)]
  
  if (length(groups) > 0) {
    chrono_data[, clust_vars := paste0(groups, collapse = ", ")]
    
  } else {
    chrono_data[, clust_vars := "all"]
    
  }
  
  chrono_data |> 
    select(description, field, time_var, any_of(c("analysis_group")),
           clust_vars, clust, time, state_in_seq, wn) # state_var
}








agregate_partitions_at_main_level <- function(partitions) {
  partitions |>
    group_by(description, field, time_var, analysis_group, part_level, new_clust) |>
    arrange(.by_group = TRUE) |> 
    summarise(
      across(starts_with("SM_NETR") & ends_with("_sd"), 
             ~ agregate_sd(mean = eval_tidy(sym(str_remove(cur_column(), "_sd$"))), 
                           sd   = .,
                           wt   = eval_tidy(sym(
                             str_replace(str_remove(cur_column(), "_sd$"), "SM_NETR_", "n_")
                           )) 
             )
      ), 
      
      across(starts_with("SM_NETR") & !ends_with("_sd"), 
             ~ weighted.mean(., 
                             w = eval_tidy(sym( str_replace(cur_column(), "SM_NETR_", "n_") )) 
             )
      ), 
      
      across(where(is.double) & !starts_with("SM_NETR") & -n & -wn & !starts_with("n_"), 
             ~ weighted.mean(., n) ), 
      
      across(c(n, wn) | starts_with("n_"), ~ sum(., na.rm = TRUE)),
      
      chrono = list2(chrono), 
      
      clust = list2(clust),
      
      .groups = "drop"
      
    ) |> 
    mutate(chrono = map(
      chrono, 
      ~ bind_rows(.) |> 
        group_by(time, state_in_seq) |>
        summarise(across(c(n, wn) | starts_with("n_"), 
                         ~ sum(., na.rm = TRUE)),
                  .groups = "drop")
    )) |> 
    select(description, field, time_var, analysis_group, part_level, 
           new_clust, chrono, n, wn, !ends_with("_sd"), everything()) |> 
    group_nest(description, field, time_var, analysis_group, 
               .key = "partitions", keep = TRUE) |>
    mutate(partitions = map(
      partitions, 
      ~group_by(., part_level) |> group_split() %>%
        set_names(map_chr(., ~ first(as.character(pull(., part_level))))) #|>
      #map(~ select(., -part_level))
    )
    )
}




imbricate_partitions <-  function(large_part, small_part, recode_vect) {
  large_part <- large_part |> #last(parts) |> 
    mutate(small_clust = fct_recode(clust, !!!recode_vect) |> sort_factor_as_integer(),
           part        = factor("large", levels = c("small", "large")),
           clust       = fct_cross(small_clust, clust, sep = " "),
           .after = "clust")
  
  
  small_part <- small_part |>
    mutate(small_clust = clust, 
           part        = factor("small", levels = c("small", "large"))
    )
  
  bind_rows(large_part, small_part) |> 
    arrange(small_clust, part) |> 
    mutate(clust = as_factor(as.character(clust))) |> # arrange(clust) 
    group_by(small_clust) |>
    mutate(order = as.factor(as.character(row_number() - 1L)) |>
             sort_factor_as_integer() |> fct_recode("bold(PRINCIPALE)" = "0")
    ) |>
    ungroup() 
}

make_imbricated_partitions <- function(id_small = NA_character_, hierarchy, parts, id_large) {
  if (missing(id_large)) id_large <- pull(hierarchy, n_clust) |> first() |> as.character()
  if (!missing(id_large)) if (id_large == "last") id_large <- last(names(parts)) # pull(hierarchy, n_clust) |> first() |> as.character()
  
  if (!is.na(id_small)) {
    recode_vect <-  hierarchy |>
      mutate(recode_vect = set_names(
        as.character(!!sym(id_large)), 
        !!sym(id_small)
      )
      ) |> 
      pull(recode_vect)
    
    large_part <- parts[[id_large]] 
    small_part <- parts[[id_small]] 
    
    imbricate_partitions(large_part, small_part, recode_vect)
    
  } else {
    parts[[id_large]]
  }
  
}





# data <- seq_data$partitions[[1]]$`10` |>  
#   # filter(field == "ETAT 10 ans" & time_var == "AGE20" & clust_vars == "clust" ) |> 
#   # filter(field == "A2E00-Administrateur civil" ) |> 
#   select(description, field, time_var, analysis_group, part_level, clust, chrono, 
#          n_clust = n) |>
#   unnest(chrono) 
# 
# pattern_params <- 
#   tribble(
#     ~state_in_seq     , ~fill    , ~pattern, ~patfill, ~density, ~spacing, ~angle, # ~group  ,
#     "NA"              , "white"  , "none"  , NA      , NA      , NA      , NA    , # "NA"    ,
#     "ZZDZZ-PRIV"      , "white"  , "stripe", "grey50", 0.4     , 0.05    , 30    , # "Autres",
#     "WZDZZ-INDT"      , "white"  , "circle", "grey50", 0.8     , 0.04    , 0     , # "Autres",
#     "XZDZZ-TERR"      , "white"  , "stripe", "black" , 0.5     , 0.02    , 330   , # "Autres",
#     "YZDZZ-HOSP"      , "white"  , "stripe", "black" , 0.5     , 0.02    , 90    , # "Autres",
#     "00-Fonctionnaire", "#440154", "none"  , NA      , NA      , NA      , NA    , # "État"  ,
#     "01-CDI"          , "#35608D", "none"  , NA      , NA      , NA      , NA    , # "État"  ,
#     "02-CDD"          , "#22A884", "none"  , NA      , NA      , NA      , NA    , # "État"  ,
#     "99-Indéterminé"  , "#BBDF27", "none"  , NA      , NA      , NA      , NA    , # "État"  ,
#   )
# # tabxplor:::fct_recode_helper(data, state_in_seq)
# # image(
# #   1:4, 1, as.matrix(1:4),
# #   col = viridis::viridis(4, option = "D", begin = 0, end = 0.9, direction = -1), # "D", "G"
# # )
# 
# grayscale <- FALSE
# cleannames <- TRUE
# 
# data <- imbricated_parts 
# salary_data <- SM_NETR_data

# data <- seq_data$imbricated_parts[[2]]
# chrono = "chrono"
# salary_vars_prefix = "SM_NETR"
# salary_sd = FALSE
# clust_desc_vars <- c("Femme.[SEXE]", "mob_DEPT")
# reverse_facets = FALSE
# cleannames = TRUE
# text_size = 1.5
# clust_text_space = 0.1


# data <- seq3c_manual$manual_parts[[1]]
# pattern_params = pattern_params_GRAD4
# agregate_states = agregate_states
# max_time = 40
# remove_na_after = 25
# salary_vars_prefix = "SM_NETR" # clust_names = FALSE, cluster_hierarchy = FALSE,
# clust_desc_vars    = c("Femme.[SEXE]",
#                        "1980_1989.[ENTPAN4]", "1990_1999.[ENTPAN4]", "2000_2009.[ENTPAN4]",
#                        "mob_DEPT" #,
# 
# )
# text_size = 2
# clust_text_space = 0.1
# 
# no_patterns = TRUE
# 
# grayscale = FALSE
# na = "keep"
# # na_cap_below
# salary_sd = FALSE
# clust_names = TRUE
# cluster_hierarchy = TRUE
# chrono = "chrono"
# reverse_facets = FALSE
# cleannames = TRUE
# 
# #pattern_scale = 1
# bar_color = "black"
# bar_stroke = 0
# pct_femme = "f"
# bars_width = 1.05

chrono_plot <- function(data, pattern_params, agregate_states, grayscale = FALSE, 
                        na = c("keep", "drop"), remove_na_after, na_cap_below,
                        salary_vars_prefix = "SM_NETR", salary_sd = FALSE, 
                        clust_desc_vars = NULL, max_time, allow_second_legend = TRUE,
                        clust_names = TRUE, cluster_hierarchy = TRUE, chrono = "chrono", 
                        reverse_facets = FALSE, cleannames = TRUE, no_patterns = FALSE,
                        text_size = 1.5, legend_text_size = 10, 
                        clust_text_space = 0.1, space_from_graph = 0.05, #pattern_scale = 1,
                        remove_bar_stroke_after = 25, # bar_color = "black", bar_stroke = NULL, 
                        pct_femme = "f",
                        reverse_top_bottom = FALSE, # bars_width = 1, 
                        ...) { # 
  
  # chronograms / sequence states plots
  chrono_data <- data |> 
    select(description, field, time_var, analysis_group, clust,
           any_of(c("part_level", "small_clust", "order", "part")), 
           chrono = all_of(chrono), n_clust = n) |>
    unnest(chrono)
  
  if (!missing(max_time)) chrono_data <- chrono_data |> filter(time <= max_time)
  
  if (!missing(agregate_states)) {
    chrono_data <- chrono_data |> 
      mutate(state_in_seq = fct_recode(state_in_seq, !!!agregate_states)) |>
      #pull(state_in_seq) |> fct_drop() |> levels()
      group_by(analysis_group, clust, time, state_in_seq) |>
      summarise(across(everything() & !n & !wn, first), 
                across(c(n, wn), ~ sum(., na.rm = TRUE)), 
                .groups = "drop"
                )
  }
  
  if ("rename_states" %in% names(pattern_params)) {
    recode_vect <- set_names(pattern_params$state_in_seq[!is.na(pattern_params$rename_states)], 
                             pattern_params$rename_states[!is.na(pattern_params$rename_states)])
    recode_vect <- recode_vect[recode_vect != names(recode_vect)]
    
    if (length(recode_vect) > 0) {
      chrono_data <- chrono_data |> 
        mutate(state_in_seq = fct_recode(state_in_seq, !!!recode_vect))
      
      pattern_params <- pattern_params |>
        mutate(state_in_seq = fct_recode(state_in_seq, !!!recode_vect))   
    }
  }
  
    time_points <- chrono_data$time |> unique() |> length()
    if (time_points >= remove_bar_stroke_after) {
      bar_stroke <- 0 
      bars_width <- 1.05
      bar_color  <- NA
    } else {
      bar_stroke <- 0.2 
      bars_width <- 1
      bar_color  <- "black"
    }

  
  chrono_data$state_in_seq <- chrono_data$state_in_seq |> fct_drop()
  
  if (na[1] == "keep") {
    chrono_data <- chrono_data |> mutate(state_in_seq = fct_na_value_to_level(state_in_seq, level = "NA"))
    
    if (!missing(remove_na_after)) {
      chrono_data <- chrono_data |>
        mutate(min_na = if_else(state_in_seq == "NA", n, NA_integer_)) |> 
        group_by(analysis_group, clust, time) |>
        mutate(min_na = sum(min_na, na.rm = TRUE)) |> 
        group_by(analysis_group, clust) |> 
        mutate(min_na = min(min_na, na.rm = TRUE)) |> 
        ungroup() #|> 
        #select(clust, time, state_in_seq, n, min_na) |> print(n = 200)
      
      chrono_data <- chrono_data |>
        mutate(n = if_else(state_in_seq == "NA" & time >= remove_na_after, 
                           true  = min_na,
                           false = n))
      
      #       if (!missing(na_cap_below)) {
      #   
      # } else {
      #   
      # }
    }
      
    } else if (na[1] == "drop") {
    chrono_data <- chrono_data |> filter(!is.na(state_in_seq))
  } else  {
    stop("na must be among 'keep' and 'drop'")
  }
  
  # if (any(is.na(pattern_params$state_in_seq))) { # na.rm
  #   chrono_data <- chrono_data |> mutate(state_in_seq = fct_na_value_to_level(state_in_seq, level = NA))
  # } else if ("NA" %in% pattern_params$state_in_seq) {
  #   chrono_data <- chrono_data |> mutate(state_in_seq = fct_na_value_to_level(state_in_seq, level = "NA"))
  # } else {
  # 
  # }
  
  # use different colors for CDI and CDD for CONTRAT and CORPS1
  if (any(str_detect(unique(chrono_data$description), "CORPS1"))) {
    pattern_params <- pattern_params |> 
      filter(! state_in_seq %in% c("01-CDI", "02-CDD")) |>
      mutate(state_in_seq = str_remove(state_in_seq, "_CORPS1$"))
  }
  
  
  if (cleannames) {
    chrono_data <- chrono_data |>
      mutate(state_in_seq = fct_relabel(state_in_seq, ~ str_remove_all(., tabxplor:::cleannames_condition())) )
    
    pattern_params <- pattern_params |> 
      mutate(state_in_seq = fct_relabel(state_in_seq, ~ str_remove_all(., tabxplor:::cleannames_condition())))
  }
  
  
  pattern_params <- pattern_params |> 
    distinct(state_in_seq, .keep_all = TRUE) |>
    filter(state_in_seq %in% levels(chrono_data$state_in_seq) | 
             is.na(state_in_seq) | state_in_seq == "NA")
  
  
  if (grayscale) {
    pattern_params$fill    <- DescTools::ColToGrey(pattern_params$fill)
    pattern_params$patfill <- DescTools::ColToGrey(pattern_params$patfill)
  }
  
  scales_for_pattern <- 
    list(                                                                     # =>>>>>>>>>
      if ("pattern" %in% names(pattern_params)) {scale_pattern_manual("",         values = set_names(pattern_params$pattern, pattern_params$state_in_seq), guide = guide_legend(order = 2))} else {NULL},
      if ("patfill" %in% names(pattern_params)) {scale_pattern_fill_manual("",    values = set_names(pattern_params$patfill, pattern_params$state_in_seq), guide = guide_legend(order = 2))} else {NULL},
      if ("density" %in% names(pattern_params)) {scale_pattern_density_manual("", values = set_names(pattern_params$density, pattern_params$state_in_seq), guide = guide_legend(order = 2))} else {NULL}, 
      if ("spacing" %in% names(pattern_params)) {scale_pattern_spacing_manual("", values = set_names(pattern_params$spacing, pattern_params$state_in_seq), guide = guide_legend(order = 2))} else {NULL}, 
      if ("angle"   %in% names(pattern_params)) {scale_pattern_angle_manual("",   values = set_names(pattern_params$angle  , pattern_params$state_in_seq), guide = guide_legend(order = 2))} else {NULL},
      if ("type"    %in% names(pattern_params)) {scale_pattern_type_manual("",    values = set_names(pattern_params$type   , pattern_params$state_in_seq), guide = guide_legend(order = 2))} else {NULL}#,
    )
  scales_for_pattern <- discard(scales_for_pattern, map_lgl(scales_for_pattern, is.null))
  
  pattern_list <- 
    list(pattern         = if ("pattern" %in% names(pattern_params)) {sym("state_in_seq")} else {NULL}, 
         pattern_fill    = if ("patfill" %in% names(pattern_params)) {sym("state_in_seq")} else {NULL},    # pattern_color = state_in_seq,
         pattern_density = if ("density" %in% names(pattern_params)) {sym("state_in_seq")} else {NULL}, 
         pattern_spacing = if ("spacing" %in% names(pattern_params)) {sym("state_in_seq")} else {NULL}, 
         pattern_angle   = if ("angle"   %in% names(pattern_params)) {sym("state_in_seq")} else {NULL},
         pattern_type    = if ("type"    %in% names(pattern_params)) {sym("state_in_seq")} else {NULL}
    )
  pattern_list <- discard(pattern_list, map_lgl(pattern_list, is.null))
  
  chrono_data <- chrono_data |>
    mutate(state_in_seq = fct_relevel(state_in_seq, !!!as.character(pattern_params$state_in_seq))
           
    ) |> 
    arrange(clust, time, state_in_seq) |> 
    mutate(#clust = paste0(clust, " (n = ", n_clust, ")") |> as_factor(), 
           time = as.integer(time)) |> 
    group_by(analysis_group, clust, time) |>
    mutate(pct = n/sum(n)) |> # n_clust
    ungroup() 
  
  not_in_scale <- 
    levels(chrono_data$state_in_seq)[!levels(chrono_data$state_in_seq) %in% pattern_params$state_in_seq]
  if (length(not_in_scale) > 0) {
    warning(paste0("levels not in pattern_params: ", 
                   paste0(not_in_scale, collapse = ", ")))
  }
  
  
  
  # clusters description (above graphs in each facet)
  if (TRUE) { # !missing(clust_desc_vars)
    #    we remove factors with 0% or 100% everywhere (or with only NAs)
    not_all_zero_or_100 <- data |> 
      select(all_of(clust_desc_vars)) |> 
      map_lgl(~ any(!is.na(.)) & 
                (any(. < -0.00005 | . > 0.00005) & any(. < 1 - 0.00005 | . > 1 + 0.00005)) )
    clust_desc_vars <- clust_desc_vars[not_all_zero_or_100]
    
    cluster_hierarchy_var <- if (cluster_hierarchy) {
      "appear"
    } else {
      NULL
    }
    
    clust_desc_data <- data |> 
      select(clust, any_of(c("small_clust", "order", "part", "analysis_group")), n, 
             any_of(cluster_hierarchy_var),
             all_of(clust_desc_vars), any_of(paste0(clust_desc_vars, "_sd"))
      )
    
    if (!is.null(clust_desc_vars)) clust_desc_data <- clust_desc_data |> 
      tab_dt_to_tab(n_var = "n") 
    
    #clust_desc_data <- clust_desc_data |> # , group = "part" # (same with complete partition)
    #  rename_with(~ str_remove_all(., "\\[.*\\]") |> str_replace("\\.", "") |> str_squish()) #|>
    ##select(-all_of(c("description", "field", "time_var", "analysis_group", "part_level", "small_clust", "order")))
    
    if ("small_clust" %in% names(clust_desc_data) & "order" %in% names(clust_desc_data) ) {
      clust_desc_data <- clust_desc_data |> group_by(part) 
      if ("analysis_group" %in% names(clust_desc_data)) clust_desc_data <- clust_desc_data |> group_by(analysis_group, .add = TRUE) 
      clust_desc_data <- clust_desc_data |> mutate(pct = round(n/sum(n), 3)) |> ungroup()
    
      } else {
      if ("analysis_group" %in% names(clust_desc_data)) clust_desc_data <- clust_desc_data |> group_by(analysis_group, .add = TRUE) 
      clust_desc_data <- clust_desc_data |> mutate(pct = round(n/sum(n), 3)) |> ungroup() 
    }
    clust_desc_data <- clust_desc_data |> 
      mutate(n = paste0(format(n), " (", format(pct*100, nsmall = 1), "%)"))
    
    # clust_desc_vars_simp <- clust_desc_vars |> 
    #   str_remove_all("\\[.*\\]") |> str_replace("\\.", "") |> str_squish()
    
    min_time <- min(chrono_data$time, na.rm = TRUE)
    max_time_max <- max(chrono_data$time, na.rm = TRUE)
    
    if (!is.null(clust_desc_vars)) {
      clust_desc_vars_bracket <- 
        if_else(!str_detect(clust_desc_vars, "\\["), paste0(clust_desc_vars, "[]"), clust_desc_vars)
    } else {
      clust_desc_vars_bracket <- NULL
    }

    if (!is.null(clust_desc_vars)) {
    clust_desc_color <- clust_desc_data |> 
      select(clust, any_of(c("small_clust", "order", "part")), 
             all_of(clust_desc_vars)) |>
      rename(all_of(set_names(clust_desc_vars, clust_desc_vars_bracket))) |>
      mutate(across(all_of(clust_desc_vars_bracket), 
                    tabxplor:::fmt_get_color_code)) |> 
      pivot_longer(cols = all_of(clust_desc_vars_bracket), 
                   names_to = c("variable", "whole_var"), 
                   names_pattern = "(^[^\\[]+)\\[([^\\]]*)\\]",
                   values_to = "color") |>
      mutate(variable  = str_remove_all(variable, "\\."), 
             whole_var = if_else(whole_var == "", variable, whole_var))
    
    if (grayscale) clust_desc_color <- clust_desc_color |> mutate(color = "black")
    
    join_colors_by <- if ("small_clust" %in% names(chrono_data) & "order" %in% names(chrono_data) ) {
      c("clust","small_clust", "order", "part", "variable", "whole_var")
    } else {
      c("clust", "variable", "whole_var")
    }
    
  with_brackets_match <- set_names(clust_desc_vars, clust_desc_vars_bracket)
   
    } else {
      with_brackets_match <- NULL
    }
   
 if (clust_names) {
   clust_desc_data <- clust_desc_data |> mutate(`clust[]` = clust)
   
   if (cleannames) {
     clust_desc_data <- clust_desc_data |>
       mutate(`clust[]` = fct_relabel(`clust[]`, ~ str_remove_all(., tabxplor:::cleannames_condition())) )
   }
 } 
    
    clust_desc_data <- clust_desc_data |> 
      mutate(across(all_of(clust_desc_vars), format)) |> 
      rename(`n[]` = n, any_of(c("appear[]" = "appear")), 
             all_of(with_brackets_match)) |>
      pivot_longer(cols = any_of(c("n[]", "clust[]", "appear[]", 
                                   clust_desc_vars_bracket)), 
                   names_to = c("variable", "whole_var"), 
                   names_pattern = "(^[^\\[]+)\\[([^\\]]*)\\]",
                   values_to = "value") |> 
      mutate(variable  = str_remove_all(variable, "\\."), 
             whole_var = if_else(whole_var == "", variable, whole_var))
    
    if (!is.null(clust_desc_vars)) {
      clust_desc_data <- clust_desc_data |> 
      left_join(clust_desc_color, by = join_colors_by)
    } else {
      clust_desc_data <- clust_desc_data |> mutate(color = NA_character_)
    }
      
    # n + femme même ligne ; name et appear sur la même ligne
    clust_desc_data <- clust_desc_data |> 
      mutate(value    = if_else(variable == "n", paste0("n=", value), value), 
             value    = if_else(variable == "Femme", paste0(value, pct_femme), value),
             value    = if_else(variable == "appear", paste0("niv ", value), value),
             variable = if_else(whole_var == "SEXE", "n", variable), 
             variable = if_else(whole_var %in% c("clust", "appear"), "name", variable)
      )
    
    # tous les décennies d'entrées sur la même ligne
    if (any("ENTPAN4" %in% clust_desc_data$whole_var)) {
      ENTPAN4_levels <- clust_desc_data |> filter(whole_var == "ENTPAN4") |> pull(variable) |> unique()
      ENTPAN4_new_level <- ENTPAN4_levels |> str_sub(3L, 4L) |> paste0("'") |>
        paste0(collapse = "")
      
      clust_desc_data <- clust_desc_data |>
        mutate(ENTPANnb = map_if(variable, whole_var == "ENTPAN4", 
                                 ~ which(ENTPAN4_levels == .), 
                                 .else = ~ NA_integer_) |> flatten_int(), 
               variable = if_else(whole_var == "ENTPAN4", ENTPAN4_new_level, variable))
      }
    
    clust_desc_data <- clust_desc_data |> 
      mutate(# color    = tabxplor:::fmt_get_color_code(value), 
             # value    = format(value), 
             variable = as_factor(variable) |> # => levels in order of appearance
               fct_relevel("name"), 
             
             # up from the graph
             y_coord  = 1 + space_from_graph + clust_text_space*nlevels(variable) - clust_text_space*as.integer(variable), #  + 1/(4*nlevels(variable)
             ## right from the graph
             # y_coord  = 1 - 1/nlevels(variable) * as.integer(variable) + 1/(2*nlevels(variable)), 
             # offset   = 1/(2*nlevels(variable))
      )
    # clust_desc_data |> print(n = 40)
    
    clust_desc_data <- clust_desc_data |> 
      pivot_longer(cols = c(variable, value), names_to = "type", values_to = "text") |>
      mutate(
        fontface = if_else(type == "value" & !is.na(color), "bold", "plain"),
        color    = if_else(type == "variable", "black", replace_na(color, "black")), 
        text     = if_else(type == "variable", paste0(text, ":"), text),
        
        # up from the graph
        x_coord  = if_else(type == "variable", min_time, max_time_max + 1), # min_time - 0.5, max_time_max + 0.5
        ## right from the graph
        # x_coord  = if_else(type == "variable", max_time_max + 1L, max_time_max + 6L), 
        # y_coord  = if_else(type == "variable", y_coord, y_coord - offset + 1/4 * offset), 
        
        hjust    = if_else(type == "variable", 0, 1), 
      ) 
    
    # n + femme sur la même ligne
    y_coord_n <- clust_desc_data |> filter(whole_var == "n") |> pull(y_coord) |> first()
    
    # appear et name sur la même ligne
    y_coord_name <- clust_desc_data |> filter(whole_var %in% c("clust", "appear")) |> pull(y_coord) |> first()
    
    clust_desc_data <- clust_desc_data |> 
      filter(!(type == "variable" & whole_var %in% c("n", "SEXE", "clust", "appear"))) |> 
      mutate(x_coord = if_else(whole_var == "n" & type == "value", min_time, x_coord),  # - 0.5
             hjust   = if_else(whole_var == "n" & type == "value", 0  , hjust), 
             
             x_coord = if_else(whole_var == "SEXE" & type == "value", max_time_max + 1, x_coord), # 0.5
             y_coord = if_else(whole_var == "SEXE" & type == "value", 
                               true  = y_coord_n, 
                               false = y_coord), 
             hjust   = if_else(whole_var == "SEXE" & type == "value", 1, hjust), 
             
             x_coord = if_else(whole_var == "clust" & type == "value", min_time, x_coord),  # - 0.5
             #y_coord = if_else(whole_var == "clust" & type == "value", 
             #                  true  = y_coord_name, 
             #                  false = y_coord), 
             hjust   = if_else(whole_var == "clust" & type == "value", 0, hjust), 
             
             fontface= if_else(whole_var %in% c("clust", "appear"), "bold", fontface),
             
      )
    
    # Remove clust name if they are already on facet_grid, when many analysis groups
    if ("analysis_group" %in% names(data)) {
      if (length(unique(pull(data, analysis_group))) >= 2) {
        #print(clust_desc_data)
        clust_desc_data <- clust_desc_data |> filter(whole_var != "clust")
      }
    } 
    
    # toutes les décennies d'entrée sur la même ligne
    if (any("ENTPAN4" %in% clust_desc_data$whole_var)) {
      max_ENTPANnb <- clust_desc_data$ENTPANnb |> max(na.rm = TRUE)
      
      start <- (min_time + max_time_max + 1)*4.5/10 # (0.5 + max_time_max + 0.5)/2
      stop  <- max_time_max + 1
      steps <- (stop - start)/(max_ENTPANnb + 0.5)
      #  start + steps * 1 ; start + steps * 2 ; start + steps * 3 ; start + steps * 3.5
      
      clust_desc_data <- clust_desc_data |>
        filter(!(type == "variable" & !is.na(ENTPANnb) & ENTPANnb > 1)) |> 
        mutate(x_coord = if_else(type == "value" & whole_var == "ENTPAN4",
                                 true  = start + steps * ENTPANnb, 
                                 false = x_coord),
               # hjust = if_else(type == "value" & whole_var == "ENTPAN4" & ENTPANnb < max_ENTPANnb,
               #                 true  = 0, 
               #                 false = hjust),
               text = if_else(type == "value" & whole_var == "ENTPAN4", 
                              true  = str_remove(text, "%$"), 
                              false = text), 
               #text = if_else(type == "value" & whole_var == "ENTPAN4" & ENTPANnb == max_ENTPANnb, 
               #               true  = str_remove(text, "%$"), 
               #               false = text)
        )
      
      pct_sign <- clust_desc_data |>
        filter(!is.na(ENTPANnb) & ENTPANnb == max_ENTPANnb) |> 
        mutate(text = "%", x_coord = max_time_max + 1, hjust = 1,  # + 0.5
               color = "black", fontface = "plain")
      
      clust_desc_data <- clust_desc_data |> bind_rows(pct_sign)
      
    }
    
    clust_desc_graph <-
      list(
        geom_text(data = clust_desc_data, 
                  aes(x = x_coord, y = y_coord, 
                      label = text, color = color, fontface = fontface, hjust = hjust), 
                  vjust = 0.5, size = text_size
        ), 
        
        scale_color_identity()
      )
    
  } else {
    clust_desc_graph <-  NULL
  }
  

  # salary evolution curve, with second y axis
  with_salary_evolution <- any(str_detect(names(data), paste0("^", salary_vars_prefix)))
  if (with_salary_evolution) {
       salary_data <- 
        left_join(
          data |> 
            select(clust, any_of(c("analysis_group", "small_clust", "order")),
                   starts_with("SM_NETR") & ! ends_with("_sd") & !where(~all(is.na(.)))) |> 
            pivot_longer(cols = starts_with("SM_NETR"), names_to = "time", names_transform = as.integer,
                         names_prefix = "SM_NETR_", values_to = "SM_NETR"),
          
          data |> 
            select(clust, any_of(c("analysis_group", "small_clust", "order")),
                   starts_with("SM_NETR") & ends_with("_sd") & !where(~all(is.na(.)))) |> 
            rename_with(~ str_remove(., "_sd")) |>
            pivot_longer(cols = starts_with("SM_NETR"), names_to = "time", names_transform = as.integer,
                         names_prefix = "SM_NETR_", values_to = "SM_NETR_sd"),
          
          by = if ("small_clust" %in% names(data) & "order" %in% names(data)) {
            c("clust", "small_clust", "order", "time")
          } else {
            if ("analysis_group" %in% names(data)) {
              c("analysis_group", "clust", "time")
            } else {
              c("clust", "time")
            }
            
          }
        )

       if (!missing(max_time)) salary_data <- salary_data |> filter(time <= max_time)
       
    salary_coeff <- max(salary_data$SM_NETR, na.rm = TRUE) # + salary_data$SM_NETR_sd
    
    salary_breaks <- with(salary_data, labeling::extended(0, salary_coeff*9/10, m = 5))
    
    salary_curve_color <- "#3366FF"
    if (grayscale) salary_curve_color <- salary_curve_color |> DescTools::ColToGray()
    salary_sd_fill <- "red"
    if (grayscale) salary_sd_fill <- salary_sd_fill |> DescTools::ColToGray()
    
    salary_graph_and_scale_y <- 
      list(
        if (salary_sd) {
          NULL
        } else {
          geom_line(data = salary_data,
                    aes(y = SM_NETR / salary_coeff, 
                        group = analysis_group),
                    stat = "identity", color = "white", alpha = 0.8, linewidth = 2)
        }, 
        
        if (salary_sd) {
          geom_smooth(data = salary_data,
                      aes(y    = SM_NETR / salary_coeff, 
                          ymax = (SM_NETR + SM_NETR_sd) / salary_coeff,
                          ymin = (SM_NETR - SM_NETR_sd) / salary_coeff, 
                          group = analysis_group),
                      stat = "identity", color = salary_curve_color, fill = salary_sd_fill) 
        } else {
          geom_line(data = salary_data,
                    aes(y = SM_NETR / salary_coeff, 
                        group = analysis_group),
                    stat = "identity", color = salary_curve_color, linewidth = 0.75) # 0.5
        }, 
        
        scale_y_continuous("", 
                           breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75), 
                           labels = ~ paste0(round(.*100), "%"),
                           #limits = c(0, 1), oob = scales::squish, 
                           sec.axis = sec_axis(trans  = ~.*salary_coeff, 
                                               breaks = salary_breaks, 
                                               labels = function(br){paste0(round(br), "€")}
                           ) # , name="Second Axis"
        )
      ) 
    
  } else {
    salary_graph_and_scale_y <- list(
      scale_y_continuous("", 
                         breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75), 
                         labels = ~ paste0(round(.*100), "%") )
    )
  }
  
  
  # facets 
  facets <- if ("small_clust" %in% names(chrono_data) & "order" %in% names(chrono_data) ) {
    list(facet_grid(rows = if (reverse_facets) {vars(order)} else {vars(small_clust)},
                    cols = if (reverse_facets) {vars(small_clust)} else {vars(order)},
                    
                    #labeller = "label_parsed", switch = "y", # no used anymore
                    as.table = !reverse_top_bottom
                    ),

         # annotate(geom = 'segment', x = -Inf, xend = -Inf, y = -Inf, yend = Inf, 
         #          linewidth = 0.1), 
         # 
         # annotate(geom = 'segment', x =  Inf, xend =  Inf, y = -Inf, yend = Inf, 
         #          linewidth = 0.1),
         
         geom_rect(
           data = data.frame(order = factor(levels(chrono_data$order)[1], levels = levels(chrono_data$order)),
                             xmin = -Inf #  ff = factor(0)
           ), 
           aes(xmin = xmin), # aes(color = ff),
           color = "black", fill = NA, linewidth = 1, inherit.aes = FALSE, 
           xmax=Inf, ymin=-Inf, ymax=Inf) #, 
         
         #scale_color_manual(values = c("0" = "black"), breaks = NULL)
    )
  } else {
    
    # S'il n'y a qu'un seul analysis group ou si la variable n'existe pas
    if (!"analysis_group" %in% names(data)) {
      see_grid <- FALSE
      facet_wrap("clust", ...) #, theme(strip.text = element_blank() )
      
    } else {
      
      if (length(unique(pull(data, analysis_group))) == 1) {
        see_grid <- FALSE
        facet_wrap("clust", ...) 
        
        # S'il y a plusieurs analysis group
      } else {
        see_grid <- TRUE
        list(
          facet_grid(rows = vars(analysis_group),
                     cols = vars(clust),
                     switch = "y", 
                     #scales = "free_y", 
                     
                     #labeller = "label_parsed", switch = "y", # no used anymore
                     as.table = !reverse_top_bottom
          ) ,
          
          geom_hline(color = "black", linewidth = 0.5,  yintercept = Inf),
          geom_hline(color = "black", linewidth = 0.5,  yintercept = -Inf)
          
          #   theme(strip.text       = element_text(face = "bold", margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")), 
          #         # strip.placement  = "",#strip.clip       = , 
          #         panel.border     = element_rect(color = "black", fill = NA),  
          #         strip.background = element_rect(fill = "grey80")
          #   )
         )
        }
      
      
    }
    
  }
  

  
  
  # Add another fill legend (with empty plot) if in pattern_params
  if (allow_second_legend & "fill_legend2" %in% names(pattern_params)) {
    fill2_values <- pattern_params |> 
      filter(!is.na(state_in_seq) & state_in_seq != "NA") |> 
      select(fill, fill_legend2) |> 
      distinct(fill_legend2, .keep_all = TRUE) |> 
      mutate(recode_vect = set_names(as.character(fill), fill_legend2)) |> pull(recode_vect)
    
    legend2_recode_vect <- set_names(as.character(pattern_params$state_in_seq),
                                     pattern_params$fill_legend2)
    
    fill_legend2_data <- chrono_data |> 
      filter(!is.na(state_in_seq) & !state_in_seq == "NA") |> 
      mutate(fill_legend2 = fct_recode(state_in_seq, !!!legend2_recode_vect) |> 
               fct_relevel(sort), 
             pct = pct/10
      ) #|>
    #pull(fill_legend2)
    
    if (cleannames) {
      fill_legend2_data <- fill_legend2_data |>
        mutate(fill_legend2 = fct_relabel(fill_legend2, ~ str_remove_all(., tabxplor:::cleannames_condition())) )
      
      names(fill2_values) <- str_remove_all(names(fill2_values), tabxplor:::cleannames_condition())
    }
    
    legend2 <- list(geom_bar(data = fill_legend2_data, 
                             aes(fill = fill_legend2),
                             stat = "identity", color = bar_color, 
                             linewidth = bar_stroke, just = 0),
                    scale_fill_manual("", values = fill2_values, 
                                      guide = guide_legend(order = 1)), 
                    ggnewscale::new_scale_fill()
    )
  } else {
    legend2 <- NULL
  }
  

  
  # plot the final graph
  bar_graph <- if (no_patterns) {
    geom_bar(aes(fill = state_in_seq), 
             stat = "identity", color = bar_color, linewidth = bar_stroke, 
             width = bars_width, just = 0
    )
  } else {
    geom_bar_pattern(aes(!!!pattern_list, fill = state_in_seq),
                     stat = "identity", color = bar_color, linewidth = bar_stroke, 
                     width = bars_width,
                     pattern_size = NA, pattern_color = NA, # no stroke color (only fill)
                     #pattern_scale = pattern_scale, # not doing anything
                     pattern_key_scale_factor = 1/3, just = 0
    )
  }
  
  time_variable <- first(chrono_data$time_var)
  
  chrono_data |>
    ggplot(aes(x = time, y = pct)) + 
    legend2 + 
    bar_graph + 
    scale_x_continuous(
      if (time_variable == "AGE20") {"Age"} else {as.character(time_variable)}, 
      breaks = break_uniform_integer(range(chrono_data$time)), 
      labels = if (time_variable == "AGE20") {
        ~ as.character(round(. + 20, 1)) 
      } else {
        ~ as.character(round(., 1)) 
      }
    ) + 
    scale_fill_manual("", values = set_names(pattern_params$fill, pattern_params$state_in_seq), 
                      guide = guide_legend(order = 2)) +
    scales_for_pattern + 
    salary_graph_and_scale_y + 
    clust_desc_graph +
    facets +
    theme_minimal() + 
    theme(text = element_text(size = legend_text_size), 
          legend.position   = "right", 
          legend.text       = element_text(size = rel(0.6)), # text_size
          legend.box.margin = margin(0, 0, 0, 0), 
          legend.margin     = margin(0, 0, 0, 0), 
          legend.box.spacing= unit(4, "pt"), 
          #panel.grid      = element_blank()
          panel.grid.major.x = element_blank(), # element_line(color = "grey80", linewidth = 0.2, linetype = "dashed"), #element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2, linetype = "dashed"), # element_line(color = "grey80", linewidth = 0.2, linetype = "longdash"),
          panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2, linetype = "dashed"), # element_blank(), # element_line(color = "grey92"), 
          axis.ticks         = element_line(color = "grey30", linewidth = 0.2),
          axis.text          = element_text(color = "grey30", size = rel(0.5)), # default 11
          axis.title.y       = element_blank(), 
          #panel.ontop        = TRUE 
          
          panel.spacing    = unit(0, "lines"), 
          strip.text       = if (see_grid) {element_text(face = "bold", size = rel(1), margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt"))} else {element_blank()}, 
          # strip.placement  = "",#strip.clip       = , 
          #panel.border     = if (see_grid) {element_rect(color = "black", fill = NA, linewidth = 0.1)} else {element_blank()},
          strip.background = if (see_grid) {element_rect(color = "black", linewidth = 0.5, fill = "grey85")} else {element_blank()} ,
          #axis.line.y.left = element_blank(),
          axis.line.y.right= if (see_grid) {element_line(color = "black", linewidth = 0.5)} else {element_blank()},
          axis.line.x      = if (see_grid) {element_line(color = "black", linewidth = 0.5)} else {element_blank()} #,
          
             )
}

# if (FALSE) {
#   min_theme <- theme_minimal()
#   min_theme$panel.grid
#   min_theme$axis.ticks
#   min_theme$axis.text
#   min_theme$text
#   min_theme$legend.text
#   min_theme$legend.key
#   min_theme$legend.key.size
#   min_theme$legend.margin
#   min_theme$legend.box.spacing
#   # min_theme$plot.margin
#   
#   classic_theme <- theme_classic()
#   classic_theme$line
#   classic_theme$axis.ticks
#   classic_theme$axis.ticks.length
#   classic_theme$axis.text
# 
# }













## legacy + MSA ----


# data <- ctp
# vars_prefixes <- one_for_each_version #c("DPO2", "PPP1", "INCIDENT")
# remove_levels = c("^pas ", "^non ", "^no ", "^not ")
# sep = "/"
# keep_intermediate = FALSE
# n_min = 30
# other_level = "Autres"
# max_levels = 50
# na = "drop"

longitudinal_cross <- 
  function(data, vars_prefixes, remove_levels = c("^pas ", "^non ", "^no ", "^not "),
           sep = "/", keep_intermediate = FALSE, n_min = 30, other_level = "Autres",
           max_levels = 50, na = "drop"
           #type = c("/", "_")
  ) {
    
    vars <- purrr::map(vars_prefixes, ~ names(tidyselect::eval_select(
      tidyselect::starts_with(paste0(., "_")), data)
    )) %>% set_names(vars_prefixes)
    
    sep               <- vctrs::vec_recycle(sep, length(vars))
    n_min             <- vctrs::vec_recycle(n_min, length(vars))
    #other_level       <- vctrs::vec_recycle(other_level, length(vars))
    keep_intermediate <- vctrs::vec_recycle(keep_intermediate, length(vars))
    
    less_than_2_vars <- purrr::map_int(vars, length) < 2
    if(any(less_than_2_vars)) {
      if (all(less_than_2_vars)) stop("all variables not found or uniques")
      
      warning(paste0("some variables prefixes not found, ",
                     "or corresponding to less than 2 variables: ", 
                     paste0(names(less_than_2_vars)[less_than_2_vars], collapse = ", ")))
      vars <- vars[!less_than_2_vars]
      sep               <- sep[!less_than_2_vars]
      n_min             <- n_min[!less_than_2_vars]
      #other_level       <- other_level[!less_than_2_vars]
      keep_intermediate <- keep_intermediate[!less_than_2_vars]
    }
    
    num_vars <- purrr::map_lgl(purrr::map(vars, ~ pull(data, .[1])), 
                               ~ is.numeric(.) | is.logical(.))
    if(any(num_vars)) {
      if (all(num_vars)) stop("all remaining variables are numeric")
      
      warning(paste0("some variables are numeric and were not crossed: ",
                     paste0(names(num_vars)[num_vars], collapse = ", ")))
      vars <- vars[!num_vars]
      sep               <- sep[!num_vars]
      n_min             <- n_min[!num_vars]
      #other_level       <- other_level[!num_vars]
      keep_intermediate <- keep_intermediate[!num_vars]
    }
    
    char_vars <- purrr::map_lgl(purrr::map(vars, ~ pull(data, .[1])), is.character)
    if(any(char_vars)) {
      data <- data %>% mutate(across(starts_with(names(char_vars)[char_vars]), as.factor))
    }
    
    
    less_than_2_lvs <- purrr::map_lgl(purrr::map(vars, ~ pull(data, .[1])), 
                                      ~ replace_na(nlevels(.) < 2), FALSE)
    if(any(less_than_2_lvs)) {
      if (all(less_than_2_lvs)) stop("all remaining variables have less than 2 levels")
      
      warning(paste0("some variables have less than 2 levels: ",
                     paste0(names(less_than_2_lvs)[less_than_2_lvs], collapse = ", ")))
      vars <- vars[!less_than_2_lvs]
      sep               <- sep[!less_than_2_lvs]
      n_min             <- n_min[!less_than_2_lvs]
      #other_level       <- other_level[!less_than_2_lvs]
      keep_intermediate <- keep_intermediate[!less_than_2_lvs]
    }
    
    more_than_X_lvs <- purrr::map_lgl(purrr::map(vars, ~ pull(data, .[1])), 
                                      ~ replace_na(nlevels(.) > max_levels), FALSE)
    if(any(more_than_X_lvs)) {
      if (all(more_than_X_lvs)) stop("all remaining variables have more than ",
                                     max_levels , " levels")
      
      warning(paste0("some variables have more than ", max_levels , " levels: ", 
                     paste0(names(more_than_X_lvs)[more_than_X_lvs], collapse = ", ")))
      vars <- vars[!more_than_X_lvs]
      n_min             <- n_min[!more_than_X_lvs]
      sep               <- sep[!more_than_X_lvs]
      #other_level       <- other_level[!more_than_X_lvs]
      keep_intermediate <- keep_intermediate[!more_than_X_lvs]
    }
    
    if (any(!keep_intermediate)) vars <- purrr::map_if(vars, !keep_intermediate,
                                                       ~ c(first(.), last(.)))
    
    if (na[1] == "keep") {
      data  <- data %>% 
        mutate(across(all_of(flatten_chr(vars)), ~ fct_explicit_na(., "NA") ))
    }
    
    lvs <- purrr::map(vars,
                      ~ levels(pull(data, .[1])) %>% str_replace_all(., "/", " ") %>% 
                        str_remove(tabxplor:::cleannames_condition()) %>% 
                        str_remove("\\([^\\(]+\\)") %>% 
                        str_squish()
    )
    
    new_vars <-  data %>% 
      mutate(across(all_of(suppressWarnings(flatten_chr(vars))),
                    ~ fct_relabel(., ~ as.character(1:length(.)) %>% 
                                    paste0(., str_replace(cur_column(),
                                                          "^.+(_[^_]+$)",
                                                          "\\1" ))
                    )
      ))
    
    new_vars <- 
      purrr::imap(vars, 
                  ~ transmute(new_vars,
                              !!.y := fct_cross(!!!syms(sort(.x, decreasing = TRUE)),
                                                sep = "/")
                  )) %>% 
      flatten_df()
    
    new_vars <- 
      purrr::map2_df(new_vars, n_min, 
                     ~ fct_lump_min(..1, min = ..2, other_level = "0/")
      )
    
    
    levels_matrix <- new_vars %>% 
      purrr::imap(~ map_df(str_split(levels(.), "/"), 
                           ~ as_tibble(set_names(as.list(.), length(.):1))) %>% 
                    select(sort(names(.)))
      )
    
    levels_matrix <- levels_matrix %>% 
      map(function(df) set_names(map_dfc(df, function(var) str_remove(var, "_[^_]+$")), 
                                 str_replace(slice(df, 1), "^.+(_[^_]+$)", "\\1") %>% 
                                   replace(which(. == ""), "error")
      ))
    
    levels_matrix <- 
      levels_matrix %>% 
      #map_if(map_lgl(levels_matrix, ~ ncol(.) > 0 & nrow(.) > 0 & !"" %in% names(.)), 
      map( ~ rename_with(., ~ replace(., which(. == ""), "error")) %>% 
             rowwise(.) %>% 
             mutate(other = any(c_across() == "0")) %>% #if(length(last(names(.))) != 0) {!!sym(last(names(.))) == "§O§"} else {FALSE}) %>%
             ungroup() %>%
             mutate(across(-other, ~ if_else(other, "0", .))) %>%
             select(-other)
      )
    
    # levels_matrix <- levels_matrix %>% 
    #   map_if(map_lgl(levels_matrix, ~ ncol(.) > 0), 
    #          ~ mutate(., other = !!sym(last(names(.))) == "0") %>% #if(length(last(names(.))) != 0) {!!sym(last(names(.))) == "§O§"} else {FALSE}) %>% 
    #            mutate(across(-other, ~ if_else(other, "0", .))) %>% 
    #            select(-other)
    #   )
    
    # any(map_lgl(levels_matrix, ~ ncol(.) == 0))
    
    # levels_matrix <- 
    #   purrr::imap(vars, 
    #               ~ map_df(str_split(levels(pull(new_vars, .y)), "/"), 
    #                        ~ as_tibble(set_names(as.list(.), length(.):1))) %>% 
    #                 select(sort(names(.))) %>% 
    #                 rename(set_names(names(.), str_remove(.x, .y)))
    #   )
    
    remove_levels <- str_c(remove_levels, collapse = "|")
    negative_levels <- purrr::map(lvs, ~ as.character(
      (1:length(.))[which(str_detect(., regex(remove_levels, ignore_case = TRUE)))]
    ))
    
    new_levels <- 
      purrr::map2(levels_matrix, negative_levels,
                  ~  mutate(.x, across(everything(), 
                                       ~ if_else(. %in% .y, true  = "", false = .)))
      )
    
    new_levels <- 
      purrr::pmap(list(new_levels, lvs, sep),
                  ~ rowwise(..1) %>% 
                    mutate(unique_lv = list(unique(str_c(c_across()) %>% discard(. == "")))) %>% 
                    ungroup() %>% 
                    mutate(unique_lv = case_when(
                      map_int(unique_lv, length) == 0 ~ list(""), 
                      map_int(unique_lv, length) == 1 ~ unique_lv, 
                      map_int(unique_lv, length) >= 2 ~ list(NA_character_)
                    )) %>% 
                    mutate(unique_lv = flatten_chr(unique_lv)) %>% 
                    select(unique_lv, everything()) %>% 
                    mutate(across(-unique_lv, ~ case_when(
                      unique_lv == "0"   ~ "",
                      !is.na(unique_lv)  ~ if_else(. == "", "", cur_column()),
                      TRUE               ~ if_else(. == "", "", paste0(..2[replace(as.integer(.),
                                                                                   which(. == "0"), 
                                                                                   1
                      )],
                      "_",
                      cur_column(), ..3))
                    ))) %>%
                    mutate(unique_lv = case_when(
                      unique_lv == ""          ~ "Aucun",
                      unique_lv == "0"         ~ other_level,
                      !is.na(unique_lv)        ~ ..2[replace(as.integer(unique_lv), 
                                                             which(. == "0"), 
                                                             1
                      )],
                      TRUE                     ~ ""
                    )) %>%
                    unite(var, sep = "") %>% deframe() %>%
                    str_replace_all("_+", "_") %>%
                    str_remove("_$") %>% str_remove(paste0(..3, "$"))
      )
    
    
    new_vars <- 
      reduce2(names(vars), new_levels, .init = new_vars, 
              ~ mutate(..1, !!..2 := fct_relabel(!!sym(..2), function(lv) ..3)
              ))
    
    replace_vars <- names(vars) %in% names(data) 
    if (any(replace_vars)) {
      warning(paste0("some variables have been replaced: ", 
                     paste0(names(vars)[replace_vars], collapse = ", ")))
      data <- select(data, -any_of(names(vars)))
    }
    
    bind_cols(data, new_vars)
  }
# data <- #filter(ctp, str_detect(PPP1_19, "Cadres")) %>%
#   mutate(ctp, across(starts_with("DPO2"),
#                 ~ fct_relabel(., ~ str_replace(., "Direction par objectifs", "DPO"))))
# 
#   data %>% 
#   longitudinal_cross(c("DPO2", "PPP1", "INCIDENT")) %>% 
#   tab(PPP1)
#   
#   data %>% 
#     longitudinal_cross(c("DPO2", "PPP1", "INCIDENT"), 
#                        sep = c("/", ">", "/"),
#                        n_min = c(3, 50, 3)) %>% 
#     tab(PPP1)





mds_per_class <- function(dissim, part) {
  classes <- sort(unique(part))
  #classes <- map(classes, ~ names(part)[part == .]) %>% set_names(classes)
  classes <- map(classes, ~ (1:length(part))[part == .]) %>% set_names(classes)
  
  if (is.null(dimnames(dissim))) {
    dissim <- `dimnames<-`(dissim, list(1:nrow(dissim), 1:ncol(dissim)))  
  }
  
  dissim_per_class <- map(classes, ~ dissim[., .])
  
  
  gc()
  mds.order.group <- 
    map(dissim_per_class, 
        ~ cmdscale(., k = 1)                                           # heavy calculation
        #bigmds::fast_mds(., r = 1, l = 2000, s_points = 2*1, dist_fn = as.dist)$points
    )
  #mds.order.group %>% map(~ dimnames(.)[[1]]) #%>% unlist()
  
  mds.order <- 
    map_if(mds.order.group, 
           map_lgl(mds.order.group, ~ dim(.)[2] == 0), 
           ~ matrix(0, dim(.)[1], 1, dimnames = list(dimnames(.)[[1]]))
    ) %>% 
    map(as.data.frame) 
  
  mds.order <- 
    reduce(mds.order, rbind) %>% rownames_to_column() %>% 
    mutate(rowname = as.integer(rowname)) %>% 
    arrange(rowname) %>% column_to_rownames() %>% 
    as.matrix()
  if (!length(mds.order) == nrow(dissim)) stop("nrow is wrong")
  mds.order
}




seq_select <- function(data, starts, na_levels = " ", add_ = FALSE) {
  if (missing(starts)) stop("no variable selector chosen")
  if (add_) {
    starts <- if_else(str_detect(starts, "_$"),
                      starts, 
                      paste0(starts, "_")      )
  }
  data %>% 
    select(starts_with(starts)) %>% 
    select(sort(colnames(.))) %>% 
    mutate(across(everything(), 
                  ~ fct_recode(., set_names(
                    levels(.)[which(str_detect(levels(.), "^Pas |^Non "))], 
                    "NULL"                                                  )) %>% 
                    fct_explicit_na(na_levels)
    ))
}

seq_dissim <- function(seqs, type = c("nbPCA", "dis", "disMCA", "OMconst"),
                       substit = 2, indel = 1.5
) {
  type <- type[1]
  gc(msg = FALSE)
  tictoc::tic()
  
  dissim <- seq_dissim_core(seqs = seqs, type = type, 
                            substit = substit, indel = indel)
  
  gc()
  
  elapsed <- tictoc::toc(quiet = TRUE)
  elapsed <- str_c("(", round(elapsed$toc - elapsed$tic, 1), " sec)")
  
  if (type != "OMconst") {
    message(paste0("Distance matrix ",
                   format(object.size(dissim), units = "MB"), 
                   elapsed)) 
  }
  
  dissim
}

seq_dissim_core <- function(seqs, type = c("nbPCA", "dis", "disMCA", "OMconst"),
                            substit = 2, indel = 1.5
) {
  # Matrice de distance à partir du nombre d'épisodes de chaque type (+ ACP)
  if (type == "nbPCA") {
    indics     <- seqhandbook::seqinepi(seqs)
    acp_coords <- 
      FactoMineR::PCA(indics, scale.unit=FALSE, ncp=5, graph=FALSE)$ind$coord
    return(Rfast::Dist(acp_coords, method='euclidean') %>% as.matrix() )
    
  } else if (type == "dis") {
    disjo <- GDAtools::dichotom(seqs)
    disjo <- disjo[,colSums(disjo)>0]
    
    disjo <- map_df(disjo, as.factor) %>%
      ade4::dudi.acm(scannf=FALSE, nf=ncol(disjo))
    return(ade4::dist.dudi(disjo) %>% as.matrix())
    
    # Matrice de distance par codage disjonctif complet (distance du chi2 + ACM)
    
  } else if (type == "disMCA") {
    disjo <- GDAtools::dichotom(seqs)
    disjo <- disjo[,colSums(disjo)>0]
    acm_res <- purrr::map_df(disjo, as.factor) %>% MCA(ncp=5, graph=FALSE)
    return(dissim  <- Rfast::Dist(acm_res$ind$coord, method='euclidean') %>%
             as.matrix())
    
  } else if (type == "OMconst") {
    couts  <- seqsubm(seqs, method="CONSTANT", cval=substit)
    return(seqdist(seqs, method="OM", sm=couts, indel=indel))
  }
}



tree <- function(agnes, type = c("tree", "heatmap"), branches = 20, seqs, 
                 main = "") {
  oldpar <- par() 
  switch(type[1],
         "tree"   = {
           par(mfrow=c(1,2))
           plot(as.dendrogram(agnes) , leaflab="none", main = main)   
           plot(sort(agnes$height, decreasing=TRUE)[1:branches], type="s")
         },
         
         # dendrogramme + index plot
         "heatmap" = seqhandbook::seq_heatmap(seqs, agnes) 
  )
  suppressWarnings(par(oldpar))
  invisible(agnes)
}




# #MDS : 
# # Matrice de distance à partir du nombre d'épisodes de chaque type (+ ACP)
# mds.order <- 
#   #cmdscale(dissim, k = 1) # memory crash
#   #bigmds::fast_mds(dissim, r = 1, l = 200, s_points = 2*1)$points # heavy calculation
#   bigmds::fast_mds(acp_coords, r = 1, l = 2000, s_points = 2*1,
#                    dist_fn = Rfast::Dist, method='euclidean'   )$points
# gc()
# 
# # Matrice de distance par codage disjonctif complet (avec ou sans ACM)
# gc()
# mds.order <- mds_per_class(dissim, part)


# Add : - Y axis : number of sequences in group (only for first sequence)
#       - X axis : only for the last group
#       - Group title  : only for the first group ? 
#       - Tri hiérarchique de la première à la dernière (+ repères dans ggplot2)
MSA_plot <- function(seqs, type = c("chrono", "index"), part.MSA, 
                     sort = "first", xtlab) {
  oldpar <- par()
  if (missing(part.MSA)) part.MSA <- factor(rep(1, length(seqs[[1]])))
  miss_lab <- missing(xtlab)
  
  switch(
    type[1], 
    "chrono" = {
      par(mfcol = c(nlevels(part.MSA) + 1, length(seqs)), 
          mar=c(2.1, 2.1, 2.1, 2.1))
      walk(seqs,
           function(.seq) {
             map(levels(part.MSA), 
                 ~ seqdplot(.seq[part.MSA == .,], 
                            xtlab = if(miss_lab) { names(.seq) } else { xtlab },
                            border = NA, with.legend = FALSE, main = .)
             )
             seqlegend(.seq, cex = 1.2)
           }
      )
    }, 
    
    
    "index" = {
      norm.from.start <- 
        switch(
          sort[1], 
          "first" = list(do.call(order, as.data.frame(seqs[[1]])[
            , 1:max(seqlength(seqs[[1]]))
          ])), 
          
          "each"  = map(seqs,
                        ~ do.call(order, 
                                  as.data.frame(.)[ , 1:max(seqlength(.))]) ), 
          
          list(do.call(order, as.data.frame(seqs[[sort[1]]])[
            , 1:max(seqlength(seqs[[sort[1]]]))
          ]))
        )
      
      par(mfcol = c(nlevels(part.MSA) + 1, length(seqs)), 
          mar = c(2.1, 2.1, 2.1, 2.1))
      walk2(seqs, norm.from.start, 
            function(.seq, .start) {
              map(levels(part.MSA), 
                  ~ seqIplot(
                    .seq[(1:length(part.MSA))[.start][part.MSA[.start] == .], ],
                    xtlab = if(miss_lab) { names(.seq) } else { xtlab },
                    border = NA, with.legend = FALSE, main = .
                  ))
              seqlegend(.seq, cex = 1.2)
            })
    }
  )
  
  suppressWarnings(par(oldpar))
  invisible(seqs)
}
# MSA_plot(trajexec, "chrono", part.MSA)
# MSA_plot(trajexec, "index" , part.MSA, sort = "first", xtlab = c(13, 16, 19))
# MSA_plot(trajexec, "index" , part.MSA, sort = "each" , xtlab = c(13, 16, 19))
# MSA_plot(trajexec, "index" , part.MSA, sort = "RWDEM", xtlab = c(13, 16, 19))


GIMSA_dim_choice <- function(dissims, mds) {
  oldpar <- par()
  
  if (! is.list(dissims)) {
    dissims <- list(dissims)
    if (! is.list(dissims)) mds <- list(mds)
  }
  
  pwalk(list(dissims, mds, names(dissims)), function(.dissims, .mds, .names) {
    par(mfrow= c(1,2))
    seqhandbook::seqmds.stress(.dissims, .mds) %>% # mesure de stress
      plot(type='l', xlab='nombre de facteurs', ylab='stress', main = .names)
    (.mds$eig[1:10]/.mds$eig[1]) %>%               # part de variance expliquée
      plot(type='s', xlab='nombre de facteurs', 
           ylab='part de variance expliquée', main = .names)   
  })
  
  suppressWarnings(par(oldpar))
}


















# Times series ----

time_series_graph <- list(
  geom_line(linewidth = 1, show.legend = FALSE),
  geom_point(size = 3, color = "white"), 
  geom_point(size = 2), 
  scale_color_viridis_d(begin = 0, end = 0.80, direction = -1)
)

agregate_sd <- function(mean, sd, wt) {
  sqrt(sum(sd^2 * wt + mean^2 * wt)/sum(wt) - weighted.mean(mean, wt)^2)
}

# data <- serie80_20_CORPS_GRADE$CORPS_CONT_2 |>
#   filter(!is.na(CORPS1)) |>
#   group_by(ANNEE, CORPS1) |>
#   summarise(SM_NETR_sd = sum(SM_NETR_sd^2 * n + SM_NETR^2 * n)/sum(n), 
#             SM_NETR    = weighted.mean(SM_NETR, n), 
#             n          = sum(n), 
#             
#             .groups = "drop") |>
#   mutate(SM_NETR_sd = sqrt(SM_NETR_sd - SM_NETR^2))
# 
# serie80_20_CORPS_GRADE$CORPS_CONT_2 |> 
#   select(ANNEE, CORPS_CONT_2, SM_NETR_sd,  SM_NETR, n) |>
#   filter(str_detect(CORPS_CONT_2, "^A2") & ANNEE == 1980) |> 
#   print(n = 30)
# 
# time <- expr(ANNEE)
# vars <- expr(c(n, SM_NETR))
# group <- expr(CORPS1)
# evol_from = 1988
# digits = 0L
# sd_vars = c(NA_character_, "SM_NETR_sd")

# data <- serie80_20_CORPS_GRADE$CORPS_CONT |>
#   filter(!is.na(type) & ANNEE >= 1988 & FAPPP2 == "F1-Finances publiques")
# time <- expr(ANNEE)
# vars <- expr(c(n, SM_NETR))
# group <- expr(CORPS_CONT)
# evol_from = 1988
# digits = 0L



time_series_panel <- function(data, time, vars, group, evol_from, digits = 0L, 
                              sd_vars) {
  time <- ensym(time)
  group <- ensym(group)
  
  vars <- syms(names(tidyselect::eval_select(enquo(vars), data)))
  sd_vars <- if (!missing(sd_vars)) {
    vctrs::vec_recycle(sd_vars, length(vars)) |> replace_na("") #|> set_names(vars)
  } else {
    rep("", length(vars))
  } 
  
  data <- select(data, !!group, !!time, !!!vars, all_of(sd_vars[sd_vars != ""]))
  
  if (any(sd_vars != "")) {
    data_sd <- data |> 
      select(-c(!!!vars)) |> 
      pivot_longer(cols = all_of(sd_vars[sd_vars != ""]), #c(!!!vars) | all_of(sd_vars[sd_vars != ""]), 
                   names_to = "variable", values_to = "sd") |>
      mutate(variable = fct_recode(variable, !!!set_names(sd_vars[sd_vars != ""], vars[sd_vars != ""])) |>
               fct_relevel(as.character(vars[sd_vars != ""])))
    
  }
  
  data <- data |> 
    select(-all_of(sd_vars[sd_vars != ""])) |> 
    pivot_longer(cols = c(!!!vars),
                 names_to = "variable", values_to = "value") |>
    mutate(variable = as.factor(variable) |> fct_relevel(as.character(vars)))
  
  if (any(sd_vars != "")) {
    data <- data |> left_join(data_sd, by = c(as_label(group), as_label(time), "variable"))
  }
  
  
  if (!missing(evol_from)) {
    data <- data |> 
      group_by(!!group, variable) |>
      mutate(value_evol = 
               if (any(!!time == evol_from)) {value / value[!!time == evol_from] * 100} 
             else {NA_real_}
      ) |>
      ungroup()
    
    data <- data |> 
      pivot_longer(cols = c(value, value_evol), 
                   names_to = "evol", values_to = "value") |>
      mutate(evol = fct_recode(as.factor(evol), "base" = "value", "evol" = "value_evol"))
    
    if (any(sd_vars != "")) {
      data <- data |> mutate(sd = if_else(evol == "base" & !is.na(sd), sd, 0))
      sd_ribbon <- list(
        ggiraph::geom_ribbon_interactive(
          aes(x = !!time, group = !!group, 
              ymax = value + sd,
              ymin = pmax(value - sd, 0), data_id = group_id),
          # hover_css = "stroke:none;fill:orange;opacity:0.5;"
          color = NA, fill = NA,
          show.legend = FALSE, na.rm = TRUE, inherit.aes = FALSE), 
          ggiraph::geom_ribbon_interactive( # to neutralise hover effect of the area
            aes(x = !!time, group = !!group,  
                ymax = value + sd,
                ymin = pmax(value - sd, 0)),
            hover_css = "",
            color = NA, fill = NA,
            show.legend = FALSE, na.rm = TRUE, inherit.aes = FALSE)
      )
      
    } else {
      data <- data |> mutate(sd = 0) 
      sd_ribbon <- NULL
      }
    
  } 
  
  
  if (!missing(evol_from)) {
    data <- data |>
      group_by(!!group) |> 
      mutate(group_id = cur_group_id() ) |>
      ungroup() |>
      mutate(current_tooltips = paste0(if_else(evol == "evol", 
                                               true  = paste0("evol_", variable), 
                                               false = variable), 
                                       ": ", 
                                       if_else(sd != 0, 
                                               true  = paste0(round(value, digits), " ±", round(sd, digits)), 
                                               false = as.character(round(value, digits))),
                                       "\n")
      ) |>
      group_by(!!group, !!time) |>
      #mutate(point_id = cur_group_id() ) |>
      mutate(other_tooltips = list(current_tooltips), 
             other_tooltips = map2_chr(other_tooltips, current_tooltips, 
                                       ~ paste0(discard(.x, .x == .y), collapse = ""))
      ) |>
      ungroup() |>
      #mutate(point_id = point_id + max(group_id, na.rm = TRUE)) |> 
      mutate(tooltips = paste0(!!group, " ", !!time, "\n",
                               current_tooltips, other_tooltips), 
             evol_hline = if_else(evol == "evol", 100L, NA_integer_)) |> 
      select(-any_of(c("current_tooltips", "other_tooltips")))
    
    # data$tooltips[[1]]
    
    graph <- data |>
      ggplot(aes(x = !!time, y = value, group = !!group,
                 tooltip = tooltips, data_id = group_id)) + 
      sd_ribbon +
      ggiraph::geom_line_interactive(aes(color = !!group), linewidth = 0.5) + 
      geom_point(size = 1.5, color = "white") + 
      ggiraph::geom_point_interactive(aes(color = !!group), size = 1, shape = 19) + # aes(data_id = point_id)
      geom_hline(aes(yintercept = evol_hline), na.rm = TRUE) + 
      scale_color_viridis_d(begin = 0, end = 0.90, direction = -1) + # aesthetics = c("color", "fill"),
      facet_wrap(c("evol", "variable"), nrow = 2L, scales = "free_y", 
                 labeller = labeller(
                   .multi_line = FALSE
                 )) + 
      labs(y = "") + 
      theme(legend.position = "bottom") 
    
  } else {
    data <- data |>
      group_by(!!group) |> 
      mutate(group_id = cur_group_id() ) |>
      ungroup() |>
      mutate(current_tooltips = paste0(variable, ": ", 
                                       if_else(sd != 0, 
                                               true  = paste0(round(value, digits), " ±", round(sd, digits)), 
                                               false = as.character(round(value, digits))),
                                       "\n") ) |> 
      group_by(!!group, !!time) |>
      #mutate(point_id = cur_group_id() ) |>
      mutate(other_tooltips = list(current_tooltips), 
             other_tooltips = map2_chr(other_tooltips, current_tooltips, 
                                       ~ paste0(discard(.x, .x == .y), collapse = ""))
      ) |> 
      ungroup() |>
      mutate(tooltips = paste0(!!group, "\n", 
                               as_label(time), ": ", !!time, "\n",
                               current_tooltips, other_tooltips) 
      ) |>
      select(-any_of(c("current_tooltips", "other_tooltips")))
    
    graph <-  ggplot(data, aes(x = !!time, y = value, color = !!group, 
                               tooltip = !!group, data_id = group_id)) + 
      sd_ribbon +
      ggiraph::geom_line_interactive(linewidth = 0.5) + 
      geom_point(size = 1.5, color = "white") + 
      ggiraph::geom_point_interactive(size = 1, shape = 19) + # aes(data_id = point_id)
      geom_hline(aes(yintercept = evol_hline), na.rm = TRUE) + 
      scale_color_viridis_d(begin = 0, end = 0.90, direction = -1) + 
      facet_wrap(c("variable"), scales = "free_y", 
                 labeller = labeller(
                   .multi_line = FALSE
                 )) + 
      labs(y = "") + 
      theme(legend.position = "bottom")
  }
  
  suppressWarnings(ggiraph::girafe(ggobj = graph, pointsize = 1) |>
                     ggiraph::girafe_options(
                       ggiraph::opts_hover(css = ggiraph::girafe_css(
                         css   = "", #"fill:none;opacity:0;", #"stroke:none;fill:orange;opacity:0;", 
                         line  = "stroke-width:5px;", 
                         area  = "stroke:none;fill:orange;opacity:0.3;", 
                         point = "stroke:none;fill:black"
                       ) 
                       )
                     )
  )
  
}


time_series_panel_safely <- purrr::safely(time_series_panel, quiet = FALSE)














# Event History Analysis -----

# Règles du calcul d'ancienneté 
# 1) On compte l'ancienneté pour les postes d'affilée, 
#  ou avec éventuellement un trou d'une année (maximum)
# 2) Deux possibilités d'usage : 
# - Variable codée niveau individu*annee : dans la catégorie actuelle (année n)
# - Usage niveau individu : dans la catégorie de 2019 ; prendre variable 2019
# (Ce n'est pas le nombre de lignes mais le nombre d'années qu'on compte.)

# ID    <- pan$NNINOUV
# ANNEE <- pan$ANNEE
# var   <- pan$N

same_since_groups <- function(ID, ANNEE, var) {
  same_ind <- ID == dplyr::lag(ID, 1L, default = "N00")
  
  same1 <- same_ind & 
    var == dplyr::lag(var, 1L) & 
    (ANNEE == dplyr::lag(ANNEE, 1L) + 1L | ANNEE == dplyr::lag(ANNEE, 1L) + 2L)
  
  same2 <- same_ind & (!same1 | is.na(lag(var, 1L))) & 
    var == dplyr::lag(var, 2L) &
    ANNEE == dplyr::lag(ANNEE, 2L) + 2L
  same2 <-  replace_na(same2, FALSE) 
  
  same <- if_else(
    same2 | (lead(same2, 1L) & ID == dplyr::lead(ID, 1L, default = "N00")), 
    true  = TRUE,
    false = same1) 
  same <- replace_na(same, FALSE) 
  
  group_nb <- cumsum(if_else(same, 0L, 1L))
  
  group_nb
}



# Nombre d'année dans chaque catégorie
nb_years_in_cat <- function(ID, ANNEE, var) {
  dt_nb_years <- data.table(ID = ID, ANNEE = ANNEE, var = var)
  dt_nb_years[, rn := 1:.N]
  setorder(dt_nb_years, ID, var, ANNEE)
  
  dt_nb_years[, same_ind_and_cat := (ID == dplyr::lag(ID, 1L, default = "N00") &
                                       var == dplyr::lag(var, 1L)) |> replace_na(FALSE)
  ]
  
  dt_nb_years[, same_ind_and_cat := (ID == dplyr::lag(ID, 1L, default = "N00") &
                                       var == dplyr::lag(var, 1L)) |> replace_na(FALSE)
  ]
  
  dt_nb_years[, group_nb := cumsum(if_else(same_ind_and_cat, 0L, 1L)) ]
  
  dt_nb_years[, nb_years := (1L:.N), by = "group_nb"] # -1L
  
  setorder(dt_nb_years, rn)
  
  dt_nb_years$nb_years
}



# var   <- pan$Nbis
# ID    <- pan$NNINOUV
# ANNEE <- pan$ANNEE
# new_ind <- pan$new_ind
# max_dist = 2L

#   On ne prend pas l'ancienneté en nombre d'années, mais en temps écoulé, 
#    sinon quand on saute une année on classe mal la personne. 
#    Il faut donc compter, pour chaque ligne, la distance par rapport à la 
#    ligne précédente (si une catégorie est suivie par la même avec un trou 
#    de plusieurs années, on ne les compte que si c'est au plus X années).
nb_years_in_cat_fill_holes <- function(ID, ANNEE, var, new_ind, max_dist = 2L) {
  dt_nb_years <- data.table(ID = ID, ANNEE = ANNEE, var = var, new_ind = new_ind)
  dt_nb_years[, rn := 1:.N]
  
  setorder(dt_nb_years, ID, ANNEE)
  dt_nb_years[, dist := lead(ANNEE, 1L, default = 1L) - ANNEE, ]
  # Si la catégorie suivante n'est pas la même, on ne compte qu'une année. 
  # On ne compte que les trous inférieurs ou égaux à X années (les autres ont 
  #  plus de chances d'être des "vrais" trous).
  dt_nb_years[
    ,
    dist := if_else(replace_na(var != dplyr::lead(var, 1L) | dist > max_dist, TRUE),
                    true  = 1L, 
                    false = dist)
  ]
  dt_nb_years[, dist := if_else(lead(new_ind, default = FALSE), 1L, dist)]
  # dt_nb_years[, dist := pmin(dist, 2L)]
  
  # 2.2 sec
  dt_nb_years[, nb_years := cumsum(dist), by = c("ID", "var")] # -1L
  
  # # 3.6 secs
  # setorder(dt_nb_years, ID, var, ANNEE)
  # dt_nb_years[, same_ind_and_cat := (!new_ind & var == dplyr::lag(var, 1L)) |> 
  #               replace_na(FALSE)
  # ]
  # dt_nb_years[, group_nb := cumsum(if_else(same_ind_and_cat, 0L, 1L)) ]
  # 
  # dt_nb_years[, nb_years := sum(dist), by = "group_nb"] # -1L
  
  
  
  
  dt_nb_years <- select(dt_nb_years, nb_years, rn)
  setorder(dt_nb_years, rn)
  
  dt_nb_years$nb_years
}


# var     <- pan_samp$CORPS_plus
# var_NB  <- pan_samp$CORPS_plus_NB
# new_ind <- pan_samp$new_ind


# Nombre d'année dans chaque catégorie : pour des catégories précises, 
#  agréger l'information pour toutes les années (une variable par catégorie, 
#  avec si l'individu est dans une autre catégorie l'année N l'information
#  de l'année précédente)
#  - Ne fonctionne qu'avec toutes les lignes pour un individu
#  - La base de données doit être triée par setorder(ID, ANNEE) 
#  - new_ind : TRUE quand il s'agit d'un nouvel individu
#  - Si var_NB (résultat de nb_years_in_cat() ) et cat sont donnés,
#    on prépare la variable en mettant NA à toute ce qui n'est pas 
#    la catégorie choisie
#  - Si seule var est donnée on suppose qu'elle a été préparée
#  - Fonctionne pour integer(), double() et factor()
nby_comp <- function(var, var_NB, cat, new_ind, lag_in_cat = FALSE, 
                     zero_to_na = TRUE, type = "locf") { # type = "locf"
  stopifnot(is.logical(new_ind))
  
  
  if(!missing(var_NB) | !missing(cat)) {
    stopifnot(is.numeric(var_NB))
    data <- data.table(var = var, var_NB = var_NB, cat = cat, new_ind = new_ind)
    
    #var_X_nb <- if_else(var == cat, var_NB, NA_integer_)
    data[, var_in_cat := var == cat]
    data[, var := if_else(var_in_cat, var_NB, NA_integer_)]
    
  } else {
    data <- data.table(var = var, new_ind = new_ind)
  }
  
  # var_X_nb <- pan_samp$ADMIN_NB1_CS1
  
  var_is_factor <- is.factor(data$var) #is.factor(var_X_nb)
  if (var_is_factor) {
    restore_levels <- set_names(as.character(seq_along(levels(data$var))),
                                levels(data$var))
    # restore_levels <- c(restore_levels, "NULL" = "0")
    data[, var := as.integer(var)] # var_X_nb <- as.integer(var_X_nb)
  }
  
  var_is_logical <- is.logical(data$var)
  if (var_is_logical) data[, var := as.integer(var)]
  
  if (type[1] == "locf") {
    
    if (is.integer(data$var)) {
      data[, var := dplyr::if_else(is.na(var) & new_ind, 0L, var)]
      # var_X_nb <- dplyr::if_else(is.na(var_X_nb) & new_ind, 0L, var_X_nb)
      
    } else if (is.double(data$var)) {
      data[, var := dplyr::if_else(is.na(var) & new_ind, 0, var)]
      #var_X_nb <- dplyr::if_else(is.na(var_X_nb) & new_ind, 0, var_X_nb)
    } 
    
  } else if (type[1] == "nocb") {
    if (is.integer(data$var)) {
      data[, var := dplyr::if_else(is.na(var) & lead(new_ind), 0L, var)]
      # var_X_nb <- dplyr::if_else(is.na(var_X_nb) & new_ind, 0L, var_X_nb)
      
    } else if (is.double(data$var)) {
      data[, var := dplyr::if_else(is.na(var) & lead(new_ind), 0, var)]
      #var_X_nb <- dplyr::if_else(is.na(var_X_nb) & new_ind, 0, var_X_nb)
    } 
    
  }
  setnafill(data, type = type, cols = "var")
  
  
  if (zero_to_na & !var_is_factor & !lag_in_cat) {
    if (is.integer(var)) {
      data[, var := na_if(var, 0L)]
    } else{
      data[, var := na_if(var, 0)]
    }
  }
  
  
  if (var_is_factor) {
    data[, var := as.factor(dplyr::if_else(var == 0L, NA_integer_, var))]
    data[, var := fct_recode(var, !!!restore_levels[restore_levels %in% levels(var)])]
    
    # var_X_nb <- dplyr::if_else(var_X_nb == 0L, NA_integer_, var_X_nb)
    # var_X_nb <- var_X_nb |> as.factor()
    # var_X_nb <- var_X_nb |>
    #   fct_recode(!!!restore_levels[restore_levels %in% levels(var_X_nb)])
    
  } else if (var_is_logical) {
    data[, var := as.logical(var)]
    
  } else if (lag_in_cat & !missing(cat)) {
    data[, var := case_when(
      new_ind    ~ 0L, 
      var_in_cat ~ lag(var, 1L), 
      TRUE       ~ var
    )]
  }
  
  data$var
  
  
  # # Visualiser ce qu'on fait :
  #  tibble(NNINOUV = pan_samp$NNINOUV,
  #        #EMP_CHAMP = pan_samp$EMP_CHAMP,
  #        #ANNEE = pan_samp$ANNEE,
  #        #EMP_CHAMP_NB = pan_samp$EMP_CHAMP_NB,
  #        new_ind = pan_samp$new_ind,
  #        nb_fact = var_X_nb,
  #        nb = as.integer(var_X_nb),
  #        #nb = if_else(pan_samp$EMP_CHAMP == "PRIV", pan_samp$EMP_CHAMP_NB, NA_integer_),
  #        #nb2 = if_else(new_ind & is.na(nb), 0L, nb),
  #        #locf = nafill(nb2, "locf"),
  # ) |> new_tab() |> group_by(NNINOUV) |> print(n = 200)
  
  
  # Le but c'est d'obtenir le bon vecteur sans grouper (trop long
  # sur grandes bases de données). 
  
}


# Memory crash : do it one by one with the former function
nby_comp_all <- function(var, var_NB, cat = "all", new_ind) {
  stopifnot(is.logical(new_ind))
  
  if (cat[1] == "all") {
    cat <- var |> unique() |> sort() |> as.character()
  }
  
  # If this is a factor alone
  if (missing(var_NB)) {
    var_X_nb <- var
    
    if (is.factor(var)) {
      restore_levels <- set_names(as.character(seq_along(levels(var_X_nb))), 
                                  levels(var_X_nb))
      # restore_levels <- c(restore_levels, "NULL" = "0")
      var_X_nb <- as.integer(var_X_nb)
      
      var_X_nb <- dplyr::if_else(is.na(var_X_nb) & new_ind, 
                                 true  = 0L, 
                                 false = var_X_nb
      )
      
      var_X_nb <- nafill(var_X_nb, "locf")
      
      var_X_nb <- dplyr::if_else(var_X_nb == 0L, NA_integer_, var_X_nb)
      var_X_nb <- var_X_nb |> as.factor()  
      var_X_nb <- var_X_nb |> 
        fct_recode(!!!restore_levels[restore_levels %in% levels(var_X_nb)])
      
      return(var_X_nb) 
    }
  }
  
  # If this is a numeric vector of counts, for the given categories.
  stopifnot(is.numeric(var_NB))
  
  var_X_nb <- map(cat, ~ if_else(var == ., var_NB, NA_integer_)) |>
    set_names(cat) |> as.data.table()
  
  var_X_nb <- cbind(var_X_nb, new_ind)
  var_X_nb[, eval(cat) := map(.SD, 
                              ~ dplyr::if_else(is.na(.) & new_ind, 0L, .)
  ), 
  .SDcols = cat]
  
  var_X_nb[, new_ind := NULL]
  setnafill(var_X_nb, "locf")
  
  var_X_nb
  
  # # Visualiser ce qu'on fait :
  # tibble(NNINOUV = pan_samp$NNINOUV,
  #        #EMP_CHAMP = pan_samp$EMP_CHAMP,
  #        #ANNEE = pan_samp$ANNEE,
  #        #EMP_CHAMP_NB = pan_samp$EMP_CHAMP_NB,
  #        new_ind = pan_samp$new_ind,
  #        nb_fact = var_X_nb,
  #        nb = as.integer(var_X_nb),
  #        #nb = if_else(pan_samp$EMP_CHAMP == "PRIV", pan_samp$EMP_CHAMP_NB, NA_integer_),
  #        #nb2 = if_else(new_ind & is.na(nb), 0L, nb),
  #        #locf = nafill(nb2, "locf"),
  # ) |> new_tab() |> group_by(NNINOUV) |> print(n = 200)
  
  
}


# Compléter les NA pour un même individu et une même catégorie "by_var"
#  (si completer par comp_by ne fonctionne pas parce qu'un même corps doit
#  avoir la même information partout mais qu'on peut être passé par d'autres
#  corps entre)
comp_by <- function(var, by_var, ID, ANNEE, zero_to_na = TRUE) {
  data <- data.table(var = var, by_var = by_var, ID = ID, ANNEE = ANNEE)
  data[, rn := 1:.N]
  setorder(data, ID, by_var, ANNEE)
  
  data[, new_ind_by := ID != lag(ID, 1L, default = "N00") | 
         by_var != lag(as.character(by_var), 1L, default = "N00")]
  
  var_is_factor <- is.factor(var)
  if (var_is_factor) {
    restore_levels <- set_names(as.character(seq_along(levels(var))),
                                levels(var))
    # restore_levels <- c(restore_levels, "NULL" = "0")
    data[, var := as.integer(var)]   #var <- as.integer(var)
  }
  
  if (is.integer(data$var)) {
    data[, var := dplyr::if_else(is.na(var) & new_ind_by, 0L, var)]
    
  } else {
    data[, var := dplyr::if_else(is.na(var) & new_ind_by, 0, var)]
  }
  
  data[, `:=`(ID = NULL, ANNEE = NULL, new_ind_by = NULL)]
  setnafill(data, "locf", cols = "var")
  setorder(data, rn)
  
  if(zero_to_na & !var_is_factor) {
    if (is.integer(var)) {
      data[, var := na_if(var, 0L)]
    } else{
      data[, var := na_if(var, 0)]
    }
  }
  
  if (var_is_factor) {
    data[, var := as.factor(dplyr::if_else(var == 0L, NA_integer_, var))]
    data[, var := fct_recode(var, !!!restore_levels[restore_levels %in% levels(var)])]
  }
  
  data$var
}


# var_X_nb <- pan_samp$ADMIN_NB1_CS1





# # Ancienne version, pour les variables non numériques (par groupes = long)
# data <- pan_anciennete
# vars_and_category_list <- list("CS1_NB" = c("3", "4"),
#                               "STATUT_NB" = "N-Non titulaire")
# ID <- "NNINOUV"
# ANNEE <- "ANNEE"
# 
# 
# nb_years_in_cat_factor <- function(data, vars_and_category_list,
#                                      ID, ANNEE, already_ordered = FALSE,
#                                      fill_NA_down = TRUE) {
# 
#   vars_and_category_list <- vars_and_category_list |>
#     imap(~ set_names(.x, .y)) |>
#     flatten_chr()
# 
#   data_base <- data
# 
#   data <- data |>
#     select(any_of(c(ID, ANNEE,
#                     str_remove(unique(names(vars_and_category_list)), "_NB$"),
#                     unique(names(vars_and_category_list))))
#     )
# 
#   if (!already_ordered) {
#     data <- data |> as.data.table()
#     data[, rn := 1:.N]
#   setorderv(data, c(ID, ANNEE))
#   }
# 
#   data <-
#     reduce2(vars_and_category_list,
#             str_remove(names(vars_and_category_list), "_NB$"),
#             .init = as_tibble(data),
#             .f = ~ mutate(..1, !!sym(paste0(..3, "_", ..2, "_nb")) :=
#                             if_else(!!sym(..3) == ..2,
#                                     true  = !!sym(paste0(..3, "_NB")),
#                                     false = NA_integer_)
#             ))
# 
#   new_vars <- paste0(str_remove(names(vars_and_category_list), "_NB$"),
#                      "_", vars_and_category_list, "_nb")
# 
#   if (fill_NA_down) {
#   data <-  data |>
#     group_by(!!sym(ID)) |>
#     fill(all_of(new_vars), .direction = "down") |>
#     ungroup() |>
#     mutate(across(all_of(new_vars), ~ replace_na(., 0L) ))
#   }
# 
#   data <- data |> select(any_of(c("rn")), all_of(new_vars))
# 
#   if (!already_ordered) {
#     data <- data |> as.data.table()
#     setorder(data, rn)
#     data[, rn := NULL]
#   }
# 
#   data_base <- data_base |> bind_cols(data)
# }

# pan_anciennete |>
#   nb_years_in_cat_complete(
#     ID =  "NNINOUV", ANNEE = "ANNEE",
#     vars_and_category_list = list("CS1_NB" = c("3", "4"),
#                                   "STATUT_NB" = "N-Non titulaire")
#   ) |>
#   select(NNINOUV, ANNEE, starts_with(c("CS1", "STATUT"))) |>
#   filter(NNINOUV %in% c(NNIsamp)) |>
#   new_tab() |> group_by(NNINOUV) |>
#   print(n = 200)


# Tests, pour version sans grouper 
# pan_anciennete |> tibble::as_tibble() |>
#   dplyr::select(NNINOUV, ANNEE, N, N_A_nb, N_B_nb, N_C_nb) |>
#   dplyr::filter(NNINOUV %in% c(NNIsamp, "N200200527330")) |>
#   tabxplor::new_tab() |> dplyr::group_by(NNINOUV) |>
#   print(n = 200)
# # NNIsamp <- pan_anciennete |> dplyr::select(NNINOUV, EMP_CHAMP) |> 
# #   dplyr::filter(EMP_CHAMP %in% c("ETAT", "FPH", "FPT")) |> 
# #   dplyr::pull(NNINOUV) |> sample(10) |> as.character()
# 
# 
# # => MARCHE PAS DU TOUT. FAIRE UN TEST SUR UN PETIT NOMBRE.
# NNIsamp <-pan_anciennete |>
#   select(NNINOUV, EMP_CHAMP) |>
#   dplyr::filter(EMP_CHAMP %in% c("ETAT", "FPH", "FPT")) |> 
#   dplyr::pull(NNINOUV) |> sample(10) |> as.character()
# #mutate(test = cumany(NNINOUV ==  as.character(sample(NNINOUV, 1)))) |>
# # filter(test) |> slice(1:500) |> pull(NNINOUV)
# pan_samp <- pan_anciennete |> select(NNINOUV, ANNEE, N) |> filter(NNINOUV %in% NNIsamp)
# 
# 
# pan_samp[, N_NB := nb_years_in_cat(NNINOUV, ANNEE, var = N)]
# pan_samp[, N_A_nb := if_else(N == "A", N_NB, NA_integer_)]
# pan_samp[, N_B_nb := if_else(N == "B", N_NB, NA_integer_)]
# pan_samp[, N_C_nb := if_else(N == "C", N_NB, NA_integer_)]
# 
# # var_X_nb <- pan_samp$N_A_nb         # var <- pan_samp$N ; var_NB <- pan_samp$N_NB ; cat <- "A"
# 
# 
# pan_samp[, N_A_nb := nb_years_in_cat_complete(N_A_nb)]
# 
# 
# 
# # pan_samp_to_fill <- pan_samp[, c("N_A_nb", "N_B_nb", "N_C_nb")] 
# # pan_samp_na_locf <- data.table::nafill(pan_samp_to_fill, type = "locf")
# # pan_samp_na_nocb <- data.table::nafill(pan_samp_to_fill, type = "nocb")
# # 
# # names(pan_samp_na_locf) <- paste0(names(pan_samp_to_fill), "_locf")
# # names(pan_samp_na_nocb) <- paste0(names(pan_samp_to_fill), "_nocb")
# # 
# # pan_samp_to_fill <- dplyr::bind_cols(pan_samp_to_fill, pan_samp_na_locf, pan_samp_na_nocb)
# # pan_samp_to_fill <- pan_samp_to_fill |> select(sort(names(pan_samp_to_fill)))
# # 
# # pan_samp_to_fill[, N_A_fin := dplyr::if_else(is.na(N_A_nb) & N_A_nb_nocb == 0L, 0L, N_A_nb_locf)]
# # pan_samp_to_fill[, N_B_fin := dplyr::if_else(is.na(N_B_nb) & N_B_nb_nocb == 0L, 0L, N_B_nb_locf)]
# # pan_samp_to_fill[, N_C_fin := dplyr::if_else(is.na(N_C_nb) & N_C_nb_nocb == 0L, 0L, N_C_nb_locf)]
# #
# #pan_samp <- pan_samp |> 
# #  select(-any_of(c("N_A_nb", "N_B_nb", "N_C_nb"))) |>
# #  bind_cols(pan_samp_to_fill)
# 
# pan_samp |> tibble::as_tibble() |>
#   dplyr::select(NNINOUV, ANNEE, N, starts_with("N_A_nb"), any_of(c("N_A_fin")) ) |>
#   tabxplor::new_tab() |> dplyr::group_by(NNINOUV) |>
#   print(n = 200)



# data <- pan_cadres
# event_var = "event_A2"
# time_var  = "AGE20"
# id = "NNINOUV"
# new_ind = "new_ind"
# constant_vars = c("SEXE", "GEN6", "GEN3") # "ANNAI" # "DIP5", "DEPNAI", "CS_MERE", "CS_PERE", "AFER" 
# time_varying_vars = c(
#   "ETAT_NB1_AGE20", "ETAT_NB1_EMP_MIN12", "ETAT_NB1_CJ_SER", # "ETAT_NB1_CORPS_CONT"
#   "ETAT_TIT_NB1_AGE20", "TIT_TIME", # "ETAT_TIT_NB1_CORPS" 
#   "ETAT_TIT_NB1_EMP_MIN12", "ETAT_TIT_NB1_CJ_SER", # SERRR, CJ
#   "ETAT_TIT_NB1_REGT", "CJ_SER", 
#   
#   "EMP_MIN12", "FAPPP", "FAPPP2", 
#   
#   "CORPS_plus_deb_A3", 
#   
#   # => starts at ETAT_TIT_NB1 and constant_vars ?
#   "ec_ENA", "ec_X", "ec_ENM", "ec_IRA", "ec_INGE_A", "ec_ENS",  
#   
#   "CORPS_plus_3" #, #"CORPS_CONT_3"
# )
# 
# count_vars = c(
#   "CONTRAT_E_CDD", "EF_nb", # "CONTRAT_E_FON", "CONTRAT_E_CDI", "CONTRAT_E_IND",
#   "Nbis_A_nb", "Nbis_B_nb", "Nbis_C_nb", "Nbis_P_nb", 
#   "Nbis_Aplus_nb", # nombre d'année sur des emplois A+ avant d'entrer dans un corps A+
#   "mob_DEPT", "mob_REGT", "mob_CORPS")
# already_ordered = TRUE
# time_max = 65 - 20
# n_min = 5L
# vif = TRUE
# zph = TRUE
# linearity_plot = TRUE
# print_plots = FALSE

# data <- pan_cadres[ENTPAN >= 1980 & ENTPAN != 2002, ] 


eha_mf <- function(data, event_var, time_var, time_max = NULL, id, 
                   start_event = NULL, start_vars = NULL, time_from_start_event = FALSE,
                   constant_vars = NULL, 
                   time_varying_vars = NULL, 
                   count_vars = NULL, 
                   out_of_range_condition,
                   new_ind = NULL, already_ordered = FALSE #, 
) {
  out_of_range <- !missing(out_of_range_condition)
  if (out_of_range) {
    out_of_range_condition_vars <- formula.tools::get.vars(enexpr(out_of_range_condition))
    out_of_range_condition <- enquo(out_of_range_condition)
    
  } else {
    out_of_range_condition <- NULL
    out_of_range_condition_vars <- NULL
  }
  
  data <- data |>
    select(all_of(c(id = id, time = time_var, event = event_var, start_event = start_event, 
                    new_ind = new_ind, start_vars, 
                    constant_vars, time_varying_vars, count_vars,
                    out_of_range_condition_vars))) |>
    as.data.table()
  
  if (!already_ordered) setorder(data, id, time)
  if (length(new_ind) == 0) {
    data[, new_ind := id != lag(id, 1L, default = ".N00.")]
  }
  
  if (length(start_event) > 0) {
    
    data[, start_event_and_after := cumany(start_event), by = "id"]
    data <- data[start_event_and_after == TRUE, ]
    data[, start_event_and_after := NULL]
    
    if (time_from_start_event) start_vars <- unique(c(start_vars, "time"))
    
    if (length(start_vars) > 0) {
      if_else_na <- map(select(data, all_of(start_vars)), 
                        ~ if (is.factor(.)) {
                          factor(NA_character_)
                        } else if (is.character(.)) {
                          NA_character_
                        } else if (is.integer(.) ) {
                          NA_integer_
                        } else if (is.double(.) ) {
                          NA_real_
                        } else if (is.logical(.) ) {
                          NA
                        }
      )
      
      #i <- 2
      start_names <- paste0("start_", start_vars)
      for (i in 1:length(start_vars) ) {
        START_VAR  <- str2expression(start_vars[i])
        START_NAME <- str2expression(start_names[i])
        
        data[, eval(start_names[i]) := if_else(start_event, eval(START_VAR), if_else_na[[i]])]
        data[, eval(start_names[i]) := nby_comp(eval(START_NAME), new_ind = new_ind, zero_to_na = FALSE)]
      }
      # data |> select(id, time, start_event, start_vars) |> as_tibble()
      
      if (time_from_start_event) {
        data[, time := time - start_time]
        #data[, start_time := NULL]
      }
      
    } else {
      start_names <- NULL
    }
    
  } else {
    start_names <- NULL 
  }
  start_names <- start_names[start_names != "start_time"]
  
  data[, event_and_before := (event | !cumany(event)) & !is.na(time) & time >= 0, by = "id"]
  data <- data[event_and_before == TRUE]
  data[, event_and_before := NULL]
  
  if (length(time_max) > 0 ) data <- data[time <= time_max, ]
  
  
  
  
  
  
  # data[, time2 := if_else(!lead(new_ind), lead(time, 1), NA_integer_)]
  # data[, time2 := time + 1L]
  # data[, stop := if_else(!lead(new_ind), lead(time, 1), NA_integer_)]
  
  
  # Que faire des NA à CORPS1, age et ministère de titularisation, etc. ? 
  
  
  
  
  # Regrouper périodes contigues avec mêmes informations
  # (pour les variables qui comptent les années dans une catégorie, on ne 
  #  prend que la dernière si les autres variables sont constantes)
  info_vars <- c("event", constant_vars, time_varying_vars, start_names)
  
  constant_on_period <- c("id", "event", constant_vars, time_varying_vars, start_names)
  
  # Faut-il privilégier ? : 
  # - les changements de statuts (persistent années d'après)  ou 
  # - les événements (juste l'année n, enregistré en n-1 puisqu'il arrive 
  #  à la fin de l'intervalle précédent) ?
  
  
  
  ### La logique de la mise en forme des données d'eha :
  # - La mesure doit se faire sur "start" (début de new_period, 
  # last known measure), on ne prends jamais la dernière mesure de la période
  # - Tous les changements d'une année donnée sont enregistrés 
  #    au début de l'année suivante.
  # - Événements arrivant entre deux points pas comptés 
  # (s'ils changent un état c'est celui de la période qui suit)
  # - L'intervalle est (15, 20] : pour 20 on prend la valeur n-1
  # - L'évenement arrive au temps stop de fin de dernière période
  # - On ne doit pas craindre les correlations entre les lignes : à chaque 
  #    temps, l'algorithme ne prend que les individus présents
  #    (sauf événements multiples).
  data <- copy(data) |> 
    mutate(new_period = if_any(all_of(constant_on_period), 
                               ~ replace_na(. != lag(., 1L), FALSE)
    ))
  #data[, period_group := cumsum(new_period)]
  
  
  # data <- data[, 
  #              map(.SD, first), 
  #              .SDcols = c("time", constant_on_period, count_vars),
  #              by = "period_group"
  # ]
  data <- data[new_period == TRUE, ] # !!
  
  data <- data |> rename(start = time)
  data[, new_ind := id != lag(id, 1L, default = ".N00.")]
  data[, stop := if_else(!lead(new_ind, default = last(new_ind)), 
                         true  = lead(start), 
                         false = start + 1L)]
  data[, event2 := if_else(!lead(new_ind, default = TRUE), lead(event), FALSE)]
  data <- data[event == FALSE, ]
  data[, event := event2]
  data[, event2 := NULL]
  
  if (out_of_range) {
    data <- data |> mutate(.condition = !!out_of_range_condition)
    message(paste0(sum(data$.condition), " obs removed with out_of_range_condition = ", 
                   str_remove(paste0(format(out_of_range_condition), collapse = " "), "~" ) |>
                     str_squish()))
    data <- data[.condition == FALSE, ]
  }
  
  data <- data |> 
    select(id, start, stop, event, any_of(c("start_time")), 
           all_of(c(constant_vars, start_names, time_varying_vars, count_vars))
    )
  
  # data |> select(start, stop, event, start_AGE, start_CJ_SER,  mob_DEPT, SEXE) |> samp(40)
  
}




# x <- coxreg(Surv(start, stop, event) ~ strata(SEXE)  +strata(GEN6) + 
#               # Nbis_Aplus_nb + + Nbis_A_nb + Nbis_P_nb + # Nbis_B_nb + Nbis_C_nb + 
#               ETAT_NB1_AGE20 + ETAT_TIT_NB1_CJ_SER + ETAT_TIT_NB1_EMP_MIN12 +
#               CORPS_plus_deb_A3 +
#               EF_nb + mob_REGT + mob_CORPS, # mob_DEPT + 
#             data = data)
# 
# 
# tidy_coxreg(coxreg(Surv(start, stop, event) ~ SEXE  + # GEN6 + 
#                      # Nbis_Aplus_nb + + Nbis_A_nb + Nbis_P_nb + # Nbis_B_nb + Nbis_C_nb + 
#                      ETAT_NB1_AGE20 + ETAT_TIT_NB1_CJ_SER + ETAT_TIT_NB1_EMP_MIN12 +
#                      CORPS_plus_deb_A3 +
#                      EF_nb + mob_REGT + mob_CORPS, # mob_DEPT + 
#                    data = data))
# 
# tidy_coxreg(coxreg(Surv(start, stop, event) ~ strata(SEXE) + 
#                      # Nbis_Aplus_nb + + Nbis_A_nb + Nbis_P_nb + # Nbis_B_nb + Nbis_C_nb + 
#                      ETAT_NB1_AGE20 + ETAT_TIT_NB1_CJ_SER + ETAT_TIT_NB1_EMP_MIN12 +
#                      CORPS_plus_deb_A3 +
#                      EF_nb + mob_REGT + mob_CORPS, # mob_DEPT + 
#                    data = data))
# 
# tidy_coxreg(coxreg(Surv(start, stop, event) ~ strata(SEXE)  +strata(GEN6) + 
#                      # Nbis_Aplus_nb + + Nbis_A_nb + Nbis_P_nb + # Nbis_B_nb + Nbis_C_nb + 
#                      ETAT_NB1_AGE20 + ETAT_TIT_NB1_CJ_SER + ETAT_TIT_NB1_EMP_MIN12 +
#                      CORPS_plus_deb_A3 +
#                      EF_nb + mob_REGT + mob_CORPS, # mob_DEPT + 
#                    data = data))






# eha:::print.coxreg
tidy_coxreg <- function(x) {
  
  # Coefficients etc.
  # coef <- x$coef
  # se <- sqrt(diag(x$var))
  # wald.p <- #formatC(
  #   1 - pchisq((coef/se)^2, 1) #, digits = 3, 
  # # width = 9, format = "f")
  # e.coef <- exp(coef)
  
  if (x$nullModel) return(new_tab())
  
  ci <- exp(confint(x, level = 0.95))
  
  coef_data <- tibble(name_join= names(x$coef), 
                      coef     = x$coef, 
                      rel_risk = exp(coef), 
                      std.error= sqrt(diag(x$var)), 
                      wald.p   =  1 - pchisq((coef/std.error)^2, 1), 
                      conf.high = ci[, 2],
                      conf.low   = ci[, 1],
  )
  
  
  # Récupérer et ajouter les catégories de référence
  
  factors <- attr(x$terms, "factors")
  resp <- attr(x$terms, "response")
  row.strata <- attr(x$terms, "specials")$strata
  row_strata <- rownames(attr(x$terms, "factors"))[row.strata]
  
  #if (!is.null(row.strata)) {
  #  col.strata <- which(factors[row.strata, ] == 1)
  #  
  #  which(factors[row.strata, ] == 1)
  #  
  #} else { 
  #  col.strata <- NULL 
  #} 
  if (!is.null(row.strata)) {
    factors <- attr(x$terms, "factors")[-c(resp, row.strata), 
                                        , drop = FALSE]
    
    col.strata <-  which(colnames(factors) %in% row_strata)
    
    factors <- factors[, -col.strata]
    
  } else {
    factors <- attr(x$terms, "factors")[-c(resp, row.strata), 
                                        , drop = FALSE]
  }
  covar.names <- x$covars
  term.names <- colnames(factors)[!str_detect(colnames(factors), ":")] # interactions
  isF <- x$isF
  #ord <- attr(x$terms, "order")
  #if (!is.null(col.strata)) ord <- ord[-col.strata]
  #index <- 0
  
  vars_factor <- term.names[isF] 
  covar.no <- map_int(1:length(term.names), ~ which(factors[, .] == 1)) |>
    set_names(term.names)
  
  levels_of_vars <- tibble(predictor = term.names[isF],
                           level =  x$levels[covar.no[isF]], 
                           means = x$w.means[covar.no[isF]]
  )
  
  means <- tibble(predictor = term.names[!isF], 
                  means2 = flatten_dbl(x$w.means[covar.no[!isF]]))
  
  
  res <- tibble(predictor = term.names)
  if (nrow(levels_of_vars) > 0) {
    res <- res |>
      left_join(levels_of_vars, by = "predictor") |> 
      mutate(level = map(level, ~ if (length(.) == 0) {NA_character_} else {.})) |> 
      unnest(c(level, means))  |> 
      group_by(predictor) |>
      mutate(reference = level != "" & row_number() == 1L) |>
      ungroup() |>
      left_join(means, by = "predictor") |>
      mutate(means = if_else(!is.na(means), means, means2)) |>
      select(-any_of(c("means2"))) #|> 
    #new_tab()
    
  } else {
    res <- res |>
      left_join(means, by = "predictor") |>
      rename(means = means2) #|> 
    #new_tab()
  }
  
  
  if (length(row_strata) > 0) {
    res <- bind_rows(tibble(predictor = row_strata, 
                            level = rep(NA_character_, length(row_strata)), 
                            means = rep(NA_real_, length(row_strata)),
                            reference = rep(NA, length(row_strata))), 
                     res # , 
                     
    )
    
  }
  
  if (nrow(levels_of_vars) > 0) {
    res <- 
      coef_data |> 
      mutate(name_join = as_factor(name_join)) |> # to keep order
      full_join(mutate(res, name_join = as.factor(paste0(predictor, replace_na(level, "")))), 
                by = "name_join") |>
      #select(-any_of(c("name_join"))) |>
      mutate(coef     = if_else(!is.na(reference) & reference, 0, coef), 
             rel_risk = if_else(!is.na(reference) & reference, 1, rel_risk), 
      ) |>
      select(name_join, predictor, level, reference, means, everything()) |>
      mutate(
        order = fct_relabel(name_join,
                            ~ reduce(flatten_chr(levels_of_vars$level),
                                     .init = .,  
                                     ~ str_remove(.x, fixed(.y)))
                            #~str_remove(., paste0(paste0(level, "$"), collapse = "|") )
        )
      ) |> 
      arrange(order, !replace_na(reference, FALSE), name_join) |>
      mutate(interact_fact = is.na(reference) & 
               str_detect(order, paste0(vars_factor, collapse = "|")) )
    
    if (any(res$interact_fact)) {
      res <-  res |> 
        mutate(
          reference = if_else(interact_fact, FALSE, reference), 
          predictor = if_else(
            !interact_fact, 
            true  = predictor, 
            false = reduce(flatten_chr(levels_of_vars$level),
                           .init = name_join,  
                           ~ str_remove(.x, fixed(.y)))
            #str_remove_all(name_join, fixed(paste0(level, collapse = "|"), ))
          ), 
          level = if_else(
            !interact_fact, 
            true  = level, 
            false = reduce(vars_factor, .init = name_join, ~ str_remove(.x, fixed(.y)))
            #str_remove_all(name_join, fixed(paste0(level, collapse = "|"), ))
          )
        )
    }
    
    res <- res |> 
      select(-any_of(c("order", "name_join","interact_fact"))) |>
      select(predictor, level, reference, means, everything()) |> 
      new_tab()
    
    
  } else {
    res <- coef_data |> 
      rename(predictor = name_join) |> 
      full_join(res, by = "predictor") |> 
      select(predictor, means, everything()) |>
      mutate(level = NA_character_, reference = NA) |> 
      new_tab()
  }
  
  #formula <- 
  # suppressMessages({
  #   
  #   # if (!is.null(cl <- x$call)) {
  #   #   paste0( as.character(dput(cl)), collapse = " ; "  )
  #   # } else {
  #   #   NA_character_
  #   # }
  #   
  # })
  
  # # baseline hazard function (nelson-aalen and breslow estimator) :
  # time <- c(1, 3, 4, 6, 2, 7, 9, 11)
  # status <- c(1, 0, 1, 1, 1, 0, 1, 1)
  # 
  # df <- tibble(time, status) |> arrange(time) |>
  #   mutate(n  = length(d):1, 
  #          H0 = cumsum(status/n)
  #   )
  # 
  # model <- coxph(Surv(time, status) ~ 1, data = df, method = "breslow")
  # 
  # df |> add_column(nelson_aalen = basehaz(model)$hazard, # predictors taken to be 0
  #                  breslow      = basehaz(model, centered = FALSE)$hazard
  # )
  # 
  # plot(basehaz(x) |> select(time, hazard))
  
  # # basehaz(x) # code
  # 
  # sfit <- survfit(x, se.x = FALSE)
  # if (!centered) {
  #   zcoef <- ifelse(is.na(coef(x)), 0, coef(x))
  #   offset <- sum(x$means * zcoef)
  #   chaz <- sfit$cumhaz * exp(-offset)
  # } else {chaz <- sfit$cumhaz}
  # new <- data.frame(hazard = chaz, time = sfit$time)
  # strata <- sfit$strata
  # if (!is.null(strata)) 
  #   new$strata <- factor(rep(names(strata), strata), levels = names(strata))
  # new
  
  
  # mesures globales
  global_stats <- tibble(formula            = paste0(format(x$formula), collapse = "") |> str_squish(), 
                         n_observations     = length(x$residuals),
                         n_events           = x$n.events,
                         total_time_at_risk = x$ttr,
                         baseline_hazard_function = list(paste0(round(basehaz(x)$hazard, 4), collapse = ", ")),
                         max_loglik         = x$loglik[2],
                         log_rank_test      = -2 * (x$loglik[1] - x$loglik[2]),
                         df                 = if (is.null(x$df)) { sum(!is.na(coef)) } else { round(sum(x$df), 2) } ,
                         overall.p          = 1 - pchisq(log_rank_test, df),
                         frailty_sig        = if (!is.null(x$frailty)) {x$sigma} else {NA_real_},
                         frailty_sd         = if (!is.null(x$frailty)) {x$sigma.sd} else {NA_real_},
                         AIC                = AIC(x), 
                         BIC                = BIC(x),
                         icc                = if (length(x$icc) > 0 ) {
                           paste0("n_clust", x$icc[1], "ICC", x$icc[2], " ; ", x$icc[3])
                         } else {
                           ""
                         },
  )
  
  if (nrow(global_stats) >= 2) stop("global stats have more than one row")
  
  bind_cols(res, global_stats)
  
}




# formula <- expr(Surv(start, stop, event) ~ SEXE  + 
#                   ETAT_NB1_AGE20 + ETAT_TIT_NB1_CJ_SER + ETAT_TIT_NB1_EMP_MIN12 +
#                   CORPS_plus_deb_A3 +
#                   EF_nb + mob_REGT + mob_CORPS)

eha_table <- function(data, formula, id = "id",
                      constant_vars_pattern, # constant_vars_pattern_promo, 
                      n_min = 5L, remove_zero_event_cat = FALSE,
                      vif = TRUE, zph = TRUE, linearity_plot = FALSE,
                      print_plots = FALSE, join_model = FALSE, ...) {
  #formula <- enexpr(formula)
  formula_chr <- paste0(format(formula), collapse = "")
  #formula_chr <- formula_chr[!formula_chr == "~"]
  response_formula <- str_extract(formula_chr, "^[^~]+") |> str_squish()
  response_vars <- response_formula |>
    str_remove("^[^\\(]+\\(") |> str_remove("\\)$") |>
    str_split(pattern = ",") |> flatten_chr() |>
    str_squish()
  predictors_formula <- str_extract(formula_chr, "~.+$") |> str_remove("~") |> str_squish() #formula_chr[!str_detect(formula_chr, "^Surv\\(")]
  predictor_vars <- predictors_formula |> str_split(pattern = "\\+") |>
    flatten_chr() |> str_squish()
  strata_vars <- str_detect(predictor_vars, "strata\\(")
  predictor_vars <- predictor_vars[!str_detect(predictor_vars, ":")]
  predictor_vars <- predictor_vars |> str_remove("^[^\\(]+\\(") |> str_remove("\\)$")
  strata_vars <- predictor_vars[strata_vars]
  
  
  # model_frame <- model.frame(formula, data = data) |> as_tibble()
  # # as_tibble(get_all_vars(formula, data = data)) # can pass further vars
  
  
  if (remove_zero_event_cat) {
    factor_vars <- map_lgl(select(data, predictor_vars), is.factor)
    factor_vars <- names(factor_vars)[factor_vars]
    factor_vars <- set_names(factor_vars, factor_vars)
    
    if(length(factor_vars) > 0) {
      
      tabs_no_event <- map(factor_vars, 
                           ~ data[!is.na(eval(str2expression(.))), 
                                  .(event = sum(event, na.rm = TRUE)), 
                                  keyby = .]) |> 
        imap_dfr(~ .x[event == 0, ][, predictor := .y]  |>
                   rename(any_of(c(set_names(.y, "level"))))
        ) |> 
        as_tibble() |> select(predictor, level)  
      no_event_vars <- tabs_no_event$predictor |> unique() |> sort()
      
      if (length(no_event_vars) > 0) {
        
        levels_to_filter <- tabs_no_event |> 
          group_split(predictor) %>%
          set_names(map_chr(., ~ as.character(first(pull(., "predictor"))))) |>
          map(~ as.character(pull(., level)))
        
        levels_to_filter <- levels_to_filter[!is.na(levels_to_filter)]
        
        if (length(levels_to_filter) > 0) {
          for(i in 1:length(levels_to_filter)) {
            vars_recode <- names(levels_to_filter)[i]
            VARS_RECODE <- str2expression(vars_recode)
            
            nrow_start <- nrow(data)
            data[!eval(VARS_RECODE) %in% levels_to_filter[[i]], ]
            nrow_end <- nrow(data)
            warning(paste0(nrow_start - nrow_end, 
                           "rows were removed, due to categories with no event"))
          }
        }
      }
    }
  }
  
  
  
  # we redo the formula on the current environment, otherwise basehaz() no working in tidy_coxreg
  formula_in_current_env <- str2expression(paste0(format(formula), collapse = ""))
  model     <- coxreg(eval(formula_in_current_env), data = data, ...) 
  eha_table <- tidy_coxreg(model) 
  
  # max number of predictors in cox regression : n_events/15
  n_pred <- length(eha_table$coef[!is.na(eha_table$coef) & (!eha_table$reference | is.na(eha_table$reference) )])
  n_events <- eha_table$n_events[1]
  n_events_ratio <- n_events/15
  
  if (n_pred > round(n_events_ratio)) {
    message(paste0(n_events, " events ; max advised nb of predictors: ",
                   round(n_events_ratio), " ; ",
                   n_pred, " predictors ; " ))
  }
  
  
  # stats descriptives predictors
  num_vars    <- model$covars[!model$isF]
  factor_vars <- model$covars[model$isF]
  
  # num_vars    <- data |> select(all_of(predictor_vars[predictor_vars != "1"]) & where(is.numeric)) |> names()
  # factor_vars <- data |>
  #   select(all_of(predictor_vars[predictor_vars != "1"]) & 
  #            where(~ is.factor(.) | is.character(.))) |>
  #   names()
  
  if (!missing(constant_vars_pattern)) {
    constant_vars <- 
      names(data)[str_detect(names(data), 
                             paste0(constant_vars_pattern, 
                                    collapse = "|" ))]
    
  } else {
    constant_vars <- NULL
  }
  
  if (length(num_vars) > 0) {
    constant_num_vars <- num_vars[num_vars %in% constant_vars]
    # other_num_vars  <- num_vars[!num_vars %in% constant_vars]
    # 
    # if (length(other_num_vars) > 0) {
    #   constant_other_num_vars <- constant_cols(select(data, id, all_of(other_num_vars)), "id")
    #   constant_num_vars <- c(constant_num_vars, constant_other_num_vars)
    # }
    
    # # found two ids with varying SEXE
    # distinct(select(data,id, SEXE, start), id, SEXE, .keep_all = TRUE ) |>
    #   group_by(id) |> mutate(n = n()) |> ungroup() |> filter(n >= 2)
    
    # for all vars, constant and varying, means over all observations
    means <- data[, map(.SD, ~ mean(., na.rm = TRUE)), .SDcols = num_vars]
    sds   <- data[, map(.SD, ~ sd  (., na.rm = TRUE)), .SDcols = num_vars]
    mins  <- data[, map(.SD, ~ min (., na.rm = TRUE)), .SDcols = num_vars]
    maxs  <- data[, map(.SD, ~ max (., na.rm = TRUE)), .SDcols = num_vars]
    
    means <- means |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "mean")
    sds   <- sds   |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "sd")
    mins  <- mins  |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "min")
    maxs  <- maxs  |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "max")
    
    # means at event
    means_event <- data[event == TRUE, map(.SD, ~ mean(., na.rm = TRUE)), .SDcols = num_vars]
    sds_event   <- data[event == TRUE, map(.SD, ~ sd  (., na.rm = TRUE)), .SDcols = num_vars]
    
    means_event  <- means_event |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "mean_at_event")
    sds_event    <- sds_event   |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "sd_at_event")
    
    # for constant_vars, means for individuals
    if (length(constant_num_vars) > 0) {
      data_id <- setorder(copy(data), -start)[!duplicated(id), ]
      
      means_id <- data_id[, map(.SD, ~ mean(., na.rm = TRUE)), .SDcols = constant_num_vars]
      sds_id   <- data_id[, map(.SD, ~ sd  (., na.rm = TRUE)), .SDcols = constant_num_vars]
      
      means_id <- means_id|> pivot_longer(cols = everything(), names_to = "predictor", values_to = "mean_ind")
      sds_id   <- sds_id  |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "sd_ind")
      
      num_data <- reduce(list(means, sds, mins, maxs, means_event, sds_event, 
                              means_id, sds_id), 
                         ~ left_join(.x, .y, by = "predictor")
      )
      
    } else {
      num_data <- reduce(list(means, sds, mins, maxs, means_event, sds_event), 
                         ~ left_join(.x, .y, by = "predictor")
      )
    }
    
    
    
    eha_table <- eha_table |> left_join(num_data, by = "predictor")
    # eha_table |> select(predictor, level, mean, sd, max, min)
    
  } else {
    constant_num_vars <- NULL
  }
  
  if (length(factor_vars) > 0) {
    constant_factors <- factor_vars[factor_vars %in% constant_vars]
    # other_factors <- factor_vars[!factor_vars %in% constant_vars]
    # 
    # if (length(other_factors) > 0) {
    #   constant_other_factors <- constant_cols(select(data, id, all_of(other_factors)), "id")
    #   constant_factors <- c(constant_factors, constant_other_factors)
    # }
    
    # for all factors, counts and event.prob
    factor_data <- factor_vars |> map_dfr( 
      ~ data[!is.na(eval(sym(.))),
             .(n = .N, event.prob = sum(event == TRUE)/.N),
             keyby = .] |>
        rename(set_names(., "level")) |>
        mutate(predictor = .)
    )
    
    # for constant vars, ids counts
    if (length(constant_factors) > 0) {
      if (length(constant_num_vars) == 0) data_id <- setorder(copy(data), -start)[!duplicated(id), ]
      
      factor_data_id <- constant_factors |> map_dfr( 
        ~ data_id[!is.na(eval(sym(.))), .(n_id = .N), keyby = .] |>
          rename(set_names(., "level")) |>
          mutate(predictor = .)
      )
      
      factor_data <- factor_data |> 
        left_join(factor_data_id, by = c("predictor", "level"))
      
      if (n_min > 1) factor_data <- factor_data[is.na(n_id) | n_id >= n_min, ]
    }
    
    if (n_min > 1) factor_data <- factor_data[is.na(n) | n >= n_min, ]
    
    eha_table <- eha_table |>
      left_join(factor_data, by = c("predictor", "level"))
    # eha_table |> select(predictor, level, n, event.prob)
    
  }
  
  eha_table <- eha_table |> 
    mutate(event_median_time = median(data$start[data$event]))
  
  
  
  
  
  # # we can only print predicted survival for a selection of values for predictors
  # df_predict <- 
  #   expand.grid(event = 1, 
  #               time = purrr::transpose(list(start = 0:44, stop = 1:45)), 
  #               SEXE = levels(data$SEXE), 
  #               
  #               ETAT_TIT_NB1_CJ_SER    = factor(levels(data$ETAT_TIT_NB1_CJ_SER    )[1], levels = levels(data$ETAT_TIT_NB1_CJ_SER    )),
  #               ETAT_TIT_NB1_EMP_MIN12 = factor(levels(data$ETAT_TIT_NB1_EMP_MIN12 )[1], levels = levels(data$ETAT_TIT_NB1_EMP_MIN12 )),
  #               CORPS_plus_deb_A3      = factor(levels(data$CORPS_plus_deb_A3      )[1], levels = levels(data$CORPS_plus_deb_A3      )),
  #               ETAT_NB1_AGE20         = mean(data$ETAT_NB1_AGE20, na.rm = TRUE), 
  #               EF_nb                  = mean(data$EF_nb         , na.rm = TRUE), 
  #               mob_REGT               = mean(data$mob_REGT      , na.rm = TRUE), 
  #               mob_CORPS              = mean(data$mob_CORPS     , na.rm = TRUE)
  #   ) |>
  #   mutate(start = map_int(time, ~ .$start), 
  #          stop  = map_int(time,  ~ .$stop)
  #   ) |> 
  #   select(-time) |> 
  #   as_tibble()
  # 
  # df_predict <- df_predict |>
  #   mutate(surv_perd = predict(model, df_predict, type = "survival"))
  # 
  # df_predict |> 
  #   mutate(promo = 1 - surv_perd) |> 
  #   ggplot(aes(x = start, y = promo, color = SEXE)) + 
  #   geom_line() + 
  #   theme_minimal()
  # 
  
  
  #coxreg(Surv(start, stop, event) ~ ETAT_NB1_CJ_SER + ETAT_TIT_NB1_CJ_SER + CJ_SER,
  #       data = data)
  
  
  # À quel moment de la trajectoire l'effet de CJ_SER prédomine-t-il ?
  # - LVCEE, l'effet du AC/SD d'entrée dans l'État n'est pas significatif
  # - Le AC/SD de titularisation joue autant que l'actuel
  # - L'effet de EP est inversé : pourrait /2 chances de promo 
  #   quand on y commence (pas signif), mais les multiplie par 2.5 quand 
  #   on y est au moment de la promotion (signif)
  
  
  
  
  ## car::Anova type 3 ; when 2 levels, same than p.value en coef table
  ## multiple degree of freedom Wald tests for categorical vars with levels > 2 
  #car::Anova(model, type = 3, test.statistic = "Wald")
  ## low p.value => significant hasards disparities between levels
  
  # likelihood ratio tests ?
  
  
  
  # Problems of collinearity : use vif() with lm over time
  if (length(predictor_vars) <= 1) vif <- FALSE
  if (vif) {
    lm_formula <- str2expression(paste0(response_vars[1], " ~ ", predictors_formula))
    vif_res <- car::vif(lm(eval(lm_formula), data = data), type = "predictor")
    
    if (is.data.frame(vif_res)) {
      vif_res <- vif_res |> as.data.frame() |> 
        rownames_to_column("predictor") |> as_tibble() |>
        rename(GVIF_df = `GVIF^(1/(2*Df))`)
    } else {
      vif_res <- tibble(predictor = predictor_vars, GVIF = vif_res)
    }
    
    eha_table <- eha_table |>
      left_join(select(vif_res, predictor, GVIF, any_of(c("GVIF_df"))), 
                by = "predictor")
    
    # if (print_plots) print(vif_res)
  }
  #warning(paste0(vif))
  
  # Testing non-proportional hazards (effects that increase/decrease over time)
  # - stata() # also used when a factor has too much levels
  # - with a continuous predictor : we categorize
  # - interaction between time and a predictor (function based on cox.zph curve)
  #    example : tt = function(x, t, ...) x * t ; tt = function(x, t, ...) x * log(t)
  #              in the equation : ... + var + tt(var) ...
  #   (can be done over a categorical predictor if it is a dummy predictor)
  
  # To test it with cox.zph, we need to transform factors into dummy variables
  # (in a df with no na), and to redo the model with coxph()
  
  
  if (zph | linearity_plot) {
    data_no_na <- select(data, all_of(c(response_vars, predictor_vars)) ) |> 
      as.data.table() |> na.omit()
  }
  
  if (zph) {
    model_no_na <- coxph(eval(formula_in_current_env), data = data_no_na
    )
    modmat <- bind_cols(select(data_no_na, all_of(response_vars)), 
                        model.matrix(model_no_na)
    ) |>
      mutate(across(where(is.logical), as.integer), 
             across(where(is.integer), as.double), 
      ) |> as_tibble()
    model_dummies <- coxph( eval(str2expression(paste0(response_formula, " ~ ."))), 
                            data = modmat) # same coefs
    zph_res <- survival::cox.zph(model_dummies)
    zph_splines <- zph_res |> plot.cox.zph_extract_spline() |> 
      group_by(predictor) |>
      summarise(across(c(spline, spline.high, spline.low), 
                       ~ paste0(round(., 3), collapse = ", "))
      )
    
    zph_p <- zph_res$table[, "p"]
    zph_p <- tibble(predictor = names(zph_p), zph.p = zph_p)
    zph_p <- zph_p |> left_join(zph_splines, by = "predictor")
    
    zph_p <- zph_p |> mutate(predictor =  str_remove_all(predictor, "`"))
    
    # HO : le coefficient ne varie pas avec le temps
    # => p < 5% means the coeff varies over time (=> non-proportional hazards)
    # - `ETAT_TIT_NB1_CJ_SER3-EP` 4.8% (rare en fin de carrière)
    # - `ETAT_TIT_NB1_EMP_MIN1205-Affaires étrangères` 2% (en U, fort début carrière)
    # - `CORPS_plus_deb_A399-Pas encore cadre A État`  3.4% (rare fin de carrière ; 
    #      et tj en dessous de 0)
    # - EF_nb 4% (baisse après 30 ans de carrière = après age de 50 ans)
    #        (=> categorize, or stata over 0-30/30-45 ans)
    # - GLOBAL is BAD too (0.024%) ? H0 : all predictors meet the PH assumption
    
    eha_table <- eha_table |> 
      select(-any_of(c("name_join", "zph.p"))) |> 
      mutate(name_join = paste0(predictor, replace_na(level, "")) )  |> 
      left_join(zph_p, by = c("name_join" = "predictor")) |>
      select(-any_of(c("name_join" ))) |> 
      select(predictor, level, reference, means, coef, rel_risk, std.error, wald.p, 
             conf.high, conf.low, zph.p, spline, spline.high, spline.low, everything())
    #eha_table  |> select(predictor:zph.p) |> select(-std.error, -reference, -means)
    
    noph <- zph_p |> filter(zph.p < 0.05) |> pull(predictor)
    #noph <- noph[noph != "GLOBAL"]
    
    if (length(noph) > 0) {
      message(paste0("some predictors do not respect proportional hazards assumption:\n ", 
                     paste0(noph, collapse = "\n ")))
      
      # zph_res$y <- 
      #   zph_res$y[, str_remove_all(colnames(zph_res$y), "`") %in% noph, drop = FALSE]   
      # 
      # colnames(zph_res$y)[str_remove_all(colnames(zph_res$y), "`") %in% noph]
      # #colnames(zph_res$y) <- colnames(zph_res$y) |> str_remove_all(tabxplor:::cleannames_condition())
      # 
      # if (print_plots) {
      #   par(mfrow = c(ceiling(ncol(zph_res$y)/3), 3), mar = c(2, 4, 1, 1), cex = 1.1) # b l t r
      #   plot(zph_res) # horizontal = GOOD
      #   par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
      # }
      
    }
  } else {
    
    noph <- NULL
  }
  
  if (linearity_plot) {
    # testing linearity assumption for continous predictors : martingale residuals
    # plot(residuals(model))
    num_vars <-  model$covars[! model$isF]
    residuals_data <- select(data_no_na, all_of(c(num_vars))) |> 
      add_column(residuals = resid(model, type = "martingale")) |> 
      pivot_longer(cols = -residuals, names_to = "predictor", values_to = "value") 
    
    # X <- residuals_data$EF_nb
    # Y <- residuals_data$residuals
    # plot(X, Y, pch = 20, col = "darkgray")
    # abline(h = 0)
    # lines(smooth.spline(X, Y, df = 7), lty = 2, lwd = 2)
    
    linearity_plot_res <-
      residuals_data |> 
      ggplot(aes(x = value, y = residuals)) +
      geom_point(shape = 1, alpha = 0.4) + 
      geom_smooth(formula = y ~ x, method = "gam", #"loess", 
                  linetype = "dashed", 
                  na.rm = TRUE) + 
      facet_wrap("predictor", scales = "free_x")
    if (print_plots) print(linearity_plot_res)
  }
  
  #   print_tidy_coxreg(eha_table)
  
  #if (vif)                    eha_table <- eha_table |> `attr<-`("vif"           , vif_res)
  if (zph & length(noph) > 0) eha_table <- eha_table |> `attr<-`("zph_res"       , zph_res)
  if (linearity_plot)         eha_table <- eha_table |> `attr<-`("linearity_plot", linearity_plot_res)
  if (join_model) eha_table <- eha_table |> `attr<-`("model", model)
  
  eha_table
}








print_tidy_coxreg <- function(eha_table, n = 100, vars = NULL, mean_and_sd = FALSE,
                              diff_factor = 10L, graphs = FALSE, warnings = TRUE) {
  
  if (is.list(eha_table) & !is.data.frame(eha_table)) {
    iwalk(eha_table, 
          ~ { 
            cat(paste0("\n", .y, "\n"))
            print_tidy_coxreg(.x) 
          }
    )
    return(invisible(eha_table))
  }
  
  mean_and_sd <- if (mean_and_sd) { c("mean", "sd")} else {NULL}
  
  if (warnings & "warnings" %in% names(eha_table)){ warnings <- "warnings" } else {warnings <- NULL }
  
  res <- eha_table |> 
    group_by(predictor) |>
    mutate(refrows = dplyr::row_number() == 1 & !is.na(reference)) |>
    ungroup() 
  
  nbrows <- nrow(res)
  
  if(no_sd <- !"sd" %in% names(res)) {
    res <- res |> mutate(sd = 1)
  }
  
  
  if("event.prob" %in% names(res)) {
    res <- res |> 
      mutate(n_event = n * event.prob, 
             event.prob = tabxplor:::new_fmt(n = as.integer(round(n * event.prob)),
                                             type = "row", 
                                             digits =  rep(1L, nbrows), 
                                             pct  = event.prob, 
                                             diff = (event.prob - n_events/n_observations)*diff_factor,
                                             in_refrow = refrows, 
                                             col_var   = "event.prob", 
                                             color = "diff"
             )
      )
  }
  
  if("zph.p" %in% names(res)) {
    res <- res |> 
      mutate(zph.p = tabxplor:::new_fmt(n = rep(0L, nbrows), 
                                        type      = "row", 
                                        digits    = rep(1L, nbrows), 
                                        pct       = zph.p, 
                                        diff      = if_else(zph.p < 0.05, -100, 100), 
                                        diff_type = "1", 
                                        color     = "diff", 
      ))
  }
  
  
  
  res <- res |> 
    mutate(rel_risk = tabxplor:::new_fmt(n         = rep(0L, nbrows), 
                                         type      = "mean", 
                                         digits    = rep(2L, nbrows), 
                                         mean     = rel_risk, 
                                         diff      = case_when(is.na(reference) ~ rel_risk ^sd, 
                                                               reference        ~ rep(0, length(rel_risk)), 
                                                               TRUE             ~ rel_risk, 
                                         ), 
                                         
                                         diff_type = "1", 
                                         color     = "diff", 
                                         in_refrow = refrows, 
                                         col_var   = "rel_risk", 
                                         comp_all  = FALSE, 
    )) |>
    mutate(wald.p = tabxplor:::new_fmt(n         = rep(0L, nbrows), 
                                       type      = "row", 
                                       digits    = rep(2L, nbrows), 
                                       pct       = wald.p, 
                                       diff      = if_else(wald.p >= 0.05, -100, 100), 
                                       
                                       diff_type = "1", 
                                       color     = "diff", 
    ), 
    
    across(any_of(c("n", "n_event")), ~ tabxplor:::new_fmt(n = as.integer(.))), 
    
    across(any_of(c("mean")), ~ tabxplor:::new_fmt(n = n_observations, type = "mean", mean = ., digits = rep(2L, nbrows), )),
    
    across(any_of(c("sd")), ~ tabxplor:::new_fmt(n = n_observations, type = "mean", var = ., display = "var", digits = rep(3L, nbrows), )),
    
    
    across(any_of(c("level")), ~ if_else(is.na(.), ".", .)),
    
    
    ) |>
    select(any_of(c("event", "model_name", "split_var")), 
           predictor, any_of(c("level")), rel_risk, 
           wald.p, vars, 
           any_of(c("n", "n_event", "event.prob", "zph.p", "GVIF", 
                    mean_and_sd)), 
           warnings,
    ) |>
    mutate(across(any_of(c("predictor", "level")), as.factor)) |> 
    new_tab() 
  
  groups <- names(res)[names(res) %in% c("event", "model_name", "split_var")]
  if (length(groups) > 0 ) {
    res <- res |> 
      group_by(!!!syms(groups))  |>
      mutate(rn1 = row_number() == 1) |> 
      ungroup() |>
      mutate(across(any_of(c("event", "model_name","split_var")), 
                    ~ if_else(rn1, as.character(.), "."))
      ) |> 
      select(-rn1) #|>
    #group_by(!!!syms(groups))
  }
  
  if(no_sd) res <- select(res, -any_of(c("sd")))
  
  print(res, n = n)
  
  if (graphs) {
    # if(length(attr(eha_table, "vif")) > 0) {
    #   print(attr(eha_table, "vif"))
    # }
    
    # if(length(zph_res <- attr(eha_table, "zph_res")) > 0) {
    #   par(mfrow = c(ceiling(ncol(zph_res$y)/3), 3), mar = c(2, 4, 1, 1), cex = 1.1) # b l t r
    #   plot(zph_res) # horizontal = GOOD
    #   par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
    # }
    
    if(length(attr(eha_table, "linearity_plot")) > 0) {
      print(attr(eha_table, "linearity_plot"))
    }
  }
  
  invisible(eha_table)
}



# response_formula <- expr(Surv(start, stop, event))
# predictor_vars <- c("1", "SEXE", "ETAT_NB1_AGE20", "ETAT_TIT_NB1_CJ_SER",
# "ETAT_TIT_NB1_EMP_MIN12", "CORPS_plus_deb_A3", "EF_nb",         
# "mob_REGT", "mob_CORPS"
# )

# kms <- univariate_KM_tables(data, Surv(start, stop, event), 
# predictor_vars = c("1", "SEXE", "ETAT_NB1_AGE20", "ETAT_TIT_NB1_CJ_SER",
#                    "ETAT_TIT_NB1_EMP_MIN12", "CORPS_plus_deb_A3", "EF_nb",         
#                    "mob_REGT", "mob_CORPS"
# ) )


univariate_km_tables <- function(data, response_formula, id = "id",
                                 constant_vars_pattern = constant_vars_pattern_promo, 
                                 remove_zero_event_cat = FALSE,
                                 predictor_vars, n_min = 5L,
                                 eha_other_cats = eha_other_cats_promo) {
  response_formula <- enexpr(response_formula)
  response_vars <- as_label(response_formula) |>
    str_remove("^[^\\(]+\\(") |> str_remove("\\)$") |>
    str_split(pattern = ",") |> flatten_chr() |>
    str_squish()
  
  univariate_formulas <- 
    paste0(as_label(response_formula), " ~ ", predictor_vars) |>
    map(str2expression)
  
  missvar <- !c(response_vars, predictor_vars[predictor_vars != "1"]) %in% names(data)
  if (any(missvar)) {
    stop(paste0("missing_variables: ", paste0(c(response_vars, predictor_vars[predictor_vars != "1"])[missvar], 
                                              collapse = ", ")))
  }
  
  data <- data |> select(all_of(c(response_vars, predictor_vars[predictor_vars != "1"], id = id)))
  setDT(data)
  
  if (remove_zero_event_cat) {
    factor_vars <- map_lgl(select(data, predictor_vars[predictor_vars != "1"]), is.factor)
    factor_vars <- names(factor_vars)[factor_vars]
    factor_vars <- set_names(factor_vars, factor_vars)
    
    tabs_no_event <- map(factor_vars, 
                         ~ data[!is.na(eval(str2expression(.))), 
                                .(event = sum(event, na.rm = TRUE)), 
                                keyby = .]) |> 
      imap_dfr(~ .x[event == 0, ][, predictor := .y] |>
                 rename_with(.cols = 1, ~ "level")
      ) |> 
      as_tibble() |> select(predictor, level)  
    no_event_vars <- tabs_no_event$predictor |> unique() |> sort()
    
    if (length(no_event_vars) > 0) {
      
      levels_to_filter <- tabs_no_event |> 
        mutate(new_level = eha_other_cats[as.character(predictor)],
               recode_vector = set_names(as.character(level), "NULL")) |>
        filter(!is.na(new_level)) |>
        group_split(predictor) %>%
        set_names(map_chr(., ~ as.character(first(pull(., "predictor"))))) |>
        map(~ pull(., recode_vector))
      
      levels_to_filter <- levels_to_filter[!is.na(levels_to_filter)]
      
      if (length(levels_to_filter) > 0) {
        for(i in 1:length(levels_to_filter)) {
          vars_recode <- names(levels_to_filter)[i]
          VARS_RECODE <- str2expression(vars_recode)
          
          #if (is.logical(data[, eval(VARS_RECODE)])) stop(paste0(vars_recode, " is logical"))
          
          data[, eval(vars_recode) := fct_recode(eval(VARS_RECODE), !!!(levels_to_filter[[i]]))]
        }
      }
      
    }
  }
  
  
  # # first the cox models, with numeric vars as is (gather warnings)
  # cox_and_tidy <- function(formula, data) {
  #   nrow_start <- nrow(data)
  # 
  #   res <- coxreg(eval(formula), data) |> tidy_coxreg()
  #  # res <- tidy_coxreg(res) |> `attr<-`("model", res)
  #   
  #   if ("n_observations" %in% names(res)) {
  #     nrow_end <- res$n_observations[1]
  #     removed_rows <- nrow_start - nrow_end
  #     if (removed_rows >= 10) {
  #       warning(paste0(removed_rows, 
  #                      " rows removed (NA and cat with no event)"))
  #     }
  #   }
  # 
  #   res
  # }
  # 
  # quiet_cox_and_tidy <- quietly(cox_and_tidy)
  # cox <- map(univariate_formulas, 
  #            ~ quiet_cox_and_tidy(., data = data)
  # ) 
  # 
  # cox <- cox |> map_dfr(
  #   ~ mutate(.$result, 
  #            warnings = if (length(.$warnings) == 0) {
  #              ""
  #            } else {
  #              paste0(.$warnings, collapse = " ; ")
  #            } 
  #   )
  # )
  # # cox |> print_tidy_coxreg()
  # 
  # 
  # 
  # 
  # 
  # stats descriptives predictors
  num_vars    <- data |> select(all_of(predictor_vars[predictor_vars != "1"]) & 
                                  where(is.numeric)) |> names()
  factor_vars <- data |>
    select(all_of(predictor_vars[predictor_vars != "1"]) & 
             where(~ is.factor(.) | is.character(.))) |>
    names()
  # 
  # if (!missing(constant_vars_pattern)) {
  #   constant_vars <- 
  #     names(data)[str_detect(names(data), 
  #                            paste0(constant_vars_pattern, 
  #                                   collapse = "|" ))]
  #   
  # } else {
  #   constant_vars <- NULL
  # }
  # 
  # if (length(num_vars) > 0) {
  #   constant_num_vars <- num_vars[num_vars %in% constant_vars]
  #   # other_num_vars  <- num_vars[!num_vars %in% constant_vars]
  #   # 
  #   # if (length(other_num_vars) > 0) {
  #   #   constant_other_num_vars <- constant_cols(select(data, id, all_of(other_num_vars)), "id")
  #   #   constant_num_vars <- c(constant_num_vars, constant_other_num_vars)
  #   # }
  # 
  #   # # found two ids with varying SEXE
  #   # distinct(select(data,id, SEXE, start), id, SEXE, .keep_all = TRUE ) |>
  #   #   group_by(id) |> mutate(n = n()) |> ungroup() |> filter(n >= 2)
  #   
  #   # for all vars, constant and varying, means over all observations
  #   means <- data[, map(.SD, ~ mean(., na.rm = TRUE)), .SDcols = num_vars]
  #   sds   <- data[, map(.SD, ~ sd  (., na.rm = TRUE)), .SDcols = num_vars]
  #   mins  <- data[, map(.SD, ~ min (., na.rm = TRUE)), .SDcols = num_vars]
  #   maxs  <- data[, map(.SD, ~ max (., na.rm = TRUE)), .SDcols = num_vars]
  #   
  #   means <- means |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "mean")
  #   sds   <- sds   |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "sd")
  #   mins  <- mins  |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "min")
  #   maxs  <- maxs  |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "max")
  #   
  #   # means at event
  #   means_event <- data[event == TRUE, map(.SD, ~ mean(., na.rm = TRUE)), .SDcols = num_vars]
  #   sds_event   <- data[event == TRUE, map(.SD, ~ sd  (., na.rm = TRUE)), .SDcols = num_vars]
  #   
  #   means_event  <- means_event |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "mean_at_event")
  #   sds_event    <- sds_event   |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "sd_at_event")
  #   
  #   # for constant_vars, means for individuals
  #   if (length(constant_num_vars) > 0) {
  #     data_id <- setorder(copy(data), -start)[!duplicated(id), ]
  # 
  #     means_id <- data_id[, map(.SD, ~ mean(., na.rm = TRUE)), .SDcols = constant_num_vars]
  #     sds_id   <- data_id[, map(.SD, ~ sd  (., na.rm = TRUE)), .SDcols = constant_num_vars]
  #     
  #     means_id <- means_id|> pivot_longer(cols = everything(), names_to = "predictor", values_to = "mean_ind")
  #     sds_id   <- sds_id  |> pivot_longer(cols = everything(), names_to = "predictor", values_to = "sd_ind")
  #     
  #     num_data <- reduce(list(means, sds, mins, maxs, means_event, sds_event, 
  #                             means_id, sds_id), 
  #                        ~ left_join(.x, .y, by = "predictor")
  #     )
  #     
  #   } else {
  #     num_data <- reduce(list(means, sds, mins, maxs, means_event, sds_event), 
  #                        ~ left_join(.x, .y, by = "predictor")
  #     )
  #   }
  #   
  # 
  #   
  #   cox <- cox |> left_join(num_data, by = "predictor")
  #   # cox |> select(predictor, level, mean, sd, max, min)
  # 
  #   } else {
  #   constant_num_vars <- NULL
  # }
  # 
  # if (length(factor_vars) > 0) {
  #   constant_factors <- factor_vars[factor_vars %in% constant_vars]
  #   # other_factors <- factor_vars[!factor_vars %in% constant_vars]
  #   # 
  #   # if (length(other_factors) > 0) {
  #   #   constant_other_factors <- constant_cols(select(data, id, all_of(other_factors)), "id")
  #   #   constant_factors <- c(constant_factors, constant_other_factors)
  #   # }
  #   
  #   # for all factors, counts and event.prob
  #   factor_data <- factor_vars |> map_dfr( 
  #     ~ data[!is.na(eval(sym(.))),
  #            .(n = .N, event.prob = sum(event == TRUE)/.N),
  #            keyby = .] |>
  #       rename(set_names(., "level")) |>
  #       mutate(predictor = .)
  #   )
  #   
  #   # for constant vars, ids counts
  #   if (length(constant_factors) > 0) {
  #     if (length(constant_num_vars) == 0) data_id <- setorder(copy(data), -start)[!duplicated(id), ]
  #     
  #     factor_data_id <- constant_factors |> map_dfr( 
  #       ~ data_id[!is.na(eval(sym(.))), .(n_id = .N), keyby = .] |>
  #         rename(set_names(., "level")) |>
  #         mutate(predictor = .)
  #     )
  #     
  #     factor_data <- factor_data |> 
  #       left_join(factor_data_id, by = c("predictor", "level"))
  #     
  #     if (n_min > 1) factor_data <- factor_data[is.na(n_id) | n_id >= n_min, ]
  #   }
  #   
  #   if (n_min > 1) factor_data <- factor_data[is.na(n) | n >= n_min, ]
  #   
  #   
  #   cox <- cox |>
  #     left_join(factor_data, by = c("predictor", "level"))
  #   # cox |> select(predictor, level, n, event.prob)
  #   
  # }
  # 
  # cox <- cox |> 
  #   mutate(event_median_time = median(data$start[data$event]))
  # 
  
  
  
  
  
  
  
  
  
  # kaplan-meier estimates 
  # cut numeric variables with sd (we expect them to be > 0)
  if (length(num_vars) > 0) {
    
    
    num_vars_cut <- cut_points(data, num_vars)
    
    for (i in 1:length(num_vars)) {
      data[, eval(num_vars[i]) := 
             cut(eval(str2expression(num_vars[i])), 
                 right = FALSE,
                 breaks = num_vars_cut[[i]], 
                 include.lowest = TRUE)  ]
    }
    
    #pmap(list(num_vars, num_vars_cut), function(.var, .cut)
    #  data[, cut(eval(str2expression(.var)), right = FALSE,
    #             breaks = .cut, include.lowest = TRUE)]
    #) |> 
    #  set_names(num_vars)
    
  }
  
  
  res <- map(univariate_formulas, 
             ~ broom::tidy(survfit(eval(.), data = data))
  ) |>
    set_names(predictor_vars)
  
  if ("1" %in% predictor_vars) {
    no_strata <- map_lgl(res, ~ !"strata" %in% names(.)) #one <- predictor_vars == "1"
    res <- res |> map_if(no_strata, ~ mutate(., strata = ""))
  }
  
  res <- res |> imap_dfr(~ mutate(.x, predictor = .y, .before = 1) |>
                           mutate(level = str_remove(strata, paste0(.y, "=")), 
                                  .after = "predictor") |>
                           select(-strata)
  )
  if (n_min >= 2) res <- res |> filter(n.risk >= n_min)
  
  res <- res |> mutate(predictor = if_else(predictor == "1", "null model", predictor))
  
  res <- res |> mutate(event.prob = n.event/n.risk, .after = "n.censor") |>
    select(-n.event)
  
  
  
  # # Kaplan-Meyer (KM) estimate of the survival function (survival/estimate)
  # #   S(t) = (1 - n.event/n.risk) * S(tprev)
  # KM <- survfit(Surv(start, stop, event) ~ 1, data = data)
  # summary(KM)
  # plot(KM)
  # 
  # # l'inverse : KM estimate de la probabilité de promotion en A+ : 
  # broom::tidy(KM) |>
  #   mutate(promo = 1 - estimate) |> 
  #   ggplot(aes(x = time, y = promo, ymin = 1 - conf.high, ymax = 1 - conf.low )) + 
  #   geom_line() + 
  #   geom_errorbar(color = "gray", linetype = "dashed") + 
  #   theme_minimal()
  # 
  # # non-parametric estimation
  # # plot(cumhaz(Surv(enter, exit, event) ~ 1, data = data))
  # 
  # # parametric estimation (proportional hazards model with parametric baseline hazards)
  # empty_model_phreg <- phreg(Surv(start, stop, event) ~ 1, data = data)
  # plot(empty_model_phreg)
  # 
  # # median survival time : only if survival drop above O.5
  # 
  # 
  # # KM for different categories
  # KM_SEXE <- survfit(Surv(start, stop, event) ~ DIP4, data = data)
  # 
  # broom::tidy(KM_SEXE) |>
  #   mutate(promo = 1 - estimate) |> 
  #   ggplot(aes(x = time, y = promo, ymin = 1 - conf.high, ymax = 1 - conf.low,
  #              color = strata)) + 
  #   geom_line() + 
  #   geom_errorbar(color = "gray", linetype = "dashed") + 
  #   theme_minimal()
  
  #list("km" = res, "cox" = cox)
  
  res  
}




print_km_inv <- function(kms, condition) {
  condition <- enquo(condition)
  
  df <-  kms |> 
    filter(!!condition) |> 
    mutate(promo = 1 - estimate)
  
  if(nrow(df) == 0) stop("zero rows df")
  
  max_time <- max(df$time)
  
  if (unique(df$predictor) >= 2 ) {
    df |> 
      ggplot(aes(x = time, y = promo, ymin = 1 - conf.high, ymax = 1 - conf.low,
                 color = level)) + 
      geom_line() + 
      facet_wrap("predictor") +  # scales = "free_y"
      geom_errorbar(linetype = "dashed", alpha = 0.3) +  # color = "gray"
      theme_minimal() + xlim(c(0, max_time))
    
  } else {
    df |> 
      ggplot(aes(x = time, y = promo, ymin = 1 - conf.high, ymax = 1 - conf.low,
                 color = level)) + 
      geom_line() + 
      geom_errorbar(linetype = "dashed", alpha = 0.3) +  # color = "gray"
      theme_minimal() + xlim(c(0, max_time))
  }
  
  
}



cut_points <- function(data, num_vars, centered = TRUE) {
  data <- data |> select(all_of(num_vars)) |> as.data.table()
  
  num_vars_int <- select(data, all_of(num_vars)) |> map(is.integer)
  sds   <- data[, map(.SD, ~ sd(., na.rm = TRUE)), .SDcols = num_vars]
  means <- data[, map(.SD, ~ mean(., na.rm = TRUE)), .SDcols = num_vars]
  mins  <- data[, map(.SD, ~ min (., na.rm = TRUE)), .SDcols = num_vars]
  maxs  <- data[, map(.SD, ~ max (., na.rm = TRUE)), .SDcols = num_vars]
  
  pmap(list(num_vars_int, means, sds, mins, maxs), function(.int, .mean, .sd, .min, .max)
  {
    if (centered) {
      if (.min >= 0) {
        res <- c(.min,  
                 if_else(.mean - 2 * .sd >= 0, .mean - 2 * .sd, 0),  
                 #if_else(.mean - 1 * .sd >= 0, .mean - 1 * .sd, 0), 
                 .mean - 0.5 * .sd, 
                 .mean + 0.5 * .sd, 
                 .mean + 2 * .sd, 
                 .max)
      } else {
        res <- c(if_else(.min <= .mean - 2 * .sd, .min, .mean - 2 * .sd),  
                 .mean - 2 * .sd, 
                 .mean - 0.5 * .sd, 
                 .mean + 0.5 * .sd, 
                 .mean + 2 * .sd, 
                 if_else(.max >= .mean + 2 * .sd, .max, .mean + 2 * .sd)#,  
        )
        
      }
      
    } else { # not centered
      if (.min >= 0) {
        res <- c(.min,  
                 if_else(.mean - 2 * .sd >= 0, .mean - 2 * .sd, 0),  
                 if_else(.mean - 1 * .sd >= 0, .mean - 1 * .sd, 0), 
                 .mean, 
                 .mean + 1 * .sd, 
                 .mean + 2 * .sd, 
                 .max)
      } else {
        res <- c(if_else(.min <= .mean - 2 * .sd, .min, .mean - 2 * .sd),  
                 .mean - 2 * .sd, 
                 .mean - 1 * .sd, 
                 .mean, 
                 .mean + 1 * .sd, 
                 .mean + 2 * .sd, 
                 if_else(.max >= .mean + 2 * .sd, .max, .mean + 2 * .sd)#,  
        )
        
      }
    }
    
    if (.int) res <- as.integer(round(res))
    res <- res[!duplicated(res)]
    res
  }
  
  ) |> 
    set_names(num_vars)
  
}


cut_with_sd <- function(x) {
  x <- data.table(num = x)
  num_vars_cut <- cut_points(x , "num")[[1]]
  
  x[, cut(num, right = FALSE,
          breaks = num_vars_cut, include.lowest = TRUE)
  ]
}





# data <- pan_cadres_eha_light
# 
# responses <- list(
#   "promo_A2" = ~ Surv(start, stop, event)
# )
# 
# predictors_sequence <- list(
#   "sociodemo" = ~ CORPS_plus_deb_A3,
#   "complet"   = ~ ETAT_NB1_AGE20 + ETAT_TIT_NB1_CJ_SER + ETAT_TIT_NB1_EMP_MIN12 +
#     CORPS_plus_deb_A3 + EF_nb + mob_REGT + mob_CORPS, 
#   "interact"  = ~ ETAT_TIT_NB1_CJ_SER + CORPS_plus_deb_A3 + EF_nb:CORPS_plus_deb_A3 +
#     EF_nb + mob_REGT + EF_nb:mob_REGT
# )
# # predictors_sequence <- list(
# #   "sociodemo" = ~ SEXE + CORPS_plus_deb_A3,
# #   "complet"   = ~ SEXE + ETAT_NB1_AGE20 + ETAT_TIT_NB1_CJ_SER + ETAT_TIT_NB1_EMP_MIN12 +
# #     CORPS_plus_deb_A3 + EF_nb + mob_REGT + mob_CORPS
# # )
# 
# split_var <- "SEXE"
# join_model <- FALSE
# vif = "complet"
# zph = "complet"
# linearity_plot = "complet"


eha_multi <- function(data, responses, predictors_sequence, split_var = NULL, 
                      n_min = 5L, join_model = FALSE, remove_zero_event_cat = FALSE,
                      eha_other_cats = eha_other_cats_promo, 
                      constant_vars_pattern = constant_vars_pattern_promo, 
                      linearity_plot = NULL) {
  
  responses_chr <- map_chr(responses, ~ paste0(format(.), collapse = "") |>
                             str_remove("~") |> str_squish() )
  responses_vars <- map(responses_chr, ~ str_remove(., "^[^\\(]+\\(") |> str_remove("\\)$") |>
                          str_split(pattern = ",") |> flatten_chr() |>
                          str_squish())
  
  predictors_chr <- map_chr(predictors_sequence, ~ paste0(format(.), collapse = "")  |>
                              str_remove("~") |> str_squish()  )
  
  predictor_vars <- map(predictors_chr, ~ str_split(., pattern = "\\+") |>
                          flatten_chr() |> str_squish())
  predictor_vars <- map(predictor_vars, ~ .[!str_detect(., ":")]) # remove interactions
  
  response_all  <- unique(flatten_chr(responses_vars))
  predictor_all <- unique(flatten_chr(predictor_vars))
  
  missvar <- !c(response_all, predictor_all, split_var) %in% names(data)
  if (any(missvar)) {
    stop(paste0("missing_variables: ", paste0(c(response_all, predictor_all, split_var)[missvar], 
                                              collapse = ", ")))
  }
  
  data <- dplyr::select(data, all_of(c(response_all, predictor_all, split_var)))
  setDT(data)
  # data <- na.omit(data)
  
  if (is.null(split_var)) {
    if (remove_zero_event_cat) data <- zero_event_cat_to_other(data, eha_other_cats = eha_other_cats)
    
    models_vars <- tidyr::crossing(event = as_factor(names(responses_chr)),
                                   #response   = responses_chr, 
                                   model_name = as_factor(names(predictors_chr)), 
                                   #predictors = predictors_chr,
                                   data       = list(data)
    ) |> 
      dplyr::mutate(split_var = "") |>
      left_join(tibble(event = names(responses_chr), response = responses_chr), 
                by = "event") |> 
      left_join(tibble(model_name = names(predictors_chr), predictors = predictors_chr), 
                by = "model_name") |>
      mutate(formulas = map2(response, predictors, ~ as.formula(paste0(.x, "~", .y))))
    
  } else {
    data[, eval(split_var) := fct_drop(eval(str2expression(split_var)))]
    data_split <- data |> 
      dplyr::rename(any_of(c("split_var" = split_var))) |>
      dplyr::group_by(split_var) |> group_nest() |> 
      dplyr::filter(if_any(1, ~!is.na(.))) |> 
      dplyr::rename(data = data) |>
      mutate(data = map(data, setDT))
    
    if (remove_zero_event_cat) {
      data_split <- data_split |>
        mutate(data = map(data, ~ zero_event_cat_to_other(., eha_other_cats = eha_other_cats)))
    }
    
    models_vars <- tidyr::crossing(event = as_factor(names(responses_chr)),
                                   #response   = responses_chr, 
                                   model_name = as_factor(names(predictors_chr)), 
                                   #predictors = predictors_chr,
                                   split_var   = data_split$split_var
    ) |> 
      # arrange(event, model_name) |> 
      left_join(tibble(event = as_factor(names(responses_chr)), response = responses_chr), 
                by = "event") |> 
      left_join(tibble(model_name = as_factor(names(predictors_chr)), predictors = predictors_chr), 
                by = "model_name") |>
      left_join(data_split, by = "split_var") |> 
      mutate(formulas = map2(response, predictors, ~ as.formula(paste0(.x, "~", .y)))) # as.formula(
    
  }
  
  models_vars <- models_vars |>
    mutate(#vif            = model_name %in% vif, 
      #zph            = model_name %in% zph, 
      linearity_plot = model_name %in% linearity_plot, 
    )
  
  #if (length(vif) > 0) if (vif == "all") models_vars$vif <- TRUE
  #if (length(zph) > 0) if (zph == "all") models_vars$zph <- TRUE
  if (length(linearity_plot) > 0) if (linearity_plot == "all") models_vars$linearity_plot <- TRUE
  
  # if (length(predictor_vars) <= 1) vif <- FALSE
  
  quiet_eha_table <- possibly(quietly(eha_table), otherwise = tibble(), quiet = FALSE)
  models_vars <- models_vars |>
    mutate(models = pmap(list(data, formulas, linearity_plot), # vif, zph
                         ~ quiet_eha_table(..1, ..2, 
                                           print_plots = FALSE,
                                           n_min       = n_min, 
                                           join_model  = join_model, 
                                           #vif = ..3, 
                                           #zph = ..3, 
                                           linearity_plot = ..3,
                                           remove_zero_event_cat = remove_zero_event_cat, 
                                           constant_vars_pattern = constant_vars_pattern # # constant_vars_pattern_promo, 
                         ))
    )
  #print("ok")
  models_vars <- models_vars |>
    mutate(models = set_names(models, str_squish(paste(event, model_name, split_var)))
    )
  
  
  
  # gather warning messages
  models_vars <- models_vars |>
    mutate(warnings = map(models, ~ unique(.$warnings)), 
           messages = map(models, ~ unique(.$messages[!str_detect(.$messages, "computed for predictors")])), 
           models   = map(models, ~ .$result)
    )
  
  #print(models_vars$models[[1]] |> attributes())
  
  model_messages <- models_vars |> 
    #mutate(name = paste(event, model_name, split_var)) |> 
    select(event, model_name, split_var, warnings, messages) |>
    pivot_longer(cols = c(warnings, messages), values_to = "messages", names_to = "type") |>
    filter(map_lgl(messages, ~ length(.) > 0)) |>
    unnest(messages) |> 
    mutate(messages = paste0(if_else(type == "warnings", "Warning: ","Message: "), messages)) |>
    mutate(messages = str_remove_all(messages, "\n") |>
             str_replace("some predictors do not respect proportional hazards assumption:",
                         "no PH:") |>
             str_replace("max advised nb of predictors", "max advised preds")
    ) |> 
    mutate(messages = map(messages, ~ str_split(str_wrap(., 50, exdent  = 2), "\n"))) |>
    unnest(messages) |> 
    unnest(messages) |> 
    group_by(event, model_name, split_var) |>
    mutate(rn1 = row_number() == 1) |> 
    ungroup() |>
    mutate(across(any_of(c("event", "model_name","split_var")), 
                  ~ if_else(rn1, as.character(.), "."))
    ) |> 
    select(-type, -rn1)
  
  if (all(model_messages$split_var == "")) model_messages <- model_messages |> select(-split_var)
  
  if (nrow(model_messages) > 0) print(model_messages, n = 500)
  
  error_models <- map_lgl(models_vars$models, is.null)
  if(any(error_models)) {
    
    error_models_names <- paste(models_vars$event, models_vars$name, models_vars$split_var) |>
      str_squish()
    error_models_names <- error_models_names[error_models]
    warning(paste0("the following models had an error and were removed: ", 
                   paste0(error_models_names, collapse = ", ")))
    
    models_vars <- models_vars |> filter(!error_models)
  } 
  
  models_vars
}





# from survival:::plot.cox.zph
plot.cox.zph_extract_spline <- 
  function (x, resid = FALSE, se = TRUE, df = 4, nsmo = 40, var, 
            xlab = "Time", ylab = "", lty = 1:2, col = 1, 
            lwd = 1, hr = FALSE, ...) {
    xx <- x$x
    yy <- x$y
    df <- max(df)
    nvar <- ncol(yy)
    #pred.x <- seq(from = min(xx), to = max(xx), length = nsmo)
    pred.time <- seq.int(0, max(x$time), by = 1)
    pred.x <- pred.time / max(x$time) *max(xx)
    
    #  pred.x <- seq_along()
    temp <- c(pred.x, xx)
    lmat <- splines::ns(temp, df = df, intercept = TRUE)
    pmat <- lmat[1:length(pred.x), ]
    xmat <- lmat[-(1:length(pred.x)), ]
    
    var <- 1:nvar
    ylab <- paste("Beta(t) for", dimnames(yy)[[2]])
    
    #else if (x$transform != "identity") {
    xtime <- x$time
    indx <- !duplicated(xx)
    apr1 <- approx(xx[indx], xtime[indx], seq(min(xx), max(xx), 
                                              length = 17)[2 * (1:8)])
    temp <- signif(apr1$y, 2)
    apr2 <- approx(xtime[indx], xx[indx], temp)
    xaxisval <- apr2$y
    xaxislab <- rep("", 8)
    for (i in 1:8) xaxislab[i] <- format(temp[i])
    #}
    col <- rep(col, length = 2)
    lwd <- rep(lwd, length = 2)
    lty <- rep(lty, length = 2)
    
    res_list <- list_along(var)
    for (i in var) {
      y <- yy[, i]
      keep <- !is.na(y)
      if (!all(keep))  y <- y[keep]
      qmat <- qr(xmat[keep, ])
      if (qmat$rank < df) {
        warning("spline fit is singular, variable ", 
                i, " skipped")
        next
      }
      yhat <- pmat %*% qr.coef(qmat, y)
      #if (resid) yr <- range(yhat, y)
      #else
      yr <- range(yhat)
      if (se) {
        bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
        xtx <- bk %*% t(bk)
        seval <- ((pmat %*% xtx) * pmat) %*% rep(1, df)
        temp <- 2 * sqrt(x$var[i, i] * seval)
        yup <- yhat + temp
        ylow <- yhat - temp
        yr <- range(yr, yup, ylow)
      }
      # on veut : yhat, yup, ylow
      
      res_list[[i]] <- 
        tibble(
          predictor   = dimnames(yy)[[2]][i], 
          time        = pred.time, 
          spline      = yhat[, 1],
          spline.high = yup[, 1], 
          spline.low  = ylow[, 1]
        )
      
      
      # doxaxis <- TRUE
      #       #if (!hr) {
      # # if (x$transform == "identity") 
      # #   plot(range(xx), yr, type = "n", xlab = xlab, 
      # #        ylab = ylab[i])
      # # else if (x$transform == "log") 
      # #   plot(range(xx[keep]), yr, type = "n", xlab = xlab, 
      # #        ylab = ylab[i], log = "x")
      # # else {
      # if (doxaxis) {
      #   plot(range(xx[keep]), yr, type = "n", 
      #        xlab = xlab, ylab = ylab[i], xaxt = "n")
      #   axis(1, xaxisval, xaxislab)
      # } else plot(range(xx[keep]), yr, type = "n", 
      #              xlab = xlab, ylab = ylab[i])
      # #}
      # # if (resid)  points(xx[keep], y)
      #  lines(pred.x, yhat, lty = lty[1], col = col[1], lwd = lwd[1])
      # if (se) {
      #   lines(pred.x, yup, col = col[2], lty = lty[2], 
      #         lwd = lwd[2])
      #   lines(pred.x, ylow, col = col[2], lty = lty[2], 
      #         lwd = lwd[2])
      # }
      
      # }
      # else {
      #   if (x$transform == "identity") 
      #     plot(range(xx), exp(yr), type = "n", xlab = xlab, 
      #          ylab = ylab[i], log = "y", ...)
      #   else if (x$transform == "log") 
      #     plot(range(xx[keep]), exp(yr), type = "n", 
      #          xlab = xlab, ylab = ylab[i], log = "xy", 
      #          ...)
      #   else {
      #     if (doxaxis) {
      #       plot(range(xx[keep]), exp(yr), type = "n", 
      #            xlab = xlab, ylab = ylab[i], log = "y", 
      #            xaxt = "n", ...)
      #       axis(1, xaxisval, xaxislab)
      #     }
      #     else plot(range(xx[keep]), exp(yr), type = "n", 
      #               xlab = xlab, ylab = ylab[i], log = "y", 
      #               ...)
      #   }
      #   if (resid) 
      #     points(xx[keep], exp(y))
      #   lines(pred.x, exp(yhat), lty = lty[1], col = col[1], 
      #         lwd = lwd[1])
      #   if (se) {
      #     lines(pred.x, exp(yup), col = col[2], lty = lty[2], 
      #           lwd = lwd[2])
      #     lines(pred.x, exp(ylow), col = col[2], lty = lty[2], 
      #           lwd = lwd[2])
      #   }
      # }
    }
    
    bind_rows(res_list)
  }





eha_table_print_splines <- function(eha_table, zph.p_max = 0.05, exp = TRUE, ...) {
  data_spline <- eha_table |> 
    select(predictor, level, spline, spline.high, spline.low, zph.p) |> 
    filter(zph.p < zph.p_max) |>
    mutate(across(
      c(spline, spline.high, spline.low),
      ~ map(str_split(., ","), ~ as.double(str_squish(.)))
    ), 
    time = map(spline,~  1:length(.))
    ) |>
    unnest(c(spline, spline.high, spline.low, time))
  
  if (exp) data_spline <- data_spline |> 
      mutate(across(c(spline, spline.high, spline.low),
                    ~ exp(.)))
  data_spline |> 
    ggplot(aes(x = time, y = spline, ymax = spline.high, ymin = spline.low)) +
    #geom_line() + 
    geom_hline(yintercept = if(exp) {1} else{0}, 
               color = "black") + 
    geom_smooth(stat = "identity") +
    scale_y_log10() + 
    facet_wrap(c("predictor", "level"), ... = ...)
}




zero_event_cat_to_other <- function(data, 
                                    eha_other_cats = eha_other_cats_promo) {
  
  
  factor_vars <- map_lgl(data, is.factor)
  factor_vars <- names(factor_vars)[factor_vars]
  factor_vars <- set_names(factor_vars, factor_vars)
  
  if(length(factor_vars) > 0) {
    
    setDT(data)
    
    tabs_no_event <- map(factor_vars, 
                         ~ data[!is.na(eval(str2expression(.))), 
                                .(event = sum(event, na.rm = TRUE)), 
                                keyby = .]) |> 
      imap_dfr(~ .x[event == 0, ][, predictor := .y] |>
                 rename(any_of(c(set_names(.y, "level"))))
      ) |> 
      as_tibble() |> select(any_of(c("predictor", "level")) )
    #print(tabs_no_event)
    no_event_vars <- tabs_no_event$predictor |> unique() |> sort()
    
    if (length(no_event_vars) > 0) {
      #if (predictor %in% names(no_event_vars)) {
      recode_to_other <- tabs_no_event |> 
        mutate(new_level = eha_other_cats[as.character(predictor)],
               recode_vector = set_names(as.character(level), new_level)) |>
        filter(!is.na(new_level)) |>
        group_split(predictor) %>%
        set_names(map_chr(., ~ as.character(first(pull(., "predictor"))))) |>
        map(~ pull(., recode_vector))
      
      #recode_to_other <- tabs_no_event |> 
      #  mutate(new_level = eha_other_cats[as.character(predictor)],
      #         recode_vector = set_names(as.character(level), 
      #                                   if_else(!is.na(new_level), new_level, "NULL" ))) |>
      #  group_split(predictor) %>%
      #  set_names(map_chr(., ~ as.character(first(pull(., "predictor"))))) |>
      #  map(~ pull(., recode_vector))
      
      recode_to_other <- recode_to_other[!is.na(names(recode_to_other))]
      
      if (length(recode_to_other) > 0) {
        for(i in 1:length(recode_to_other)) {
          vars_recode <- names(recode_to_other)[i]
          VARS_RECODE <- str2expression(vars_recode)
          
          data[, eval(vars_recode) := fct_recode(eval(VARS_RECODE), !!!(recode_to_other[[i]]))]
        }
        #}
      }
    }
  }
  
  data
}




# thanks to hadley and others on stack overflow (modified for dt)
constant_cols <- function(df, grp) {
  matching_breaks <- function(group, col) {
    !any(col & !group)
  }
  n <- nrow(df)
  vars <- names(df)
  changed <- function(x) c(TRUE, x[-1] != x[-n])
  df <- select(df, all_of(grp), everything())
  
  if (is.data.table(df)) {
    
    if (is.unsorted(pull(df, grp))) setorderv(df, grp)
    #changes <- lapply(df, changed)
    df[, eval(vars) := map(.SD, changed), .SDcols = vars]
    res <- vapply(df[, -1], matching_breaks, group = df[[1]],
                  FUN.VALUE = logical(1))
    
  } else {
    if (is.unsorted(df[, grp])) df <- df[order(df[, grp]), ]
    changes <- lapply(df, changed)
    res <- vapply(changes[-1], matching_breaks, group = changes[[1]],
                  FUN.VALUE = logical(1))
  }
  names(res)[res]
}






# Network analysis ----
transition_matrix <- function(X, prob = FALSE, wide = FALSE, na_to = NULL) { # Time is in columns (like in seqs)
  #X <- data.frame(replicate(20, sample(c("A", "B", "C", "D"), size = 100, replace = TRUE)))
  #X <- mutate(X, across(.fns = as.factor))
  # tt <- table( c( X[, -ncol(X)]), c(X[,-1]) )
  
  X <- data.table::as.data.table(X)
  
  suppressWarnings({
    X1 <- forcats::fct_c(!!!X[, -ncol(X), with = FALSE]) |> fct_recode("*" = "%")
    X2 <- forcats::fct_c(!!!X[, -1, with = FALSE]) |> fct_recode("*" = "%")
  })
  
  tt <- data.table::data.table(X1, X2)
  
  tt <- tt[, .N, keyby = eval(c("X1", "X2"))]
  
  if (wide) {
    tt <- data.table::dcast(tt, formula = X1 ~ X2, value.var = "N", fill = 0, 
                            drop = FALSE)
    
    if (prob) tt <- tt/rowSums(tt)
    
    tt <- tibble::as_tibble(tt) |> rename(from = X1)
    
    if (length(na_to) == 0) {
      tt <- tt |> 
        select(-any_of(c("*", "%"))) |> 
        filter(!from %in% c("*", "%"))
    } else{
      tt <- tt |> mutate(across(c(from, to), ~ suppressWarnings(fct_recode(., !!!set_names("*", na_to) ))))
    }
    
    #column_to_rownames(var = "X1") |> 
    #  as.matrix()
  } else {
    tt <- tibble::as_tibble(tt) |> rename(from = X1, to = X2)
    
    if (length(na_to) == 0) {
      tt <- tt |> filter(!from %in% c("*", "%") & !to %in% c("*", "%"))
    } else{
      tt <- tt |> mutate(across(c(from, to), ~ suppressWarnings(fct_recode(., !!!set_names("*", na_to)  ))))
    }
    
  }
  tt
}



# data <- data_4D
# grouping <- c(groups, var1, var2)
agregate_rows <- function(data, grouping, n_vars, mean_vars, wn = NULL, 
                          keep_all = FALSE){
  
  data <- data.table::as.data.table(data)
  
  sd_vars <- paste0(mean_vars, "_sd") |> set_names(mean_vars)
  sd_vars <- sd_vars[sd_vars %in% names(data)]
  means_for_sd_vars <- names(sd_vars)
  
  if (length(wn) > 0) {
    data <- data |> rename(all_of(set_names(wn, "wn")))
    n_vars <- c(n_vars[!n_vars == wn], "wn")
  }
  
  if (keep_all) {
    data[, to_agregate := .N, by = c(grouping)]
    data <- data[to_agregate >= 2, ]
    data[, to_agregate := NULL]
  }
  
  suppressWarnings({
    sum_data_n_mean <- data[
      , 
      .(map(.SD[, ..n_vars],    ~ sum(., na.rm = TRUE)), 
        map(.SD[, ..mean_vars], ~ weighted.mean(., wn, na.rm = TRUE)) #, 
      ) |> flatten(), 
      .SDcols = c(n_vars, mean_vars),
      keyby = c(grouping) # var_agreg, var_fixed
    ]
  })
  
  # NaN en NA
  sum_data_n_mean[
    ,
    c(n_vars, mean_vars) := map(.SD, ~ ifelse(is.nan(.), NA, .)),
    .SDcols = c(n_vars, mean_vars)
  ]
  
  if (length(sd_vars) > 0) {
    suppressWarnings({
      sum_data_sd <- data[
        , 
        map2(.SD[, ..sd_vars], .SD[, ..means_for_sd_vars],
             ~ sqrt( ( ( sum( (.x^2) * wn, na.rm = TRUE) + 
                           sum( (.y^2) * wn, na.rm = TRUE)  ) /
                         sum(wn, na.rm = TRUE) 
             ) -
               (weighted.mean(.y, wn, na.rm = TRUE)^2) )
        ), 
        .SDcols = c(sd_vars, means_for_sd_vars),
        keyby = c(grouping) # var_agreg, var_fixed
      ]
    })
    # # Approximation de l'écart-type d'une moyenne de moyennes
    # # (si les mesures sont indépendantes, ce qui bien sûr n'est pas le cas)
    # # (source : https://math.stakexchange/questions/2190772, szw1710)
    # 
    # # Mean of means
    # mean_of_means = weighted.mean(mean, wn)
    # 
    # # Sd of a mean of means
    # ( sum(sd)^2 * sum(wn) + sum(means^2) * sum(wn) )/sum(wn) - mean_of_means^2
    
    # NaN en NA
    sum_data_sd[
      ,
      c(sd_vars) := map(.SD, ~ ifelse(is.nan(.), NA, .)),
      .SDcols = c(sd_vars)
    ]
    
    dup_vars <- names(sum_data_sd)[names(sum_data_sd) %in% names(sum_data_n_mean)]
    sum_data_sd[, paste0(dup_vars) := NULL]
    sum_data <- cbind(sum_data_n_mean, sum_data_sd) # fill = TRUE
    #  print(names(sum_data))
    
    # # Visualise what happens
    # data |> 
    #   arrange(!!!syms(grouping)) |> 
    #   select(from = CORPS_CONT_4_from, to = CORPS_CONT_4_to, 
    #          n, wn, 
    #          S_from = PSMC_SM_NETR_from, Sf_sd = PSMC_SM_NETR_from_sd
    #   ) |> 
    #   as_tibble() |> print(n = 20)
    # 
    # sum_data |> arrange(!!!syms(grouping)) |> 
    #   select(from = CORPS_CONT_4_from, to = CORPS_CONT_4_to, n, wn, 
    #                    S_from = PSMC_SM_NETR_from, Sf_sd = PSMC_SM_NETR_from_sd ) |> 
    #   as_tibble() |> print(n = 10)
    
  } else {
    sum_data <- sum_data_n_mean
  }
  
  if (length(wn) > 0 & wn != "wn") {
    sum_data <- sum_data |> rename(all_of(set_names("wn", wn) ))
  }
  
  sum_data
}


# data <- 
#   pan_any_in_CORPS[CORPS4_any == "A2E00-Administrateur civil" & ANNEE4 == "2000-2009", 
#                    .(NNINOUV, ANNEE, CORPS_CONT_4, # CORPS_plus_4, CORPS4,
#                      POND_trans4, AGE, PSMC_SM_NETR, SEXE)]

# data <- pan_contrat
# nodes_id = "CORPS_CONTRAT_3"
# nodes_group = NULL
# time = "ANNEE"
# time_group = "ANNEE4"
# id = "NNINOUV"
# time_panel_entry = "ENTPAN2"
# false_panel_entry = 2002
# join_missing_max_time = 1L
# wt = "POND_trans4"
# num_vars = c("AGE", "SM_NETR", "TIT_TIME", "CONTRAT_E_CDI",
#              "CONTRAT_E_CDD", "Nbis_P_nb")
# vars_levels_list = NULL #list(SEXE = "2-Femme"),
# skip_transition_if = "WZDZ-INDT"
# true_exit_condition = c(AGE = 60)
# true_exit = "EXIT > 60 ANS"
# na_enter = "ENTRY"
# na_exit = "EXIT"


# data <- pan_any_in_CORPS
# nodes_id = "CORPS_CONT_4"
# time = "ANNEE"
# nodes_group <- "CORPS4_any"
# time_group <- "ANNEE4"
# time_panel_entry  <- "ENTPAN"
# false_panel_entry = 2002
# num_vars = c("AGE", "PSMC_SM_NETR") # "CORPS_plus_4", "CORPS4"
# vars_levels_list = list(SEXE = "2-Femme")
# id = "NNINOUV"
# na_enter = "ENTRY"
# na_exit = "EXIT"
# wt = "POND_trans4"
# skip_transition_if <- "WZDZ-INDT"
# join_missing_max_time = 4L
# true_exit_condition = c(AGE = 60)
# true_exit = "EXIT > 60 ANS"
# 
# transition_infos(pan_any_in_CORPS,  "CORPS_CONT_4", nodes_group = "CORPS4_any", 
#                  time = "ANNEE", time_group = "ANNEE4", id = "NNINOUV",
#                  time_panel_entry = "ENTPAN", false_panel_entry = 2002, 
#                  join_missing_max_time = 4L, wt = "POND_trans4", 
#                  num_vars = c("AGE", "PSMC_SM_NETR"),
#                  vars_levels_list = list(SEXE = "2-Femme"),
#                  skip_transition_if = "WZDZ-INDT", 
#                  true_exit_condition = c(AGE = 60), true_exit = "EXIT > 60 ANS"
#                  )


# problem (in all my functions) : id is not always character, can be numeric
transition_infos <- function(data, nodes_id, nodes_group = NULL, 
                             time, time_group = NULL, lead = NULL,
                             time_panel_entry = NULL, false_panel_entry = NULL,
                             #min_weight = 1,
                             id = "NNINOUV", 
                             vars_levels_list = NULL, 
                             num_vars = NULL, n_min = 1L, 
                             na_enter = "ENTRY", na_exit = "EXIT",
                             true_exit_condition = NULL, true_exit = "TRUE EXIT",
                             wt = NULL, join_missing_max_time = 2L,
                             skip_transition_if = NULL, 
                             memory_problem = FALSE #,
                             #na_to = NULL, na_to_last_value = FALSE,
                             #majo_vars = NULL, majo_vars_seuil = 0.9
) {
  #X <- data.frame(replicate(20, sample(c("A", "B", "C", "D"), size = 100, replace = TRUE)))
  #X <- mutate(X, across(.fns = as.factor))
  # tt <- table( c( X[, -ncol(X)]), c(X[,-1]) )
  groups <- c(nodes_group, time_group)
  
  data <- data |> 
    select(any_of(groups), 
           all_of(c(nodes_id, time = time, num_vars, panel_entry = time_panel_entry,
                    names(vars_levels_list), id = id )), 
           any_of(c(wt = wt))
    ) |>
    data.table::as.data.table()
  
  
  
  # if (length(groups) == 0) {
  #   setorder(data, id, time)
  # } else {
  setorderv(data, cols = c(groups, "id", "time"))
  # }
  
  # On enlève les NA a time, pour être sur que ceux qui apparaissent sont 
  #  des entrées ou des sorties du panel. 
  if (! memory_problem) data <- data[!is.na(time), ]
  
  vars <- c(groups, nodes_id, num_vars, names(vars_levels_list))
  vars_no_groups <- c(nodes_id, num_vars, names(vars_levels_list))
  vars_from     <- paste0(vars_no_groups, "_from")
  vars_to       <- paste0(vars_no_groups, "_to")
  vars_from_to  <- c(vars_from, vars_to)
  nodes_id_from <- paste0(nodes_id, "_from")
  nodes_id_to   <- paste0(nodes_id, "_to")
  num_vars_from_to <- c(paste0(num_vars, "_from"), paste0(num_vars, "_to"))
  
  
  
  
  if (length(lead) == 0) {
    # On ne prend pas l'année + 1, mais l'année qui suit 
    #  (il peut y avoir des années qui manquent pour tout le monde, 
    #  pour lesquelles on prends les transitions sur 2 ans)
    if (length(time_group) == 0) {
      times <- data$time |> unique() |> sort()
      time_interval <- max(times) - min(times)
      
      times_plus_1 <- set_names(times[-1], times[-length(times)])
      data[, time2 := times_plus_1[as.character(time)] ]
      
    } else {
      times <- data[, .(time = sort(unique(time))), by = time_group] |>
        group_by(!!sym(time_group)) |> group_split() %>% 
        set_names(map_chr(., ~ as.character(first(pull(., time_group))))) |>
        map(~ pull(., time))
      time_interval <- map(times, ~ max(.) - min(.))
      times_plus_1 <- map(times, ~ set_names(.[-1], .[-length(.)]))
      
      
      data[, time2 := map2_int(times_plus_1[as.character(eval(str2expression(time_group)))], time,
                               ~ .x[as.character(.y)]) 
      ]
    }
    
    # If lead >= 1, we assume the user want to control the exact time : 
    # on prend l'année + lead même si elle n'existe pas (=> EXIT)
  } else {
    
    if (length(time_group) == 0) {
      times <- data$time |> unique() |> sort()
      time_interval <- max(times) - min(times)
      if (lead > time_interval ) stop("lead is equal or more than time interval")
      data[, time2 := time + as.integer(lead) ]
      data[, time2 := if_else(time2 %in% times, time2, NA_integer_)]
      
    } else {
      times <- data[, .(time = sort(unique(time))), by = time_group] |>
        group_by(!!sym(time_group)) |> group_split() %>% 
        set_names(map_chr(., ~ as.character(first(pull(., time_group))))) |>
        map(~ pull(., time))
      time_interval <- map(times, ~ max(.) - min(.))
      
      if (any( map_lgl(time_interval, ~ lead > .) )) stop("lead is equal or more than time interval in at least one time_group")
      data[, time2 := time + as.integer(lead)]
      data[, times_ok := times[as.character(eval(str2expression(time_group)))]]
      data[, time2 := if_else(map2_lgl(time2, times_ok, ~ .x %in% .y), 
                              time2, NA_integer_)]
      data[, times_ok := NULL]
      # data |> select(time, times_ok ) |> as_tibble() |> samp(40)
      # data$times_ok |> sample(10)
      
      ## test
      # data[, .(time2 = sort(unique(time2))), by = time_group] |>
      #  group_by(!!sym(time_group)) |> group_split() %>% 
      #  set_names(map_chr(., ~ as.character(first(pull(., time_group))))) |>
      #  map(~ pull(., time2))
      
    }
    
    
  }
  
  
  data_from  <- data |> 
    select(any_of(groups), id, time, time2, any_of(c("wt","panel_entry")),
           all_of(set_names(vars_no_groups, paste0(vars_no_groups, "_from"))) 
    )
  data_from <- data_from[!is.na(time2), ]
  
  data_to <- data |> 
    select(any_of(groups), id, time2 = time, any_of(c("wt2" = "wt")),
           all_of(set_names(vars_no_groups, paste0(vars_no_groups, "_to"))) 
    )
  
  remove(data)
  
  if (length(lead) == 0) {
    if (length(time_group) == 0) {
      data_to <- data_to[time2 != first(times), ]
      data_to[, matched := TRUE]
      
    } else {
      first_times <- map_int(times, first)
      data_to[, time_dep := first_times[as.character(eval(str2expression(time_group)))]]
      data_to <- data_to[time2 != time_dep]
      data_to[, time_dep := NULL]
      data_to[, matched := TRUE]
    }
    
  } else { # if lead >= 1
    if (length(time_group) == 0) {
      data_to <- data_to[time2 != first(times), ]
      data_to[, matched := TRUE]
      
    } else {
      first_times <- map(times, ~ .[. < min(.) + lead])
      data_to[, times_first := first_times[as.character(eval(str2expression(time_group)))]]
      data_to[, time2 := if_else(map2_lgl(time2, times_first, ~ ! .x %in% .y), 
                                 time2, NA_integer_)]
      data_to[, times_first := NULL]
      data_to[, matched := TRUE]
      
      # # test
      # data_to[, .(time2 = sort(unique(time2))), by = time_group] |>
      #   group_by(!!sym(time_group)) |> group_split() %>% 
      #   set_names(map_chr(., ~ as.character(first(pull(., time_group))))) |>
      #   map(~ pull(., time2))
      
    }
  }
  
  
  # Full join in data.table
  data_res <- rbind(data_to[data_from, on = c(groups, "id", "time2")], 
                    data_to[!data_from, on = c(groups, "id", "time2")], 
                    fill = TRUE
  )
  remove(data_to, data_from)
  
  data_res <- data_res |> 
    select(any_of(c(groups)), id, time, time2, matched, #gap, post_gap, 
           any_of(c("wt", "wt2", "panel_entry")),
           all_of(c(nodes_id_from, nodes_id_to, vars_from, vars_to)))
  
  #if (length(groups) == 0) {
  #  setorder(data_res, id, time2)
  #} else {
  setorderv(data_res, cols = c(groups, "id", "time2"))
  #}
  
  
  # Entrées et sorties
  #     Entrée
  data_res[
    , 
    eval(nodes_id_from) := if_else(
      !is.na(time), 
      true  = fct_expand(eval(str2expression(nodes_id_from)), na_enter ), 
      false = factor(na_enter) )
  ]
  
  #     Première entrée dans le panel (ENPTAN dans PTS)
  # (il faut ajouter une ligne l'année précédent l'entrée : pour cela
  #  on copie puis modifie la ligne d'entrée)
  if (length(time_panel_entry) > 0) {
    
    if (length(time_group) > 0) {
      data_res[, time_not_in_range := (time - 1L) < min(time, na.rm = TRUE), 
               by = c(time_group)]
      
    } else {
      data_res[, time_not_in_range := (time - 1L) < min(time, na.rm = TRUE) ]
    }
    
    data_panel_entries <- 
      select(data_res, any_of(c(groups)), id, time, time2, any_of("wt"),
             panel_entry, all_of(c(nodes_id_from)), time_not_in_range) |> 
      rename(any_of(c(set_names(nodes_id_from, nodes_id_to))))
    
    
    data_panel_entries[, time_not_in_range := replace_na(time_not_in_range, FALSE)]
    data_panel_entries <- 
      data_panel_entries[time == panel_entry & !time_not_in_range, ]
    data_panel_entries[, time := time - 1L]
    data_panel_entries[, time2 := time2 - 1L]
    data_panel_entries[, time_not_in_range := NULL]
    data_panel_entries[, matched := TRUE] # Otherwise -> "EXIT"
    
    
    if (length(false_panel_entry) == 0) {
      data_panel_entries[, eval(nodes_id_from) := factor("PANEL ENTRY")]
      
      # Année de "fausse première entrée", lorsque des individus sont ajoutés 
      #  en milieu de carrière (2002 dans PTS)
    } else {
      data_panel_entries[
        ,  
        eval(nodes_id_from) := if_else(panel_entry %in% false_panel_entry, 
                                       true  = factor( "FALSE PANEL ENTRY", 
                                                       levels = c("PANEL ENTRY", 
                                                                  "FALSE PANEL ENTRY")), 
                                       false = factor("PANEL ENTRY")
        )
      ] 
    }
    
    data_res[, time_not_in_range := NULL]
    data_res <- rbind(data_res, data_panel_entries, fill = TRUE)
    
    #if (length(groups) == 0) {
    #  setorder(data_res, id, time2)
    #} else {
    setorderv(data_res, cols = c(groups, "id", "time2"))
    # }
  }
  
  # # Visualiser ce qui se passe
  # cat("\14") ; data_res |>
  #   select(any_of(c("id", "time", "time2")), everything()) |>
  #   filter(CORPS4_any == "A3E10-Attaché admin État" & ANNEE4 == "1999-2008") |>
  #   filter(panel_entry %in% c(2002:2003)) |> # False entry
  #   #filter(panel_entry %in% c(2000, 2001, 2003:2008)) |> # Real panel entries
  #   #filter(is.na(time)) |>
  #   #filter(is.na(time) & time2 == panel_entry) |>
  #   rename(any_of(c(Sf = "PSMC_SM_NETR_from", St = "PSMC_SM_NETR_to",
  #                   ENT = "panel_entry" , Af = "AGE_from", At = "AGE_to",
  #                   SXf = "SEXE_from", SXt = "SEXE_to"
  #   ))) |> as_tibble() |>
  #   select(-any_of(c("At", "St", "matched", "SXt", "wt2")), -all_of(groups)) |>  # "wt"
  #   mutate(across(any_of(c("CORPS4_any", "CORPS_CONT_4_to",  "CORPS_CONT_4_from")),
  #                 ~ fct_relabel(., ~ str_remove_all(., tabxplor:::cleannames_condition()))
  #   ),
  #   id = as.integer(as.factor(id))
  #   ) |>
  #   new_tab() |> group_by(id) |>
  #   print(n = 400) 
  
  # cat("\14") ; data_res |>
  #   select(ANNEE4, id, time, time2, from = CORPS_CONTRAT_3_from, to = CORPS_CONTRAT_3_to, 
  #           Sf = SM_NETR_from, St = SM_NETR_to, TIT_TIME_to, 
  #          CDD = CONTRAT_E_CDD_to, panel_entry) |>
  #   #filter(panel_entry %in% c(2002:2003)) |> # False entry
  #   filter(panel_entry %in% c(2000, 2001, 2003:2008)) |> # Real panel entries
  #   #filter(ANNEE4 %in% c("2010-2019")) |> 
  #   mutate(id = as.integer(as.factor(id))) |>
  #   new_tab() |> group_by(id) |>  print(n = 400)
  
  
  #     Sortie
  if (length(true_exit_condition) == 0) {
    data_res[
      , 
      eval(nodes_id_to) := if_else(
        !is.na(matched), 
        true  = fct_expand(eval(str2expression(nodes_id_to)), na_exit ), 
        false = factor(na_exit) )
    ]
    
    # true_exit : par exemple retraite ; "EXIT > 60 ANS"
  } else {
    true_exit_var <- paste0(names(true_exit_condition), "_from")
    
    data_res[, true_exit_logical := eval(str2expression(true_exit_var)) >= true_exit_condition]
    data_res[, true_exit_logical := replace_na(true_exit_logical, FALSE)]
    
    
    data_res[
      , 
      eval(nodes_id_to) := case_when(
        !is.na(matched)    ~ fct_expand(eval(str2expression(nodes_id_to)), na_exit, true_exit ), 
        true_exit_logical  ~ factor(true_exit),
        TRUE               ~ factor(na_exit) 
      )
    ]
    
    data_res[, true_exit_logical := NULL]
  }
  
  
  
  
  # S'il n'y a qu'une année manquante, remplir le gap (plutôt que d'avoir
  #  une sortie puis une entrée l'année d'après).
  # (join_missing_max_time : nombre maximal d'années d'écart)
  # (sauf si lead >= 2, puisqu'on travaille sur l'année exacte)
  if (join_missing_max_time >= 2L & length(lead) > 0) {
    data_res[, new_ind := id != lag(id, 1L, default = "N00")]
    data_res[, gap := is.na(matched) & is.na(lead(time, default = 0L)) &
               !lead(new_ind, default = TRUE) & 
               lead(time2) - time <= join_missing_max_time
    ]
    data_res[, post_gap := lag(gap, default = FALSE) ]
    
    data_res[
      , 
      c("time2", vars_to) := map(.SD, ~ if_else(gap, lead(.), .)), 
      .SDcols = c("time2", vars_to)
    ]
    data_res <- data_res[post_gap == FALSE, ]
    data_res[, gap := NULL][, post_gap := NULL]
  }
  
  # Si nodes_id prend certaines valeurs (comme "INDT"), on saute 
  #  la ligne et on prend la transition suivante, mais une fois seulement
  # (sauf si la transition est plus éloignée que "join_missing_max_time")
  if (length(skip_transition_if) > 0) {
    if (!"new_ind" %in% names(data_res)) data_res[, new_ind := id != lag(id, 1L, default = "N00")]
    
    data_res[
      ,
      skip := !lead(new_ind, default = TRUE) & 
        eval(str2expression(nodes_id_to)) %in% skip_transition_if &
        lead(time2) - time <= join_missing_max_time
    ]
    data_res[, post_skip := lag(skip, default = FALSE) ]
    
    data_res[
      , 
      c("time2", vars_to) := map(.SD, ~ if_else(skip, lead(.), .)), 
      .SDcols = c("time2", vars_to)
    ]
    data_res <- data_res[post_skip == FALSE, ]
    data_res[, skip := NULL][, post_skip := NULL]
    
  }
  
  data_res[, matched := NULL]
  if ("wt2" %in% names(data_res)) {
    data_res[, wt := if_else(!is.na(wt), wt, wt2)]
    data_res[, wt2 := NULL]
  }
  
  
  
  
  # Pourcentages sur les variables catégorielles :
  # (faire une variable logique pour chaque catégorie)
  if (length(vars_levels_list) > 0) {
    vars_levels_list_flat <-  vars_levels_list |>
      imap_dfr(~ tibble(var = .y, cat = .x)) |>
      mutate(vect = set_names(cat, var)) |> pull(vect)
    
    vars_levels_list_from_to_flat <- 
      c(set_names(vars_levels_list_flat, paste0(names(vars_levels_list_flat), "_from")), 
        set_names(vars_levels_list_flat, paste0(names(vars_levels_list_flat), "_to"))
      )
    
    # vars_levels_list_from_to <- 
    #   c(set_names(vars_levels_list, paste0(names(vars_levels_list), "_from") ),
    #     set_names(vars_levels_list, paste0(names(vars_levels_list), "_to") )
    #     
    #   )
    # 
    # var_names <- vars_levels_list_from_to |> 
    #   imap(~paste0(str_remove_all(.y, "_from$|_to$"), "_",
    #                str_remove_all(.x, tabxplor:::cleannames_condition()), 
    #                str_extract(.y, "_from$|_to$")  ))
    
    #print(vars_levels_list_from_to_flat)
    
    var_names <- paste0(
      str_remove_all(names(vars_levels_list_from_to_flat), "_from$|_to$"), "_",
      str_remove_all(vars_levels_list_from_to_flat, tabxplor:::cleannames_condition()), 
      str_extract(names(vars_levels_list_from_to_flat), "_from$|_to$") 
    ) |> set_names(names(vars_levels_list_from_to_flat))
    
    for(i in 1:length(vars_levels_list_from_to_flat)) {
      var <- names(vars_levels_list_from_to_flat)[i]
      cat <- vars_levels_list_from_to_flat[i]
      var_name <- var_names[i]
      
      #print(paste0("cat:", cat, "; var_name:", var_name))
      
      data_res[, eval(var_name) := as.integer(eval(str2expression(var)) == cat) ]
    }
    
  } else {
    var_names <- NULL
  }
  
  
  # Summarise data to create transition table
  if (!"wt" %in% names(data_res)) {
    
    if (! memory_problem) {
      transition_table <- data_res[
        , 
        .(n  = .N, 
          map(.SD, ~ mean(., na.rm = TRUE)),
          map(.SD[, ..num_vars_from_to], ~ sd(., na.rm = FALSE)) |> 
            set_names(paste0(num_vars_from_to, "_sd"))
        ) |> flatten(), 
        .SDcols = c(num_vars_from_to, var_names),
        keyby = c(groups, nodes_id_from, nodes_id_to)
      ]
      
    } else {
      transitions_n <- data_res[
        , 
        .(n  = .N),
        keyby = c(groups, nodes_id_from, nodes_id_to)
      ]
      
      
      transitions_means <- data_res[
        , 
        map(.SD, ~ mean(., na.rm = TRUE)), 
        .SDcols = c(num_vars_from_to, var_names),
        keyby = c(groups, nodes_id_from, nodes_id_to)
      ]
      
      transitions_sds <- data_res[
        , 
        map(.SD, ~ sd(., na.rm = FALSE)) |> 
          set_names(paste0(num_vars_from_to, "_sd")), 
        .SDcols = c(num_vars_from_to),
        keyby = c(groups, nodes_id_from, nodes_id_to)
      ]
      
      transition_table <- transitions_n |> 
        left_join(transitions_means, by = c(groups, nodes_id_from, nodes_id_to))
      
      transition_table <- transition_table |> 
        left_join(transitions_sds, by = c(groups, nodes_id_from, nodes_id_to))
      
    }
    
    # weighted
  } else {
    transition_table <- data_res[
      , 
      .(n  = .N, 
        wn = sum(wt, na.rm = TRUE),
        map(.SD, ~ weighted.mean(., wt, na.rm = TRUE)), 
        map(.SD[, ..num_vars_from_to], 
            ~ sqrt(tabxplor:::weighted.var(., wt, na.rm = TRUE))
        ) |> 
          set_names(paste0(num_vars_from_to, "_sd"))
      ) |> flatten(), 
      .SDcols = c(num_vars_from_to, var_names),
      keyby = c(groups, nodes_id_from, nodes_id_to)
    ]
  }
  
  remove(data_res)
  
  # NaN en NA
  if (!memory_problem) {
    transition_table[
      , 
      c(num_vars_from_to, paste0(num_vars_from_to, "_sd"), 
        var_names) := map(.SD, ~ ifelse(is.nan(.), NA, .)), 
      .SDcols = c(num_vars_from_to, paste0(num_vars_from_to, "_sd"), 
                  var_names) 
    ]
    
  } else {
    vars <- c(num_vars_from_to, paste0(num_vars_from_to, "_sd"), 
              var_names) 
    
    for(i in 1:length(vars)) {
      VARS <- str2expression(vars[i])
      
      transition_table[, eval(vars[i]) := ifelse(is.nan(eval(VARS)), NA, eval(VARS)) ]
    }
  }
  
  # Enlever les transitions à effectifs trop faibles
  if (n_min > 1) transition_table <- transition_table[n >= n_min, ]
  
  
  # Ordre lisible
  transition_table[, n_from := sum(n, na.rm = TRUE), by = c(groups, nodes_id_from)]
  setorderv(transition_table, 
            cols = c(groups, "n_from", "n"), 
            order = c(rep(1, length(groups)), -1, -1), 
            na.last = TRUE
  )
  transition_table[, n_from := NULL]
  if (length(lead) > 0) transition_table[, time_step := as.integer(lead)]
  
  if (length(time_group) == 0) {
    transition_table[, time_interval := time_interval]
  } else {
    transition_table[, time_interval := time_interval[as.character(eval(str2expression(time_group)))]]
    
  }
  
  
  
  # Fausses entrées en 2003 aussi ("Sous-dir AC" !) ? 
  
  
  
  
  # # as_tibble(data_res)
  # cat("\14") ; transition_table |> #data_res |>
  #   select(any_of(c("id", "time", "time2")), everything()) |>
  #   #filter(CORPS4_any == "A2E00-Administrateur civil" & ANNEE4 == "1999-2008") |>
  #   #filter(panel_entry %in% c(2000:2008)) |> # False entry
  #   #filter(panel_entry %in% c(2003:2008)) |> # Real panel entries
  #   #filter(is.na(time)) |>
  #   #filter(is.na(time) & time2 == panel_entry) |>
  #   select(-starts_with(c("wt", "matched")), -all_of(groups)) |>
  #   rename(any_of(c(Sf = "PSMC_SM_NETR_from", St = "PSMC_SM_NETR_to",
  #                   ENT = "panel_entry" , Af = "AGE_from", At = "AGE_to",
  #                   SXf = "SEXE_Femme_from",  SXt = "SEXE_Femme_to",
  #                   Afsd = "AGE_from_sd", Sfsd =  "PSMC_SM_NETR_from_sd",
  #                   Atsd = "AGE_to_sd", Stsd = "PSMC_SM_NETR_to_sd" #,
  #   ))) |> as_tibble() |>
  #   mutate(across(any_of(c("CORPS4_any", "CORPS_CONT_4_to",  "CORPS_CONT_4_from")),
  #                 ~ fct_relabel(., ~ str_remove_all(., tabxplor:::cleannames_condition()))
  #   ),
  #   #id = as.integer(as.factor(id))
  #   ) |>
  #   # new_tab() |> group_by(id) |> arrange(.by_group = TRUE) |>
  #   group_by(CORPS_CONT_4_from) |>
  #   mutate(n_from = sum(n, na.rm = TRUE)) |>
  #   new_tab() |> group_by(CORPS_CONT_4_from) |>
  #   arrange(-n_from, -n) |>
  #   print(n = 400) #samp(40)
  
  #cat("\14") ; transition_table |>
  #   select(ANNEE4, CORPS_CONTRAT_3_from, CORPS_CONTRAT_3_to, n, wn, 
  #          AGE_from, AGE_to, SM_NETR_from, SM_NETR_to, TIT_TIME_to, 
  #          CONTRAT_E_CDD_to) |>
  #   filter(ANNEE4 %in% c("2010-2019")) |> 
  #   group_by(CORPS_CONTRAT_3_from) |>
  #   mutate(n_from = sum(n, na.rm = TRUE)) |>
  #   new_tab() |> group_by(CORPS_CONTRAT_3_from) |>
  #   arrange(-n_from, -n) |>
  #   print(n = 400)
  
  # - Faut-il passer en NA quand l'individu est immobile ?
  # - Ou faut-il garder comme ça, supprimer les immobiles dans la base finale, 
  #  et agréger les infos par nodes_from ou nodes_to (pondéré) pour reconstituer 
  #  les moyennes (ce qui permet de n'exporter qu'une base edges, et de 
  #    reconstituer une base nodes à partir de là, en choisissant les 
  #    lignes qu'on garde)
  
  return(transition_table)
}




# data = CORPS_CONT_4_transition_table
# n_min = 5L
# var1 = "CORPS_CONT_4_to"
# var2 = "CORPS_CONT_4_from"
# groups = c("CORPS4_any", "ANNEE4")
# nomenc = nomenc_final_with_CONT
# nomenc_vars = c(`4` = "CORPS3", `3` = "CORPS2", `2` = "CORPS1")
# n_vars = c("n", "wn")
# mean_vars = c("AGE_from", "PSMC_SM_NETR_from",
#               "AGE_to"  , "PSMC_SM_NETR_to",
#               "SEXE_Femme")
# 
# other = "AUTRES CORPS OU CONT"
# exceptions = ""
# groups_in_exceptions = FALSE


# On regroupe les petites catégories automatiquement, 
#  en prenant un chiffre de moins sur le numéro de catégorie, 
#  puis en allant chercher le nom dans la nomenclature. 
regroup_CORPS_outward <- 
  function(data, n_min = 5L, var1, var2 = NULL, groups = NULL, 
           nomenc = nomenc_final_with_CONT, 
           nomenc_vars = c(`4` = "CORPS3", `3` = "CORPS2", `2` = "CORPS1"), 
           n_vars = c("n", "wn"), 
           mean_vars = c("AGE_from", "PSMC_SM_NETR_from",   
                         "AGE_to"  , "PSMC_SM_NETR_to", 
                         "SEXE_Femme"
           ), 
           exceptions = "", group1_in_exceptions = FALSE,
           other = "OTHER"
           
  ) {
    
    ##    done in agregate_rows
    # sd_vars <- paste0(mean_vars, "_sd") |> set_names(mean_vars)
    # sd_vars <- sd_vars[sd_vars %in% names(data)]
    # means_for_sd_vars <- names(sd_vars)
    
    data <- data[n < n_min, ]
    
    if (length(groups) == 0) group1_in_exceptions <- FALSE
    
    #names_var <- "CORPS3"
    #n_numbers <- 4L
    recode_from_nomenc <- function(dataX, var, n_numbers, names_var) {
      #print(nomenc)
      # print(var) 
      # print(names_var)
      recode_nomenc <- nomenc |> 
        select(all_of(c(set_names(names_var, "variable"))) ) |> 
        distinct(variable) |> 
        mutate(rec = set_names(str_remove(variable, "-.+$"), variable)) |>
        filter(!is.na(rec) & !is.na(names(rec)) &
                 !rec == "" & !names(rec) == "") |> 
        pull(rec)
      
      recode_nomenc[!names(recode_nomenc) %in% exceptions]
      
      #   Add the current main group in exceptions (if it has the same 
      #    levels than var1 and/or var2 )
      if (group1_in_exceptions) {
        dataX[
          , 
          paste0(eval(var), ".temp") := if_else(
            eval(str2expression(var)) == as.character(eval(str2expression(groups[1]))), 
            true  = as.factor(eval(str2expression(var))), 
            false = factor(NA_character_)
          ) 
        ]
        
        dataX[, eval(var) := fct_recode(
          fct_relabel(eval(str2expression(var)),
                      ~ if_else(!. %in% exceptions, 
                                true  = str_sub(., 1L, n_numbers), 
                                false = .
                      )
          ), 
          !!!recode_nomenc)
        ] 
        
        dataX[, eval(var) := if_else(
          !is.na( eval(str2expression(paste0(eval(var), ".temp"))) ), 
          true  = fct_expand(eval(str2expression(paste0(eval(var), ".temp"))), 
                             levels(eval(str2expression(var)))
          ), 
          false = eval(str2expression(var))
        )
        ] 
        
        dataX[, paste0(eval(var), ".temp") := NULL]
        
        
      } else {
        dataX[, eval(var) := fct_recode(
          fct_relabel(eval(str2expression(var)),
                      ~ if_else(!. %in% exceptions, 
                                true  = str_sub(., 1L, n_numbers), 
                                false = .
                      )
          ), 
          !!!recode_nomenc)
        ] 
      }
      
    }
    
    
    data_small <- data |> copy()
    data_ok <- data |> slice(0)
    # nb_of_digits <- names(nomenc_vars)[3]
    for(nb_of_digits in names(nomenc_vars) ) {
      data_small <- recode_from_nomenc(data_small, var1, 
                                       names_var = nomenc_vars[nb_of_digits], 
                                       n_numbers = nb_of_digits ) 
      
      if ( "wn" %in% n_vars ) {
        data_new_rows <- agregate_rows(data_small, grouping = c(groups, var1, var2), 
                                       n_vars = n_vars, mean_vars = mean_vars, wn = "wn"
        )
        
      } else { 
        data_new_rows <- agregate_rows(data_small, grouping = c(groups, var1, var2), 
                                       n_vars = n_vars, mean_vars = mean_vars, wn = "n"
        )
        names(data_new_rows)
      }
      
      
      data_ok <- rbind(data_ok, data_new_rows[n >= n_min, ], fill = TRUE)
      data_new_rows <- data_new_rows[n < n_min, ]
      
      
      data_small <- data_small[!data_ok, on = c(groups, var1, var2)] 
      
      data_small <-  rbind(
        data_small[!data_new_rows, on = c(groups, var1, var2)], 
        data_new_rows, 
        fill = TRUE
      )
      
      
      
      if (length(var2) > 0) {
        data_small <- recode_from_nomenc(data, var2, 
                                         names_var = nomenc_vars[nb_of_digits], 
                                         n_numbers = nb_of_digits )
        
        if ("wn" %in% n_vars ) {
          data_new_rows <- agregate_rows(data_small, grouping = c(groups, var1, var2), 
                                         n_vars = n_vars, mean_vars = mean_vars, wn = "wn")
        } else {
          data_new_rows <- agregate_rows(data_small, grouping = c(groups, var1, var2), 
                                         n_vars = n_vars, mean_vars = mean_vars, wn = "n")
          
        }
        
        data_ok       <- rbind(data_ok, data_new_rows[n >= n_min, ], fill = TRUE)
        data_new_rows <- data_new_rows[n < n_min, ]
        
        data_small <- data_small[!data_ok, on = c(groups, var1, var2)] 
        
        data_small <-  rbind(
          data_small[!data_new_rows, on = c(groups, var1, var2)], 
          data_new_rows, 
          fill = TRUE
        )
        
      }
      
      
    }
    
    data_small[, eval(var1) := factor(other)]
    
    if ("wn" %in% n_vars ) {
      data_new_rows <- agregate_rows(data_small, grouping = c(groups, var1, var2), 
                                     n_vars = n_vars, mean_vars = mean_vars, wn = "wn")
    } else {
      data_new_rows <- agregate_rows(data_small, grouping = c(groups, var1, var2), 
                                     n_vars = n_vars, mean_vars = mean_vars, wn = "n")
      
    }
    
    data_ok       <- rbind(data_ok, data_new_rows[n >= n_min, ], fill = TRUE)
    data_new_rows <- data_new_rows[n < n_min, ]
    
    data_small <- data_small[!data_ok, on = c(groups, var1, var2)] 
    
    data_small <-  rbind(
      data_small[!data_new_rows, on = c(groups, var1, var2)], 
      data_new_rows, 
      fill = TRUE
    )
    
    if (length(var2) > 0) data_small[, eval(var2) := factor(other)]
    
    if ("wn" %in% n_vars ) {
      data_new_rows <- agregate_rows(data_small, grouping = c(groups, var1, var2), 
                                     n_vars = n_vars, mean_vars = mean_vars, wn = "wn")
    } else {
      data_new_rows <- agregate_rows(data_small, grouping = c(groups, var1, var2), 
                                     n_vars = n_vars, mean_vars = mean_vars, wn = "n")
      
    }
    
    data_ok       <- rbind(data_ok, data_new_rows[n >= n_min, ], fill = TRUE)
    data_new_rows <- data_new_rows[n < n_min, ]
    
    data_small <- data_small[!data_ok, on = c(groups, var1, var2)] 
    
    data_small <-  rbind(
      data_small[!data_new_rows, on = c(groups, var1, var2)], 
      data_new_rows, 
      fill = TRUE
    )
    
    # Visualiser le résultat
    # data_ok |> 
    #   arrange(!!!syms(groups), CORPS_CONT_4_from, -n) |> 
    #   select(from = CORPS_CONT_4_from, to = CORPS_CONT_4_to, 
    #          n, wn, 
    #          S_from = PSMC_SM_NETR_from, Sf_sd = PSMC_SM_NETR_from_sd
    #   ) |> 
    #   as_tibble() |> print(n = 40)
    # 
    # data_small |> 
    #   arrange(!!!syms(groups), CORPS_CONT_4_from, -n) |> 
    #   select(from = CORPS_CONT_4_from, to = CORPS_CONT_4_to, 
    #          n, wn, 
    #          S_from = PSMC_SM_NETR_from, Sf_sd = PSMC_SM_NETR_from_sd
    #   ) |> 
    #   as_tibble() |> print(n = 40)
    
    if (nrow(data_small) > 0) {
      message(paste0(nrow(data_small), 
                     " rows were removed altogether, still being less than ", 
                     n_min,
                     " individuals after all mergings"))
    } else { 
      message("all rows were kept, after merging in categories with more than ",
              n_min, " individuals")
    }
    
    if (length(var2) > 0) {
      data_ok[, n_var2 := sum(n, na.rm = TRUE), by = c(groups, var2)]
      setorderv(data_ok, 
                cols = c(groups, "n_var2", "n"), 
                order = c(rep(1, length(groups)), -1, -1), 
                na.last = TRUE
      )
      data_ok[, n_var2 := NULL]
      
    } else {
      setorderv(data_ok, 
                cols = c(groups, "n"), 
                order = c(rep(1, length(groups)), -1), 
                na.last = TRUE
      )
    }
    
    
    data_ok
  }





transition_table <- function(data, nodes_id, id = "NNINOUV",
                             nodes_group = NULL, time_group = NULL, 
                             n_min = 5L, regroup_CORPS = TRUE,
                             time_panel_entry = "ENTPAN2",
                             false_panel_entry = 2002, 
                             join_missing_max_time = 6L,
                             skip_transition_if = "WZDZ-INDT", 
                             nomenc = nomenc_final_with_CONT, 
                             nomenc_vars = c(`4` = "CORPS3", `3` = "CORPS2", `2` = "CORPS1"),
                             num_vars = c("AGE", "SM_NETR"),
                             vars_levels_list = list(SEXE = "2-Femme"),
                             other = "AUTRES CORPS OU CONT", 
                             group1_in_exceptions = TRUE, 
                             wt = NULL, memory_problem = FALSE
) {
  
  nodes_id_from <- paste0(nodes_id, "_from")
  nodes_id_to <- paste0(nodes_id, "_to")
  
  tt <- data |> 
    transition_infos(nodes_id = nodes_id, nodes_group = nodes_group, 
                     time = "ANNEE", time_group = time_group, id = id,
                     time_panel_entry = time_panel_entry, false_panel_entry = false_panel_entry, 
                     join_missing_max_time = join_missing_max_time, 
                     wt = wt, memory_problem = memory_problem,
                     num_vars = num_vars,
                     vars_levels_list = vars_levels_list,
                     skip_transition_if = skip_transition_if, 
                     true_exit_condition = c(AGE = 60), true_exit = "EXIT > 60 ANS"
    )
  
  if (!is.null(vars_levels_list) & "SEXE" %in% names(vars_levels_list)) {
    tt[, SEXE_Femme := if_else(!is.na(SEXE_Femme_from), 
                               SEXE_Femme_from, 
                               SEXE_Femme_to)
    ]
    tt[, SEXE_Femme_from := NULL]
    tt[, SEXE_Femme_to   := NULL] 
  }
  
  num_vars_from_to <- c(paste0(num_vars, "_from"), paste0(num_vars, "_to"))
  
  if (length(vars_levels_list) > 0) {
    vars_levels_list_flat <-  vars_levels_list |>
      imap_dfr(~ tibble(var = .y, cat = .x)) |>
      mutate(vect = set_names(cat, var)) |> pull(vect)
    
    var_names <- paste0(
      names(vars_levels_list_flat), "_",
      str_remove_all(vars_levels_list_flat, tabxplor:::cleannames_condition()) #, 
    ) |> set_names(names(vars_levels_list_flat))
    
    not_in_tt <- var_names[!var_names %in% names(tt)] 
    if (length(not_in_tt) > 0){
      not_in_tt_from_to <- 
        c(set_names(not_in_tt, paste0(names(not_in_tt), "_from")), 
          set_names(not_in_tt, paste0(names(not_in_tt), "_to"))
        )
      var_names <- c(var_names[var_names %in% names(tt)], not_in_tt_from_to)
    }
  } else { 
    var_names <- NULL}
  
  
  
  # On regroupe les petites catégories automatiquement (sauf la catégorie principale), 
  #  en prenant un chiffre de moins sur le numéro de catégorie, 
  #  puis en allant chercher le nom dans la nomenclature. 
  
  exceptions <- unique(c(levels(pull(tt, nodes_id_from)), 
                         levels(pull(tt, nodes_id_to))
  ))
  #exceptions <- exceptions[!str_detect(exceptions, "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-")]
  exceptions <- exceptions[!str_detect(exceptions, "^[^-]+-")]
  
  if (regroup_CORPS) {
    
    if ( length(wt) > 0 ) {
      tt_new_rows <- tt |>
        regroup_CORPS_outward(n_min = n_min, 
                              var1 = nodes_id_to,
                              var2 = nodes_id_from,
                              groups = c(nodes_group, time_group),
                              nomenc = nomenc,
                              nomenc_vars = nomenc_vars,
                              n_vars = c("n", "wn"),
                              mean_vars = c(num_vars_from_to, var_names),
                              other = other, 
                              exceptions = exceptions, 
                              group1_in_exceptions = TRUE
        )
      
    } else {
      
      tt_new_rows <- tt |>
        regroup_CORPS_outward(n_min = n_min, 
                              var1 = nodes_id_to,
                              var2 = nodes_id_from,
                              groups = c(nodes_group, time_group),
                              nomenc = nomenc,
                              nomenc_vars = nomenc_vars,
                              n_vars = c("n"),
                              mean_vars = c(num_vars_from_to, var_names),
                              other = other, 
                              exceptions = exceptions, 
                              group1_in_exceptions = TRUE
        )
      
    }
    # cat("\14") ;tt_new_rows |>
    #   arrange(CORPS4_any, ANNEE4, CORPS_CONT_4_from, -n) |>
    #   select(CORPS4_any, ANNEE4,
    #     from = CORPS_CONT_4_from, to = CORPS_CONT_4_to,
    #          n, wn,
    #          S_from = SM_NETR_from, Sf_sd = SM_NETR_from_sd
    #   ) |>
    #   mutate(across(any_of(c("from", "to",  nodes_group)),
    #                 ~ fct_relabel(., ~ str_remove_all(., tabxplor:::cleannames_condition()))
    #   ) ) |>
    #   new_tab() |> group_by(CORPS4_any, ANNEE4) |> print(n = 500)
    
    
    tt <- 
      rbind(
        tt[n >= n_min, ], 
        tt_new_rows, 
        fill = TRUE
      )
  } 
  
  else {
    nb_remove <- tt[n < n_min, .N]
    message(paste(nb_remove, " rows with less than ", n_min,
                  "individuals were removed"))
    
    tt <- tt[n >= n_min]
  }
  
  
  
  # Remettre l'ordre
  tt[
    ,
    n_from := sum(n, na.rm = TRUE), 
    by = c(nodes_group, time_group, nodes_id_from)
  ]
  setorderv(tt, 
            cols = c(nodes_group, time_group, "n_from", "n"), 
            order = c(rep(1, length(c(nodes_group, time_group))), -1, -1), 
            na.last = TRUE
  )
  tt[, n_from := NULL]
  
}






siasp_network_prep <- function(siasp, corps_prefix, nodes_id, nodes_names,
                               no_loops = TRUE, min_weight = 1,
                               id_pan = "ID_PAN",  ID_PAN_select, 
                               vars_levels_list = NULL, 
                               num_vars = NULL, 
                               na_to = NULL, na_to_last_value = FALSE,
                               majo_vars = NULL, majo_vars_seuil = 0.9) {
  # Informations agrégées niveau nodes_id (SIREN, SIRET, etc.)
  if (id_pan != "ID_PAN") {
    siasp <- siasp |>
      rename(any_of(c("ID_PAN_SAVE_" = "ID_PAN"))) |>
      rename(all_of(c("ID_PAN" = id_pan)))
  }
  
  
  siasp_nodes <- siasp 
  
  if (!missing(corps_prefix)) {
    siasp_nodes <- siasp_nodes |> filter(!!sym(corps_prefix)) 
  }
  
  if (length(num_vars) != 0) {
    siasp_nodes_num <-  siasp_nodes |> 
      select(!!sym(nodes_id), all_of(num_vars)) |>
      group_by(!!sym(nodes_id)) |>
      summarise(across(all_of(num_vars), ~ mean(., na.rm = TRUE)), .groups = "drop") #|>
    #rename(!!!set_names(num_vars, paste0(nodes_id, "_", num_vars)))
  } else {
    siasp_nodes_num <- siasp_nodes |> select(all_of(nodes_id)) |> slice(0)
  }
  
  if (length(majo_vars) != 0) {
    siasp_nodes_majo <- map(majo_vars, 
                            ~ tab_majo(siasp_nodes, !!sym(nodes_id), !!sym(.x),
                                       seuil = majo_vars_seuil)
    ) |> 
      reduce(~ left_join(.x, .y, by = nodes_id))
  } else {
    siasp_nodes_majo <- siasp_nodes |> select(all_of(nodes_id)) |> slice(0)
  }
  
  if (length(vars_levels_list) != 0) {
    siasp_nodes_frequencies <- siasp_nodes |> 
      summarise_factors(nodes_id, name = "", vars_levels_list = vars_levels_list)
  } else {
    siasp_nodes_frequencies <- siasp_nodes |> select(all_of(nodes_id)) |> slice(0)
  }
  
  siasp_nodes <- siasp_nodes_num  |> 
    full_join(siasp_nodes_majo, by = nodes_id) |> 
    full_join(siasp_nodes_frequencies, by = nodes_id) |>
    rename_with(~ str_remove(., "^_"))
  
  # Network analysis
  if (!missing(corps_prefix) & !missing(ID_PAN_select)) {
    siasp_lg_nodes <-  siasp |> 
      select(ID_PAN, ANNEE, all_of(nodes_id), all_of(c(corps_prefix, ID_PAN_select))) |>
      filter(!!sym(corps_prefix) & ID_PAN %in% !!sym(ID_PAN_select)) 
  } else if (!missing(corps_prefix)) {
    siasp_lg_nodes <-  siasp |> 
      select(ID_PAN, ANNEE, all_of(nodes_id), all_of(corps_prefix)) |>
      filter(!!sym(corps_prefix)) 
  } else if (!missing(ID_PAN_select)) {
    siasp_lg_nodes <-  siasp |> 
      select(ID_PAN, ANNEE, all_of(nodes_id), all_of(ID_PAN_select)) |>
      filter(ID_PAN %in% !!sym(ID_PAN_select)) 
  } else {
    siasp_lg_nodes <- siasp
  }
  
  siasp_lg_nodes <-  siasp_lg_nodes |> 
    select(ID_PAN, ANNEE, all_of(nodes_id)) |> 
    as.data.table()
  
  if (na_to_last_value) {
    siasp_lg_nodes <- dcast(siasp_lg_nodes, ANNEE ~ ID_PAN, value.var = nodes_id)
    siasp_lg_nodes <- siasp_lg_nodes |> fill(-ANNEE, .direction = "down")
    siasp_lg_nodes <- melt(siasp_lg_nodes, id.vars = "ANNEE",
                           measure.vars = names(siasp_lg_nodes)[!names(siasp_lg_nodes) == "ANNEE"], 
                           value.name = nodes_id)
    siasp_lg_nodes <- siasp_lg_nodes |> rename("ID_PAN" = "variable")
    
    # siasp_lg_nodes <- dcast(siasp_lg_nodes, ID_PAN  ~ ANNEE, value.var = nodes_id)
    # siasp_lg_nodes |> as_tibble()
    
    # pan_lg_sample <- pan_lg_GRADE |> samp(40)
    #   pan_lg_sample <- pan_lg_sample |> 
    #     pivot_longer(cols = -ID_PAN,
    #                  names_to = "ANNEE", 
    #                  values_to = "GRADE")
    #   pan_lg_sample <- pan_lg_sample |> as.data.table()
    #   
    #   pan_lg_sample <- dcast(pan_lg_sample, ANNEE ~ ID_PAN, value.var = "GRADE")
    # 
    #   pan_lg_sample <- pan_lg_sample |> fill(-ANNEE, .direction = "down")
    #   
    #   
    #   pan_lg_sample <- melt(pan_lg_sample, id.vars = "ANNEE",
    #        measure.vars = names(pan_lg_sample)[!names(pan_lg_sample) == "ANNEE"], 
    #        value.name = "GRADE")
    #   
    #   pan_lg_sample <- pan_lg_sample |> rename("ID_PAN" = "variable")
    #   
    #   pan_lg_sample <- dcast(pan_lg_sample, ID_PAN  ~ ANNEE, value.var = "GRADE")
    #   
    #   pan_lg_sample |> as_tibble()
    
  }
  
  siasp_lg_nodes <- dcast(siasp_lg_nodes, ID_PAN ~ ANNEE, value.var = nodes_id) |> 
    as_tibble()
  
  if (na_to_last_value) {
    siasp_lg_nodes <- siasp_lg_nodes |> 
      pivot_longer(cols = -ID_PAN,
                   names_to = "ANNEE", 
                   values_to = nodes_id) |> 
      pivot_wider(names_from = "ID_PAN", values_from = nodes_id) |>
      fill(-ANNEE, .direction = "down") |>
      pivot_longer(cols = -ANNEE, names_to = "ID_PAN", values_to = nodes_id) |>
      pivot_wider(names_from = "ANNEE", values_from = nodes_id)
  }
  
  transition_mat <- siasp_lg_nodes |> 
    select(-ID_PAN) |>
    mutate(across(everything(), ~fct_explicit_na(as.factor(.), "*")  )) |>
    transition_matrix(na_to = na_to) |>
    rename(weight = N) #|>
  #filter(as.character(from) != as.character(to)) # On supprime la diagonale
  # rownames_to_column(as.data.frame(transition_mat), var = "X1") |> as_tibble()
  
  #corps_network <-  graph_from_data_frame(transition_mat, directed = TRUE, vertices = nodes)
  
  corps_network <- as_tbl_graph(transition_mat, directed = TRUE)
  
  corps_network <- corps_network |> activate(edges)
  if(no_loops) corps_network <- corps_network |> filter(!edge_is_loop())
  if(min_weight > 1) corps_network <- corps_network |> filter(weight >= min_weight)
  
  corps_network <- corps_network |> activate(nodes) |>
    filter(!node_is_isolated() & !is.na(name))
  
  if (!missing(nodes_names)) {
    corps_network <- corps_network |>
      left_join(
        distinct(
          select(siasp, all_of(c(name = nodes_id, complete_name = nodes_names))),
          name,
          .keep_all = TRUE
        ) |>
          mutate(name = as.factor(name) ), 
        by = "name"
      )
  }
  
  corps_network <- corps_network |>
    left_join(mutate(siasp_nodes, across(all_of(nodes_id), as.character)), 
              by = c("name" = nodes_id))
  
  corps_network
}








recode_before_CORPS_NH_mobility <- function(siasp) {
  suppressWarnings({
    siasp <- siasp |> 
      mutate(across(
        any_of(c("CORPS_NH", "CORPS_NH_open")), 
        ~ fct_relabel(., ~ case_when(
          str_detect(., "Contractuel|Vacataire|Études recherche autre|autres FPT|Apprenti")  ~ NA_character_, 
          str_detect(., "Secrétaires admin")                   ~ "B1Aa-Secrétaires administratifs",
          str_detect(., "Adjoints admin")                      ~ "CZAa-Adjoints administratifs" ,
          str_detect(., "Adjoints tech")                       ~ "CZAb-Adjoints techniques État" ,
          str_detect(., "^APG")                                ~ "APGz-Enseignants",
          TRUE                                                 ~ .
        )) |> 
          fct_recode("A2Ia-Ingénieurs Ponts, Eaux et Forets" = "A2Ia-Ingénieurs Ponts EF",
                     
                     "A2Aa2-Magistrats TA CRC"  = "A2Aa2-Magistrats TA CAA",  
                     "A2Aa2-Magistrats TA CRC"  = "APAk-Magistrats CRC", 
                     
                     "A2Aa3-Magist ordre jud"   =  "APDa-Magistrats ordre jud", 
                     "A2Ta-Direction FPT" = "A2Ta-Administrateurs FPT", 
                     "A2Ta-Direction FPT" = "A2Ta-Cadres direction FPT EF",
                     "A2Ta-Direction FPT" = "A1Ta-Cadres direction FPT EF", 
                     
                     "A1Al-Sous-directeur AC" =  "A1Fb-Sous-directeur AE", 
                     "A1Aj-Chef de service AC" =  "A1Fa-Chef de service AE", 
                     
                     "A3Cf-Resp unité locale PN EF" =  "A2Cd-Resp unité locale PN EF", # 3000 euros, mobilités depuis policiers
                     "A2If-Ingé des études Eco"  = "APIh-Ingé des études Eco" # 6000 euros, mobilité directeurs techniques
          ), 
      ))
  })
  siasp
}
# cd("^A2I")     ; cd("Resp unité", data = pan)






# data <- pan
# nodes_id = "CORPS_NH_open"
# condition = expr(str_detect(CORPS_NH_open, "Attachés administration"))
# years = 2010:2019
# min_weight = 5L
# panel_wt = 12L
# num_vars = c("AGE", "PSMC_SM_NET")
# vars_levels_list = list(SEXE = "2-Femme")


pan_net_data <- function(data, nodes_id = "CORPS_NH_open", 
                         condition, no_network = FALSE,
                         years = 2010:2019, min_weight = 5L, panel_wt = 12L,
                         num_vars = c("AGE", "PSMC_SM_NET"), 
                         vars_levels_list = list(SEXE = "2-Femme") ) {
  
  there_is_condition <- !missing(condition)
  if (there_is_condition) {
    condition <- enquo(condition)
    data <- data |> mutate(net_champ = !!condition) 
  }
  
  # condition <- expr(str_detect(CORPS_NH, "Personnels direction EN")) # Cadre de santé
  
  data <- data |>
    select(NNINOUV, ANNEE, all_of(nodes_id), any_of(c("net_champ")),
           all_of(c(num_vars, names(vars_levels_list)))) |> 
    filter(as.integer(as.character(ANNEE)) %in% years)
  
  if (there_is_condition) {
    data <- data |> 
      mutate(rien = FALSE) |>
      siasp_ID_any_in_corps(ID_corps_vars_list = c("net_champ", "rien"), 
                            id_pan = "NNINOUV") 
    # On ajoute la variable "rien" sinon problème de noms base longit (`2010`, `2011`)
    
    data <- data |> filter(net_champ)
  }
  
  if(!no_network) {
    data <- data |> 
      recode_before_CORPS_NH_mobility() |>
      siasp_network_prep(
        id_pan = "NNINOUV", nodes_id = nodes_id, 
        min_weight = min_weight, num_vars = num_vars, vars_levels_list = vars_levels_list, 
      )
    
    if (length(panel_wt) != 0) {
      data <- data |>
        activate(edges) |> 
        mutate(weight = weight * panel_wt)
    }
  }
  
  data
}




pan_ID_any <- function(data, condition, ID = "NNINOUV") {
  
  if (missing(condition)) stop("condition is empty")
  
  condition <- enquo(condition)
  data <- data |> mutate(champ = !!condition) 
  # condition <- expr(str_detect(CORPS_NH, "Personnels direction EN")) # Cadre de santé
  
  data <- data |> 
    select(NNINOUV, ANNEE, "champ") |> 
    mutate(rien = FALSE) |>
    siasp_ID_any_in_corps(ID_corps_vars_list = c("champ", "rien"), 
                          id_pan = ID) 
  # On ajoute la variable "rien" sinon problème de noms base longit (`2010`, `2011`)
  
  data <- data |> filter(champ)
  
  unique(data$NNINOUV)
}










edges_with_nodes_info <- function(data) {
  nodes <- data |> activate(nodes) |> as_tibble()
  edges <- data |> activate(edges) |> as_tibble()
  
  edges |> 
    mutate(from = nodes$name[from], to = nodes$name[to]) |>
    left_join(rename_with(nodes, ~paste0(., "_from"), .cols = -name), 
              by = c("from" = "name") ) |> 
    left_join(rename_with(nodes, ~paste0(., "_to"), .cols = -name), 
              by = c("to" = "name") ) |>
    mutate(across(c(from, to), as.factor))
}





geom_node_point_interactive <- function(...) {
  ggiraph:::layer_interactive(ggraph::geom_node_point, ...)
}


geom_node_label_interactive <- function(...) {
  ggiraph:::layer_interactive(ggraph::geom_node_label, ...)
}






geom_edge_link_interactive <- function(...) {
  ggiraph:::layer_interactive(ggraph::geom_edge_link, ...)
}









# CORPS_NH_net_base <- mobility_data[[2]]
# min_weight = 30
# nb_years = 9
# layers = CJ_layers
# layer_labels = NULL # CJ_layer_labels
# nb_char_layer = 1L
# 
# num_vars = NULL
# remove_patterns = NULL
# type = "labels"
# nudge_y = 0
# text_size = 3
# nb_size = 3
# label_vjust = 0
# stroke_divide = 500
# full_width = NULL
# add_empty_layer = NULL
# repel = TRUE
# xlim_plus = 0.25
# seed = 333

# min_weight = 80
# nb_years = 9
# add_empty_layer = "A2"
# full_width = c("A1", "A2", "A3")
# num_vars = NULL
# type = c("labels", "points")
# nudge_y = 0
# text_size = 3
# nb_size = 3
# label_vjust = 0
# stroke_divide = 500
# repel = TRUE
# xlim_plus = 0.25
# seed = 333
# 
# remove_patterns = NULL
# repel <- FALSE
# 
# layers <- NULL

# layers = c("A0" = 1L,   # Direction politique"
#            "A1" = 1L,   # Cadre dirigeant"    
#            "A2" = 1L,   # Cadre supérieur"    
#            "A3" = 2L,   # Cadre intermédiaire"
#            "AP" = 3L,   # Profession A"       
#            "B1" = 4L,   # CBI"                
#            "BP" = 4L,   # Profession B"       
#            "CZ" = 5L )


gg_mobility_trans <- function(CORPS_NH_net_base, #corps_prefix,
                              min_weight = 80, nb_years = 8L,
                              num_vars = NULL,     
                              remove_patterns = NULL,
                              type = c("labels", "points"),
                              nudge_y = 0, y_flip = FALSE,
                              text_size = 3, nb_size = 3, 
                              edge_width = c(0.2, 1.5),
                              hfan_strength = 8,
                              label_vjust = 0,
                              stroke_divide = 500, 
                              full_width = NULL, constant_space = TRUE,
                              add_empty_layer = NULL,
                              repel = TRUE, xlim_plus = 0.25, ylim_plus = 0.1,
                              seed = 333, 
                              layers = c("A0" = 1L,   # Direction politique"
                                         "A1" = 1L,   # Cadre dirigeant"    
                                         "A2" = 1L,   # Cadre supérieur"    
                                         "A3" = 2L,   # Cadre intermédiaire"
                                         "AP" = 3L,   # Profession A"       
                                         "B1" = 4L,   # CBI"                
                                         "BP" = 4L,   # Profession B"       
                                         "CZ" = 5L ), # Exécutant C"  
                              layer_labels =  c("A0" = "A+",      # "A0" = 1L
                                                "A1" = "A+",      # "A1" = 1L
                                                "A2" = "A+",      # "A2" = 1L
                                                "A3" = "A" ,      # "A3" = 2L
                                                "AP" = "A" ,      # "AP" = 3L
                                                "B1" = "B" ,      # "B1" = 4L
                                                "BP" = "B" ,      # "BP" = 4L
                                                "CZ" = "C"  ),     # # "CZ" = 5L
                              nb_char_layer = 2L,
                              pattern_in_bold = "main",
                              layer_main_corps = NULL, 
                              ... 
) {
  
  if (length(num_vars) == 0) num_vars <- 
      names(as_tibble(activate(CORPS_NH_net_base, nodes)))[-1]
  
  # levels(siasp$CORPS_NH)[str_detect(levels(siasp$CORPS_NH), "Adjoint")]
  
  
  
  CORPS_NH_net <- CORPS_NH_net_base |>
    activate(edges) |> 
    filter(weight >= min_weight) |>
    mutate(h_onedir = FALSE) |>
    arrange(weight) |> # Tracer en dernier les liens forts
    activate(nodes) |>
    mutate(Cdeg  = centrality_degree(weights = weight, mode = "all"), #Nb of adjacent edges
           `in`  = centrality_degree(weights = weight, mode = "in"), # incoming nodes # local_size(order = 1L, mode = "in"),  
           out   = centrality_degree(weights = weight, mode = "out"), # outgoing nodes # local_size(order = 1L, mode = "out"), 
           #group_size = local_size(order = 100L)
    ) |> 
    #arrange(main) |>  # Tracer en dernier les corps principal
    filter(!node_is_isolated() & !is.na(name)) |> # & Cdeg >= 50 & group_size >= 10 
    mutate(number = str_extract(name, "^[^-]+"),
           name   = str_remove_all(name, tabxplor:::cleannames_condition()), 
           NH     = str_sub(number, 1L, nb_char_layer),
    )
  
  if (length(pattern_in_bold) != 0) {
    if(pattern_in_bold == "main") {
      CORPS_NH_net <- CORPS_NH_net |>
        mutate(in_bold = if_else(Cdeg == max(Cdeg, na.rm = TRUE), "bold", "plain"))
      
    } else {
      CORPS_NH_net <- CORPS_NH_net |>
        mutate(in_bold = if_else(str_detect(name, pattern_in_bold), "bold", "plain"))
    }
    
  } else {
    CORPS_NH_net <- CORPS_NH_net |>mutate(in_bold = "plain")
  }
  
  
  if(!is.null(layers)) {
    CORPS_NH_net <- CORPS_NH_net |>
      mutate(NH_int = suppressWarnings(recode(NH, !!!layers)))
    
    
    if (length(layer_main_corps) == 1 & !is.null(layers)) {
      CORPS_NH_net <- CORPS_NH_net |>
        mutate(NH_int = if_else(main, layer_main_corps, NH_int))
    }
    
    removed_corps <- is.na(as_tibble(CORPS_NH_net)$NH_int)
    if (any(removed_corps)) {
      message(paste0("Some CORPS_NH were removed (not in hierarchy): ", 
                     paste0("\"", as_tibble(CORPS_NH_net)$number[removed_corps], "-",
                            as_tibble(CORPS_NH_net)$name[removed_corps], "\" (C ", 
                            as_tibble(CORPS_NH_net)$Cdeg[removed_corps],
                            ")", collapse = ", ")))
    }
    
    
    # For horizontal edges (within layer), if there is only one direction, 
    #  add an edge on the other direction to plot better with geom_edge_fan()
    nodes_NH <- select(as_tibble(activate(CORPS_NH_net, nodes)), NH_int) |>
      mutate(rn = row_number(), NH_int = as.factor(NH_int))
    
    edges <- CORPS_NH_net |> activate(edges) |> as_tibble() |> 
      mutate(rn = row_number()) |>
      left_join(nodes_NH, by = c("from" = "rn")) |> rename(NH_from = NH_int) |>
      left_join(nodes_NH, by = c("to" = "rn")) |> rename(NH_to = NH_int) 
    
    edges_horizontal <- edges |> filter(NH_from == NH_to) 
    
    from_to <- map2(edges_horizontal$from, edges_horizontal$to, ~ c(.x, .y))
    to_from_all <- map(from_to, rev)
    only_one_dir <- 
      map_lgl(from_to, function(.fr_to)
        sum(map_lgl(to_from_all, ~ all(.fr_to == .))) == 0
      )
    
    edges_h_onedir <- edges_horizontal |>
      filter(only_one_dir) |> 
      mutate(from2 = to, to = from, from = from2, weight = 1, 
             h_onedir = TRUE) |> 
      select(from, to, weight, h_onedir)
    
    CORPS_NH_net <- CORPS_NH_net |> activate(edges) |>
      bind_edges(edges_h_onedir) |> activate(nodes)
    
    
  }
  
  if(length(remove_patterns) != 0) {
    remove_patterns <- paste0(remove_patterns, collapse = "|")
    CORPS_NH_net <- CORPS_NH_net |>
      filter(!str_detect(name, remove_patterns))
  }
  
  if (length(num_vars) != 0) {
    CORPS_NH_net <- CORPS_NH_net |> activate(nodes) |>
      mutate(across(all_of(num_vars), ~ case_when(
        cur_column() == "AGE"                           ~ str_c(round(.), "ans"), 
        str_detect(cur_column(), "_NET$|_NETR$|_BRUT$|_EQTP$") ~ str_c(round(.), "€"), 
        cur_column() == "SEXE_Femme" ~ str_c(round(.*100), "%", "f"
                                             #stringi::stri_unescape_unicode("\\u2640")
        ), 
        max(., na.rm = TRUE) <= 1          ~ str_c(cur_column(), " ", round(.*100), "%"),
        TRUE                               ~ str_c(cur_column(), " ", round(.))
      ) |> replace_na("")
      ), 
      
      name = str_c(name, "\n", str_squish(str_c(!!!syms(num_vars), sep = " ")) ) |>
        str_remove("\n +$")
      ) 
    # CORPS_NH_net |> as_tibble() |> print(n = 50)
  }
  
  
  
  
  
  # Calculate position of nodes in the graph 
  set.seed(seed)
  
  if(!is.null(layers)) {
    CORPS_NH_net_layout <- # No mutate() cause it destroys class ?
      create_layout(CORPS_NH_net, "with_sugiyama", #coords = start_matrix, #niter = 25,
                    layers = as_tibble(activate(CORPS_NH_net, nodes))$NH_int
                    #, ...
      ) 
    
  } else {
    CORPS_NH_net_layout <- create_layout(CORPS_NH_net, "with_sugiyama" #, ...
    ) 
  }
  
  # central_layer <- CORPS_NH_net_layout$y[CORPS_NH_net_layout$main] |> first()
  
  
  if (!is.null(add_empty_layer)) {
    corps_hierarchy <- select(as_tibble(CORPS_NH_net_layout), y, NH) |> 
      distinct(NH, .keep_all = TRUE)
    corps_hierarchy <- set_names(corps_hierarchy$y, corps_hierarchy$NH)
    if (is.character(add_empty_layer)) add_empty_layer <- corps_hierarchy[add_empty_layer]
    
    CORPS_NH_net_layout$y <- if_else(CORPS_NH_net_layout$y >= add_empty_layer,
                                     true = CORPS_NH_net_layout$y + 1, 
                                     false = CORPS_NH_net_layout$y)
  }
  
  
  # Take all width (in the selected layers)
  if (length(full_width != 0)) {
    
    if (is_character(full_width)) {
      corps_hierarchy <- select(as_tibble(CORPS_NH_net_layout), y, NH) |> 
        distinct(NH, .keep_all = TRUE)
      corps_hierarchy <- set_names(corps_hierarchy$y, corps_hierarchy$NH)
      full_width <- corps_hierarchy[full_width] %>% discard(is.na(.)) |> unique()
      # print(corps_hierarchy) ; print(full_width)
    } 
    
    # x_large <- reduce(
    #   full_width, .init = CORPS_NH_net_layout |> 
    #     mutate(xrange  = max(x, na.rm = TRUE) - min(x, na.rm = TRUE), 
    #            xmean   = max(x)/2 ), 
    #   .f = ~ mutate(
    #     .x, 
    #     x0      = if_else(y == .y, x, NA_real_), 
    #     x0range = max(x0, na.rm = TRUE) - min(x0, na.rm = TRUE),
    #     x       = if_else(y != .y,   x,   ((x - xmean ) * xrange / x0range) + xmean),
    #   )
    # ) |> pull(x)
    
    x_large <- reduce(
      full_width, .init = CORPS_NH_net_layout |> as_tibble() |> 
        mutate(x_tot_range  = max(x, na.rm = TRUE) - min(x, na.rm = TRUE), 
               xmean   = (max(x) + min(x))/2),
      .f = ~ mutate(
        .x,
        x0      = if_else(y == .y, x, NA_real_),
        x0_min  = min(x0, na.rm = TRUE),
        x0_max  = max(x0, na.rm = TRUE),
        x0range = x0_max - x0_min,
        x0mean  = (x0_max + x0_min)/2,
        x       = if_else(y != .y,   x,   ((x0 - x0mean) * x_tot_range / x0range) + xmean),
      )
    ) |> pull(x)
    
    # print(tibble(y = CORPS_NH_net_layout$y,
    #              x = CORPS_NH_net_layout$x, 
    #              x_large,
    # ))
    
    CORPS_NH_net_layout$x <- x_large
    # CORPS_NH_net_layout |> select(y, x, x0, xrange, x0range, xmean, x1)
    
    constant_space <- vctrs::vec_recycle(constant_space, size = length(full_width))
    constant_space <- full_width[constant_space]
    
    if (length(constant_space) != 0) {
      x_constant <-  CORPS_NH_net_layout |> as_tibble() |>
        select(x, y) |>
        mutate(row_base = row_number()) |> 
        group_by(y) |>
        arrange(x, .by_group = TRUE) |> 
        mutate(n = n(), rn = row_number(), x_max = max(x), x_min = min(x)) |> 
        ungroup() |>
        mutate(x_range = x_max - x_min, 
               steps   = if_else(n > 1, x_range/(n - 1), x_range/2), 
               x2      = if_else(y %in% constant_space, 
                                 steps * (rn - 1L) + x_min, 
                                 x)
        ) |>
        arrange(row_base) |> 
        pull(x2)
      
      # print(tibble(y = CORPS_NH_net_layout$y,
      #              x = CORPS_NH_net_layout$x,
      #              x_constant,
      # ))
      
      CORPS_NH_net_layout$x <- x_constant
    }
  }
  
  #Y axis label names
  Y_axis_label_names <- function(y) {
    if(is.null(layers)) return(rep("", length(y)))
    if(is.null(layer_labels)) return(rep("", length(y)))
    corps_hierarchy <- select(as_tibble(CORPS_NH_net_layout), y, NH) |> 
      distinct(y, .keep_all = TRUE)
    corps_hierarchy <- set_names(corps_hierarchy$NH, corps_hierarchy$y)
    
    labels <- layer_labels    
    
    labels[corps_hierarchy[as.character(y)]] |> replace_na("")
  }
  # Y_axis_label_names(c(6, 4, 3, 1))
  
  
  xrange <- range(CORPS_NH_net_layout$x)
  yrange <- range(CORPS_NH_net_layout$y) 
  
  if (y_flip)  CORPS_NH_net_layout$y <- - CORPS_NH_net_layout$y 
  
  # # not working
  # filter_edges_data <- function(keep_condition) {
  #    keep_condition <- enquo(keep_condition)
  #    
  #    function(.data) {
  #      
  #    edges <- get_edges()(.data)
  #    attt <- attributes(edges)
  #    edges <- edges  |> 
  #      mutate(across(c(x, y, xend, yend), 
  #                    ~ if_else(!!keep_condition, ., NA_real_)))
  #    
  #    #edges <- edges |> filter(!!keep_condition)
  #    
  #    edges |> `attributes<-`(attt)
  #    }
  #  }
  # # filter_edges_data(y == yend)(CORPS_NH_net_layout)
  
  
  # Plot the graph
  if (type[1] == "points") {
    
    CORPS_NH_net_graph_options <- list(
      geom_edge_fan(
        aes(width = log(weight), 
            label = round(weight/nb_years),
            #alpha = ..index..
            color = case_when(
              yend > y   ~ factor("Ascendante", levels = c("Ascendante", 
                                                           "Descendante", 
                                                           "Horizontale")),
              yend < y   ~ factor("Descendante")#,
              #yend == y  ~ factor("Horizontale"),
            )
        ), 
        strength = 0.5, 
        arrow = arrow(length = unit(2, "mm"), type = "closed"), 
        start_cap = circle(0.5), end_cap = circle(0.5), # "mm"
        angle_calc = "along", label_dodge = unit(-2.5, units = "mm"), 
        label_colour = NA, label_size = nb_size
      ), 
      geom_edge_fan(
        aes(width = log(weight), 
            label = round(weight/nb_years),
            color = case_when(yend == y ~ factor("Horizontale"))
        ), 
        strength = 4, 
        arrow = arrow(length = unit(2, "mm"), type = "closed"), 
        start_cap = circle(0.5), end_cap = circle(0.5), # "mm"
        angle_calc = "along", label_dodge = unit(2, units = "mm"), 
        label_colour = NA, label_size = nb_size
      ), 
      scale_edge_width(range = edge_width, guide = "none"),
      scale_edge_color_manual("Nombre de mobilités (moyenne annuelle)", 
                              values = c("Ascendante"  = "#d32f2f", 
                                         "Descendante" = "#0077c2", 
                                         "Horizontale" = "#388e3c")),  # "#9a0007" "#0077c2"
      scale_y_continuous(labels = Y_axis_label_names, 
                         breaks = 1:max(CORPS_NH_net_layout$y, na.rm = TRUE),
                         limits = function(lims){c(lims[1] - ylim_plus, lims[2] + nudge_y + ylim_plus)},
      ),
      #theme_minimal(),
      theme(plot.background = element_rect(fill = "white"), 
            panel.background = element_rect(fill = "white"), 
            axis.text.y = element_text(hjust = 0), # face = "bold" 
            legend.position = "bottom" #, 
      )
    )
    
    labels_plot <- if (repel) {
      list(
        ggrepel::geom_label_repel(aes(x = x, y = y, label = name, fontface = in_bold),
                                  size = text_size, 
                                  direction = "y", #alpha = 0.75,
                                  fill = rgb(1, 1, 1, alpha = 0.5), #parse = TRUE,
                                  vjust = label_vjust,
                                  # ylim = yrange, #  xlim = c(NA, NA),
                                  min.segment.length = 5, force = 0.5, force_pull = 1,
                                  point.size = 0.25, # point.padding = 0, box.padding = 0, 
                                  nudge_y = nudge_y,
        )
      )
    } else {
      list(
        geom_node_label(aes(x = x, y = y, label = name, fontface = in_bold ),
                        size = text_size, 
                        nudge_y = nudge_y, #parse = TRUE,
                        fill = rgb(1, 1, 1, alpha = 0.5),
                        vjust = label_vjust,
                        
                        # ylim = yrange, #  xlim = c(NA, NA),
        ), 
        xlim( c(xrange[1] - xlim_plus, xrange[2] + xlim_plus))
      )
    }
    
    plot_output <- 
      ggraph(CORPS_NH_net_layout) + 
      geom_node_point(aes(size = `in`/nb_years, stroke = out/stroke_divide), 
                      shape = 21, color = "black", fill = "gray"
      ) + #color = REG,
      scale_size_area(max_size = 12, guide = "none") + 
      CORPS_NH_net_graph_options + 
      labels_plot
    
    
    
    
  } else if (type[1] == "labels") {
    CORPS_NH_net_graph_options <- list(
      geom_edge_fan(
        #data = filter_edges_data(y != yend),
        aes(# y = if_else(yend == y, y + 0.4, y), 
          # yend = if_else(yend == y, yend + 0.4, yend), 
          y    = if_else(yend != y, y, NA_real_), 
          yend = if_else(yend != y, yend, NA_real_), 
          width = log(weight), 
          label = round(weight/nb_years),
          start_cap = label_rect(node1.name ), # padding = margin(0.5, 0.5, 0.5, 0.5, "mm") fontsize = text_size
          end_cap   = label_rect(node2.name ), # padding = margin(0.5, 0.5, 0.5, 0.5, "mm") fontsize = text_size
          #alpha = ..index..
          color = case_when(
            yend > y   ~ factor("Ascendante", levels = c("Ascendante", 
                                                         "Descendante", 
                                                         "Horizontale")),
            yend < y   ~ factor("Descendante"), 
            yend == y  ~ factor("Horizontale")
          )
        ), 
        strength = 0.5, 
        arrow = arrow(length = unit(2, "mm"), type = "closed"), 
        angle_calc = "along", label_dodge = unit(-2.5, units = "mm"), 
        label_colour = NA, label_size = nb_size
      ), 
      geom_edge_fan(
        #data = filter_edges_data(y == yend),
        aes(y    = if_else(yend == y & !h_onedir, y, NA_real_), 
            yend = if_else(yend == y & !h_onedir, yend, NA_real_), 
            width = log(weight),
            label = round(weight/nb_years),
            start_cap = label_rect(node1.name, cex = 0.75, padding = margin(t = 1, r = 0, b = 1.5, l = 0, "mm") ), 
            end_cap   = label_rect(node2.name, cex = 0.75, padding = margin(t = 1, r = 0, b = 1.5, l = 0, "mm") ), 
            color = factor("Horizontale"),  # if_else(!h_onedir, factor("Horizontale"), factor(NA_character_)),
        ),
        strength = hfan_strength, # position = position_nudge(y = 0.4),
        arrow = arrow(length = unit(2, "mm"), type = "closed"),
        angle_calc = "along", label_dodge = unit(2, units = "mm"),
        label_colour = NA, label_size = nb_size
      ),
      scale_edge_width(range = edge_width, guide = "none"),
      scale_edge_color_manual("Nombre de mobilités (moyenne annuelle)", 
                              values = c("Ascendante"  = "#d32f2f", 
                                         "Descendante" = "#0077c2", 
                                         "Horizontale" = "#388e3c")),  # "#9a0007" "#0077c2"
      #theme_minimal(),
      scale_y_continuous(labels = Y_axis_label_names, 
                         breaks = 1:max(CORPS_NH_net_layout$y, na.rm = TRUE),
                         limits = function(lims){c(lims[1] - ylim_plus, lims[2] + ylim_plus)},
      ),
      theme(plot.background = element_rect(fill = "white"), 
            panel.background = element_rect(fill = "white"), 
            axis.text.y = element_text(hjust = 0), # face = "bold" 
            legend.position = "bottom" #, 
      )
    )
    
    plot_output <- 
      ggraph(CORPS_NH_net_layout) + 
      CORPS_NH_net_graph_options +  
      geom_node_label(aes(x = x, y = y, label = name, fontface = in_bold),
                      size = text_size, label.padding = unit(0.5, "mm"),
                      fill = rgb(1, 1, 1, alpha = 0.5),
      ) + 
      # geom_node_label(aes(x = if_else(in_bold, x, NA_real_), 
      #                     y = y, label = name),
      # size = text_size, fontface = "bold"
      # ) + 
      xlim( c(xrange[1] - xlim_plus, xrange[2] + xlim_plus))
    
  } else {
    stop("type must be among 'points' or 'labels'")
  }
  
  plot_output$xrange <- xrange
  plot_output$yrange <- yrange
  
  return(plot_output)
}
























































# Start--------------------------------------------------------------------------------------
library(tidyverse)
library(rlang)
library(tabxplor)
library(FactoMineR)
library(ggfacto)

## Fonts management ----

#windowsFonts(sans = windowsFont("DejaVu Sans Condensed")) #Replace "TT Arial"
windowsFonts(sans = windowsFont("DejaVu Sans"))
windowsFonts(mono = windowsFont("DejaVu Sans Mono"))



# Tab functions --------------------------------------------------
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


# Logit functions ---- 

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







# Geometrical data analysis ------------------------------------------------------------------

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







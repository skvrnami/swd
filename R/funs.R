get_var_label <- function(x){
    tmp <- attr(x, "label")
    if(is.null(tmp)) stop("label is null")
    tmp
}

possibly_get_var_label <- purrr::possibly(get_var_label, NA_character_)

get_codebook <- function(df){
    purrr::map_df(colnames(df), function(x) {
        data.frame(
            var_name = x, 
            var_label = as.character(possibly_get_var_label(df[[x]]))
        )
    })
}

get_value_label <- function(x){
    labels <- attr(x, "labels")
    data.frame(
        value = as.character(unname(labels)), 
        label = names(labels)
    ) %>% tidyr::pivot_wider(
        ., names_from = value, 
        values_from = label, 
        names_glue = "value_{value}"
    )
}

possibly_get_value_label <- purrr::possibly(get_value_label)

get_value_labels_df <- function(df){
    purrr::map_dfr(colnames(df), function(x) {
        tmp <- possibly_get_value_label(df[[x]])
        if(!is.null(tmp)){
            tmp %>% 
                mutate(var_name = x) %>% 
                relocate(var_name, .before = everything())
        }
    })
}

add_suffix <- function(x, suffix){
    paste0(x, suffix)
}

recode_voters <- function(x, nda, abstain){
    case_when(
        is.na(x) ~ NA_integer_,
        x %in% nda ~ NA_integer_, 
        x %in% abstain ~ 0L, 
        TRUE ~ 1L
    )
}

recode_winners <- function(x, winners){
    case_when(
        is.na(x) ~ NA_integer_, 
        x %in% winners ~ 1L,
        TRUE ~ 0L
    )
}

normalize_scale <- function(x, base_multiplier = 1){
    minimum <- min(x, na.rm = TRUE)
    maximum <- max(x, na.rm = TRUE)
    
    ((x - minimum) / (maximum - minimum)) * base_multiplier
}

# Weighting
calc_weight_turnout <- function(df, df_turnout, election_name){
    df_weight <- df %>% 
        filter(!is.na(swd_diff)) %>% 
        filter(!is.na(voted))
    
    n <- nrow(df_weight)
    
    turnout_freq <- df_turnout %>% 
        filter(election == election_name) %>% 
        mutate(voted = turnout, 
               not_voted = 100 - turnout) %>% 
        select(voted, not_voted) %>% 
        tidyr::pivot_longer(., cols = 1:2, 
                            names_to = "turnout", values_to = "share") %>% 
        mutate(freq = share / 100 * n, 
               voted = as.numeric(turnout == "voted")) %>% 
        select(voted, freq)
    
    weighting_margins <- list(~voted)
    weighting_targets <- list(turnout_freq)
    
    svy_design <- svydesign(ids = ~1, data = df_weight)
    rake(design = svy_design, 
         sample.margins = weighting_margins, 
         population.margins = weighting_targets,
         control = list(maxit = 100000, epsilon = 1, verbose = FALSE)) %>% 
        trimWeights(rake_out, lower = 0.2, upper = 5, strict = TRUE) %>% 
        weights()
}

calc_weight_turnout_presidential <- function(df, df_turnout, election_name){
    df_weight <- df %>% 
        filter(!is.na(swd_diff)) %>% 
        filter(!is.na(voted_1r)) %>% 
        filter(!is.na(voted_2r))
    
    n <- nrow(df_weight)
    
    turnout_freq <- df_turnout %>% 
        filter(election == election_name) %>% 
        rename(voted_1r = turnout, voted_2r = turnout_2nd_round) %>% 
        mutate(not_voted_1r = 100 - voted_1r, 
               not_voted_2r = 100 - voted_2r) %>% 
        select(-election) %>% 
        tidyr::pivot_longer(., cols = 1:ncol(.), 
                            names_to = "turnout", values_to = "share") %>% 
        mutate(round = stringr::str_extract(turnout, "[1-2]r$"), 
               turnout = as.numeric(grepl("^voted", turnout)), 
               freq = share / 100 * n) 
    
    fst_round <- turnout_freq %>% 
        filter(round == "1r") %>% 
        select(voted_1r = turnout, freq)
    
    snd_round <- turnout_freq %>% 
        filter(round == "2r") %>% 
        select(voted_2r = turnout, freq)
    
    weighting_margins <- list(~voted_1r, ~voted_2r)
    weighting_targets <- list(fst_round, snd_round)
    
    svy_design <- svydesign(ids = ~1, data = df_weight)
    
    rake(design = svy_design, 
         sample.margins = weighting_margins, 
         population.margins = weighting_targets,
         control = list(maxit = 100000, epsilon = 1, verbose = FALSE)) %>% 
        trimWeights(rake_out, lower = 0.2, upper = 5, strict = TRUE) %>% 
        weights()
}

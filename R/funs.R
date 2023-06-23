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

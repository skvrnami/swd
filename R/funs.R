get_codebook <- function(df){
    purrr::map_df(colnames(df), function(x) {
        data.frame(
            var_name = x, 
            var_label = attr(df[[x]], "label")
        )
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

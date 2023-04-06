library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "haven", "here", "dplyr", 
               "labelled"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  # pre-election wave CZ 1996
  tar_target(
    cz_96_pre, 
    read_sav(here("data", "CZ_1996", "ZA3631.sav"), user_na = TRUE)
  ),
  
  tar_target(
    cbook_cz_96_pre, 
    get_codebook(cz_96_pre)
  ),
  
  tar_target(
    cbook_cz_96_pre_file, 
    writexl::write_xlsx(cbook_cz_96_pre, here("data", "CZ_1996", "codebook_pre-election_wave.xlsx"))
  ),
  
  # post-election wave CZ 1996
  tar_target(
    cz_96_post,
    read_sav(here("data", "CZ_1996", "ZA3633.sav"), user_na = TRUE)
  ), 
  
  tar_target(
    cbook_cz_96_post, 
    get_codebook(cz_96_post)
  ),
  
  tar_target(
    cbook_cz_96_post_file, 
    writexl::write_xlsx(cbook_cz_96_post, here("data", "CZ_1996", "codebook_post-election_wave.xlsx"))
  ),
  
  # merged waves CZ 1996
  tar_target(
    cz_96_merged, 
    full_join(
      cz_96_pre %>% 
        rename_with(., ~add_suffix(.x, "_w1"), .cols = -c("V2")) %>% 
        mutate(w1 = 1), 
      cz_96_post %>% 
        rename_with(., ~add_suffix(.x, "_w2"), .cols = -c("V2")) %>% 
        mutate(w2 = 1), 
      by = "V2"
    ) %>% 
      mutate(
        voted = recode_voters(V10_w2, nda = c(98, 99), abstain = 87), 
        winner_biggest_party = recode_winners(as_factor(V10_w2), winners = "ODS"), 
        winner_government = recode_winners(
          as_factor(V10_w2), winners = c("ODS", "KDU-CSL", "ODA")),
        swd_w1 = case_when(
          V29_w1 == 1 ~ 2, # Very satisfied
          V29_w1 == 2 ~ 1, # Fairly satisfied
          V29_w1 == 3 ~ -1, # Not very satisfied 
          V29_w1 == 4 ~ -2, # Not satisfied at all
          TRUE ~ V29_w1
        ), 
        swd_w2 = case_when(
          V71_w2 == 1 ~ 2, # Very satisfied
          V71_w2 == 2 ~ 1, # Fairly satisfied
          V71_w2 == 3 ~ -1, # Not very satisfied 
          V71_w2 == 4 ~ -2, # Not satisfied at all
          TRUE ~ V71_w2
        ),
        swd_diff = swd_w2 - swd_w1, 
        vote_intention_w1 = as_factor(V13_w1), 
        vote_intention_w1 = case_when(
          vote_intention_w1 %in% c("None", "Refused to answer", "No answer") ~ NA_character_, 
          vote_intention_w1 == "KDS (joint list with ODS)" ~ "ODS", 
          vote_intention_w1 == "Other'" ~ "Other", 
          TRUE ~ as.character(vote_intention_w1)
        ), 
        vote_choice_w2 = as_factor(V10_w2),
        stable_voter = (vote_intention_w1 == vote_choice_w2), 
        stable_voter = if_else(is.na(stable_voter), FALSE, stable_voter),
        w1 = if_else(is.na(w1), 0, w1), 
        w2 = if_else(is.na(w2), 0, w2), 
        both_waves = w1 == 1 & w2 == 1) %>% 
      set_variable_labels(.,
                          swd_w1 = "Satisfaction with democracy (pre-election wave)", 
                          swd_w2 = "Satisfaction with democracy (post-election wave)")
  ), 
  
  tar_render(
    swd_rmd, 
    "swd.Rmd"
  )
)

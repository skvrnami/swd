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
  # CZ 1996 -----------------------------------------------
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
        both_waves = w1 == 1 & w2 == 1, 
        gender = haven::as_factor(V46_w1),
        birth_year = 1900 + V45_w1, 
        age = 1996 - birth_year, 
        lrself = V44_w1, 
        education = haven::as_factor(V49_w1), 
        r_education = case_when(
          education %in% c("less than primary", "primary") ~ "primary", 
          TRUE ~ education
        )) %>% 
      set_variable_labels(.,
                          swd_w1 = "Satisfaction with democracy (pre-election wave)", 
                          swd_w2 = "Satisfaction with democracy (post-election wave)")
  ), 
  
  # EU 2019 -----------------------------------------------
  tar_target(
    eu_2019, 
    read_sav(here("data", "EU_2019", "10080_da_en_v1_0.zsav"),
             encoding = "latin1") %>% 
      as_factor()
  ),
  

  # PL 2019 -----------------------------------------------------------------
  tar_target(
    pl_2019, 
    eu_2019 %>% 
      filter(W1_S0 == "Poland") %>% 
      select(where(function(x) any(!is.na(x)))) %>% 
      rename(
        swd_w1 = W1_Q9, 
        swd_w2 = W2_Q9, 
        lrself = W1_Q16,
      ) %>% 
      mutate(
        swd_diff = as.numeric(swd_w2) - as.numeric(swd_w1), 
        voted = case_when(
          W2_Q59a == "I am sure that I voted on May 26 2019" ~ 1L,
          W2_Q59a %in% c("Refuse") | grepl("know", W2_Q59a) ~ NA_integer_,
          is.na(W2_Q59a) ~ NA_integer_,
          TRUE ~ 0
        )
      )
  ), 

  # HU 2019 -----------------------------------------------------------------
  tar_target(
    hu_2019, 
    eu_2019 %>% 
      filter(W1_S0 == "Hungary") %>% 
      select(where(function(x) any(!is.na(x)))) %>% 
      rename(
        swd_w1 = W1_Q9, 
        swd_w2 = W2_Q9, 
        lrself = W1_Q16,
        pid = W1_Q17
      ) %>% 
      mutate(
        swd_diff = as.numeric(swd_w2) - as.numeric(swd_w1),
        voted = case_when(
          W2_Q59a == "I am sure that I voted on May 26 2019" ~ 1L,
          W2_Q59a %in% c("Refuse") | grepl("know", W2_Q59a) ~ NA_integer_,
          is.na(W2_Q59a) ~ NA_integer_,
          TRUE ~ 0
        )
      )
      
  ), 
  

  # RO 2009 -----------------------------------------------------------------
  tar_target(
    ro_2009, 
    read_sav(here("data", "RO_2009", "UBB_P2009_val_1_3.sav"), user_na = TRUE)
  ),
  
  tar_target(
    ro_2009_codebook, {
    cbook <- get_codebook(ro_2009)
    value_labels <- get_value_labels_df(ro_2009)
    cbook %>% 
      left_join(., value_labels, by = "var_name")
    }
  ),
  
  tar_target(
    ro_2009_cbook_file, 
    writexl::write_xlsx(ro_2009_codebook, here("data", "RO_2009", "codebook1.xlsx"))
  ),
  
  tar_target(
    ro_2009_final, 
    ro_2009 %>% 
      as_factor() %>% 
      rename(
        swd_w1 = D10, 
        swd_w3 = Q19
      ) %>% 
      mutate(
        swd_w1 = case_when(
          swd_w1 == "Deloc multumit" ~ 0,
          swd_w1 == "Nu prea multumit" ~ 1, 
          swd_w1 == "Destul de multumit" ~ 2,
          swd_w1 == "Foarte multumit" ~ 3,
          swd_w1 %in% c("NS", "NR") ~ NA_integer_
        ), 
        swd_w3 = case_when(
          swd_w3 == "Deloc multumit" ~ 0,
          swd_w3 == "Nu prea multumit" ~ 1, 
          swd_w3 == "Destul de multumit" ~ 2,
          swd_w3 == "Foarte multumit" ~ 3,
          swd_w3 %in% c("NS", "NR") ~ NA_integer_, 
          is.na(swd_w3) ~ NA_integer_
        ), 
        swd_diff = swd_w3 - swd_w1,
        voted_2r = case_when(
          Q23XX == "Sunt sigur ca am votat" ~ 1, 
          Q23XX %in% c("Nu am votat la turul 2 al alegerilor prezidentiale din 6 dec", 
                       "M-am gandit sa votez de aceasta data, dar nu am votat", 
                       "De obicei votez, dar de aceasta data nu am votat") ~ 0, 
          Q23XX %in% c("NS", "NR") ~ NA_integer_
        ),
        winner = as.numeric(Q24B == "Traian Basescu"), 
        candidate_qualified_2nd = case_when(
          Q21B %in% c("Traian Basescu", "Mircea Geoana") ~ 1, 
          Q21B %in% c("Nu stiu", "Nu raspund") ~ NA_integer_, 
          is.na(Q21B) ~ NA_integer_, 
          TRUE ~ 0
        ), 
        same_candidate_in_2r = case_when(
          Q24B %in% c("NS", "NR", "Am anulat votul") | Q21B %in% c("Nu stiu", "Nu raspund", "Am anulat votul") ~ NA_integer_, 
          as.character(Q24B) == as.character(Q21B) ~ 1, 
          as.character(Q24B) != as.character(Q21B) ~ 0
        )
      )
  ),
  
  # RO 2012 -----------------------------------------------------------------
  # P1 wave 1 (pre-campaign)
  tar_target(
    ro_2012_p1v1, 
    read_sav(here("data", "RO_2012", "UBB_P1V1_121116.sav"), user_na = TRUE)
  ),
  
  # P1 wave 2 (pre-election)
  tar_target(
    ro_2012_p1v2, 
    read_sav(here("data", "RO_2012", "UBB_P1V2_130204.sav"), user_na = TRUE)
  ),
  
  # P1 wave 3 (post-election)
  tar_target(
    ro_2012_p1v3, 
    read_sav(here("data", "RO_2012", "UBB_P1V3_130301.sav"), user_na = TRUE)
  ),
  
  # P1 merged
  tar_target(
    ro_2012_p1, 
    ro_2012_p1v1 %>% 
      left_join(., ro_2012_p1v2 %>% rename_with(., ~paste0(.x, "_w2"), .cols = -c(Id)), by = c("ID"="Id")) %>% 
      left_join(., ro_2012_p1v3 %>% rename_with(., ~paste0(.x, "_w3"), .cols = -c(ID)), by = "ID")
  ),
  
  tar_target(
    ro_2012_codebook1, {
      cbook <- get_codebook(ro_2012_p1)
      value_labels <- get_value_labels_df(ro_2012_p1)
      cbook %>% 
        left_join(., value_labels, by = "var_name")
    }
  ),
  
  tar_target(
    ro_2012_cbook_file1, 
    writexl::write_xlsx(ro_2012_codebook1, here("data", "RO_2012", "codebook1.xlsx"))
  ),
  
  tar_target(
    ro_2012_p1_final, 
    ro_2012_p1 %>% 
      as_factor() %>% 
      mutate(
        swd_w1 = if_else(D10 %in% c("Nu stiu", "Nu raspund"), NA_integer_, as.numeric(D10)), 
        swd_w2 = if_else(D10_w2 %in% c("Nu stiu", "Nu raspund"), NA_integer_, as.numeric(D10_w2)), 
        swd_w3 = if_else(D10_w3 %in% c("Nu stiu", "Nu raspund"), NA_integer_, as.numeric(D10_w3)),
        swd_diff = swd_w3 - swd_w1, 
        swd_diff_w2 = swd_w3 - swd_w2, 
        voted = case_when(
          Q5LH_AG1_w3 == "Sunt sigur ca am votat" | Q5LH_AG2_w3 == "Am votat" ~ 1, 
          Q5LH_AG1_w3 %in% c("Nu am votat la alegerile parlamentare din 9 decembrie 2012", 
                             "M-am gândit sa votez de aceasta data, dar nu am votat", 
                             "De obicei votez, dar de aceasta data nu am votat") | 
            Q5LH_AG2_w3 %in% c("Nu am votat", "Nu am fost trecut în listele electorale") ~ 0, 
          Q5LH_AG1_w3 %in% c("Nu stiu", "Nu raspund") |
            Q5LH_AG2_w3 %in% c("Refuz", "Nu stiu") ~ NA_integer_
        ), 
        r_EDUC = case_when(
          EDUC == "fara scoala" ~ "Without education",
          EDUC %in% c("scoala primara neterminata", "scoala primara terminata", 
                      "gimnaziu incomplet", "gimnaziu complet", "scoala de ucenici (complementara)") ~ "Elementary school", 
          EDUC %in% c("scoala profesionala", "liceu neterminat", "liceu terminat") ~ "High school",
          EDUC %in% c("scoala postliceala", "facultate neterminata", 
                      "facultate – subingineri sau colegiu", "facultate complet", 
                      "masterat", "doctorat") ~ "Higher education",
          EDUC %in% c("Nu stiu", "Nu raspund") ~ NA_character_
        ), 
        r_age = if_else(age == 0, NA_integer_, as.numeric(age)), 
        winner = case_when(
          Q5LH_CS_w3 == "Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)" | 
            Q5LH_CD_w3 == "Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)" ~ 1, 
          Q5LH_CS_w3 %in% c("Refuz", "Nu stiu") |
            Q5LH_CD_w3 %in% c("Refuz", "Nu stiu") ~ NA_integer_, 
          !is.na(Q5LH_CS_w3) ~ 0
          ), 
        stable_voter = as.numeric(as.character(P12V) == as.character(Q5LH_CS_w3))
      )
  ),
  
  tar_target(
    ro_2012_p2v1, 
    read_sav(here("data", "RO_2012", "UBB_P2V1_130211.sav"), user_na = TRUE)
  ),
  
  tar_target(
    ro_2012_p2v2, 
    read_sav(here("data", "RO_2012", "UBB_P2V2_130221.sav"), user_na = TRUE)
  ),
  
  tar_target(
    ro_2012_p2, 
    ro_2012_p2v1 %>% 
      rename_with(., ~paste0(.x, "_w2"), .cols = -c(ID)) %>% 
      left_join(., ro_2012_p2v2 %>% rename_with(., ~paste0(.x, "_w3"), .cols = -c(ID)), 
                by = "ID")
  ),
  
  tar_target(
    ro_2012_codebook2, {
      cbook <- get_codebook(ro_2012_p2)
      value_labels <- get_value_labels_df(ro_2012_p2)
      cbook %>% 
        left_join(., value_labels, by = "var_name")
    }
  ),
  
  tar_target(
    ro_2012_cbook_file2, 
    writexl::write_xlsx(ro_2012_codebook2, here("data", "RO_2012", "codebook2.xlsx"))
  ),
  
  tar_target(
    ro_2012_p2_final, 
    ro_2012_p2 %>% 
      as_factor %>% 
      mutate(
        swd_w2 = case_when(
          D10_w2 == "Deloc multumit" ~ 0, 
          D10_w2 == "Nu prea multumit" ~ 1, 
          D10_w2 == "Destul de multumit" ~ 2, 
          D10_w2 == "Foarte multumit" ~ 4, 
          D10_w2 %in% c("Nu stiu", "Nu raspund") ~ NA_integer_
        ), 
        swd_w3 = case_when(
          Q15_w3 == "Deloc multumit" ~ 0, 
          Q15_w3 == "Nu prea multumit" ~ 1, 
          Q15_w3 == "Destul de multumit" ~ 2, 
          Q15_w3 == "Foarte multumit" ~ 4, 
          Q15_w3 %in% c("Nu stiu", "Refuz") ~ NA_integer_
        ), 
        swd_diff_w2 = swd_w3 - swd_w2, 
        voted = case_when(
          Q5LH_AG1_w3 == "Sunt sigur ca am votat" | Q5LH_AG2_w3 == "Am votat" ~ 1, 
          Q5LH_AG1_w3 %in% c("Nu am votat la alegerile parlamentare din 9 decembrie 2012", 
                             "M-am gândit sa votez de aceasta data, dar nu am votat", 
                             "De obicei votez, dar de aceasta data nu am votat") | 
            Q5LH_AG2_w3 %in% c("Nu am votat", "Nu am fost trecut în listele electorale") ~ 0, 
          Q5LH_AG1_w3 %in% c("Nu stiu", "Nu raspund") |
            Q5LH_AG2_w3 %in% c("Refuz", "Nu stiu") ~ NA_integer_
        ),
        r_EDUC = case_when(
          EDUC_w2 == "fara scoala" ~ "Without education",
          EDUC_w2 %in% c("scoala primara neterminata", "scoala primara terminata", 
                      "gimnaziu incomplet", "gimnaziu complet", "scoala de ucenici (complementara)") ~ "Elementary school", 
          EDUC_w2 %in% c("scoala profesionala", "liceu neterminat", "liceu terminat") ~ "High school",
          EDUC_w2 %in% c("scoala postliceala", "facultate neterminata", 
                      "facultate – subingineri sau colegiu", "facultate complet", 
                      "masterat", "doctorat") ~ "Higher education",
          EDUC_w2 %in% c("Nu stiu", "Nu raspund") ~ NA_character_
        ), 
        r_age = if_else(age_w2 == 0, NA_integer_, as.numeric(age_w2)), 
        winner = case_when(
          Q5LH_CS_w3 == "Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)" | 
            Q5LH_CD_w3 == "Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)" ~ 1, 
          Q5LH_CS_w3 %in% c("Refuz", "Nu stiu") |
            Q5LH_CD_w3 %in% c("Refuz", "Nu stiu") ~ NA_integer_, 
          !is.na(Q5LH_CS_w3) ~ 0
        ), 
        stable_voter = as.numeric(as.character(P12V_w2) == as.character(Q5LH_CS_w3)), 
        GEN = GEN_w2
      )
  ),
  
  tar_target(
    ro_2012_all, {
      COMMON_VARS <- c("ID", "r_EDUC", "r_age", "GEN", "swd_w2", "swd_w3", "swd_diff_w2", 
                       "winner", "stable_voter", "voted")
      bind_rows(
        ro_2012_p1_final %>% select(any_of(COMMON_VARS)), 
        ro_2012_p2_final %>% select(any_of(COMMON_VARS))  
      )
    }
  ),
  
  # DE 2021 -----------------------------------------------
  
  # a month before election
  tar_target(
    de_2021_w18, 
    read_sav(here("data", "DE_2021", "ZA7724_v2-0-0.sav"), user_na = TRUE)
  ),
  
  # pre-election
  # tar_target(
  #   de_2021_w19, 
  #   read_sav(here("data", "DE_2021", "ZA7725_v1-0-0.sav"), user_na = TRUE)
  # ),
  
  # post-election
  tar_target(
    de_2021_w20, 
    read_sav(here("data", "DE_2021", "ZA7726_v2-0-0.sav"), user_na = TRUE)
  ),
  
  tar_target(
    de_2021_final, 
    inner_join(
      de_2021_w18, 
      de_2021_w20, 
      by = "lfdn", suffix = c("_w18", "_w20")
    ) %>% 
      as_factor() %>% 
      filter(kp20_2601 %in% c("Mecklenburg-Vorpommern", "Sachsen", 
                              "Sachsen-Anhalt", "Thueringen", 
                              "Brandenburg", "Berlin")) %>% 
      mutate(
        swd_w18 = case_when(
          kp18_020 == "sehr zufrieden" ~ 2, 
          kp18_020 == "zufrieden" ~ 1, 
          kp18_020 == "teils/teils" ~ 0,
          kp18_020 == "unzufrieden" ~ -1,
          kp18_020 == "sehr unzufrieden" ~ -2, 
          TRUE ~ NA_integer_
        ), 
        swd_w20 = case_when(
          kp20_020 == "sehr zufrieden" ~ 2, 
          kp20_020 == "zufrieden" ~ 1, 
          kp20_020 == "teils/teils" ~ 0,
          kp20_020 == "unzufrieden" ~ -1,
          kp20_020 == "sehr unzufrieden" ~ -2, 
          TRUE ~ NA_integer_
        ), 
        swd_diff = swd_w20 - swd_w18, 
        voted = case_when(
          kp20_180 %in% c(
            "Ich habe gewaehlt.", 
            "Ich hatte zuvor bereits Briefwahl gemacht.") ~ 1,
          kp18_170 == "habe bereits Briefwahl gemacht" ~ 1,
          # TODO: kp19_170 == hlasování poštou
          kp20_180 == "Ich habe nicht gewaehlt." ~ 0, 
          TRUE ~ NA_integer_
        ), 
        winner_party = case_when(
          kp20_200ba == "SPD" | kp20_200bb == "SPD" ~ 1,
          TRUE ~ 0
        ), 
        winner_government = case_when(
          kp20_200ba %in% c("SPD", "GRUENE", "FDP") | 
            kp20_200bb %in% c("SPD", "GRUENE", "FDP") ~ 1,
          TRUE ~ 0
        ), 
        sex = case_when(
          kp20_2280 == "maennlich" ~ "male", 
          kp20_2280 == "weiblich" ~ "female", 
          TRUE ~ NA_character_
        )
      )
  ),
  
  tar_render(
    swd_rmd, 
    "swd.Rmd"
  )
)

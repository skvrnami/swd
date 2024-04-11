library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "haven", "here", "dplyr", 
               "labelled", "survey"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

tar_source()

# Replace the target list below with your own:
list(
  
  tar_target(
    turnout_df, {
      tribble(
        ~election, ~turnout, ~turnout_2nd_round,
        "CZ 1996", 76.4, NA_real_,
        "RO 2012", 41.8, NA_real_,
        "DE 2017 (East)", 73.5, NA_real_,
        "RO 2009", 54.4, 58.0,
        "CZ 2023", 68.2, 70.2,
        "PL 2019", 45.7, NA_real_,
        "HU 2019", 43.6, NA_real_
      )
    }
  ),
  
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
    cz_1996, 
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
        winner_party = recode_winners(as_factor(V10_w2), winners = "ODS"), 
        winner = winner_party, 
        winner_government = recode_winners(
          as_factor(V10_w2), winners = c("ODS", "KDU-CSL", "ODA")),
        swd_pre = case_when(
          V29_w1 == 1 ~ 4, # Very satisfied
          V29_w1 == 2 ~ 3, # Fairly satisfied
          V29_w1 == 3 ~ 2, # Not very satisfied 
          V29_w1 == 4 ~ 1, # Not satisfied at all
          TRUE ~ NA_integer_ # V29_w1
        ), 
        swd_post = case_when(
          V71_w2 == 1 ~ 4, # Very satisfied
          V71_w2 == 2 ~ 3, # Fairly satisfied
          V71_w2 == 3 ~ 2, # Not very satisfied 
          V71_w2 == 4 ~ 1, # Not satisfied at all
          TRUE ~ NA_integer_ # V71_w2
        ),
        swd_diff = swd_post - swd_pre, 
        vote_intention_w1 = as_factor(V13_w1), 
        vote_intention_w1 = case_when(
          vote_intention_w1 %in% c("None", "Refused to answer", "No answer") ~ NA_character_, 
          vote_intention_w1 == "KDS (joint list with ODS)" ~ "ODS", 
          vote_intention_w1 == "Other'" ~ "Other", 
          TRUE ~ as.character(vote_intention_w1)
        ), 
        vote_previous_nat_election = as_factor(V9_w1),
        vote_previous_nat_election = case_when(
            vote_previous_nat_election %in% c("No answer", "Do not know", "Refused to answer") ~
                NA_character_,
            vote_previous_nat_election == "ODS (joint list with KDS)" ~ "ODS",
            vote_previous_nat_election == "KDS (joint list with ODS)" ~ "ODS",
            TRUE ~ as.character(vote_previous_nat_election)
        ),
        vote_choice = as_factor(V10_w2),
        stable_voter = case_when(
            vote_previous_nat_election == "Did not vote" |
                vote_choice == "Did not vote" ~ 0,
            as.character(vote_previous_nat_election) == as.character(vote_choice) ~ 1,
            vote_previous_nat_election == "LB (KSCM etc)" & vote_choice == "KSCM" ~ 1,
            vote_previous_nat_election %in% c("OH", "CSS") & vote_choice == "SD-LSNS" ~ 1,
            TRUE ~ 0
        ),
        stable_intention = (vote_intention_w1 == vote_choice), 
        stable_intention = if_else(is.na(stable_intention), FALSE, stable_intention),
        w1 = if_else(is.na(w1), 0, w1), 
        w2 = if_else(is.na(w2), 0, w2), 
        both_waves = w1 == 1 & w2 == 1, 
        gender = haven::as_factor(V46_w1),
        birth_year = 1900 + V45_w1, 
        age = 1996 - birth_year, 
        lrself = V44_w1, 
        education = haven::as_factor(V49_w1), 
        education_w2 = haven::as_factor(V149_w2),
        r_education = case_when(
          education %in% c("less than primary", "primary") ~ "primary", 
          education %in% c("vocational", "secondary") ~ "secondary",
          education == "higher" ~ "post-secondary",
          TRUE ~ education
        ) %>% factor(., levels = c("primary", "secondary", "post-secondary")), 
        r_education_w2 = case_when(
          education_w2 %in% c("incomplete primary", "primary") ~ "primary", 
          education_w2 %in% c("vocational, no exam", "vocational with exams", 
                              "some secondary", "secondary with specialization") ~ "secondary", 
          education_w2 == "university degree" ~ "post-secondary"
        ) %>% factor(., levels = c("primary", "secondary", "post-secondary")), 
        postsecondary_edu = case_when(
          !is.na(r_education) & !is.na(r_education_w2) ~ as.numeric(r_education == "post-secondary" | 
                                                         r_education_w2 == "post-secondary"), 
          !is.na(r_education) ~ as.numeric(r_education == "post-secondary"), 
          !is.na(r_education_w2) ~ as.numeric(r_education_w2 == "post-secondary")
        ),
        female = as.numeric(gender == "female"), 
        V4_w2 = as_factor(V4_w2),
        pol_interest_num = case_when(
            V4_w2 == "DK,NA" ~ NA_integer_, 
            V4_w2 == "A great deal" ~ 3L,
            V4_w2 == "Some extent" ~ 2L,
            V4_w2 == "Not much" ~ 1L, 
            V4_w2 == "Not at all" ~ 0L
        ),
        # correct answers to political knowledge questions
        pol_knowledge1 = as.numeric(V144_w2 == 5),
        pol_knowledge2 = as.numeric(V145_w2 == 1), 
        pol_knowledge3 = as.numeric(V146_w2 == 200), 
        pol_knowledge = rowSums(across(matches("pol_knowledge[1-3]{1}"))), 
        pol_knowledge_pct = (pol_knowledge / 3) * 100,
        party_close = as.numeric((V73_w2 == 1) | 
          as.numeric(V78_w2 == 1))
        ) %>% 
      set_variable_labels(.,
                          swd_pre = "Satisfaction with democracy (pre-election wave)", 
                          swd_post = "Satisfaction with democracy (post-election wave)")
  ), 
  
  tar_target(
    cz_1996_weight, {
      # pl_2019_weight <- calc_weight_turnout(pl_2019, turnout, "PL 2019")
      cz_1996_weight <- calc_weight_turnout(cz_1996, turnout_df, "CZ 1996")
      cz_1996 %>% 
        filter(!is.na(swd_diff)) %>% 
        filter(!is.na(voted)) %>% 
        mutate(weight_turnout = cz_1996_weight) %>% 
        mutate(weight_turnout_norm = weight_turnout / sum(weight_turnout) * 1000)
    }
  ),
  
  # CZ 2023 -----------------------------------------------
  tar_target(
    cz_2023, 
    
    # readRDS(here("data", "CZ_2023", "wide_w1_w4_v02.rds")) %>% 
    readRDS(here("data", "CZ_2023", "wide_w1_w5.rds")) %>% 
      mutate(across(starts_with("SWD"), as.numeric)) %>% 
      as_factor() %>% 
      mutate(
        # swd_diff = SWD_w4 - SWD_w2,
        swd_diff = SWD_w4 - SWD_w1,
        winner_1r = case_when(
          PRES2023CAND1r_w3 == "Petr Pavel" ~ "winner", 
          PRES2023CAND1r_w3 == "Andrej Babiš" ~ "qualified to 2nd round",
          PRES2023CAND1r_w3 == "nevolič" ~ "didn't vote", 
          !is.na(PRES2023CAND1r_w3) ~ "loser"
        ), 
        winner_2r = case_when(
          PRES2023CAND2r_w4 == "Petr Pavel" ~ "winner", 
          PRES2023CAND2r_w4 == "Andrej Babiš" ~ "loser", 
          PRES2023CAND2r_w4 == "nevolič" ~ "didn't vote"
        ), 
        voted = as.numeric(
          PRES2023CAND2r_w4 != "nevolič"
        ),
        voted_1r = as.numeric(
          PRES2023PART1_w3 == "Ano"
        ),
        voted_2r = as.numeric(
          PRES2023PART2_w4 == "Ano"
        ),
        know_true1 = as.numeric(KNOW1_w4 == "poměrného"), 
        know_true2 = as.numeric(KNOW2_w4 == "ne"), 
        know_true3 = as.numeric(KNOW3_w4 == "nepravdivé"),
        know_true4 = as.numeric(KNOW4_w4 == "nepravdivé"), 
        know_true5 = as.numeric(KNOW5_w4 == "pravdivé"), 
        know_true6 = as.numeric(KNOW6_w4 == "nepravdivé"),
        pol_knowledge_pct = (rowSums(across(matches("know_true[1-6]+"))) / 6) * 100, 
        pol_interest_num = case_when(
          POLINT_w1 == "Vůbec ne" ~ 0, 
          POLINT_w1 == "Jen trochu" ~ 1, 
          POLINT_w1 == "Docela" ~ 2, 
          POLINT_w1 == "Velmi" ~ 3
        ), 
        postsecondary_edu = as.numeric(
          EDU %in% c(
            "Vyšší odborné", 
            "VŠ do úrovně bakalářského včetně", 
            "Vysokoškolské nad úroveň bakalářského")
        ), 
        winner = as.numeric(winner_2r == "winner"), 
        stable_voter = as.numeric(as.character(PRES2023CAND1r_w3) == as.character(PRES2023CAND2r_w4) & 
                                    PRES2023CAND1r_w3 != "nevolič"), 
        duty_to_vote = as.numeric(CIVIC_w1 == "Občanská povinnost"), 
        voted_1r = as.numeric(PRES2023CAND1r_w3 != "nevolič"), 
        female = as.numeric(SEX_w1 == "Žena"), 
        party_close = case_when(
            PIDHAS_w1 == "Ano" ~ 1,
            CLOSER_w1 == "Ano" ~ 1, 
            TRUE ~ 0
        ), 
        candidate_qualified_2nd = as.numeric(
          PRES2023CAND2r_w4 %in% c("Petr Pavel", "Andrej Babiš")
        ), 
        swd_pre = SWD_w1,
        # swd_pre = SWD_w2,
        swd_post = SWD_w4, 
        econ_precariousness = ECOENERGY_w1, 
        econ_precariousness2 = as.numeric(
          ECOENERGY_w1 %in% c("s velkými obtížemi", "s obtížemi", 
                              "s menšími obtížemi")
        ),
        econ_precariousness3 = case_when(
          ECOENERGY_w1 %in% c("snadno", "velmi snadno") ~ "Easy",
          ECOENERGY_w1 %in% c("docela snadno") ~ "Fairly easy",
          ECOENERGY_w1 %in% c("s menšími obtížemi") ~ "With minor difficulties",
          ECOENERGY_w1 %in% c("s obtížemi", "s velkými obtížemi") ~ "With difficulties"
        ) %>% factor(., levels = c("Easy", "Fairly easy", "With minor difficulties", "With difficulties")),
        soc_class = S12A, 
        soc_class_r = case_when(
          soc_class == "Nezaměstnaný" ~ "Unemployed", 
          soc_class == "Student, v domácnosti, na rodičovské dovolené apod." ~ 
            "Econ. inactive (student, in household)",
          soc_class == "Nepracující důchodce" ~ "Pensioner",
          soc_class == "Zaměstnanec bez podřízených" ~ "Employee",
          soc_class %in% c("Zaměstnanec - nižší vedoucí (1-5 podřízených)", 
                           "Zaměstnanec - vyšší vedoucí (6 a více podřízených)", 
                           "Zaměstnanec - vrcholový manažer, ředitel podnik", 
                           "Soukromý podnikatel s 1-5 zaměstnanci", 
                           "Soukromý podnikatel s 6 a více zaměstnanci") ~ 
            "Manager/Businessperson",
          soc_class == "Soukromý podnikatel bez zaměstnanců (OSVČ)" ~ "Freelancer",
        ) %>% factor(., levels = c("Employee", "Unemployed", "Econ. inactive (student, in household)", 
                                   "Pensioner", "Manager/Businessperson", "Freelancer", 
                                   "Businessperson")), 
        vote_intention_w1 = as.numeric(IFPRESPART_1_w1 %in% c("Určitě ano", "Spíše ano"))
      ) %>% 
      rename(
        age = AGE_w1
      ) 
  ),
  
  tar_target(
    cz_2023_weight, {
      cz_2023_weight <- calc_weight_turnout_presidential(cz_2023, turnout_df, "CZ 2023")
      cz_2023 %>% 
        filter(!is.na(swd_diff)) %>% 
        filter(!is.na(voted_1r)) %>% 
        filter(!is.na(voted_2r)) %>% 
        mutate(weight_turnout = cz_2023_weight,
               weight_turnout_norm = weight_turnout / sum(weight_turnout) * 1000)
    }
  ),
  
  tar_target(
    cz_2023_models, {
      cz_2023_weight %>% 
        mutate(
          pol_knowledge_pct = pol_knowledge_pct / 100, 
          nonvoter = 1 - voted, 
          voter_type = case_when(
            nonvoter == 1 ~ "Abstainer", 
            winner == 0 ~ "Voted for loser", 
            winner == 1 ~ "Voted for winner"
          ) %>% factor(., levels = c("Abstainer", "Voted for winner", "Voted for loser")), 
          prez_vote_type = 
            case_when(
              winner_1r == "didn't vote" | winner_2r == "didn't vote" ~ "didn't vote (at least in one round)", 
              TRUE ~ paste0(winner_1r, " + ", winner_2r)
            ) %>% factor()
        )
    }
  ),
  
  tar_target(
    cz_2023_long, 
    readRDS("data/CZ_2023/long_w1_w5.rds") %>% 
      haven::as_factor() %>% 
      mutate(SWD_num = (as.numeric(SWD) - 1) / 10, 
             wave = as.factor(wave)) %>% 
      group_by(RADIOMETER_ID2) %>% 
      mutate(# SWD_wi = SWD_num - mean(SWD_num, na.rm = TRUE), 
        # SWD_bw = mean(SWD_num),
        winner = case_when(
          any(PRES2023PART2 == "Ne") ~ "abstainer",
          any(PRES2023CAND2r == "Petr Pavel") ~ "winner", 
          TRUE ~ "loser"
        ) %>% factor(., levels = c("abstainer", "winner", "loser")), 
        n_waves = n(), 
        psp2021 = zoo::na.locf(V20),
        vote_gov = as.numeric(psp2021 %in% c("Koalice SPOLU (ODS, TOP09 a KDU-ČSL)", 
                                             "Koalice Piráti a Starostové a nezávislí (STAN)")),
        fst_round = zoo::na.locf(PRES2023CAND1r, na.rm = FALSE) %>% 
          zoo::na.locf0(., fromLast = TRUE), 
        fst_round_r = case_when(
          fst_round %in% c("Karel Diviš", "Tomáš Zima", "Jaroslav Bašta") ~ "other", 
          TRUE ~ fst_round
        ), 
        winner_1r = case_when(
          fst_round == "Petr Pavel" ~ "winner", 
          fst_round == "Andrej Babiš" ~ "qualified to 2nd round", 
          TRUE ~ "loser"
        ),
        winner_cat = case_when(
          fst_round == "nevolič" | winner == "abstainer" ~ "didn't vote (at least in one round)", 
          TRUE ~ paste0(winner_1r, "+", winner)
        ), 
        winner_stable = case_when(
          fst_round == "abstainer" & winner == "abstainer" ~ "stable non-voter",
          fst_round == "Petr Pavel" & winner == "winner" ~ "stable winner", 
          fst_round == "Andrej Babiš" & winner == "loser" ~ "stable loser", 
          winner == "winner" ~ "unstable winner", 
          winner == "loser" ~ "unstable loser",
          TRUE ~ "unstable non-voter",
        ), 
        winner_stable2 = case_when(
          winner == "abstainer" ~ "abstainer",
          fst_round == "Petr Pavel" & winner == "winner" ~ "sincere winner", 
          fst_round == "Andrej Babiš" & winner == "loser" ~ "sincere loser", 
          winner == "winner" ~ "strategic winner", 
          winner == "loser" ~ "strategic loser"
        )) %>% 
      ungroup %>% 
      filter(n_waves == 5) %>% 
      left_join(., cz_2023_weight %>% select(RADIOMETER_ID2, weight_turnout), 
                by = "RADIOMETER_ID2") %>% 
      mutate(winner_stable2 = case_when(
        winner_stable2 == "sincere winner" ~ "full winner", 
        winner_stable2 == "strategic winner" ~ "sub-optimal winner",
        winner_stable2 == "sincere loser" ~ "full loser",
        winner_stable2 == "strategic loser" ~ "partial loser",
        TRUE ~ winner_stable2
      ) %>% factor(., levels = c("abstainer", "full winner", 
                                 "sub-optimal winner",
                                 "partial loser", 
                                 "full loser"))) 
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
        swd_pre = W1_Q9, 
        swd_post = W2_Q9, 
        lrself = W1_Q16,
        # pid = W1_Q17, 
        age = W1_Q1b, 
        age_cat = W1_Q1c, 
        gender = W1_Q2, 
        edu = W1_Q_edu, 
        vote_previous_nat_election = W1_Q58, 
        vote_intention = W1_Q60,
        vote_choice = W2_Q60a
      ) %>% 
      mutate(
        swd_diff = as.numeric(swd_post) - as.numeric(swd_pre), 
        voted = case_when(
          W2_Q59a == "I am sure that I voted on May 26 2019" ~ 1L,
          W2_Q59a %in% c("Refuse") | grepl("know", W2_Q59a) ~ NA_integer_,
          is.na(W2_Q59a) ~ NA_integer_,
          TRUE ~ 0
        ), 
        swd_pre = as.numeric(swd_pre), 
        swd_post = as.numeric(swd_post),
        stable_intention = case_when(
          vote_intention %in% c("Don't know", "Refuse", "Other party", 
                                "Would spoil the ballot") | 
            vote_choice %in% c("Don't know", "Refuse", "Other party", 
                               "Spoilt the ballot") ~ NA_integer_,
          as.character(vote_intention) == as.character(vote_choice) ~ 1,
          as.character(vote_intention) != as.character(vote_choice) ~ 0, 
          W1_Q59 < 5 ~ 0
        ), 
        stable_voter = case_when(
            vote_previous_nat_election %in% c("Refuse", "Don't know") | 
                vote_choice %in% c("Refuse", "Don't know") ~ NA_integer_, 
            vote_previous_nat_election %in% c("I did not vote", "I spoiled the ballot") ~ 0,
            vote_choice == "PSL-PO-.N-SLD-Zielo" & 
                vote_previous_nat_election %in% c(
                    "Platforma Obywatelska (PO)", 
                    "Polskie Stronnictwo Ludowe (PSL)", 
                    "Nowoczesna (.N)", 
                    "Zjednoczona Lewica (ZL))") ~ 1, 
            vote_previous_nat_election == "Zjednoczona Lewica (ZL))" & 
                vote_choice == "Wiosna" ~ 1,
            vote_choice == "PiS/Zjednoczsona Prawica" & 
                vote_previous_nat_election == "Prawo i Sprawiedliwosc (PiS)" ~ 1,
            vote_choice == "Wolnosc (KORWIN)-Ruch Narodowy" & 
                vote_previous_nat_election == "Wolnosc (KORWIN)" ~ 1,
            as.character(vote_choice) == as.character(vote_previous_nat_election) ~ 1,
            TRUE ~ 0
        ),
        winner_party = case_when(
          vote_choice == "PiS/Zjednoczsona Prawica" ~ 1,
          vote_choice %in% c("Don't know", "Refuse") ~ NA_integer_, 
          !is.na(vote_choice) ~ 0 
        ), 
        winner = winner_party,
        female = as.numeric(gender == "Female"), 
        postsecondary_edu = as.numeric(edu == "High"), 
        party_close = as.numeric(
          W1_Q17 == "Yes" | W1_Q18 == "Yes"
        ), 
        pol_interest_cat = factor(W1_Q5, levels = rev(c(
          "Very interested", "Fairly interested", "A little interested", 
          "Not at all interested"))), 
        pol_interest_num = as.numeric(pol_interest_cat), 
        know1 = as.numeric(W1_Q33 == "The European Parliament"), 
        know2 = as.numeric(W1_Q35 == "He was approved by a majority of the members of the European Parliament"), 
        know3 = as.numeric(W1_Q37 == "The European Commission"), 
        know4 = as.numeric(W1_Q65 == "Expenditure on the salaries of the EU officials is the largest item in the EU budget"), 
        know5 = as.numeric(W1_Q67 == "The Council of the European Union"), 
        pol_knowledge_pct = (rowSums(across(matches("know[1-5]{1}"))) / 5) * 100
      )
  ), 
  
  tar_target(
    pl_2019_weight, {
      pl_2019_weight <- calc_weight_turnout(pl_2019, turnout_df, "PL 2019")
      pl_2019 %>% 
        filter(!is.na(swd_diff)) %>% 
        filter(!is.na(voted)) %>% 
        mutate(weight_turnout = pl_2019_weight) %>% 
        mutate(weight_turnout_norm = weight_turnout / sum(weight_turnout) * 1000)
    }
  ),

  # HU 2019 -----------------------------------------------------------------
  tar_target(
    hu_2019, 
    eu_2019 %>% 
      filter(W1_S0 == "Hungary") %>% 
      select(where(function(x) any(!is.na(x)))) %>% 
      rename(
        swd_pre = W1_Q9, 
        swd_post = W2_Q9, 
        lrself = W1_Q16,
        # pid = W1_Q17, 
        age = W1_Q1b, 
        age_cat = W1_Q1c, 
        gender = W1_Q2, 
        edu = W1_Q_edu, 
        vote_previous_nat_election = W1_Q58, 
        vote_intention = W1_Q60, 
        vote_choice = W2_Q60a
      ) %>% 
      mutate(
        swd_diff = as.numeric(swd_post) - as.numeric(swd_pre),
        voted = case_when(
          W2_Q59a == "I am sure that I voted on May 26 2019" ~ 1L,
          W2_Q59a %in% c("Refuse") | grepl("know", W2_Q59a) ~ NA_integer_,
          is.na(W2_Q59a) ~ NA_integer_,
          TRUE ~ 0
        ), 
        swd_pre = as.numeric(swd_pre), 
        swd_post = as.numeric(swd_post),
        stable_intention = case_when(
          vote_intention %in% c("Don't know", "Refuse", "Other party", 
                                "Would spoil the ballot") | 
            vote_choice %in% c("Don't know", "Refuse", "Other party", 
                               "Spoilt the ballot") ~ NA_integer_,
          as.character(vote_intention) == as.character(vote_choice) ~ 1,
          as.character(vote_intention) != as.character(vote_choice) ~ 0, 
          W1_Q59 < 5 ~ 0
        ), 
        stable_voter = case_when(
            vote_choice %in% c("Refuse", "Don't know") |
                vote_previous_nat_election %in% c("Refuse", "Don't know") ~ NA_integer_,
            vote_previous_nat_election %in% c("I did not vote", "I spoiled the ballot") ~ 0,
            as.character(vote_choice) == as.character(vote_previous_nat_election) ~ 1,
            TRUE ~ 0
        ),
        winner_party = case_when(
          vote_choice == "FIDESZ-KNDP" ~ 1,
          vote_choice %in% c("Don't know", "Refuse") ~ NA_integer_, 
          !is.na(vote_choice) ~ 0 
        ), 
        winner = winner_party, 
        female = as.numeric(gender == "Female"), 
        postsecondary_edu = as.numeric(edu == "High"), 
        party_close = as.numeric(
          W1_Q17 == "Yes" | W1_Q18 == "Yes"
        ), 
        pol_interest_cat = factor(W1_Q5, levels = rev(c(
          "Very interested", "Fairly interested", "A little interested", 
          "Not at all interested"))), 
        pol_interest_num = as.numeric(pol_interest_cat), 
        know1 = as.numeric(W1_Q33 == "The European Parliament"), 
        know2 = as.numeric(W1_Q35 == "He was approved by a majority of the members of the European Parliament"), 
        know3 = as.numeric(W1_Q37 == "The European Commission"), 
        know4 = as.numeric(W1_Q65 == "Expenditure on the salaries of the EU officials is the largest item in the EU budget"), 
        know5 = as.numeric(W1_Q67 == "The Council of the European Union"), 
        pol_knowledge_pct = (rowSums(across(matches("know[1-5]{1}"))) / 5) * 100
      
  )), 
  
  tar_target(
    hu_2019_weight, {
      hu_2019_weight <- calc_weight_turnout(hu_2019, turnout_df, "HU 2019")
      hu_2019 %>% 
        filter(!is.na(swd_diff)) %>% 
        filter(!is.na(voted)) %>% 
        mutate(weight_turnout = hu_2019_weight) %>% 
        mutate(weight_turnout_norm = weight_turnout / sum(weight_turnout) * 1000)
    }
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
        swd_pre = swd_w1, 
        swd_post = swd_w3,
        swd_diff = swd_w3 - swd_w1,
        voted_1r = case_when(
          Q21XX == "Sunt sigur ca am votat" ~ 1,
          Q21XX %in% c("Nu am votat la turul 1 al alegerilor prezidentiale din 22 no", 
                       "M-am gandit sa votez de aceasta data, dar nu am votat", 
                       "De obicei votez, dar de aceasta data nu am votat") ~ 0,
          Q21XX %in% c("NS", "NR") ~ NA_integer_
        ),
        voted = case_when(
          Q23XX == "Sunt sigur ca am votat" ~ 1, 
          Q23XX %in% c("Nu am votat la turul 2 al alegerilor prezidentiale din 6 dec", 
                       "M-am gandit sa votez de aceasta data, dar nu am votat", 
                       "De obicei votez, dar de aceasta data nu am votat") ~ 0, 
          Q23XX %in% c("NS", "NR") ~ NA_integer_
        ),
        voted_2r = voted,
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
        ), 
        stable_voter = same_candidate_in_2r,
        female = as.numeric(GEN == "Femeie"), 
        age = as.numeric(as.character(VARSTA)), 
        postsecondary_edu = case_when(
          EDUC %in% c("scoala postliceala", "facultate neterminata", 
                      "facultate - subingineri sau colegiu", "facultate complet", 
                      "masterat", "doctorat") ~ 1, 
          EDUC %in% c("NS", "NR") ~ NA_integer_, 
          TRUE ~ 0
        ), 
        pol_interest_cat = case_when(
          c1_a %in% c("NS", "NR") ~ NA_character_, 
          c1_a == "deloc interesat" ~ "not at all interested", 			
          c1_a == "putin interesat" ~ "little interested", 
          c1_a == "destul de interesat" ~ "quite interested", 
          c1_a == "foarte interesat" ~ "very interested"
       ) %>% factor(., levels = c(
         "not at all interested", "little interested", "quite interested", 
         "very interested"), ordered = TRUE), 
       pol_interest_num = as.numeric(pol_interest_cat), 
       pol_knowledge = cun_pol1,
       pol_knowledge_pct = (pol_knowledge / 8) * 100, 
       party_close = as.numeric(Q4 == "Da" | Q4A == "Da"), 
       winner_1r = case_when(
         Q21B == "Traian Basescu" ~ "winner", 
         Q21B == "Mircea Geoana" ~ "qualified to 2nd round", 
         Q21B %in% c("Crin Antonescu", "Sorin Oprescu", 
                     "Corneliu Vadim Tudor", " Kelemen Hunor", 
                     "George Becali", "Alt candidat") ~ "loser", 
         Q21X != "Sunt sigur ca am votat" ~ "didn't vote"
       ), 
       winner_2r = case_when(
         Q24B == "Traian Basescu" ~ "winner", 
         Q24B == "Mircea Geoana" ~ "loser", 
         Q23XX != "Sunt sigur ca am votat" ~ "didn't vote"
       )
       )
  ),
  
  tar_target(
    ro_2009_weight, {
      ro_2009_weight <- calc_weight_turnout_presidential(ro_2009_final, turnout_df, "RO 2009")
      ro_2009_final %>% 
        filter(!is.na(swd_diff)) %>% 
        filter(!is.na(voted_1r)) %>% 
        filter(!is.na(voted_2r)) %>% 
        mutate(weight_turnout = ro_2009_weight,
               weight_turnout_norm = weight_turnout / sum(weight_turnout) * 1000)
    }
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
        swd_pre = swd_w1, 
        swd_post = swd_w3,
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
        winner = case_when(
          Q5LH_CS_w3 == "Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)" | 
            Q5LH_CD_w3 == "Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)" ~ 1, 
          Q5LH_CS_w3 %in% c("Refuz", "Nu stiu") |
            Q5LH_CD_w3 %in% c("Refuz", "Nu stiu") ~ NA_integer_, 
          !is.na(Q5LH_CS_w3) ~ 0
          ), 
        winner_government = winner,
        stable_intention = as.numeric(as.character(P12V) == as.character(Q5LH_CS_w3)), 
        stable_voter = case_when(
            # TODO
            Q5LH_C3_w3 %in% c("Refuz", "Nu stiu") | 
                Q5LH_CD_w3 %in% c("Refuz", "Nu stiu") ~ NA_integer_,
            Q5LH_C3_w3 %in% c("Vot invalid (alb/anulat)") | 
                Q5LH_CD_w3 %in% c("Vot invalid (alb/anulat)") ~ 0, 
            as.character(Q5LH_C3_w3) == as.character(Q5LH_CD_w3) ~ 1,
            Q5LH_C3_w3 %in% c("Partidul National Liberal (PNL)", "Partidul Social Democrat (PSD)", 
                                  "Partidul Conservator (PC)") &
                Q5LH_CD_w3 %in% c("Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)") ~ 1,
            Q5LH_C3_w3 == "Alianta PSD+PC" & 
                Q5LH_CD_w3 %in% c("Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)") ~ 1,
            Q5LH_C3_w3 %in% c("Partidul Democrat Liberal (PDL)", 
                                  "Partidul National Taranesc Crestin Democrat (PNTCD)") &
                Q5LH_CD_w3 %in% c("Alianta România Dreapta (ARD: PDL+PNT-CD+FC)") ~ 1,
            TRUE ~ 0
        ),
        vote_previous_nat_election = Q5LH_C3_w3,
        vote_choice = Q5LH_CD_w3, 
        female = as.numeric(GEN == "Femeie"), 
        age = varsta, 
        postsecondary_edu = as.numeric(r_EDUC == "Higher education"), 
        pol_interest_cat = case_when(
          IP1_1 %in% c("Nu stiu", "Nu raspund") ~ NA_character_,
          IP1_1 == "Deloc interesat" ~ "not at all interested", 			
          IP1_1 == "Putin interesat" ~ "little interested", 
          IP1_1 == "Destul de interesat" ~ "quite interested", 
          IP1_1 == "Foarte interesat" ~ "very interested"
        ) %>% factor(., levels = c("not at all interested", "little interested", "quite interested", 
                                   "very interested"), 
                     ordered = TRUE), 
        pol_interest_num = as.numeric(pol_interest_cat),
        party_close = as.numeric(Q4 == "Da" | Q4a == "Da")
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
        swd_pre = swd_w2, 
        swd_post = swd_w3,
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
        # r_age = if_else(age_w2 == 0, NA_integer_, as.numeric(age_w2)), 
        winner = case_when(
          Q5LH_CS_w3 == "Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)" | 
            Q5LH_CD_w3 == "Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)" ~ 1, 
          Q5LH_CS_w3 %in% c("Refuz", "Nu stiu") |
            Q5LH_CD_w3 %in% c("Refuz", "Nu stiu") ~ NA_integer_, 
          !is.na(Q5LH_CS_w3) ~ 0
        ), 
        stable_voter = case_when(
            Q5LH_CD2008_w3 %in% c("Refuz", "Nu stiu") | 
                Q5LH_CD_w3 %in% c("Refuz", "Nu stiu") ~ NA_integer_,
            Q5LH_CD2008_w3 %in% c("Vot invalid (alb/anulat)") | 
                Q5LH_CD_w3 %in% c("Vot invalid (alb/anulat)") ~ 0, 
            as.character(Q5LH_CD2008_w3) == as.character(Q5LH_CD_w3) ~ 1,
            Q5LH_CD2008_w3 %in% c("Partidul National Liberal (PNL)", "Partidul Social Democrat (PSD)", 
                                  "Partidul Conservator (PC)") &
                Q5LH_CD_w3 %in% c("Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)") ~ 1,
            Q5LH_CD2008_w3 == "Alianta PSD+PC" & 
                Q5LH_CD_w3 %in% c("Uniunea Social-Liberala (USL: PSD+PNL+PC+UNPR)") ~ 1,
            Q5LH_CD2008_w3 %in% c("Partidul Democrat Liberal (PDL)", 
                                  "Partidul National Taranesc Crestin Democrat (PNTCD)") &
                Q5LH_CD_w3 %in% c("Alianta România Dreapta (ARD: PDL+PNT-CD+FC)") ~ 1,
            TRUE ~ 0
        ),
        vote_previous_nat_election = Q5LH_CD2008_w3,
        vote_choice = Q5LH_CD_w3, 
        stable_intention = as.numeric(as.character(P12V_w2) == as.character(Q5LH_CS_w3)), 
        GEN = GEN_w2, 
        female = as.numeric(GEN == "Femeie"), 
        age = as.numeric(as.character(age_w2)), 
        postsecondary_edu = as.numeric(r_EDUC == "Higher education"), 
        pol_interest_cat = case_when(
          IP1_a_w2 %in% c("Nu stiu", "Nu raspund") ~ NA_character_,
          IP1_a_w2 == "Deloc interesat" ~ "not at all interested", 			
          IP1_a_w2 == "Putin interesat" ~ "little interested", 
          IP1_a_w2 == "Destul de interesat" ~ "quite interested", 
          IP1_a_w2 == "Foarte interesat" ~ "very interested"
        ) %>% factor(., levels = c("not at all interested", "little interested", "quite interested", 
                                   "very interested"), 
                     ordered = TRUE), 
        pol_interest_num = as.numeric(pol_interest_cat),
        party_close = as.numeric(Q4_w2 == "Da" | Q4a_w2 == "Da")
      )
  ),
  
  tar_target(
    ro_2012_all, {
      COMMON_VARS <- c("ID", "r_EDUC", "age", "swd_w2", "swd_w3", "swd_diff_w2", 
                       "swd_pre", "swd_post", "pol_interest_num",
                       "winner", "stable_voter", "stable_intention", 
                       "voted", "female", "vote_previous_nat_election", 
                       "vote_choice", "postsecondary_edu", "party_close")
      bind_rows(
        ro_2012_p1_final %>% select(any_of(COMMON_VARS)), 
        ro_2012_p2_final %>% select(any_of(COMMON_VARS))  
      )
    }
  ),
  
  
  tar_target(
    ro_2012_weight, {
      ro_2012_all2 <- ro_2012_all %>% 
        mutate(swd_diff = swd_post - swd_pre)
      ro_2012_weight <- calc_weight_turnout(ro_2012_all2, turnout_df, "RO 2012")
      ro_2012_all2 %>% 
        filter(!is.na(swd_diff)) %>% 
        filter(!is.na(voted)) %>% 
        mutate(weight_turnout = ro_2012_weight) %>% 
        mutate(weight_turnout_norm = weight_turnout / sum(weight_turnout) * 1000)
    }
  ),
  
  # DE 2017 -----------------------------------------------
  
  tar_target(
    de_2017_final, 
    read_sav(here("data", "DE_2017", "ZA6838_w1to9_sA_v5-0-0.sav"), user_na = TRUE) %>% 
        haven::as_factor() %>% 
        rename(birth_year = kpx_2290s) %>% 
        filter(kp9_2601 %in% c("Mecklenburg-Vorpommern", "Sachsen", 
                               "Sachsen-Anhalt", "Thueringen", 
                               "Brandenburg", "Berlin")) %>% 
        mutate(
            birth_year = case_when(
                birth_year == "1955 und frueher" ~ 1955, 
                TRUE ~ as.numeric(birth_year)
            ), 
            age = 2017 - birth_year, 
            female = as.numeric(kpx_2280 == "weiblich"),
            postsecondary_edu = as.numeric(
                kp1_2330 %in% c("Fachhochschulabschluss", "Hochschulabschluss") 
            ),
            swd_post = case_when(
                kp8_020 == "sehr zufrieden" ~ 2, 
                kp8_020 == "zufrieden" ~ 1, 
                kp8_020 == "teils/teils" ~ 0,
                kp8_020 == "unzufrieden" ~ -1,
                kp8_020 == "sehr unzufrieden" ~ -2, 
                TRUE ~ NA_integer_
            ), 
            swd_pre = case_when(
                kp5_020 == "sehr zufrieden" ~ 2, 
                kp5_020 == "zufrieden" ~ 1, 
                kp5_020 == "teils/teils" ~ 0,
                kp5_020 == "unzufrieden" ~ -1,
                kp5_020 == "sehr unzufrieden" ~ -2, 
                TRUE ~ NA_integer_
            ), 
            swd_diff = swd_post - swd_pre, 
            voted_post = rowSums(across(ends_with("_170"), ~.x == "habe bereits Briefwahl gemacht"), na.rm = TRUE), 
            voted = case_when(
                kp8_180 %in% c(
                    "gewaehlt", 
                    "hatte bereits Briefwahl gemacht") ~ 1,
                voted_post == 1 ~ 1,
                # hlasování poštou
                kp8_180 == "habe nicht gewaehlt" ~ 0, 
                TRUE ~ NA_integer_
            ), 
            winner_party = case_when(
                kp8_200ba == "CDU/CSU" | kp8_200bb == "CDU/CSU" ~ 1,
                TRUE ~ 0
            ), 
            winner = winner_party,
            winner_government = case_when(
                kp8_200ba %in% c("CDU/CSU", "SPD") | 
                    kp8_200bb %in% c("CDU/CSU", "SPD") ~ 1,
                TRUE ~ 0
            ),
            vote_intention = as_factor(kp7_190ba),
            vote_previous_nat_election = as_factor(kp1_350ba),
            vote_choice = as_factor(kp8_200ba), 
            stable_voter = case_when(
                kp1_2780 %in% c("nein, ich habe nicht gewaehlt", "nicht wahlberechtigt") ~ 0,
                vote_choice %in% c("keine Angabe", "trifft nicht zu", 
                                       "Interview abgebrochen", "andere Partei") | 
                    vote_previous_nat_election %in% c("keine Angabe", "weiss nicht", 
                                                "andere Partei", "nicht teilgenommen") ~ NA_integer_,
                as.character(vote_choice) == as.character(vote_previous_nat_election) ~ 1,
                TRUE ~ 0
            ), 
            stable_intention = case_when(
                as.character(vote_choice) == as.character(vote_intention) ~ 1, 
                TRUE ~ 0
            ),
            vote_duty = case_when(
                kp8_050l %in% c("keine Angabe", "Interview abgebrochen", 
                                "nicht teilgenommen") ~ NA_character_, 
                TRUE ~ as.character(kp8_050l)
            ) %>% factor(., levels = c(
                "stimme ueberhaupt nicht zu", "stimme eher nicht zu", 
                "teils/teils", "stimme eher zu", "stimme voll und ganz zu"
            ), ordered = TRUE), 
            party_close = case_when(
                kp8_2090a %in% c("keine Angabe", "Interview abgebrochen", 
                                 "nicht teilgenommen") ~ NA_integer_, 
                kp8_2090a != "keine Partei" ~ 1, 
                kp8_2090a == "keine Partei" ~ 0 
            ),
            pol_interest_cat = case_when(
                kp7_010 %in% c("keine Angabe", "nicht teilgenommen") ~ NA_character_, 
                TRUE ~ kp7_010
            ) %>% factor(., levels = rev(c(
                "sehr stark", "stark", "mittelmaessig", 
                "weniger stark", "ueberhaupt nicht")), 
                ordered = TRUE), 
            pol_interest_num = as.numeric(pol_interest_cat), 
            pol_know1 = case_when(
                kp1_090_v1 %in% c("nicht teilgenommen", 
                                  "Interview abgebrochen") ~ NA_integer_, 
                kp1_090_v1 == "Richtige Antwort (5%)" ~ 
                    1, 
                TRUE ~ 0
            ), 
            pol_know2 = case_when(
                kp1_110 %in% c("nicht teilgenommen", 
                               "Interview abgebrochen") ~ NA_integer_,
                kp1_110 == "Zweitstimme*" ~ 1,
                TRUE ~ 0
            ), 
            pol_know3 = case_when(
                kp1_130 %in% c("nicht teilgenommen", 
                               "Interview abgebrochen") ~ NA_integer_, 
                kp1_130 == "den Bundestag*" ~ 1, 
                TRUE ~ 0
            ), 
            pol_know4 = case_when(
                kp2_3430q %in% c("nicht teilgenommen", 
                               "Interview abgebrochen") ~ NA_integer_, 
                grepl("\\*", kp2_3430q) ~ 1, 
                TRUE ~ 0
            ), 
            pol_know5 = case_when(
                kp2_3430c %in% c("nicht teilgenommen", 
                                 "Interview abgebrochen") ~ NA_integer_, 
                grepl("\\*", kp2_3430c) ~ 1, 
                TRUE ~ 0
            ), 
            pol_know6 = case_when(
                kp2_3430l %in% c("nicht teilgenommen", 
                                 "Interview abgebrochen") ~ NA_integer_, 
                grepl("\\*", kp2_3430l) ~ 1, 
                TRUE ~ 0
            ), 
            pol_know7 = case_when(
                kp2_3430p %in% c("nicht teilgenommen", 
                                 "Interview abgebrochen") ~ NA_integer_, 
                grepl("\\*", kp2_3430p) ~ 1, 
                TRUE ~ 0
            ), 
            pol_know8 = case_when(
                kp2_3430m %in% c("nicht teilgenommen", 
                                 "Interview abgebrochen") ~ NA_integer_, 
                grepl("\\*", kp2_3430m) ~ 1, 
                TRUE ~ 0
            ), 
            pol_know9 = case_when(
                kp2_3430r %in% c("nicht teilgenommen", 
                                 "Interview abgebrochen") ~ NA_integer_, 
                grepl("\\*", kp2_3430r) ~ 1, 
                TRUE ~ 0
            ), 
            pol_know10 = case_when(
                kp2_3440 %in% c("nicht teilgenommen", 
                                 "Interview abgebrochen") ~ NA_integer_, 
                grepl("\\*", kp2_3440) ~ 1, 
                TRUE ~ 0
            ), 
            pol_know = rowSums(across(matches("pol_know[0-9]+"))), 
            pol_knowledge_pct = (pol_know / 10) * 100
        )
  ),
  
  tar_target(
    de_2017_weight, {
      de_2017_weight <- calc_weight_turnout(de_2017_final, turnout_df, "DE 2017 (East)")
      de_2017_final %>% 
        filter(!is.na(swd_diff)) %>% 
        filter(!is.na(voted)) %>% 
        mutate(weight_turnout = de_2017_weight) %>% 
        mutate(weight_turnout_norm = weight_turnout / sum(weight_turnout) * 1000)
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
        ), 
        vote_intention_w18 = as_factor(kp18_190ba),
        vote_choice_w20 = as_factor(kp20_200ba), 
        stable_voter = case_when(
          vote_choice_w20 %in% c("keine Angabe", "trifft nicht zu", 
                                 "Interview abgebrochen", "andere Partei") | 
            vote_intention_w18 %in% c("keine Angabe", "trifft nicht zu", 
                                   "Interview abgebrochen", "andere Partei") ~ NA_integer_,
          as.character(vote_choice_w20) == as.character(vote_intention_w18) ~ 1,
          TRUE ~ 0
        ), 
        female = as.numeric(sex == "female"), 
        party_close = case_when(
          kp18_2090a %in% c("keine Angabe", "Interview abgebrochen") ~ NA_integer_, 
          kp18_2090a != "keiner Partei" ~ 1, 
          kp18_2090a == "keiner Partei" ~ 0 
        ), 
        party_interest_cat = case_when(
          kp18_010 %in% c("keine Angabe") ~ NA_character_, 
          TRUE ~ kp18_010
        ) %>% factor(., levels = c(
          "sehr stark", "stark", "mittelmaessig", 
          "weniger stark", "ueberhaupt nicht"), 
          ordered = TRUE), 
        party_interest_num = as.numeric(party_interest_cat), 
        vote_duty = case_when(
          kp18_050l %in% c("keine Angabe", "Interview abgebrochen") ~ NA_character_, 
          TRUE ~ kp18_050l
        ) %>% factor(., levels = c(
          "stimme ueberhaupt nicht zu", "stimme eher nicht zu", 
          "teils/teils", "stimme eher zu", "stimme voll und ganz zu"
        ), ordered = TRUE)
      )
  ),
  
  # All panels --------------------------------------------------------------
  tar_target(
      all_panels, command = {
          common_vars <- c("age", "swd_diff", "postsecondary_edu",
                           "winner", "winner_government",
                           "stable_voter", "stable_intention", 
                           "voted", "female", "vote_previous_nat_election", 
                           "vote_choice", "swd_pre", "swd_post", 
                           "pol_knowledge_pct", "pol_interest_num", 
                           "party_close", "candidate_qualified_2nd", 
                           "duty_to_vote", "winner_1r", "winner_2r", 
                           "weight_turnout", "weight_turnout_norm")
          
          cz_1996_norm <- cz_1996_weight %>% 
              select(any_of(common_vars)) %>% 
              mutate(election = "CZ 1996") %>% 
              mutate(across(c("swd_pre", "swd_post", "pol_interest_num"), 
                            normalize_scale), 
                     swd_diff = swd_post - swd_pre)
          
          pl_2019_norm <- pl_2019_weight %>% 
              select(any_of(common_vars)) %>% 
              mutate(election = "PL 2019") %>% 
              mutate(across(c("swd_pre", "swd_post", "pol_interest_num"), 
                            normalize_scale), 
                     swd_diff = swd_post - swd_pre)
          
          hu_2019_norm <- hu_2019_weight %>% 
              select(any_of(common_vars)) %>% 
              mutate(election = "HU 2019") %>% 
              mutate(across(c("swd_pre", "swd_post", "pol_interest_num"), 
                            normalize_scale), 
                     swd_diff = swd_post - swd_pre)
          
          ro_2009_norm <- ro_2009_weight %>% 
              select(any_of(common_vars)) %>% 
              mutate(election = "RO 2009") %>% 
              mutate(across(c("swd_pre", "swd_post", "pol_interest_num"), 
                            normalize_scale), 
                     swd_diff = swd_post - swd_pre)
          
          ro_2012_norm <- ro_2012_weight %>% 
              select(any_of(common_vars)) %>% 
              mutate(election = "RO 2012") %>% 
              mutate(across(c("swd_pre", "swd_post", "pol_interest_num"), 
                            normalize_scale), 
                     swd_diff = swd_post - swd_pre)
          
          de_2017_norm <- de_2017_weight %>% 
              select(any_of(common_vars)) %>% 
              mutate(election = "DE (Ost) 2017") %>% 
              mutate(across(c("swd_pre", "swd_post", "pol_interest_num"), 
                            normalize_scale), 
                     swd_diff = swd_post - swd_pre)
          
          cz_2023_norm <- cz_2023_weight %>% 
            select(any_of(common_vars)) %>% 
            mutate(election = "CZ 2023") %>% 
            mutate(across(c("swd_pre", "swd_post", "pol_interest_num"), 
                          normalize_scale), 
                   swd_diff = swd_post - swd_pre)
          
          bind_rows(
              cz_1996_norm, 
              pl_2019_norm, 
              hu_2019_norm,
              ro_2009_norm, 
              ro_2012_norm, 
              de_2017_norm, 
              cz_2023_norm
          ) %>% 
            mutate(
              pol_knowledge_pct = pol_knowledge_pct / 100, 
              election = factor(election), 
              election_type = case_when(
                election %in% c("CZ 2023", "RO 2009") ~ "presidential", 
                election %in% c("PL 2019", "HU 2019") ~ "EP election", 
                TRUE ~ "parliamentary"
              ) %>% factor(., levels = c("parliamentary", "presidential", 
                                         "EP election")), 
              nonvoter = 1 - voted, 
              voter_type = case_when(
                nonvoter == 1 ~ "Abstainer", 
                winner == 0 ~ "Voted for loser", 
                winner == 1 ~ "Voted for winner"
              ) %>% factor(., levels = c("Abstainer", "Voted for winner", "Voted for loser")), 
              prez_vote_type = 
                case_when(
                  winner_1r == "didn't vote" | winner_2r == "didn't vote" ~ "didn't vote (at least in one round)", 
                  TRUE ~ paste0(winner_1r, " + ", winner_2r)
                ) %>% factor()
              )
      }
  ),
  
  tar_target(
    all_panels_data, command = {
      all_panels
      haven::write_sav(data = all_panels, path = "data/all_panels_merged.sav") 
    }
  ),
  
  # Datasets for models 
  tar_target(
    cz_2023_all_waves_resp, 
    command = {
      cz_2023_models %>% 
      filter(w1 == 1 & w2 == 1 & w3 == 1 & w4 == 1) %>% 
      rename(pol_knowledge = pol_knowledge_pct) %>% 
      mutate(prez_vote_type = case_when(
        PRES2023CAND1r_w3 == "nevolič" & 
          PRES2023CAND2r_w4 == "nevolič" ~ "abstainer",
        PRES2023CAND1r_w3 == "Petr Pavel" & 
          PRES2023CAND2r_w4 == "Petr Pavel" ~ "full winner",
        PRES2023CAND1r_w3 == "Andrej Babiš" & 
          PRES2023CAND2r_w4 == "Andrej Babiš" ~ "full loser",
        PRES2023CAND2r_w4 == "Petr Pavel" ~ "sub-optimal winner",
        PRES2023CAND2r_w4 == "Andrej Babiš" ~ "partial loser",
        PRES2023CAND1r_w3 %in% c("Petr Pavel", "Andrej Babiš") &
          PRES2023CAND2r_w4 == "nevolič" ~ "partial loser",
        !PRES2023CAND1r_w3 %in% c("Petr Pavel", "Andrej Babiš") &
          PRES2023CAND2r_w4 == "nevolič" ~ "full loser"
      )) %>% 
      mutate(prez_vote_type = factor(
        prez_vote_type, 
        levels = c("abstainer", 
                   "full winner", 
                   "sub-optimal winner",
                   "partial loser",
                   "full loser"))) %>% 
      mutate(across(c("swd_pre", "swd_post", "pol_interest_num",
                      "age", "SWD_w1", "SWD_w2", "SWD_w3", "SWD_w4"), 
                    normalize_scale), 
             swd_diff = swd_post - swd_pre) %>% 
        mutate(pavel_decision = case_when(
          IFPRESCAND_1_w1 == "Petr Pavel" & 
            IFPRESCAND_2_w2 == "Petr Pavel" & 
            PRES2023CAND1r_w3 == "Petr Pavel" ~ "V 1. vlně",
          IFPRESCAND_2_w2 == "Petr Pavel" & 
            PRES2023CAND1r_w3 == "Petr Pavel" ~ "V 2. vlně",
          PRES2023CAND1r_w3 == "Petr Pavel" ~ "V 3. vlně", 
          PRES2023CAND2r_w4 == "Petr Pavel" ~ "V 4. vlně"
        ) %>% factor(., levels = c("V 1. vlně", "V 2. vlně", 
                                   "V 3. vlně", "V 4. vlně"))) %>% 
        mutate(
          stable_voter = as.numeric(pavel_decision == "V 1. vlně")
        )
  }),
  
  tar_target(
    cz_2023_model_data, 
    saveRDS(cz_2023_all_waves_resp, "output/data/cz_2023_data_for_models.rds")
  ),
  
  tar_target(
    cz_2023_model_data_dta, 
    haven::write_dta(cz_2023_all_waves_resp, "output/data/cz_2023_data_for_models.dta")
  ),
  
  tar_target(
    all_panels_both_waves, command = { 
      all_panels %>% 
      filter(!is.na(swd_pre) & !is.na(swd_post)) %>% 
      filter(!is.na(female) & !is.na(age) & 
               !is.na(postsecondary_edu) & 
               !is.na(pol_interest_num) & 
               !is.na(party_close)) %>% 
      filter(election != "CZ 2023") %>% 
      mutate(across(c("age"), normalize_scale))
  }),
  
  tar_target(
    all_panels_model_data, 
    saveRDS(all_panels_both_waves, "output/data/all_panels_data_for_models.rds")
  ),
  
  tar_target(
    all_panels_model_data_dta, 
    haven::write_dta(all_panels_both_waves, "output/data/all_panels_data_for_models.dta")
  ),
  
  tar_target(
    cz_2023_long_file, 
    saveRDS(cz_2023_long, "output/data/cz_2023_long_data_for_models.rds")
  ),
  
  tar_target(
    cz_2023_long_file_dta, 
    haven::write_dta(cz_2023_long, "output/data/cz_2023_long_data_for_models.dta")
  ),

  # Tables ------------------------------------------------------------------
  tar_render(
    weights_check, 
    "weights.Rmd"
  ),
  
  tar_render(
    swd_final_rmd,
    "swd_final.Rmd"
  )
)

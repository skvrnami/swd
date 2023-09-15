library(dplyr)
library(lme4)
library(ggplot2)

long_w1_w5 <- readRDS("~/github/swd/data/CZ_2023/long_w1_w5.rds") %>% 
    haven::as_factor() %>% 
    mutate(SWD_num = as.numeric(SWD) - 1, 
           wave = as.factor(wave)) %>% 
    group_by(RADIOMETER_ID2) %>% 
    mutate(# SWD_wi = SWD_num - mean(SWD_num, na.rm = TRUE), 
           # SWD_bw = mean(SWD_num),
           winner = case_when(
               any(PRES2023PART2 == "Ne") ~ "nevolič",
               any(PRES2023CAND2r == "Petr Pavel") ~ "winner", 
               TRUE ~ "loser"
           ) %>% factor(., levels = c("nevolič", "winner", "loser")), 
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
               fst_round == "nevolič" | winner == "nevolič" ~ "didn't vote (at least in one round)", 
               TRUE ~ paste0(winner_1r, "+", winner)
           ), 
           winner_stable = case_when(
               fst_round == "nevolič" & winner == "nevolič" ~ "stable non-voter",
               fst_round == "Petr Pavel" & winner == "winner" ~ "stable winner", 
               fst_round == "Andrej Babiš" & winner == "loser" ~ "stable loser", 
               winner == "winner" ~ "unstable winner", 
               winner == "loser" ~ "unstable loser",
               fst_round == "nevolič" | winner == "nevolič" ~ "unstable non-voter",
           )) %>% 
    ungroup %>% 
    filter(n_waves == 5)

table(long_w1_w5$winner, useNA = "always")

m1 <- lmer(SWD_num ~ winner * wave + (1 | RADIOMETER_ID2), 
     data = long_w1_w5) 

ggeffects::ggeffect(m1, c("wave", "winner")) %>% plot()
modelsummary::modelsummary(m1, stars = TRUE)

m2 <- lmer(SWD_num ~ winner_stable * wave + (1 | RADIOMETER_ID2), 
           data = long_w1_w5) 

ggeffects::ggeffect(m2, c("wave", "winner_stable")) %>% plot()
modelsummary::modelsummary(m2, stars = TRUE)

m3 <- lmer(SWD_num ~ vote_gov * wave + (1 | RADIOMETER_ID2), 
           data = long_w1_w5) 

ggeffects::ggeffect(m3, c("wave", "vote_gov")) %>% plot()
modelsummary::modelsummary(m3, stars = TRUE)
# brms::brm()
library(tidyverse)
library(broom)
library(gtsummary)
library(gtreg)


df <- read_rds("data/df_analysis.RDS")
#

# Method section stuff ----------------------------------------------------

# how many excluded from primary analysis, and for what reasons?
df %>% 
    select(exclude) %>% 
    table

# same but by year
df %>% 
    group_by(election_year) %>% 
    select(exclude) %>% 
    table

# how many get removed for any of the following secondary models?
df %>% 
    filter(exclude == 0) %>% 
    select(iv_triv,            # 62
           iv_prox_sup,        # 12
           iv_prox_sup_state,  # 85
           dv_vp_polz) %>%     # 1152
    map(~is.na(.x) %>% table)


# percentage of responses within X days 
# 50% @ 12 days, 75% @ 22
df %>% 
    filter(exclude == 0) %>% 
    group_by(cov_days) %>% 
    count %>% 
    ungroup %>% 
    mutate(cumulative_pct = cumsum(n)/sum(n)) %>% 
    print(n = 40)


# demographics ------------------------------------------------------------

df %>% 
    filter(exclude == 0) %>% 
    select(election_year, 
           cov_party3_f,
           demog_age,
           demog_race,
           cov_edu_recode_f,
           demog_marital,
           cov_pk,
           cov_days,
           voted_early,
           cov_voted_f,
           pref_switcher,
           cov_expect_f,
           cdv_groups
           ) %>% 
    mutate(pref_switcher = case_when(is.na(pref_switcher) ~ "no",
                                     TRUE ~ "yes"),
           cdv_groups = stringr::str_to_sentence(cdv_groups),
           demog_marital = stringr::str_to_sentence(demog_marital),
           cov_edu_recode_f = case_when(cov_edu_recode_f == "lt_hs" ~ "Less than high school",
                                        cov_edu_recode_f == "hs_grad" ~ "High school grad",
                                        cov_edu_recode_f == "lt_bach" ~ "Less than bachelor's",
                                        cov_edu_recode_f == "bach" ~ "Bachelor's",
                                        cov_edu_recode_f == "master" ~ "Master's",
                                        cov_edu_recode_f == "advanced" ~ "Advanced degree"),
           cov_edu_recode_f = factor(cov_edu_recode_f,
                                     levels = c(
                                         "Less than high school",
                                         "High school grad",
                                         "Less than bachelor's",
                                         "Bachelor's",
                                         "Master's",
                                         "Advanced degree"
                                     )),
           cov_expect_f = ifelse(cov_expect_f == "loser", "yes", "no"),
           cov_party3_f = factor(case_when(cov_party3_f == "ind" ~ "Ind.",
                                    cov_party3_f == "gop" ~ "Rep.",
                                    cov_party3_f == "dem" ~ "Dem."),
                          levels = c("Dem.", "Rep.", "Ind."))) %>% 
     tbl_strata(
         strata = election_year,
         .header = "**{strata}**  (N = {n})",
         ~.x %>%
    tbl_summary(by = cov_party3_f, 
                
                type = cov_pk ~ "continuous",
        
                statistic = list(
                    all_continuous() ~ "{mean} ({sd})",
                    all_categorical() ~ "{n} ({p}%)"),
                
                sort =  list(
                    c(demog_race, cdv_groups, demog_marital) ~ "frequency"
                ),
                
                label = 
                    c(demog_age ~ "Age",
                      demog_marital ~ "Marital status",
                      cov_party3_f ~ "Party",
                      demog_race ~ "Race/ethnicity",
                      cov_pk ~ "Political knowledge",
                      cov_days ~ "Post-election time (days)",
                      cov_voted_f ~ "Voted",
                      voted_early ~ "Voted early",
                      pref_switcher ~ "Switched preference",
                      cov_edu_recode_f ~ "Education",
                      cov_expect_f ~ "Expected loser",
                      cdv_groups ~ "Difficulty/valence group"
                      ),
                
                missing = "no") %>% 
        bold_labels() %>% 
        italicize_levels() %>% 
        modify_footnote(all_stat_cols() ~ NA) %>% 
        modify_header(all_stat_cols() ~ "**{level}**<br>n = {n}") %>%
        modify_caption("**Table 1. Respondent Demographics and Study Characteristics by Year and Party**") 
        
     )  
     

# H1/H2 -------------------------------------------------------------------

df %>% 
    filter(
        exclude == 0,
        cov_party_f %in% c("dem", "gop"),
           ) %>% 
    group_by(election_year) %>% 
    nest() %>% 
    mutate(h1_h2_model = map(data,
                             ~lm(dv_cand_polz ~ (iv_cand_pref_f * iv_cdv_f) + 
                                     cov_expect_f + cov_pre_polz + cov_party_n + 
                                     cov_pk + cov_edu_recode_n + cov_days,
                                 data = .x)
                             ),
           h1_h2_tidy = map(h1_h2_model, ~tidy(.x, conf.int = T)),
           
           h1_h2_dummy_model = map(data,
                             ~lm(dv_cand_polz ~ dummy_pref_loser + dummy_cdv_ref_easy + dummy_cdv_ref_neg + 
                                     dummy_expect + cent_cov_pk_m + dummy_party_ref_dem + 
                                     cent_cov_pk_m + dummy_edu_3_ltbach_ref + cov_days,
                                 data = .x)
           ),
           h1_h2__dummy_tidy = map(h1_h2_dummy_model, ~tidy(.x, conf.int = T))
           
           ) %>% 
    select(election_year, h1_h2_tidy) %>% 
    unnest(h1_h2_tidy) %>% 
    #write_csv("")
    
    
    
    
    
    ggplot(aes(x = term)) + theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
    facet_grid(~election_year) + 
    geom_hline(yintercept = 0) + 
    coord_flip()


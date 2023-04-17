# Analysis script for dissonance dissertation
# Author: Austin Eubanks (AEubanks@gmail.com)

# libraries ---------------------------------------------------------------
library(gtsummary)
library(gt)
library(tidyverse)
library(stringr)
library(broom)
library(huxtable)
library(corrr)
library(naniar)

# Other in-line libraries that are used: 
# lmtest, sandwich, jtools, ggtext, trelliscopejs, psych, mosaic

# read in data ------------------------------------------------------------

# This script begins with "df_analysis.RDS," which is generated from the raw
# data in the script "cleanup.R"

df <- read_rds("data/df_analysis.RDS")

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
           iv_prox_sup) %>%    # 85 
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

# to export these tables, you (annoyingly) need to run the code below which will
# make the html files. Open those in a browser, then copy and paste into GOOGLE
# SHEETS (not excel). Then, copy from google sheets and paste into excel. If you
# try going straight into excel and skip the google sheets step, excel will
# auto-convert stuff (e.g., "(45%)" becomes "-45%") 

df %>% 
    filter(exclude == 0,
           election_year == "2016") %>% 
    select(
        cov_party3_f,
        demog_age,
        demog_sex,
        demog_race,
        cov_edu_recode_f,
        demog_marital
    ) %>% 
    mutate(
        demog_marital = str_to_sentence(demog_marital),
        demog_marital = factor(demog_marital,
                               levels = c(
                                   "Married",
                                   "Never married",
                                   "Formerly married"
                               )),
        demog_race = factor(demog_race,
                            levels = c(
                                "White",
                                "Black",
                                "Asian",
                                "Hispanic",
                                "Native American/Alaskan",
                                "Other"
                            )),
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
        cov_party3_f = factor(case_when(cov_party3_f == "ind" ~ "Ind.",
                                        cov_party3_f == "gop" ~ "Rep.",
                                        cov_party3_f == "dem" ~ "Dem."),
                              levels = c("Dem.", "Rep.", "Ind."))
    ) %>% 
    tbl_summary(statistic = list(
        all_continuous() ~ "{mean}  \n ({sd})",
        all_categorical() ~ "{n}  \n ({p}%)"),
        
        label = 
            c(demog_age ~ "Age",
              demog_sex ~ "Sex",
              demog_marital ~ "Marital status",
              demog_race ~ "Race/ethnicity",
              cov_edu_recode_f ~ "Education"
            ),
        
        missing = "no",
        
        by = cov_party3_f
        
    ) %>%     
    modify_header(all_stat_cols() ~ "**{level}**<br>n = {n}") %>%
    add_overall(last = T,
                col_label = "**(Total)**<br>N = {N}") %>% 
    bold_labels() %>% 
    italicize_levels() %>% 
    modify_footnote(all_stat_cols() ~ NA) %>% 
    modify_caption("**Table 1. Respondent Demographics by Year and Party**") %>% 
    remove_row_type(
        variables = demog_sex,
        type =  "level",
        level_value = "Other"
    )  %>% 
    as_gt() %>% 
    fmt_markdown(columns = all_stat_cols()) %>% 
    gtsave(file = "tables/demo_tbl_2016.html")


df %>% 
    filter(exclude == 0,
           election_year == "2020") %>% 
    select(
        cov_party3_f,
        demog_age,
        demog_sex,
        demog_race,
        cov_edu_recode_f,
        demog_marital
    ) %>% 
    mutate(
        demog_marital = str_to_sentence(demog_marital),
        demog_marital = factor(demog_marital,
                               levels = c(
                                   "Married",
                                   "Never married",
                                   "Formerly married"
                               )),
        demog_race = factor(demog_race,
                            levels = c(
                                "White",
                                "Black",
                                "Asian",
                                "Hispanic",
                                "Native American/Alaskan",
                                "Other"
                            )),
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
        cov_party3_f = factor(case_when(cov_party3_f == "ind" ~ "Ind.",
                                        cov_party3_f == "gop" ~ "Rep.",
                                        cov_party3_f == "dem" ~ "Dem."),
                              levels = c("Dem.", "Rep.", "Ind."))
    ) %>% 
    tbl_summary(statistic = list(
        all_continuous() ~ "{mean}  \n ({sd})",
        all_categorical() ~ "{n}  \n ({p}%)"),
        
        label = 
            c(demog_age ~ "Age",
              demog_sex ~ "Sex",
              demog_marital ~ "Marital status",
              demog_race ~ "Race/ethnicity",
              cov_edu_recode_f ~ "Education"
            ),
        
        missing = "no",
        
        by = cov_party3_f
        
    ) %>%     
    modify_header(all_stat_cols() ~ "**{level}**<br>n = {n}") %>%
    add_overall(last = T,
                col_label = "**(Total)**<br>N = {N}") %>% 
    bold_labels() %>% 
    italicize_levels() %>% 
    modify_footnote(all_stat_cols() ~ NA) %>% 
    modify_caption("**Table 1. Respondent Demographics by Year and Party**") %>% 
    remove_row_type(
        variables = demog_sex,
        type =  "level",
        level_value = "Other"
    )  %>% 
    as_gt() %>% 
    fmt_markdown(columns = all_stat_cols()) %>% 
    gtsave(file = "tables/demo_tbl_2020.html")


# study characteristics ---------------------------------------------------

df %>% 
    filter(exclude == 0,
           election_year == "2016") %>% 
    select(
        cov_party3_f,
        iv_cand_pref_f,
        cdv_groups,
        iv_prox_sup,
        iv_triv,
        cov_pre_polz,
        cov_pk,
        cov_expect_f,
        cov_voted_f,
        cov_days,
        pref_switcher
    ) %>% 
    mutate(
        cov_party3_f = factor(case_when(cov_party3_f == "ind" ~ "Ind.",
                                        cov_party3_f == "gop" ~ "Rep.",
                                        cov_party3_f == "dem" ~ "Dem."),
                              levels = c("Dem.", "Rep.", "Ind.")),
        pref_switcher = case_when(is.na(pref_switcher) ~ "no", TRUE ~ "yes"),
        iv_cand_pref_f = case_when(iv_cand_pref_f == "loser" ~ "yes", TRUE ~ "no"),
        cdv_groups = str_to_sentence(cdv_groups),
        cdv_groups = factor(cdv_groups,
                            levels = c("Likes both", "Likes one", "Dislikes both")),
        cov_expect_f = ifelse(cov_expect_f == "loser", "yes", "no"),
    ) %>% 
    tbl_summary(
        type = c(cov_pk, iv_prox_sup, iv_triv) ~ "continuous",
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n}  \n ({p}%)"),
        
        label = 
            c(
                cov_pk ~ "Political knowledge",
                cov_days ~ "Post-election time (days)",
                cov_voted_f ~ "Voted",
                pref_switcher ~ "Switched preference",
                cov_expect_f ~ "Expected loser",
                cdv_groups ~ "Difficulty/valence group",
                cov_pre_polz ~ "Pre-election polarization",
                iv_prox_sup ~ "Proximal in-group support",
                iv_cand_pref_f ~ "Preferred loser",
                iv_triv ~ "Trivialization"
            ),
        
        missing = "no",
        
        by = cov_party3_f
        
    ) %>%     
    modify_header(all_stat_cols() ~ "**{level}**<br>n = {n}") %>%
    add_overall(last = T,
                col_label = "**(Total)**<br>N = {N}") %>% 
    bold_labels() %>% 
    italicize_levels() %>% 
    modify_footnote(all_stat_cols() ~ NA) %>% 
    modify_caption("**Table 2. Survery Related Respondent Characteristics by Year and Party**") %>% 
    as_gt() %>% 
    fmt_markdown(columns = all_stat_cols()) %>% 
    gtsave(file = "tables/studytbl2016.html")

df %>% 
    filter(exclude == 0,
           election_year == "2020") %>% 
    select(
        cov_party3_f,
        iv_cand_pref_f,
        cdv_groups,
        iv_prox_sup,
        iv_triv,
        cov_pre_polz,
        cov_pk,
        cov_expect_f,
        cov_voted_f,
        cov_days,
        pref_switcher
    ) %>% 
    mutate(
        cov_party3_f = factor(case_when(cov_party3_f == "ind" ~ "Ind.",
                                        cov_party3_f == "gop" ~ "Rep.",
                                        cov_party3_f == "dem" ~ "Dem."),
                              levels = c("Dem.", "Rep.", "Ind.")),
        pref_switcher = case_when(is.na(pref_switcher) ~ "no", TRUE ~ "yes"),
        iv_cand_pref_f = case_when(iv_cand_pref_f == "loser" ~ "yes", TRUE ~ "no"),
        
        cdv_groups = str_to_sentence(cdv_groups),
        cdv_groups = factor(cdv_groups,
                            levels = c("Likes both", "Likes one", "Dislikes both")),
        cov_expect_f = ifelse(cov_expect_f == "loser", "yes", "no"),
    ) %>% 
    tbl_summary(
        type = c(cov_pk, iv_prox_sup, iv_triv) ~ "continuous",
        
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n}  \n ({p}%)"),
        
        label = 
            c(
                cov_pk ~ "Political knowledge",
                cov_days ~ "Post-election time (days)",
                cov_voted_f ~ "Voted",
                pref_switcher ~ "Switched preference",
                cov_expect_f ~ "Expected loser",
                cdv_groups ~ "Difficulty/valence group",
                cov_pre_polz ~ "Pre-election polarization",
                iv_prox_sup ~ "Proximal in-group support",
                iv_cand_pref_f ~ "Preferred loser",
                iv_triv ~ "Trivialization"
            ),
        
        missing = "no",
        
        by = cov_party3_f
        
    ) %>%     
    modify_header(all_stat_cols() ~ "**{level}**<br>n = {n}") %>%
    add_overall(last = T,
                col_label = "**(Total)**<br>N = {N}") %>% 
    bold_labels() %>% 
    italicize_levels() %>% 
    modify_footnote(all_stat_cols() ~ NA) %>% 
    modify_caption("**Table 2. Survery Related Respondent Characteristics by Year and Party**") %>% 
    as_gt() %>% 
    fmt_markdown(columns = all_stat_cols()) %>% 
    gtsave(file = "tables/studytbl2020.html")

# models ------------------------------------------------------------------

all_mods <- 
    df %>% 
    filter(exclude == 0) %>% 
    mutate(cov_edu_recode_n = cov_edu_recode_n - 3.5) %>% 
    group_by(election_year) %>% 
    nest() %>% 
    mutate(
        ###################### primary models
        mod_1 = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_2 = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_prox_sup + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_3 = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_triv + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        

        
        ###################### secondary models
        
        # add independents back in
        mod_1_ind = 
            map(data,
                ~.x %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + 
                           cov_party3_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_2_ind = 
            map(data,
                ~.x %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_prox_sup + 
                           cov_party3_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_3_ind = 
            map(data,
                ~.x %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_triv + 
                           cov_party3_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        
        
        # no covs
        mod_1_nocov = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f,
                       data = .)),
        
        mod_2_nocov = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_prox_sup,
                       data = .)),
        
        mod_3_nocov = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_triv,
                       data = .)),
        
        
        # remove switchers
        mod_1_noswitch = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0, is.na(pref_switcher)) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_2_noswitch = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0, is.na(pref_switcher)) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_prox_sup + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_3_noswitch = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0, is.na(pref_switcher)) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_triv + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        # add interactions 
        mod_1_int = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ (iv_cand_pref_f * iv_cdv_f) + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_2_int = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ (iv_cand_pref_f * iv_cdv_f) + iv_prox_sup + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_3_int = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ (iv_cand_pref_f * iv_cdv_f) + iv_triv + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        # alternate prox support operationalization
        mod_2_alt = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_prox_sup_alt + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_2_alt_ind = 
            map(data,
                ~.x %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_prox_sup_alt + 
                           cov_party3_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_2_alt_nocov = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_prox_sup_alt,
                       data = .)),
        
        mod_2_alt_noswitch = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0, is.na(pref_switcher)) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_prox_sup_alt + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_2_alt_int = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ (iv_cand_pref_f * iv_cdv_f) + iv_prox_sup_alt + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .))
        
    ) %>% 
    ungroup %>% 
    pivot_longer(-c(election_year, data), 
                 names_to = "model_name", 
                 values_to = "model") %>% 
    mutate(model_type = rep(
        c(rep(c("primary", "B_w_ind", "D_no_cov", "A_no_switch", "C_w_interact"), each = 3), 
            rep("E_alt_sup", 5)), times = 2),
           tidy_mods = map(model, ~tidy(.x, conf.int = T)),
           glanced_mods = map(model, ~glance(.x)),
           augmented_mods = map(model, ~augment(.x)),
        robust = map(model,
                     ~lmtest::coeftest(.x, vcov =sandwich::vcovHC(.x, type = 'HC0'))),
        robust_tbl = map(robust,
                         ~.x[,] %>% 
                             as_tibble() %>%
                             mutate(term_robust = rownames(.x)) %>% 
                             select(term_robust, 
                                    "estimate_robust" = Estimate, 
                                    "std.error_robust" = `Std. Error`, 
                                    "statistic_robust" = `t value`,
                                    "p.value_robust" = `Pr(>|t|)`) %>% 
                             mutate(conf.low_robust = estimate_robust - (1.96 * std.error_robust),
                                    conf.high_robust = estimate_robust + (1.96 * std.error_robust))))


# primary models table ----------------------------------------------------

primary_mod_table <- 
    all_mods %>% 
    filter(model_type == "primary") %>% 
    arrange(model_name) %>%
    mutate(model_name = str_to_sentence(model_name),
           name = paste(election_year, model_name, sep = ":")) %>% 
    select(name, model) %>% 
    deframe() %>% 
    
    huxreg(ci_level = .95, error_format = "[{conf.low}, {conf.high}]",
           
           statistics = c("N" = "nobs",
                          "R sq." = "r.squared",
                          "Adj. R sq." = "adj.r.squared",
                          "df",
                          "df.residual"),
           coefs = c(
               "Intercept" = "(Intercept)",
               "Preference" = "iv_cand_pref_floser",
               "C1: Difficulty" = "iv_cdv_fc1_difficulty",
               "C2: Valence" = "iv_cdv_fc2_valence",
               "Party" = "cov_party_n",
               "Expectation" = "cov_expect_n",
               "Voted" = "cov_voted_n",
               "Pre-pol" = "cent_cov_pre_polz_m",
               "Pol. Knowledge" = "cent_cov_pk_m",
               "Education" = "cov_edu_recode_n",
               "Days" = "cov_days",
               "Trivialization" = "iv_triv",
               "Proximal Support" = "iv_prox_sup"
           ))  

number_format(primary_mod_table) <- 3
quick_xlsx(primary_mod_table, file = "tables/primary_mod_table_round3.xlsx")
number_format(primary_mod_table) <- 2
quick_xlsx(primary_mod_table, file = "tables/primary_mod_table.xlsx")


# marginal means plots ----------------------------------------------------

fig_1 <- 
    df %>% 
    filter(exclude == 0) %>% 
    mutate(cov_party3_f = case_when(cov_party3_f == "dem" ~  "Dem.", 
                                    cov_party3_f ==  "ind" ~ "Ind.", 
                                    cov_party3_f == "gop" ~ "Rep."),
           cov_party3_f = factor(cov_party3_f, levels = c("Dem.", "Ind.", "Rep.")),
           iv_cand_pref_f = case_when(iv_cand_pref_f == "winner" ~ "Preferred Winner",
                                      iv_cand_pref_f == "loser" ~ "Preferred Loser"),
           iv_cand_pref_f = factor(iv_cand_pref_f, levels = c("Preferred Loser",
                                                              "Preferred Winner")),
           iv_cdv_f = case_when(iv_cdv_f == "likes one" ~  "Easy", 
                                iv_cdv_f ==  "likes both" ~ "Hard\n(Positive)", 
                                iv_cdv_f == "dislikes both" ~ "Hard\n(Negative)"),
           iv_cdv_f = factor(iv_cdv_f, levels = c("Hard\n(Negative)", 
                                                  "Easy",
                                                  "Hard\n(Positive)"))) %>% 
    group_by(election_year, iv_cand_pref_f, iv_cdv_f, cov_party3_f) %>% 
    summarise(mean_cl_normal(dv_cand_polz)) %>% 
    ungroup %>% 
    ggplot(aes(x = iv_cdv_f, y = y, shape = cov_party3_f)) + 
    jtools::theme_apa()+
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  width = 0.4, 
                  position = position_dodge(width = .5),
                  color = "grey",
                  size = .5) + 
    geom_point(aes(shape = cov_party3_f, size = cov_party3_f),
               position = position_dodge(width = .5), color = "black") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
    scale_shape_manual("Party:", values = c(20, 8, 4)) + 
    scale_size_manual("Party:", values = c(.5, 1, .75)) + 
    facet_grid(election_year ~ iv_cand_pref_f) + 
    theme(text = element_text(size = 12, family = "Times New Roman"),
          plot.title = element_text(size = 12,
                                    family = "Times New Roman",
                                    face = "bold"),
          plot.subtitle = element_text(size = 12,
                                       family = "Times New Roman",
                                       face = "italic"),
          legend.position = c(.75, .07),
          legend.direction = "horizontal",
          legend.background = element_rect(color = "black",
                                           size = .5),
          legend.spacing.x = unit(0.01,"cm"),
          legend.margin = margin(r = .1, l = .1, 
                                 t = .05, b = .05, unit = "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          legend.key.width = unit(.5, "cm"),
          
          axis.title.x = element_text(margin = 
                                          margin(r = 0, l = 0, 
                                                 t = .5, b = .0, 
                                                 unit = "cm")),
          plot.caption = ggtext::element_markdown(hjust = 0)
    ) +
    guides(size = guide_legend(override.aes = list(size = 1.5))) + 
    labs(title = "Figure 1", 
         subtitle = "Quasi-Marginal Means of Model 1",
         x = "Choice Difficulty (Valence)", y = "Post-Election Polarization",
         caption = "_Note._ The means and 95% CIs shown above are considered “quasi” 
         marginal means of Model 1 <br>because they do not account for e.g., 
         the covariates in the model (i.e., they are simply the <br>_M_/95%CIs 
         broken down by candidate preference, choice difficulty/valence, 
         party, and year.") 
                        
ggsave("plots/figure_1.PNG", 
       plot = fig_1,
       width = 6, height = 7, unit = "in")    

# same but without party
fig_1_nopty <- 
    df %>% 
    filter(exclude == 0) %>% 
    mutate(iv_cand_pref_f = case_when(iv_cand_pref_f == "winner" ~ "Preferred Winner",
                                      iv_cand_pref_f == "loser" ~ "Preferred Loser"),
           iv_cand_pref_f = factor(iv_cand_pref_f, levels = c("Preferred Loser",
                                                              "Preferred Winner")),
           iv_cdv_f = case_when(iv_cdv_f == "likes one" ~  "Easy", 
                                iv_cdv_f ==  "likes both" ~ "Hard\n(Positive)", 
                                iv_cdv_f == "dislikes both" ~ "Hard\n(Negative)"),
           iv_cdv_f = factor(iv_cdv_f, levels = c("Hard\n(Negative)", 
                                                  "Easy",
                                                  "Hard\n(Positive)"))) %>% 
    group_by(election_year, iv_cand_pref_f, iv_cdv_f) %>% 
    summarise(mean_cl_normal(dv_cand_polz)) %>% 
    ungroup %>% 
    ggplot(aes(x = iv_cdv_f, y = y, color = iv_cand_pref_f)) + 
    jtools::theme_apa()+
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  width = 0.4, 
                  position = position_dodge(width = .5),
                  size = .5) + 
    geom_point(
         position = position_dodge(width = .5)
    ) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
    scale_color_manual("Preference:", values = c("dark grey", "black")) + 
    facet_grid(~election_year ) + 
    theme(text = element_text(size = 12, family = "Times New Roman"),
          plot.title = element_text(size = 12,
                                    family = "Times New Roman",
                                    face = "bold"),
          plot.subtitle = element_text(size = 12,
                                       family = "Times New Roman",
                                       face = "italic"),
          legend.position = c(.88, .12),
          legend.background = element_rect(color = "black",
                                           size = .5),
          legend.spacing.x = unit(0.01,"cm"),
          legend.margin = margin(r = .1, l = .1, 
                                 t = .05, b = .05, unit = "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.key.width = unit(.5, "cm"),
          
          axis.title.x = element_text(margin = 
                                          margin(r = 0, l = 0, 
                                                 t = .5, b = .0, 
                                                 unit = "cm")),
          plot.caption = ggtext::element_markdown(hjust = 0)
    ) +
    labs(title = "Figure 1", 
         subtitle = "Quasi-Marginal Means of Model 1",
         x = "Choice Difficulty (Valence)", y = "Post-Election Polarization",
         caption = "_Note._ The means and 95% CIs shown above are considered “quasi” 
         marginal means of Model 1 <br>because they do not account for e.g., 
         the covariates in the model (i.e., they are simply the <br>_M_/95%CIs 
         broken down by candidate preference, choice difficulty/valence, 
         and year.") 
    

fig_1_nopty
ggsave("plots/figure_1_nopty.PNG", 
       plot = fig_1_nopty,
       width = 6, height = 6, unit = "in")   

# model comparison --------------------------------------------------------

df %>% 
    filter(exclude == 0) %>% 
    mutate(cov_edu_recode_n = cov_edu_recode_n - 3.5) %>% 
    group_by(election_year) %>% 
    nest() %>% 
    mutate(
        mod_1 = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0, !is.na(iv_triv)) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_3 = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0, !is.na(iv_triv)) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_triv + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        mod_compare = map2(.x = mod_1, .y = mod_3,
                           ~anova(.y, .x))) %>% 
    unnest(mod_compare) 


df %>% 
    filter(exclude == 0) %>% 
    mutate(cov_edu_recode_n = cov_edu_recode_n - 3.5) %>% 
    group_by(election_year) %>% 
    nest() %>% 
    mutate(
        mod_1 = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0, !is.na(iv_triv)) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        tidy_mod_1 = map(mod_1, ~tidy(.x, conf.int = T))) %>% 
    unnest(tidy_mod_1) %>% 
    select(election_year, term, estimate, std.error, conf.low, conf.high) %>% 
    mutate(label = "filtered triv") %>% 
    
    bind_rows(

all_mods %>% 
    filter(model_name == "mod_1") %>% 
    unnest(tidy_mods) %>% 
    select(election_year, term, estimate, std.error, conf.low, conf.high) %>% 
    mutate(label = "no filter")
        
) 
# collapse across years ---------------------------------------------------

### models 
year_collapsed_mods <- 
df %>% 
    filter(exclude == 0) %>% 
    mutate(cov_edu_recode_n = cov_edu_recode_n - 3.5) %>% 
    nest(data = everything()) %>% 
    mutate(
        ###################### primary models
        mod_1 = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_2 = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_prox_sup + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .)),
        
        mod_3 = 
            map(data,
                ~.x %>% 
                    filter(cov_party3_n != 0) %>% 
                    lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + iv_triv + 
                           cov_party_n + cov_expect_n + cov_voted_n + 
                           cent_cov_pre_polz_m + cent_cov_pk_m + 
                           cov_edu_recode_n + cov_days,
                       data = .))) %>% 
    ungroup %>% 
    pivot_longer(-data, 
                 names_to = "model_name", 
                 values_to = "model") %>% 
    mutate(tidy_mods = map(model, ~tidy(.x, conf.int = T)),
           glanced_mods = map(model, ~glance(.x)))

#### table
collapsed_mod_table <- 
    year_collapsed_mods %>% 
    select(model_name, model) %>% 
    deframe() %>% 
    
    huxreg(ci_level = .95, error_format = "[{conf.low}, {conf.high}]",
           
           statistics = c("N" = "nobs",
                          "R sq." = "r.squared",
                          "Adj. R sq." = "adj.r.squared",
                          "df",
                          "df.residual"),
           coefs = c(
               "Intercept" = "(Intercept)",
               "Preference" = "iv_cand_pref_floser",
               "C1: Difficulty" = "iv_cdv_fc1_difficulty",
               "C2: Valence" = "iv_cdv_fc2_valence",
               "Party" = "cov_party_n",
               "Expectation" = "cov_expect_n",
               "Voted" = "cov_voted_n",
               "Pre-pol" = "cent_cov_pre_polz_m",
               "Pol. Knowledge" = "cent_cov_pk_m",
               "Education" = "cov_edu_recode_n",
               "Days" = "cov_days",
               "Trivialization" = "iv_triv",
               "Proximal Support" = "iv_prox_sup"
           ))  

number_format(collapsed_mod_table) <- 3
quick_xlsx(collapsed_mod_table, file = "tables/collapsed_mod_table_round3.xlsx")
number_format(collapsed_mod_table) <- 2
quick_xlsx(collapsed_mod_table, file = "tables/collapsed_mod_table.xlsx")

### plot 
fig_2 <- 
    df %>% 
    filter(exclude == 0) %>% 
    mutate(cov_party3_f = case_when(cov_party3_f == "dem" ~  "Dem.", 
                                    cov_party3_f ==  "ind" ~ "Ind.", 
                                    cov_party3_f == "gop" ~ "Rep."),
           cov_party3_f = factor(cov_party3_f, levels = c("Dem.", "Ind.", "Rep.")),
           iv_cand_pref_f = case_when(iv_cand_pref_f == "winner" ~ "Preferred Winner",
                                      iv_cand_pref_f == "loser" ~ "Preferred Loser"),
           iv_cand_pref_f = factor(iv_cand_pref_f, levels = c("Preferred Loser",
                                                              "Preferred Winner")),
           iv_cdv_f = case_when(iv_cdv_f == "likes one" ~  "Easy", 
                                iv_cdv_f ==  "likes both" ~ "Hard\n(Positive)", 
                                iv_cdv_f == "dislikes both" ~ "Hard\n(Negative)"),
           iv_cdv_f = factor(iv_cdv_f, levels = c("Hard\n(Negative)", 
                                                  "Easy",
                                                  "Hard\n(Positive)"))) %>% 
    group_by(iv_cand_pref_f, iv_cdv_f, cov_party3_f) %>% 
    summarise(mean_cl_normal(dv_cand_polz)) %>% 
    ungroup %>% 
    ggplot(aes(x = iv_cdv_f, y = y, shape = cov_party3_f)) + 
    jtools::theme_apa()+
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  width = 0.4, 
                  position = position_dodge(width = .5),
                  color = "grey",
                  size = .5) + 
    geom_point(aes(shape = cov_party3_f, size = cov_party3_f),
               position = position_dodge(width = .5), 
               color = "black", size = 2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
    scale_shape_manual("Party:", values = c(20, 8, 4)) + 
    scale_size_manual("Party:", values = c(.5, 1, .75)) + 
    facet_grid(.~ iv_cand_pref_f) + 
    theme(text = element_text(size = 12, family = "Times New Roman"),
          plot.title = element_text(size = 12,
                                    family = "Times New Roman",
                                    face = "bold"),
          plot.subtitle = element_text(size = 12,
                                       family = "Times New Roman",
                                       face = "italic"),
          legend.position = c(.75, .1),
          legend.direction = "horizontal",
          legend.background = element_rect(color = "black",
                                           size = .5),
          legend.spacing.x = unit(0.01,"cm"),
          legend.margin = margin(r = .1, l = .1, 
                                 t = .05, b = .05, unit = "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          legend.key.width = unit(.5, "cm"),
          
          axis.title.x = element_text(margin = 
                                          margin(r = 0, l = 0, 
                                                 t = .5, b = .0, 
                                                 unit = "cm")),
          plot.caption = ggtext::element_markdown(hjust = 0)
    ) +
    guides(size = guide_legend(override.aes = list(size = 1.5))) + 
    labs(title = "Figure 2", 
         subtitle = "Marginal Means Collapsed Across Years",
         x = "Choice Difficulty (Valence)", y = "Post-Election Polarization",
         caption = "_Note._ Shown above are the means and 95% CIs of polarization 
         across 2016 and 2020 (comcined) <br>broken down by candidate preference, 
         choice difficulty/valence, party, and year.") 
fig_2

ggsave("plots/figure_2.PNG", 
       plot = fig_2,
       width = 6, height = 5.5, unit = "in")    


# ancillary models -------------------------------------------------------

# This table was reformatted for visual ease and listed in supplementary
# materials as "All models table"
anc_mod_table <- 
    all_mods %>% 
    mutate(model_name = str_to_sentence(model_name),
           name = paste(election_year, model_name, sep = ":")) %>% 
    arrange(model_type, model_name, election_year) %>% 
    select(name, model) %>% 
    deframe() %>% 
    
    huxreg(ci_level = .95, error_format = "[{conf.low}, {conf.high}]",
           
           statistics = c("N" = "nobs",
                          "R sq." = "r.squared",
                          "Adj. R sq." = "adj.r.squared",
                          "df",
                          "df.residual"),
           coefs = c(
               "Intercept" = "(Intercept)",
               "Preference" = "iv_cand_pref_floser",
               "C1: Difficulty" = "iv_cdv_fc1_difficulty",
               "C2: Valence" = "iv_cdv_fc2_valence",
               "Party" = "cov_party_n",
               "Expectation" = "cov_expect_n",
               "Voted" = "cov_voted_n",
               "Pre-pol" = "cent_cov_pre_polz_m",
               "Pol. Knowledge" = "cent_cov_pk_m",
               "Education" = "cov_edu_recode_n",
               "Days" = "cov_days",
               "Trivialization" = "iv_triv",
               "Proximal Support" = "iv_prox_sup",
               "Support (alt)" = "iv_prox_sup_alt",
               "Party (all 3)" = "cov_party3_n",
                 "Pref. x C1" = "iv_cand_pref_floser:iv_cdv_fc1_difficulty",
                "Pref. x C2" = "iv_cand_pref_floser:iv_cdv_fc2_valence"
           ))  

number_format(anc_mod_table) <- 3
quick_xlsx(anc_mod_table, file = "tables/anc_mod_table_round3.xlsx")
number_format(anc_mod_table) <- 2
quick_xlsx(anc_mod_table, file = "tables/primary_mod_table.xlsx")

# plot models -------------------------------------------------------------


mod_plot_data <- 
all_mods %>% 
    select(election_year, model_name, model_type, tidy_mods, robust_tbl) %>% 
    unnest(c(tidy_mods, robust_tbl)) %>% 
    mutate(term = 
               case_when(
                   term == "(Intercept)" ~ "(int)",
                   term == "iv_cand_pref_floser" ~ "iv_pref",
                   term == "iv_cdv_fc1_difficulty" ~ "iv_diff",
                   term == "iv_cdv_fc2_valence" ~ "iv_ valence",
                   term == "cov_party_n" ~ "cov_party",
                   term == "cov_expect_n" ~ "cov_expect",
                   term == "cov_voted_n" ~ "cov_vote",
                   term == "cent_cov_pre_polz_m" ~ "cov_prepol",
                   term == "cent_cov_pk_m" ~ "cov_pk",
                   term == "cov_edu_recode_n" ~ "cov_edu",
                   term == "cov_days" ~ "cov_days",
                   term == "iv_triv" ~ "iv_triv",
                   term == "iv_prox_sup" ~ "iv_sup",
                   term == "iv_prox_sup_alt" ~ "iv_sup_alt",
                   term == "cov_party3_n" ~ "cov_pty3",
                   term == "iv_cand_pref_floser:iv_cdv_fc1_difficulty" ~ "int_pref:diff",
                   term == "iv_cand_pref_floser:iv_cdv_fc2_valence" ~ "int_pref:valence"
               ),
           term_robust = 
               case_when(
                   term_robust == "(Intercept)" ~ "(int)",
                   term_robust == "iv_cand_pref_floser" ~ "iv_pref",
                   term_robust == "iv_cdv_fc1_difficulty" ~ "iv_diff",
                   term_robust == "iv_cdv_fc2_valence" ~ "iv_ valence",
                   term_robust == "cov_party_n" ~ "cov_party",
                   term_robust == "cov_expect_n" ~ "cov_expect",
                   term_robust == "cov_voted_n" ~ "cov_vote",
                   term_robust == "cent_cov_pre_polz_m" ~ "cov_prepol",
                   term_robust == "cent_cov_pk_m" ~ "cov_pk",
                   term_robust == "cov_edu_recode_n" ~ "cov_edu",
                   term_robust == "cov_days" ~ "cov_days",
                   term_robust == "iv_triv" ~ "iv_triv",
                   term_robust == "iv_prox_sup" ~ "iv_sup",
                   term_robust == "iv_prox_sup_alt" ~ "iv_sup_alt",
                   term_robust == "cov_party3_n" ~ "cov_pty3",
                   term_robust == "iv_cand_pref_floser:iv_cdv_fc1_difficulty" ~ "int_pref:diff",
                   term_robust == "iv_cand_pref_floser:iv_cdv_fc2_valence" ~ "int_pref:valence"
               )
    ) 


# The plot below was exported to html as "all_model_params_visual.html"
mod_plot_data %>% 
    ggplot(aes(x = term, color = election_year)) + theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high),
                    position = position_dodge(width = .5),
                    size = .25) +
    geom_hline(yintercept = 0) + 
    scale_color_manual(values = c("red", "blue"))+
    guides(color = "none") + 
    coord_flip()+
    trelliscopejs::facet_trelliscope(~model_name, scales = "free_x",
                                     nrow = 2, ncol = 3,
                                     self_contained = T) + 
    labs(title = "Red = 2016; Blue = 2020")



# residuals
all_mods %>% 
    filter(model_type == "primary") %>% 
    select(election_year, model_name, augmented_mods) %>% 
    unnest(augmented_mods) %>% 
    ggplot(aes(x = .fitted, y = .resid, color = .cooksd)) + 
    theme_minimal() +
    geom_point(alpha = .5) + 
    geom_smooth(method = "loess") + 
    facet_wrap(election_year ~ model_name) + 
    labs(x = "fitted values", y = "residuals", 
         title = "Residuals vs. Fitted")


all_mods %>% 
    filter(model_type == "primary") %>% 
    select(election_year, model_name, augmented_mods) %>% 
    unnest(augmented_mods) %>% 
    mutate(sqrt_std_res = sqrt(.std.resid)) %>% 
    ggplot(aes(x = .fitted, y = sqrt_std_res, color = .cooksd)) + 
    theme_minimal() +
    geom_point(alpha = .5) + 
    geom_smooth(method = "loess") + 
    facet_wrap(election_year ~ model_name) + 
    labs(x = "fitted values", y = "sqrt of std. residuals", 
         title = "Scale location")
    


# reported vs. robust se plots --------------------------------------------

mod_plot_data %>% 
    select(election_year:conf.high) %>% 
    mutate(se_type = "regular") %>% 
    bind_rows(
        mod_plot_data %>% 
            select(-c(term:conf.high)) %>% 
            mutate(se_type = "robust") %>% 
            rename_all(.funs = ~str_remove(.x, "_robust"))
    ) %>% 
    ggplot(aes(x = term, color = se_type, shape = election_year)) + theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high),
                    position = position_dodge(width = .5),
                    size = .25) +
    geom_hline(yintercept = 0) + 
    scale_color_manual(values = c("red", "blue"))+
    coord_flip()+
    trelliscopejs::facet_trelliscope(~model_name, scales = "free_x",
                                     nrow = 2, ncol = 3,
                                     self_contained = T) 

# non-response (no post survey) -------------------------------------------

df %>% 
    mutate(missing_post = ifelse(is.na(survey_date_post), "Missing", "Responded")) %>% 
    select(election_year,
           missing_post,
           cov_party3_f,
           demog_age,
           demog_sex,
           demog_race,
           cov_edu_recode_f,
           demog_marital
    ) %>% 
    mutate(
        demog_marital = str_to_sentence(demog_marital),
        demog_marital = factor(demog_marital,
                               levels = c(
                                   "Married",
                                   "Never married",
                                   "Formerly married"
                               )),
        demog_race = factor(demog_race,
                            levels = c(
                                "White",
                                "Black",
                                "Asian",
                                "Hispanic",
                                "Native American/Alaskan",
                                "Other"
                            )),
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
        cov_party3_f = factor(case_when(cov_party3_f == "ind" ~ "Ind.",
                                        cov_party3_f == "gop" ~ "Rep.",
                                        cov_party3_f == "dem" ~ "Dem."),
                              levels = c("Dem.", "Rep.", "Ind."))
    ) %>% 
    
    tbl_strata(
        strata = election_year,
        .header = "**{strata}**",
        ~.x %>%
            tbl_summary(statistic = list(
                all_continuous() ~ "{mean}  \n ({sd})",
                all_categorical() ~ "{n}  \n ({p}%)"),
                
                label = 
                    c(demog_age ~ "Age",
                      demog_sex ~ "Sex",
                      demog_marital ~ "Marital status",
                      demog_race ~ "Race/ethnicity",
                      cov_edu_recode_f ~ "Education",
                      cov_party3_f ~ "Party"
                    ),
                
                percent = "row",
                
                by = missing_post
                
            ) %>%     
            modify_header(all_stat_cols() ~ "**{level}**<br>n = {n}") %>%
            bold_labels() %>% 
            italicize_levels() %>% 
            modify_footnote(all_stat_cols() ~ NA) %>% 
            modify_caption("**Table 1b. Non-response Demographics x Year x Party**") %>% 
            remove_row_type(
                variables = demog_sex,
                type =  "level",
                level_value = "Other"
            )  ) %>% 
    as_gt() %>%
    fmt_markdown(columns = all_stat_cols()) %>%
    gtsave(file = "tables/missing_demo_tbl.html")


df %>% 
    mutate(missing_post = ifelse(is.na(survey_date_post), "Missing", "Responded")) %>% 
    select(election_year,
           missing_post,
           iv_cand_pref_f,
           cdv_groups,
           iv_prox_sup,
           cov_pre_polz,
           cov_pk,
           cov_expect_f,
    ) %>% 
    mutate(
        iv_cand_pref_f = case_when(iv_cand_pref_f == "loser" ~ "yes", TRUE ~ "no"),
        cdv_groups = str_to_sentence(cdv_groups),
        cdv_groups = factor(cdv_groups,
                            levels = c("Likes both", "Likes one", "Dislikes both")),
        cov_expect_f = ifelse(cov_expect_f == "loser", "yes", "no"),
    ) %>% 
    tbl_strata(
        strata = election_year,
        .header = "**{strata}**",
        ~.x %>%
            tbl_summary(
                type = c(cov_pk, iv_prox_sup) ~ "continuous",
                percent = "row",
                statistic = list(
                    all_continuous() ~ "{mean} ({sd})",
                    all_categorical() ~ "{n}  \n ({p}%)"),
                
                label = 
                    c(
                        cov_pk ~ "Political knowledge",
                        cov_expect_f ~ "Expected loser",
                        cdv_groups ~ "Difficulty/valence group",
                        cov_pre_polz ~ "Pre-election polarization",
                        iv_prox_sup ~ "Proximal in-group support",
                        iv_cand_pref_f ~ "Preferred loser"
                    ),
                
                #missing = "no",
                
                by = missing_post,
                
            ) %>%     
            modify_header(all_stat_cols() ~ "**{level}**<br>n = {n}") %>%
            
            bold_labels() %>% 
            italicize_levels() %>% 
            modify_footnote(all_stat_cols() ~ NA) %>% 
            modify_caption("**Table 2b. Non-response Survery Related  
                   Characteristics x Year x Party**"))  %>% 
    as_gt() %>%
    fmt_markdown(columns = all_stat_cols()) %>%
    gtsave(file = "tables/missing_studytbl.html")

# missing data  -----------------------------------------------------------

df %>% 
    mutate(pref_switcher = ifelse(is.na(pref_switcher), 0, NA)) %>% 
    # exclude == 1 are the people who didn't do post survey
    filter(exclude != 1) %>% 
    select(dv_cand_polz, iv_cand_pref_f, iv_cdv_f, iv_prox_sup, iv_triv,
           cov_pre_polz, cov_party3_f, cov_pk, cov_edu_recode_f, cov_expect_f,
           cov_voted_f, contains("demog")) %>% 
    gg_miss_upset(nsets = 15) 



df %>% 
    filter(exclude != 1) %>% 
    select(dv_cand_polz, iv_cand_pref_f, iv_cdv_f, iv_prox_sup, iv_triv,
           cov_pre_polz, cov_party3_f, cov_pk, cov_edu_recode_f, cov_expect_f,
           cov_voted_f, contains("demog")) %>% 
    miss_var_summary() %>% 
    arrange(desc(pct_miss)) %>% # max missing = 4.8%
    print(n = 15)


df %>%  # 93% are missing 0 or 1 
    filter(exclude != 1) %>% 
    select(dv_cand_polz, iv_cand_pref_f, iv_cdv_f, iv_prox_sup, iv_triv,
           cov_pre_polz, cov_party3_f, cov_pk, cov_edu_recode_f, cov_expect_f,
           cov_voted_f, contains("demog")) %>% 
    miss_case_table() 




# change score/direction --------------------------------------------------

fig_3 <- 
    df %>% 
    drop_na(iv_cdv_f) %>% 
    mutate(cand_change_winner = case_when(election_year == "2016" ~ cand_change_gop,
                                          election_year == "2020" ~ cand_change_dem),
           cand_change_loser = case_when(election_year == "2016" ~ cand_change_dem,
                                         election_year == "2020" ~ cand_change_gop)) %>% 
    group_by(election_year, iv_cand_pref_f,  iv_cdv_f) %>% 
    summarise(`Winner\nChange` = mean_cl_normal(cand_change_winner),
              `Loser\nChange` = mean_cl_normal(cand_change_loser)) %>% 
    pivot_longer(`Winner\nChange`:`Loser\nChange`, names_to = "change_tgt") %>% 
    mutate(iv_cand_pref_f = case_when(iv_cand_pref_f == "winner" ~ "Preferred Winner",
                                      iv_cand_pref_f == "loser" ~ "Preferred Loser"),
           iv_cand_pref_f = factor(iv_cand_pref_f, 
                                   levels = c(
                                       "Preferred Loser",
                                       "Preferred Winner"
                                   )),
           iv_cdv_f = case_when(iv_cdv_f == "dislikes both" ~ "Dislikes\nBoth", 
                                iv_cdv_f == "likes both" ~ "Likes\nBoth", 
                                iv_cdv_f == "likes one" ~ "Likes\nOne"),
           
           iv_cdv_f = factor(iv_cdv_f, levels = c("Dislikes\nBoth", 
                                                  "Likes\nOne",
                                                  "Likes\nBoth"))) %>% 
    ggplot(aes(x = iv_cdv_f, y = value$y, 
               color = change_tgt)) + 
    geom_hline(yintercept = 0, color = "black", linetype = "dotted")+
    geom_errorbar(aes(ymin = value$ymin, ymax = value$ymax),
                  position = position_dodge(width = .5),
                  width = .5) + 
    scale_color_manual("", values = c("black", "dark grey"))+
    facet_wrap(election_year ~ iv_cand_pref_f, 
               labeller = label_wrap_gen(multi_line = FALSE)) +
    jtools::theme_apa()+
    theme(text = element_text(size = 12, family = "Times New Roman"),
          plot.title = element_text(size = 12,
                                    family = "Times New Roman",
                                    face = "bold"),
          plot.subtitle = element_text(size = 12,
                                       family = "Times New Roman",
                                       face = "italic"),
          legend.position = c(.7, .07),
          legend.direction = "horizontal",
          legend.background = element_rect(color = "black",
                                           size = .5),
          legend.spacing.x = unit(0.01,"cm"),
          legend.margin = margin(r = .1, l = .1, 
                                 t = .05, b = .05, unit = "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          legend.key.width = unit(.5, "cm"),
          
          axis.title.x = element_text(margin = 
                                          margin(r = 0, l = 0, 
                                                 t = .5, b = .0, 
                                                 unit = "cm")),
          plot.caption = ggtext::element_markdown(hjust = 0)
    ) +
    labs(x = NULL, y = "Candidate Change Scores",
         title = "Figure 3", subtitle = "Marginal Means of Candidate Change Scores",
         caption = "_Note._ The  95% CIs of the means are shown above. “Winner change”
         was the change in attitudes<br>towards Trump and Biden in 2016 and 2020, 
         respectively; “loser change” was the change in attitudes<br>towards 
         Clinton and Trump in 2016 and 2020, respectively. Postive scores indicate
         a respondent rated<br>the candidate more positively after the election and vice versa.")

ggsave("plots/figure_3.PNG", 
       plot = fig_3,
       width = 6, height = 6, unit = "in")  


# trivialization plot -----------------------------------------------------

fig_4 <- 
    df %>% 
    drop_na(iv_cdv_f) %>% 
    group_by(election_year, iv_cdv_f) %>% 
    summarize(triv = mean_cl_normal(iv_triv)) %>% 
    mutate(iv_cand_pref_f = "Collapsed Across\nCandidate Preference") %>% 
    bind_rows(
        df %>% 
            drop_na(iv_cdv_f) %>% 
            group_by(election_year, iv_cand_pref_f, iv_cdv_f) %>% 
            summarize(triv = mean_cl_normal(iv_triv))
    ) %>% 
    mutate(iv_cand_pref_f = case_when(iv_cand_pref_f == "winner" ~ "Preferred\nWinner",
                                      iv_cand_pref_f == "loser" ~ "Preferred\nLoser",
                                      TRUE ~ iv_cand_pref_f),
           iv_cand_pref_f = factor(iv_cand_pref_f, 
                                   levels = c(
                                       "Preferred\nLoser",
                                       "Preferred\nWinner",
                                       "Collapsed Across\nCandidate Preference"
                                   )),
           iv_cdv_f = case_when(iv_cdv_f == "dislikes both" ~ "Dislikes\nBoth", 
                                iv_cdv_f == "likes both" ~ "Likes\nBoth", 
                                iv_cdv_f == "likes one" ~ "Likes\nOne"),
           iv_cdv_f = factor(iv_cdv_f, levels = c("Dislikes\nBoth", 
                                                  "Likes\nOne",
                                                  "Likes\nBoth"))
    ) %>% drop_na() %>% 
    ggplot(aes(x = iv_cdv_f, y = triv$y, linetype = election_year)) + 
    geom_errorbar(aes(ymin = triv$ymin, ymax = triv$ymax),
                  color = "black",
                  position = position_dodge(width = .5),
                  width = .5) + 
    scale_linetype_manual("Year", values = c("solid", "dotted"))+
    facet_wrap(iv_cand_pref_f~.) + 
    jtools::theme_apa()+
    theme(text = element_text(size = 12, family = "Times New Roman"),
          plot.title = element_text(size = 12,
                                    family = "Times New Roman",
                                    face = "bold"),
          plot.subtitle = element_text(size = 12,
                                       family = "Times New Roman",
                                       face = "italic"),
          legend.position = c(.87, .1),
          legend.direction = "horizontal",
          legend.background = element_rect(color = "black",
                                           size = .5),
          legend.spacing.x = unit(0.01,"cm"),
          legend.margin = margin(r = .1, l = .1, 
                                 t = .05, b = .05, unit = "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          legend.key.width = unit(.5, "cm"),
          
          axis.title.x = element_text(margin = 
                                          margin(r = 0, l = 0, 
                                                 t = .5, b = .0, 
                                                 unit = "cm")),
          plot.caption = ggtext::element_markdown(hjust = 0)
    ) +
    scale_y_continuous(breaks = c(2, 2.5, 3, 3.5), limits = c(2, 3.5))+
    labs(x = NULL, y = "Trivialization",
         title = "Figure 4", subtitle = "Marginal Means of Trivialization by Year",
         caption = "_Note._ The  95% CIs of the means are shown above. 
         Trivialization is reverse scored such that higher<br>scores indicate less 
         trivialization.")


ggsave("plots/figure_4.PNG", 
       plot = fig_4,
       width = 6, height = 5, unit = "in")   


# fig 2 but vote vs non vote  ---------------------------------------------


fig_5 <- 
    df %>% 
    filter(exclude == 0) %>% 
    mutate(cov_party3_f = case_when(cov_party3_f == "dem" ~  "Dem.", 
                                    cov_party3_f ==  "ind" ~ "Ind.", 
                                    cov_party3_f == "gop" ~ "Rep."),
           cov_party3_f = factor(cov_party3_f, levels = c("Dem.", "Ind.", "Rep.")),
           iv_cand_pref_f = case_when(iv_cand_pref_f == "winner" ~ "Preferred Winner",
                                      iv_cand_pref_f == "loser" ~ "Preferred Loser"),
           iv_cdv_f = case_when(iv_cdv_f == "likes one" ~  "Easy", 
                                iv_cdv_f ==  "likes both" ~ "Hard\n(Positive)", 
                                iv_cdv_f == "dislikes both" ~ "Hard\n(Negative)"),
           iv_cdv_f = factor(iv_cdv_f, levels = c("Hard\n(Negative)", 
                                                  "Easy",
                                                  "Hard\n(Positive)"))) %>% 
    group_by(iv_cand_pref_f, iv_cdv_f, cov_voted_f) %>% 
    summarise(mean_cl_normal(dv_cand_polz)) %>% 
    ungroup %>% 
    ggplot(aes(x = iv_cdv_f, y = y)) + 
    jtools::theme_apa()+
    geom_errorbar(aes(ymin = ymin, ymax = ymax, color = cov_voted_f),
                  width = 0.4, 
                  position = position_dodge(width = .5)) + 
     geom_point(color = "black",
                position = position_dodge2(width = .5)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
    scale_color_manual("Voted:", values = c("dark grey", "black")) +
    facet_grid(.~ iv_cand_pref_f) + 
    theme(text = element_text(size = 12, family = "Times New Roman"),
          plot.title = element_text(size = 12,
                                    family = "Times New Roman",
                                    face = "bold"),
          plot.subtitle = element_text(size = 12,
                                       family = "Times New Roman",
                                       face = "italic"),
          legend.position = c(.75, .1),
          legend.direction = "horizontal",
          legend.background = element_rect(color = "black",
                                           size = .5),
          legend.spacing.x = unit(0.01,"cm"),
          legend.margin = margin(r = .1, l = .1, 
                                 t = .05, b = .05, unit = "cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          legend.key.width = unit(.5, "cm"),
          
          axis.title.x = element_text(margin = 
                                          margin(r = 0, l = 0, 
                                                 t = .5, b = .0, 
                                                 unit = "cm")),
          plot.caption = ggtext::element_markdown(hjust = 0)
    ) +
    guides(size = guide_legend(override.aes = list(size = 1.5))) + 
    labs(title = "Figure 5", 
         subtitle = "Marginal Means of Polarization by Voting Behavior (Collapsed Across Years)",
         x = "Choice Difficulty (Valence)", y = "Post-Election Polarization",
         caption = "_Note._ Shown above are the means and 95% CIs of polarization 
         across 2016 and 2020 (comcined) <br>broken down by candidate preference, 
         choice difficulty/valence, party, and year.") 
fig_5

ggsave("plots/figure_5.PNG", 
       plot = fig_5,
       width = 6, height = 5.5, unit = "in")   


# cors --------------------------------------------------------------------
df %>%
    mutate(pref_switch = ifelse(!is.na(pref_switcher), 0, 1)) %>%
    select(dv_cand_polz, iv_cand_pref_n, iv_cdv_diff_c1, iv_cdv_val_c2,
           iv_prox_sup, iv_prox_sup_alt, iv_triv, cov_pre_polz, cov_party3_n,
           party_strength, cov_pk, cov_edu_recode_n, cov_expect_n, cov_voted_n,
           cov_days, cand_change_dem, cand_change_gop, pref_switch, voted_early,
           demog_age) %>%
    correlate() %>% shave() %>% write_csv("tables/total_cor_tbl.csv")

df %>%
    filter(election_year == "2016") %>%
    mutate(pref_switch = ifelse(!is.na(pref_switcher), 0, 1)) %>%
    select(dv_cand_polz, iv_cand_pref_n, iv_cdv_diff_c1, iv_cdv_val_c2,
           iv_prox_sup, iv_prox_sup_alt, iv_triv, cov_pre_polz, cov_party3_n,
           party_strength, cov_pk, cov_edu_recode_n, cov_expect_n, cov_voted_n, 
           cov_days, cand_change_dem, cand_change_gop, pref_switch, voted_early, 
           demog_age) %>% 
    correlate() %>% shave() %>% write_csv("tables/2016_cor_tbl.csv")

df %>% 
    filter(election_year == "2020") %>% 
    mutate(pref_switch = ifelse(!is.na(pref_switcher), 0, 1)) %>% 
    select(dv_cand_polz, iv_cand_pref_n, iv_cdv_diff_c1, iv_cdv_val_c2,
           iv_prox_sup, iv_prox_sup_alt, iv_triv, cov_pre_polz, cov_party3_n,
           party_strength, cov_pk, cov_edu_recode_n, cov_expect_n, cov_voted_n, 
           cov_days, cand_change_dem, cand_change_gop, pref_switch, voted_early, 
           demog_age) %>% 
    correlate() %>% shave() %>% write_csv("tables/2020_cor_tbl.csv")


df %>% 
    select(iv_cand_pref_n, iv_cdv_diff_c1, iv_cdv_val_c2,
           iv_prox_sup, iv_prox_sup_alt, iv_triv, cov_pre_polz,
           party_strength, cov_pk, cov_edu_recode_n, cov_expect_n, demog_age) %>% 
    correlate() %>%
    shave() %>% 
    write_csv("tables/manu_cor_tbl.csv")


df %>% 
    select(iv_cand_pref_n, iv_cdv_diff_c1, iv_cdv_val_c2,
           iv_prox_sup, iv_prox_sup_alt, iv_triv, cov_pre_polz,
           party_strength, cov_pk, cov_edu_recode_n, cov_expect_n,demog_age) %>% 
    psych::pairwiseCount() %>% as_tibble %>% 
    pivot_longer(everything()) %>% 
    summarise(min = min(value),
              max = max(value))



# standardized triv and pol -----------------------------------------------

std_plot <- 
df %>% 
    drop_na(iv_cdv_f) %>% 
    filter(exclude == 0) %>% 
    mutate(dv_cand_polz = mosaic::zscore(dv_cand_polz)) %>% 
    group_by(election_year, iv_cand_pref_f, iv_cdv_f) %>% 
    summarise(mean_cl_normal(dv_cand_polz)) %>% 
    mutate(outcome = "attitude") %>% 
    bind_rows(
        df %>% 
            drop_na(iv_cdv_f) %>% 
            filter(exclude == 0) %>% 
            mutate(iv_triv = mosaic::zscore(iv_triv, na.rm = T)) %>% 
            group_by(election_year, iv_cand_pref_f, iv_cdv_f) %>% 
            summarize(mean_cl_normal(iv_triv)) %>% 
            mutate(outcome = "triv")
    ) %>% 
    mutate(iv_cand_pref_f = case_when(iv_cand_pref_f == "winner" ~ "Preferred Winner",
                                      iv_cand_pref_f == "loser" ~ "Preferred Loser"),
           iv_cand_pref_f = factor(iv_cand_pref_f, levels = c("Preferred Loser",
                                                              "Preferred Winner")),
           iv_cdv_f = case_when(iv_cdv_f == "likes one" ~  "Easy", 
                                iv_cdv_f ==  "likes both" ~ "Hard\n(Positive)", 
                                iv_cdv_f == "dislikes both" ~ "Hard\n(Negative)"),
           iv_cdv_f = factor(iv_cdv_f, levels = c("Hard\n(Negative)", 
                                                  "Easy",
                                                  "Hard\n(Positive)"))) %>% 
    ungroup %>% 
    ggplot(aes(x = iv_cdv_f, y = y, 
               color = outcome)) + 
    jtools::theme_apa()+
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  width = 0.4, size = 1.75,
                  position = position_dodge(width = .5)) + 
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
    scale_color_manual("Outcome:", values = c("purple3", "darkorange2")) + 
    facet_grid(iv_cand_pref_f~election_year ) + 
    theme(text = element_text(size = 12, family = "Times New Roman"),
          plot.title = element_text(size = 12,
                                    family = "Times New Roman",
                                    face = "bold"),
          plot.subtitle = element_text(size = 12,
                                       family = "Times New Roman",
                                       face = "italic"),
          legend.position = "bottom",
          legend.background = element_rect(color = "black",
                                           size = .5),
          legend.spacing.x = unit(0.01,"cm"),
          legend.margin = margin(r = .1, l = .1, 
                                 t = .05, b = .05, unit = "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.key.width = unit(.5, "cm"),
          
          axis.title.x = element_text(margin = 
                                          margin(r = 0, l = 0, 
                                                 t = .5, b = .0, 
                                                 unit = "cm")),
          plot.caption = ggtext::element_markdown(hjust = 0)
    ) +
    labs(title = "Standardized Polarization and Trivialization", 
         x = NULL,
         y = "Purple = polarization\nOrange = trivialization"
         ) + 
    guides(shape = "none")

ggsave("plots/std_plot.PNG", 
       plot = std_plot,
       width = 6, height = 6, unit = "in")   
# elephant graveyard ------------------------------------------------------
# beware all who venture beyond the elephant graveyard... (i.e., down here is
# just a section where I create temp notes and such as building the scirpts. You
# can ignore anything you find down here, if anything.)
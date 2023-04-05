df %>% 
    filter(exclude == 0) %>% 
    select(election_year, 
           cov_party3_f,
           demog_age,
           demog_sex,
           demog_race,
           cov_edu_recode_f,
           demog_marital
           # cov_pk,
           # cov_days,
           # voted_early,
           # cov_voted_f,
           # pref_switcher,
           # cov_expect_f,
           # cdv_groups
    ) %>% 
    mutate(
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
        cov_party3_f = factor(case_when(cov_party3_f == "ind" ~ "Ind.",
                                        cov_party3_f == "gop" ~ "Rep.",
                                        cov_party3_f == "dem" ~ "Dem."),
                              levels = c("Dem.", "Rep.", "Ind."))
        
        #pref_switcher = case_when(is.na(pref_switcher) ~ "no", TRUE ~ "yes"),
        #cdv_groups = stringr::str_to_sentence(cdv_groups),
        #cov_expect_f = ifelse(cov_expect_f == "loser", "yes", "no"),
    ) %>% 
    tbl_strata(
        strata = election_year,
        .header = "**{strata}**  (N = {n})",
        ~.x %>%
            tbl_summary(by = cov_party3_f, 
                        
                        # type = cov_pk ~ "continuous",
                        
                        statistic = list(
                            all_continuous() ~ "{mean} ({sd})",
                            all_categorical() ~ "{n} ({p}%)"),
                        
                        sort =  list(
                            c(demog_race, 
                              demog_marital
                              #cdv_groups, 
                            ) ~ "frequency"
                        ),
                        
                        label = 
                            c(demog_age ~ "Age",
                              demog_sex ~ "Sex",
                              demog_marital ~ "Marital status",
                              cov_party3_f ~ "Party",
                              demog_race ~ "Race/ethnicity",
                              cov_edu_recode_f ~ "Education"
                              # cov_pk ~ "Political knowledge",
                              # cov_days ~ "Post-election time (days)",
                              # cov_voted_f ~ "Voted",
                              # voted_early ~ "Voted early",
                              # pref_switcher ~ "Switched preference",
                              # cov_expect_f ~ "Expected loser",
                              # cdv_groups ~ "Difficulty/valence group"
                            ),
                        
                        missing = "no") %>% 
            bold_labels() %>% 
            italicize_levels() %>% 
            modify_footnote(all_stat_cols() ~ NA) %>% 
            modify_header(all_stat_cols() ~ "**{level}**<br>n = {n}") %>%
            modify_caption("**Table 1. Respondent Demographics and Study Characteristics by Year and Party**") %>% 
            remove_row_type(
                variables = demog_sex,
                type =  "level",
                level_value = "Other"
            )
        
    )  


df_analysis %>% 
    filter(!is.na(dv_cand_polz)) %>% 
    map(is.na) %>% map(table)

##### in B&J:
# DV = dv_cand_polz;

# pref_loser 0 = no, 1 = yes
# vote 0 no, 1 = yes

# expect 0 = winner, 1 = loser
# abs_att = mf_group - 0 = one in each group, 1 if both over 50, -1 for MF
# pre_polz = abs(ft_dem_cand_pre - ft_gop_cand_pre)

# party -1 = D, 0 = I, 1 = R

# edu was 1-4, 
# info was 1-5
#####

primary_mods <- 
    df_analysis %>% 
    filter(!is.na(dv_cand_polz)) %>% 
    filter(is.na(pref_switcher)) %>% 
    filter(cov_party_n %in% c(-.5, .5)) %>% 
    
    group_by(election_year) %>% 
    nest %>% 
    
    mutate(
        # the original/candidate polarization 
        cand_mod = map(data,
                       ~lm(dv_cand_polz ~ 
                               iv_cand_pref_n + cov_voted_at_all_n + 
                               cov_expect_n + iv_mf_group_n + cov_pre_polz + 
                               cov_party_n + cov_edu_recode_n + cov_pk,
                           data = .x)),
        glance_cand = map(cand_mod, ~glance(.x)),
        tidy_cand = map(cand_mod, ~tidy(.x, conf.int = TRUE)),
        
        # B&J's model candidate polarization 
        bj_mod = map(data,
                     ~lm(dv_cand_polz ~ 
                             bj_pref_loser + bj_vote + 
                             bj_expect + bj_abs_att + bj_pre_polz + 
                             bj_party + bj_edu + bj_info,
                         data = .x)),
        glance_bj = map(bj_mod, ~glance(.x)),
        tidy_bj = map(bj_mod, ~tidy(.x, conf.int = TRUE)),
        
        # vp polarization 
        vp_mod = map(data,
                     ~lm(dv_vp_polz ~ 
                             iv_cand_pref_n + cov_voted_at_all_n + 
                             cov_expect_n + iv_mf_group_n + cov_pre_polz + 
                             cov_party_n + cov_edu_recode_n + cov_pk,
                         data = .x)),
        glance_vp = map(vp_mod, ~glance(.x)),
        tidy_vp = map(vp_mod, ~tidy(.x, conf.int = TRUE)),
        
        # ideo polarization 
        ideo_mod = map(data,
                       ~lm(dv_ideo_polz ~ 
                               iv_cand_pref_n + cov_voted_at_all_n + 
                               cov_expect_n + iv_mf_group_n + cov_pre_polz + 
                               cov_party_n + cov_edu_recode_n + cov_pk,
                           data = .x)),
        glance_ideo = map(ideo_mod, ~glance(.x)),
        tidy_ideo = map(ideo_mod, ~tidy(.x, conf.int = TRUE)),
        
    )

primary_mods %>% 
    select(glance_cand) %>% unnest 

primary_mods %>% 
    select(tidy_vp) %>% unnest 

primary_mods %>% 
    select(tidy_ideo) %>% unnest 



secondary_mods <- 
    df_analysis %>% 
    filter(!is.na(dv_cand_polz)) %>% 
    filter(is.na(pref_switcher)) %>% 
    filter(cov_party_n %in% c(-.5, .5)) %>% 
    
    group_by(election_year) %>% 
    nest %>% 
    
    # iv_prox_sp_tot
    # iv_sup_state_color
    # iv_sup_nat_color
    # iv_triv
    
    mutate(
        prox_mod = map(data,
                       ~lm(dv_cand_polz ~ 
                               iv_cand_pref_n + cov_voted_at_all_n + 
                               cov_expect_n + iv_mf_group_n + cov_pre_polz + 
                               cov_party_n + cov_edu_recode_n + cov_pk+
                               iv_prox_sp_tot,
                           data = .x)),
        glance_prox = map(prox_mod, ~glance(.x)),
        tidy_prox = map(prox_mod, ~tidy(.x, conf.int = TRUE)),
        
        state_mod = map(data,
                        ~lm(dv_cand_polz ~ 
                                iv_cand_pref_n + cov_voted_at_all_n + 
                                cov_expect_n + iv_mf_group_n + cov_pre_polz + 
                                cov_party_n + cov_edu_recode_n + cov_pk+
                                iv_sup_state_color,
                            data = .x)),
        glance_state = map(state_mod, ~glance(.x)),
        tidy_state = map(state_mod, ~tidy(.x, conf.int = TRUE)),
        
        nat_mod = map(data,
                      ~lm(dv_cand_polz ~ 
                              iv_cand_pref_n + cov_voted_at_all_n + 
                              cov_expect_n + iv_mf_group_n + cov_pre_polz + 
                              cov_party_n + cov_edu_recode_n + cov_pk+
                              iv_sup_nat_color,
                          data = .x)),
        glance_nat = map(nat_mod, ~glance(.x)),
        tidy_nat = map(nat_mod, ~tidy(.x, conf.int = TRUE)),
        
        triv_mod = map(data,
                       ~lm(dv_cand_polz ~ 
                               iv_cand_pref_n + cov_voted_at_all_n + 
                               cov_expect_n + iv_mf_group_n + cov_pre_polz + 
                               cov_party_n + cov_edu_recode_n + cov_pk+
                               iv_triv,
                           data = .x)),
        glance_triv = map(triv_mod, ~glance(.x)),
        tidy_triv = map(triv_mod, ~tidy(.x, conf.int = TRUE))
    )

secondary_mods %>% 
    select(contains("tidy")) %>% 
    pivot_longer(cols = tidy_prox:tidy_triv, names_to = "model") %>% 
    unnest(value) %>% 
    ggplot(aes(x = term, color = model)) + theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
    facet_grid(~election_year) + 
    geom_hline(yintercept = 0) + 
    coord_flip()

# elephant graveyard ------------------------------------------------------
df_analysis %>% group_by(election_year) %>% select(iv_triv) %>% table

#
full_df %>% 
    filter(party %in% c("dem", "gop")) %>% 
    group_by(election_year) %>% 
    nest %>% 
    mutate(mod = map(data,
                     ~lm(dv_cand_polz ~ iv_mf_group_n + iv_cand_pref_f + iv_triv + 
                             cov_party_f + cov_voted_at_all_f + cov_expect_f +
                             cov_pk + cov_edu_recode_n, 
                         data = .x)),
           glance_mod = map(mod, ~glance(.x)),
           tidy_mod = map(mod, ~tidy(.x, conf.int = TRUE))) %>% 
    unnest(tidy_mod) %>% 
    ggplot(aes(x = term)) + theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
    facet_wrap(~election_year) + 
    geom_hline(yintercept = 0) + 
    coord_flip()

# replicate B&J01?Eea20

# DV = dv_cand_polz;

# pref_loser 0 = no, 1 = yes
# vote 0 no, 1 = yes

# expect 0 = winner, 1 = loser
# abs_att = mf_group - 0 = one in each group, 1 if both over 50, -1 for MF
# pre_polz = abs(ft_dem_cand_pre - ft_gop_cand_pre)

# party -1 = D, 0 = I, 1 = R

# edu was 1-4, 
# info was 1-5

full_df %>% 
    mutate(bj_iv = dv_cand_polz,
           bj_pref_loser = ifelse(iv_cand_pref_f == "winner", 0, 1),
           bj_vote = ifelse(cov_voted_at_all_f == "no", 0, 1),
           bj_expect = ifelse(cov_expect_f == "winner", 0, 1),
           
           pre_dem = ifelse(ft_dem_cand_pre <= 50, "neg", "pos"),
           pre_gop = ifelse(ft_gop_cand_pre <= 50, "neg", "pos"),
           bj_abs_att =  case_when(pre_dem == "pos" & pre_gop == "pos" ~ 1,
                                   pre_dem == "pos" & pre_gop == "neg" ~ 0,
                                   pre_dem == "neg" & pre_gop == "pos" ~ 0,
                                   pre_dem == "neg" & pre_gop == "neg" ~ -1),
           
           bj_pre_polz = abs(ft_dem_cand_pre - ft_gop_cand_pre),
           bj_party = case_when(party == "dem" ~ -1,
                                party == "ind" ~ 0,
                                party == "gop" ~ 1),
           bj_edu = cov_edu_recode_n,
           bj_info = cov_pk) %>% 
    drop_na(iv_mf_group) %>% 
    group_by(election_year, iv_mf_group) %>% 
    nest %>% 
    mutate(mod = map(data,
                     ~lm(bj_iv ~ bj_pref_loser + bj_vote + bj_expect +
                             bj_pre_polz + cov_party_n + bj_edu + 
                             bj_info + iv_triv,
                         data = .x)),
           glance_mod = map(mod, ~glance(.x)),
           tidy_mod = map(mod, ~tidy(.x, conf.int = TRUE))) %>% 
    unnest(tidy_mod) %>% 
    ggplot(aes(x = term)) + theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
    facet_wrap(election_year~iv_mf_group) + 
    geom_hline(yintercept = 0) + 
    coord_flip()


combined_data %>% 
    select(ft_dem_cand_pre, ft_gop_cand_pre) %>%
    mutate(mf = ifelse(ft_dem_cand_pre <= 50 & ft_gop_cand_pre <= 50, "mf", "not_mf")) %>% 
    select(mf) %>% table




bj_data <- 
    full_df %>% 
    mutate(bj_iv = dv_cand_polz,
           bj_pref_loser = ifelse(iv_cand_pref_f == "winner", 0, 1),
           bj_vote = ifelse(cov_voted_at_all_f == "no", 0, 1),
           bj_expect = ifelse(cov_expect_f == "winner", 0, 1),
           
           pre_dem = ifelse(ft_dem_cand_pre <= 50, "neg", "pos"),
           pre_gop = ifelse(ft_gop_cand_pre <= 50, "neg", "pos"),
           bj_abs_att =  case_when(pre_dem == "pos" & pre_gop == "pos" ~ 1,
                                   pre_dem == "pos" & pre_gop == "neg" ~ 0,
                                   pre_dem == "neg" & pre_gop == "pos" ~ 0,
                                   pre_dem == "neg" & pre_gop == "neg" ~ -1),
           
           bj_pre_polz = abs(ft_dem_cand_pre - ft_gop_cand_pre),
           bj_party = case_when(party == "dem" ~ -1,
                                party == "ind" ~ 0,
                                party == "gop" ~ 1),
           bj_edu = cov_edu_recode_n,
           bj_info = cov_pk)

bj_data %>% 
    group_by(election_year) %>% 
    select(bj_abs_att) %>% 
    table

bj_data %>% 
    drop_na(bj_abs_att) %>% 
    group_by(election_year, bj_abs_att) %>% 
    nest %>% 
    mutate(mod = map(data,
                     ~lm(bj_iv ~ bj_pref_loser + bj_vote + bj_expect +
                             bj_pre_polz + cov_party_n + bj_edu + 
                             bj_info,
                         data = .x)),
           glance_mod = map(mod, ~glance(.x)),
           tidy_mod = map(mod, ~tidy(.x, conf.int = TRUE))) %>% 
    unnest(tidy_mod) %>% 
    ggplot(aes(x = term,
               color = election_year)) + 
    theme_linedraw() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
    facet_wrap(~bj_abs_att) + 
    geom_hline(yintercept = 0) + 
    coord_flip()





# post-election survey dates ----------------------------------------------

# November 3, 2020
raw2020 %>% 
    select("survey_date_post" = V203078,  "id_2020" = V200001) %>% 
    mutate(post_date = lubridate::as_date(survey_date_post),
           days_after_election = post_date - lubridate::ymd(20201103)) %>% 
    select(id_2020, days_after_election) %>% 
    right_join(df_analysis, by = "id_2020") %>% 
    ggplot(aes(y = dv_cand_polz, x = days_after_election)) + 
    geom_point() + 
    geom_smooth(method)+
    facet_wrap(~iv_cand_pref_f) 


# November 8, 2016
raw2016 %>% select("survey_date_post" = V165002,   "id_2016" = V160001) %>% 
    mutate(post_date = lubridate::as_date(survey_date_post),
           days_after_election = post_date - lubridate::ymd(20161108)) %>% 
    select(id_2016, days_after_election) %>% 
    right_join(df_analysis, by = "id_2016")


has_post_16 <- 
    raw2016 %>% 
    select("survey_date_post" = V165002,   
           "id_2016" = V160001) %>% 
    mutate(post_date = lubridate::as_date(survey_date_post),
           days_after_election = post_date - lubridate::ymd(20161108)) %>% 
    filter(!is.na(post_date)) %>% 
    pull(id_2016) 

has_post_20 <- 
    raw2020 %>% 
    select("survey_date_post" = V203078,  
           "id_2020" = V200001) %>% 
    mutate(post_date = lubridate::as_date(survey_date_post),
           days_after_election = post_date - lubridate::ymd(20201103)) %>% 
    filter(!is.na(post_date)) %>% 
    pull(id_2020) 




# manuscript stuff --------------------------------------------------------

df_manny <- 
    df_analysis %>%  #12,550 to 11,101
    filter(id_2016 %in% has_post_16 | id_2020 %in% has_post_20)

# df_manny %>% 
#     mutate(dv_missing = is.na(dv_cand_polz)) %>% 
#     group_by(election_year) %>% 
#     select(dv_missing) %>% 
#     summarise(table(dv_missing))

df_manny <- 
    df_manny %>% #11,101 to 10,615
    mutate(dv_missing = is.na(dv_cand_polz)) %>% 
    filter(dv_missing == FALSE)

df_manny <- 
    df_manny %>% # remove expect = 480; 10,615 to 10,135
    filter(!is.na(cov_expect_f)) 

df_manny <- 
    df_manny %>% # remove PK = 361; 10,135 to 9,774
    filter(!is.na(cov_pk)) 

df_manny <- 
    df_manny %>% # remove edu = 182; 9,774 to 9,592
    filter(!is.na(cov_edu_recode_n)) 

df_manny <- 
    df_manny %>% # remove party = 9; 9,592 to 9,583
    filter(!is.na(cov_party_f)) 

df_manny %>% 
    filter(is.na(iv_triv)) #62

df_manny %>% 
    filter(is.na(iv_sup_state_color)) 
df_manny %>% 
    filter(is.na(iv_prox_sp_tot)) 

df_manny %>% 
    filter(is.na(dv_vp_polz)) 

# percentage of responses within X days 
# 50% @ 12 days, 75% @ 22
df_manny %>% 
    group_by(days_after_election) %>% 
    count %>% 
    ungroup %>% 
    mutate(cum = cumsum(n)/sum(n)) %>% 
    print(n = 40)



# missing party example
full_df %>% mutate(missing = party_id < 0) %>% select(missing) %>% table

full_df %>% 
    filter(is.na(ft_dem_cand_post)) %>% 
    mutate(missing = is.na(party_id)) %>% 
    select(missing) %>% table

df_analysis %>% 
    filter(id_2016 %in% has_post_16 | id_2020 %in% has_post_20) %>% 
    filter(is.na(dv_cand_polz)) %>% 
    filter(is.na(cov_pk)) %>% 
    mutate(missing = is.na(cov_party_n)) %>% 
    select(missing) %>% table


df_manny %>% 
    group_by(election_year) %>% 
    count





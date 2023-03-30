library(tidyverse)
library(broom)

# read in/select down -----------------------------------------------------
# read in raw data from ANES
raw2016 <- haven::read_sav("data/anes_timeseries_2016.sav")
raw2020 <- haven::read_sav("data/anes_timeseries_2020_spss_20220210.sav")

working_2016_vars <- 
    raw2016 %>% 
    select(
        "id_2016" = V160001, 
        "ft_dem_cand_pre" = V161086, 
        "ft_gop_cand_pre" = V161087, 
        "ft_dem_cand_post" = V162078, 
        "ft_gop_cand_post" = V162079, 
        "ft_dem_vp_cand_pre" = V161090, 
        "ft_gop_vp_cand_pre" = V161091, 
        "ft_dem_vp_cand_post" = V162091, 
        "ft_gop_vp_cand_post" = V162092, 
        "lc7_self_pre" = V161126, 
        "lc7_dempty_pre" = V161130, 
        "lc7_goppty_pre" = V161131, 
        "lc10_self_post" = V162289, 
        "lc10_dempty_post" = V162287, 
        "lc10_goppty_post" = V162288, 
        "pref_pre" = V161064x, 
        "pref_post" = V162058x, 
        "soc_sup_talkcand" =  V162010, 
        "soc_sup_rally" =  V162011, 
        "soc_sup_candwrk" =  V162013, 
        "soc_sup_discuss" =  V162174, 
        "last_12_protest" =  V162018a, 
        "last_12_petition" =  V162018b, 
        "last_12_online" =  V162018e, 
        "last_12_commwrk" =  V162195, 
        "last_12_commissue" =  V162196, 
        "days_discussed_past_week" =  V162174a, 
        "who_win_nat" = V161146, # 1 = dem, 2 = gop
        "nat_win_close" = V161147, # 1 = close, 2 = by a lot
        "who_win_state" = V161148, # 1 = dem, 2 = gop
        "state_win_close" = V161149, # 1 = close, 2 = by a lot
        "make_dif_pwr" = V162281, # 1 = "no dif", 5 = "great dif"
        "make_dif_vote" = V162282, # 1 = "no dif", 5 = "great dif"
        "expectations" = V161146, # 1 = dem, 2 = gop
        "party_id" = V161158x, # 1-7 = strong D, not very strong D, ind D, independent, ind R, nvs R, strong R
        "education" = V161270, 
        "pk_senate_yrs" = V161513, # should be 6
        "pk_spend_least" = V161514, # should be 1
        "pk_house_ctrl" = V161515, # should be 2
        "pk_senate_ctrl" = V161516, # should be 2
        "survey_date_post" = V165002
    ) %>% 
    mutate(survey_date_post = ifelse(str_detect(survey_date_post, "-"), NA, survey_date_post),
           across(everything(), ~labelled::remove_labels(.x)))



working_2020_vars <- 
raw2020 %>% 
    select(
        "id_2020" = V200001, 
        "id_dual_16" = V160001_orig, 
        "ft_dem_cand_pre" = V201151, 
        "ft_gop_cand_pre" = V201152, 
        "ft_dem_cand_post" = V202143, 
        "ft_gop_cand_post" = V202144, 
        "ft_dem_vp_cand_pre" = V201153, 
        "ft_gop_vp_cand_pre" = V201154, 
        "ft_dem_vp_cand_post" = V202156, 
        "ft_gop_vp_cand_post" = V202157, 
        "lc7_self_pre" = V201200, 
        "lc7_dempty_pre" = V201206, 
        "lc7_goppty_pre" = V201207, 
        "lc10_self_post" = V202439, 
        "lc10_dempty_post" = V202437, 
        "lc10_goppty_post" = V202438, 
        "pref_pre" = V201075x, 
        "pref_post" = V202105x, 
        "soc_sup_talkcand" = V202009, 
        "soc_sup_rally" = V202014, 
        "soc_sup_candwrk" = V202016, 
        "soc_sup_discuss" = V202022, 
        "last_12_protest" = V202025, 
        "last_12_petition" = V202026, 
        "last_12_online" = V202029, 
        "last_12_commwrk" = V202031, 
        "last_12_commissue" = V202032, 
        "days_discussed_past_week" = V202023, 
        "who_win_nat" = V201217, # 1 = dem, 2 = gop
        "nat_win_close" = V201218, # 1 = close, 2 = by a lot
        "who_win_state" = V201219, # 1 = dem, 2 = gop
        "state_win_close" = V201220, # 1 = close, 2 = by a lot
        "make_dif_pwr" = V202431, # 1 = "no dif", 5 = "great dif"
        "make_dif_vote" = V202432, # 1 = "no dif", 5 = "great dif"
        "expectations" = V201217, # 1 = dem, 2 = gop
        "party_id" = V201231x, # 1-7 = strong D, not very strong D, ind D, independent, ind R, nvs R, strong R
        "education" = V201510, 
        "pk_senate_yrs" = V201644, # should be 6
        "pk_spend_least" = V201645, # should be 1
        "pk_house_ctrl" = V201646, # should be 1
        "pk_senate_ctrl" = V201647, # should be 2
        "survey_date_post" = V203078
    ) %>% 
    mutate(survey_date_post = ifelse(str_detect(survey_date_post, "-"), NA, survey_date_post),
           across(everything(), ~labelled::remove_labels(.x)))


# join datasets -----------------------------------------------------------

combined_data <- 
    bind_rows(
    working_2016_vars %>% 
        mutate(election_year = 2016),
    
    working_2020_vars %>% 
        mutate(election_year = 2020)
) %>% 
    select(starts_with("id_"), everything())


# recoding/cleaning values 
full_df <- 
combined_data %>% 
    mutate(in_both_years = case_when(id_dual_16 == -1 ~ "no",
                                     is.na(id_dual_16) ~ "no",
                                     TRUE ~ "yes"),
           
           # how many days after the election was the survey
           survey_date_post = lubridate::as_date(survey_date_post),
           election_day = case_when(election_year == 2016 ~ lubridate::ymd(20161108),
                                    election_year == 2020 ~ lubridate::ymd(20201103)),
           
           days_after_election = as.numeric(survey_date_post - election_day),
           
           # feeling therms.
           ft_dem_cand_pre =  ifelse(ft_dem_cand_pre == 998, NA, ft_dem_cand_pre),# 998 = NA
           ft_dem_cand_post = ifelse(ft_dem_cand_post == 998, NA, ft_dem_cand_post), # 998 = NA
           ft_gop_cand_post = ifelse(ft_gop_cand_post == 998, NA, ft_gop_cand_post), # 998 = NA
           ft_dem_vp_cand_pre = ifelse(ft_dem_vp_cand_pre >= 998, NA, ft_dem_vp_cand_pre),# 998 or 999 = NA
           ft_gop_vp_cand_pre = ifelse(ft_gop_vp_cand_pre >= 998, NA, ft_gop_vp_cand_pre), # 998 or 999 = NA
           ft_dem_vp_cand_post = ifelse(ft_dem_vp_cand_post >= 998, NA, ft_dem_vp_cand_post), # 998 or 999 = NA
           ft_gop_vp_cand_post = ifelse(ft_gop_vp_cand_post >= 998, NA, ft_gop_vp_cand_post), # 998 or 999 = NA
           
           lc7_self_pre = ifelse(lc7_self_pre == 99, NA, lc7_self_pre),  # 99 = NA (n = 2145)
           
           #pref_pre  end in 0 = Dem, 1 = GOP, 2 = other; starts with 1 = vote, 2 = intent, 3 = not going to vote
           voted_early = ifelse(pref_pre %in% 10:12, 1, 0), # 0 = no, 1 = yes
           cand_pref = case_when(pref_pre %in% c(10, 20, 30) ~ "dem",
                                 pref_pre %in% c(11, 21, 31) ~ "gop",
                                 pref_pre %in% c(12, 22, 32) ~ "other"),
           
           # pref_post  # end in 0 = Dem, 1 = GOP, 2 = other; starts with 1 = vote, 3 = no vote
           cand_pref_post = case_when(pref_post %in% c(10, 30) ~ "dem",
                                      pref_post %in% c(11, 31) ~ "gop",
                                      pref_post %in% c(12, 32) ~ "other"),
           
           # proximal social support
           soc_sup_talkcand =  ifelse(soc_sup_talkcand == 2, 0, soc_sup_talkcand), # was 1 = Y, 2 = N, changed to 1 = y, 0 = n
           soc_sup_rally =  ifelse(soc_sup_rally == 2, 0, soc_sup_rally), # was 1 = Y, 2 = N, changed to 1 = y, 0 = n
           soc_sup_candwrk =  ifelse(soc_sup_candwrk == 2, 0, soc_sup_candwrk), # was 1 = Y, 2 = N, changed to 1 = y, 0 = n
           soc_sup_discuss =  ifelse(soc_sup_discuss == 2, 0, soc_sup_discuss), # was 1 = Y, 2 = N, changed to 1 = y, 0 = n
           last_12_protest =  ifelse(last_12_protest == 2, 0, last_12_protest), # was 1 = Y, 2 = N, changed to 1 = y, 0 = n
           last_12_petition =  ifelse(last_12_petition == 2, 0, last_12_petition), # was 1 = Y, 2 = N, changed to 1 = y, 0 = n
           last_12_online =  ifelse(last_12_online == 2, 0, last_12_online), # was 1 = Y, 2 = N, changed to 1 = y, 0 = n
           last_12_commwrk =  ifelse(last_12_commwrk == 2, 0, last_12_commwrk), # was 1 = Y, 2 = N, changed to 1 = y, 0 = n
           last_12_commissue =  ifelse(last_12_commissue == 2, 0, last_12_commissue), # was 1 = Y, 2 = N, changed to 1 = y, 0 = n
           
           who_win_nat = ifelse(who_win_nat > 2, NA, who_win_nat), 
           who_win_state = ifelse(who_win_state > 2, NA, who_win_state), 
           
           expectations =   ifelse(expectations > 2, NA, expectations), 
           
           # party_id  1-7 = strong D, not very strong D, ind D, (4) independent, ind R, nvs R, strong R
           party = case_when(party_id %in% 1:3 ~ "dem",
                             party_id == 4 ~ "ind",
                             party_id %in% 5:7 ~ "gop"), 
           
           party_strength = case_when(party_id %in% c(1, 7) ~ 2, # 0 = weak, 1 = mod, 2 = strong
                                      party_id %in% c(2, 6) ~ 1,
                                      party_id %in% c(3, 5) ~ 0),
           
           pk_senate_yrs =  case_when(pk_senate_yrs < 0 ~ pk_senate_yrs, 
                                      pk_senate_yrs == 6 ~ 1, 
                                      TRUE ~ 0), # should be 6
           
           pk_spend_least =  case_when(pk_spend_least < 0 ~ pk_spend_least, 
                                       pk_spend_least == 1 ~ 1, 
                                       TRUE ~ 0), # should be 1
           
           pk_house_ctrl =  case_when(pk_house_ctrl < 0 ~ pk_house_ctrl, 
                                      pk_house_ctrl == 2 & election_year == 2016 ~ 1, 
                                      pk_house_ctrl == 1 & election_year == 2020 ~ 1, 
                                      TRUE ~ 0), # should be 2 in 2016 and 1 in 2020
           
           pk_senate_ctrl =  case_when(pk_senate_ctrl < 0 ~ pk_senate_ctrl, 
                                       pk_senate_ctrl == 2 ~ 1, 
                                       TRUE ~ 0), # should be 2
           
           cov_edu_recode_n = case_when(education < 0 ~ education, 
                                  
                                  education %in% 1:8 & election_year == 2016 ~ 1, # lt_hs
                                  education == 9 & election_year == 2016 ~ 2,  # hs_grad
                                  education %in% 10:12 & election_year == 2016 ~ 3,  # lt_bach
                                  education == 13 & election_year == 2016 ~ 4, # bach
                                  education == 14 & election_year == 2016 ~ 5, # master
                                  education == 15 & election_year == 2016 ~ 6,  # advanced
                                  
                                  education == 1 & election_year == 2020 ~ 1, # lt_hs
                                  education == 2 & election_year == 2020 ~ 2, # hs_grad
                                  education %in% 3:5 & election_year == 2020 ~ 3, # lt_bach
                                  education == 6 & election_year == 2020 ~ 4, # bach
                                  education == 7 & election_year == 2020 ~ 5, # master
                                  education == 8 & election_year == 2020 ~ 6, # advanced
                                  
                                  education == 90 ~ 2, # specify HS grad
                                  education == 95 ~ -2), # other/specified, unavailable
           
           cov_edu_recode_f = factor(cov_edu_recode_n, 
                               levels = 1:6,
                               labels = c("lt_hs", "hs_grad", "lt_bach", "bach", "master", "advanced"))
           
               ) %>% 
    
    mutate(across(everything(), ~ifelse(.x < 0 , NA, .x))) %>% 
    
    # compute vars that need computations
    mutate(
        # directional change scores
        cand_change_dem = ft_dem_cand_post - ft_dem_cand_pre,
        cand_change_gop = ft_gop_cand_post - ft_gop_cand_pre,
        vp_change_dem = ft_dem_vp_cand_post - ft_dem_vp_cand_pre,
        vp_change_gop = ft_gop_vp_cand_post - ft_gop_vp_cand_pre,
    
        # Ideo polarization
        in_pty_distance_pre = case_when(party == "gop" ~ abs(lc7_self_pre - lc7_goppty_pre),
                                        party == "dem" ~ abs(lc7_self_pre - lc7_dempty_pre)),
        
        out_pty_distance_pre = case_when(party == "gop" ~ abs(lc7_self_pre - lc7_dempty_pre),
                                        party == "dem" ~ abs(lc7_self_pre - lc7_goppty_pre)),
        
        ideo_polz_pre = out_pty_distance_pre - in_pty_distance_pre, 

        lc7_self_post = (1/3) + (2/3) * lc10_self_post,
        lc7_dempty_post = (1/3) + (2/3) * lc10_dempty_post,
        lc7_goppty_post = (1/3) + (2/3) * lc10_goppty_post,
        
        in_pty_distance_post = case_when(party == "gop" ~ abs(lc7_self_post - lc7_goppty_post),
                                        party == "dem" ~ abs(lc7_self_post - lc7_dempty_post)),
        
        out_pty_distance_post = case_when(party == "gop" ~ abs(lc7_self_post - lc7_dempty_post),
                                         party == "dem" ~ abs(lc7_self_post - lc7_goppty_post)),
        
        ideo_polz_post = out_pty_distance_post - in_pty_distance_post,
        
        
        ##### primary vars
        
        # cand polz = "affective polarization" 
        dv_cand_polz = abs(ft_dem_cand_post - ft_gop_cand_post) - abs(ft_dem_cand_pre - ft_gop_cand_pre),
        dv_vp_polz = abs(ft_dem_vp_cand_post - ft_gop_vp_cand_post) - abs(ft_dem_vp_cand_pre - ft_gop_vp_cand_pre),
        dv_ideo_polz = ideo_polz_post - ideo_polz_pre,
        
        #mf group = does not like either 
        pre_dem = ifelse(ft_dem_cand_pre < 50, "neg", "pos"),
        pre_gop = ifelse(ft_gop_cand_pre < 50, "neg", "pos"),
        
        # 0 = MF group, 1 = one pos (will filter our -1)
        iv_mf_group_n = case_when(pre_dem == "pos" & pre_gop == "pos" ~ -1,
                                  pre_dem == "pos" & pre_gop == "neg" ~ 0,
                                  pre_dem == "neg" & pre_gop == "pos" ~ 0,
                                  pre_dem == "neg" & pre_gop == "neg" ~ 1),
        
        iv_mf_group_f = factor(iv_mf_group_n, levels = c(-1, 0, 1),
                               labels = c("both_pos", "one_pos", "neither_pos_MF")),
        
        # prefer winner = -.5, prefer loser = .5
        iv_cand_pref_n = case_when(election_year == 2016 & cand_pref == "gop" ~ - .5,
                                   election_year == 2020 & cand_pref == "dem" ~ - .5,
                                   TRUE ~ .5),
        iv_cand_pref_f = factor(iv_cand_pref_n, levels = c(-.5, .5), labels = c("winner", "loser")),
        
        iv_prox_sup = rowSums(select(., contains("soc_sup"))),
        iv_prox_sup_yr = rowSums(select(., contains("last_12"))),
        iv_prox_sp_tot = iv_prox_sup + iv_prox_sup_yr,
        
        
        iv_sup_state_color = case_when(election_year == 2016 & who_win_state == 2 ~ 1 * state_win_close,
                                       election_year == 2016 & who_win_state == 1 ~ -1 * state_win_close,
                                       election_year == 2020 & who_win_state == 1 ~ 1 * state_win_close,
                                       election_year == 2020 & who_win_state == 2 ~ -1 * state_win_close),

        iv_sup_nat_color = case_when(election_year == 2016 & who_win_nat == 2 ~ 1 * nat_win_close,
                                       election_year == 2016 & who_win_nat == 1 ~ -1 * nat_win_close,
                                       election_year == 2020 & who_win_nat == 1 ~ 1 * nat_win_close,
                                       election_year == 2020 & who_win_nat == 2 ~ -1 * nat_win_close),

        iv_triv = rowMeans(select(., contains("make")), na.rm = T) - 1,
        
        
        # covariates
        #education
        # cov_edu_recode_n
        cov_party_n = case_when(party == "dem" ~ -.5,
                                party == "gop" ~ .5,
                                party == "ind" ~ 0),
        
        cov_party_f = factor(cov_party_n, levels = c(-.5, 0, .5), labels = c("dem", "ind", "gop")),
        
        # voted or not:  -.5 = no, .5 = yes
        cov_voted_at_all_n = ifelse(pref_post %in% 10:12, .5, -.5), # -.5 = no, .5 = yes
        cov_voted_at_all_f = factor(cov_voted_at_all_n, levels = c(-.5, .5), labels = c("no", "yes")),
        
        
        # -.5 = expect winner, .5 = expect loser
        cov_expect_n = case_when(election_year == 2016 & expectations == 1 ~ .5, # "expectations" 1 = dem, 2 = gop 
                               election_year == 2016 & expectations == 2 ~ -.5,
                               election_year == 2020 & expectations == 2 ~ .5,
                               election_year == 2020 & expectations == 1 ~ -.5), 
        cov_expect_f = factor(cov_expect_n, levels = c(-.5, .5), labels = c("winner", "loser")),
        
        cov_pre_polz = abs(ft_dem_cand_pre - ft_gop_cand_pre),
        
        # pol knowledge
        cov_pk =  rowSums(select(., contains("pk_"))),
    
        
        # BJ operationalizaitons 
        bj_dv = dv_cand_polz,
        bj_pref_loser = ifelse(iv_cand_pref_f == "winner", 0, 1), # same but dummy coded (rather than -5./.5 contrast)
        bj_vote = ifelse(cov_voted_at_all_f == "no", 0, 1), # same but dummy coded (rather than -5./.5 contrast)
        bj_expect = ifelse(cov_expect_f == "winner", 0, 1), # same but dummy coded (rather than -5./.5 contrast)
        
        # chage order: make 0 = one pos, MF = -1, both pos = 1
        bj_abs_att =  case_when(iv_mf_group_f == "both_pos" ~ 1,
                                iv_mf_group_f == "one_pos" ~ 0,
                                iv_mf_group_f == "neither_pos_MF" ~ -1),
        
        bj_pre_polz = cov_pre_polz, 
        bj_party = case_when(party == "dem" ~ -1,
                             party == "ind" ~ 0,
                             party == "gop" ~ 1),
        bj_edu = cov_edu_recode_n,
        bj_info = cov_pk,
        
        # pref switchers 
        pre_pref = case_when(ft_dem_cand_pre > ft_gop_cand_pre ~ "prefer_dem",
                             ft_dem_cand_pre < ft_gop_cand_pre ~ "prefer_gop"), 
        post_pref = case_when(ft_dem_cand_post > ft_gop_cand_post ~ "prefer_dem",
                             ft_dem_cand_post < ft_gop_cand_post ~ "prefer_gop"), 
        
        pref_switcher = case_when(pre_pref == "prefer_dem" & post_pref == "prefer_gop" ~ "switcher",
                                  pre_pref == "prefer_gop" & post_pref == "prefer_dem" ~ "switcher")
    ) 

# analysis data -----------------------------------------------------------

df_analysis <- 
    full_df %>% 
    select(starts_with("id_"),
           election_year,
           in_both_years,
           days_after_election,
           voted_early,
           starts_with("dv_"),
           starts_with("iv_"),
           starts_with("cov_"),
           starts_with("bj_"),
           starts_with("cand_change"),
           days_discussed_past_week,
           pref_switcher) 
    
           
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

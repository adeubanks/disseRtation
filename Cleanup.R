library(tidyverse)

# read in/select down -----------------------------------------------------
# read in raw data from ANES
raw_2016 <- haven::read_sav("data/anes_timeseries_2016.sav")
raw_2020 <- haven::read_sav("data/anes_timeseries_2020_spss_20220210.sav")

picked_2016_vars <- 
    raw_2016 %>% 
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
        "survey_date_post" = V165002,
        "demog_age" = V161267,
        "demog_marital" = V161268,
        "demog_race" = V161310x,
    ) %>% 
    mutate(election_year = "2016", 
           survey_date_post = ifelse(str_detect(survey_date_post, "-"), NA, survey_date_post),
           across(everything(), ~labelled::remove_labels(.x)))

picked_2020_vars <- 
    raw_2020 %>% 
    select(
        "id_2020" = V200001, 
        "id_matched_16" = V160001_orig, 
        "ft_dem_cand_pre" = V201151, 
        "ft_gop_cand_pre" = V201152, 
        "ft_dem_cand_post" = V202143, 
        "ft_gop_cand_post" = V202144, 
        "ft_dem_vp_cand_pre" = V201153, 
        "ft_gop_vp_cand_pre" = V201154, 
        "ft_dem_vp_cand_post" = V202156, 
        "ft_gop_vp_cand_post" = V202157, 
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
        "survey_date_post" = V203078,
        "demog_age" = V201507x,
        "demog_marital" = V201508,
        "demog_race" = V201549x,
    ) %>% 
    mutate(election_year = "2020",
           survey_date_post = ifelse(str_detect(survey_date_post, "-"), NA, survey_date_post),
           across(everything(), ~labelled::remove_labels(.x)))

# join datasets -----------------------------------------------------------

full_df <- 
bind_rows(picked_2016_vars, 
          picked_2020_vars) %>% 
    select(starts_with("id_"), everything()) %>% 
    
    mutate(in_both_years = case_when(id_matched_16 == -1 ~ "no",
                                     is.na(id_matched_16) ~ "no",
                                     TRUE ~ "yes"),
           
           id_matched_20 = ifelse(is.na(id_matched_16), NA, id_2020),
           
           # how many days after the election was the survey
           survey_date_post = lubridate::as_date(survey_date_post),
           election_day = case_when(election_year == 2016 ~ lubridate::ymd(20161108),
                                    election_year == 2020 ~ lubridate::ymd(20201103)),
           
           # feeling therms.
           ft_dem_cand_pre =  ifelse(ft_dem_cand_pre == 998, NA, ft_dem_cand_pre),# 998 = NA
           ft_dem_cand_post = ifelse(ft_dem_cand_post == 998, NA, ft_dem_cand_post), # 998 = NA
           ft_gop_cand_post = ifelse(ft_gop_cand_post == 998, NA, ft_gop_cand_post), # 998 = NA
           ft_dem_vp_cand_pre = ifelse(ft_dem_vp_cand_pre >= 998, NA, ft_dem_vp_cand_pre),# 998 or 999 = NA
           ft_gop_vp_cand_pre = ifelse(ft_gop_vp_cand_pre >= 998, NA, ft_gop_vp_cand_pre), # 998 or 999 = NA
           ft_dem_vp_cand_post = ifelse(ft_dem_vp_cand_post >= 998, NA, ft_dem_vp_cand_post), # 998 or 999 = NA
           ft_gop_vp_cand_post = ifelse(ft_gop_vp_cand_post >= 998, NA, ft_gop_vp_cand_post), # 998 or 999 = NA
           
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
           
           #who_win_nat = ifelse(who_win_nat > 2, NA, who_win_nat), 
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
           
           edu_recode_n = case_when(education < 0 ~ education, 
                                        
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
           
           edu_recode_f = factor(edu_recode_n, 
                                     levels = 1:6,
                                     labels = c("lt_hs", "hs_grad", "lt_bach", 
                                                "bach", "master", "advanced")),
           
           
           # marital status:
           # 1 - 2 = currently married (spouse present/absent, respectively)
           # 3-5 = formerly married (widowed, divorced, separated, respectively)
           # 6 = never married
           
           demog_marital = case_when(demog_marital %in% 1:2 ~ "married",
                                     demog_marital %in% 3:5 ~ "formerly married",
                                     demog_marital == 6 ~ "never married"),
           # race: 
           # 1 = white, 
           # 2 = black, 
           # 3 = Asian, native Hawaiian or other Pacif Islr, 
           # 4 = Native American or Alaska Native, 
           # 5 = hispanic, 
           # 6 = Other non-Hispanic incl multiple races
           demog_race = case_when(demog_race == 1 ~ "White",
                                  demog_race == 2 ~ "Black",
                                  demog_race == 3 ~ "Asian", #(incl. native Hawaiian/other Pacif Islr)
                                  demog_race == 4 ~ "Native American/Alaskan",
                                  demog_race == 5 ~ "Hispanic",
                                  demog_race == 6 ~ "Other" #(non-Hispanic incl multiple races
                                  )
           
    ) %>% 
    
    mutate(across(where(is.numeric), ~ifelse(.x < 0 , NA, .x))) %>% 
    
    # compute vars that need computations
    mutate(
        # directional change scores
        cand_change_dem = ft_dem_cand_post - ft_dem_cand_pre,
        cand_change_gop = ft_gop_cand_post - ft_gop_cand_pre,
        vp_change_dem = ft_dem_vp_cand_post - ft_dem_vp_cand_pre,
        vp_change_gop = ft_gop_vp_cand_post - ft_gop_vp_cand_pre,
        
       
        ##### primary vars
        
        #############  DV: cand polz = "affective polarization" 
        dv_cand_polz = abs(ft_dem_cand_post - ft_gop_cand_post) - abs(ft_dem_cand_pre - ft_gop_cand_pre),
        dv_vp_polz = abs(ft_dem_vp_cand_post - ft_gop_vp_cand_post) - abs(ft_dem_vp_cand_pre - ft_gop_vp_cand_pre),

        
        #############   IVs: 
    
        # candidate preference
        # prefer winner = -.5, prefer loser = .5
        iv_cand_pref_n = case_when(election_year == 2016 & cand_pref == "gop" ~ - .5,
                                   election_year == 2020 & cand_pref == "dem" ~ - .5,
                                   TRUE ~ .5),
        iv_cand_pref_f = factor(iv_cand_pref_n, levels = c(-.5, .5), labels = c("winner", "loser")),
        
        # Choice difficulty/valence (CDV)
        pre_dem = ifelse(ft_dem_cand_pre < 50, "neg", "pos"),
        pre_gop = ifelse(ft_gop_cand_pre < 50, "neg", "pos"),
        
        choice_dif_valence = case_when(pre_dem == "pos" & pre_gop == "pos" ~ "likes both",
                                       pre_dem == "pos" & pre_gop == "neg" ~ "likes one",
                                       pre_dem == "neg" & pre_gop == "pos" ~ "likes one",
                                       pre_dem == "neg" & pre_gop == "neg" ~ "dislikes both"),
        
        iv_cdv_diff_c1 = case_when(choice_dif_valence == "likes one" ~ 1,
                                   choice_dif_valence %in% c("likes both", "dislikes both") ~ -0.5),
        
        iv_cdv_val_c2 = case_when(choice_dif_valence == "likes one" ~ 0,
                                  choice_dif_valence == "likes both" ~ 0.5,
                                  choice_dif_valence == "dislikes both" ~ -0.5),
        
        iv_cdv_diff_c1_char = case_when(iv_cdv_diff_c1 == 1 ~ "easy choice",
                                        iv_cdv_diff_c1 %in% c(-.5, .5) ~ "hard choice"),
        
        iv_cdv_val_c2_char = case_when(iv_cdv_val_c2 == 0 ~ "ignored",
                                       iv_cdv_val_c2 == 0.5 ~ "positive easy",
                                       iv_cdv_val_c2 == -0.5 ~ "negative easy"),
        
        iv_cdv_f = factor(choice_dif_valence,
                          levels = c("likes one", "likes both", "dislikes both")),

        # proximal in-group support 
        iv_prox_sup = rowSums(select(., contains("soc_sup"), contains("last_12"))),
        
        # prox support #2: state "color"                   
            # who win state ==  1 = dem, 2 = gop, win close  == 1 =  by a little, 2 = by a lot,
            # so if the opposite cand is expected to win, multiply by -1 to create -2 , -1, 1, 2 scale,
            # then re-scale that to be 0 - 3. 
        iv_prox_sup_state = case_when(election_year == 2016 & who_win_state == 2 ~ state_win_close,
                                      election_year == 2016 & who_win_state == 1 ~ -1 * state_win_close,
                                      election_year == 2020 & who_win_state == 1 ~ state_win_close,
                                      election_year == 2020 & who_win_state == 2 ~ -1 * state_win_close),
        
        iv_prox_sup_state = case_when(iv_prox_sup_state == -2 ~ 0,
                                      iv_prox_sup_state == -1 ~ 1,
                                      iv_prox_sup_state == 1 ~ 2,
                                      iv_prox_sup_state == 2 ~ 3),
        
        # Trivialization 
        iv_triv = rowMeans(select(., contains("make")), na.rm = T) - 1,
        
        
        ############# covariates
        
        # pre-election polarization 
        cov_pre_polz = abs(ft_dem_cand_pre - ft_gop_cand_pre),
        
        # partisanship with only two levels
        cov_party_n = case_when(party == "dem" ~ -.5,
                                party == "gop" ~ .5),
        
        cov_party_f = factor(cov_party_n, levels = c(-.5, .5), labels = c("dem", "gop")),
        
        # partisanship with all three levels
        cov_party3_n = case_when(party == "dem" ~ -.5,
                                party == "gop" ~ .5,
                                party == "ind" ~ 0),
        
        cov_party3_f = factor(cov_party3_n, levels = c(0, -.5, .5), labels = c("ind","dem", "gop")),
        
        # pol knowledge
        cov_pk =  rowSums(select(., contains("pk_"))),
        
        # education
        cov_edu_recode_n = edu_recode_n, 
        cov_edu_recode_f = edu_recode_f,
        
        # -.5 = expect winner, .5 = expect loser
        cov_expect_n = case_when(election_year == 2016 & expectations == 1 ~ .5, # "expectations" 1 = dem, 2 = gop 
                                 election_year == 2016 & expectations == 2 ~ -.5,
                                 election_year == 2020 & expectations == 2 ~ .5,
                                 election_year == 2020 & expectations == 1 ~ -.5), 
        cov_expect_f = factor(cov_expect_n, levels = c(-.5, .5), labels = c("winner", "loser")),
        
        # voted or not:  -.5 = no, .5 = yes
        cov_voted_n = ifelse(pref_post %in% 10:12, .5, -.5), # -.5 = no, .5 = yes
        cov_voted_f = factor(cov_voted_n, levels = c(-.5, .5), labels = c("no", "yes")),
        
        # days between election and survey
        cov_days = as.numeric(survey_date_post - election_day), 
        
        
        ########### alternate codings
        
        # 0 = pref winner, 1 = pref loser
        dummy_pref_loser = ifelse(iv_cand_pref_f == "winner", 0, 1), 
        
        # dummy coded difficulty/valence where reference group is the middle (i.e., likes one but not the other)
        dummy_cdv_ref_easy = factor(choice_dif_valence,
                           levels = c("likes one", "likes both", "dislikes both")),
        # reference group is the negative choice (i.e., liking one but not the other is still positive)
        dummy_cdv_ref_neg = factor(choice_dif_valence,
                                    levels = c("dislikes both", "likes one", "likes both")),
        
        # dummy coded part with all three optionf for reference levels
        dummy_party_ref_ind = factor(cov_party_n, levels = c(0, -.5, .5), labels = c("ind","dem", "gop")),
        dummy_party_ref_dem = factor(cov_party_n, levels = c(-.5, .5, 0), labels = c("dem", "gop", "ind")),
        dummy_party_ref_gop = factor(cov_party_n, levels = c(.5, -.5, 0), labels = c("gop","dem", "ind")),
        
        # dummy education less than bach vs bach or beyond
        dummy_edu_2 = factor(
            case_when(edu_recode_n %in% 1:3 ~ 0, 
                      edu_recode_n %in% 4:6 ~ 1),
            levels = c(0, 1),
            labels = c("lt_bach", "bach_or_beyond")),
        
        # dummy education less than bach, bach, beyond bach
        dummy_edu_3_bach_ref = factor(
            case_when(edu_recode_n %in% 1:3 ~ 1,
                      edu_recode_n %in% 4 ~ 0,
                      edu_recode_n %in% 5:6 ~ 2),
            levels = c(0, 1, 2),
            labels = c("bach", "lt_bach", "bach_or_beyond")),
        
        dummy_edu_3_ltbach_ref = factor(
            case_when(edu_recode_n %in% 1:3 ~ 0,
                      edu_recode_n %in% 4 ~ 1,
                      edu_recode_n %in% 5:6 ~ 2),
            levels = c(0, 1, 2),
            labels = c("lt_bach", "bach", "bach_or_beyond")),
        
        # 0 = expect winner, 1 = expect loser
        dummy_expect = factor(cov_expect_n, levels = c(-.5, .5), labels = c("winner", "loser")),
        
        # 0 = did not vote, 1 = voted
        dummy_vote = factor(cov_voted_n, levels = c(-.5, .5), labels = c("no", "yes")),
        
        ######## mean or median centering options
        cent_iv_prox_sup_m = iv_prox_sup - mean(iv_prox_sup, na.rm = T), 
        cent_iv_prox_sup_state_m = iv_prox_sup_state - mean(iv_prox_sup_state, na.rm = T), 
        cent_cov_pre_polz_m = cov_pre_polz - mean(cov_pre_polz, na.rm = T), 
        cent_cov_pk_m = cov_pk - mean(cov_pk, na.rm = T), 
        cent_cov_days_med = cov_days - median(cov_days, na.rm = T),
        
        # "pref switchers" 
        pre_pref = case_when(ft_dem_cand_pre > ft_gop_cand_pre ~ "prefer_dem",
                             ft_dem_cand_pre < ft_gop_cand_pre ~ "prefer_gop"), 
        post_pref = case_when(ft_dem_cand_post > ft_gop_cand_post ~ "prefer_dem",
                              ft_dem_cand_post < ft_gop_cand_post ~ "prefer_gop"), 
        
        pref_switcher = case_when(pre_pref == "prefer_dem" & post_pref == "prefer_gop" ~ "switcher",
                                  pre_pref == "prefer_gop" & post_pref == "prefer_dem" ~ "switcher"),
        
        # exclusions 
        exclude = NA,
        exclude = case_when(
            
            # did post-election survey
            is.na(survey_date_post) & is.na(exclude) ~ 1, #12,550 to 11,101 (1,449)
            
            # is not missing DV
            is.na(dv_cand_polz) & is.na(exclude) ~ 2,     #11,101 to 10,615 (486)
            
            # no expectation
            is.na(cov_expect_f) & is.na(exclude) ~ 3,    # 10,615 to 10,135 (480)
            
            # missing PK
            is.na(cov_pk) & is.na(exclude) ~ 4, # 10,135 to 9,774 (361)
            
            # missing education
            is.na(cov_edu_recode_n) & is.na(exclude) ~ 5, # 9,774 to 9,592 (182)
            
            # missing party
            is.na(cov_party3_f) & is.na(exclude) ~ 6, # 9,592 to 9,583 (9)
            
            # keep everyone else for primary analyses (filter to exclude == 0 for analyses)
            TRUE ~ 0
        )
    ) 

full_df %>% select(contains("_f")) %>% colnames

contrasts(full_df$cov_expect_f)
contrasts(full_df$cov_expect_f) <- c(-.5, .5)
colnames(attr(full_df$cov_expect_f, "contrasts")) <- "loser"

contrasts(full_df$iv_cand_pref_f)
contrasts(full_df$iv_cand_pref_f) <- c(-.5, .5)
colnames(attr(full_df$iv_cand_pref_f, "contrasts")) <- "loser"

contrasts(full_df$cov_voted_f)
contrasts(full_df$cov_voted_f) <- c(-.5, .5)
colnames(attr(full_df$cov_voted_f, "contrasts")) <- "yes"

contrasts(full_df$cov_party_f)
contrasts(full_df$cov_party_f) <- c(-.5, .5)
colnames(attr(full_df$cov_party_f, "contrasts")) <- "gop"

contrasts(full_df$iv_cdv_f)
contrasts(full_df$iv_cdv_f) <- matrix(c(1, -.5, -.5, 0, .5, -.5), nrow = 3)
colnames(attr(full_df$iv_cdv_f, "contrasts")) <- c("c1_difficulty", "c2_valence")

# write_rds(full_df, "data/full_df.RDS")

# analysis data -----------------------------------------------------------

df_analysis <- 
    full_df %>% 
    select(exclude, 
           starts_with("id_"),
           election_year,
           in_both_years,
           starts_with("dv_"),
           starts_with("iv_"),
           starts_with("cov_"),
           starts_with("dummy_"),
           starts_with("cent_"),
           starts_with("cand_change"),
           pref_switcher,
           voted_early,
           starts_with("demog_"),
           "cdv_groups" = choice_dif_valence,
           party_strength, 
           survey_date_post) 

# write_rds(df_analysis, "data/df_analysis.RDS")

# missing party example (full_df needed) ----------------------------------

full_df %>% # 58 raw
    mutate(missing = ifelse(is.na(party_id), TRUE, FALSE)) %>% 
    select(missing) %>% table

full_df %>%  # 18 after removing those with no post
    filter(is.na(ft_dem_cand_post)) %>% 
    mutate(missing = ifelse(is.na(party_id), TRUE, FALSE)) %>% 
    select(missing) %>% table

full_df %>% # 18 after removing those with no post survey or DV
    filter(is.na(survey_date_post), is.na(dv_cand_polz)) %>% 
    mutate(missing = ifelse(is.na(party_id), TRUE, FALSE)) %>% 
    select(missing) %>% table

full_df %>% # 18 after removing those with no post survey, DV, or PK
    filter(is.na(survey_date_post), 
           is.na(dv_cand_polz),
           is.na(cov_pk)) %>% 
    mutate(missing = ifelse(is.na(party_id), TRUE, FALSE)) %>% 
    select(missing) %>% table






# -------------------------------------------------------------------------




full_df %>% 
    mutate(id = row_number()) %>% 
    select(id,
           lc7_self_post,
           lc7_dempty_post,
           lc7_goppty_post,
           lc10_self_post,
           lc10_dempty_post,
           lc10_goppty_post) %>% 
    drop_na() %>% 
    #    slice_sample(n = 1000) %>% 
    pivot_longer(!id) %>% 
    separate(name, sep = "_", into = c("lc", "tgt", "ignore")) %>% 
    select(!ignore) %>% 
    pivot_wider(names_from = lc) %>% 
    ggplot(aes(x = lc7, y = lc10)) + 
    geom_jitter(aes(color = id), alpha = .1) + 
    facet_grid(.~tgt) + theme_classic()





df_analysis_x <- 
    df_analysis %>% 
    mutate(iv_easy_v_hard = case_when(iv_mf_group_f == "one_pos" ~ 1,
                                      iv_mf_group_f %in% c("both_pos", "neither_pos_MF") ~ -.5),
           iv_hard_compare_mf_v_pos = case_when(iv_mf_group_f == "one_pos" ~ 0,
                                                iv_mf_group_f == "both_pos" ~ .5,
                                                iv_mf_group_f == "neither_pos_MF" ~ -.5))

contrasts(df_analysis_x$iv_mf_group_f)

matrix(c(-.5, 1, -.5, .5, 0, -.5), nrow = 3)

primary_mods_2 <- 
    df_analysis_x %>% 
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
                               cov_expect_n + iv_mf_group_n + 
                               cov_pre_polz + 
                               cov_party_n + cov_edu_recode_n + cov_pk,
                           data = .x)),
        glance_cand = map(cand_mod, ~glance(.x)),
        tidy_cand = map(cand_mod, ~tidy(.x, conf.int = TRUE)),
        
        # B&J's model candidate polarization 
        cand_mod_cont = map(data,
                       ~lm(dv_cand_polz ~ 
                               iv_cand_pref_n + cov_voted_at_all_n + 
                               cov_expect_n + iv_easy_v_hard + iv_hard_compare_mf_v_pos + 
                               cov_pre_polz + 
                               cov_party_n + cov_edu_recode_n + cov_pk,
                           data = .x)),
        glance_cand_cont = map(cand_mod_cont, ~glance(.x)),
        tidy_cand_cont = map(cand_mod_cont, ~tidy(.x, conf.int = TRUE)),
        
        
    )


primary_mods_2 %>% 
    select(contains("tidy")) %>% 
    pivot_longer(cols = tidy_cand:tidy_cand_cont, names_to = "model") %>% 
    unnest(value) %>% 
    #filter(model == "tidy_cand") %>% 
    ggplot(aes(x = term, color = model)) + theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high),
                    position = position_dodge(width = .5)) +
    facet_grid(~election_year) + 
    geom_hline(yintercept = 0) + 
    coord_flip()


# -------------------------------------------------------------------------


# Target value = 1/3 + (2/3)*Current scale value

tibble(lc10 = dnorm(100, ))


raw2020 %>% 
    select("lc10_self_post" = V202439, 
           "lc10_dempty_post" = V202437, 
           "lc10_goppty_post" = V202438) %>% 
    mutate(across(everything(), ~labelled::remove_labels(.x))) %>% 
    pivot_longer(everything(), names_to = "var", values_to = "lc") %>% 
    filter(lc > 0) %>% 
    ggplot(aes(x = lc)) + 
    geom_histogram(binwidth = 1) +
    facet_wrap(~var)


raw2020 %>% 
    select("lc10_self_post" = V202439, 
           "lc10_dempty_post" = V202437, 
           "lc10_goppty_post" = V202438) %>% 
    mutate(across(everything(), ~labelled::remove_labels(.x)),
           self7 = (1/3) + (2/3) * lc10_self_post,
           dem7 = (1/3) + (2/3) * lc10_dempty_post,
           gop7 = (1/3) + (2/3) * lc10_goppty_post,) %>% 
    pivot_longer(everything(), names_to = "var", values_to = "lc") %>% 
    filter(lc > 0)%>% 
    ggplot(aes(x = lc)) + 
    geom_histogram(binwidth = .5) +
    facet_wrap(~var, scales = "free")



raw2016 %>% 
    select("lc7_self_post" = V162171, 
           "lc10_self_post" = V162289) %>% 
    mutate(across(everything(), ~labelled::remove_labels(.x)),
           converted = (1/3) + (2/3) * lc10_self_post) %>% 
    pivot_longer(everything(), names_to = "var", values_to = "lc") %>% 
    filter(lc > 0, lc <11)%>% 
    ggplot(aes(x = lc)) + 
    geom_histogram(binwidth = .5) +
    facet_wrap(~var, scales = "free")


raw2016 %>% 
    select("lc7_self_post" = V162171, 
           "lc10_self_post" = V162289) %>% 
    filter(lc7_self_post %in% 1:7, 
           lc10_self_post %in% 1:10) %>% 
    mutate(across(everything(), ~labelled::remove_labels(.x)),
           converted = (1/3) + ((2/3) * lc10_self_post)
               ) %>% 
    #select(lc7_self_post, converted) %>% 
    mutate(dif = converted - lc7_self_post) %>% 
    view


# That looks just a little messy, but let's apply it to the example of a 5-point
# scale to be converted to a 7-point scale. Since the minimum of the 5-point
# scale is 1, we have a=1, b=5 in the first transformation. Similarly for the
# second transformation, we have A=1, B=7. Putting them together we get:

(7 - 1) * (lc7_self_post - 1) / (10 - 1) + 1 
() * (lc7_self_post - 1) / (9) + 1 


# vote_yn #(pref_post)
# cand_pref (pref_pre)
# pre_election_pol
# affective pol cand
# affective pol vp
# dem cand change
# gop cand change
# lc7_dem_dif_pre
# lc7_gop_dif_pre
# lc7_ideo_pol_pre
# lc10_to_7_self
# lc10_to_7_dem
# lc10_to_7_gop
# lc7_dem_dif_post
# lc7_gop_dif_post
# lc7_ideo_pol_post
# lc7_pre_post_pol
# ingrp_prox_supprt
# pk
### RECODE
# education
# party_id
# expectations
# 
# 
# 
# 
# 
# 
# 
# 
# 




# looking at group means --------------------------------------------------

#   Means broken down by:
#    Year, prefer, mf group, voted, party


df_analysis %>% 
    mutate(party = case_when(bj_party == -1 ~ "dem", 
                             bj_party == 0 ~ "ind", 
                             bj_party == 1 ~ "gop"),
           election_year = factor(election_year)) %>% 
    drop_na(iv_mf_group_f, party, dv_cand_polz) %>% 
    group_by(election_year, iv_cand_pref_f, iv_mf_group_f) %>% 
    summarise(party_count = n(),
              polz = mean_cl_normal(dv_cand_polz)) %>% 
    ggplot(aes(x = iv_mf_group_f, color = iv_cand_pref_f)) + 
    theme_classic()+
    geom_hline(yintercept = 0)+
    geom_pointrange(aes(y = polz$y, ymin = polz$ymin, ymax = polz$ymax),
                    position = position_dodge(width = .5)) + 
    facet_grid(election_year ~.) + 
    scale_y_continuous(limits = c(-10, 30))



df_analysis %>% 
    mutate(party = case_when(bj_party == -1 ~ "dem", 
                             bj_party == 0 ~ "ind", 
                             bj_party == 1 ~ "gop"),
           election_year = factor(election_year),
           iv_sup_nat_color = case_when(iv_sup_nat_color == -2 ~ 0,
                                        iv_sup_nat_color == -1 ~ 1,
                                        iv_sup_nat_color == 1 ~ 2,
                                        iv_sup_nat_color == 2 ~ 3),
           iv_sup_state_color = case_when(iv_sup_state_color == -2 ~ 0,
                                          iv_sup_state_color == -1 ~ 1,
                                          iv_sup_state_color == 1 ~ 2,
                                          iv_sup_state_color == 2 ~ 3)) %>% 
    drop_na(dv_cand_polz, iv_sup_nat_color) %>% 
    group_by(election_year, iv_cand_pref_f, iv_sup_nat_color) %>% 
    summarise(count = n(),
              polz = mean_cl_normal(dv_cand_polz)) %>% 
    ggplot(aes(x = iv_sup_nat_color, color = iv_cand_pref_f, y = polz$y)) + 
    theme_classic()+
    geom_hline(yintercept = 0)+
    geom_pointrange(aes(ymin = polz$ymin, ymax = polz$ymax),
                    position = position_dodge(width = .5)) + 
    facet_grid(election_year ~ .) + 
        scale_y_continuous(limits = c(-15, 50)) + 
    geom_smooth(method = "lm", se = F) + 
    scale_x_continuous(labels = c("lose by a lot", "lose by a little",
                                  "win by a little", "win by a lot"))

df_analysis %>% 
    mutate(party = case_when(bj_party == -1 ~ "dem", 
                             bj_party == 0 ~ "ind", 
                             bj_party == 1 ~ "gop"),
           election_year = factor(election_year),
           iv_sup_nat_color = case_when(iv_sup_nat_color == -2 ~ 0,
                                        iv_sup_nat_color == -1 ~ 1,
                                        iv_sup_nat_color == 1 ~ 2,
                                        iv_sup_nat_color == 2 ~ 3),
           iv_sup_state_color = case_when(iv_sup_state_color == -2 ~ 0,
                                          iv_sup_state_color == -1 ~ 1,
                                          iv_sup_state_color == 1 ~ 2,
                                          iv_sup_state_color == 2 ~ 3)) %>% 
    drop_na(dv_cand_polz, iv_prox_sp_tot) %>% 
    group_by(election_year, iv_cand_pref_f, iv_prox_sp_tot) %>% 
    summarise(count = n(),
              polz = mean_cl_normal(dv_cand_polz)) %>% 
    ggplot(aes(x = iv_prox_sp_tot, color = iv_cand_pref_f, y = polz$y)) + 
    theme_classic()+
    geom_hline(yintercept = 0)+
    geom_pointrange(aes(ymin = polz$ymin, ymax = polz$ymax),
                    position = position_dodge(width = .5)) + 
    facet_grid(election_year ~ .) + 
    scale_y_continuous(limits = c(-15, 50)) + 
    geom_smooth(method = "lm", se = F) 


df_analysis %>% 
    mutate(party = case_when(bj_party == -1 ~ "dem", 
                             bj_party == 0 ~ "ind", 
                             bj_party == 1 ~ "gop"),
           election_year = factor(election_year)) %>% 
    drop_na(dv_cand_polz, iv_triv) %>% 
    group_by(election_year, iv_cand_pref_f, iv_triv) %>% 
    summarise(count = n(),
              polz = mean_cl_normal(dv_cand_polz)) %>% 
    ggplot(aes(x = iv_triv, color = iv_cand_pref_f, y = polz$y)) + 
    theme_classic()+
    geom_hline(yintercept = 0)+
    geom_pointrange(aes(ymin = polz$ymin, ymax = polz$ymax),
                    position = position_dodge(width = .5)) + 
    facet_grid(election_year ~ .) + 
    scale_y_continuous(limits = c(-15, 50)) + 
    geom_smooth(method = "lm", se = F) 


secondary_mods %>% 
    select(tidy_triv) %>% 
    unnest(tidy_triv) %>% 
    mutate(mod = "triv") %>% 
    bind_rows(

primary_mods %>% 
    select(tidy_cand) %>% 
    unnest(tidy_cand) %>% 
    mutate(mod = "no_triv")
) %>% 
    ggplot(aes(x = term, color = mod)) + 
    theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
    facet_grid(~election_year) + 
    geom_hline(yintercept = 0) + 
    coord_flip()

    

    primary_mods %>% 
        select(tidy_cand) %>% 
        unnest(tidy_cand) %>% 
        mutate(mod = "president") %>% 
        bind_rows(
            primary_mods %>% 
                select(tidy_vp) %>% 
                unnest(tidy_vp) %>% 
                mutate(mod = "vp")
        ) %>% 
    
    ggplot(aes(x = term, color = mod)) + 
        theme_classic() + 
        geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
        facet_grid(~election_year) + 
        geom_hline(yintercept = 0) + 
        coord_flip() 
    
    
    primary_mods %>% select(contains("glance")) %>% unnest(cols = c(glance_ideo))
    
    
    
primary_mods %>% select(tidy_ideo) %>% unnest %>% 
ggplot(aes(x = term, color = as.factor(election_year))) + 
    theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high),
                    position = position_dodge(width = .5)) +
#    facet_grid(~election_year) + 
    geom_hline(yintercept = 0) + 
    coord_flip()






# demographics ------------------------------------------------------------


# race: 
# 1 = white, 
# 2 = black, 
# 3 = Asian, native Hawaiian or other Pacif Islr, 
# 4 = Native American or Alaska Native, 
# 5 = hispanic, 
# 6 = Other non-Hispanic incl multiple races

# marital status:
# 1 & 2 = married,
# 3-5 = widowed, divorced, separated
# 6 = never married



df %>% 
    filter(
        exclude == 0,
        cov_party_f %in% c("dem", "gop"),
        is.na(pref_switcher),
    ) %>% 
    group_by(election_year) %>% 
    nest() %>% 
    mutate(
        h1_h2_model_int = map(data,
                             ~lm(dv_cand_polz ~ iv_cand_pref_f * iv_cdv_f + 
                                     cov_expect_f + cov_pre_polz + cov_party_n + 
                                     cov_pk + cov_edu_recode_n + cov_days,
                                 data = .x)),
    h1_h2_model = map(data,
                      ~lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + 
                              cov_expect_f + cov_pre_polz + cov_party_n + 
                              cov_pk + cov_edu_recode_n + cov_days,
                          data = .x)),
    
    h1_h2_int_tidy = map(h1_h2_model_int, ~tidy(.x, conf.int = T)), 
    h1_h2_tidy = map(h1_h2_model, ~tidy(.x, conf.int = T))) %>% 
    select(contains("tidy"))



    select(election_year, h1_h2_tidy) %>% 
    unnest(h1_h2_tidy) %>% 
ggplot(aes(x = term)) + theme_classic() + 
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
    facet_grid(~election_year) + 
    geom_hline(yintercept = 0) + 
    coord_flip()




    
    
    df %>% 
        filter(
            exclude == 0,
            cov_party_f %in% c("dem", "gop"),
            is.na(pref_switcher),
        ) %>% group_by(election_year, iv_cand_pref_f, iv_cdv_f) %>% count
    
    
    
    
    
    df %>% 
        filter(
            exclude == 0,
            cov_party_f %in% c("dem", "gop"),
            is.na(pref_switcher),
        ) %>% 
        group_by(election_year) %>% 
        nest() %>% 
        mutate(h1_h2_model = map(data,
                                 ~lm(dv_cand_polz ~ iv_cand_pref_f + iv_cdv_f + 
                                         cov_expect_f + cov_pre_polz + cov_party_n + 
                                         cov_pk + cov_edu_recode_n + cov_days,
                                     data = .x)
        ),
        h1_h2_tidy = map(h1_h2_model, ~tidy(.x, conf.int = T))
        
        ) %>% 
        select(election_year, h1_h2_tidy) %>% 
        unnest(h1_h2_tidy) %>% 
        print(n = 24) %>% 
        write_csv("~/Desktop/h1h2.csv")
    
    
    


# pasted may 6 ------------------------------------------------------------

    haven::read_sav("data/anes_timeseries_2016.sav") %>% 
        select(
            "make_dif_pwr" = V162281, # 1 = "no dif", 5 = "great dif"
            "make_dif_vote" = V162282, # 1 = "no dif", 5 = "great dif"
            "survey_date_post" = V165002
        ) %>% mutate(election_year = "2016",
                     across(everything(), ~labelled::remove_labels(.x)),
                     survey_date_post = ifelse(str_detect(survey_date_post, "-"), NA, survey_date_post)) %>% 
        bind_rows(
            
            haven::read_sav("data/anes_timeseries_2020_spss_20220210.sav") %>% 
                select(
                    "make_dif_pwr" = V202431, # 1 = "no dif", 5 = "great dif"
                    "make_dif_vote" = V202432, # 1 = "no dif", 5 = "great dif"
                    "survey_date_post" = V203078
                ) %>% 
                mutate(election_year = "2020",
                       across(everything(), ~labelled::remove_labels(.x)),
                       survey_date_post = ifelse(str_detect(survey_date_post, "-"), NA, survey_date_post))
        ) 
    
    temp %>% 
        filter(!is.na(survey_date_post),
               make_dif_pwr >= 0,
               make_dif_vote >= 0) %>% 
        group_by(election_year) %>% 
        nest %>% 
        mutate(cor = map(data, 
                         ~.x %>% select(make_dif_pwr, make_dif_vote) %>% cor(use = "pair"))) %>% 
        unnest(cor)
    
    temp %>% 
        filter(!is.na(survey_date_post),
               make_dif_pwr >= 0,
               make_dif_vote >= 0) %>% 
        group_by(election_year) %>% 
        summarise(pwr = mean(make_dif_pwr, na.rm = T),
                  vote = mean(make_dif_vote, na.rm = T),
                  pwr_sd = sd(make_dif_pwr, na.rm = T),
                  vote_sd = sd(make_dif_vote, na.rm = T),
                  pwr_skew = psych::skew(make_dif_pwr, na.rm = T),
                  vote_skew = psych::skew(make_dif_vote, na.rm = T))
    ?skew
    temp %>% 
        filter(!is.na(survey_date_post),
               make_dif_pwr >= 0,
               make_dif_vote >=0 )  %>% 
        nest(data = everything()) %>% 
        mutate(cor = map(data, 
                         ~.x %>% select(make_dif_pwr, make_dif_vote) %>% cor(use = "pair"))) %>% 
        unnest(cor)
    
    
    temp %>% 
        filter(!is.na(survey_date_post),
               make_dif_pwr >= 0,
               make_dif_vote >= 0) %>%
        ggplot(aes(x = make_dif_pwr)) + 
        theme_classic() + 
        geom_hist() +
        facet_wrap(~election_year) 
    
    temp %>% 
        filter(!is.na(survey_date_post),
               make_dif_pwr >= 0,
               make_dif_vote >= 0) %>%
        ggplot(aes(x = make_dif_vote)) + 
        theme_classic() + 
        geom_histogram() +
        facet_wrap(~election_year) 
    
    
    
    temp %>% 
        filter(!is.na(survey_date_post),
               make_dif_pwr >= 0,
               make_dif_vote >= 0) %>% 
        # group_by(election_year, make_dif_pwr, make_dif_vote) %>% 
        # count %>% 
        ggplot(aes(x = make_dif_pwr, y = make_dif_vote)) + 
        #geom_text(aes(label = n)) + 
        geom_jitter()+
        geom_smooth(method = "lm", color = "black")+
        facet_wrap(~election_year)
    
    
    
    temp %>% 
        filter(!is.na(survey_date_post),
               make_dif_pwr >= 0,
               make_dif_vote >= 0,
               election_year == "2016") %>% 
        select(make_dif_pwr, make_dif_vote) %>% 
        cor(use = "pair")
    
    temp %>% 
        filter(!is.na(survey_date_post),
               make_dif_pwr >= 0,
               make_dif_vote >= 0,
               election_year == "2020") %>% 
        select(make_dif_pwr, make_dif_vote) %>% 
        cor(use = "pair")
    
    
    temp %>% 
        filter(!is.na(survey_date_post),
               make_dif_pwr >= 0,
               make_dif_vote >= 0,
               election_year == "2020") %>% 
        select(make_dif_pwr, make_dif_vote) %>% 
        psych::alpha()
    
    
    # may 6th -----------------------------------------------------------------
    
    
    df %>% 
        group_by(election_year) %>% 
        nest() %>% 
        mutate(triv = map(data,
                          ~lm(iv_triv ~ iv_cand_pref_f * iv_cdv_f, data = .)),
               tidy_triv = map(triv, ~tidy(.x, conf.int = T))) %>% 
        # unnest(tidy_triv) %>% 
        #filter(model_type == "primary") %>% 
        # arrange(model_name) %>%
        # mutate(model_name = stringr::str_to_sentence(model_name),
        #        name = paste(election_year, model_name, sep = ":")) %>% 
        select(triv) %>% 
        deframe() %>% 
        
        huxreg(ci_level = .95, error_format = "[{conf.low}, {conf.high}]")#,
    
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
        #"Support (alt)" = "iv_prox_sup"
        #"Party (all 3)" = "cov_party3_n",
        #  "Pref. x C1" = "iv_cand_pref_floser:iv_cdv_fc1_difficulty",
        # "Pref. x C2" = "iv_cand_pref_floser:iv_cdv_fc2_valence"
    ))  



    
    
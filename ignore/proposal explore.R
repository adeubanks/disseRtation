library(tidyverse)

# start -------------------------------------------------------------------

year20 <- haven::read_sav("ind_years/anes_timeseries_2020_spss_20220210.sav")
year16 <- haven::read_sav("ind_years/anes_timeseries_2016.sav")


post_16 <- 
  year16  %>%
  rename("p_id_16" = V160001_orig,
        # "ft_dem_post" = V162078, 
        # "ft_gop_post" = V162079,
       # # "ft_dem_pre" = V161086, 
        # "ft_gop_pre" = V161087,
         
         "expectation" = V161146,
         "post_did_vote" = V162031x,
         "pre_intent" = V161030,
         "party_id" = V161158x,
         "education" = V161270,
         "age" = V161267,
         "gender" = V161342,
       #  "info_pre" = V168016,
        # "info_post" = V168112,
         "weight" = V160102,
         
         #new things added since proposal 
         "pres_vote_pre" = V161064x,
         "pres_vote_post" = V162058x
  ) %>% 
  mutate(year = 2016) %>% 
  select(!starts_with("V")) %>%
  mutate(across(everything(), ~labelled::remove_labels(.x)),
         post_did_vote = case_when(post_did_vote == 0 ~ "no",
                                   post_did_vote == 1 ~ "yes"),
         pre_intent = factor(pre_intent, levels = c(1, 2, -1), labels = c("dem", "gop", "nv")),
         party_id = case_when(party_id %in% 1:3 ~ "dem",
                              party_id %in% 4 ~ "ind",
                              party_id %in% 5:7 ~ "gop"),
         education = case_when(education %in% 1:8 ~ "lt_hs",
                               education %in% c(9, 90) ~ "hs",
                               education %in% 10:12 ~ "some_col",
                               education %in% 13:16 ~ "bach_or_more"),
         age = ifelse(age > 0, age, NA),
         gender = case_when(gender == 1 ~ "male",                           
                            gender == 2 ~ "female"),
        # info_pre = ifelse(info_pre %in% 1:5, info_pre, NA),
         #info_post = ifelse(info_post %in% 1:5, info_post, NA),
         expectation = case_when(expectation %in% c(2, 4) ~ "right",
                                 expectation %in% c(1, 3, 5:9) ~ "wrong"),
         
         #added since proposal
         pres_vote_pre = case_when(pres_vote_pre == 10 ~ "dem vote",
                                   pres_vote_pre == 11 ~ "gop vote",
                                   pres_vote_pre == 12 ~ "other vote",
                                   pres_vote_pre == 20 ~ "dem intent",
                                   pres_vote_pre == 21 ~ "gop intent",
                                   pres_vote_pre == 22 ~ "other intent",
                                   pres_vote_pre == 30 ~ "dem nv-pref",
                                   pres_vote_pre == 31 ~ "gop nv-pref",
                                   pres_vote_pre == 32 ~ "other nv-pref"),
         
         pres_vote_post = case_when(pres_vote_post == 10 ~ "dem vote",
                               pres_vote_post == 11 ~ "gop vote",
                               pres_vote_post == 12 ~ "other vote",
                               pres_vote_post == 30 ~ "dem nv-pref",
                               pres_vote_post == 31 ~ "gop nv-pref",
                               pres_vote_post == 32 ~ "other nv-pref")
  ) 

post_20 <- 
  year20 %>%
  rename("p_id" = V200001,
         "p_id_16" = V160001_orig,
        # "ft_dem_post" = V202143, 
        # "ft_gop_post" = V202144,
        # "ft_dem_pre" = V201151, 
        # "ft_gop_pre" = V201152,
         
         "expectation" = V201217,
         "post_did_vote" = V202109x,
         "pre_intent" = V201033,
         "party_id" = V201231x,
         "education" = V201511x,
         "age" = V201507x,
         "gender" = V201600,
        # "info_pre" = V201654,
        # "info_post" = V202640,
         "weight" = V200010b,
         
         #new things added since proposal 
         "pres_vote_pre" = V201075x,
         "pres_vote_post" = V202105x
  ) %>% 
  mutate(year = 2020) %>% 
  select(!starts_with("V")) %>%
  mutate(across(everything(), ~labelled::remove_labels(.x)),
         post_did_vote = case_when(post_did_vote == 1 ~ "yes",                                    
                                   post_did_vote %in% 0 ~ "no"), 
         
         pre_intent = case_when(pre_intent %in% 1 ~ "dem",
                                pre_intent %in% 2 ~ "gop",
                                pre_intent %in% -1 ~ "nv"),
         party_id = case_when(party_id %in% 1:3 ~ "dem",
                              party_id %in% 4 ~ "ind",
                              party_id %in% 5:7 ~ "gop"),
         education = case_when(education == 1 ~ "lt_hs",
                               education == 2 ~ "hs",
                               education == 3 ~ "some_col",
                               education %in% 4:5 ~ "bach_or_more"),
         age = ifelse(age %in% 17:80, age, NA),
         gender = case_when(gender == 1 ~ "male",                           
                            gender == 2 ~ "female"),
        # info_pre = ifelse(info_pre %in% 1:5, info_pre, NA),
        # info_post = ifelse(info_post %in% 1:5, info_post, NA),
         expectation = case_when(expectation == 1 ~ "right",
                                 expectation%in% 2:5 ~ "wrong"),
         
         #added since proposal
         pres_vote_pre = case_when(pres_vote_pre == 10 ~ "dem vote",
                                   pres_vote_pre == 11 ~ "gop vote",
                                   pres_vote_pre == 12 ~ "other vote",
                                   pres_vote_pre == 20 ~ "dem intent",
                                   pres_vote_pre == 21 ~ "gop intent",
                                   pres_vote_pre == 22 ~ "other intent",
                                   pres_vote_pre == 30 ~ "dem nv-pref",
                                   pres_vote_pre == 31 ~ "gop nv-pref",
                                   pres_vote_pre == 32 ~ "other nv-pref"),
         
         pres_vote_post = case_when(pres_vote_post == 10 ~ "dem vote",
                                    pres_vote_post == 11 ~ "gop vote",
                                    pres_vote_post == 12 ~ "other vote",
                                    pres_vote_post == 30 ~ "dem nv-pref",
                                    pres_vote_post == 31 ~ "gop nv-pref",
                                    pres_vote_post == 32 ~ "other nv-pref")
         
  ) 

df <- 
  post_16 %>% 
  bind_rows(post_20) %>% 
  mutate(r_id = row_number()) %>% 
  select(r_id, p_id_16, "p_id_20" = p_id, everything()) 



# post-affect
pa_20 <- 
  year20 %>% 
  select(V200001,
         V202433,
         V202434,
         V202435,
         V202436,
         V202437,
         V202438,
         V202439) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) 

colnames(pa_20) <- 
  c("p_id_20",
    "like_dem_pty",
    "like_gop_pty",
    "like_dem_cand",
    "like_gop_cand",
    "lr_dem_pty",
    "lr_gop_pty",
    "lr_self")

pa_16 <- 
year16 %>% 
  select(V160001_orig,
         V162283,
         V162284,
         V162285,
         V162286,
         V162287,
         V162288,
         V162289) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x))

colnames(pa_16) <- 
  c("p_id_16",
    "like_dem_pty",
    "like_gop_pty",
    "like_dem_cand",
    "like_gop_cand",
    "lr_dem_pty",
    "lr_gop_pty",
    "lr_self")


# party closeness
pa_20 <- year20 %>% 
  select("p_id_20" = V200001,
         V202443,
         V202444,
         "sat_democratic_proc" = V202440) %>% 
  mutate(pty = case_when(V202443 == 1 ~ "dem",
                         V202443 == 3 ~ "gop",
                         TRUE ~ "NA"),
         pty = ifelse(pty == "NA", NA, pty),
         pty_closeness = case_when(V202444 == 1 ~ 3,
                               V202444 == 2 ~ 2,
                               V202444 == 3 ~ 1,
                               TRUE ~ -99),
         pty_closeness = ifelse(pty_closeness == -99, NA, pty_closeness),
         pty_closeness = factor(pty_closeness,
                               labels = c("weak", "moderate", "strong")),
         sat_democratic_proc = ifelse(sat_democratic_proc < 0, NA, sat_democratic_proc),
         sat_democratic_proc = case_when(sat_democratic_proc == 1 ~ "very",
                                         sat_democratic_proc == 2 ~ "moderately",
                                         sat_democratic_proc == 4 ~ "barely",
                                         sat_democratic_proc == 5 ~ "not"),
         sat_democratic_proc = factor(sat_democratic_proc, 
                                      levels = c("not", "barely", "moderately", "very"))) %>% 
  select(p_id_20, pty, pty_closeness, sat_democratic_proc) %>% 
  right_join(pa_20)

pa_16 <- year16 %>% 
  select("p_id_16" = V160001_orig,
         V162292a,
         V162292b,
         "sat_democratic_proc" = V162290) %>% 
  mutate(pty = case_when(V162292a == 1 ~ "dem",
                         V162292a == 3 ~ "gop",
                         TRUE ~ "NA"),
         pty = ifelse(pty == "NA", NA, pty),
         pty_closeness = case_when(V162292b == 1 ~ 3,
                                   V162292b == 2 ~ 2,
                                   V162292b == 3 ~ 1,
                                   TRUE ~ -99),
         pty_closeness = ifelse(pty_closeness == -99, NA, pty_closeness),
         pty_closeness = factor(pty_closeness,
                                labels = c("weak", "moderate", "strong")),
         sat_democratic_proc = ifelse(sat_democratic_proc < 0, NA, sat_democratic_proc),
         sat_democratic_proc = case_when(sat_democratic_proc == 1 ~ "very",
                                         sat_democratic_proc == 2 ~ "moderately",
                                         sat_democratic_proc == 4 ~ "barely",
                                         sat_democratic_proc == 5 ~ "not"),
         sat_democratic_proc = factor(sat_democratic_proc, 
                                      levels = c("not", "barely", "moderately", "very"))) %>% 
  select(p_id_16, pty, pty_closeness, sat_democratic_proc) %>% 
  right_join(pa_16)

# diffs in what pty stands for V202216	V162190  1 = y 2 = N
# votes counted fairly V202219	V162219  (1 all, 2 most, 3 half, 4 some, 5 never)

# makes dif who is in power/people vote for V202431 V202432    V162281 V162282

# V202425	V162275  widespread corruption in gvt - 1 very, 2 quite, 3 not very, 4 hardly any 
# V202426	V162276   gvt should address income inequality 1 agree strongly to 5 disagree strongly
# V202427	V162277   gvt has done good.bad job last 4 years 1 very good to 5 very bad

# V202406	V162256	R's interest in politics     1 very to 4 not at all
# V202407	V162257	R follows politics in media     1 very to 4 not at all
# V202408	V162258	R understands most important political issues  1 agree strongly to 5 disagree strongly
pa_16 <- 
  year16 %>% 
  select("p_id_16" = V160001_orig,
         "dif_bw_ptys" = V162190,
         "vote_counted_fair" = V162219,
         "make_dif_pwr" = V162281, 
         "make_dif_vote" = V162282,
         
         "corrupt" = V162275,
         "income_inc" = V162276,
         "gov_doing_good_bad" = V162277,
         
         "pol_interest" = V162256,
         "pol_follow" = V162257,
         "pol_understand" = V162258) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  right_join(pa_16)

pa_20 <- 
  year20 %>% 
  select("p_id_20" = V200001,
         "dif_bw_ptys" = V202216,
         "vote_counted_fair" = V202219,
         "make_dif_pwr" = V202431, 
         "make_dif_vote" = V202432,
         
         "corrupt" = V202425,
         "income_inc" = V202426,
         "gov_doing_good_bad" = V202427,
         
         "pol_interest" = V202406,
         "pol_follow" = V202407,
         "pol_understand" = V202408)  %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  right_join(pa_20)


#FTs:

pa_20 <- 
  year20 %>% 
  select("p_id_20" = V200001,
         "ft_dem_cand_pre" = V201151,
         "ft_gop_cand_pre" = V201152,
         "ft_dem_vp_cand_pre" = V201153,
         "ft_gop_vp_cand_pre" = V201154,
         "ft_dem_pty_pre" = V201156,
         "ft_gop_pty_pre" = V201157,
         "ft_dem_cand_post" = V202143,
         "ft_gop_cand_post" = V202144,
         "ft_dem_vp_cand_post" = V202156,
         "ft_gop_vp_cand_post" = V202157,
         "ft_libs_post" = V202161,
         "ft_cons_post" = V202164,
         "ft_scotus_post" = V202165,
         "ft_congress_post" = V202167,
         "ft_house_dem_cand_post" = V202145,
         "ft_house_gop_cand_post" = V202146,
         "ft_senate_dem_cand_post" = V202148,
         "ft_senate_gop_cand_post" = V202149) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  right_join(pa_20)
  

pa_16 <- 
  year16 %>% 
  select("p_id_16" = V160001_orig,
         "ft_dem_cand_pre" = V161086,
         "ft_gop_cand_pre" = V161087,
         "ft_dem_vp_cand_pre" = V161090,
         "ft_gop_vp_cand_pre" = V161091,
         "ft_dem_pty_pre" = V161095,
         "ft_gop_pty_pre" = V161096,
         "ft_dem_cand_post" = V162078,
         "ft_gop_cand_post" = V162079,
         "ft_dem_vp_cand_post" = V162091,
         "ft_gop_vp_cand_post" = V162092,
         "ft_libs_post" = V162097,
         "ft_cons_post" = V162101,
         "ft_scotus_post" = V162102,
         "ft_congress_post" = V162104,
         "ft_house_dem_cand_post" = V162082,
         "ft_house_gop_cand_post" = V162083,
         "ft_senate_dem_cand_post" = V162085,
         "ft_senate_gop_cand_post" = V162086)%>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  right_join(pa_16)


pa_16 <- 
  pa_16 %>% 
  inner_join(df %>% filter(year == 2016) %>% select(r_id, p_id_16))

pa_20 <- 
  pa_20 %>% 
  inner_join(df %>% filter(year == 2020) %>% select(r_id, p_id_20))

#### CSES 5:
# attitudes about elites   CODED: higher = strongly agree (generally a bad thing with the one rev. scored item)
#1 V202409	V162259	Compromise in politics is selling out on one's principles
#2 V202410	V162260	Most politicians do not care about the people
#3 V202411	V162261	Most politicians are trustworty
#4 V202412	V162262	Politicians are the main problem in the U.S.
#5 V202413	V162263	Strong leader is good for U.S. even if bends rules to get things done
#6 V202414	V162264	People not politicians should make most important policy decisions
#7 V202415	V162265	Most politicians only care about interests of rich and powerful
year16 %>% 
  select("p_id_16" = V160001_orig,
         "elite_1" = V162259,
         "elite_2" = V162260,
         "elite_3" = V162261,
         "elite_4" = V162262,
         "elite_5" = V162263,
         "elite_6" = V162264,
         "elite_7" = V162265) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  mutate(elite_3 = 6 - elite_3) %>% 
  select(-p_id_16) %>% psych::alpha()

year20 %>% 
  select("p_id_20" = V200001,
        "elite_1" = V202409,
        "elite_2" = V202410,
        "elite_3" = V202411,
        "elite_4" = V202412,
        "elite_5" = V202413,
        "elite_6" = V202414,
        "elite_7" = V202415) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  mutate(elite_3 = 6 - elite_3) %>% 
  select(-p_id_20) %>% psych::alpha()
         

#outgroup attitudes       1 = strongly agree/low vals are poor attitudes towards minorities
# V202416	V162266	Minorities should adapt to to customs/traditions of U.S.
# V202417	V162267	The will of the majority should always prevail
# V202418	V162268	Immigrants are generally good for America's economy
# V202419	V162269	America's culture is generally harmed by immigrants
# V202420	V162270	Immigrants increase crime rates in the U.S.
year16 %>% 
  select("p_id_16" = V160001_orig,
         "out_1" = V162266,
         "out_2" = V162267,
         "out_3" = V162268,
         "out_4" = V162269,
         "out_5" = V162270) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  mutate(out_3 = 6 - out_3) %>% 
  select(-p_id_16) %>% psych::alpha()

year20 %>% 
  select("p_id_20" = V200001,
         "out_1" = V202416,
         "out_2" = V202417,
         "out_3" = V202418,
         "out_4" = V202419,
         "out_5" = V202420) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  mutate(out_3 = 6 - out_3) %>% 
  select(-p_id_20) %>% psych::alpha()


# national identity
# V202421	V162271	To be truly American important to have been born in U.S.
# V202422	V162272	To be truly American important to have American ancestry
# V202423	V162273	To be truly American important to speak English
# V202424	V162274	To be truly American important to follow America's customs/traditions

year16 %>% 
  select("p_id_16" = V160001_orig,
         "nat_id_1" = V162271,
         "nat_id_2" = V162272,
         "nat_id_3" = V162273,
         "nat_id_4" = V162274) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  select(-p_id_16) %>% psych::alpha()

year20 %>% 
  select("p_id_20" = V200001,
         "nat_id_1" = V202421,
         "nat_id_2" = V202422,
         "nat_id_3" = V202423,
         "nat_id_4" = V202424) %>% 
  mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
  select(-p_id_20) %>% psych::alpha()


# 
pa_16 <- 
  pa_16 %>% 
  left_join(
    year16 %>% 
      select("p_id_16" = V160001_orig,
             "out_1" = V162266,
             "out_2" = V162267,
             "out_3" = V162268,
             "out_4" = V162269,
             "out_5" = V162270, 
             "nat_id_1" = V162271,
             "nat_id_2" = V162272,
             "nat_id_3" = V162273,
             "nat_id_4" = V162274) %>% 
      mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
      mutate(out_3 = 6 - out_3) %>% 
      rowwise() %>% 
      mutate(outgroup_attitdue = mean(c(out_1, out_2, out_3, out_4, out_5), na.rm = T),
             nat_identity = mean(c(nat_id_1, nat_id_2, nat_id_3, nat_id_4), na.rm = T)) %>% 
      select(p_id_16, outgroup_attitdue, nat_identity)
  )

pa_20 <- 
  pa_20 %>% 
  left_join(
    year20 %>% 
      select("p_id_20" = V200001,
             "out_1" = V202416,
             "out_2" = V202417,
             "out_3" = V202418,
             "out_4" = V202419,
             "out_5" = V202420,
             "nat_id_1" = V202421,
             "nat_id_2" = V202422,
             "nat_id_3" = V202423,
             "nat_id_4" = V202424) %>% 
      mutate_all(~ifelse(.x < 0, NA, .x)) %>% 
      mutate(out_3 = 6 - out_3)%>% 
      rowwise() %>% 
      mutate(outgroup_attitdue = mean(c(out_1, out_2, out_3, out_4, out_5), na.rm = T),
             nat_identity = mean(c(nat_id_1, nat_id_2, nat_id_3, nat_id_4), na.rm = T)) %>% 
      select(p_id_20, outgroup_attitdue, nat_identity)
  )





df <- 
  df %>% 
  left_join(pa_16 %>% 
              select(-p_id_16) %>% 
              bind_rows(pa_20 %>% 
                          select(-p_id_20)), by = "r_id") 

df %>% glimpse
# df %>% write_rds("proposal_computed_stuff.R")

# df <- read_rds("proposal_computed_stuff.R")
df %>% filter(year == 2020) %>% glimpse

# df_lite <- df %>% filter(party_id %in% c("gop", "dem")) 
# 
# df_lite <- 
#   df_lite %>% 
#   select(r_id:year, pty:sat_democratic_proc, ft_dem_cand_pre:pol_understand, 
#          like_dem_pty:nat_identity) %>% 
#   mutate(across(contains("ft_"), ~ifelse(.x > 100, NA, .x)))
# 
# df_lite %>% glimpse
# df %>% glimpse
# df %>% select(pres_vote_pre, year) %>% table



df <- 
  df %>% 
  mutate(mutate(across(contains("ft_"), ~ifelse(.x > 100, NA, .x))) ,
         pre_cand = abs(ft_dem_cand_pre - ft_gop_cand_pre),
         post_cand = abs(ft_dem_cand_post - ft_gop_cand_post),
         cand_polz = post_cand - pre_cand,
         mf_group = ifelse(ft_dem_cand_pre <=50 & ft_gop_cand_pre <= 50,
                           "MFer", "non-MFer"),
         pre_vp = abs(ft_dem_vp_cand_pre - ft_gop_vp_cand_pre),
         post_vp = abs(ft_dem_vp_cand_post - ft_gop_vp_cand_post),
         vp_polz = post_vp - pre_vp)

df %>% glimpse

new_vars <- 
  year20 %>% 
  select("p_id_20" = V200001,
         "duty_to_choice" = V201225x,
         "care_alot_to_none" = V201216,
         "who_win_nat" = V201217,
         "nat_win_close" = V201218,
         "who_win_state" = V201219,
         "sate_win_close" = V201220) %>% 
  mutate(who_win_nat = ifelse(who_win_nat < 1, NA, who_win_nat),
         who_win_nat = ifelse(who_win_nat == 1, "right", "wrong")) %>% 
  mutate(across(everything(), ~labelled::remove_labels(.x))) %>% 
  left_join(df %>% select(p_id_20, r_id)) %>% 
  select(-p_id_20) %>% 
    
    bind_rows(
  
  year16 %>% 
    select("p_id_16" = V160001_orig,
           "duty_to_choice" = V161151x,
           "care_alot_to_none" = V161145,
           "who_win_nat" =    V161146,
           "nat_win_close" =  V161147,
           "who_win_state" =  V161148,
           "sate_win_close" = V161149) %>% 
    mutate(who_win_nat = ifelse(who_win_nat < 1, NA, who_win_nat),
           who_win_nat = ifelse(who_win_nat == 2, "right", "wrong")) %>% 
    mutate(across(everything(), ~labelled::remove_labels(.x))) %>% 
    left_join(df %>% filter(year == 2016) %>% select(p_id_16, r_id)) %>% 
    select(-p_id_16)
    )
  

df <- 
  df %>% 
  left_join(new_vars, by = "r_id") 


# write_rds(df, "second_step_data.RDS")

## LOOK HERE TO RESET DATA!! 
#df <- read_rds("second_step_data.RDS")

df %>% glimpse
  drop_na(party_id) %>% 
  filter(cand_polz >= 0, 
         duty_to_choice >= 0) %>% 
  group_by(year, mf_group, duty_to_choice, party_id) %>% 
  summarise(mean_cl_normal(cand_polz)) %>% rename("cand_polz" = y) %>% 
ggplot(aes(x = duty_to_choice, y = cand_polz,
           color = party_id)) + 
  theme_classic() + 
  geom_pointrange(aes(ymin = ymin, ymax = ymax),
                  position = position_dodge(width = .5))+
  facet_wrap(mf_group~year)

         
df %>% select(year, cand_polz) %>% table
  
  year20 %>% 
    select("p_id_20" = V200001,
           "duty_to_choice" = V201225x)
  

# probing a bit -----------------------------------------------------------


df %>% drop_na(mf_group, party_id) %>% 
    ggplot(aes(x = cand_polz, y = vp_polz,
               color = mf_group)) + 
    geom_point() + geom_smooth(method = "lm")+
    facet_wrap(party_id~year)

df %>% select(make_dif_pwr, make_dif_vote) %>% map(is.na) %>% map(table)  
  
  
df %>% select(make_dif_pwr, make_dif_vote) %>% corrr::correlate()

df %>% drop_na(mf_group, pre_intent) %>% 
  
  ggplot(aes(x = make_dif_pwr, y = make_dif_vote,
             color = mf_group)) + 
  geom_point() + geom_smooth(method = "lm")+
  facet_wrap(year~pre_intent)
  

df %>% select(make_dif_pwr, make_dif_vote) %>% psych::alpha()


# efa ---------------------------------------------------------------------


df %>% glimpse

# check nat_identity outgroup_attitdue items individuaoly 

full_efa_df <- 
df %>% 
  select(r_id, ends_with("_post"), -pres_vote_post, 
         like_dem_pty:like_gop_cand, lr_dem_pty, lr_gop_pty) 
  
partial_20 <- 
    year20 %>% 
      select("p_id_20" = V200001,
             "out_1" = V202416,
             "out_2" = V202417,
             "out_3" = V202418,
             "out_4" = V202419,
             "out_5" = V202420,
             "nat_id_1" = V202421,
             "nat_id_2" = V202422,
             "nat_id_3" = V202423,
             "nat_id_4" = V202424,
             "fundies" = V202159,
             "feminists" = V202160,
             "labor_unions" = V202162,
             "big_bus" = V202163,
             "lgb_ppl" = V202166,
             "muslims" = V202168,
             "christians" = V202169,
             "jews" = V202170,
             "police" = V202171,
             "trans_ppl" = V202172,
             "scientists" = V202173,
             "blm" = V202174) %>% 
      mutate_all(~ifelse(.x < 0, NA, .x),
                 ~ifelse(.x > 100, NA, .x)) %>% 
      mutate(out_3 = 6 - out_3) %>% 
      left_join(df %>% filter(year == 2020) %>% select(r_id, p_id_20))

partial_16 <- 
        year16 %>% 
          select("p_id_16" = V160001_orig,
                 "out_1" = V162266,
                 "out_2" = V162267,
                 "out_3" = V162268,
                 "out_4" = V162269,
                 "out_5" = V162270, 
                 "nat_id_1" = V162271,
                 "nat_id_2" = V162272,
                 "nat_id_3" = V162273,
                 "nat_id_4" = V162274,
                 "fundies" = V162095,
                 "feminists" = V162096,
                 "labor_unions" = V162098,
                 "big_bus" = V162100,
                 "lgb_ppl" = V162103,
                 "muslims" = V162106,
                 "christians" = V162107,
                 "jews" = V162108,
                 "police" = V162110,
                 "trans_ppl" = V162111,
                 "scientists" = V162112,
                 "blm" = V162113,) %>% 
          mutate_all(~ifelse(.x < 0, NA, .x),
                     ~ifelse(.x > 100, NA, .x)) %>% 
          mutate(out_3 = 6 - out_3) %>% 
          left_join(df %>% filter(year == 2016) %>% select(r_id, p_id_16)) 
    


partial_joined <- 
  partial_16 %>% 
  bind_rows(partial_20) %>% 
  mutate(year = ifelse(!is.na(p_id_20), 2020, 2016)) %>% 
  select(-p_id_20, -p_id_16)


full_efa_df <- 
  full_efa_df %>% 
  left_join(partial_joined) 


efa_16 <- 
  full_efa_df %>% 
  filter(year == 2016) %>%  
  select(-r_id, -year)

efa_20 <- 
  full_efa_df %>% 
  filter(year == 2020) %>%  
  select(-r_id, -year)


efa_16 %>% 
  drop_na %>% 
  count

efa_20 %>% 
  drop_na %>% 
  count

nomiss_16 <- 
  efa_16 %>% 
  drop_na %>% 
  #select(-lr_dem_pty, -lr_gop_pty)
  select(-c(out_1:nat_id_4))

nomiss_20 <- 
  efa_20 %>% 
  drop_na %>% 
  #select(-lr_dem_pty, -lr_gop_pty)
  select(-c(out_1:nat_id_4))

#### 2016

# multicollinearity/singularity?
corrr::correlate(nomiss_16)

#4
psych::KMO(nomiss_16)
psych::cortest.bartlett(nomiss_16) 

#6
pca_16 <- prcomp(nomiss_16, center = TRUE, scale. = TRUE)

# Theory = 2
sum(pca_16$sdev^2 > 1) # Kaiser’s criterion = 7
fviz_eig(pca_16, addlabels = TRUE) #Skree = 4 or 5
paran::paran(nomiss_16, iterations = 1000, all = T) # parallel = 4
summary(pca_16) # > 50% explained = 5


### 2020
# multicollinearity/singularity?
corrr::correlate(nomiss_20)

#4
psych::KMO(nomiss_20)
psych::cortest.bartlett(nomiss_20) 

#66
pca_20 <- prcomp(nomiss_20, center = TRUE, scale. = TRUE)

# Theory = 2
sum(pca_20$sdev^2 > 1) # Kaiser’s criterion = 7
fviz_eig(pca_20, addlabels = TRUE) #Skree = 4 or 5
paran::paran(nomiss_20, iterations = 1000, all = T) # parallel = 4
summary(pca_20) # > 50% explained = 5





# jan 20 ------------------------------------------------------------------


df %>% select(r_id, p_id_16, p_id_20) %>% view

sup_20 <- 
  year20 %>% 
  select("p_id_20" = V200001,
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
         
         "pays_attn" = V201005
         "interest_follow_campaigns" = V201006,
         "ft_dem_pty_pre" = V201156,
         "ft_gop_pty_pre" = V201157
         ) %>% 
  mutate(across(soc_sup_talkcand:last_12_commissue, ~case_when(.x == 1 ~ 1,
                                                               .x == 2 ~ 0,
                                                               TRUE ~ -99)),
         across(soc_sup_talkcand:last_12_commissue, ~ifelse(.x == -99, NA, .x)),
         days_discussed_past_week = ifelse(days_discussed_past_week < 0, NA, days_discussed_past_week)) %>% 
  mutate(soc_sup = rowSums(select(., contains("soc_sup"))),
         last_12 = rowSums(select(., contains("last_12"))),
         combined_sup = soc_sup + last_12)

sup_16 <- 
year16 %>% 
  select("p_id_16" = V160001_orig,
         "soc_sup_talkcand" = V162010,
         "soc_sup_rally" = V162011,
         "soc_sup_candwrk" = V162013,
         "soc_sup_discuss" = V162174,
         "last_12_protest" = V162018a,
         "last_12_petition" = V162018b,
         "last_12_online" = V162018e,
         "last_12_commwrk" = V162195,
         "last_12_commissue" = V162196,
         "days_discussed_past_week" = V162174a ) %>% 
  mutate(across(soc_sup_talkcand:last_12_commissue, ~case_when(.x == 1 ~ 1,
                                                              .x == 2 ~ 0,
                                                               TRUE ~ -99)),
         across(soc_sup_talkcand:last_12_commissue, ~ifelse(.x == -99, NA, .x)),
         days_discussed_past_week = ifelse(days_discussed_past_week < 0, NA, days_discussed_past_week)) %>% 
  mutate(soc_sup = rowSums(select(., contains("soc_sup"))),
         last_12 = rowSums(select(., contains("last_12"))),
         combined_sup = soc_sup + last_12)


hist(sup_16$combined_sup)
hist(sup_20$combined_sup)

table(sup_16$combined_sup)
table(sup_20$combined_sup)

hist(sup_16$days_discussed_past_week)
hist(sup_20$days_discussed_past_week)




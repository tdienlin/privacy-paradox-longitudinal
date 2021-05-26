# load library
library(DataCombine); library(magrittr); library(tidyverse)

# load data
# note: the actual data can be obtained from http://dx.doi.org/10.7802/1937
# paste this file into folder "data"
d_raw <- read_csv("data/privacy-longitudinal-study_data_2019-10-10.csv")

# Please note that during the submission process of this article, 
# a newer version of the data set was published (https://doi.org/10.7802/2117). 
# The new dataset contains additional data of participants that initially 
# were lost because of a transmission error. Using this newer version yields to 
# the same inferences and comparable results, with changes only in decimals

# select variables
d_wide <- d_raw %>%
  # first, we need to adapt variable measuring education
  # currently, each option exists as specific variable
  # needs to be combined into one single measure
  mutate(edu = ifelse(T1_SOZ_BIL_08 == 1, "Promotion", ifelse(
    T1_SOZ_BIL_07 == 1, "Master/Diplom/Magister", ifelse(
      T1_SOZ_BIL_06 == 1, "Bachelor", ifelse(
        T1_SOZ_BIL_05 == 1, "Berufausbildung", ifelse(
          T1_SOZ_BIL_04 == 1, "Abitur", ifelse( 
            T1_SOZ_BIL_03 == 1, "Realschule", ifelse(
              T1_SOZ_BIL_02 == 1, "Hauptschule", ifelse(
                T1_SOZ_BIL_01 == 1, "No Degree", NA
              ))))))))) %>% 
  mutate(edu = factor(edu, levels = c("No Degree", "Hauptschule", "Realschule",
                                        "Berufausbildung", "Abitur", "Bachelor",
                                        "Master/Diplom/Magister", "Promotion"))) %>%
  mutate(edu_num = as.integer(edu)) %>% 
  select(id = T1_paginiernummer, 
         contains("PBE_ONL"), 
         contains("PVE"), 
         contains("PEI"),
         sex_t = contains("_SOZ_GES_01"), 
         year_birth_t = contains("_SOZ_ALT_01"),
         contains("edu"),
         -contains("_neu"),
         contains("_TIM_INT"),
         )

# rename variables
names <- names(d_wide)
names <- gsub("T(.)\\_PBE_ONL_(..)", "pri_con_\\2_t\\1", names)
names <- gsub("T(.)\\_PVE_(..)", "inf_sha_beh_\\2_t\\1", names)
names <- gsub("T(.)\\_PEI_(..)", "inf_sha_att_\\2_t\\1", names)
names <- gsub("T(.)\\_TIM_INT_(..)", "tim_int_\\2_t\\1", names)
names(d_wide) <- names

# filter respondents with inconsistent socio-dem data
d_wide %<>% 
  filter(sex_t1 == sex_t2 | is.na(sex_t2),
         sex_t1 == sex_t3 | is.na(sex_t3),
         sex_t1 == sex_t4 | is.na(sex_t4),
         sex_t1 == sex_t5 | is.na(sex_t5),
         year_birth_t1 == year_birth_t2 | is.na(year_birth_t2),
         year_birth_t1 == year_birth_t3 | is.na(year_birth_t3),
         year_birth_t1 == year_birth_t4 | is.na(year_birth_t4),
         year_birth_t1 == year_birth_t5 | is.na(year_birth_t5))

# recode vars
d_wide %<>%
  mutate_at(vars(contains("inf_sha_beh")), funs(6 - .)) %>% # invert so that higher numbers equal more inf sharing 
  mutate(age = 2014 - year_birth_t1,
         male = sex_t1,
         na_per_t1 = rowSums(is.na(select(., contains("_t1"))) / ncol(select(., contains("_t1")))),
         na_per_t2 = rowSums(is.na(select(., contains("_t2"))) / ncol(select(., contains("_t2")))),
         na_per_t3 = rowSums(is.na(select(., contains("_t3"))) / ncol(select(., contains("_t3")))),
         na_per_t4 = rowSums(is.na(select(., contains("_t4"))) / ncol(select(., contains("_t4")))),
         na_per_t5 = rowSums(is.na(select(., contains("_t5"))) / ncol(select(., contains("_t5")))),
         na_per_inf_sha_beh_t1 = rowSums(is.na(select(d_wide, matches(".*beh_.._t1"))) / ncol(select(d_wide, matches(".*beh_.._t1")))),
         na_per_inf_sha_beh_t2 = rowSums(is.na(select(d_wide, matches(".*beh_.._t2"))) / ncol(select(d_wide, matches(".*beh_.._t2")))),
         na_per_inf_sha_beh_t3 = rowSums(is.na(select(d_wide, matches(".*beh_.._t3"))) / ncol(select(d_wide, matches(".*beh_.._t3")))),
         na_per_inf_sha_beh_t4 = rowSums(is.na(select(d_wide, matches(".*beh_.._t4"))) / ncol(select(d_wide, matches(".*beh_.._t4")))),
         na_per_inf_sha_beh_t5 = rowSums(is.na(select(d_wide, matches(".*beh_.._t5"))) / ncol(select(d_wide, matches(".*beh_.._t5")))),
         # time was measured both for weekdays and weenends
         # separated into hours and minutes; needs to be combined
         # result is time measured in minutes
         tim_int_wor_t1 = tim_int_01_t1 * 60 + tim_int_02_t1,
         tim_int_wee_t1 = tim_int_03_t1 * 60 + tim_int_04_t1,
         tim_int_wor_t2 = tim_int_01_t2 * 60 + tim_int_02_t2,
         tim_int_wee_t2 = tim_int_03_t2 * 60 + tim_int_04_t2,
         tim_int_wor_t3 = tim_int_01_t3 * 60 + tim_int_02_t3,
         tim_int_wee_t3 = tim_int_03_t3 * 60 + tim_int_04_t3,
         tim_int_wor_t4 = tim_int_01_t4 * 60 + tim_int_02_t4,
         tim_int_wee_t4 = tim_int_03_t4 * 60 + tim_int_04_t4,
         tim_int_wor_t5 = tim_int_01_t5 * 60 + tim_int_02_t5,
         tim_int_wee_t5 = tim_int_03_t5 * 60 + tim_int_04_t5,
         # check whether Internet was used at all
         use_int_t1 = -tim_int_05_t1 + 1,
         use_int_t2 = -replace_na(tim_int_05_t2, 0) + 1,
         use_int_t3 = -replace_na(tim_int_05_t3, 0) + 1,
         use_int_t4 = -replace_na(tim_int_05_t4, 0) + 1,
         use_int_t5 = -replace_na(tim_int_05_t5, 0) + 1) %>%
  mutate(tim_int_t1 = ((tim_int_wor_t1 * 5) + (tim_int_wee_t1 * 2)) / 7,
         tim_int_t2 = ((tim_int_wor_t2 * 5) + (tim_int_wee_t2 * 2)) / 7,
         tim_int_t3 = ((tim_int_wor_t3 * 5) + (tim_int_wee_t3 * 2)) / 7,
         tim_int_t4 = ((tim_int_wor_t4 * 5) + (tim_int_wee_t4 * 2)) / 7,
         tim_int_t5 = ((tim_int_wor_t5 * 5) + (tim_int_wee_t5 * 2)) / 7
  )

# replace NAs in information sharing for people who did not use the Internet
d_wide[d_wide$na_per_inf_sha_beh_t1 == 1, grepl(".*beh_.._t1", colnames(d_wide))] <- 1
d_wide[d_wide$na_per_inf_sha_beh_t2 == 1, grepl(".*beh_.._t2", colnames(d_wide))] <- 1
d_wide[d_wide$na_per_inf_sha_beh_t3 == 1, grepl(".*beh_.._t3", colnames(d_wide))] <- 1
d_wide[d_wide$na_per_inf_sha_beh_t4 == 1, grepl(".*beh_.._t4", colnames(d_wide))] <- 1
d_wide[d_wide$na_per_inf_sha_beh_t5 == 1, grepl(".*beh_.._t5", colnames(d_wide))] <- 1

# calculate means
d_wide %<>% 
  mutate(tim_int_m = rowMeans(select(., contains("tim_int_t")), na.rm = TRUE),
         tim_int_wor_m = rowMeans(select(., contains("tim_int_wor")), na.rm = TRUE),
         tim_int_wee_m = rowMeans(select(., contains("tim_int_wee")), na.rm = TRUE),
         use_int_s = rowSums(select(., use_int_t1, use_int_t2, use_int_t3, use_int_t4, use_int_t5), na.rm = TRUE),
         pri_con_m_b = rowMeans(select(., contains("pri_con_01")), na.rm = TRUE),
         pri_con_m_t1 = rowMeans(select(., pri_con_01_t1 : pri_con_09_t1), na.rm = TRUE),
         pri_con_m_t2 = rowMeans(select(., pri_con_01_t2 : pri_con_09_t2), na.rm = TRUE),
         pri_con_m_t3 = rowMeans(select(., pri_con_01_t3 : pri_con_09_t3), na.rm = TRUE),
         pri_con_m_t4 = rowMeans(select(., pri_con_01_t4 : pri_con_09_t4), na.rm = TRUE),
         pri_con_m_t5 = rowMeans(select(., pri_con_01_t5 : pri_con_09_t5), na.rm = TRUE),
         inf_sha_att_m_b = rowMeans(select(., contains("inf_sha_att_01")), na.rm = TRUE),
         inf_sha_att_m_t1 = rowMeans(select(., inf_sha_att_01_t1 : inf_sha_att_11_t1), na.rm = TRUE),
         inf_sha_att_m_t2 = rowMeans(select(., inf_sha_att_01_t2 : inf_sha_att_11_t2), na.rm = TRUE),
         inf_sha_att_m_t3 = rowMeans(select(., inf_sha_att_01_t3 : inf_sha_att_11_t3), na.rm = TRUE),
         inf_sha_att_m_t4 = rowMeans(select(., inf_sha_att_01_t4 : inf_sha_att_11_t4), na.rm = TRUE),
         inf_sha_att_m_t5 = rowMeans(select(., inf_sha_att_01_t5 : inf_sha_att_11_t5), na.rm = TRUE),
         inf_sha_beh_m_b = rowMeans(select(., contains("inf_sha_beh_01")), na.rm = TRUE),
         inf_sha_beh_m_t1 = rowMeans(select(., inf_sha_beh_01_t1 : inf_sha_beh_11_t1), na.rm = TRUE),
         inf_sha_beh_m_t2 = rowMeans(select(., inf_sha_beh_01_t2 : inf_sha_beh_11_t2), na.rm = TRUE),
         inf_sha_beh_m_t3 = rowMeans(select(., inf_sha_beh_01_t3 : inf_sha_beh_11_t3), na.rm = TRUE),
         inf_sha_beh_m_t4 = rowMeans(select(., inf_sha_beh_01_t4 : inf_sha_beh_11_t4), na.rm = TRUE),
         inf_sha_beh_m_t5 = rowMeans(select(., inf_sha_beh_01_t5 : inf_sha_beh_11_t5), na.rm = TRUE)
         )

# drop unnecessary vars
d_wide %<>%
  select(-contains("sex_"), -contains("year_birth"))

# drop participants who never used the Internet
d_wide %<>% 
  filter(use_int_s > 0)

# create data set in long format
d_long <- d_wide %>% 
  gather(key = "item", value = "value", 
         -c("id", "age", "edu", "edu_num", "male", "use_int_s", "tim_int_m", "tim_int_wee_m", "tim_int_wor_m",
            "pri_con_m_b", "inf_sha_att_m_b", "inf_sha_beh_m_b")) %>% 
  separate(item, c("item", "wave"), sep = "_t") %>% 
  spread(item, value) %>% 
  group_by(id) %>% 
  mutate_at(vars(inf_sha_beh_m, inf_sha_att_m, pri_con_m), funs(w = . - mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate_at(vars(ends_with("_b")), scale, scale = F) %>% 
  # calculate lagged vars
  mutate_at(vars(inf_sha_beh_m_w, inf_sha_att_m_w, pri_con_m_w), funs(lag = lag(., 1))) %>% 
  mutate_at(vars(contains("_m")), funs(std = scale))

# rename to put "t" first (simply looks better)
names <- names(d_wide)
names <- gsub("(.*)_t(.)", "t\\2_\\1", names)
names(d_wide) <- names

# reorder columns
d_wide %<>% select(id, age, edu, edu_num, male, use_int_s, 
              pri_con_m_b, inf_sha_att_m_b, inf_sha_beh_m_b,
              starts_with("tim"), 
              starts_with("t1"), starts_with("t2"), starts_with("t3"), 
              starts_with("t4"), starts_with("t5"))

# filter respondents who provided less than 50% of the data per wave
d_wide %<>% 
  filter(t1_na_per < .5,
         t2_na_per < .5,
         t3_na_per < .5)

# for analyses of all data
d_wide_all <- 
  d_wide %>% 
  filter(t1_na_per < .5,
         t3_na_per < .5,
         t4_na_per < .5,
         t5_na_per < .5)

# long data frame
d_long %<>% 
  filter(na_per < .5)

# filter respondents with missing sociodemographic data
d_wide %<>%
  filter(!is.na(age),
         !is.na(male),
         !is.na(edu))

# # save data
# write_csv(d_wide, "data/data_wide.csv")
# write_csv(d_wide_all, "data/data_wide_all.csv")
# write_csv(d_long, "data/data_long.csv")

# uncomment to save as synthetic dataset
# save synthetic data
write_csv(d_wide, "data/data_synthetic_wide.csv")
write_csv(d_wide_all, "data/data_synthetic_wide_all.csv")
write_csv(d_long, "data/data_synthetic_long.csv")
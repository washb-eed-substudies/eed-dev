library(haven)
library(tidyverse)
library(here)
library(lubridate, include = c('month'))

rm(list = ls())

# exposure (eed)
k_urine <- read_csv(here('raw-data/kenya/washb-kenya-eed-urine.csv'))
k_stool <- read_csv(here('raw-data/kenya/washb-kenya-eed-stool.csv')) 


k_stool <- k_stool %>% 
  select(childid, hhid, clusterid,
         setdiff(colnames(k_stool), colnames(k_urine)),
         starts_with("father")) 

k_eed <- k_urine %>% 
  full_join(k_stool, 
            by = 'childid') %>% 
  select(childid, Mann1, Lact1, Mann2, Lact2, Mann3, Lact3, tr, aged1, aged2, 
         aat1, aat2, mpo1, mpo2, neo1, neo2, stool_bl_date, stool_ml_date, 
         urine_bl_date, urine_ml_date) %>% 
  mutate(month_st1 = month(parse_date(stool_bl_date, format = '%m/%d/%y')), 
         month_ut1 = month(parse_date(urine_bl_date, format = '%m/%d/%y')), 
         month_st2 = month(parse_date(stool_ml_date, format = '%m/%d/%y')), 
         month_ut2 = month(parse_date(urine_ml_date, format = '%m/%d/%y')), 
         ln_Lact1 = log(Lact1), ln_Lact2 = log(Lact2),  ln_Lact3 = log(Lact3), 
         ln_Mann1 = log(Mann1), ln_Mann2 = log(Mann2), ln_Mann3 = log(Mann3),
         ln_aat1 = log(aat1), ln_aat2 = log(aat2),
         ln_mpo1 = log(mpo1), ln_mpo2 = log(mpo2), 
         ln_neo1 = log(neo1), ln_neo2 = log(neo2))

# outcomes
# year 1
k_mm <- read_dta(here('raw-data/kenya/washk_motormile_CA_20171121.dta')) %>% 
  select(-c('sex', 'roof', 'Ncomp', 'floor')) %>% 
  rename(month_mm = month) %>% 
  mutate(aged_mm = childage_devmm * 30.42) # avg number of days per month
  
# pivot and group by to sum group

k_who_total <- k_mm %>%   
  pivot_longer(cols = ends_with('supp')) %>% 
  group_by(childid) %>% 
  summarize(who_sum_total = sum(value, na.rm = TRUE))
  

k_who_sub <- k_mm %>%   
  pivot_longer(cols = ends_with('supp')) %>% 
  group_by(childid) %>% 
  # filter for 2, 4, 5, 6 and sum
  filter(name %in% c('stand_supp', 'walk_supp',
                     'stand_nosupp', 'walk_nosupp')) %>% 
  summarize(who_sub_total = sum(value, na.rm = TRUE)) 

k_mm <- k_who_sub %>% 
  full_join(k_who_total, 
            by = 'childid') %>% 
  right_join(k_mm, 
             by = 'childid') %>% 
  select(childid, month_mm,
         agedays_motor = aged_mm,
         who_sum_total, who_sub_total,
         ends_with('supp'), 
         nulliparous) # not in other datasets

labelled::var_label(k_mm$agedays_motor) <- 'childage at motor milestone, days'

# year 2
k_easq <- read_dta(here('raw-data/kenya/kenya_easq_no4_14april2021.dta'), 
                   col_select = c(childid, childage_dev, matches("z_.*_no4_activec", perl = TRUE), -starts_with("z_cat"))) %>% 
  left_join(read_dta(here('raw-data/kenya/washk_development_allkids_CA_20171121.dta'),
                     col_select = c(childid, month)) %>% 
              rename(month_easq = month), 
            by = "childid")
  

# ----
# covariates

k_cov <- read_csv(here('raw-data/kenya/washb-kenya-enrol.csv')) %>% 
  rename(tr_enrol = tr) %>% 
  left_join(k_stool %>% 
              select(childid, hhid, 
                     starts_with("father")))

k_anthro <- read_csv(here('raw-data/kenya/kenya-dm-ee-anthro-ee.csv')) %>% 
  select(childid, laz_t1, waz_t1, 
         laz_t2, waz_t2)

k_diarr <- read_csv("raw-data/bangladesh/bg_diarrhea.csv") %>% 
  select(time, childid, diarr7) %>% 
  pivot_wider(id_cols = childid, 
              names_from = time, 
              names_prefix = "diarr7_t",
              values_from = diarr7)
  

# surveys

source(here('raw-data/kenya/k_phq_pss_with_quant.R'))

# ----
# wealth index 

k_hhwealth <- read_csv(here('raw-data/kenya/kenya-wealth-index.csv')) %>% 
  select(childid, hhid, clusterid, hh_index = HHwealth.PC1)

# ----

k_full <- k_eed %>% 
  left_join(k_mm,
            by = 'childid', 
            suffix = c('', '_dup')) %>% 
  left_join(k_easq, 
            by = 'childid', 
            suffix = c('', '_dup')) %>% 
  left_join(k_cov, 
            by = 'childid', 
            suffix = c('', '_dup')) %>% 
  left_join(k_anthro, 
            by = 'childid', 
            suffix = c('', '_dup')) %>% 
  left_join(k_phq_pss_final, 
            by = 'childid', 
            suffix = c('', '_dup')) %>% 
  left_join(k_hhwealth, 
            by = 'childid', 
            suffix = c('', '_dup')) %>% 
  select(-ends_with('_dup')) %>% 
  left_join(k_diarr, 
            by = "childid")


# actually clean data to match SAP categories 

k_full <- k_full %>% 
  mutate(sex = case_when(sex == 'Male' ~ 'male', 
                         sex == 0 ~ 'female', 
                         is.na(sex) ~ 'missing',
                         TRUE ~ '999'), 
         birthord = case_when(nulliparous == 1 ~ 'first', # never given birth before -> first born 
                              nulliparous == 0 ~ 'second or greater', 
                              is.na(nulliparous) ~ 'missing'), # has given birth before -> second or greater
         HHS = case_when(between(HHS, 1, 3) ~ as.character(HHS), 
                         TRUE ~ 'missing'),
         momheight_raw = momheight, 
         momheight = scale(momheight, center = TRUE, scale = TRUE),
         momheight = cut(momheight, 
                         c(min(momheight, na.rm = T), -2, -1, 0, 1, 2, max(momheight, na.rm = T)),
                         right = FALSE,
                         include.lowest = TRUE))

# create factor variables

factor.vars = c('sex', 'birthord', 'momedu', 'momheight', 'HHS', 'floor', 'roof')

k_full <- k_full %>% 
  mutate(roof = replace(roof, 9, "missing"),
         floor = replace(floor, 9, "missing"),
         across(.cols = all_of(factor.vars), 
                .fns = ~ as.factor(replace_na(as.character(.), 'missing'))),
         tr = as.factor(tr)) %>% 
  rename(water_time = dminwat)
  


saveRDS(k_full, here('final-data/eed-dev_k.RDS'))

# ----
# check EED subset

all_eed_kids <- read_csv(here('raw-data/kenya/washb-kenya-allkids-ee-med-age-no-dob.csv'))

setdiff(k_full$childid, all_eed_kids$childid)
# setdiff(all_eed_kids$childid, k_full$childid)

setdiff(k_stool$childid, all_eed_kids$childid)
setdiff(k_urine$childid, all_eed_kids$childid)









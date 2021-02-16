library(haven)
library(tidyverse)
library(lubridate, include = c('month'))


`%notin%` = Negate(dplyr::`%>%`)

# month_at# - month of measurement variables for the anthropometry
# 
# ageday_st1 = age at time of stool measurement @ age 3 months
# monsoon_st1 = monsoon season at time of stool measurement @ age 3 months
# 
# ageday_at1 = age at time of anthropometry measurement @ age 3 months
# monsoon_at1 = monsoon season at time of anthropometry measurement @ age 3 months
# 
# ageday_ut1 = age at time of urine measurement @ age 3 months
# monsoon_ut1 = monsoon season at time of urine measurement @ age 3 months

# exposure (eed)
k_urine <- read_csv('./kenya/washb-kenya-eed-urine.csv')
k_stool <- read_csv('./kenya/washb-kenya-eed-stool.csv') 


k_stool <- k_stool %>% 
  select(childid, hhid, clusterid,
         setdiff(colnames(k_stool), colnames(k_urine)))

k_eed <- k_urine %>% 
  full_join(k_stool, 
            by = 'childid') %>% 
  select(childid, Mann1, Lact1, Mann2, Lact2, Mann3, Lact3, tr, aged1, aged2, 
         aat1, aat2, mpo1, mpo2, neo1, neo2, stool_bl_date, stool_ml_date, 
         urine_bl_date, urine_ml_date) %>% 
  mutate(month_st1 = month(parse_date(stool_bl_date, format = '%m/%d/%y')), 
         month_ut1 = month(parse_date(urine_bl_date, format = '%m/%d/%y')), 
         month_st2 = month(parse_date(stool_ml_date, format = '%m/%d/%y')), 
         month_ut2 = month(parse_date(urine_ml_date, format = '%m/%d/%y')))

# outcomes
# year 1
k_mm <- read_dta('./kenya/washk_motormile_CA_20171121.dta') %>% 
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
         who_sum_total, who_sub_total)

labelled::var_label(k_mm$agedays_motor) <- 'childage at motor milestone, days'

# year 2
k_easq <- read_dta('./kenya/washk_development_allkids_CA_20171121.dta') %>% 
  select(-c('sex', 'roof', 'Ncomp', 'floor')) %>% 
  rename(month_easq = month) 


# covariates

k_cov <- read_csv('kenya/washb-kenya-enrol.csv')

k_anthro <- read_csv('kenya/kenya-dm-ee-anthro-ee.csv') %>% 
  select(childid, laz_t1, waz_t1, 
         laz_t2, waz_t2)

k_phq_pss <- read_dta('kenya/washk_phq_pss_20210203.dta')


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
  select(-ends_with('_dup'))



















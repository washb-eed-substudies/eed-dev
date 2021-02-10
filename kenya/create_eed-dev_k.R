library(haven)
library(tidyverse)


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
         colnames(k_stool)[!colnames(k_stool) %in% colnames(k_urine)])

k_eed <- k_urine %>% 
  full_join(k_stool) 

# outcomes
# year 1
k_mm <- read_dta('./kenya/washk_motormile_CA_20171121.dta') %>% 
  select(-c('sex', 'roof', 'Ncomp', 'floor')) %>% 
  rename(month_mm = month) %>% 
  mutate(aged_mm = childage_devmm * 30.42) # avg number of days per month

labelled::var_label(k_mm$aged_mm) <- 'childage at motor milestone, days'

# year 2
k_easq <- read_dta('./kenya/washk_development_allkids_CA_20171121.dta') %>% 
  select(-c('sex', 'roof', 'Ncomp', 'floor')) %>% 
  rename(month_easq = month) 


# covariates

k_cov <- read_csv('kenya/washb-kenya-enrol.csv')


# year 1 dataset
k_y1 <- k_eed %>% 
  left_join(k_mm,
            by = c('childid', 'hhid',
                   'clusterid'))

k_y2 <- k_eed %>% 
  left_join(k_easq, 
            by = c('childid', 'hhid',
                   'clusterid'))



















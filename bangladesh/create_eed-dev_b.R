library(haven)
library(tidyverse)
library(lubridate)

rm(list = ls())

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

# exposures (eed)
bg_eed <- read_csv('./bangladesh/bangladesh-dm-ee-ee-growth-stool-urine-lab-covariates-anthro.csv') %>% 
  mutate(dataid = as.numeric(dataid),
         clusterid = as.numeric(clusterid),
         childid = as.numeric(childid),
         svydate_eed = parse_date(svydate, format = '%d%b%Y')) %>% 
  select(-starts_with('monsoon'),
         -contains('velocity'))


# outcomes
# year 1

# WHO motor milestones
# aggregate scores (total and subset)
bg_mm <- read_dta('./bangladesh/washb-bangladesh-motormile-year1.dta') %>%
  mutate(
    dataid = as.numeric(dataid),
    tchild = as.numeric(tchild),
    childid = as.numeric(paste0(dataid, tchild)),
    clusterid = as.numeric(clusterid)
  )  %>% 
  # pivot and group by to sum group
  pivot_longer(cols = ends_with('supp')) %>% 
  group_by(childid) %>% 
  mutate(who_sum_total = sum(value, na.rm = TRUE)) %>% 
  # filter for 2, 4, 5, 6 and sum
  filter(name %in% c('stand_supp', 'walk_supp',
                      'stand_nosupp', 'walk_nosupp')) %>% 
  mutate(who_sub_total = sum(value, na.rm = TRUE)) %>% 
  select(childid, dataid, # agedays_motor from bg_dev
         month_mm = month,
         agedays_motor = agedays,
         who_sum_total, who_sub_total) %>% 
  slice(1) 

# label to overwrite existing
labelled::var_label(bg_mm$who_sum_total) <- 'WHO motor milestones, total'
labelled::var_label(bg_mm$who_sub_total) <- 'WHO motor milestones,  sum of 2, 4, 5, 6'

# cdi1 
bg_cdi2 <- read_dta('./bangladesh/washb_cdiyr1_std_17jan2021.dta', 
                    col_select = c(tchild, 
                                   ends_with('id'), 
                                   ends_with('_no4'),
                                   agedays, month) ) %>%
  mutate(
    dataid = as.numeric(dataid),
    tchild = as.numeric(tchild),
    childid = as.numeric(paste0(dataid, tchild)),
    clusterid = as.numeric(clusterid)
  ) %>% 
  select(-c(dataid, tchild, clusterid)) %>% 
  rename(month_cdi2 = month,
         agedays_cdi2 = agedays)

# year 2

# # from sophia 
# # CDI Y2 and EASQ 
# # Z scores
bg_dev <- readRDS('bangladesh/bangladesh-development.RDS') %>%
  select(childid, starts_with('fci'))

# from helen
# cdi3, easq

bg_cdi3 <- read_dta('bangladesh/washb_cdiyr2_std_17jan2021.dta', 
                    col_select = c(tchild, 
                                   ends_with('id'), 
                                   ends_with('_no4'),
                                   agedays, month) ) %>%
  mutate(
    dataid = as.numeric(dataid),
    tchild = as.numeric(tchild),
    childid = as.numeric(paste0(dataid, tchild)),
    clusterid = as.numeric(clusterid)
  ) %>% 
  select(-c(dataid, tchild, clusterid),
         agedays_cdi3 = agedays,
         month_cdi3 = month)

bg_easq <- read_dta('bangladesh/washb_easq_std_22dec2020.dta', 
                    col_select = c(tchild, 
                                   ends_with('id'), 
                                   starts_with('z_'), 
                                   agedays, 
                                   month)) %>%
  mutate(
    dataid = as.numeric(dataid),
    tchild = as.numeric(tchild),
    childid = as.numeric(paste0(dataid, tchild)),
    clusterid = as.numeric(clusterid)
  ) %>% 
  select(-c(dataid, tchild, clusterid)) %>% 
  rename(agedays_easq = agedays,
         month_easq = month)

bg_dev <- bg_dev %>% 
  left_join(bg_cdi3, 
            by = 'childid') %>% 
  left_join(bg_easq, 
            by = 'childid')


# bg_cdi2 <- read_dta('./bangladesh/washb-bangladesh-cdi-year2.dta') %>%
#   mutate(
#     dataid = as.numeric(dataid),
#     tchild = as.numeric(tchild),
#     clusterid = as.numeric(clusterid),
#     childid = as.numeric(paste0(dataid,tchild))
#   ) %>%
#   select(childid, clusterid, month_cdi2 = month,
#          arm, svydate, agedays_cdi2 = agedays,
#          tchild, ageyears,
#          starts_with('U', ignore.case = FALSE),
#          starts_with('S', ignore.case = FALSE))
# 
# bg_easq <- read_dta('./bangladesh/washb-bangladesh-easq-year2.dta') %>%
#   mutate(
#     dataid = as.numeric(dataid),
#     tchild = as.numeric(tchild),
#     clusterid = as.numeric(clusterid),
#     childid = as.numeric(paste0(dataid,tchild))
#   ) %>%
#   select(childid, tchild, clusterid, month_easq = month,
#          arm, agedays_easq = agedays, ageyears,
#          starts_with('S', ignore.case = FALSE))

bg_hhwealth <- read_csv('bangladesh/bangladesh-hhwealth.csv')

# ------------------------
# full dataset
## [ ] still need cdi1
bg_dev_full <- bg_eed %>% 
  left_join(bg_mm, 
            by = c('childid', 
                   'dataid')) %>% 
  left_join(bg_cdi2, 
            by = 'childid') %>% 
  left_join(bg_dev, 
            by = 'childid') %>% 
  left_join(bg_hhwealth, 
            by = 'dataid')

# ------------------------

bg_dev_full <- bg_dev_full %>%
  mutate(birthord = case_when(birthord == 1 ~ 'first', 
                              birthord >= 2 ~ 'second or greater', 
                              TRUE ~ 'missing'),
         momheight = scale(momheight, center = TRUE, scale = TRUE),
         momheight = cut(momheight, 
                         c(min(momheight, na.rm = T), -2, -1, 0, 1, 2, max(momheight, na.rm = T)),
                         right = FALSE,
                         include.lowest = TRUE)
         ) 

# create factor variables

factor.vars = c('sex', 'birthord', 'momedu', 'momheight', 'hfiacat', 'floor', 
                'walls', 'roof', 'cesd_sum_t2', "cesd_sum_ee_t3", "pss_sum_mom_t3", 'life_viol_any_t3')

bg_dev_full <- bg_dev_full %>% 
  mutate(across(.cols = all_of(factor.vars), 
                .fns = ~ as.factor(replace_na(as.character(.), 'missing')))) 


saveRDS(bg_dev_full, 'eed-dev_bg.RDS')







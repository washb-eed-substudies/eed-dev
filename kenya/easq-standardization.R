library(haven)
library(tidyverse)


easq <- read_dta('kenya/washk_dev_20210323.dta')
enrol <- read_csv('kenya/washb-kenya-enrol.csv') %>% 
  select(clusterid, hhid, childid, block, tr)
tr <- read_csv('kenya/washb-kenya-tr.csv')

k_easq_tr <- easq %>% 
  mutate(clusterid = as.numeric(clusterid)) %>% 
  left_join(tr, 
            'clusterid') %>% 
  mutate(activeC = case_when(str_starts('Control', tr) ~ 1))

k_easq_tr %>% 
  count(tr)

write_csv(k_easq_tr, 'easq_raw_tr.csv')

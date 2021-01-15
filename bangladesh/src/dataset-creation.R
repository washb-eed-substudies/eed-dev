rm(list=ls())
source(here::here("0-config.R"))

# load telo-growth data
d <- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.csv"))
names(d)

# load in child development datasets
setwd("C:/Users/Sophia/Documents/WASH/WASHB CD data from Kishor/2-child-development-outcomes-datasets")

# create childid
get_childid <- function(v1, v2){
  as.numeric(paste(as.character(v1), as.character(v2), sep=""))
}

## missing cdiy1

cdiy2 <- read_dta("washb_cdi_std_22dec2020.dta") 
names(cdiy2)
cdiy2_select <- cdiy2 %>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, agedays, month, z_age2mo_cdi_und_all_no4, z_age2mo_cdi_say_all_no4) %>%
  rename(agedays_cdi_t3 = agedays,
         month_cdi_t3 = month,
         z_cdi_und_t3 = z_age2mo_cdi_und_all_no4,
         z_cdi_say_t3 = z_age2mo_cdi_say_all_no4)

easq <- read_dta("washb_easq_std_22dec2020.dta") 
names(easq)
easq_select <- easq %>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, agedays, month, z_age2mo_personal_no4,  z_age2mo_motor_no4, 
         z_age2mo_com_no4, z_age2mo_combined_no4) %>%
  rename(agedays_easq = agedays,
         month_easq = month,
         z_personal_easq = z_age2mo_personal_no4,
         z_motor_easq = z_age2mo_motor_no4,
         z_comm_easq = z_age2mo_com_no4,
         z_combined_easq = z_age2mo_combined_no4)

home1 <- read.dta("washb-bangladesh-home-year1.dta")%>%
  mutate(childid = substr(childid, 2, 2)) %>%
  mutate(childid = get_childid(dataid, childid)) %>%
  select(childid, midline_stimulation) %>%
  rename(fci_t2 = midline_stimulation)

home2 <- read.dta("washb-bangladesh-home-year2.dta")%>%
  mutate(childid = substr(childid, 2, 2)) %>%
  mutate(childid = get_childid(dataid, childid)) %>%
  select(childid, endline_stimulation) %>%
  rename(fci_t3 = endline_stimulation)

motor <- read.csv("washb-bangladesh-motormile-year1.csv")%>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, agedays, month, sit_nosupp, crawl_nosupp, stand_supp, 
         walk_supp, stand_nosupp, walk_nosupp) %>%
  rename(agedays_motor = agedays,
         month_motor = month,
         who_sit = sit_nosupp, who_crawl = crawl_nosupp, 
         who_stand_supp = stand_supp, who_walk_supp = walk_supp, 
         who_stand_nosupp = stand_nosupp, who_walk_nosup = walk_nosupp)

# join separate development datasets 
development <- motor %>% full_join(cdiy2_select, by= 'childid') %>% full_join(easq_select, by = 'childid')

dev_with_fci <- development %>% left_join(home1, 'childid') %>%
  left_join(home2, 'childid') 

# save development dataset
saveRDS(dev_with_fci, paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-development.RDS"))

# join with telo-covariates dataset
telo_dev <- left_join(d, dev_with_fci, "childid")

# Z-score of telomere measurements
telo_dev <- telo_dev %>% 
  mutate(TS_t2_Z = scale(TS_t2, center=TRUE, scale=TRUE)[,1]) %>%
  mutate(TS_t3_Z = scale(TS_t3, center=TRUE, scale=TRUE)[,1]) %>%
  mutate(delta_TS_Z = scale(delta_TS, center=TRUE, scale=TRUE)[,1])

# merge in hhwealth 
d_hhwealth <- read.csv("C:/Users/Sophia/Documents/ee-secondary/sophia scripts/hhwealth.csv")
telo_dev <- left_join(telo_dev, d_hhwealth, "dataid")

saveRDS(telo_dev, paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-ee-telo-development-covariates.RDS"))

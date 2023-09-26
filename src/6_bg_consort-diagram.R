rm(list=ls())
source(here::here("0-config.R"))
library(tibble)
library(glue)
data <- tibble(x = -10:100, y= -10:100)
head(data)

d <- readRDS(here('final-data/eed-dev_bg.RDS'))


exposures_y1 <- c("ln_aat1", 
                  "ln_mpo1", 
                  "ln_neo1", 
                  "ln_L_conc_t1", 
                  "ln_M_conc_t1")
outcomes_y1 <- c("who_sum_total", "who_sub_total",
                 "z_age2mo_cdi_undyr1_all_no4", "z_age2mo_cdi_sayyr1_all_no4",
                 "z_age2mo_personal_no4", "z_age2mo_motor_no4", 
                 "z_age2mo_combined_no4", "z_age2mo_com_no4")
exposures_y2 <- c("ln_aat2", 
                  "ln_mpo2", 
                  "ln_neo2", 
                  "ln_L_conc_t2", 
                  "ln_M_conc_t2", 
                  "ln_reg2")
outcomes_y2 <- c("z_age2mo_cdi_undyr2_all_no4", 
                 "z_age2mo_cdi_sayyr2_all_no4")

# get numbers for analysis box

n_exp_outcome <- d %>% 
  mutate(has_exp_t1 = if_any(all_of(exposures_y1), 
                             ~ !is.na(.)),
         has_exp_t2 = if_any(all_of(exposures_y2), 
                             ~ !is.na(.)),
         has_outcome_t1 = if_any(all_of(outcomes_y1), 
                                 ~ !is.na(.)),
         has_outcome_t2 = if_any(all_of(outcomes_y2), 
                             ~ !is.na(.)),
         
         has_all_t1 = has_exp_t1 & has_outcome_t1,
         has_all_t2 = has_exp_t2 & has_outcome_t2,
         
         has_t1_t2 = has_all_t1 & has_all_t2
         ) 

n_children <- n_exp_outcome %>% 
  summarize(across(c(has_all_t1, has_all_t2, has_t1_t2),
                   sum))

n_clusters <- c("has_all_t1", "has_all_t2") %>% 
  map(function(col_name){
    n_exp_outcome %>% 
      filter(!!ensym(col_name)) %>% 
      summarize(!!paste0("n_cluster_", col_name) := n_distinct(clusterid))
  }) %>% 
  list_cbind()

n_children; n_clusters

# create diagram ----
# from https://docs.google.com/spreadsheets/d/1QUadSAPBwV8mM3fs_NZ-PQjXX22BXBF1jVpwySshb1s/edit#gid=1841173490
raw_consort <- read_csv("tables/bangaldesh_consort_raw.csv") %>% 
  filter(!if_all(everything(), is.na))

raw_consort <- raw_consort %>% 
  group_by(stage, timepoint) %>% 
  summarize(across(where(is.numeric),
                   sum)) %>% 
  ungroup()

pluck_count <- function(stage_val, t, desc){
  
  if(stage_val == "allocation"){
    n <- raw_consort %>% 
      filter(stage == stage_val) %>% 
      pluck(desc)
  } else {
    n <- raw_consort %>% 
      filter(stage == stage_val, 
             timepoint == t) %>% 
      pluck(desc)
  }
  
  n <- scales::comma(n)

  
  assertthat::are_equal(length(n), 1)
  
  return(n)
}

# create labels
label_df <- tribble(
  ~ stage, ~ label, 
  "Enrollment",      "13,279 compounds assessed for eligibility",
  "Allocation_all",      "720 clusters created and randomly allocated across 7 arms<br>
                      5,551 compounds randomly allocated across 7 arms<br>
                      4 of 7 arms selected into substudy",
  "Allocation_substudy", glue("**Control arm, Nutrition arm,<br>
                           Water + Sanitation + Handwashing arm, and <br>
                           Nutrition + Water + Sanitation + Handwashing arm**<br><br>
                           {pluck_count('allocation', NA, 'n_cluster')} clusters<br>
                           {pluck_count('allocation', NA, 'n_households')} households"),
  "Subsample Target", glue("**Month 14**<br>
                           {pluck_count('subsample target', 14, 'n_cluster')} clusters<br>
                           {pluck_count('subsample target', 14, 'n_children')} children<br>
                           **Month 28**<br>
                           {pluck_count('subsample target', 28, 'n_cluster')} clusters<br>
                           {pluck_count('subsample target', 28, 'n_children')} children<br>"),
  "Follow-up",         glue("**Month 14**<br>
                           {pluck_count('follow-up', 14, 'ltf')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 14, 'moved')} moved<br>
                           {pluck_count('follow-up', 14, 'absent')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 14, 'withdrew')} moved<br>
                           {pluck_count('follow-up', 14, 'no_live_birth')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 14, 'child_death')} child death<br>
                           **Month 28**<br>
                           {pluck_count('follow-up', 28, 'new_measured')}  new children measured<br>
                           {pluck_count('follow-up', 28, 'ltf')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 28, 'moved')} moved<br>
                           {pluck_count('follow-up', 28, 'absent')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 28, 'withdrew')} moved<br>
                           {pluck_count('follow-up', 28, 'no_live_birth')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 28, 'child_death')} child death<br>"),
  "Subsample Enrollment", glue("**Month 14**<br>
                           {pluck_count('subsample enrollment', 14, 'n_cluster')} clusters<br>
                           {pluck_count('subsample enrollment', 14, 'n_children')} children<br>
                           **Month 28**<br>
                           {pluck_count('subsample enrollment', 28, 'n_cluster')} clusters<br>
                           {pluck_count('subsample enrollment', 28, 'n_children')} children<br>"),
  "Specimen Collection",  glue("**Month 14**<br>
                           {pluck_count('specimen collection', 14, 'missing_stool')} missing stool<br>
                           {pluck_count('specimen collection', 14, 'missing_urine')} missing urine<br>
                           **Month 28**<br>
                           {pluck_count('specimen collection', 28, 'missing_stool')} missing stool<br>
                           {pluck_count('specimen collection', 28, 'missing_urine')} missing urine<br>"),
  "Analysis", glue("**Month 14**<br>
                           {pluck_count('analysis', 14, 'n_cluster')} clusters<br>
                           {pluck_count('analysis', 14, 'n_children')} children<br>
                           **Month 28**<br>
                           {pluck_count('analysis', 28, 'n_cluster')} clusters<br>
                           {pluck_count('analysis', 28, 'n_children')} children<br>"),
) 

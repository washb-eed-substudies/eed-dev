rm(list=ls())
source(here::here("0-config.R"))
library(tibble)
data <- tibble(x = -10:100, y= -10:100)
head(data)

d <- readRDS(here('final-data/eed-dev_k.RDS'))

exposures_y1 <- c("ln_aat1", 
                  "ln_mpo1", 
                  "ln_neo1", 
                  "ln_Lact1", 
                  "ln_Mann1")
outcomes_y1 <- c("who_sum_total", "who_sub_total")
exposures_y2 <- c("ln_aat2", 
                  "ln_mpo2", 
                  "ln_neo2", 
                  "ln_Lact2", 
                  "ln_Mann2")
outcomes_y2 <- c("z_comtot_no4_activec", "z_mottot_no4_activec", 
                 "z_pstot_no4_activec", "z_globaltot_no4_activec")

d <- d %>% 
  select(childid, clusterid, all_of(exposures_y1), 
         all_of(exposures_y2), all_of(outcomes_y1),
         all_of(outcomes_y2)) %>% 
  mutate(has_exp_t1 = if_any(all_of(exposures_y1),
                             ~ !is.na(.)),
         has_exp_t2 = if_any(all_of(exposures_y2),
                             ~ !is.na(.)),
         has_outcome_t1 = if_any(all_of(outcomes_y1),
                             ~ !is.na(.)),
         has_outcome_t2 = if_any(all_of(outcomes_y2),
                                 ~ !is.na(.))) %>% 
  mutate(has_all_t1 = has_exp_t1 & has_outcome_t1,
         has_all_t2 = has_exp_t2 & has_outcome_t2,
         has_t1_t2 = has_all_t1 & has_all_t2) 

n_children <- d %>% 
  summarize(across(c(has_all_t1, has_all_t2, has_t1_t2),
                   sum))

n_clusters <- c("has_t1_t2", "has_all_t2") %>% 
  map(function(col_name){
    d %>% 
      filter(!!ensym(col_name)) %>% 
      summarize(!!paste0("n_cluster_", col_name) := n_distinct(clusterid))
  }) %>% 
  list_cbind()

n_children; n_clusters

# sum numbers across arms

# from https://docs.google.com/spreadsheets/d/1QUadSAPBwV8mM3fs_NZ-PQjXX22BXBF1jVpwySshb1s/edit#gid=1841173490
raw_consort <- read_csv("tables/kenya_consort_raw.csv") %>% 
  filter(!if_all(everything(), is.na))

raw_consort <- raw_consort %>% 
  group_by(stage, timepoint) %>% 
  summarize(across(where(is.numeric),
                   sum)) %>% 
  ungroup()

pluck_count <- function(stage_val, t, desc){
  
  if(stage_val %in% c("allocation", "subsample target")){
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

# create box contents

label_df <- tribble(
  ~ stage, ~ label, 
  "attendance",      "2,569 villages assessed for eligibility",
  "Allocation_all",      "702 clusters created and randomly allocated across 7 arms<br>
                      8,246 households randomly allocated across 8 arms<br>
                      4 of 8 arms selected into substudy",
  "Allocation_substudy", glue("**Control arm, Nutrition arm,<br>
                           Water + Sanitation + Handwashing arm, and <br>
                           Nutrition + Water + Sanitation + Handwashing arm**<br><br>
                           {pluck_count('allocation', NA, 'n_cluster')} clusters<br>
                           {pluck_count('allocation', NA, 'n_households')} households"),
  "Subsample Target", glue("Targeted<br>
                           {pluck_count('subsample target', NA, 'n_cluster')} clusters<br>
                           {pluck_count('subsample target', NA, 'n_households')} households<br>"),
  "Follow-up",         glue("**Month 17**<br>
                           {pluck_count('follow-up', 17, 'ltf')} children lost to follow-up<br>
                           {pluck_count('follow-up', 17, 'absent')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 17, 'no_live_birth')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 17, 'child_death')} child death<br>
                           **Month 22**<br>
                           {pluck_count('follow-up', 22, 'ltf')} children lost to follow-up<br>
                           {pluck_count('follow-up', 22, 'absent')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 22, 'no_live_birth')}  children lost to follow-up<br>
                           {pluck_count('follow-up', 22, 'child_death')} child death<br>"),
  "Subsample Attendance", glue("**Month 17**<br>
                           {pluck_count('subsample attendance', 17, 'n_cluster')} clusters<br>
                           {pluck_count('subsample attendance', 17, 'n_children')} children<br>
                           **Month 22**<br>
                           {pluck_count('subsample attendance', 22, 'n_cluster')} clusters<br>
                           {pluck_count('subsample attendance', 22, 'n_children')} children<br>"),
  "Analysis", glue("**Month 17**<br>
                           {pluck_count('analysis', 17, 'n_cluster')} clusters<br>
                           {pluck_count('analysis', 17, 'n_children')} children<br>
                           **Month 22**<br>
                           {pluck_count('analysis', 22, 'n_cluster')} clusters<br>
                           {pluck_count('analysis', 22, 'n_children')} children<br>"),
)


label_df %>% 
  pull(label)


library(haven)
library(readr)
library(dplyr)
library(ggplot2)
library(here)
library(stringr)
library(boxr)
library(officedown)
library(officer)

source(here::here("0-config.R"))


# load data

## bangladesh 

bg <- read_rds(here("final-data/eed-dev_bg.RDS"))

#clean covariates to avoid high missingness
bg <- bg %>% mutate(
  laz_t1 = factor(case_when(is.na(laz_t1) ~ "Missing",
                            laz_t1 < -2 ~ "Stunted",
                            laz_t1 >= (-2) ~ "Not stunted")),
  waz_t1 = factor(case_when(is.na(waz_t1) ~ "Missing",
                            waz_t1 < -2 ~ "Wasted",
                            waz_t1 >= (-2) ~ "Not wasted")),
  cesd_sum_t2=as.numeric(as.character(cesd_sum_t2)),
  cesd_sum_t2 = factor(case_when(is.na(cesd_sum_t2) ~ "Missing",
                                 cesd_sum_t2 < 16 ~ "Not depressed",
                                 cesd_sum_t2 >= 16 ~ "Depressed")))

bf_bl <- read_dta(here("raw-data/bangladesh/ffq/EE_Baseline_FFQ_Raw_data_13Sep2017_deidentified.dta"), 
                  col_select = c(contains("id"), starts_with("child"),
                                 c607, c607a, 
                                 starts_with("c608"),
                                 starts_with("c609")))
bf_ml <- read_dta(here("raw-data/bangladesh/ffq/EE_Midline_FFQ_Raw_data_28Sep2017_deidentified.dta"), 
                  col_select = c(contains("id"), starts_with("child"),
                                 c607, c607a, 
                                 starts_with("c608"),
                                 starts_with("c609")))

# kenya 

k_ffq <- read_dta(here("raw-data/kenya/washk_excbf_20180531_share.dta")) %>% 
  select(childid, contains("excbf"))

k_eed <- read_rds(here("final-data/eed-dev_k.RDS"))





ebf_bl <- bf_bl %>% 
  rowwise() %>% 
  mutate(childid = as.numeric(paste0(dataid, childId)), 
         ebf_bl = all(c607>0, c607a==5, c608_1_3==2, c608_2_3==2, 
                      c608_3_3==2, c608_4_3==2, c608_5_3==2, 
                      c608_6_3==2, c608_7_3==2, c608_8_3==2, 
                      c608_9_3==2, c608_10_3==2, c608_11_3==2, 
                      c608_12_3==2, c609_1_1==2, c609_2_1==2,
                      c609_2_2==2, c609_2_3==2, c609_2_4==2, 
                      c609_2_5==2, c609_3_1==2, c609_3_2==2, 
                      c609_3_3==2, c609_4_1==2, c609_4_2==2, 
                      c609_5_1==2, c609_5_2==2, c609_5_3==2, 
                      c609_5_4==2, c609_5_5==2, c609_6_1==2, 
                      c609_6_2==2, c609_7_1==2, c609_7_2==2, 
                      c609_7_3==2, c609_7_4==2, c609_7_5==2, 
                      c609_7_6==2, c609_7_7==2, c609_8_1==2, 
                      c609_8_2==2, c609_8_3==2, c609_8_4==2,
                      c609_8_5==2, c609_8_6==2, c609_9_1==2, 
                      c609_9_2==2, c609_9_3==2, c609_10_1==2, 
                      c609_11_1==2, c609_12_1==2, c609_12_2==2, 
                      c609_12_3==2, c609_13_1==2, c609_13_2==2, 
                      c609_13_3==2, c609_13_4==2, c609_13_5==2, 
                      c609_13_6==2, c609_13_7==2, c609_14_1==2, 
                      c609_14_2==2, c609_14_3==2, c609_15_1==2, 
                      c609_15_2==2, c609_15_3==2, c609_16_1==2, 
                      c609_16_2==2, c609_16_3==2, c609_16_4==2, 
                      c609_16_5==2, c609_17_1==2, c609_17_2==2, 
                      c609_17_3==2, c609_17_4==2, c609_18_1==2, 
                      c609_18_2==2)) %>% 
  select(contains("childid"), ebf_bl)

ebf_ml <- bf_ml %>% 
  rowwise() %>% 
  mutate(childid = as.numeric(paste0(dataid, childId)), 
         ebf_ml = all(c607>0, c607a==5, c608_1_3==2, c608_2_3==2, 
                      c608_3_3==2, c608_4_3==2, c608_5_3==2, 
                      c608_6_3==2, c608_7_3==2, c608_8_3==2, 
                      c608_9_3==2, c608_10_3==2, c608_11_3==2, 
                      c608_12_3==2, c609_1_1==2, c609_2_1==2,
                      c609_2_2==2, c609_2_3==2, c609_2_4==2, 
                      c609_2_5==2, c609_3_1==2, c609_3_2==2, 
                      c609_3_3==2, c609_4_1==2, c609_4_2==2, 
                      c609_5_1==2, c609_5_2==2, c609_5_3==2, 
                      c609_5_4==2, c609_5_5==2, c609_6_1==2, 
                      c609_6_2==2, c609_7_1==2, c609_7_2==2, 
                      c609_7_3==2, c609_7_4==2, c609_7_5==2, 
                      c609_7_6==2, c609_7_7==2, c609_8_1==2, 
                      c609_8_2==2, c609_8_3==2, c609_8_4==2,
                      c609_8_5==2, c609_8_6==2, c609_9_1==2, 
                      c609_9_2==2, c609_9_3==2, c609_10_1==2, 
                      c609_11_1==2, c609_12_1==2, c609_12_2==2, 
                      c609_12_3==2, c609_13_1==2, c609_13_2==2, 
                      c609_13_3==2, c609_13_4==2, c609_13_5==2, 
                      c609_13_6==2, c609_13_7==2, c609_14_1==2, 
                      c609_14_2==2, c609_14_3==2, c609_15_1==2, 
                      c609_15_2==2, c609_15_3==2, c609_16_1==2, 
                      c609_16_2==2, c609_16_3==2, c609_16_4==2, 
                      c609_16_5==2, c609_17_1==2, c609_17_2==2, 
                      c609_17_3==2, c609_17_4==2, c609_18_1==2, 
                      c609_18_2==2)) %>% 
  select(contains("childid"), ebf_ml)

ebf_bg <- bg %>% 
  full_join(ebf_bl, by = "childid") %>% 
  full_join(ebf_ml, by = "childid")


k_ebf <- k_ffq %>% 
  inner_join(k_eed, by = "childid") %>% 
  mutate(across(c("excbf_combined", "excbf_eeblinf", "excbf_eemlinf", "excbf_mselup", "excbf_msmlup"),
                factor))




#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", 'roof', "HHwealth", 
         "life_viol_any_t3", "tr",
         
         'laz_t1', 'waz_t1', "cesd_sum_t2")




fit_gam_model <- function(data, Xvars, Yvars, covariates, forced_cov){
  assertthat::assert_that(length(setdiff(covariates, colnames(data))) == 0,
                          msg = glue::glue("Missing: {str_flatten_comma(setdiff(covariates, colnames(data)))}"))
  
  suppressMessages({
    adj_models <- NULL
    for(i in Xvars){
      for(j in Yvars){
        print(i)
        print(j)
        res_adj = NULL
        try(res_adj <- fit_RE_gam(d=data, X=i, Y=j, W=covariates, forcedW = forced_cov))
        if(is.null(res_adj)){
          try(res_adj <- fit_RE_gam(d=data, X=i, Y=j, W=covariates, forcedW = forced_cov, vim=FALSE))
        }
        res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
        adj_models <- bind_rows(adj_models, res)
      }
    }
    
    
    #Get primary contrasts
    adj_res <- NULL
    for(i in 1:nrow(adj_models)){
      res <- data.frame(X=adj_models$X[i], Y=adj_models$Y[i])
      preds <- predict_gam_diff(fit=adj_models$fit[i][[1]], d=adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
      adj_res <-  bind_rows(adj_res , preds$res)
    }
    
    #Make list of plots
    adj_plot_list <- NULL
    adj_plot_data <- NULL
    for(i in 1:nrow(adj_models)){
      res <- data.frame(X=adj_models$X[i], Y=adj_models$Y[i])
      simul_plot <- gam_simul_CI(adj_models$fit[i][[1]], adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
      adj_plot_list[[i]] <-  simul_plot$p
      adj_plot_data <-  rbind(adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
    }
    
  })
  
  results <- list("models" = adj_models, 
                  "contrasts" = adj_res, 
                  "plot_list" = adj_plot_list,
                  "plot_data" = adj_plot_data)
  
  return(results)
}


compare_wo_forced_var <- function(df_with_forced_var, df_wo_forced_var){
  df_with_forced_var %>% 
    left_join(df_wo_forced_var,
              by = c("X", "Y"),
              suffix = c("_bf", "_nobf")) %>% 
    select(Y, X, starts_with("N", ignore.case = FALSE),
           starts_with("point.diff"), starts_with("lb.diff"), starts_with("ub.diff"),
           starts_with("Pval")) %>% 
    relocate(ends_with("_nobf"), .after = last_col()) %>% 
    mutate(change = case_when(Pval_nobf < 0.05 & Pval_bf > 0.05 ~ as.character(emo::ji("sad")), # addition is no longer sig
                              Pval_nobf < 0.05 & Pval_bf < 0.05 ~ as.character(emo::ji("happy")), # addition is still sig
                              Pval_nobf > 0.05 & Pval_bf < 0.05 ~ as.character(emo::ji("exclamation")))) # addition makes sig
}


## Hypothesis 1


H1_W <- c(Wvars)




##########################
# adjustment sets 1-3
## exposure: fecal markers
## outcome: who mm
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c('who_sum_total', 'who_sub_total')

H1a_W <- c(H1_W, 'ageday_st1', 'agedays_motor', 
           'month_st1', 'month_mm', "ebf_bl")

H1a_results <- ebf_bg %>% 
  fit_gam_model(Xvars, Yvars, H1a_W, "ebf_bl")

H1a <- compare_wo_forced_var(H1a_results$contrasts, 
                             readRDS(here("results/adjusted/H1a_adj_res.RDS")))

H1a_results$contrasts


##########################
# adjustment sets 4-5
## exposure: urine markers
## outcome: who mm
##########################

Xvars <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars <- c('who_sum_total', 'who_sub_total')

H1b_W <- c(H1_W, 'ageday_ut1', 'agedays_cdi2', 
           'month_ut1', 'month_cdi2', 'ebf_bl')


H1b_results <- ebf_bg %>% 
  fit_gam_model(Xvars, Yvars, H1b_W, "ebf_bl")

H1b <- compare_wo_forced_var(H1b_results$contrasts, 
                             readRDS(here("results/adjusted/H1b_adj_res.RDS")))




##########################
# adjustment sets 6-8
## exposure: fecal markers
## outcome: cdi2
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c('z_age2mo_cdi_undyr1_all_no4', 
           'z_age2mo_cdi_sayyr1_all_no4')

H1c_W <- c(H1_W, 'ageday_st1', 'agedays_cdi2', 
           'month_st1', 'month_cdi2', "ebf_bl")

H1c_results <- ebf_bg %>% 
  fit_gam_model(Xvars, Yvars, H1c_W, "ebf_bl")

H1c <- compare_wo_forced_var(H1c_results$contrasts, 
                             readRDS(here("results/adjusted/H1c_adj_res.RDS")))





##########################
# adjustment sets 9-10
## exposure: urine markers
## outcome: cdi2
##########################

Xvars <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars <- c('z_age2mo_cdi_undyr1_all_no4', 
           'z_age2mo_cdi_sayyr1_all_no4')

H1d_W <- c(H1_W, 'ageday_ut1', 'agedays_cdi2', 
           'month_ut1', 'month_cdi2', 'ebf_bl')

H1d_results <- ebf_bg %>% 
  fit_gam_model(Xvars, Yvars, H1d_W, "ebf_bl")

H1d <- compare_wo_forced_var(H1d_results$contrasts, 
                             readRDS(here("results/adjusted/H1d_adj_res.RDS")))


## Hypothesis 2 

#eed markers at t1/t2 v. dev (cdi3, easq) at t3


H2_W <- c(Wvars, 'laz_t2', 'waz_t2',
          'cesd_sum_ee_t3',	'pss_sum_mom_t3') %>% 
  # cesd is colinear with bf
  setdiff('cesd_sum_ee_t3')



##########################
# adjustment sets 11-13
## exposure: fecal markers t1
## outcome: easq t3
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
           "z_age2mo_combined_no4", "z_age2mo_com_no4")

H2a_W <- c(H2_W, 'ageday_st1',	'agedays_easq', 
           'month_st1',	'month_easq', 'ebf_bl')

H2a_results <- ebf_bg %>% 
  fit_gam_model(Xvars, Yvars, H2a_W, "ebf_bl")

H2a <- compare_wo_forced_var(H2a_results$contrasts, 
                             readRDS(here("results/adjusted/H2a_adj_res.RDS")))


##########################
# adjustment sets 14-15
## exposure: urine markers t1
## outcome: easq t3
##########################

Xvars <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
           "z_age2mo_combined_no4", "z_age2mo_com_no4")

H2b_W <- c(H2_W, 'ageday_ut1',	'agedays_easq', 
           'month_ut1',	'month_easq', 'ebf_bl')

H2b_results <- ebf_bg %>% 
  fit_gam_model(Xvars, Yvars, H2b_W, "ebf_bl")

H2b <- compare_wo_forced_var(H2b_results$contrasts, 
                             readRDS(here("results/adjusted/H2b_adj_res.RDS")))




##########################
# adjustment sets 16-18
## exposure: fecal markers t1
## outcome: cdi t3
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")

H2c_W <- c(H2_W, 'ageday_st1', 'agedays_cdi3',
           'month_st1', 'month_cdi3', 'ebf_bl')

H2c_results <- ebf_bg %>% 
  fit_gam_model(Xvars, Yvars, H2c_W, "ebf_bl")

H2c <- compare_wo_forced_var(H2c_results$contrasts, 
                             readRDS(here("results/adjusted/H2c_adj_res.RDS")))




##########################
# adjustment sets 19-20
## exposure: urine markers t1
## outcome: cdi t3
##########################

Xvars <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")

H2d_W <- c(H2_W, 'ageday_ut1', 'agedays_cdi3',
           'month_ut1', 'month_cdi3', 'ebf_bl')

H2d_results <- ebf_bg %>% 
  fit_gam_model(Xvars, Yvars, H2d_W, "ebf_bl")

H2d <- compare_wo_forced_var(H2d_results$contrasts, 
                             readRDS(here("results/adjusted/H2d_adj_res.RDS")))



bangladesh_bf <- list(H1a, H1b, H1c, H1d, H2a, H2b, H2c, H2d)


# Kenya


Wvars <- c("sex","birthord", "momage","momheight","momedu", 
           "HHS", "Nlt18","Ncomp", "water_time", 
           "floor", 'roof', "hh_index", 
           "tr")




H4_W <- c(Wvars,
          'laz_t1', 'waz_t1',
          'aged1',	'agedays_motor', 
          'month_mm', "excbf_eeblinf")


##########################
# adjustment sets 33-35
## exposure: fecal markers t1
## outcome: who mm t2
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c("who_sub_total", "who_sum_total")

H4a_W <- c(H4_W, 'month_st1') %>% 
  setdiff("roof")

H4a_results <- k_ebf %>% 
  fit_gam_model(Xvars, Yvars, H4a_W, "excbf_eeblinf")

H4a <- compare_wo_forced_var(H4a_results$contrasts, 
                             readRDS(here("results/adjusted/H4a_adj_res.RDS")))



##########################
# adjustment sets 36-37
## exposure: urine markers t1
## outcome: who mm t2
##########################

Xvars <- c('ln_Lact1', 'ln_Mann1')           
Yvars <- c("who_sub_total", "who_sum_total")

H4b_W <- c(H4_W, 'month_ut1')

H4b_results <- k_ebf %>% 
  fit_gam_model(Xvars, Yvars, H4b_W, "excbf_eeblinf")

H4b <- compare_wo_forced_var(H4b_results$contrasts, 
                             readRDS(here("results/adjusted/H4b_adj_res.RDS")))


H5_W <- c(Wvars,
          'laz_t1', 'waz_t1',
          'laz_t2', 'waz_t2',
          'childage_dev',	'month_easq', 
          'quantile_phq',	'quantile_pss')





##########################
# adjustment sets 38-40
## exposure: fecal markers t1
## outcome: easq t3
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c("z_comtot_no4_activec", "z_mottot_no4_activec", "z_pstot_no4_activec", "z_globaltot_no4_activec")

H5a_W <- c(H5_W, 'aged1', 'month_st1', "excbf_eeblinf")

H5a_results <- k_ebf %>% 
  fit_gam_model(Xvars, Yvars, H5a_W, "excbf_eeblinf")

H5a <- compare_wo_forced_var(H5a_results$contrasts, 
                             readRDS(here("results/adjusted/H5a_adj_res.RDS")))



##########################
# adjustment sets 41-42
## exposure: urine markers t1
## outcome: easq t3
##########################

Xvars <- c('ln_Lact1', 'ln_Mann1')           
Yvars <- c("z_comtot_no4_activec", "z_mottot_no4_activec", "z_pstot_no4_activec", "z_globaltot_no4_activec")

H5b_W <- c(H5_W, 'aged1', 'month_ut1', "excbf_eeblinf")

H5b_results <- k_ebf %>% 
  fit_gam_model(Xvars, Yvars, H5b_W, "excbf_eeblinf")

H5b <- compare_wo_forced_var(H5b_results$contrasts, 
                             readRDS(here("results/adjusted/H5b_adj_res.RDS")))




kenya_bf <- list(H4a, H4b, H5a, H5b)




# Clean Results


format_num <- function(number, digits = 2){
  number %>% 
    round(digits = digits) %>% 
    {case_when(. > 10^6 ~ "Inf",
               . < -10^6 ~ "-Inf",
               TRUE ~ format(., nsmall = digits, scientific = FALSE))}
  
}

clean_results <- function(data, hyp_num, measure = "gam"){
  
  pasted_var <- case_when(measure == "gam" ~ "Adj. Diff (95% CI)", 
                          measure == "hr" ~ "Adj. HR (95% CI)")
  
  ub <- case_when(measure == "gam" ~ "ub.diff", 
                  measure == "hr" ~ "ub.HR")
  lb <- case_when(measure == "gam" ~ "lb.diff", 
                  measure == "hr" ~ "lb.HR")
  
  point <- case_when(measure == "gam" ~ "point.diff", 
                     measure == "hr" ~ "point.HR")
  
  y_sd_name <- case_when(measure == "gam" ~ "Outcome Subdomain", 
                         measure == "hr" ~ "Motor Milestone")
  
  bf_vars <- list(point, lb, ub) %>% 
    map(~ str_c(., "_bf"))
  nobf_vars <- list(point, lb, ub) %>% 
    map(~ str_c(., "_nobf"))
  
  
  format_ci <- function(point.est, lower, upper){
    
    fmt_point <- format_num(point.est)
    fmt_lb <- format_num(lower)
    fmt_ub <- format_num(upper)
    
    paste0(fmt_point, " (", fmt_lb, ", ", fmt_ub, ")")
  }
  
  
  data %>% 
    select(-starts_with("pred")) %>% 
    mutate(# across(.cols = where(is.numeric),
      # .fns = ~ round(., 2)), 
      hyp = hyp_num, 
      t_exp = str_sub(X, start = -1, end = -1),
      outcome_domain = case_when(str_detect(Y, "who") ~ "WHO Motor Milestones", 
                                 str_detect(Y, "cdi") ~ "Communicative Development Inventory",
                                 str_detect(Y, "(personal|motor|combined|com|pstot|mottot|globaltot|comtot)") ~ 
                                   "Extended Ages and Stages"), 
      # add timepoint
      outcome_domain = case_when(hyp_num %in% c(1, 4) ~ str_c(outcome_domain, " (Year 1)"), 
                                 hyp_num %in% c(2, 3, 5) ~ str_c(outcome_domain, " (Year 2)")),
      x_human = case_when(str_detect(X, "aat") ~ "alpha-1 antitrypsin", 
                          str_detect(X, "mpo") ~ "myeloperoxidase", 
                          str_detect(X, "neo") ~ "neopterin", 
                          str_detect(X, "reg") ~ "regenerating gene 1B", 
                          str_detect(X, "_L") ~ "lactulose", 
                          str_detect(X, "_M") ~ "mannitol"),
      x_human = case_when(between(hyp_num, 1, 3) ~ case_when(t_exp == 1 ~ str_c(x_human, " (3 mo)"),
                                                             t_exp == 2 ~ str_c(x_human, " (14 mo)")),
                          between(hyp_num, 4, 5) ~ case_when(t_exp == 1 ~ str_c(x_human, " (6 mo)"),
                                                             t_exp == 2 ~ str_c(x_human, " (17 mo)"))),
      y_subdomain = case_when(# WHO MM
        str_detect(Y, "sum") ~ "Sum Total", 
        str_detect(Y, "sub") ~ "Milestones 2,4,5,6", 
        
        str_detect(Y, "sit") ~ "Sitting", 
        str_detect(Y, "crawl") ~ "Crawling", 
        str_detect(Y, "walk") ~ "Walking", 
        str_detect(Y, "stand") ~ "Standing", 
        
        # CDI
        str_detect(Y, "und") ~ "Receptive", 
        str_detect(Y, "say") ~ "Expression without Language", 
        # EASQ Bangladesh
        str_detect(Y, "personal") ~ "Personal Social", 
        str_detect(Y, "motor") ~ "Motor", 
        str_detect(Y, "combined") ~ "Combined", 
        str_detect(Y, "com") ~ "Communication", 
        # EASQ Kenya
        str_detect(Y, "pstot") ~ "Personal Social", 
        str_detect(Y, "mottot") ~ "Motor", 
        str_detect(Y, "globaltot") ~ "Combined", 
        str_detect(Y, "comtot") ~ "Communication"),
      y_subdomain = case_when(str_detect(Y, "nosupp") & !str_detect(Y, "crawl") ~ 
                                str_c(y_subdomain, " (w/o support)"), 
                              str_detect(Y, "supp") ~ str_c(y_subdomain, " (w/ support)"), 
                              TRUE ~ y_subdomain),
      
      # another layer of correction for WHO MM
      case_match(y_subdomain,
                 "Sitting (w/o support)"     ~ "Sitting without support",
                 "Crawling (w/o support)"    ~ "Hands-and-knees crawling",
                 "Standing (w/ support)"     ~ "Standing with assistance",
                 "Walking (w/ support)"      ~ "Walking with assistance",
                 "Standing (w/o support)"    ~ "Standing alone",
                 "Walking (w/o support)"     ~ "Walking alone"),
      
      # format numbers 
      pasted_results_bf := format_ci(!!rlang::sym(bf_vars[[1]]), !!rlang::sym(bf_vars[[2]]), !!rlang::sym(bf_vars[[3]])),
      pasted_results_nobf := format_ci(!!rlang::sym(nobf_vars[[1]]), !!rlang::sym(nobf_vars[[2]]), !!rlang::sym(nobf_vars[[3]])),
    ) %>% 
    select(hyp,
           "Outcome Domain" = outcome_domain, 
           !!y_sd_name := y_subdomain, 
           "Exposure" = x_human, t_exp,
           starts_with("N"), 
           starts_with("q1"), starts_with("q3"), 
           !!paste0(pasted_var, " (BF)") := pasted_results_bf, 
           
           !!paste0(pasted_var, " (No BF)") := pasted_results_nobf, 
           starts_with("Pval"),
           ends_with("bf"))
}




list(
  bangladesh_bf[1:4] %>% 
    map(~ clean_results(.x, 1)), 
  bangladesh_bf[5:8] %>% 
    map(~ clean_results(.x, 2)), 
  kenya_bf[1:2] %>% 
    map(~ clean_results(.x, 4)), 
  kenya_bf[3:4] %>% 
    map(~ clean_results(.x, 5))
) %>% 
  list_flatten() %>% 
  write_rds(here("results/excl_breastfeeding.RDS"))



res <- list(
  bangladesh_bf[1:4] %>% 
    map(~ clean_results(.x, 1)), 
  bangladesh_bf[5:8] %>% 
    map(~ clean_results(.x, 2)), 
  kenya_bf[1:2] %>% 
    map(~ clean_results(.x, 4)), 
  kenya_bf[3:4] %>% 
    map(~ clean_results(.x, 5))
) %>% 
  list_flatten()  
res

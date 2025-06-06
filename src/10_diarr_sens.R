rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(here('final-data/eed-dev_bg.RDS'))

#clean covariates to avoid high missingness
d <- d %>% mutate(
  diar7d_t3 = factor(case_when(is.na(diar7d_t3) ~ "Missing",
                               diar7d_t3 ==0 ~ "No diar",
                               diar7d_t3 == 1 ~ "Diar")),
  diar7d_t2 = factor(case_when(is.na(diar7d_t2) ~ "Missing",
                               diar7d_t2 ==0 ~ "No diar",
                               diar7d_t2 == 1 ~ "Diar")),
  laz_t1 = factor(case_when(is.na(laz_t1) ~ "Missing",
                            laz_t1 < -2 ~ "Stunted",
                            laz_t1 >= (-2) ~ "Not stunted")),
  waz_t1 = factor(case_when(is.na(waz_t1) ~ "Missing",
                            waz_t1 < -2 ~ "Wasted",
                            waz_t1 >= (-2) ~ "Not wasted")),
  cesd_sum_t2=as.numeric(as.character(cesd_sum_t2)),
  cesd_sum_t2 = factor(case_when(is.na(cesd_sum_t2) ~ "Missing",
                                 cesd_sum_t2 < 16 ~ "Not depressed",
                                 cesd_sum_t2 >= 16 ~ "Depressed"))
)



#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", 'roof', "HHwealth", 
         "life_viol_any_t3", "tr",
         'laz_t1', 'waz_t1', "cesd_sum_t2","diar7d_t2")

Wvars[!(Wvars %in% colnames(d))]

# Loop over exposure-outcome pairs


# --------------------------------------------------------------------------
#### Hypothesis 1 ####
# eed markers at t1 v. dev (who, cdi2) at t2

H1_W <- c(Wvars)

##########################
# adjustment sets 1-3
## exposure: fecal markers
## outcome: who mm
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c('who_sum_total', 'who_sub_total')

H1a_W <- c(H1_W, 'ageday_st1', 'agedays_motor', 
           'month_st1', 'month_mm')
H1a_W[!(H1a_W %in% colnames(d))]

#Fit models
H1a_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H1a_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1a_adj_models <- bind_rows(H1a_adj_models, res)
  }
}



#Get primary contrasts
H1a_adj_res <- NULL
for(i in 1:nrow(H1a_adj_models)){
  res <- data.frame(X=H1a_adj_models$X[i], Y=H1a_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1a_adj_models$fit[i][[1]], d=H1a_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1a_adj_res <-  bind_rows(H1a_adj_res , preds$res)
}



##########################
# adjustment sets 4-5
## exposure: urine markers
## outcome: who mm
##########################

Xvars <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars <- c('who_sum_total', 'who_sub_total')

H1b_W <- c(H1_W, 'ageday_ut1', 'agedays_cdi2', 
           'month_ut1', 'month_cdi2')
H1b_W[!(H1b_W %in% colnames(d))]

#Fit models
H1b_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H1b_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1b_adj_models <- bind_rows(H1b_adj_models, res)
  }
}



#Get primary contrasts
H1b_adj_res <- NULL
for(i in 1:nrow(H1b_adj_models)){
  res <- data.frame(X=H1b_adj_models$X[i], Y=H1b_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1b_adj_models$fit[i][[1]], d=H1b_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1b_adj_res <-  bind_rows(H1b_adj_res , preds$res)
}


##########################
# adjustment sets 6-8
## exposure: fecal markers
## outcome: cdi2
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c('z_age2mo_cdi_undyr1_all_no4', 
           'z_age2mo_cdi_sayyr1_all_no4')

H1c_W <- c(H1_W, 'ageday_st1', 'agedays_cdi2', 
           'month_st1', 'month_cdi2')
H1c_W[!(H1c_W %in% colnames(d))]

#Fit models
H1c_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H1c_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1c_adj_models <- bind_rows(H1c_adj_models, res)
  }
}



#Get primary contrasts
H1c_adj_res <- NULL
for(i in 1:nrow(H1c_adj_models)){
  res <- data.frame(X=H1c_adj_models$X[i], Y=H1c_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1c_adj_models$fit[i][[1]], d=H1c_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1c_adj_res <-  bind_rows(H1c_adj_res , preds$res)
}

##########################
# adjustment sets 9-10
## exposure: urine markers
## outcome: cdi2
##########################

Xvars <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars <- c('z_age2mo_cdi_undyr1_all_no4', 
           'z_age2mo_cdi_sayyr1_all_no4')

H1d_W <- c(H1_W, 'ageday_ut1', 'agedays_cdi2', 
           'month_ut1', 'month_cdi2')
H1d_W[!(H1d_W %in% colnames(d))]

#Fit models
H1d_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H1d_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1d_adj_models <- bind_rows(H1d_adj_models, res)
  }
}



#Get primary contrasts
H1d_adj_res <- NULL
for(i in 1:nrow(H1d_adj_models)){
  res <- data.frame(X=H1d_adj_models$X[i], Y=H1d_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1d_adj_models$fit[i][[1]], d=H1d_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1d_adj_res <-  bind_rows(H1d_adj_res , preds$res)
}



# --------------------------------------------------------------------------
#### Hypothesis 2 ####
# eed markers at t1/t2 v. dev (cdi3, easq) at t3

H2_W <- c(Wvars, 'laz_t2', 'waz_t2',
          'cesd_sum_ee_t3',	'pss_sum_mom_t3')

##########################
# adjustment sets 11-13
## exposure: fecal markers t1
## outcome: easq t3
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
           "z_age2mo_combined_no4", "z_age2mo_com_no4")

H2a_W <- c(H2_W, 'ageday_st1',	'agedays_easq', 
           'month_st1',	'month_easq')
H2a_W[!(H2a_W %in% colnames(d))]

#Fit models
H2a_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H2a_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2a_adj_models <- bind_rows(H2a_adj_models, res)
  }
}



#Get primary contrasts
H2a_adj_res <- NULL
for(i in 1:nrow(H2a_adj_models)){
  res <- data.frame(X=H2a_adj_models$X[i], Y=H2a_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2a_adj_models$fit[i][[1]], d=H2a_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2a_adj_res <-  bind_rows(H2a_adj_res , preds$res)
}

##########################
# adjustment sets 14-15
## exposure: urine markers t1
## outcome: easq t3
##########################

Xvars <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
           "z_age2mo_combined_no4", "z_age2mo_com_no4")

H2b_W <- c(H2_W, 'ageday_ut1',	'agedays_easq', 
           'month_ut1',	'month_easq')
H2b_W[!(H2b_W %in% colnames(d))]

#Fit models
H2b_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H2b_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2b_adj_models <- bind_rows(H2b_adj_models, res)
  }
}



#Get primary contrasts
H2b_adj_res <- NULL
for(i in 1:nrow(H2b_adj_models)){
  res <- data.frame(X=H2b_adj_models$X[i], Y=H2b_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2b_adj_models$fit[i][[1]], d=H2b_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2b_adj_res <-  bind_rows(H2b_adj_res , preds$res)
}

##########################
# adjustment sets 16-18
## exposure: fecal markers t1
## outcome: cdi t3
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")

H2c_W <- c(H2_W, 'ageday_st1', 'agedays_cdi3',
           'month_st1', 'month_cdi3')
H2c_W[!(H2c_W %in% colnames(d))]

#Fit models
H2c_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H2c_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2c_adj_models <- bind_rows(H2c_adj_models, res)
  }
}



#Get primary contrasts
H2c_adj_res <- NULL
for(i in 1:nrow(H2c_adj_models)){
  res <- data.frame(X=H2c_adj_models$X[i], Y=H2c_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2c_adj_models$fit[i][[1]], d=H2c_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2c_adj_res <-  bind_rows(H2c_adj_res , preds$res)
}

##########################
# adjustment sets 19-20
## exposure: urine markers t1
## outcome: cdi t3
##########################

Xvars <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")

H2d_W <- c(H2_W, 'ageday_ut1', 'agedays_cdi3',
           'month_ut1', 'month_cdi3')
H2d_W[!(H2d_W %in% colnames(d))]

#Fit models
H2d_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H2d_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2d_adj_models <- bind_rows(H2d_adj_models, res)
  }
}



#Get primary contrasts
H2d_adj_res <- NULL
for(i in 1:nrow(H2d_adj_models)){
  res <- data.frame(X=H2d_adj_models$X[i], Y=H2d_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2d_adj_models$fit[i][[1]], d=H2d_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2d_adj_res <-  bind_rows(H2d_adj_res , preds$res)
}

##########################
# adjustment sets 21-23
## exposure: fecal markers t2
## outcome: easq t3
##########################

Xvars <- c('ln_aat2', 'ln_mpo2', 'ln_neo2')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
           "z_age2mo_combined_no4", "z_age2mo_com_no4")

H2e_W <- c(H2_W, 'ageday_st2', 'agedays_easq', 
           'month_st2',	'month_easq')
H2e_W[!(H2e_W %in% colnames(d))]

#Fit models
H2e_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H2e_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2e_adj_models <- bind_rows(H2e_adj_models, res)
  }
}



#Get primary contrasts
H2e_adj_res <- NULL
for(i in 1:nrow(H2e_adj_models)){
  res <- data.frame(X=H2e_adj_models$X[i], Y=H2e_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2e_adj_models$fit[i][[1]], d=H2e_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2e_adj_res <-  bind_rows(H2e_adj_res , preds$res)
}

##########################
# adjustment sets 24-25
## exposure: urine markers t2
## outcome: easq t3
##########################

Xvars <- c('ln_L_conc_t2', 'ln_M_conc_t2')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
           "z_age2mo_combined_no4", "z_age2mo_com_no4")

H2f_W <- c(H2_W, 'ageday_ut2', 'agedays_easq', 
           'month_ut2',	'month_easq')
H2f_W[!(H2f_W %in% colnames(d))]

#Fit models
H2f_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H2f_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2f_adj_models <- bind_rows(H2f_adj_models, res)
  }
}



#Get primary contrasts
H2f_adj_res <- NULL
for(i in 1:nrow(H2f_adj_models)){
  res <- data.frame(X=H2f_adj_models$X[i], Y=H2f_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2f_adj_models$fit[i][[1]], d=H2f_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2f_adj_res <-  bind_rows(H2f_adj_res , preds$res)
}


##########################
# adjustment sets 26-28
## exposure: fecal markers t2
## outcome: cdi t3
##########################

Xvars <- c('ln_aat2', 'ln_mpo2', 'ln_neo2')           
Yvars <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")

H2g_W <- c(H2_W, 'ageday_st2', 'agedays_cdi3',
           'month_st2', 'month_cdi3')
H2g_W[!(H2g_W %in% colnames(d))]

#Fit models
H2g_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H2g_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2g_adj_models <- bind_rows(H2g_adj_models, res)
  }
}



#Get primary contrasts
H2g_adj_res <- NULL
for(i in 1:nrow(H2g_adj_models)){
  res <- data.frame(X=H2g_adj_models$X[i], Y=H2g_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2g_adj_models$fit[i][[1]], d=H2g_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2g_adj_res <-  bind_rows(H2g_adj_res , preds$res)
}

##########################
# adjustment sets 29-30
## exposure: urine markers t2
## outcome: cdi t3
##########################

Xvars <- c('ln_L_conc_t2', 'ln_M_conc_t2')           
Yvars <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")

H2h_W <- c(H2_W, 'ageday_ut2', 'agedays_cdi3',
           'month_ut2', 'month_cdi3')
H2h_W[!(H2h_W %in% colnames(d))]

#Fit models
H2h_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H2h_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2h_adj_models <- bind_rows(H2h_adj_models, res)
  }
}



#Get primary contrasts
H2h_adj_res <- NULL
for(i in 1:nrow(H2h_adj_models)){
  res <- data.frame(X=H2h_adj_models$X[i], Y=H2h_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2h_adj_models$fit[i][[1]], d=H2h_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2h_adj_res <-  bind_rows(H2h_adj_res , preds$res)
}

# --------------------------------------------------------------------------
#### Hypothesis 3 ####
# reg1b at t2 v. dev (cdi3, easq) at t3

H3_W <- c(Wvars, 'laz_t2', 'waz_t2',
          'cesd_sum_ee_t3',	'pss_sum_mom_t3')


##########################
# adjustment set 31
## exposure: reg1b t2
## outcome: easq t3
##########################

Xvars <- c('ln_reg2')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
            "z_age2mo_combined_no4", "z_age2mo_com_no4")

H3a_W <- c(H3_W, 'ageday_st2', 'agedays_easq',
           'month_st2', 'month_easq')
H3a_W[!(H3a_W %in% colnames(d))]

#Fit models
H3a_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H3a_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3a_adj_models <- bind_rows(H3a_adj_models, res)
  }
}



#Get primary contrasts
H3a_adj_res <- NULL
for(i in 1:nrow(H3a_adj_models)){
  res <- data.frame(X=H3a_adj_models$X[i], Y=H3a_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H3a_adj_models$fit[i][[1]], d=H3a_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3a_adj_res <-  bind_rows(H3a_adj_res , preds$res)
}

##########################
# adjustment sets 32
## exposure: reg1b t2
## outcome: cdi t3
##########################

Xvars <- c('ln_reg2')           
Yvars <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")

H3b_W <- c(H3_W, 'ageday_st2', 'agedays_cdi3',
           'month_st2', 'month_cdi3')
H3b_W[!(H3b_W %in% colnames(d))]

#Fit models
H3b_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diar7d_t2",d=d, X=i, Y=j,  W=H3b_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3b_adj_models <- bind_rows(H3b_adj_models, res)
  }
}



#Get primary contrasts
H3b_adj_res <- NULL
for(i in 1:nrow(H3b_adj_models)){
  res <- data.frame(X=H3b_adj_models$X[i], Y=H3b_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H3b_adj_models$fit[i][[1]], d=H3b_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3b_adj_res <-  bind_rows(H3b_adj_res , preds$res)
}

# ------------------------------------------------------------------------
####### kenya hypotheses ####### 

d <- readRDS(here('final-data/eed-dev_k.RDS'))

d <- d %>% mutate(
  diarr7_t1 = factor(case_when(is.na(diarr7_t1) ~ "Missing",
                               diarr7_t1 ==0 ~ "No diar",
                               diarr7_t1 == 1 ~ "Diar")),
  diarr7_t2 = factor(case_when(is.na(diarr7_t2) ~ "Missing",
                               diarr7_t2 ==0 ~ "No diar",
                               diarr7_t2 == 1 ~ "Diar")))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "HHS", "Nlt18","Ncomp", "water_time", 
         "floor", 'roof', "hh_index", 
         "tr","diarr7_t1")

Wvars[!(Wvars %in% colnames(d))]

# ------------------------------------------------------------------------
# --------------------------------------------------------------------------
#### Hypothesis 4 ####
# eed markers at t2 v. dev (who mm) at t2

H4_W <- c(Wvars,
          'laz_t1', 'waz_t1',
          'aged1',	'agedays_motor', 
          'month_mm')


##########################
# adjustment sets 33-35
## exposure: fecal markers t1
## outcome: who mm t2
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c("who_sub_total", "who_sum_total")

H4a_W <- c(H4_W, 'month_st1')
H4a_W[!(H4a_W %in% colnames(d))]

#Fit models
H4a_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diarr7_t1",d=d, X=i, Y=j,  W=H4a_W[1:18])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4a_adj_models <- bind_rows(H4a_adj_models, res)
  }
}



#Get primary contrasts
H4a_adj_res <- NULL
for(i in 1:nrow(H4a_adj_models)){
  res <- data.frame(X=H4a_adj_models$X[i], Y=H4a_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H4a_adj_models$fit[i][[1]], d=H4a_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4a_adj_res <-  bind_rows(H4a_adj_res , preds$res)
}

#Make list of plots
H4a_adj_plot_list <- NULL
H4a_adj_plot_data <- NULL
for(i in 1:nrow(H4a_adj_models)){
  res <- data.frame(X=H4a_adj_models$X[i], Y=H4a_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H4a_adj_models$fit[i][[1]], H4a_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4a_adj_plot_list[[i]] <-  simul_plot$p
  H4a_adj_plot_data <-  rbind(H4a_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


# #Save models
# saveRDS(H4a_adj_models, paste0(dropboxDir,"results/eed-dev-models/models/H4a_adj_models.RDS"))
# 
# #Save results
# saveRDS(H4a_adj_res, here("results/adjusted/H4a_adj_res.RDS"))
# 
# 
# #Save plots
# saveRDS(H4a_adj_plot_list, paste0(dropboxDir,"results/eed-dev-models/figure-objects/H4a_adj_splines.RDS"))
# 
# #Save plot data
# saveRDS(H4a_adj_plot_data,  here("figure-data/H4a_adj_spline_data.RDS"))

##########################
# adjustment sets 36-37
## exposure: urine markers t1
## outcome: who mm t2
##########################

Xvars <- c('ln_Lact1', 'ln_Mann1')           
Yvars <- c("who_sub_total", "who_sum_total")

H4b_W <- c(H4_W, 'month_ut1')
H4b_W[!(H4b_W %in% colnames(d))]

#Fit models
H4b_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diarr7_t1",d=d, X=i, Y=j,  W=H4b_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4b_adj_models <- bind_rows(H4b_adj_models, res)
  }
}



#Get primary contrasts
H4b_adj_res <- NULL
for(i in 1:nrow(H4b_adj_models)){
  res <- data.frame(X=H4b_adj_models$X[i], Y=H4b_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H4b_adj_models$fit[i][[1]], d=H4b_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4b_adj_res <-  bind_rows(H4b_adj_res , preds$res)
}

#Make list of plots
H4b_adj_plot_list <- NULL
H4b_adj_plot_data <- NULL
for(i in 1:nrow(H4b_adj_models)){
  res <- data.frame(X=H4b_adj_models$X[i], Y=H4b_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H4b_adj_models$fit[i][[1]], H4b_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4b_adj_plot_list[[i]] <-  simul_plot$p
  H4b_adj_plot_data <-  rbind(H4b_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


# #Save models
# saveRDS(H4b_adj_models, paste0(dropboxDir,"results/eed-dev-models/models/H4b_adj_models.RDS"))
# 
# #Save results
# saveRDS(H4b_adj_res, here("results/adjusted/H4b_adj_res.RDS"))
# 
# 
# #Save plots
# saveRDS(H4b_adj_plot_list, paste0(dropboxDir,"results/eed-dev-models/figure-objects/H4b_adj_splines.RDS"))
# 
# #Save plot data
# saveRDS(H4b_adj_plot_data,  here("figure-data/H4b_adj_spline_data.RDS"))

# --------------------------------------------------------------------------
#### Hypothesis 5 ####
# eed markers at t1/t2 v. dev (easq) at t3

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

H5a_W <- c(H5_W, 'aged1', 'month_st1')
H5a_W[!(H5a_W %in% colnames(d))]

#Fit models
H5a_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diarr7_t1",d=d, X=i, Y=j,  W=H5a_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H5a_adj_models <- bind_rows(H5a_adj_models, res)
  }
}



#Get primary contrasts
H5a_adj_res <- NULL
for(i in 1:nrow(H5a_adj_models)){
  res <- data.frame(X=H5a_adj_models$X[i], Y=H5a_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H5a_adj_models$fit[i][[1]], d=H5a_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H5a_adj_res <-  bind_rows(H5a_adj_res , preds$res)
}

#Make list of plots
H5a_adj_plot_list <- NULL
H5a_adj_plot_data <- NULL
for(i in 1:nrow(H5a_adj_models)){
  res <- data.frame(X=H5a_adj_models$X[i], Y=H5a_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H5a_adj_models$fit[i][[1]], H5a_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H5a_adj_plot_list[[i]] <-  simul_plot$p
  H5a_adj_plot_data <-  rbind(H5a_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


# #Save models
# saveRDS(H5a_adj_models, paste0(dropboxDir,"results/eed-dev-models/models/H5a_adj_models.RDS"))
# 
# #Save results
# saveRDS(H5a_adj_res, here("results/adjusted/H5a_adj_res.RDS"))
# 
# 
# #Save plots
# saveRDS(H5a_adj_plot_list, paste0(dropboxDir,"results/eed-dev-models/figure-objects/H5a_adj_splines.RDS"))
# 
# #Save plot data
# saveRDS(H5a_adj_plot_data,  here("figure-data/H5a_adj_spline_data.RDS"))

##########################
# adjustment sets 41-42
## exposure: urine markers t1
## outcome: easq t3
##########################

Xvars <- c('ln_Lact1', 'ln_Mann1')           
Yvars <- c("z_comtot_no4_activec", "z_mottot_no4_activec", "z_pstot_no4_activec", "z_globaltot_no4_activec")

H5b_W <- c(H5_W, 'aged1', 'month_ut1')
H5b_W[!(H5b_W %in% colnames(d))]

#Fit models
H5b_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diarr7_t1",d=d, X=i, Y=j,  W=H5b_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H5b_adj_models <- bind_rows(H5b_adj_models, res)
  }
}



#Get primary contrasts
H5b_adj_res <- NULL
for(i in 1:nrow(H5b_adj_models)){
  res <- data.frame(X=H5b_adj_models$X[i], Y=H5b_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H5b_adj_models$fit[i][[1]], d=H5b_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H5b_adj_res <-  bind_rows(H5b_adj_res , preds$res)
}

#Make list of plots
H5b_adj_plot_list <- NULL
H5b_adj_plot_data <- NULL
for(i in 1:nrow(H5b_adj_models)){
  res <- data.frame(X=H5b_adj_models$X[i], Y=H5b_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H5b_adj_models$fit[i][[1]], H5b_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H5b_adj_plot_list[[i]] <-  simul_plot$p
  H5b_adj_plot_data <-  rbind(H5b_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}



##########################
# adjustment sets 43-45
## exposure: fecal markers t2
## outcome: easq t3
##########################

Xvars <- c('ln_aat2', 'ln_mpo2', 'ln_neo2')           
Yvars <- c("z_comtot_no4_activec", "z_mottot_no4_activec", "z_pstot_no4_activec", "z_globaltot_no4_activec")

H5c_W <- c(H5_W, 'aged2', 'month_st2',"diarr7_t1")
H5c_W[!(H5c_W %in% colnames(d))]

#Fit models
H5c_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj=NULL
    try(res_adj <- fit_RE_gam(forcedW = "diarr7_t1",d=d, X=i, Y=j,  W=H5c_W))
    if(is.null(res_adj)){
      try(res_adj <- fit_RE_gam(forcedW = "diarr7_t1",d=d, X=i, Y=j,  W=H5c_W, vim=FALSE))
    }
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H5c_adj_models <- bind_rows(H5c_adj_models, res)
  }
}



#Get primary contrasts
H5c_adj_res <- NULL
for(i in 1:nrow(H5c_adj_models)){
  res <- data.frame(X=H5c_adj_models$X[i], Y=H5c_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H5c_adj_models$fit[i][[1]], d=H5c_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H5c_adj_res <-  bind_rows(H5c_adj_res , preds$res)
}



##########################
# adjustment sets 46-47
## exposure: urine markers t2
## outcome: easq t3
##########################

Xvars <- c('ln_Lact2', 'ln_Mann2')           
Yvars <- c("z_comtot_no4_activec", "z_mottot_no4_activec", "z_pstot_no4_activec", "z_globaltot_no4_activec")

H5d_W <- c(H5_W, 'aged2', 'month_ut2',"diarr7_t2")
H5d_W[!(H5d_W %in% colnames(d))]

#Fit models
H5d_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(forcedW = "diarr7_t2",d=d, X=i, Y=j,  W=H5d_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H5d_adj_models <- bind_rows(H5d_adj_models, res)
  }
}



#Get primary contrasts
H5d_adj_res <- NULL
for(i in 1:nrow(H5d_adj_models)){
  res <- data.frame(X=H5d_adj_models$X[i], Y=H5d_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H5d_adj_models$fit[i][[1]], d=H5d_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H5d_adj_res <-  bind_rows(H5d_adj_res , preds$res)
}




# --------------------------------------------------------------------------
# combine dataframes for each hypothesis

# get object names
for(hyp in paste0("H", 1:3)){
  print(str_subset(ls(), str_glue("^{hyp}.*res$")))
}

bind_rows(H1a_adj_res, H1b_adj_res, H1c_adj_res, H1d_adj_res) %>%
  saveRDS("results/sensitivity/H1_diar_sens_res.RDS")

bind_rows(H2a_adj_res, H2b_adj_res, H2c_adj_res, 
          H2d_adj_res, H2e_adj_res, H2f_adj_res,
          H2g_adj_res, H2h_adj_res) %>%
  saveRDS("results/sensitivity/H2_diar_sens_res.RDS")

bind_rows(H3a_adj_res, H3b_adj_res) %>% 
  saveRDS("results/sensitivity/H3_diar_sens_res.RDS")

bind_rows(H4a_adj_res, H4b_adj_res) %>%
  saveRDS("results/sensitivity/H4_diar_sens_res.RDS")

bind_rows(H5a_adj_res, H5b_adj_res, H5c_adj_res, 
          H5d_adj_res) %>%
  saveRDS("results/sensitivity/H5_diar_sens_res.RDS")










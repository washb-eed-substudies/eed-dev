rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS('eed-dev_bg.RDS')

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", 'roof', "HHwealth", 
         "life_viol_any_t3", "tr",
         
         'laz_t1', 'waz_t1', "cesd_sum_t2")

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

Xvars <- c('aat1', 'mpo1', 'neo1')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H1a_W)
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

#Make list of plots
H1a_adj_plot_list <- NULL
H1a_adj_plot_data <- NULL
for(i in 1:nrow(H1a_adj_models)){
  res <- data.frame(X=H1a_adj_models$X[i], Y=H1a_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1a_adj_models$fit[i][[1]], H1a_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1a_adj_plot_list[[i]] <-  simul_plot$p
  H1a_adj_plot_data <-  rbind(H1a_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1a_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1a_adj_models.RDS"))

#Save results
saveRDS(H1a_adj_res, here("results/adjusted/H1a_adj_res.RDS"))


#Save plots
#saveRDS(H1a_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1a_adj_splines.RDS"))

#Save plot data
#saveRDS(H1a_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1a_adj_spline_data.RDS"))

##########################
# adjustment sets 4-5
## exposure: urine markers
## outcome: who mm
##########################

Xvars <- c('L_conc_t1', 'M_conc_t1')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H1b_W)
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

#Make list of plots
H1b_adj_plot_list <- NULL
H1b_adj_plot_data <- NULL
for(i in 1:nrow(H1b_adj_models)){
  res <- data.frame(X=H1b_adj_models$X[i], Y=H1b_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1b_adj_models$fit[i][[1]], H1b_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1b_adj_plot_list[[i]] <-  simul_plot$p
  H1b_adj_plot_data <-  rbind(H1b_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1b_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1b_adj_models.RDS"))

#Save results
saveRDS(H1b_adj_res, here("results/adjusted/H1b_adj_res.RDS"))


#Save plots
#saveRDS(H1b_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1b_adj_splines.RDS"))

#Save plot data
#saveRDS(H1b_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1b_adj_spline_data.RDS"))


##########################
# adjustment sets 6-8
## exposure: fecal markers
## outcome: cdi2
##########################

Xvars <- c('aat1', 'mpo1', 'neo1')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H1c_W)
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

#Make list of plots
H1c_adj_plot_list <- NULL
H1c_adj_plot_data <- NULL
for(i in 1:nrow(H1c_adj_models)){
  res <- data.frame(X=H1c_adj_models$X[i], Y=H1c_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1c_adj_models$fit[i][[1]], H1c_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1c_adj_plot_list[[i]] <-  simul_plot$p
  H1c_adj_plot_data <-  rbind(H1c_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1c_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1c_adj_models.RDS"))

#Save results
saveRDS(H1c_adj_res, here("results/adjusted/H1c_adj_res.RDS"))


#Save plots
#saveRDS(H1c_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1c_adj_splines.RDS"))

#Save plot data
#saveRDS(H1c_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1c_adj_spline_data.RDS"))

##########################
# adjustment sets 9-10
## exposure: urine markers
## outcome: cdi2
##########################

Xvars <- c('L_conc_t1', 'M_conc_t1')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H1d_W)
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

#Make list of plots
H1d_adj_plot_list <- NULL
H1d_adj_plot_data <- NULL
for(i in 1:nrow(H1d_adj_models)){
  res <- data.frame(X=H1d_adj_models$X[i], Y=H1d_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1d_adj_models$fit[i][[1]], H1d_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1d_adj_plot_list[[i]] <-  simul_plot$p
  H1d_adj_plot_data <-  rbind(H1d_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1d_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1d_adj_models.RDS"))

#Save results
saveRDS(H1d_adj_res, here("results/adjusted/H1d_adj_res.RDS"))


#Save plots
#saveRDS(H1d_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1d_adj_splines.RDS"))

#Save plot data
#saveRDS(H1d_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1d_adj_spline_data.RDS"))



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

Xvars <- c('aat1', 'mpo1', 'neo1')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H2a_W)
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

#Make list of plots
H2a_adj_plot_list <- NULL
H2a_adj_plot_data <- NULL
for(i in 1:nrow(H2a_adj_models)){
  res <- data.frame(X=H2a_adj_models$X[i], Y=H2a_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2a_adj_models$fit[i][[1]], H2a_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2a_adj_plot_list[[i]] <-  simul_plot$p
  H2a_adj_plot_data <-  rbind(H2a_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2a_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H2a_adj_models.RDS"))

#Save results
saveRDS(H2a_adj_res, here("results/adjusted/H2a_adj_res.RDS"))


#Save plots
#saveRDS(H2a_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2a_adj_splines.RDS"))

#Save plot data
#saveRDS(H2a_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2a_adj_spline_data.RDS"))

##########################
# adjustment sets 14-15
## exposure: urine markers t1
## outcome: easq t3
##########################

Xvars <- c('L_conc_t1', 'M_conc_t1')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H2b_W)
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

#Make list of plots
H2b_adj_plot_list <- NULL
H2b_adj_plot_data <- NULL
for(i in 1:nrow(H2b_adj_models)){
  res <- data.frame(X=H2b_adj_models$X[i], Y=H2b_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2b_adj_models$fit[i][[1]], H2b_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2b_adj_plot_list[[i]] <-  simul_plot$p
  H2b_adj_plot_data <-  rbind(H2b_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2b_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H2b_adj_models.RDS"))

#Save results
saveRDS(H2b_adj_res, here("results/adjusted/H2b_adj_res.RDS"))


#Save plots
#saveRDS(H2b_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2b_adj_splines.RDS"))

#Save plot data
#saveRDS(H2b_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2b_adj_spline_data.RDS"))


##########################
# adjustment sets 16-18
## exposure: fecal markers t1
## outcome: cdi t3
##########################

Xvars <- c('aat1', 'mpo1', 'neo1')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H2c_W)
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

#Make list of plots
H2c_adj_plot_list <- NULL
H2c_adj_plot_data <- NULL
for(i in 1:nrow(H2c_adj_models)){
  res <- data.frame(X=H2c_adj_models$X[i], Y=H2c_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2c_adj_models$fit[i][[1]], H2c_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2c_adj_plot_list[[i]] <-  simul_plot$p
  H2c_adj_plot_data <-  rbind(H2c_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2c_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H2c_adj_models.RDS"))

#Save results
saveRDS(H2c_adj_res, here("results/adjusted/H2c_adj_res.RDS"))


#Save plots
#saveRDS(H2c_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2c_adj_splines.RDS"))

#Save plot data
#saveRDS(H2c_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2c_adj_spline_data.RDS"))

##########################
# adjustment sets 19-20
## exposure: urine markers t1
## outcome: cdi t3
##########################

Xvars <- c('L_conc_t1', 'M_conc_t1')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H2d_W)
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

#Make list of plots
H2d_adj_plot_list <- NULL
H2d_adj_plot_data <- NULL
for(i in 1:nrow(H2d_adj_models)){
  res <- data.frame(X=H2d_adj_models$X[i], Y=H2d_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2d_adj_models$fit[i][[1]], H2d_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2d_adj_plot_list[[i]] <-  simul_plot$p
  H2d_adj_plot_data <-  rbind(H2d_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2d_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H2d_adj_models.RDS"))

#Save results
saveRDS(H2d_adj_res, here("results/adjusted/H2d_adj_res.RDS"))


#Save plots
#saveRDS(H2d_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2d_adj_splines.RDS"))

#Save plot data
#saveRDS(H2d_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2d_adj_spline_data.RDS"))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##########################
# adjustment sets 21-23
## exposure: fecal markers t2
## outcome: easq t3
##########################

Xvars <- c('aat2', 'mpo2', 'neo2')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H2e_W)
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

#Make list of plots
H2e_adj_plot_list <- NULL
H2e_adj_plot_data <- NULL
for(i in 1:nrow(H2e_adj_models)){
  res <- data.frame(X=H2e_adj_models$X[i], Y=H2e_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2e_adj_models$fit[i][[1]], H2e_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2e_adj_plot_list[[i]] <-  simul_plot$p
  H2e_adj_plot_data <-  rbind(H2e_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2e_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H2e_adj_models.RDS"))

#Save results
saveRDS(H2e_adj_res, here("results/adjusted/H2e_adj_res.RDS"))


#Save plots
#saveRDS(H2e_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2e_adj_splines.RDS"))

#Save plot data
#saveRDS(H2e_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2e_adj_spline_data.RDS"))

##########################
# adjustment sets 24-25
## exposure: urine markers t2
## outcome: easq t3
##########################

Xvars <- c('L_conc_t2', 'M_conc_t2')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H2f_W)
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

#Make list of plots
H2f_adj_plot_list <- NULL
H2f_adj_plot_data <- NULL
for(i in 1:nrow(H2f_adj_models)){
  res <- data.frame(X=H2f_adj_models$X[i], Y=H2f_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2f_adj_models$fit[i][[1]], H2f_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2f_adj_plot_list[[i]] <-  simul_plot$p
  H2f_adj_plot_data <-  rbind(H2f_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2f_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H2f_adj_models.RDS"))

#Save results
saveRDS(H2f_adj_res, here("results/adjusted/H2f_adj_res.RDS"))


#Save plots
#saveRDS(H2f_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2f_adj_splines.RDS"))

#Save plot data
#saveRDS(H2f_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2f_adj_spline_data.RDS"))


##########################
# adjustment sets 26-28
## exposure: fecal markers t2
## outcome: cdi t3
##########################

Xvars <- c('aat2', 'mpo2', 'neo2')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H2g_W)
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

#Make list of plots
H2g_adj_plot_list <- NULL
H2g_adj_plot_data <- NULL
for(i in 1:nrow(H2g_adj_models)){
  res <- data.frame(X=H2g_adj_models$X[i], Y=H2g_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2g_adj_models$fit[i][[1]], H2g_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2g_adj_plot_list[[i]] <-  simul_plot$p
  H2g_adj_plot_data <-  rbind(H2g_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2g_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H2g_adj_models.RDS"))

#Save results
saveRDS(H2g_adj_res, here("results/adjusted/H2g_adj_res.RDS"))


#Save plots
#saveRDS(H2g_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2g_adj_splines.RDS"))

#Save plot data
#saveRDS(H2g_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2g_adj_spline_data.RDS"))

##########################
# adjustment sets 29-30
## exposure: urine markers t2
## outcome: cdi t3
##########################

Xvars <- c('L_conc_t2', 'M_conc_t2')           
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
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H2h_W)
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

#Make list of plots
H2h_adj_plot_list <- NULL
H2h_adj_plot_data <- NULL
for(i in 1:nrow(H2h_adj_models)){
  res <- data.frame(X=H2h_adj_models$X[i], Y=H2h_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2h_adj_models$fit[i][[1]], H2h_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2h_adj_plot_list[[i]] <-  simul_plot$p
  H2h_adj_plot_data <-  rbind(H2h_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2h_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H2h_adj_models.RDS"))

#Save results
saveRDS(H2h_adj_res, here("results/adjusted/H2h_adj_res.RDS"))


#Save plots
#saveRDS(H2h_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2h_adj_splines.RDS"))

#Save plot data
#saveRDS(H2h_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2h_adj_spline_data.RDS"))


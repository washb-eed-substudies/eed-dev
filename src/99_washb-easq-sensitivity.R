rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS('eed-dev_bg.RDS')

############################################################################
############################################################################
########################### UNADJUSTED MODELS ##############################
############################################################################
############################################################################

#### Hypothesis 2 ####
# eed markers at t1/t2 v. dev (cdi3, easq) at t3
Xvars <- c('aat1', 'mpo1', 'neo1', 
           'L_conc_t1', 'M_conc_t1',
           'aat2', 'mpo2', 'neo2', 
           'L_conc_t2', 'M_conc_t2')            
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", "z_age2mo_combined_no4", "z_age2mo_com_no4", 
           "z_age2mo_personal_all", "z_age2mo_motor_all", "z_age2mo_combined_all", "z_age2mo_com_all") 


#Fit models
H2_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H2_models <- bind_rows(H2_models, res)
  }
}

#Get primary contrasts
H2_res <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  preds <- predict_gam_diff(fit=H2_models$fit[i][[1]], d=H2_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_res <-  bind_rows(H2_res , preds$res)
}

#Make list of plots
H2_plot_list <- NULL
H2_plot_data <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  simul_plot <- gam_simul_CI(H2_models$fit[i][[1]], H2_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_plot_list[[i]] <-  simul_plot$p
  H2_plot_data <-  rbind(H2_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

# correct p-value
H2_res_all <- H2_res %>%  
  separate(Y, into = c(NA, NA, 'dom', 'data_grp'), sep = "_") %>% 
  group_by(dom, X) %>% 
  # mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>% 
  # select(dom:N, point.diff, Pval) %>% 
  pivot_wider(id_cols = c(dom, X), 
              names_from = data_grp, 
              values_from = c(N, point.diff, lb.diff, ub.diff, Pval)) %>% 
  mutate(abs_diff =  point.diff_all - point.diff_no4, 
         sensitivity =  abs_diff/point.diff_all)



# --------------------------------------------------------------------------
############################################################################
############################################################################
############################ ADJUSTED MODELS ###############################
############################################################################
############################################################################


#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", 'roof', "HHwealth", 
         "life_viol_any_t3", "tr",
         
         'laz_t1', 'waz_t1', "cesd_sum_t2")

Wvars[!(Wvars %in% colnames(d))]

# Loop over exposure-outcome pairs


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
           "z_age2mo_combined_no4", "z_age2mo_com_no4", 
           "z_age2mo_personal_all", "z_age2mo_motor_all", 
           "z_age2mo_combined_all", "z_age2mo_com_all")

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



##########################
# adjustment sets 14-15
## exposure: urine markers t1
## outcome: easq t3
##########################

Xvars <- c('L_conc_t1', 'M_conc_t1')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
           "z_age2mo_combined_no4", "z_age2mo_com_no4", 
           "z_age2mo_personal_all", "z_age2mo_motor_all", 
           "z_age2mo_combined_all", "z_age2mo_com_all")

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




##########################
# adjustment sets 21-23
## exposure: fecal markers t2
## outcome: easq t3
##########################

Xvars <- c('aat2', 'mpo2', 'neo2')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
           "z_age2mo_combined_no4", "z_age2mo_com_no4", 
           "z_age2mo_personal_all", "z_age2mo_motor_all", 
           "z_age2mo_combined_all", "z_age2mo_com_all")

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




##########################
# adjustment sets 24-25
## exposure: urine markers t2
## outcome: easq t3
##########################

Xvars <- c('L_conc_t2', 'M_conc_t2')           
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", 
           "z_age2mo_combined_no4", "z_age2mo_com_no4", 
           "z_age2mo_personal_all", "z_age2mo_motor_all", 
           "z_age2mo_combined_all", "z_age2mo_com_all")

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


H2_adj_all <- bind_rows(H2a_adj_res, H2b_adj_res,
          H2e_adj_res, H2f_adj_res) %>% 
  separate(Y, into = c(NA, NA, 'dom', 'data_grp'), sep = "_") %>% 
  group_by(dom, X) %>% 
  # mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>% 
  # select(dom:N, point.diff, Pval) %>% 
  pivot_wider(id_cols = c(dom, X), 
              names_from = data_grp, 
              values_from = c(N, point.diff, lb.diff, ub.diff, Pval)) %>% 
  mutate(abs_diff =  point.diff_all - point.diff_no4, 
         sensitivity =  abs_diff/point.diff_all)


writexl::write_xlsx(list(
  'unadj' = H2_res, 
  'adj' = H2_adj_all), 
  'results/easq-sensitivity-analysis.xlsx'
)

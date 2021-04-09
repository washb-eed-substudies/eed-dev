# WHO Hazard Ratios
# WASH Benefits 
# Bangladesh and Kenya

rm(list=ls())

source(here::here("0-config.R"))

# ----
# Bangladesh

d <- readRDS('eed-dev_bg.RDS')

## Unadjusted 

Xvars <- c('aat1', 'mpo1', 'neo1',
           'ln_L_conc_t1', 'ln_M_conc_t1') 
Yvars <- c("sit_nosupp", "crawl_nosupp", "stand_supp",
           "walk_supp", "stand_nosupp", "walk_nosupp" )

#Fit models
H1_who_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_HR_GAM(d=d, X=i, Y=j, age = 'agedays_motor', W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1_who_models <- bind_rows(H1_who_models, res)
  }
}
#Get primary contrasts
H1_who_res <- NULL
for(i in 1:nrow(H1_who_models)){
  res <- data.frame(X=H1_who_models$X[i], Y=H1_who_models$Y[i])
  preds <- predict_gam_HR(fit=H1_who_models$fit[i][[1]], d=H1_who_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_who_res <-  bind_rows(H1_who_res , preds$res)
}
#Make list of plots
H1_who_plot_list <- NULL
H1_who_plot_data <- NULL
for(i in 1:nrow(H1_who_models)){
  res <- data.frame(X=H1_who_models$X[i], Y=H1_who_models$Y[i])
  simul_plot <- gam_simul_CI(H1_who_models$fit[i][[1]], H1_who_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_who_plot_list[[i]] <-  simul_plot$p
  H1_who_plot_data <-  rbind(H1_who_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

# correct p-value
H1_who_res <- H1_who_res %>%  
  mutate(corrected.Pval=p.adjust(Pval, method="BH"))
#Save models
saveRDS(H1_who_models, here("models/H1_who_models.RDS"))
#Save results
saveRDS(H1_who_res, here("results/unadjusted/H1_who_res.RDS"))
#Save plots
#saveRDS(H1_who_plot_list, here("figure-objects/H1_who_unadj_splines.RDS"))
#Save plot data
saveRDS(H1_who_plot_data, here("figure-data/H1_who_unadj_spline_data.RDS"))

# ----
## Adjusted Models


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

H1a_who_W <- c(H1_W, 'ageday_st1', 'agedays_motor', 
           'month_st1', 'month_mm')
H1a_who_W[!(H1a_who_W %in% colnames(d))]

#Fit models
H1a_who_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_HR_GAM(d=d, X=i, Y=j, age = "agedays_motor", W=H1a_who_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1a_who_adj_models <- bind_rows(H1a_who_adj_models, res)
  }
}



#Get primary contrasts
H1a_who_adj_res <- NULL
for(i in 1:nrow(H1a_who_adj_models)){
  res <- data.frame(X=H1a_who_adj_models$X[i], Y=H1a_who_adj_models$Y[i])
  preds <- predict_gam_HR(fit=H1a_who_adj_models$fit[i][[1]], d=H1a_who_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1a_who_adj_res <-  bind_rows(H1a_who_adj_res , preds$res)
}

#Make list of plots
H1a_who_adj_plot_list <- NULL
H1a_who_adj_plot_data <- NULL
for(i in 1:nrow(H1a_who_adj_models)){
  res <- data.frame(X=H1a_who_adj_models$X[i], Y=H1a_who_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1a_who_adj_models$fit[i][[1]], H1a_who_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1a_who_adj_plot_list[[i]] <-  simul_plot$p
  H1a_who_adj_plot_data <-  rbind(H1a_who_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1a_who_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1a_who_adj_models.RDS"))

#Save results
saveRDS(H1a_who_adj_res, here("results/adjusted/H1a_who_adj_res.RDS"))


#Save plots
#saveRDS(H1a_who_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1a_who_adj_splines.RDS"))

#Save plot data
#saveRDS(H1a_who_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1a_who_adj_spline_data.RDS"))

##########################
# adjustment sets 4-5
## exposure: urine markers
## outcome: who mm
##########################

Xvars <- c('ln_L_conc_t1', 'ln_M_conc_t1')

H1b_who_W <- c(H1_W, 'ageday_ut1', 'agedays_cdi2', 
           'month_ut1', 'month_cdi2')
H1b_who_W[!(H1b_who_W %in% colnames(d))]

#Fit models
H1b_who_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_HR_GAM(d=d, X=i, Y=j, age = "agedays_motor", W=H1b_who_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1b_who_adj_models <- bind_rows(H1b_who_adj_models, res)
  }
}



#Get primary contrasts
H1b_who_adj_res <- NULL
for(i in 1:nrow(H1b_who_adj_models)){
  res <- data.frame(X=H1b_who_adj_models$X[i], Y=H1b_who_adj_models$Y[i])
  preds <- predict_gam_HR(fit=H1b_who_adj_models$fit[i][[1]], d=H1b_who_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1b_who_adj_res <-  bind_rows(H1b_who_adj_res , preds$res)
}

#Make list of plots
H1b_who_adj_plot_list <- NULL
H1b_who_adj_plot_data <- NULL
for(i in 1:nrow(H1b_who_adj_models)){
  res <- data.frame(X=H1b_who_adj_models$X[i], Y=H1b_who_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1b_who_adj_models$fit[i][[1]], H1b_who_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1b_who_adj_plot_list[[i]] <-  simul_plot$p
  H1b_who_adj_plot_data <-  rbind(H1b_who_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1b_who_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1b_who_adj_models.RDS"))

#Save results
saveRDS(H1b_who_adj_res, here("results/adjusted/H1b_who_adj_res.RDS"))


#Save plots
#saveRDS(H1b_who_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1b_who_adj_splines.RDS"))

#Save plot data
#saveRDS(H1b_who_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1b_who_adj_spline_data.RDS"))


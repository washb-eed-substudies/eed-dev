# WHO Hazard Ratios
# WASH Benefits 
# Bangladesh and Kenya

rm(list=ls())

source(here::here("0-config.R"))

# ----
# Bangladesh

<<<<<<< HEAD:src/3-who-hr-eed.R
try(d <- readRDS('eed-dev_bg.RDS'))
try(d <- readRDS("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/eed-dev_bg.RDS"))
=======
d <- readRDS('final-data/eed-dev_bg.RDS')
>>>>>>> 03f86374abe529150fa4c4386c83e11460ea2919:src/3_who-hr-eed.R

## Unadjusted 

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1',
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

#Save models
saveRDS(H1_who_models, here("models/H1_who-hr_models.RDS"))
#Save results
saveRDS(H1_who_res, here("results/unadjusted/H1_who-hr_res.RDS"))
#Save plots
#saveRDS(H1_who_plot_list, here("figure-objects/H1_who_unadj_splines.RDS"))
#Save plot data
saveRDS(H1_who_plot_data, here("figure-data/H1_who-hr_unadj_spline_data.RDS"))

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

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           

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
saveRDS(H1a_who_adj_res, here("results/adjusted/H1a_who-hr_adj_res.RDS"))


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
saveRDS(H1b_who_adj_res, here("results/adjusted/H1b_who-hr_adj_res.RDS"))


#Save plots
#saveRDS(H1b_who_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1b_who_adj_splines.RDS"))

#Save plot data
#saveRDS(H1b_who_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1b_who_adj_spline_data.RDS"))

# ----
# combine 

H1a_who_adj_res %>% 
  bind_rows(H1b_who_adj_res) %>% 
  saveRDS(here("results/adjusted/H1_who-hr_all_adj_res.RDS"))

# ------------------------------------------------------------------------
####### kenya hypotheses ####### 
# ------------------------------------------------------------------------

d <- readRDS('final-data/eed-dev_k.RDS')

# Unadjusted Models

#### Hypothesis 4 ####
# eed t1 v. dev t2 (who)

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1', 
           'ln_Lact1', 'ln_Mann1')


#Fit models
H4_who_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_HR_GAM(d=d, X=i, Y=j, age = "agedays_motor", W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H4_who_models <- bind_rows(H4_who_models, res)
  }
}

#Get primary contrasts
H4_who_res <- NULL
for(i in 1:nrow(H4_who_models)){
  res <- data.frame(X=H4_who_models$X[i], Y=H4_who_models$Y[i])
  preds <- predict_gam_HR(fit=H4_who_models$fit[i][[1]], d=H4_who_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_who_res <-  bind_rows(H4_who_res , preds$res)
}

#Make list of plots
H4_who_plot_list <- NULL
H4_who_plot_data <- NULL
for(i in 1:nrow(H4_who_models)){
  res <- data.frame(X=H4_who_models$X[i], Y=H4_who_models$Y[i])
  simul_plot <- gam_simul_CI(H4_who_models$fit[i][[1]], H4_who_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4_who_plot_list[[i]] <-  simul_plot$p
  H4_who_plot_data <-  rbind(H4_who_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

#Save models
saveRDS(H4_who_models, here("models/H4_who-hr_models.RDS"))

#Save results
saveRDS(H4_who_res, here("results/unadjusted/H4_who-hr_res.RDS"))


#Save plots
#saveRDS(H4_who_plot_list, here("figure-objects/H4_who_unadj_splines.RDS"))

#Save plot data
saveRDS(H4_who_plot_data, here("figure-data/H4_who-hr_unadj_spline_data.RDS"))


## Adjusted Models

# ------------------------------------------------------------------------
####### kenya hypotheses ####### 

d <- readRDS('final-data/eed-dev_k.RDS')

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "HHS", "Nlt18","Ncomp", "water_time", 
         "hh_index", # "floor", 'roof', 
         "tr",
         
         'laz_t1', 'waz_t1')

Wvars[!(Wvars %in% colnames(d))]

# ------------------------------------------------------------------------
#### Hypothesis 4 ####
# eed markers at t2 v. dev (who mm) at t2

H4_W <- c(Wvars,
          # 'laz_t1', 'waz_t1',
          'aged1',	'agedays_motor', 
          'month_mm')


##########################
# adjustment sets 33-35
## exposure: fecal markers t1
## outcome: who mm t2
##########################

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')

H4a_who_W <- c(H4_W, 'month_st1')
H4a_who_W[!(H4a_who_W %in% colnames(d))]

#Fit models
H4a_who_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_HR_GAM(d=d, X=i, Y=j, age = "agedays_motor", W=H4a_who_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4a_who_adj_models <- bind_rows(H4a_who_adj_models, res)
  }
}


#Get primary contrasts
H4a_who_adj_res <- NULL
for(i in 1:nrow(H4a_who_adj_models)){
  res <- data.frame(X=H4a_who_adj_models$X[i], Y=H4a_who_adj_models$Y[i])
  preds <- predict_gam_HR(fit=H4a_who_adj_models$fit[i][[1]], d=H4a_who_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4a_who_adj_res <-  bind_rows(H4a_who_adj_res , preds$res)
}

#Make list of plots
H4a_who_adj_plot_list <- NULL
H4a_who_adj_plot_data <- NULL
for(i in 1:nrow(H4a_who_adj_models)){
  res <- data.frame(X=H4a_who_adj_models$X[i], Y=H4a_who_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H4a_who_adj_models$fit[i][[1]], H4a_who_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4a_who_adj_plot_list[[i]] <-  simul_plot$p
  H4a_who_adj_plot_data <-  rbind(H4a_who_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H4a_who_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H4a_who_adj_models.RDS"))

#Save results
saveRDS(H4a_who_adj_res, here("results/adjusted/H4a_who-hr_adj_res.RDS"))


#Save plots
#saveRDS(H4a_who_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H4a_who_adj_splines.RDS"))

#Save plot data
#saveRDS(H4a_who_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H4a_who_adj_spline_data.RDS"))

##########################
# adjustment sets 36-37
## exposure: urine markers t1
## outcome: who mm t2
##########################

Xvars <- c('ln_Lact1', 'ln_Mann1')

H4b_who_W <- c(H4_W, 'month_ut1')
H4b_who_W[!(H4b_who_W %in% colnames(d))]

#Fit models
H4b_who_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(c(i, j))
    res_adj <- fit_HR_GAM(d=d, X=i, Y=j, age = "agedays_motor", W=H4b_who_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4b_who_adj_models <- bind_rows(H4b_who_adj_models, res)
  }
}


#Get primary contrasts
H4b_who_adj_res <- NULL
for(i in 1:nrow(H4b_who_adj_models)){
  res <- data.frame(X=H4b_who_adj_models$X[i], Y=H4b_who_adj_models$Y[i])
  preds <- predict_gam_HR(fit=H4b_who_adj_models$fit[i][[1]], d=H4b_who_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4b_who_adj_res <-  bind_rows(H4b_who_adj_res , preds$res)
}

#Make list of plots
H4b_who_adj_plot_list <- NULL
H4b_who_adj_plot_data <- NULL
for(i in 1:nrow(H4b_who_adj_models)){
  res <- data.frame(X=H4b_who_adj_models$X[i], Y=H4b_who_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H4b_who_adj_models$fit[i][[1]], H4b_who_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4b_who_adj_plot_list[[i]] <-  simul_plot$p
  H4b_who_adj_plot_data <-  rbind(H4b_who_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H4b_who_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H4b_who_adj_models.RDS"))

#Save results
saveRDS(H4b_who_adj_res, here("results/adjusted/H4b_who-hr_adj_res.RDS"))


#Save plots
#saveRDS(H4b_who_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H4b_who_adj_splines.RDS"))

#Save plot data
#saveRDS(H4b_who_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H4b_who_adj_spline_data.RDS"))

# ---- 
H4a_who_adj_res %>% 
  bind_rows(H4b_who_adj_res) %>% 
  saveRDS(here("results/adjusted/H4_who-hr_all_adj_res.RDS"))

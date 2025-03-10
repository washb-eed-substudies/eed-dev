rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(here('final-data/eed-dev_bg.RDS'))
# d <- readRDS("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/eed-dev_bg.RDS")

# to calculate missings for each 
# exposure-outcome pair
paired_missing <- function(Xvars = Xvars, Yvars = Yvars){
  xvar <- list()
  yvar <- list()
  missing_x  <- list()
  missing_y  <- list()
  missing_or  <- list()
  total <- list()
  for(x in Xvars){
    for(y in Yvars){
      
      xvar = c(xvar, x)
      yvar = c(yvar, y)
      missing_x = c(missing_x, sum(is.na(d[x])))
      missing_y = c(missing_y, sum(is.na(d[y])))
      missing_or = c(missing_or, sum(is.na(d[x]) | is.na(d[y])))
      total = c(total, max(dim(d[x][1]), dim(d[y][1])))
    }
  }
  df <- data.frame(list('xvar' = unlist(xvar), 'yvar' = unlist(yvar), 
                        'missing_x' = unlist(missing_x),  
                        'missing_y' = unlist(missing_y), 
                        'missing_one' = unlist(missing_or), 'total' = unlist(total)))
  df <- mutate(df, percent = round(missing_one/total, 3))
  
  return(df)
}

#Example:

#Fit GAM model with random effects for childid
#res_unadj <- fit_RE_gam(d=d, X="t3_cort_z01", Y="laz_t3",  W=NULL)

#Get predictions of differences from the 25th percentile of exposure
#preds_unadj <- predict_gam_diff(fit=res_unadj$fit, d=res_unadj$dat, quantile_diff=c(0.25,0.75), Xvar="delta_TS", Yvar="laz_t3")


#Primary parameter we are estimating: difference between 25th and 75th percentile of the exposure
#preds_unadj$res

#Plot the difference from the 25th percentile for the full range of the exposure:
#NOTE: not making these plots anymore, just using for diagnostics
#p <- plot_gam_diff(preds_unadj$plotdf)
#print(p)

#Fit spline with simultaneous confidence intervals
#simul_plot <- gam_simul_CI(res_unadj$fit, res_unadj$dat, xlab="delta_TS", ylab="laz_t3", title="example title")
#simul_plot$p


#Loop over exposure-outcome pairs

#### Hypothesis 1 ####
# eed markers at t1 v. dev (who, cdi2) at t2
Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1', 
           'ln_L_conc_t1', 'ln_M_conc_t1')            
Yvars <- c('who_sum_total', 'who_sub_total',
           'z_age2mo_cdi_undyr1_all_no4', 
           'z_age2mo_cdi_sayyr1_all_no4')



h1_missing <- paired_missing(Xvars, Yvars)


#Fit models
H1_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1_models <- bind_rows(H1_models, res)
  }
}

#Get primary contrasts
H1_res <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_res <-  bind_rows(H1_res , preds$res)
}

#Make list of plots
H1_plot_list <- NULL
H1_plot_data <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  simul_plot <- gam_simul_CI(H1_models$fit[i][[1]], H1_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_plot_list[[i]] <-  simul_plot$p
  H1_plot_data <-  rbind(H1_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H1_models, here("models/H1_models.RDS"))

#Save results
saveRDS(H1_res, here("results/unadjusted/H1_res.RDS"))


#Save plots
#saveRDS(H1_plot_list, here("figure-objects/H1_unadj_splines.RDS"))

#Save plot data
saveRDS(H1_plot_data, here("figure-data/H1_unadj_spline_data.RDS"))

# ----------------------------------------------------------------------
#### Hypothesis 2 ####
# eed markers at t1/t2 v. dev (cdi3, easq) at t3
Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1', 
           'ln_L_conc_t1', 'ln_M_conc_t1',
           'ln_aat2', 'ln_mpo2', 'ln_neo2', 
           'ln_L_conc_t2', 'ln_M_conc_t2')            
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", "z_age2mo_combined_no4", "z_age2mo_com_no4", 
           "z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4") 

h2_missing <- paired_missing(Xvars, Yvars)

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

#Save models
saveRDS(H2_models, here("models/H2_models.RDS"))

#Save results
saveRDS(H2_res, here("results/unadjusted/H2_res.RDS"))


#Save plots
#saveRDS(H2_plot_list, here("figure-objects/H2_unadj_splines.RDS"))

#Save plot data
saveRDS(H2_plot_data, here("figure-data/H2_unadj_spline_data.RDS"))


#### Hypothesis 3 ####
# reg1b at t2 v. dev (cdi3, easq) at time 3
Xvars <- c("ln_reg2")            
Yvars <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", "z_age2mo_combined_no4", "z_age2mo_com_no4", 
           "z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4") 

h3_missing <- paired_missing(Xvars, Yvars)


#Fit models
H3_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}

#Get primary contrasts
H3_res <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  preds <- predict_gam_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_res <-  bind_rows(H3_res , preds$res)
}

#Make list of plots
H3_plot_list <- NULL
H3_plot_data <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  simul_plot <- gam_simul_CI(H3_models$fit[i][[1]], H3_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_plot_list[[i]] <-  simul_plot$p
  H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

#Save models
saveRDS(H3_models, here("models/H3_models.RDS"))

#Save results
saveRDS(H3_res, here("results/unadjusted/H3_res.RDS"))


#Save plots
#saveRDS(H3_plot_list, here("figure-objects/H3_unadj_splines.RDS"))

#Save plot data
saveRDS(H3_plot_data, here("figure-data/H3_unadj_spline_data.RDS"))

# ------------------------------------------------------------------------
####### kenya hypotheses ####### 
# ------------------------------------------------------------------------

d <- readRDS(here('final-data/eed-dev_k.RDS'))

#### Hypothesis 4 ####
# eed t1 v. dev t2 (who)

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1', 
           'ln_Lact1', 'ln_Mann1')            
Yvars <- c('who_sum_total', 'who_sub_total')


#Fit models
H4_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H4_models <- bind_rows(H4_models, res)
  }
}

#Get primary contrasts
H4_res <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  preds <- predict_gam_diff(fit=H4_models$fit[i][[1]], d=H4_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_res <-  bind_rows(H4_res , preds$res)
}

#Make list of plots
H4_plot_list <- NULL
H4_plot_data <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  simul_plot <- gam_simul_CI(H4_models$fit[i][[1]], H4_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4_plot_list[[i]] <-  simul_plot$p
  H4_plot_data <-  rbind(H4_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H4_models, here("models/H4_models.RDS"))

#Save results
saveRDS(H4_res, here("results/unadjusted/H4_res.RDS"))


#Save plots
#saveRDS(H4_plot_list, here("figure-objects/H4_unadj_splines.RDS"))

#Save plot data
saveRDS(H4_plot_data, here("figure-data/H4_unadj_spline_data.RDS"))


#### Hypothesis 5 ####
# eed t1/t2 v. dev t3 (easq)

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1', 
           'ln_Lact1', 'ln_Mann1',
           'ln_aat2', 'ln_mpo2', 'ln_neo2', 
           'ln_Lact2', 'ln_Mann2')            
Yvars <- c("z_comtot_no4_activec", "z_mottot_no4_activec", "z_pstot_no4_activec", "z_globaltot_no4_activec")


#Fit models
H5_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H5_models <- bind_rows(H5_models, res)
  }
}

#Get primary contrasts
H5_res <- NULL
for(i in 1:nrow(H5_models)){
  res <- data.frame(X=H5_models$X[i], Y=H5_models$Y[i])
  preds <- predict_gam_diff(fit=H5_models$fit[i][[1]], d=H5_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H5_res <-  bind_rows(H5_res , preds$res)
}

#Make list of plots
H5_plot_list <- NULL
H5_plot_data <- NULL
for(i in 1:nrow(H5_models)){
  res <- data.frame(X=H5_models$X[i], Y=H5_models$Y[i])
  simul_plot <- gam_simul_CI(H5_models$fit[i][[1]], H5_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H5_plot_list[[i]] <-  simul_plot$p
  H5_plot_data <-  rbind(H5_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H5_models, here("models/H5_models.RDS"))

#Save results
saveRDS(H5_res, here("results/unadjusted/H5_res.RDS"))


#Save plots
#saveRDS(H5_plot_list, here("figure-objects/H5_unadj_splines.RDS"))

#Save plot data
saveRDS(H5_plot_data, here("figure-data/H5_unadj_spline_data.RDS"))


# ---------------------------------------------------
# export missing exp-outcome counts

writexl::write_xlsx(list('eed_t1-dev_t2' = h1_missing, 
                         'eed_t2-dev_t23' = h2_missing, 
                         'reg1b_t2-dev_t23' = h3_missing), 
                    path = 'bg_missing_counts.xlsx')

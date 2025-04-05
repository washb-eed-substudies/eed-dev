

#-------------------------------------------------------------------------------
# Merge STH data
#-------------------------------------------------------------------------------
rm(list=ls())

source(here::here("0-config.R"))
library(haven)

d_bd <- readRDS(here('final-data/eed-dev_bg.RDS'))
bd_public_ID <- read_csv("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/public-ids.csv")

#clean covariates to avoid high missingness
d_bd <- d_bd %>% mutate(
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


#  block clusterid dataid clusterid_r dataid_r block_r
sth_bd_cov <- read_csv("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/sth data/14mo qPCR Data/df.cov.csv") %>%
  select(childid, dataid, block, clusterid) 
head(sth_bd_cov)
sth_bd <- read_csv("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/sth data/14mo qPCR Data/pathogen_log10_quantity.csv") %>%
  mutate(ascaris=ifelse(Ascaris==0, 0, 1),
         trichuris=ifelse(Trichuris==0, 0, 1),
         hookworm=ifelse(Ancyclostoma==0 & Necator==0, 0, 1),
         anySTH=ifelse(Ascaris==1 | Trichuris==1 | hookworm==1, 1, 0),
         flag=1) %>%
  select(childid, ascaris, trichuris, hookworm, anySTH,flag) #%>%
  #mutate((childid + 3252) * 10) 
  #mutate(childid=(childid/10) - 3252) 
head(sth_bd)
sth_bd <- left_join(sth_bd, sth_bd_cov, by = "childid") 

sth_bd <- sth_bd %>% rename(block_r=block, clusterid_r=clusterid, dataid_r=dataid) 


# gen double hhidr2 = (hhid + 3252)*10
# gen double childidr2 = (childid + 3252) * 10
# gen double clusteridr2 = (clusterid + 3252) * 10


head(d_bd)
head(sth_bd)
head(bd_public_ID)

sth_bd <- left_join(sth_bd, bd_public_ID, by = c("block_r","dataid_r","clusterid_r")) 
head(sth_bd)
d_bd$dataid <- as.character(d_bd$dataid)
d_bd$clusterid <- as.character(d_bd$clusterid)
d_bd$childid[1:10]
sth_bd$childid[1:10]
summary(d_bd$childid)
summary(sth_bd$childid)
summary(sth_bd$childid_r)

summary(as.numeric(d_bd$dataid))
summary(as.numeric(sth_bd$dataid))
summary(sth_bd$dataid_r)


#sth_bd <- sth_bd %>% select(block, dataid,  clusterid, childid, aged, alepg, hwepg, ttepg) %>% rename(sth_aged=aged)

dim(d_bd)
dim(sth_bd)
d <- left_join(d_bd, sth_bd, by = c("block","dataid","clusterid"))
dim(d)
table(d$ascaris)

d$clusterid <- as.numeric(d$clusterid)




Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", 'roof', "HHwealth", 
         "life_viol_any_t3", "tr",
         'laz_t1', 'waz_t1', "cesd_sum_t2")
H1_W <- c(Wvars)
Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars <- c('who_sum_total', 'who_sub_total')
H1a_W <- c(H1_W, 'ageday_st1', 'agedays_motor', 
           'month_st1', 'month_mm')
H1a_W[!(H1a_W %in% colnames(d))]


#------------------------------------------------------------------------------
# EMM function
#------------------------------------------------------------------------------
  

get_emm_res <- function(d, Xvars, Yvars, Wvars, Vvar){
 
  #Fit models
  H1a_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      for(k in Vvar){
      print(i)
      print(j) 
      res=NULL
      try(res <- fit_RE_gam(d=d, X=i, Y=j,  W=Wvars,  forcedW=NULL, V=k))
      if(is.null(res)){
        try(res <- fit_RE_gam(d=d, X=i, Y=j,  W=Wvars,  forcedW=NULL, V=k, vim=FALSE))
      }
      res <- data.frame(X=i, Y=j, V=k, int.p =res$int.p, fit=I(list(res$fit)), dat=I(list(res$dat)))
      H1a_models <- bind_rows(H1a_models, res)
      }
    }
  }
  
  
  
  
  #Get primary contrasts
  H1_res <- NULL
  for(i in 1:nrow(H1a_models)){
    res <- data.frame(X=H1a_models$X[i], Y=H1a_models$Y[i])
    if(grepl("_def", H1a_models$X[i])){
      preds <- predict_gam_emm(fit=H1a_models$fit[i][[1]], d=H1a_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=H1a_models$X[i], Yvar=H1a_models$Y[i], binaryX=T)
    }else{
      preds <- predict_gam_emm(fit=H1a_models$fit[i][[1]], d=H1a_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=H1a_models$X[i], Yvar=H1a_models$Y[i])
    }
    gamm_diff_res <- data.frame(V=H1a_models$V[i] , preds$res) 
    
    if(nrow(gamm_diff_res) == 2){
      gamm_diff_res <- gamm_diff_res %>% mutate(int.Pval = c(NA, H1a_models$int.p[i]))
    }else{
      gamm_diff_res$int.Pval=H1a_models$int.p[i]
    }
    
    H1_res <-  bind_rows(H1_res , gamm_diff_res)
  }
  
  
  return(H1_res) 
}


#------------------------------------------------------------------------------
# Set up list of analyses
#------------------------------------------------------------------------------


Wvars = c("sex","birthord", "momage","momheight","momedu", 
          "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
          "floor", 'roof', "HHwealth", 
          "life_viol_any_t3",
          'laz_t1', 'waz_t1', "cesd_sum_t2")



H1_W <- c(Wvars)
Xvar=c("ln_M_conc_t1","ln_M_conc_t2","ln_M_conc_t3")
Yvar =  c('z_age2mo_personal_no4')
Vvars= c("tr")

Xvars_H1a <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars_H1a <- c('who_sum_total', 'who_sub_total')
Wvars_H1a <- H1a_W <- c(H1_W, 'ageday_st1', 'agedays_motor', 'month_st1', 'month_mm')

Xvars_H1b <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars_H1b <- c('who_sum_total', 'who_sub_total')
Wvars_H1b <- H1b_W <- c(H1_W, 'ageday_ut1', 'agedays_cdi2', 'month_ut1', 'month_cdi2')

Xvars_H1c <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars_H1c <- c('z_age2mo_cdi_undyr1_all_no4', 
               'z_age2mo_cdi_sayyr1_all_no4')
Wvars_H1c <- H1c_W <- c(H1_W, 'ageday_st1', 'agedays_cdi2', 'month_st1', 'month_cdi2')

Xvars_H1d <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars_H1d <- c('z_age2mo_cdi_undyr1_all_no4', 
               'z_age2mo_cdi_sayyr1_all_no4')
Wvars_H1d <-H1d_W <- c(H1_W, 'ageday_ut1', 'agedays_cdi2', 'month_ut1', 'month_cdi2')

H_list <- list()
H_list$H1a <- list(Xvars_H1a, Yvars_H1b, Wvars_H1c)
H_list$H1b <- list(Xvars_H1b, Yvars_H1b, Wvars_H1b)
H_list$H1c <- list(Xvars_H1c, Yvars_H1c, Wvars_H1c)
H_list$H1d <- list(Xvars_H1d, Yvars_H1d, Wvars_H1d)


H2_W <- c(Wvars, 'laz_t2', 'waz_t2',
          'cesd_sum_ee_t3',	'pss_sum_mom_t3')

Xvars_H2a <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars_H2a <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", "z_age2mo_combined_no4", "z_age2mo_com_no4")
Wvars_H2a <- H2a_W <- c(H2_W, 'ageday_st1',	'agedays_easq', 'month_st1',	'month_easq')

Xvars_H2b <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars_H2b <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", "z_age2mo_combined_no4", "z_age2mo_com_no4")
Wvars_H2b <- H2b_W <- c(H2_W, 'ageday_ut1',	'agedays_easq', 'month_ut1',	'month_easq')

Xvars_H2c <- c('ln_aat1', 'ln_mpo1', 'ln_neo1')           
Yvars_H2c <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")
Wvars_H2c <- H2c_W <- c(H2_W, 'ageday_st1', 'agedays_cdi3','month_st1', 'month_cdi3')

Xvars_H2d <- c('ln_L_conc_t1', 'ln_M_conc_t1')           
Yvars_H2d <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")
Wvars_H2d <- H2d_W <- c(H2_W, 'ageday_ut1', 'agedays_cdi3','month_ut1', 'month_cdi3')

Xvars_H2e <- c('ln_aat2', 'ln_mpo2', 'ln_neo2')           
Yvars_H2e <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", "z_age2mo_combined_no4", "z_age2mo_com_no4")
Wvars_H2e <- H2e_W <- c(H2_W, 'ageday_st2', 'agedays_easq', 'month_st2',	'month_easq')

Xvars_H2f <- c('ln_L_conc_t2', 'ln_M_conc_t2')           
Yvars_H2f <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", "z_age2mo_combined_no4", "z_age2mo_com_no4")
Wvars_H2f <- H2f_W <- c(H2_W, 'ageday_ut2', 'agedays_easq', 'month_ut2',	'month_easq')

Xvars_H2g <- c('ln_aat2', 'ln_mpo2', 'ln_neo2')           
Yvars_H2g <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")
Wvars_H2g <- H2g_W <- c(H2_W, 'ageday_st2', 'agedays_cdi3','month_st2', 'month_cdi3')
Xvars_H2h <- c('ln_L_conc_t2', 'ln_M_conc_t2')           
Yvars_H2h <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")
Wvars_H2h <- H2h_W <- c(H2_W, 'ageday_ut2', 'agedays_cdi3','month_ut2', 'month_cdi3')

H_list$H2a <- list(Xvars_H2a, Yvars_H2a, Wvars_H2a)
H_list$H2b <- list(Xvars_H2b, Yvars_H2b, Wvars_H2b)
H_list$H2c <- list(Xvars_H2c, Yvars_H2c, Wvars_H2c)
H_list$H2d <- list(Xvars_H2d, Yvars_H2d, Wvars_H2d)
H_list$H2e <- list(Xvars_H2e, Yvars_H2e, Wvars_H2e)
H_list$H2f <- list(Xvars_H2f, Yvars_H2f, Wvars_H2f)
H_list$H2g <- list(Xvars_H2g, Yvars_H2g, Wvars_H2g)
H_list$H2h <- list(Xvars_H2h, Yvars_H2h, Wvars_H2h)

H3_W <- c(Wvars, 'laz_t2', 'waz_t2','cesd_sum_ee_t3',	'pss_sum_mom_t3')


Xvars_H3a <- c('ln_reg2')           
Yvars_H3a <- c("z_age2mo_personal_no4", "z_age2mo_motor_no4", "z_age2mo_combined_no4", "z_age2mo_com_no4")
H3a_W <- c(H3_W, 'ageday_st2', 'agedays_easq','month_st2', 'month_easq')
Xvars_H3b <- c('ln_reg2')           
Yvars_H3b <- c("z_age2mo_cdi_undyr2_all_no4", "z_age2mo_cdi_sayyr2_all_no4")
H3b_W <- c(H3_W, 'ageday_st2', 'agedays_cdi3','month_st2', 'month_cdi3')

H_list$H3a <- list(Xvars_H3a, Yvars_H3a, H3a_W)
H_list$H3b <- list(Xvars_H3b, Yvars_H3b, H3b_W)

#------------------------------------------------------------------------------
# Soil-transmitted helminths and multicellular pathogens evoke a primarily TH2 response. 
# Since STH were assessed in these children, you can (at minimum) compare infection rates in the two settings. 
# Furthermore, might you test if helminth positivity modified the effect of mannitol on personal social domain score?
#------------------------------------------------------------------------------

prop.table(table(d$anySTH)) *100

Xvar=c("ln_M_conc_t1","ln_M_conc_t2","ln_M_conc_t3")
Yvar =  c('z_age2mo_personal_no4')
Vvars= c("ascaris","trichuris","hookworm","anySTH")


Wvars = c("sex","birthord", "momage","momheight","momedu", 
          "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
          "floor", 'roof', "HHwealth", 
          "life_viol_any_t3", "tr",
          'laz_t1', 'waz_t1', "cesd_sum_t2")
H2_W <- c(Wvars, 'laz_t2', 'waz_t2',
          'cesd_sum_ee_t3',	'pss_sum_mom_t3')
H2a_W <- c(H2_W, 'ageday_st1',	'agedays_easq', 
           'month_st1',	'month_easq')

res_Mann=get_emm_res(d=d, Xvars=Xvar, Yvars=Yvar, Wvars=H2a_W, Vvar=Vvars)
res_Mann
res_Mann$corrected_p <-  p.adjust(res_Mann$int.Pval, "BH")

res_Mann <- res_Mann %>%
  mutate(`Any STH\ninfection` = factor(Vlevel))

head(res_Mann %>% filter(V=="anySTH"))

ggplot(res_Mann %>% filter(V=="anySTH"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff, group=`Any STH\ninfection`, colour =`Any STH\ninfection`)) +
  geom_pointrange(position = position_dodge(0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Effect of mannitol on personal social domain score by STH status",
       x="Mannitol measurement",
       y="Difference in z-score between 25th and 75th\npercentile of mannitol concentration (ln)",
       caption="95% CI")

ggplot(res_Mann , aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff, group=`Any STH\ninfection`, colour =`Any STH\ninfection`)) +
  geom_pointrange(position = position_dodge(0.5)) +
  facet_wrap(~V) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Effect of mannitol on personal social domain score by STH status",
       x="Mannitol concentration (ln)",
       y="Difference in z-score",
       caption="95% CI")




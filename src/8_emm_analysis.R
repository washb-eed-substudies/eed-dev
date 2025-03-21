

#-------------------------------------------------------------------------------
# Merge STH data
#-------------------------------------------------------------------------------
rm(list=ls())

source(here::here("0-config.R"))
library(haven)

d_bd <- readRDS(here('final-data/eed-dev_bg.RDS'))

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
sth_bd <- read_csv("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/sth data/washb-bangladesh-sth-public.csv") %>% rename(block_r=block, clusterid_r=clusterid, dataid_r=dataid) %>% filter(personid=="T1") %>% mutate(personid=as.numeric(gsub("T","",personid)))
bd_public_ID <- read_csv("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/public-ids.csv")
d_k <- readRDS(here('final-data/eed-dev_k.RDS'))

#need to merge in Kenya STH data
sth_k <- read_dta("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/sth data/parasites_kenya_public_ca20230105.dta") 
head(sth_k)
head(d_k)

sth_k <- sth_k %>% mutate(hhid=(as.numeric(hhidr2)/10)-3252,
                          childid=(as.numeric(childidr2)/10)-3252,
                          clusterid=(as.numeric(clusteridr2)/10)-3252)


# gen double hhidr2 = (hhid + 3252)*10
# gen double childidr2 = (childid + 3252) * 10
# gen double clusteridr2 = (clusterid + 3252) * 10


head(d_bd)
head(sth_bd)
head(bd_public_ID)

sth_bd <- left_join(sth_bd, bd_public_ID, by = c("block_r","dataid_r","clusterid_r")) %>% mutate(childid=as.numeric(dataid)*10+personid)
head(sth_bd)
d_bd$dataid <- as.character(d_bd$dataid)
d_bd$clusterid <- as.character(d_bd$clusterid)
d_bd$childid[1:10]
sth_bd$childid[1:10]



sth_bd <- sth_bd %>% select(block, dataid,  clusterid, childid, aged, alepg, hwepg, ttepg) %>% rename(sth_aged=aged)
sth_k <- sth_k %>% select(block, hhid,  clusterid, childid, ascaris_yn , trichuris_yn, hook_yn, sth_yn) %>%
  rename( ascaris=ascaris_yn,trichuris=trichuris_yn,hookworm=hook_yn, anySTH=sth_yn)

d <- left_join(d_bd, sth_bd, by = c("block","dataid","clusterid","childid"))
d_k <- left_join(d_k, sth_k, by = c("childid"))
d$clusterid <- as.numeric(d$clusterid)

#emm analysis
table(d$ageday_st3 < d$sth_aged)


head(d)

table(!is.na(d$alepg))
table(!is.na(d$hwepg))
table(!is.na(d$ttepg))
table(d$alepg > 0)
table(d$hwepg > 0)
table(d$ttepg > 0)

d$ascaris <- 1*(d$alepg > 0)
d$trichuris <- 1*(d$ttepg > 0)
d$hookworm <- 1*(d$hwepg > 0)
d$anySTH <- ifelse(d$ascaris==1 | d$trichuris==1 | d$hookworm==1, 1, 0)


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

Wvars_K=c("sex","birthord", "momage","momheight","momedu", 
                  "HHS", "Nlt18","Ncomp", "water_time", 
                  "floor", 'roof', "hh_index", 
                  "tr")

  
  

get_emm_res <- function(d, Xvars, Yvars, Wvars, Vvar){
 
  #Fit models
  H1a_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      for(k in Vvar){
      print(i)
      print(j) 
      res=NULL
      res <- fit_RE_gam(d=d, X=i, Y=j,  W=Wvars,  forcedW=NULL, V=k)
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

d$ln_mpo1
d$z_age2mo_personal_all

# Soil-transmitted helminths and multicellular pathogens evoke a primarily TH2 response. 
# Since STH were assessed in these children, you can (at minimum) compare infection rates in the two settings. 
# Furthermore, might you test if helminth positivity modified the effect of mannitol on personal social domain score?

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

res_Mann %>% filter(V=="anySTH")


#------------------------------------------------------------------------------
#Loop at all exposure outcome pairs, with any STH as the outcome
#------------------------------------------------------------------------------
Vvars= c("anySTH")
Yvar_BD =  c('z_age2mo_personal_no4')


res_K=get_emm_res(d=d_k, Xvars=Xvar, Yvars=Yvar, Wvars=Wvars_K, Vvar=Vvars)
res_K



table(d$ascaris)
table(d$trichuris)
table(d$hookworm)



#-------------------------------------------------------------------------------
# pre-rejistered EMM analysis
#-------------------------------------------------------------------------------

# Parental (maternal and paternal, separately) perceived stress (Bangladesh, Follow-up 3)
Xvar=c("ln_M_conc_t1","ln_M_conc_t2","ln_M_conc_t3")
Yvar =  c('z_age2mo_personal_no4')
Vvars= c("ascaris","trichuris","hookworm")

Wvars = c("sex","birthord", "momage","momheight","momedu", 
          "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
          "floor", 'roof', "HHwealth", 
          "life_viol_any_t3", "tr",
          'laz_t1', 'waz_t1', "cesd_sum_t2")
H2_W <- c(Wvars, 'laz_t2', 'waz_t2',
          'cesd_sum_ee_t3',	'pss_sum_mom_t3')
H2a_W <- c(H2_W, 'ageday_st1',	'agedays_easq', 
           'month_st1',	'month_easq')

Wvars_K<-c("sex","birthord", "momage","momheight","momedu", 
         "HHS", "Nlt18","Ncomp", "water_time", 
         "hh_index", # "floor", 'roof', 
         "tr",
         
         'laz_t1', 'waz_t1')



res=get_emm_res(d=d, Xvars=Xvar, Yvars=Yvar, Wvars=H2a_W, Vvar=Vvars)
res

res_K=get_emm_res(d=d_k, Xvars=Xvar, Yvars=Yvar, Wvars=Wvars_K, Vvar=Vvars)
res_K

# Maternal perceived stress (Kenya, Follow-up 3)Maternal depressive symptoms(Bangladesh and Kenya, Follow-up 2 and Follow-up 3)
# Maternal cortisol (Bangladesh only, measured during pregnancy)
# Child stress (pre-stressor cortisol, post-stressor cortisol, cortisol reactivity, and F2-isoprostanes) (Bangladesh only, Follow-up 3)
# Cumulative maternal exposure to intimate partner violenceat Follow-up 2and Follow-up 3(Bangladesh only)
# Stimulation in the home as measured by the stimulation subscale (number of stimulation activities the mother, father, or any other caregiver over age 15 years has completed with the child in the past 3 days) in the Family CareIndicators (FCI) questionnaire21
# administered at Follow-up 2and Follow-up 3in Bangladesh and Kenya. 
# It will be defined as the total number of activities an adult participated in, with a maximum score of 6.


# We will assess effect modification by including interaction terms between the proposed modifiers 
# and each EED marker within generalized additive models(GAMs). 
# We will compare GAMs with interaction terms to GAMs with only main effects 
# for the EED marker and the modifier variable using likelihood-ratio tests. 
# We will use the p-value from the likelihood-ratio test to assess the 
# significance of effect modification(p-value<0.2). 
# We will report the predicted difference between the 
# 1st and 3rd quartile of each EED marker when setting 
# the value of the effect modifier variable to the 1st and 3rd 
# quartile of its distribution (for continuous modifiers) or to 0 or 1 
# (for binary variables) for all children. We consider these effect-modification 
# analyses as pre-specified exploratory analyses



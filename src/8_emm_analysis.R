

#-------------------------------------------------------------------------------
# Merge STH data
#-------------------------------------------------------------------------------
rm(list=ls())

source(here::here("0-config.R"))

d_bd <- readRDS(here('final-data/eed-dev_bg.RDS'))

#  block clusterid dataid clusterid_r dataid_r block_r
sth_bd <- read_csv("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/sth data/washb-bangladesh-sth-public.csv") %>% rename(block_r=block, clusterid_r=clusterid, dataid_r=dataid) %>% filter(personid=="T1") %>% mutate(personid=as.numeric(gsub("T","",personid)))
bd_public_ID <- read_csv("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/public-ids.csv")
d_k <- readRDS(here('final-data/eed-dev_k.RDS'))

head(d_bd)
head(sth_bd)
head(bd_public_ID)

sth_bd <- left_join(sth_bd, bd_public_ID, by = c("block_r","dataid_r","clusterid_r")) %>% mutate(childid=as.numeric(dataid)*10+personid)
head(sth_bd)
d_bd$dataid <- as.character(d_bd$dataid)
d_bd$clusterid <- as.character(d_bd$clusterid)
d_bd$childid[1:10]
sth_bd$childid[1:10]

sth_bd <- sth_bd %>% select(block, dataid, clusterid, childid, alepg, hwepg, ttepg)

d <- left_join(d_bd, sth_bd, by = c("block","dataid","clusterid","childid"))
head(d)

table(!is.na(d$alepg))
table(!is.na(d$hwepg))
table(!is.na(d$ttepg))
table(d$alepg > 0)
table(d$hwepg > 0)
table(d$ttepg > 0)

d$ascaris <- 1*(d$alepg > 0)
table(d$ascaris )


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



#Fit models
H1a_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j) 
    res <- fit_RE_gam(d=d, X=i, Y=j,  W=H1a_W,  forcedW="sex", V="sex")
    #modelfit <- data.frame(est = res$boot.coefs[2], ci.lb = res$percentile.interval[2, 1], ci.ub = res$percentile.interval[2, 2], se = res$boot.sds[2])
    
    res$X <- i
    res$Y <- j
    H1a_adj_res <- bind_rows(H1a_adj_res, res)
  }
}

res_adj <- fit_RE_gam(d=d_sth, X="", Y=j,  W=NULL, V="ascaris")





# Parental (maternal and paternal, separately) perceived stress (Bangladesh, Follow-up 3)
# Maternal perceived stress (Kenya, Follow-up 3)●Maternal depressive symptoms(Bangladesh and Kenya, Follow-up 2 and Follow-up 3)
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


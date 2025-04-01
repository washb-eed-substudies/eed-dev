rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0_glm_functions.R"))
library(washbgam)

d <- readRDS(here('final-data/eed-dev_bg.RDS'))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", 'roof', "HHwealth", 
         "life_viol_any_t3", "tr",
         'laz_t1', 'waz_t1', "cesd_sum_t2")

table(d$laz_t1)
table(d$cesd_sum_t2)

d <- d %>% mutate(
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
  
                  

table(d$cesd_sum_t2)
table(d$waz_t1)
d$laz_t1

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


class(d$cesd_sum_ee_t3)

#Fit models
H1a_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j) 
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H1a_W)
    #modelfit <- data.frame(est = res$boot.coefs[2], ci.lb = res$percentile.interval[2, 1], ci.ub = res$percentile.interval[2, 2], se = res$boot.sds[2])
    
    res$X <- i
    res$Y <- j
    H1a_adj_res <- bind_rows(H1a_adj_res, res)
  }
}

#Save results
saveRDS(H1a_adj_res, here("results/adjusted/H1a_glm_boot_res.RDS"))

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
H1b_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H1b_W)
    res$X <- i
    res$Y <- j
    H1b_adj_res <- bind_rows(H1b_adj_res, res)
  }
}

#Save results
saveRDS(H1b_adj_res, here("results/adjusted/H1b_glm_boot_res.RDS"))



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
H1c_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H1c_W)
    res$X <- i
    res$Y <- j
    H1c_adj_res <- bind_rows(H1c_adj_res, res)
  }
}

#Save results
saveRDS(H1c_adj_res, here("results/adjusted/H1c_glm_boot_res.RDS"))

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
H1d_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H1d_W)
    res$X <- i
    res$Y <- j
    H1d_adj_res <- bind_rows(H1d_adj_res, res)
  }
}


#Save results
saveRDS(H1d_adj_res, here("results/adjusted/H1d_glm_boot_res.RDS"))




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
H2a_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H2a_W)
    res$X <- i
    res$Y <- j
    H2a_adj_res <- bind_rows(H2a_adj_res, res)
  }
}


#Save results
saveRDS(H2a_adj_res, here("results/adjusted/H2a_glm_boot_res.RDS"))


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
H2b_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H2b_W)
    res$X <- i
    res$Y <- j
    H2b_adj_res <- bind_rows(H2b_adj_res, res)
  }
}


#Save results
saveRDS(H2b_adj_res, here("results/adjusted/H2b_glm_boot_res.RDS"))

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
H2c_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H2c_W)
    res$X <- i
    res$Y <- j
    H2c_adj_res <- bind_rows(H2c_adj_res, res)
  }
}


#Save results
saveRDS(H2c_adj_res, here("results/adjusted/H2c_glm_boot_res.RDS"))


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
H2d_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H2d_W)
    res$X <- i
    res$Y <- j
    H2d_adj_res <- bind_rows(H2d_adj_res, res)
  }
}



#Save results
saveRDS(H2d_adj_res, here("results/adjusted/H2d_glm_boot_res.RDS"))



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
H2e_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H2e_W)
    res$X <- i
    res$Y <- j
    H2e_adj_res <- bind_rows(H2e_adj_res, res)
  }
}


#Save results
saveRDS(H2e_adj_res, here("results/adjusted/H2e_glm_boot_res.RDS"))

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
H2f_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H2f_W)
    res$X <- i
    res$Y <- j
    H2f_adj_res <- bind_rows(H2f_adj_res, res)
  }
}

#Save results
saveRDS(H2f_adj_res, here("results/adjusted/H2f_glm_boot_res.RDS"))




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
H2g_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res <- fit_cowboy_glm(data=d, X=i, Y=j,  W=H2g_W)
    res$X <- i
    res$Y <- j
    H2g_adj_res <- bind_rows(H2g_adj_res, res)
  }
}



#Save results
saveRDS(H2g_adj_res, here("results/adjusted/H2g_glm_boot_res.RDS"))


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
H2h_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H2h_W)
    res$X <- i
    res$Y <- j
    H2h_adj_res <- bind_rows(H2h_adj_res, res)
  }
}



#Save results
saveRDS(H2h_adj_res, here("results/adjusted/H2h_glm_boot_res.RDS"))


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
H3a_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H3a_W)
    res$X <- i
    res$Y <- j
    H3a_adj_res <- bind_rows(H3a_adj_res, res)
  }
}



#Save results
saveRDS(H3a_adj_res, here("results/adjusted/H3a_glm_boot_res.RDS"))


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
H3b_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H3b_W)
    res$X <- i
    res$Y <- j
    H3b_adj_res <- bind_rows(H3b_adj_res, res)
  }
}



#Save results
saveRDS(H3b_adj_res, here("results/adjusted/H3b_glm_boot_res.RDS"))


# ------------------------------------------------------------------------
####### kenya hypotheses ####### 

d <- readRDS(here('final-data/eed-dev_k.RDS'))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "HHS", "Nlt18","Ncomp", "water_time", 
         "floor", 'roof', "hh_index", 
         "tr")

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

i=Xvars[1]
j=Yvars[1]
#Fit models
H4a_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res=NULL
    try(res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H4a_W))
    res$X <- i
    res$Y <- j
    H4a_adj_res <- bind_rows(H4a_adj_res, res)
  }
}


#Save results
saveRDS(H4a_adj_res, here("results/adjusted/H4a_glm_boot_res.RDS"))


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
H4b_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res=NULL
    try(res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H4b_W))
    res$X <- i
    res$Y <- j
    H4b_adj_res <- bind_rows(H4b_adj_res, res)
  }
}



#Save results
saveRDS(H4b_adj_res, here("results/adjusted/H4b_glm_boot_res.RDS"))

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
H5a_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res=NULL
    try(res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H5a_W))
    res$X <- i
    res$Y <- j
    H5a_adj_res <- bind_rows(H5a_adj_res, res)
  }
}



#Save results
saveRDS(H5a_adj_res, here("results/adjusted/H5a_glm_boot_res.RDS"))


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
H5b_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res=NULL
    try(res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H5b_W))
    res$X <- i
    res$Y <- j
    H5b_adj_res <- bind_rows(H5b_adj_res, res)
  }
}


#Save results
saveRDS(H5b_adj_res, here("results/adjusted/H5b_glm_boot_res.RDS"))


##########################
# adjustment sets 43-45
## exposure: fecal markers t2
## outcome: easq t3
##########################

Xvars <- c('ln_aat2', 'ln_mpo2', 'ln_neo2')           
Yvars <- c("z_comtot_no4_activec", "z_mottot_no4_activec", "z_pstot_no4_activec", "z_globaltot_no4_activec")

H5c_W <- c(H5_W, 'aged2', 'month_st2')
H5c_W[!(H5c_W %in% colnames(d))]

#Fit models
H5c_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res=NULL
    try(res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H5c_W))
    res$X <- i
    res$Y <- j
    H5c_adj_res <- bind_rows(H5c_adj_res, res)
  }
}




#Save results
saveRDS(H5c_adj_res, here("results/adjusted/H5c_glm_boot_res.RDS"))


##########################
# adjustment sets 46-47
## exposure: urine markers t2
## outcome: easq t3
##########################

Xvars <- c('ln_Lact2', 'ln_Mann2')           
Yvars <- c("z_comtot_no4_activec", "z_mottot_no4_activec", "z_pstot_no4_activec", "z_globaltot_no4_activec")

H5d_W <- c(H5_W, 'aged2', 'month_ut2')
H5d_W[!(H5d_W %in% colnames(d))]

#Fit models
H5d_adj_res <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res=NULL
    try(res<- fit_cowboy_glm(data=d, X=i, Y=j,  W=H5d_W))
    res$X <- i
    res$Y <- j
    H5d_adj_res <- bind_rows(H5d_adj_res, res)
  }
}

#Save results
saveRDS(H5d_adj_res, here("results/adjusted/H5d_glm_boot_res.RDS"))



# --------------------------------------------------------------------------
# combine dataframes for each hypothesis

# get object names
for(hyp in paste0("H", 1:3)){
  print(str_subset(ls(), str_glue("^{hyp}.*res$")))
}

bind_rows(H1a_adj_res, H1b_adj_res, H1c_adj_res, H1d_adj_res) %>%
  saveRDS("results/adjusted/H1_all_glm_boot_res.RDS")

bind_rows(H2a_adj_res, H2b_adj_res, H2c_adj_res, 
          H2d_adj_res, H2e_adj_res, H2f_adj_res,
          H2g_adj_res, H2h_adj_res) %>%
  saveRDS("results/adjusted/H2_all_glm_boot_res.RDS")

bind_rows(H3a_adj_res, H3b_adj_res) %>% 
  saveRDS("results/adjusted/H3_all_glm_boot_res.RDS")

bind_rows(H4a_adj_res, H4b_adj_res) %>%
  saveRDS("results/adjusted/H4_all_glm_boot_res.RDS")

bind_rows(H5a_adj_res, H5b_adj_res, H5c_adj_res, 
          H5d_adj_res) %>%
  saveRDS("results/adjusted/H5_all_glm_boot_res.RDS")












#-------------------------------------------------------------------------------
# Merge STH data
#-------------------------------------------------------------------------------
rm(list=ls())

source(here::here("0-config.R"))

d_bd <- readRDS(here('final-data/eed-dev_bg.RDS'))
sth_bd <- read_csv("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/sth data/washb-bangladesh-sth-public.csv")
bd_public_ID <- read_csv("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/public-ids.csv")
d_k <- readRDS(here('final-data/eed-dev_k.RDS'))

head(d_bd)
head(sth_bd)

summary(d_bd$childid)
summary(d_bd$dataid)
summary(d_bd$clusterid)

head(bd_public_ID)

#need to convert public IDs (check Jades emails to get kenya conversion)

#d_bd is private IDs


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


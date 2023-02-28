library(haven)
library(dplyr)
library(ggplot2)
library(here)
library(stringr)
library(boxr)
library(readr)

bg <- read_rds("final-data/eed-dev_bg.RDS")

bf_bl <- read_dta("raw-data/bangladesh/ffq/EE_Baseline_FFQ_Raw_data_13Sep2017_deidentified.dta", 
                  col_select = c(contains("id"), starts_with("child"),
                                 c607, c607a, 
                                 starts_with("c608"),
                                 starts_with("c609")))
bf_ml <- read_dta("raw-data/bangladesh/ffq/EE_Midline_FFQ_Raw_data_28Sep2017_deidentified.dta", 
                  col_select = c(contains("id"), starts_with("child"),
                                 c607, c607a, 
                                 starts_with("c608"),
                                 starts_with("c609")))
bf_el <- read_dta("raw-data/bangladesh/ffq/EE_Endline_FFQ_raw_data_07March2017_deidentified.dta", 
                  col_select = c(contains("id"), starts_with("child"),
                                 c607, c607a, 
                                 starts_with("c608"),
                                 starts_with("c609")))

labelled::lookfor(bf_el) %>% 
  View()


# diff number of columns used c608_12_2
# compared to Caitlin's file

ebf_bl <- bf_bl %>% 
  rowwise() %>% 
  mutate(childid = as.numeric(paste0(dataid, childId)), 
         ebf_bl = all(c607>0, c607a==5, c608_1_3==2, c608_2_3==2, 
                       c608_3_3==2, c608_4_3==2, c608_5_3==2, 
                       c608_6_3==2, c608_7_3==2, c608_8_3==2, 
                       c608_9_3==2, c608_10_3==2, c608_11_3==2, 
                       c608_12_3==2, c609_1_1==2, c609_2_1==2,
                       c609_2_2==2, c609_2_3==2, c609_2_4==2, 
                       c609_2_5==2, c609_3_1==2, c609_3_2==2, 
                       c609_3_3==2, c609_4_1==2, c609_4_2==2, 
                       c609_5_1==2, c609_5_2==2, c609_5_3==2, 
                       c609_5_4==2, c609_5_5==2, c609_6_1==2, 
                       c609_6_2==2, c609_7_1==2, c609_7_2==2, 
                       c609_7_3==2, c609_7_4==2, c609_7_5==2, 
                       c609_7_6==2, c609_7_7==2, c609_8_1==2, 
                       c609_8_2==2, c609_8_3==2, c609_8_4==2,
                       c609_8_5==2, c609_8_6==2, c609_9_1==2, 
                       c609_9_2==2, c609_9_3==2, c609_10_1==2, 
                       c609_11_1==2, c609_12_1==2, c609_12_2==2, 
                       c609_12_3==2, c609_13_1==2, c609_13_2==2, 
                       c609_13_3==2, c609_13_4==2, c609_13_5==2, 
                       c609_13_6==2, c609_13_7==2, c609_14_1==2, 
                       c609_14_2==2, c609_14_3==2, c609_15_1==2, 
                       c609_15_2==2, c609_15_3==2, c609_16_1==2, 
                       c609_16_2==2, c609_16_3==2, c609_16_4==2, 
                       c609_16_5==2, c609_17_1==2, c609_17_2==2, 
                       c609_17_3==2, c609_17_4==2, c609_18_1==2, 
                       c609_18_2==2)) %>% 
  select(contains("childid"), ebf_bl)

ebf_ml <- bf_ml %>% 
  rowwise() %>% 
  mutate(childid = as.numeric(paste0(dataid, childId)), 
         ebf_ml = all(c607>0, c607a==5, c608_1_3==2, c608_2_3==2, 
                   c608_3_3==2, c608_4_3==2, c608_5_3==2, 
                   c608_6_3==2, c608_7_3==2, c608_8_3==2, 
                   c608_9_3==2, c608_10_3==2, c608_11_3==2, 
                   c608_12_3==2, c609_1_1==2, c609_2_1==2,
                   c609_2_2==2, c609_2_3==2, c609_2_4==2, 
                   c609_2_5==2, c609_3_1==2, c609_3_2==2, 
                   c609_3_3==2, c609_4_1==2, c609_4_2==2, 
                   c609_5_1==2, c609_5_2==2, c609_5_3==2, 
                   c609_5_4==2, c609_5_5==2, c609_6_1==2, 
                   c609_6_2==2, c609_7_1==2, c609_7_2==2, 
                   c609_7_3==2, c609_7_4==2, c609_7_5==2, 
                   c609_7_6==2, c609_7_7==2, c609_8_1==2, 
                   c609_8_2==2, c609_8_3==2, c609_8_4==2,
                   c609_8_5==2, c609_8_6==2, c609_9_1==2, 
                   c609_9_2==2, c609_9_3==2, c609_10_1==2, 
                   c609_11_1==2, c609_12_1==2, c609_12_2==2, 
                   c609_12_3==2, c609_13_1==2, c609_13_2==2, 
                   c609_13_3==2, c609_13_4==2, c609_13_5==2, 
                   c609_13_6==2, c609_13_7==2, c609_14_1==2, 
                   c609_14_2==2, c609_14_3==2, c609_15_1==2, 
                   c609_15_2==2, c609_15_3==2, c609_16_1==2, 
                   c609_16_2==2, c609_16_3==2, c609_16_4==2, 
                   c609_16_5==2, c609_17_1==2, c609_17_2==2, 
                   c609_17_3==2, c609_17_4==2, c609_18_1==2, 
                   c609_18_2==2)) %>% 
  select(contains("childid"), ebf_ml)

ebf_bg <- bg %>% 
  full_join(ebf_bl, by = "childid") %>% 
  full_join(ebf_ml, by = "childid") %>% 
  select(childid, contains("ln_mpo"),
         contains("ln_neo"), contains("ln_reg1b"), 
         contains("ln_L_conc"), contains("ln_M_conc"),
         starts_with("ebf"))


allttests <- NULL 

for (i in c("ebf_bl")){
  for (j in c(grep("ln_mpo|ln_neo|ln_reg1b|ln_L_conc|ln_M_conc", names(ebf_bg), value = T))){
    formula <- as.formula(paste(j,"~", i, sep = ""))
    test <- t.test(formula, data = ebf_bg)
    res <- data.frame(t(test$estimate), p.value = test$p.value)
    allttests <- rbind(allttests, cbind(i, j, res))
  }
}

bg_ttests <- allttests

# Kenya
# c605 for ebf at bl and ml for both countries 
## is child still bf or completely weaned
## any amount of bf affects eed markers

k_ffq <- read_dta("/Users/geneho/Downloads/washk_excbf_20180531_share.dta") %>% 
  select(childid, contains("excbf"))

k_eed <- read_rds("final-data/eed-dev_k.RDS") %>% 
  select(childid, starts_with("ln_"), -ends_with("2"), -ends_with("3"))


k_ebf <- k_ffq %>% 
  inner_join(k_eed, by = "childid") %>% 
  mutate(across(c("excbf_combined", "excbf_eeblinf", "excbf_eemlinf", "excbf_mselup", "excbf_msmlup"),
                factor))

allttests <- NULL

# original list of vars
# "excbf_combined", "excbf_eeblinf", "excbf_eemlinf", "excbf_mselup", "excbf_msmlup"
# drop most because they werent breastfed anymore

for (i in c("excbf_combined", "excbf_eeblinf")){
  for (j in c(grep("ln_", names(k_ebf), value = T))){
    formula <- as.formula(paste(j,"~", i, sep = ""))
    test <- t.test(formula, data = k_ebf)
    res <- data.frame(t(test$estimate), p.value = test$p.value)
    allttests <- rbind(allttests, cbind(i, j, res))
  }
}

k_ttests <- allttests
writexl::write_xlsx(list("bangladesh" = bg_ttests,
                         "kenya" = k_ttests), "results/exclusive_bf.xlsx")



# prevalence of ebf

k_ffq %>% 
  summarize(ebf_prev = mean(excbf_eeblinf, na.rm = TRUE))

ebf_bl %>% 
  ungroup() %>% 
  mutate(ebf_num = as.numeric(ebf_bl)) %>% 
  summarize(ebf_prev = mean(ebf_num, na.rm = TRUE))


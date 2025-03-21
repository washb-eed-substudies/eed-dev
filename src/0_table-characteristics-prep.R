# baseline characteristics prep

# needed for: 
#  - 1_table-ones.Rmd
#  - 3_draft-tables-new.Rmd

bg <- readRDS(here('final-data/eed-dev_bg.RDS')) %>% 
  rename(hh_index = HHwealth) %>% 
  mutate(across(c(cesd_sum_t2, cesd_sum_ee_t3,
                  pss_sum_mom_t3), 
                as.numeric), 
         life_viol_any_t3 = case_when(as.character(life_viol_any_t3) == "missing" ~ NA_character_, 
                                      TRUE ~ as.character(life_viol_any_t3)) %>% 
           factor(),
         sex = factor(sex, c("female", "male")),
         country = 'Bangladesh', 
         across(c(elec, floor, roof, 
                  asset_phone, asset_moto),
                ~ .x == 1, 
                .names = "has_{.col}"),
         Nlt18 = as.integer(Nlt18)) 

k <- readRDS(here('final-data/eed-dev_k.RDS')) %>% 
  mutate(across(c(pss_score, phq_score_t3), 
                as.numeric), 
         sex = factor(sex, c("female", "male")),
         country = 'Kenya',
         across(c(electricity, floor, roof, 
                  mobile, motorcycle),
                ~ .x == 1, 
                .names = "has_{.col}"),
         Nlt18 = as.integer(Nlt18))


# filter out children without any exposure or any outcome

bg <- bg %>% 
  filter(if_any(c('ln_aat1', 'ln_mpo1', 'ln_neo1',
                  'ln_L_conc_t1', 'ln_M_conc_t1',
                  
                  'ln_aat2', 'ln_mpo2', 'ln_neo2',
                  'ln_L_conc_t2', 'ln_M_conc_t2',
                  'ln_reg2'),
                ~ !is.na(.)) & 
           if_any(c(
             starts_with("who_"),
             z_age2mo_cdi_undyr1_all_no4,
             z_age2mo_cdi_sayyr1_all_no4,
             
             z_age2mo_personal_no4,
             z_age2mo_motor_no4,
             z_age2mo_combined_no4,
             z_age2mo_com_no4,
             z_age2mo_cdi_undyr2_all_no4,
             z_age2mo_cdi_sayyr2_all_no4),
             ~ !is.na(.)))

bg$hfiacat <- factor(bg$hfiacat, 
                     levels = c("Mildly Food Insecure", "Moderately Food Insecure", 
                                "Severely Food Insecure", "Food Secure"),
                     ordered = TRUE)

var_label(bg) <- list(# tr = "Treatment Arm", 
  laz_t1 = "Length-for-age Z score (3 mo)", 
  waz_t1 = "Weight-for-age Z score (3 mo)", 
  laz_t2 = "Length-for-age Z score (14 mo)", 
  waz_t2 = "Weight-for-age Z score (14 mo)", 
  tr = "Treatment Arm",
  diar7d_t2 = "Caregiver reported diarrhea 7-day recall (3 mo)",
  diar7d_t3 = "Caregiver reported diarrhea 7-day recall (14 mo)",
  cesd_sum_t2 = "Depressive symptoms (CESD-R score*) at Year 1", 
  cesd_sum_ee_t3 = "Depressive symptoms (CESD-R score*) at Year 2",
  pss_sum_mom_t3 = "Maternal Perceived Stress Scale (PSS) at Year 2",
  sex = "Female (%)",
  momage = "Age (years)", 
  momheight_raw = "Height (cm)", 
  momedu = "Education completed", 
  hfiacat = "Household food insecurity",
  ageday_st1 = "Age at EED biomarker measurement, Follow-up 1 (months)",
  ageday_st2 = "Age at EED biomarker measurement, Follow-up 2 (months)",
  agedays_motor = "Age at child development measurement, Follow-up 2 (Year 1) (months)",
  agedays_cdi3 = "Age at child development measurement, Follow-up 3 (Year 2) (months)"
  # life_viol_any_t3 = "Cumulative maternal exposure to intimate partner violence",
  # floor = "Housing material, improved floor" 
  # birthord = "Birth Order", 
  # Nlt18 = "Number of children < 18 years old", 
  # Ncomp = "Total individuals in compound",
  # watmin = "Minutes to primary water source", 
  # walls = "Housing material, improved walls", 
  # roof = "Housing material, improved roof", 
  # hh_index = "Household wealth index"
)


# Kenya ----

k <- k %>% 
  filter(if_any(c('ln_aat1', 'ln_mpo1', 'ln_neo1',
                  'ln_Lact1', 'ln_Mann1',
                  
                  'ln_aat2', 'ln_mpo2', 'ln_neo2',
                  'ln_Lact2', 'ln_Mann2'),
                ~ !is.na(.)) & 
           if_any(c(
             starts_with("who_"),
             z_comtot_no4_activec,
             z_mottot_no4_activec,
             z_pstot_no4_activec,
             z_globaltot_no4_activec),
             ~ !is.na(.)))



k <- k %>% 
  mutate(momedu = factor(momedu, 
                         levels = c("Primary", "IncompletePrimary", 
                                    "AnySecondary", "missing"),
                         labels = c("Primary", "Incomplete Primary", 
                                    "Any Secondary", "Missing"), 
                         ordered = T),
         hhs_mod_sev = HHS %in% c(2, 3),
         across(starts_with("diarr7_t"), 
                ~ factor(.x, c("0", "1"))))

var_label(k) <- list(# tr = "Treatment Arm", 
  laz_t1 = "Length-for-age Z score (6 mo)", 
  waz_t1 = "Weight-for-age Z score (6 mo)", 
  laz_t2 = "Length-for-age Z score (17 mo)", 
  waz_t2 = "Weight-for-age Z score (17 mo)", 
  tr = "Treatment Arm", 
  # diarr7_t0 = "Caregiver reported diarrhea 7-day recall (6 mo)",
  # diarr7_t1 = "Caregiver reported diarrhea 7-day recall (17 mo)",
  pss_score = "Maternal Perceived Stress Scale (PSS) at Year 2",
  phq_score_t3 = "Depressive symptoms (PHQ*) at Year 2",
  sex = "Female (%)",
  momage = "Age (years)", 
  momheight_raw = "Height (cm)", 
  momedu = "Education completed", 
  hhs_mod_sev = "Prevalence of moderate to severe household hunger",
  aged1 = "Age at EED biomarker measurement, Follow-up 1 (months)",
  aged2 = "Age at EED biomarker measurement, Follow-up 2 (months)",
  agedays_motor = "Age at child development measurement, Follow-up 2 (Year 1) (months)",
  childage_dev = "Age at child development measurement, Follow-up 3 (Year 2) (months)"
  # Nlt18 = "Number of children < 18 years old", 
  # Ncomp = "Total individuals in compound",
  # water_time = "Minutes to primary water source", 
  # wall = "Housing material, walls", 
  # floor = "Housing material, floor"
  # roof = "Housing material, roof", 
  # hh_index = "Household wealth index"
)
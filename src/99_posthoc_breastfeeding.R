library(haven)
library(dplyr)
library(ggplot2)
library(here)
library(stringr)
library(boxr)

bf <- read_dta(here("raw-data/bangladesh/bftotal.dta"))


transformFFQdf <- function(df) {
  if (all(df$timepoint != "endline")) {
    df.new <- df %>%
      rename(childNo = childId) %>%
      rowwise() %>%
      mutate(
        any_liquid24h = sum(c_across(
          c(
            c608_1_3,
            c608_2_3,
            c608_3_3,
            c608_4_3,
            c608_5_3,
            c608_6_3,
            c608_7_3,
            c608_8_3,
            c608_9_3,
            c608_10_3,
            c608_11_3,
            c608_12_3
          )
        ) == 1),
        any_liquid7d = sum(c_across(
          c(c612_1, c612_2, c612_3, c612_4, c612_5, c612_6)
        ) == 1),
        any_solid24h = sum(c_across(
          starts_with("c609") &
            !contains("_other") & !contains("c609_19")
        ) == 1),
        any_solid7d = sum(c_across(
          c(
            c611_1_2,
            c611_2_2,
            c611_3_2,
            c611_4_2,
            c611_5_2,
            c611_6_2,
            c611_7_2,
            c611_8_2,
            c611_9_2,
            c611_10_2,
            c611_11_2,
            c611_12_2,
            c611_13_2,
            c611_14_2
          )
        ) > 0)
      ) %>%
      ungroup() %>%
      mutate(
        first_brstmlk_after_brth = factor(
          ifelse(
            is.na(c604) |
              c604 == 99,
            NA,
            ifelse(
              c604 == 88,
              "Never breastfed",
              ifelse(
                c604 == 0,
                "Within 30m of birth",
                ifelse(
                  c604 == 1,
                  "Within 1h of birth",
                  ifelse(
                    c604 == 2,
                    "1-24h after birth",
                    ifelse(c604 == 3, ">24h after birth", "Problem")
                  )
                )
              )
            )
          ),
          levels = c(
            "Within 30m of birth",
            "Within 1h of birth",
            "1-24h after birth",
            ">24h after birth",
            "Never breastfed",
            "Problem"
          )
        ),
        brstfd_YN = ifelse(is.na(c605), NA, ifelse(c605 == 1, 1, 0)),
        brstfd_times = ifelse(is.na(c607), NA, c607),
        brstfd_times_cat = factor(
          ifelse(
            is.na(brstfd_times) |
              brstfd_times < 6,
            "0-5",
            ifelse(
              brstfd_times < 11,
              "6-10",
              ifelse(brstfd_times < 16, "11-15", "16-20+")
            )
          ),
          levels = c("0-5",
                     "6-10",
                     "11-15",
                     "16-20+")
        ),
        last_brstfd_age_months = ifelse(
          is.na(c606months) &
            is.na(c606days),
          NA,
          ifelse(c606days == 0, c606months, round(c606months + c606days / 30, 0))
        ),
        othrmlk = ifelse(is.na(c608_3_3) |
                           c608_3_3 == 0, NA, ifelse(c608_3_3 == 1, 1, 0)),
        othrmlk_times = ifelse(is.na(c608_3_2) |
                                 othrmlk == 0, NA, c608_3_2),
        formula = ifelse(is.na(c608_4_3) |
                           c608_4_3 == 0, NA, ifelse(c608_4_3 == 1, 1, 0)),
        formula_times = ifelse(is.na(c608_4_2) |
                                 formula == 0, NA, c608_4_2),
        brstfd_cat = factor(
          ifelse(
            is.na(brstfd_YN),
            NA,
            ifelse(
              first_brstmlk_after_brth == "Never breastfed",
              "No longer breastfed",
              ifelse(
                brstfd_YN == 1 &
                  (
                    any_liquid24h == 0 &
                      any_liquid7d == 0 &
                      any_solid24h == 0 &
                      any_solid7d == 0
                  ),
                "Exclusively breastfed",
                ifelse(
                  brstfd_YN == 1 &
                    (any_liquid24h > 0 |
                       any_liquid7d > 0) &
                    (any_solid24h == 0 & any_solid7d == 0),
                  "Breastfed &
              other liquids / porridge",
              ifelse(
                brstfd_YN == 1 & (any_solid24h > 0 | any_solid7d > 0),
                "Breastfed &
              more substantive complimentary foods",
              ifelse(brstfd_YN == 0, "No longer breastfed", "error")
              )
                )
              )
            )
          ),
          levels = c(
            "Exclusively breastfed",
            "Breastfed &
              other liquids / porridge",
            "Breastfed &
              more substantive complimentary foods",
            "No longer breastfed"
          )
        )
      ) %>%
      select(dataid, childNo, timepoint, any_liquid24h:brstfd_cat)
  } else {
    df.new <- df %>%
      rowwise() %>%
      mutate(
        any_liquid24h = sum(c_across(
          c(
            c608_1_1,
            c608_2_1,
            c608_3_1,
            c608_4_1,
            c608_5_1,
            c608_6_1,
            c608_7_1,
            c608_8_1,
            c608_9_1,
            c608_10_1,
            c608_11_1,
            c608_12_1
          )
        ) == 1),
        any_solid24h = sum(c_across(
          starts_with("c609") &
            !contains("_other") & !contains("c609_19")
        ) == 1),
        any_solid7d = sum(c_across(
          c(
            c611_1_1,
            c611_2_1,
            c611_3_1,
            c611_4_1,
            c611_5_1,
            c611_6_1,
            c611_7_1,
            c611_8_1,
            c611_9_1,
            c611_10_1,
            c611_11_1,
            c611_12_1,
            c611_13_1,
            c611_14_1
          )
        ) > 0)
      ) %>%
      ungroup() %>%
      mutate(
        brstfd_YN = ifelse(is.na(c605), NA, ifelse(c605 == 1, 1, 0)),
        # brstfd_times = ifelse(is.na(c607), NA, c607),
        ## This data is not in the endline dataset?
        # brstfd_times_cat = factor(ifelse(is.na(brstfd_times) | brstfd_times <6,  "0 - 5", ifelse(brstfd_times < 11, "6 - 10", ifelse(brstfd_times < 16, "11 - 15", "16 - 20 + "))), levels = c("0 - 5", "6 - 10", "11 - 15", "16 - 20 +")),
        ## brstfd_times data is not in the endline dataset (?) so can’t compute
        last_brstfd_age_months = ifelse(
          is.na(c606months) &
            is.na(c606days),
          NA,
          ifelse(c606days == 0, c606months, round(c606months + c606days / 30, 0))
        ),
        othrmlk = ifelse(
          is.na(c608_3_1) | c608_3_1 == 99,
          NA,
          ifelse(c608_3_1 == 1, 1, 0)
        ),
        othrmlk_times = ifelse(is.na(c608_3_2) |
                                 othrmlk == 0, NA, c608_3_2),
        formula = ifelse(
          is.na(c608_4_1) | c608_4_1 == 99,
          NA,
          ifelse(c608_4_1 == 1, 1, 0)
        ),
        formula_times = ifelse(is.na(c608_4_2) |
                                 formula == 0, NA, c608_4_2),
        brstfd_cat = factor(
          ifelse(
            is.na(brstfd_YN),
            NA,
            ifelse(
              brstfd_YN == 1 &
                (any_liquid24h == 0 &
                   any_solid24h == 0 &
                   any_solid7d == 0),
              "Exclusively breastfed",
              ifelse(
                brstfd_YN == 1 &
                  any_liquid24h > 0 &
                  (any_solid24h == 0 &
                     any_solid7d == 0),
                "Breastfed & other liquids / porridge",
                ifelse(
                  brstfd_YN == 1 &
                    (any_solid24h > 0 |
                       any_solid7d > 0),
                  "Breastfed & more substantive complimentary foods",
                  ifelse(brstfd_YN == 0, "No longer breastfed", "error")
                )
              )
            )
          ),
          levels = c(
            "Exclusively breastfed",
            "Breastfed & other liquids / porridge",
            "Breastfed & more substantive complimentary foods",
            "No longer breastfed"
          )
        )
      ) %>%
      select(dataid, childNo, timepoint, any_liquid24h:brstfd_cat)
  }
  df.new <- df.new %>%
    mutate(childid = as.numeric(paste0(dataid, childNo)))
  return(df.new)
}



# ----


bf <- bf %>% 
  select(childid, contains("ln_mpo"),
         contains("ln_neo"), contains("ln_reg1b"), 
         contains("ln_l_conc"), contains("ln_m_conc"), 
         contains("brstfd"))

allttests <- NULL

for (i in c("brstfd_yn1")){
  for (j in c(grep("ln_mpo|ln_neo|ln_reg1b|ln_l_conc|ln_m_conc", names(bf), value = T))){
    formula <- as.formula(paste(j,"~", i, sep = ""))
    test <- t.test(formula, data = bf)
    res <- data.frame(t(test$estimate), p.value = test$p.value)
    allttests <- rbind(allttests, cbind(i, j, res))
  }
}

t2 <- allttests[,2] %>% 
  str_subset("1", negate = TRUE) %>% 
  unique()

for (i in c("brstfd_yn2")){
  for (j in t2){
    formula <- as.formula(paste(j,"~", i, sep = ""))
    test <- t.test(formula, data = bf)
    res <- data.frame(t(test$estimate), p.value = test$p.value)
    allttests <- rbind(allttests, cbind(i, j, res))
  }
}

t3 <- allttests[,2] %>% 
  str_subset("1|2", negate = TRUE) %>% 
  unique()

for (i in c("brstfd_yn3")){
  for (j in t3){
    formula <- as.formula(paste(j,"~", i, sep = ""))
    test <- t.test(formula, data = bf)
    res <- data.frame(t(test$estimate), p.value = test$p.value)
    allttests <- rbind(allttests, cbind(i, j, res))
  }
}

allttests <- allttests %>% 
  mutate(time = case_when(str_sub(i, start = -1, end = -1) == str_sub(j, start = -1, end = -1) ~ "c", 
                          TRUE ~ "s"),
         i = factor(i, labels = c("3m", "14m", "28m")))

ggplot(data = as.data.frame(allttests)) +
  geom_col(aes(x = j, y = as.numeric(p.value), fill = time)) +
  geom_hline(yintercept = 0.2, color = "red") +
  facet_wrap(~i, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("darkgreen", "maroon")) + 
  labs(x = "", y = "t-test p-value")

long <- reshape2::melt(allttests, id.vars = c("i", "j", "p.value"))
long <- long %>% mutate(variable = ifelse(variable == "mean.in.group.0", "No", "Yes"))

ggplot(data = as.data.frame(long)) +
  geom_col(aes(x = j, y = as.numeric(value), fill = variable), position = "dodge") +
  facet_wrap(~i, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Mean Z-score", fill = "Currently Breastfed")




box_read_rds(841632862625) %>% 
  names()

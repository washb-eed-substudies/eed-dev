# combine results

library(fs)
library(dplyr)
library(stringr)
library(purrr)
library(here)

# read in analysis output ----

adj_file_list <- dir_ls("results/adjusted") %>% 
  str_subset("_all_adj")

unadj_file_list <- dir_ls("results/unadjusted")


for (file in adj_file_list){
  
  obj_name <- str_match(file, "/adjusted/(.*)_all")[2] %>% 
    str_replace_all("-", "_") %>% 
    str_to_lower()
  
  assign(paste0(obj_name, "_adj"), readRDS(file))
}




clean_results <- function(data, hyp_num, measure = "gam"){
  
  pasted_var <- case_when(measure == "gam" ~ "Adj. Diff (95% CI)", 
                          measure == "hr" ~ "Adj. HR (95% CI)")
  
  ub <- case_when(measure == "gam" ~ "ub.diff", 
                 measure == "hr" ~ "ub.HR")
  lb <- case_when(measure == "gam" ~ "lb.diff", 
                  measure == "hr" ~ "lb.HR")
  
  point <- case_when(measure == "gam" ~ "point.diff", 
                  measure == "hr" ~ "point.HR")
  
  y_sd_name <- case_when(measure == "gam" ~ "Outcome Subdomain", 
                         measure == "hr" ~ "Motor Milestone")
  
  format_ci <- function(point.est, lower, upper){
    format_num <- function(number, digits = 2){
      rounded_num <- round(number, digits = digits)
      ifelse(rounded_num == 0,
             case_when(sign(number) == -1 ~ paste0(">-0.", rep.int("0", digits - 1), "1"),
                       sign(number) == 1  ~ paste0("<0.", rep.int("0", digits - 1), "1"),
                       sign(number) == 0  ~ "0.000"),
             as.character(rounded_num))
    }
    
    fmt_point <- format_num(point.est)
    fmt_lb <- format_num(lower)
    fmt_ub <- format_num(upper)
    
    paste0(fmt_point, " (", fmt_lb, ", ", fmt_ub, ")")
  }
  
  
  data %>% 
    select(-starts_with("pred")) %>% 
    mutate(# across(.cols = where(is.numeric),
                  # .fns = ~ round(., 2)), 
           hyp = hyp_num, 
           t_exp = str_sub(X, start = -1, end = -1),
           outcome_domain = case_when(str_detect(Y, "who") ~ "WHO Motor Milestones", 
                                      str_detect(Y, "cdi") ~ "Communicative Development Inventory",
                                      str_detect(Y, "(personal|motor|combined|com|pstot|mottot|globaltot|comtot)") ~ 
                                        "Extended Ages and Stages"), 
           # add timepoint
           outcome_domain = case_when(hyp_num %in% c(1, 4) ~ str_c(outcome_domain, " (Year 1)"), 
                                      hyp_num %in% c(2, 3, 5) ~ str_c(outcome_domain, " (Year 2)")),
           x_human = case_when(str_detect(X, "aat") ~ "alpha-1 antitrypsin", 
                               str_detect(X, "mpo") ~ "myeloperoxidase", 
                               str_detect(X, "neo") ~ "neopterin", 
                               str_detect(X, "reg") ~ "regenerating gene 1B", 
                               str_detect(X, "_L") ~ "lactulose", 
                               str_detect(X, "_M") ~ "mannitol"),
           x_human = case_when(between(hyp_num, 1, 3) ~ case_when(t_exp == 1 ~ str_c(x_human, " (3 mo)"),
                                                                  t_exp == 2 ~ str_c(x_human, " (14 mo)")),
                               between(hyp_num, 4, 5) ~ case_when(t_exp == 1 ~ str_c(x_human, " (6 mo)"),
                                                                  t_exp == 2 ~ str_c(x_human, " (17 mo)"))),
           y_subdomain = case_when(# WHO MM
                                   str_detect(Y, "sum") ~ "Sum Total", 
                                   str_detect(Y, "sub") ~ "Milestones 2,4,5,6", 
                                   
                                   str_detect(Y, "sit") ~ "Sitting", 
                                   str_detect(Y, "crawl") ~ "Crawling", 
                                   str_detect(Y, "walk") ~ "Walking", 
                                   str_detect(Y, "stand") ~ "Standing", 
                                   
                                   # CDI
                                   str_detect(Y, "und") ~ "Understanding", 
                                   str_detect(Y, "say") ~ "Expressing", 
                                   # EASQ Bangladesh
                                   str_detect(Y, "personal") ~ "Personal Social", 
                                   str_detect(Y, "motor") ~ "Motor", 
                                   str_detect(Y, "combined") ~ "Combined", 
                                   str_detect(Y, "com") ~ "Communication", 
                                   # EASQ Kenya
                                   str_detect(Y, "pstot") ~ "Personal Social", 
                                   str_detect(Y, "mottot") ~ "Motor", 
                                   str_detect(Y, "globaltot") ~ "Combined", 
                                   str_detect(Y, "comtot") ~ "Communication"),
           y_subdomain = case_when(str_detect(Y, "nosupp") ~ str_c(y_subdomain, " (w/o support)"), 
                                   str_detect(Y, "supp") ~ str_c(y_subdomain, " (w/ support)"), 
                                   TRUE ~ y_subdomain),
           pasted_results := format_ci(!!rlang::sym(point), !!rlang::sym(lb), !!rlang::sym(ub))) %>% 
    select(hyp,
           "Outcome Domain" = outcome_domain, 
           !!y_sd_name := y_subdomain, 
           "Exposure" = x_human, t_exp,
           "n" = N, 
           "Q1" = q1, "Q3" = q3, 
           !!pasted_var := pasted_results, 
           "Adj. P-value" = Pval)
}

# h2_adj %>%
#   clean_results(2)


# create list of dfs
list_hyp <- list(h1_adj, h2_adj, h3_adj, h4_adj, h5_adj)

# apply function and hypothesis number to each df
cleaned_results <- map2(list_hyp, 1:5, clean_results)


list_who_hyp <- list(h1_who_hr_adj, h4_who_hr_adj)
cleaned_who <- map2(list_who_hyp, c(1,4), clean_results, measure = "hr")

# FDR correction ---- 

cleaned_results <- cleaned_results %>% 
  map(\(df) mutate(df, 
                   corrected.Pval_adj = p.adjust(`Adj. P-value`, "BH")))

cleaned_who <- cleaned_who %>% 
  map(\(df) mutate(df, 
                   corrected.Pval_adj = p.adjust(`Adj. P-value`, "BH")))

# flag significant p-values ----

flag_p_values <- function(column){
  case_when(column < 0.01 ~ "***", 
            column < 0.05 ~ "**", 
            column < 0.2 ~ "*")
}


cleaned_results <- cleaned_results %>% 
  map(~ mutate(., 
               adj_p_significance = flag_p_values(`Adj. P-value`),
               fdr_significance = flag_p_values(corrected.Pval_adj)))

cleaned_results %>% 
  map(~ filter(., if_any(.cols = c(ends_with("significance")), compose(`!`, is.na))))

cleaned_who <- cleaned_who %>% 
  map(~ mutate(., 
               adj_p_significance = flag_p_values(`Adj. P-value`),
               fdr_significance = flag_p_values(corrected.Pval_adj)))

cleaned_who %>% 
  map(~ filter(., if_any(.cols = c(ends_with("significance")), compose(`!`, is.na))))
# write data ----

saveRDS(cleaned_results, here("results/final/cleaned_gam_results.RDS"))
saveRDS(cleaned_who, here("results/final/cleaned_hr_results.RDS"))
# import data
k_phq_pss <- read_csv('kenya/washk_phq_pss_20210203.csv', 
                      col_types = list(phq_comments = 'c',
                                       pss_comment = 'c',
                                       phq1 = 'f', phq2 = 'f',
                                       phq3 = 'f', phq4 = 'f', 
                                       phq5 = 'f', phq6 = 'f', 
                                       phq7 = 'f', phq8 = 'f', 
                                       phq9 = 'f', 
                                       pss_1 = 'i', pss_2 = 'i', 
                                       pss_3 = 'i', pss_4 = 'i', 
                                       pss_5 = 'i', pss_6 = 'i', 
                                       pss_7 = 'i', pss_8 = 'i', 
                                       pss_9 = 'i', pss_10 = 'i'
                      )) 

# Patient Health Questionnaire

k_phq <- k_phq_pss %>% 
  select(childid, starts_with('phq'),
         -phq_comments) %>% 
  # only EED children
  filter(childid %in% k_eed$childid) %>% 
  # create factored vars
  mutate(across(.cols = num_range('phq', 1:8), 
                .fns = ~ factor(., levels = c('0-1 day', '2-6 days', '7-11 days', '12-14 days'), 
                                ordered = TRUE)), 
         phq9 = factor(phq9, levels = c('Not difficult at all', 'Somewhat difficult', 
                                        'Very difficult', 'Extremely difficult'),
                       ordered = TRUE), 
         # to match PHQ-9 scale 
         across(.cols = starts_with('phq'), 
                .fns = ~ as.integer(.) - 1)) %>% 
  # get data into a format R likes
  pivot_longer(cols = starts_with('phq')) %>% 
  group_by(childid) %>% 
  # remove children that didn't do survey
  filter(!all(is.na(value)))

# ----
# PHQ checks

k_phq %>% 
  summarize(num_na = sum(is.na(value))) %>% 
  arrange(desc(num_na))
# there is 1 observation with more than 3 missing values
# will drop
# for the rest, we will impute the values with the mean

# ----
# calculate total PHQ score

# remove observations with more than 1 missing
k_phq <- k_phq %>% 
  filter(sum(is.na(value)) <= 1) %>% 
  # impute NA with mean for person
  mutate(value = replace_na(value, mean(value, na.rm = TRUE)))

# check values to make sure they are in range 0-3
k_phq %>% 
  group_by(name) %>% 
  summarize(min = min(value, na.rm = TRUE), 
            max = max(value, na.rm = TRUE))

# calculate total scores
k_phq <- k_phq %>%
  summarize(phq_score_t3 = sum(value))

# find quartiles 
# cut at each point
k_phq$quantile <- cut(k_phq$phq_score_t3, 
                      breaks = quantile(k_phq$phq_score_t3, probs = seq(0, 1, 0.25)),
                      include.lowest = TRUE,
                      labels = FALSE)

# last checks 
summary(k_phq)
k_phq$quantile %>% 
  table(useNA = 'ifany')

# ----
# Perceived Stress Score

k_pss <- k_phq_pss %>% 
  select(childid, starts_with('pss'),
         -pss_comment, -pss_note1) %>% 
  filter(childid %in% k_eed$childid) %>%
  mutate(across(.cols = starts_with('pss'), 
                .fns = ~ replace(., . > 4, NA_integer_)),
         across(.cols = c(pss_4, pss_5, pss_7, pss_8), 
                .funs = ~ abs(. - 4))) %>% 
  pivot_longer(cols = starts_with('pss')) %>% 
  group_by(childid) %>% 
  filter(all(!is.na(value))) %>% # remove children without any answers
  summarize(pss_score = sum(value)) 


# bin scores

k_pss$quantile <- cut(k_pss$pss_score, 
                      breaks = quantile(k_pss$pss_score, probs = seq(0, 1, 0.25)), 
                      labels = FALSE)

# ----
# test for PSS

# how many NAs for each question
k_phq_pss %>% 
  select(childid, starts_with('pss'),
         -pss_comment, -pss_note1) %>% 
  filter(childid %in% k_eed$childid) %>% 
  summarize(across(.cols = starts_with('pss'), 
                   .fns = ~ sum(is.na(.))))

# filter children with at least 1 NA, but not all
k_phq_pss %>% 
  select(childid, starts_with('pss'),
         -pss_comment, -pss_note1) %>% 
  filter(childid %in% k_eed$childid) %>% 
  pivot_longer(cols = starts_with('pss')) %>% 
  group_by(childid) %>% 
  filter(any(is.na(value)),
         !all(is.na(value)))
# none!

k_phq_pss_final <- k_phq %>% 
  full_join(k_pss, 
            by = 'childid', 
            suffix = c('_phq', '_pss'))

# 5 will be the missing category
# since we're using quartiles
k_phq_pss_final <- k_phq_pss_final %>% 
  right_join(select(k_eed, childid), 
             by = 'childid') %>% 
  mutate(across(.cols = starts_with('quantile'), 
                .fns = ~ replace_na(., 'missing')))

write_csv(k_phq_pss_final, 'k_phq_pss_with_quant.csv')
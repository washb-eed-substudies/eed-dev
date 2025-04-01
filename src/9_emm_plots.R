
full_res_tr = readRDS(here('results/tr-emm-results.RDS')) %>% rename(`Treatment\narm` = Vlevel)
full_res_sth = readRDS(here('results/sth-emm-results.RDS')) %>%
  mutate(`Any STH\ninfection` = factor(Vlevel))

prop.table(table(full_res_tr$int.Pval <0.05))*100
full_res_tr$corrected_p <-  p.adjust(full_res_tr$int.Pval, "BH")
prop.table(table(full_res_tr$corrected_p <0.05))*100
full_res_tr %>% filter(int.Pval < 0.05)
full_res_tr %>% filter(corrected_p < 0.05)

full_res_tr %>% group_by(`Treatment\narm`) %>% summarise(n = n(), mean = mean(point.diff), Pval = mean(Pval))

prop.table(table(full_res_sth$corrected_p <0.05))*100
prop.table(table(full_res_sth$int.Pval <0.05))*100
full_res_sth$corrected_p <-  p.adjust(full_res_sth$int.Pval, "BH")
full_res_sth %>% filter(corrected_p < 0.05)
prop.table(table(full_res_sth$corrected_p <0.05))*100

full_res_sth %>% filter(Y=="who_sub_total" & X=="ln_aat1")
full_res_sth %>% filter(Y=="z_age2mo_cdi_sayyr1_all_no4" & X=="ln_mpo1")
full_res_sth %>% filter(Y=="z_age2mo_cdi_sayyr2_all_no4" & X=="ln_mpo1")
full_res_sth %>% filter(Y=="z_age2mo_com_no4" & X=="ln_neo2")

ggplot(full_res_tr , aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff, group=`Treatment\narm`, colour =`Treatment\narm`)) +
  geom_pointrange(position = position_dodge(0.5)) +
  facet_wrap(~Y, scales="free", ncol=3) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="",
       x="EED exposure",
       y="Difference in z-score between 25th and 75th percentile")


ggplot(full_res_sth %>% filter(!is.na(`Any STH\ninfection`)), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff, group=`Any STH\ninfection`, colour =`Any STH\ninfection`)) +
  geom_pointrange(position = position_dodge(0.5)) +
  facet_wrap(~Y, scales="free") +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="",
       x="EED exposure",
       y="Difference in z-score between 25th and 75th percentile")


ggplot(full_res_sth %>% filter(!is.na(`Any STH\ninfection`),int.Pval <0.05|is.na(int.Pval)), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff, group=`Any STH\ninfection`, colour =`Any STH\ninfection`)) +
  geom_pointrange(position = position_dodge(0.5)) +
  facet_wrap(~Y, scales="free") +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="",
       x="EED exposure",
       y="Difference in z-score between 25th and 75th percentile")





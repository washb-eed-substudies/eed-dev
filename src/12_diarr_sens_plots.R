
rm(list=ls())

source(here::here("0-config.R"))


main_h1 <- readRDS(here("results/adjusted/H1_all_adj_res.RDS")) %>% mutate(H=1, Analysis="Main") 
diarrhea_h1 <- readRDS(here("results/sensitivity/H1_diar_sens_res.RDS"))  %>% mutate(H=1, Analysis="Diarrhea") 
main_h2 <- readRDS(here("results/adjusted/H2_all_adj_res.RDS"))  %>% mutate(H=2, Analysis="Main")
diarrhea_h2 <- readRDS(here("results/sensitivity/H2_diar_sens_res.RDS"))  %>% mutate(H=2, Analysis="Diarrhea") 
main_h3 <- readRDS(here("results/adjusted/H3_all_adj_res.RDS"))  %>% mutate(H=3, Analysis="Main")
diarrhea_h3 <- readRDS(here("results/sensitivity/H3_diar_sens_res.RDS")) %>% mutate(H=3, Analysis="Diarrhea") 
main_h4 <- readRDS(here("results/adjusted/H4_all_adj_res.RDS")) %>% mutate(H=4, Analysis="Main")
diarrhea_h4 <- readRDS(here("results/sensitivity/H4_diar_sens_res.RDS")) %>% mutate(H=4, Analysis="Diarrhea") 
main_h5 <- readRDS(here("results/adjusted/H5_all_adj_res.RDS")) %>% mutate(H=5, Analysis="Main")
diarrhea_h5 <- readRDS(here("results/sensitivity/H5_diar_sens_res.RDS")) %>% mutate(H=5, Analysis="Diarrhea") 


d <- bind_rows(main_h1, diarrhea_h1,
               main_h2, diarrhea_h2,
               main_h3, diarrhea_h3,
               main_h4, diarrhea_h4,
               main_h5, diarrhea_h5
               ) %>% 
  mutate(H = factor(H, levels=1:5, labels=c("H1", "H2", "H3", "H4", "H5")),
         Analysis = factor(Analysis, levels=c("Main", "Diarrhea")),
         point.diff = round(point.diff, 2),
         lb.diff = round(lb.diff, 2),
         ub.diff = round(ub.diff, 2)) %>%
  select(Y,X, point.diff, lb.diff, ub.diff, H,Analysis)

head(d)

diarrhea_comp_p1 <- ggplot(d %>% filter(H=="H1"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Analysis, color=Analysis)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Diarrhea sensitivity analysis:  Hypothesis 1") +
  scale_color_manual(values=c("Main"=tableau10[2], "Diarrhea"=tableau10[1]))

diarrhea_comp_p2 <- ggplot(d %>% filter(H=="H2"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Analysis, color=Analysis)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Diarrhea sensitivity analysis:  Hypothesis 2") +
  scale_color_manual(values=c("Main"=tableau10[2], "Diarrhea"=tableau10[1]))

diarrhea_comp_p3 <- ggplot(d %>% filter(H=="H3"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Analysis, color=Analysis)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Diarrhea sensitivity analysis:  Hypothesis 3") +
  scale_color_manual(values=c("Main"=tableau10[2], "Diarrhea"=tableau10[1]))


diarrhea_comp_p4 <- ggplot(d %>% filter(H=="H4"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Analysis, color=Analysis)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Diarrhea sensitivity analysis:  Hypothesis 4") +
  scale_color_manual(values=c("Main"=tableau10[2], "Diarrhea"=tableau10[1]))

diarrhea_comp_p5 <- ggplot(d %>% filter(H=="H5"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Analysis, color=Analysis)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Diarrhea sensitivity analysis:  Hypothesis 5") +
  scale_color_manual(values=c("Main"=tableau10[2], "Diarrhea"=tableau10[1]))

                  
saveRDS(d, file=here("results/diarrhea_main_comparison.RDS"))   
save(diarrhea_comp_p1, diarrhea_comp_p2, diarrhea_comp_p3, diarrhea_comp_p4, diarrhea_comp_p5, file=here("figure-data/diarrhea_sens_comparison_plots.Rdata"))

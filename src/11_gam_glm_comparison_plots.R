
rm(list=ls())

source(here::here("0-config.R"))


gam_h1 <- readRDS(here("results/adjusted/H1_all_adj_res.RDS")) %>% mutate(H=1, Model="GAM") 
glm_h1 <- readRDS(here("results/adjusted/H1_all_glm_boot_res.RDS"))  %>% mutate(H=1, Model="GLM") %>% rename(point.diff=est, lb.diff=ci.lb, ub.diff=ci.ub  )

gam_h2 <- readRDS(here("results/adjusted/H2_all_adj_res.RDS"))  %>% mutate(H=2, Model="GAM")
glm_h2 <- readRDS(here("results/adjusted/H2_all_glm_boot_res.RDS"))  %>% mutate(H=2, Model="GLM") %>% rename(point.diff=est, lb.diff=ci.lb, ub.diff=ci.ub  )

gam_h3 <- readRDS(here("results/adjusted/H3_all_adj_res.RDS"))  %>% mutate(H=3, Model="GAM")
glm_h3 <- readRDS(here("results/adjusted/H3_all_glm_boot_res.RDS")) %>% mutate(H=3, Model="GLM") %>% rename(point.diff=est, lb.diff=ci.lb, ub.diff=ci.ub  )

gam_h4 <- readRDS(here("results/adjusted/H4_all_adj_res.RDS")) %>% mutate(H=4, Model="GAM")
glm_h4 <- readRDS(here("results/adjusted/H4_all_glm_boot_res.RDS")) %>% mutate(H=4, Model="GLM") %>% rename(point.diff=est, lb.diff=ci.lb, ub.diff=ci.ub  )

gam_h5 <- readRDS(here("results/adjusted/H5_all_adj_res.RDS")) %>% mutate(H=5, Model="GAM")
glm_h5 <- readRDS(here("results/adjusted/H5_all_glm_boot_res.RDS")) %>% mutate(H=5, Model="GLM") %>% rename(point.diff=est, lb.diff=ci.lb, ub.diff=ci.ub  )




d <- bind_rows(gam_h1, glm_h1,
               gam_h2, glm_h2,
               gam_h3, glm_h3,
               gam_h4, glm_h4,
               gam_h5, glm_h5
               ) %>% 
  mutate(H = factor(H, levels=1:5, labels=c("H1", "H2", "H3", "H4", "H5")),
         Model = factor(Model, levels=c("GAM", "GLM")),
         point.diff = round(point.diff, 2),
         lb.diff = round(lb.diff, 2),
         ub.diff = round(ub.diff, 2)) %>%
  select(Y,X, point.diff, lb.diff, ub.diff, H,Model)

head(d)

glm_comp_p1 <- ggplot(d %>% filter(H=="H1"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Comparison of GAM and GLM effect sizes: Hypothesis 1") +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))

glm_comp_p2 <- ggplot(d %>% filter(H=="H2"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Comparison of GAM and GLM effect sizes: Hypothesis 2") +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))


glm_comp_p3 <- ggplot(d %>% filter(H=="H3"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Comparison of GAM and GLM effect sizes: Hypothesis 3") +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))

glm_comp_p4 <- ggplot(d %>% filter(H=="H4"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Comparison of GAM and GLM effect sizes: Hypothesis 4") +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))

glm_comp_p5 <- ggplot(d %>% filter(H=="H5"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Difference in effect size", x="Hypothesis", title="Comparison of GAM and GLM effect sizes: Hypothesis 5") +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))

                  
saveRDS(d, file=here("results/glm_gam_comparison.RDS"))   
save(glm_comp_p1, glm_comp_p2, glm_comp_p3, glm_comp_p4, glm_comp_p5, file=here("figure-data/glm_gam_comparison_plots.Rdata"))

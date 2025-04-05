
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



d <- d %>% 
  mutate(
    Xvar=X,
    Yvar=Y,
    t_exp = str_sub(Xvar,start = -1, end = -1),
    outcome_domain = case_when(str_detect(Yvar,"who") ~ "WHO Motor Milestones", 
                               str_detect(Yvar,"supp") ~ "WHO Motor Milestones", 
                               str_detect(Yvar,"cdi") ~ "Communicative Development Inventory",
                               str_detect(Yvar,"(personal|motor|combined|com|pstot|mottot|globaltot|comtot)") ~ 
                                 "Extended Ages and Stages"), 
    # add timepoint
    X = case_when(str_detect(Xvar,"aat") ~ "alpha-1 antitrypsin", 
                        str_detect(Xvar,"mpo") ~ "myeloperoxidase", 
                        str_detect(Xvar,"neo") ~ "neopterin", 
                        str_detect(Xvar,"reg") ~ "regenerating gene 1B", 
                        str_detect(Xvar,"_L") ~ "lactulose", 
                        str_detect(Xvar,"_M") ~ "mannitol"),
    X = case_when(H %in% c("H1","H2","H3") ~ case_when(t_exp == 1 ~ str_c(X, " (3 mo)"),
                                                           t_exp == 2 ~ str_c(X, " (14 mo)")),
                        H %in% c("H4","H5") ~ case_when(t_exp == 1 ~ str_c(X, " (6 mo)"),
                                                           t_exp == 2 ~ str_c(X, " (17 mo)"))),
    Y = case_when(# WHO MM
      str_detect(Y,"sum") ~ "Sum Total", 
      str_detect(Y,"sub") ~ "Milestones 2,4,5,6", 
      
      str_detect(Y,"sit") ~ "Sitting", 
      str_detect(Y,"crawl") ~ "Crawling", 
      str_detect(Y,"walk") ~ "Walking", 
      str_detect(Y,"stand") ~ "Standing", 
      
      # CDI
      str_detect(Y,"und") ~ "Understanding", 
      str_detect(Y,"say") ~ "Expressing", 
      # EASQ Bangladesh
      str_detect(Y,"personal") ~ "Personal Social", 
      str_detect(Y,"motor") ~ "Motor", 
      str_detect(Y,"combined") ~ "Combined", 
      str_detect(Y,"com") ~ "Communication", 
      # EASQ Kenya
      str_detect(Y,"pstot") ~ "Personal Social", 
      str_detect(Y,"mottot") ~ "Motor", 
      str_detect(Y,"globaltot") ~ "Combined", 
      str_detect(Y,"comtot") ~ "Communication"),
    Y = case_when(str_detect(Y,"nosupp") ~ str_c(Y, " (w/o support)"), 
                            str_detect(Y,"supp") ~ str_c(Y, " (w/ support)"), 
                            TRUE ~ Y),
    # pasted_results := format_ci(!!rlang::sym(point), !!rlang::sym(lb), !!rlang::sym(ub))
  ) 

head(d)

glm_comp_p1 <- ggplot(d %>% filter(H=="H1"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Effect size",x="Exposure"#, 
       #title="Comparison of GAM and GLM effect sizes:\n3 month EED measures and combined motor milestone\noutcomes in Bangladesh"
       ) +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))

glm_comp_p2 <- ggplot(d %>% filter(H=="H2"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Effect size",x="Exposure"#, 
       #title="Comparison of GAM and GLM effect sizes: 3 month EED measures and EASQ outcomes in Bangladesh"
       ) +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))

unique(d$X)
d$X <- gsub("regenerating gene 1B \\(14 mo\\)", "regenerating gene\n1B (14 mo)", d$X)
glm_comp_p3 <- ggplot(d %>% filter(H=="H3"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Effect size",x="Exposure", 
       #title="Comparison of GAM and GLM effect sizes: Regenerating gene 1B exposure in Bangladesh"
       ) +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))

glm_comp_p4 <- ggplot(d %>% filter(H=="H4"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Effect size",x="Exposure", 
       #title="Comparison of GAM and GLM effect sizes: 6 month EED measures and combined motor milestone outcomes in Kenya  "
       ) +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))

glm_comp_p5 <- ggplot(d %>% filter(H=="H5"), aes(x=X, y=point.diff, ymin=lb.diff, ymax=ub.diff,  group=Model, color=Model)) +
  geom_pointrange(position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~Y, scales="free_x") +
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_flip() +
  labs(y="Effect size",x="Exposure", #title="Comparison of GAM and GLM effect sizes: EED measures and EASQ outcomes in Kenya"
       ) +
  scale_color_manual(values=c("GAM"=tableau10[2], "GLM"=tableau10[1]))

                  
saveRDS(d, file=here("results/glm_gam_comparison.RDS"))   
save(glm_comp_p1, glm_comp_p2, glm_comp_p3, glm_comp_p4, glm_comp_p5, file=here("figure-data/glm_gam_comparison_plots.Rdata"))

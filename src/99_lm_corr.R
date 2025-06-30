rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(here('final-data/eed-dev_bg.RDS'))

d$ln_M_conc_t1
d$ln_L_conc_t1

#correlation
cor(d$ln_M_conc_t1, d$ln_L_conc_t1, use = "pairwise.complete.obs")
cor(d$ln_M_conc_t2, d$ln_L_conc_t2, use = "pairwise.complete.obs")
cor(d$ln_M_conc_t3, d$ln_L_conc_t3, use = "pairwise.complete.obs")


cor.test(d$ln_M_conc_t1, d$ln_L_conc_t1, use = "pairwise.complete.obs")
cor.test(d$ln_M_conc_t2, d$ln_L_conc_t2, use = "pairwise.complete.obs")
cor.test(d$ln_M_conc_t3, d$ln_L_conc_t3, use = "pairwise.complete.obs")



dk <- readRDS(here('final-data/eed-dev_k.RDS'))


#correlation
cor(dk$ln_Mann1, dk$ln_Lact1, use = "pairwise.complete.obs")
cor(dk$ln_Mann2, dk$ln_Lact2, use = "pairwise.complete.obs")
cor(dk$ln_Mann3, dk$ln_Lact3, use = "pairwise.complete.obs")


cor.test(dk$ln_Mann1, dk$ln_Lact1, use = "pairwise.complete.obs")
cor.test(dk$ln_Mann2, dk$ln_Lact2, use = "pairwise.complete.obs")
cor.test(dk$ln_Mann3, dk$ln_Lact3, use = "pairwise.complete.obs")

rm(list=ls())
source(here::here("0-config.R"))
library(tibble)
data <- tibble(x = -10:100, y= -10:100)
head(data)

d <- readRDS(here('final-data/eed-dev_k.RDS'))


exposures_y1 <- c("ln_aat1", 
                  "ln_mpo1", 
                  "ln_neo1", 
                  "ln_Lact1", 
                  "ln_Mann1")
outcomes_y1 <- c("who_sum_total", "who_sub_total")
exposures_y2 <- c("ln_aat2", 
                  "ln_mpo2", 
                  "ln_neo2", 
                  "ln_Lact2", 
                  "ln_Mann2")
outcomes_y2 <- c("z_comtot_no4_activec", "z_mottot_no4_activec", 
                 "z_pstot_no4_activec", "z_globaltot_no4_activec")

#function for filtering for only participants with at least one outcome
filtering <- function(row){
  any(!is.na(row))}

y1_has_exposures<-d[apply(select(d, all_of(exposures_y1)), 1, filtering),]
y1_has_both<-y1_has_exposures[apply(select(y1_has_exposures, all_of(outcomes_y1)), 1, filtering),]
y1_clusters<-length(unique(y1_has_both$clusterid))
y1_n<-nrow(y1_has_both)

if(is.null(exposures_y2)){
  y2_has_exposures <- y1_has_exposures
}else{
  y2_has_exposures <- d[apply(select(d, all_of(exposures_y2)), 1, filtering),]
}
y2_has_both<-y2_has_exposures[apply(select(y2_has_exposures, all_of(outcomes_y2)), 1, filtering),]
y2_clusters<-length(unique(y2_has_both$clusterid))
y2_n<-nrow(y2_has_both)

data %>% 
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(-10, 100, 10)) +
  theme_void() ->
  p

p +
  geom_rect(xmin = 25, xmax=75, ymin=96, ymax=100, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=98,label= '2,569 villages assessed for eligibility', size=3) ->
  p

p +
  geom_rect(xmin = 54, xmax=92, ymin=84, ymax=94, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 73, y=89, label= 'Excluded: 1,343 villages \n 646 villages did not meet enrollment criteria\n 737 villages did not have enough pregnant women\n Enrolled: 1,226 villages', size=3) +
  annotate('text', x= 10, y=89,label= 'Enrollment', size=4) +
  
  
  geom_rect(xmin = 20, xmax=80, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=79,label= '702 clusters created and randomly allocated across 8 arms \n 8,246 households randomly allocated across 8 arms \n 4 of 8 arms selected into substudy', size=3)  +
  annotate('text', x= 10, y=79,label= 'Allocation', size=4) +
  
  geom_rect(xmin = 27, xmax=73, ymin=66, ymax=74, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=70,label= "paste(bold('                 Control arm, Nutrition arm, \n.  Water + Sanitation + Handwashing arm, and\nNutrition + Water + Sanitation + Handwashing arm'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=68.5,label= '\n391 clusters \n 4,595 households', size=3) +
  annotate('text', x= 10, y=67,label= 'Subsample Target', size=4) +
  
  
  # geom_rect(xmin = 76, xmax=96, ymin=58, ymax=68, color='black',
  #           fill='white', size=0.25) +
  # annotate('text', x= 86, y=64.8,label= "paste(bold('Number of clusters not \n selected into substudy'))", parse=TRUE, size=3) + 
  # annotate('text', x= 86, y=63.4,label= "paste(bold('Month 6 '))", parse=TRUE, size=3) + 
  # annotate('text', x= 86, y=62.1,label= '139 clusters', size=3) + 
  # annotate('text', x= 86, y=60.9,label= "paste(bold('Month 17'))", parse=TRUE, size=3) + 
  # annotate('text', x= 86, y=59.7,label= '135 clusters', size=3) +
  
  geom_rect(xmin = 42, xmax=58, ymin=58, ymax=64, color='black',
            fill='white', size=0.25) +
  annotate('text', x=50, y=62.5,label= "paste(bold('Targeted'))", parse=TRUE, size=3) + 
  annotate('text', x=50, y=61,label= '\n190 clusters\n2,304 households', size=3) +
  
  
  geom_rect(xmin = 37, xmax=63, ymin=28, ymax=56, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=54.5,label= "paste(bold('Month 6'))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=52,label= '\n\n852 children lost to follow-up \n733 absent \n79 no live birth \n40 child death ', size=3) + 
  annotate('text', x= 50, y=45.5,label= "paste(bold('Month 17'))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=42.5,label= '\n  841 children lost to follow-up \n702 absent \n79 no live birth \n60 child death ', size=3) + 
  annotate('text', x= 50, y=36,label= "paste(bold('Month 22'))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=33,label= '\n  901 children lost to follow-up \n742 absent \n79 no live birth \n80 child death ', size=3) + 
  annotate('text', x= 10, y=42,label= 'Follow-up', size=4) +
  
  geom_rect(xmin = 42, xmax=58, ymin=8, ymax=26, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=24.5,label= "paste(bold('Month 6'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=23.5,label= '\n\n165 clusters \n1,493 children', size=3) +
  annotate('text', x= 50, y=19,label= "paste(bold('Month 17'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=18,label= '\n\n190 clusters \n1,504 children  ', size=3) +
  annotate('text', x= 50, y=13.5,label= "paste(bold('Month 22'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=12,label= '\n\n190 clusters \n1,444 children  ', size=3) +
  annotate('text', x= 10, y=17,label= 'Subsample Attendance', size=4) +


  geom_rect(xmin = 32, xmax=68, ymin=-2, ymax=6, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=4.5,label= "paste(bold('Month 6'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=3.5,label= paste("\n", 1493-y1_n, ' missing maternal exposure or outcome', sep=""), size=3) +
  annotate('text', x= 50, y=1,label= "paste(bold('Month 17'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=6.6,label= paste("\n", 1504-y2_n, ' missing maternal exposure or outcome', sep=""), size=3) +
  annotate('text', x= 50, y=1,label= "paste(bold('Month 22'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=6.6,label= paste("\n", 1444-y2_n, ' missing maternal exposure or outcome', sep=""), size=3) +
  annotate('text', x= 10, y=8,label= 'Specimen Collection', size=4) +
  # 
  # 
  # geom_rect(xmin = 42, xmax=58, ymin=-8, ymax=2, color='black',
  #           fill='white', size=0.25) +
  # annotate('text', x= 50, y=1.2,label= "paste(bold('Month 6'), sep='')", parse=T, size=3) +
  # annotate('text', x= 50, y=.2,label= paste("\n\n", y1_clusters, ' clusters \n ', y1_n, ' children', sep=''), size=3) +
  # annotate('text', x= 50, y=-3.8,label= "paste(bold('Month 17'), sep='')", parse=T, size=3) +
  # annotate('text', x= 50, y=-4.8,label= paste("\n\n", y2_clusters, ' clusters \n ', y2_n, ' children', sep=''), size=3) +
  annotate('text', x= 10, y=-3,label= 'Analysis', size=4) ->
  p


p +
  geom_segment(
    x=50, xend=50, y=96, yend=82, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=54, y=89, yend=89, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=76, yend=74, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=66, yend=64, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # geom_segment(
  #   x=50, xend=76, y=63, yend=63, 
  #   size=0.15, linejoin = "mitre", lineend = "butt",
  #   arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=58, yend=56, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=28, yend=26, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=8, yend=6, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=4, yend=2, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
  p
p

# YOU MAY NEED TO CHANGE THE FILE PATHS HERE
ggsave(p, file = here("figures/k_enrollment-figure.jpg"), height=14, width=9)

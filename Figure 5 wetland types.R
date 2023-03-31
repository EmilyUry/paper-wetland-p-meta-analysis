

#' ---
#' title: "Figure 5. Box plots of wetland type"
#' author: "Emily Ury"
#' date: "March 30, 2023"
#' ---
#' 

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop

library(ggplot2)
library(tidyverse)
library(cowplot)
library(rstatix)

x <- read.csv("Wetland_P_Clean3.csv", header = T)

## data set-up
{

  x$Water_regime <- as.factor(x$Water_regime)
  lX<-log(x[,c(11,13, 16,17)])
  colnames(lX)<-paste("log",colnames(lX),sep="")
  x<-cbind(x,lX); rm(lX)
  
  ## mass at outflow
  x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
  x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)
  
  ### Hydraulic loading rate
  x$HLR <- x$Inflow_m3_yr/x$Area_m2
  
  x$TP_retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
  x$SRP_retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out
}

x$Catchment_Type <- as.factor(x$Catchment_Type)
x$group <- ifelse(x$Catchment_Type == "WWTP", "WWT", 
                  ifelse(x$Wetland_Type == "Mesocosm", "Mesocosm", "Restored/\nConstructed"))


x$Water_regime <- as.factor(x$Water_regime)
table(x$group, x$Water_regime)

x$WR <- ifelse(x$Water_regime == "continuous, constant", "Continuous\nregulated", 
               ifelse(x$Water_regime == "continuous, variable", "Continuous\nunregulated",
                      ifelse(x$Water_regime == "intermittent, constant", "Intermittent\nregulated",
                             ifelse(x$Water_regime == "intermittent, variable", "Intermittent\nunregulated", "other"))))

table(x$WR)
table(x$Water_regime)

x$flow <- ifelse(x$Water_regime == "continuous, constant", "Regulated", 
               ifelse(x$Water_regime == "continuous, variable", "Unregulated",
                      ifelse(x$Water_regime == "intermittent, constant", "Regulated",
                             ifelse(x$Water_regime == "intermittent, variable", "Unregulated", "other"))))
table(x$flow)

x$flow2 <- ifelse(x$Water_regime == "continuous, constant", "Continuous", 
                  ifelse(x$Water_regime == "continuous, variable", "Continuous",
                         ifelse(x$Water_regime == "intermittent, constant", "Intermittent",
                                ifelse(x$Water_regime == "intermittent, variable", "Intermittent", "other"))))

y <- x[which(x$WR != "other"),]
table(y$group, y$flow)

summary <- y %>%
  group_by(flow, group) %>%
  summarise(count = n()) %>%
  mutate(TP_Retention_percent = -178,
         SRP_Retention_percent = -178)

sig <- y %>%
  group_by(flow, group) %>%
  summarise(count = n()) %>%
  mutate(TP_Retention_percent = 115,
         SRP_Retention_percent = 115)




### stats

y %>%
  group_by(group) %>%
  get_summary_stats(TP_Retention_percent, type = "median_mad")


y %>%
  group_by(group) %>%
  get_summary_stats(SRP_Retention_percent, type = "median_mad")


y %>%
  group_by(group) %>%
  get_summary_stats(TP_load_in_g_m2_yr, type = "median_mad")


y %>%
  group_by(group) %>%
  get_summary_stats(SRP_load_in_g_m2_yr, type = "median_mad")



y$newgroup <- paste(y$group, y$flow)


y %>%
  group_by(newgroup) %>%
  get_summary_stats(TP_Retention_percent, type = "median_mad")


y %>%
  group_by(newgroup) %>%
  get_summary_stats(SRP_Retention_percent, type = "median_mad")

pwc <- y %>%
  pairwise_t_test(TP_Retention_percent ~  newgroup, p.adjust.method = 'bonferroni')
pwc

pwc <- y %>%
  pairwise_t_test(SRP_Retention_percent ~  newgroup, p.adjust.method = 'bonferroni')
pwc


reg <- y[which(y$flow == "Regulated"),]
ureg <- y[which(y$flow == "Unregulated"),]


pwc <- reg %>%
  pairwise_t_test(TP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
pwc

pwc <- reg %>%
  pairwise_t_test(SRP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
pwc


pwc <- ureg %>%
  pairwise_t_test(TP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
pwc

pwc <- ureg %>%
  pairwise_t_test(SRP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
pwc





pwc <- y %>%
  pairwise_t_test(TP_Retention_percent ~  newgroup, p.adjust.method = 'bonferroni')
pwc

pwc <- y %>%
  pairwise_t_test(SRP_Retention_percent ~  newgroup, p.adjust.method = 'bonferroni')
pwc



##### same plot but with points jitter

### stat differences between all 6 groups at once
sig$TP <- c("A", "A", "A" , "A", "A" , "A")
sig$SRP <- c("A", "A", "AC", "ABC", "BC", "C")

pal <- c('#e5a77f', '#3e783f', '#6fabd9')



pal <- c('#e5a77faa', '#3e783f33', '#6fabd9ee')

a <- ggplot(y, aes(x = group, y = TP_Retention_percent, fill = group )) +
  geom_boxplot(position = position_dodge2(preserve = "single"), outlier.alpha = 0) +
  scale_fill_manual(values = pal) +
  geom_point(position = position_jitterdodge(), alpha = 0.7, size = 2, aes(pch = flow2)) +
  #scale_color_manual(values = c("black", "gray70"))+
  scale_shape_manual(values = c(17,1)) +
  scale_x_discrete(labels = c("M", "R/C", "W")) +
  coord_cartesian(ylim = c(-180,120)) +
  facet_grid(.~flow, space = "free") +
  theme_bw(base_size = 14) +
  geom_text(data = summary, aes(label = count),
            position = position_dodge(width = 1.0), size = 5, color = "gray50")+
  geom_text(data = sig, aes(label = TP), 
            position = position_dodge(width = 1.0), size = 5) +
  ylab("TP Retention (%)") +
  xlab(" ") +
  theme(plot.margin = unit(c(t = 0.3, r = 0.5, b = 0, l = 0.5), "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text( hjust = 0, vjust = 1, size = 13, face = "bold")) +
  ggtitle("A                                                          B")


b <- ggplot(y, aes(x = group, y = SRP_Retention_percent, fill = group )) +
  geom_boxplot(position = position_dodge2(preserve = "single"), outlier.alpha = 0) +
  geom_point(position = position_jitterdodge(), size = 2, alpha = 0.7, aes(pch = flow2)) +
  scale_shape_manual(values = c(17,1)) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(labels = c("M", "R/C", "W")) +
  coord_cartesian(ylim = c(-180,120)) +
  facet_grid(.~flow, space = "free") +
  theme_bw(base_size = 14) +
  geom_text(data = summary, aes(label = count),
            position = position_dodge(width = 1.0), size = 5, color = "gray50")+
  geom_text(data = sig, aes(label = SRP), 
            position = position_dodge(width = 1.0), size = 5) +
  ylab(expression(paste("PO"[4]^"3-", " Retention (%)"))) +
  xlab(" ") +
  theme(plot.margin = unit(c(t = 0.0, r = 0.5, b = 0, l = 0.5), "cm"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,5,-10),
        plot.title = element_text( hjust = 0.0, size = 13, face = "bold")) +
  ggtitle("C                                                         D")


plot_grid(a,b, nrow = 2, labels = c(" ", " "), rel_heights = c(0.85, 1))





tiff(filename = "figures/Type_box_facet_jitter.tif", height=7.5, width=7, units= "in", res=800, compression= "lzw")

plot_grid(a,b, nrow = 2, labels = c(" ", " "), rel_heights = c(0.85,1))

dev.off()




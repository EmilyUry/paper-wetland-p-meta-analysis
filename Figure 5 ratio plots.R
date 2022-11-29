
### ratio plots


### confirms no trends between driver variables and SRP:TP ratio
### except for some differences by wetland type, catchment type, and hydrologic regime


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")

library(rstatix)
library(ggpubr) 
library(tidyr)
library(cowplot)

x <- read.csv("Wetland_P_Clean3.csv", header = T)
#set up
{ x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
  x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"
  x$Water_regime <- as.factor(x$Water_regime)
  lX<-log(x[,c(11,13, 16,17)])
  colnames(lX)<-paste("log",colnames(lX),sep="")
  x<-cbind(x,lX); rm(lX)
  x$ratio <- (x$SRP_outflow_mg_L/x$TP_outflow_mg_L)/(x$SRP_Inflow_mg_L/x$TP_Inflow_mg_L)
  ## mass at outflow
  x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
  x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)
  ### Hydraulic loading rate
  x$HLR <- x$Inflow_m3_yr/x$Area_m2
  x$mag <- ifelse(x$ratio < 1 , "SRP uptake", "SRP magnif.")
  x$source.sink <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "Source", "Sink") 
  
}

## plot options
options(scipen = 99)  ## no sci. notation
par(mfrow = c(1,1))



##### distribution
{
  x$quad <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "< 0", 
                   ifelse(x$TP_Retention_percent < 33 | x$SRP_Retention_percent < 33, "0-33",
                          ifelse(x$TP_Retention_percent < 67 | x$SRP_Retention_percent < 67, "33-67", "67-100")))

  x$source.sink <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "source", "sink")

    mu <- x %>%
    group_by(source.sink) %>%
    summarise(mean = mean(ratio, na.rm = TRUE),
              median = median(ratio, na.rm = TRUE))
  
dist <-  ggplot(x, aes(x = ratio, color = source.sink, fill = source.sink)) +
    geom_density() +
    xlim(0,5) +
    theme_classic(base_size = 9) +
    geom_vline(data = mu, aes(xintercept = median, color = source.sink),
               linetype="dashed", size=1)+
    geom_vline(aes(xintercept=1),
               color="black", linetype="solid", size=0.5) +
    scale_fill_manual(values = c("#bababa55", "#ff000025" )) +
    scale_color_manual(values = c("black", "red" )) +
    theme(legend.title = element_blank(),
          legend.position = c(0.7,0.75),
          legend.text = element_text(size = 10),
          legend.key.size = unit(0.5, 'cm')) +
    xlab("PO4 magnification ratio")
dist  
  
## four quadrant density plot
  
  # mu <- x %>%
  #   group_by(quad) %>%
  #   summarise(mean = mean(ratio, na.rm = TRUE))
  # 
  # dist <- ggplot(x, aes(x = ratio, fill = quad)) +
  #   geom_density(alpha = 0.3) +
  #   xlim(0,5) +
  #   xlab("PO4:TP ratio") +
  #   theme_classic(base_size = 16) +
  #   geom_vline(data = mu, aes(xintercept = mean, color = quad),
  #              linetype="dashed", size=1)+
  #   geom_vline(aes(xintercept=1),
  #              color="black", linetype="solid", size=0.5) +
  #   #scale_fill_manual(values = c("#ffc8c2", "#ffe2c2", "#fffdc2", "#c5ffc2" )) +
  #   scale_fill_manual(values = c("#f5503d", "#f5b53d", "#f5e93d", "#3df580" )) +
  #   scale_color_manual(values = c("#f5503d", "#f5b53d", "#f5e93d", "#3df580" )) +
  #   theme(legend.position = c(0.8,0.5), legend.title = element_blank())
  # dist
  
  
  ### test for sig diff between groups

  
  x %>%
    group_by(source.sink) %>%
    get_summary_stats(ratio, type = "median_mad")
  
  #pairwise comparison T-test with bonferroni adjustment
  pwc <- x %>%
    pairwise_t_test(ratio ~ source.sink, p.adjust.method = "bonferroni")
  pwc ## the source wetlands is significantly different from sink wetlands
  # Show adjusted p-values
  pwc <- pwc %>% add_xy_position(x = "source.sink")
  res.aov <- x %>% anova_test(ratio ~ source.sink)
  
  ggboxplot(x, x = "source.sink", y = "ratio") +
    stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif", tip.length = 0, step.increase = 0.1) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
}


x %>% kruskal_test(ratio ~source.sink)
wilcox.test(ratio~source.sink, data = x)



tt <- x %>% group_by(source.sink) %>%
  wilcox_test(ratio ~ 1, mu = 1)
tt

# Compare means of multiple groups w ANOVA test

res.aov <- x %>% anova_test(ratio ~ Catchment_Type)
res.aov

## pairwise T-test for multiple groups
pwc <- x %>% pairwise_t_test(ratio ~ Catchment_Type, p.adjust.method = "bonferroni") %>% 
  p_round(digits = 2) %>%
  p_format(p.adj, add.p = TRUE, accuracy = 0.0001, space = TRUE)

pwc

tt <- x %>% group_by(Catchment_Type) %>%
  wilcox_test(ratio ~ 1, mu = 1)
tt

A <- ggboxplot(x, x = "Catchment_Type", y = "ratio", ylim = c(0,12.5)) +
  stat_pvalue_manual(pwc, label = "p.adj", y.position = c(9),
                     hide.ns = TRUE)+
  ylab("PO4 magnification ratio") + 
  xlab("Catchment Type") +
  geom_hline(yintercept = 1, linetype = 1, color = "red")+
  annotate("text", x =  1, y = 5, label = "*", size = 12)+
  annotate("text", x =  2, y = 5, label = "*", size = 12)+
  annotate("text", x =  3, y = 5, label = "*", size = 12)+
  scale_x_discrete(labels = c("Ag.", "WWT", "Urban")) +
  theme_pubr(base_size = 9) +
  theme(plot.margin = margin(t = 0, r = 0.2, b = 0.5, l = 0.2, unit = "cm"))
A



####

###wetland type
boxplot(ratio ~ Wetland_Type, data = x, ylim = c(0,4), 
        xlab = "", ylab = "SRP:TP ratio")

# Compare means of multiple groups w ANOVA test
res.aov <- x %>% anova_test(ratio ~ Wetland_Type)
res.aov

## pairwise T-test for multiple groups
pwc <- x %>% pairwise_t_test(ratio ~ Wetland_Type, p.adjust.method = "bonferroni")%>% 
  p_round(digits = 3) %>%
  p_format(p.adj, add.p = TRUE, accuracy = 0.0001, space = TRUE)
pwc

tt <- x %>% group_by(Wetland_Type) %>%
  wilcox_test(ratio ~ 1, mu = 1)
tt

B <- ggboxplot(x, x = "Wetland_Type", y = "ratio", ylim = c(0,12.5)) +
  stat_pvalue_manual(pwc, label = "p.adj", y.position = c(9.5, 8.5), 
                     hide.ns = TRUE)+
  ylab("PO4 magnification ratio") + 
  xlab("Wetland Type")+
  geom_hline(yintercept = 1, linetype = 1, color = "red")+
  annotate("text", x =  2, y = 5, label = "*", size = 12)+
  annotate("text", x =  4, y = 5, label = "*", size = 12)+
  theme_pubr(base_size = 9)



B


###hydrologic regime
levels(x$Water_regime) <- c("Cont. reg.", "Cont. var.", "Int. reg.", "Int. var.", "n.s.")
y <- x[which(x$Water_regime != "n.s."),]
y$Water_regime <-droplevels(y$Water_regime)

# Compare means of multiple groups w ANOVA test
res.aov <- y %>% anova_test(ratio ~ Water_regime)
res.aov

## pairwise T-test for multiple groups
pwc <- y %>% pairwise_t_test(ratio ~ Water_regime, p.adjust.method = "bonferroni") %>% 
  p_round(digits = 3) %>%
  p_format(p.adj, add.p = TRUE, accuracy = 0.0001, space = TRUE)
pwc

tt <- y %>% group_by(Water_regime) %>%
  wilcox_test(ratio ~ 1, mu = 1)
tt

c <- ggboxplot(y, x = "Water_regime", y = "ratio", ylim = c(0,12.5)) +
  stat_pvalue_manual(pwc, label = "p.adj", y.position = c(8.5, 9.5),
                     hide.ns = TRUE)+
  ylab("PO4 magnification ratio") + 
  xlab("Hydrologic regime") +
  geom_hline(yintercept = 1, linetype = 1, color = "red") +
  annotate("text", x =  2, y = 5, label = "*", size = 12)+
  annotate("text", x =  4, y = 5, label = "*", size = 12) +
  scale_x_discrete(labels = c("Continuous\nregulated", "Continuous\nnatural",
                              "Intermittent\nregulated", "Intermittent\nnatural"))+
  theme_pubr(base_size = 9)


c

### transformation figure

top <- plot_grid(dist, B, labels = c("A", "B"), ncol = 2, rel_widths = c(1,1), label_size = 10)
bottom <- plot_grid(A, c, labels = c("C", "D"), ncol = 2, rel_widths = c(2,3), label_size = 10)


tiff(filename = "figures/Fig5_ratio_plots.tif", height=5, width=6, units= "in", res=800, compression= "lzw")
plot_grid(top, bottom, labels = c(" ", " "), ncol = 1, rel_heights = c(1,1))
dev.off()





########### 


plot((x$TP_Inflow_mg_L), x$ratio, log = 'x',
     ylab = "SRP:TP ratio", xlab = "[TP]in (log axis)")
abline(h=1, col = "red")


x %>% ggplot(aes(TP_Inflow_mg_L, y = ratio)) +
  scale_x_log10() + 
  geom_point(data = . %>% filter(TP_Retention_percent > 0), col = 3) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), col = 2)
  
ggplot(x, aes(TP_Inflow_mg_L, y = ratio)) +
  scale_x_log10() + 
  geom_point(data = . %>% filter(TP_Retention_percent > 0), col = 3) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), col = 2)


ggplot(x, aes(TP_Inflow_mg_L, y = ratio)) +
  scale_x_log10() + 
  geom_point(data = . %>% filter(SRP_Retention_percent > 0), col = 3) +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), col = 2)


ggplot(x, aes(TP_Inflow_mg_L, y = ratio)) +
  scale_x_log10() + 
  geom_point(data = . %>% filter(source.sink == "Sink"), col = 3) +
  geom_point(data = . %>% filter(source.sink == "Source"), col = 2)



ggplot(x, aes(SRP_Retention_percent, y = TP_Retention_percent)) +
  xlim(-200,100) + 
  ylim(-200,100) +
  geom_point(data = . %>% filter(ratio < 0.7), col = 2) +
  geom_point(data = . %>% filter(ratio < 1), col = "green") +
  geom_point(data = . %>% filter(ratio > 1), col = "orange") +
  geom_point(data = . %>% filter(ratio > 2), col = "orange")



plot((x$SRP_Inflow_mg_L), x$ratio, log = 'x',
     ylab = "SRP:TP ratio", xlab = "[SRP]in (log axis)")
abline(h=1, col = "red")

plot((x$Area_m2), x$ratio, log = 'x',
     ylab = "SRP:TP ratio", xlab = "Area m2 (log axis)")
abline(h=1, col = "red")

plot((x$Age_yr), x$ratio, #log = 'x', 
     xlim = c(0,18),
     ylab = "SRP:TP ratio", xlab = "Age yrs)")
abline(h=1, col = "red")

plot((x$HLR), x$ratio, log = 'x',
     ylab = "SRP:TP ratio", xlab = "HLR (log axis)")
abline(h=1, col = "red")


x$CWRatio <- x$Catchment_area_ha/x$Area_m2*10000
plot((x$CWRatio), x$ratio, log = 'x',
     ylab = "SRP:TP ratio", xlab = "HLR (log axis)")
abline(h=1, col = "red")










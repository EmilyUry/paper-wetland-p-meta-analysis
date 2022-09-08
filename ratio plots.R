
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
{
  
  x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
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


options(scipen = 99)

par(mfrow = c(1,1))



##### distribution
{
  hist(x$ratio, breaks = 50, xlim = c(0,5))
  abline(v=1, col = "red", lwd = 2)
  text(1.7,55, "SRP \nmagnification", col = "red")
  text(0.3, 55, "SRP \nretention", col = "red")
  arrows(1.3,51, 2.2, 51, length = 0.15, angle = 30, code = 2, col = "red")
  arrows(0.6,51, 0.0, 51, length = 0.15, angle = 30, code = 2, col = "red")
  
  
  x$source.sink <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "source", "sink")
  
  ggplot(x, aes(x = ratio))+
    geom_density() +
    xlim(0,5) +
    theme_classic()+
    geom_vline(aes(xintercept=1),
               color="blue", linetype="dashed", size=1)
  
  mu <- x %>%
    group_by(source.sink) %>%
    summarise(mean = mean(ratio, na.rm = TRUE))
  
  ggplot(x, aes(x = ratio, color = source.sink)) +
    geom_density() +
    xlim(0,5) +
    theme_classic() +
    geom_vline(data = mu, aes(xintercept = mean, color = source.sink),
               linetype="dashed", size=1)+
    geom_vline(aes(xintercept=1),
               color="black", linetype="solid", size=0.5)
  
  
  
  mu <- x %>%
    group_by(quad) %>%
    summarise(mean = mean(ratio, na.rm = TRUE))
  
  dist <- ggplot(x, aes(x = ratio, fill = quad)) +
    geom_density(alpha = 0.3) +
    xlim(0,5) +
    xlab("PO4:TP ratio") +
    theme_classic(base_size = 16) +
    geom_vline(data = mu, aes(xintercept = mean, color = quad),
               linetype="dashed", size=1)+
    geom_vline(aes(xintercept=1),
               color="black", linetype="solid", size=0.5) +
    #scale_fill_manual(values = c("#ffc8c2", "#ffe2c2", "#fffdc2", "#c5ffc2" )) +
    scale_fill_manual(values = c("#f5503d", "#f5b53d", "#f5e93d", "#3df580" )) +
    scale_color_manual(values = c("#f5503d", "#f5b53d", "#f5e93d", "#3df580" )) +
    theme(legend.position = c(0.8,0.5), legend.title = element_blank())
  dist
  
  
  source <- (x[which(x$source.sink == "source"),])
  unique(source$Source)
  unique(x$Source)
  22/50
  
  
  ### test for sig diff between groups
  
  library(rstatix)
  library(ggpubr)
  
  x %>%
    group_by(quad) %>%
    get_summary_stats(ratio, type = "mean_sd")
  
  #pairwise comparison T-test with bonferroni adjustment
  pwc <- x %>%
    pairwise_t_test(ratio ~ quad, p.adjust.method = "bonferroni")
  pwc ## the source group is significantly different from all other groups. sink groups not distinguishable from one another
  # Show adjusted p-values
  pwc <- pwc %>% add_xy_position(x = "quad")
  ggboxplot(x, x = "quad", y = "ratio") +
    stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif", tip.length = 0, step.increase = 0.1) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
  
}














hist(x$ratio)
tt <- x %>%
  wilcox_test(ratio ~ 1, mu = 1)
tt

boxplot(ratio ~ Catchment_Type, data = x, ylim = c(0,8), 
        xlab = "", ylab = "SRP:TP ratio")
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
  ylab("SRP:TP ratio") + 
  geom_hline(yintercept = 1, linetype = 1, color = "red")+
  annotate("text", x =  1, y = 5, label = "*", size = 20)+
  annotate("text", x =  2, y = 5, label = "*", size = 20)+
  annotate("text", x =  3, y = 5, label = "*", size = 20)
A

####
table(x$mag, x$Catchment_Type)
summary <- table(x$mag, x$Catchment_Type)
m <- as.data.frame(summary)
CT <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar( stat = "identity") +
  theme_classic(base_size = 10) +
  labs(x = "SRP:TP ratio ", y = "Frequency ", fill = "Catchment \nType") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
CT

summary <- table(x$mag, x$Catchment_Type, x$source.sink)
m <- as.data.frame(summary)
m$group <- paste(m$Var1, m$Var3)

CT <- ggplot(m, aes(x = group, y = Freq, fill = Var2)) +
  geom_bar( stat = "identity") +
  theme_classic(base_size = 10) +
  labs(x = "SRP:TP ratio ", y = "Frequency ", fill = "Catchment \nType") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
CT


  
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
  ylab("SRP:TP ratio") + 
  geom_hline(yintercept = 1, linetype = 1, color = "red")+
  annotate("text", x =  2, y = 5, label = "*", size = 20)+
  annotate("text", x =  4, y = 5, label = "*", size = 20)


B

### stack bar
table(x$mag, x$Wetland_Type)
summary <- table(x$mag, x$Wetland_Type)
m <- as.data.frame(summary)
WT <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar( stat = "identity") +
  theme_classic(base_size = 10) +
  scale_fill_manual(labels = c("Constructed", "Mesocosm", "Natural", "Restored"), 
                    values = c("#440154FF", "#44015477", "#2c728e55",  "#2c728eFF"  )) +
  labs(x = "SRP:TP ratio ", y = "Frequency ", fill = "Wetland \nType") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
WT


###hydraulic regime
levels(x$Water_regime) <- c("Cont. reg.", "Cont. var.", "Int. reg.", "Int. var.", "n.s.")

boxplot(ratio ~ Water_regime, data = x, ylim = c(0,4), 
        xlab = "", ylab = "SRP:TP ratio")

# Compare means of multiple groups w ANOVA test
res.aov <- x %>% anova_test(ratio ~ Water_regime)
res.aov

## pairwise T-test for multiple groups
pwc <- x %>% pairwise_t_test(ratio ~ Water_regime, p.adjust.method = "bonferroni") %>% 
  p_round(digits = 3) %>%
  p_format(p.adj, add.p = TRUE, accuracy = 0.0001, space = TRUE)
pwc

tt <- x %>% group_by(Water_regime) %>%
  wilcox_test(ratio ~ 1, mu = 1)
tt

c <- ggboxplot(x, x = "Water_regime", y = "ratio", ylim = c(0,12.5)) +
  stat_pvalue_manual(pwc, label = "p.adj", y.position = c(8.5, 9.5),
                     hide.ns = TRUE)+
  ylab("SRP:TP ratio") + 
  geom_hline(yintercept = 1, linetype = 1, color = "red") +
  annotate("text", x =  2, y = 5, label = "*", size = 20)+
  annotate("text", x =  4, y = 5, label = "*", size = 20)

c

table(x$mag, x$Water_regime)
summary <- table(x$mag, x$Water_regime)
m <- as.data.frame(summary)
FlowR <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar( stat = "identity") +
  theme_classic(base_size = 10) +
  scale_fill_manual(labels = c("Continuous,\n  regulated", "Intermittent,\n  regulated", "Continuous,\n  variable", "Intermittent,\n  variable", "Not \n  specified" ),
                    values = c("#440154FF",   "#44015477", "#2c728eFF","#2c728e55",  "#31313122")) +
  labs(x = "Retention bins (%)", y = " ", fill = "Hydrologic\n Regime") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
  guides(fill=guide_legend(ncol=1))
FlowR


### transformation figure


left <- plot_grid(A, B, labels = c("A", "B"), ncol = 2, rel_widths = c(1,2))
left

tiff(filename = "figures/Fig6_ratio_plots.tif", height=5000, width=5500, units= "px", res=800, compression= "lzw")
plot_grid(left, c, labels = c(" ", "C"), ncol = 1, rel_heights = c(1,1))
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










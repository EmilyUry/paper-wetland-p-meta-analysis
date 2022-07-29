
### ratio plots

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")

x <- read.csv("Wetland_P_Clean2.csv", header = T)
{
  x$Water_regime <- as.factor(x$Water_regime)
  
  lX<-log(x[,c(11,13, 16,17)])
  colnames(lX)<-paste("log",colnames(lX),sep="")
  x<-cbind(x,lX); rm(lX)
  
  x$ratio <- x$SRP_Retention_percent/x$TP_Retention_percent
  
  ## mass at outflow
  x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
  x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)
  
  ### Hydraulic loading rate
  x$HLR <- x$Inflow_m3_yr/x$Area_m2
  
  
  ### mass removed
  x$TP_retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
  x$SRP_retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out
  

}

options(scipen = 99)

par(mfrow = c(1,1))



x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
x <- x[which(x$ratio < 1),]
x$ratio <- x$SRP_Inflow_mg_L/x$TP_Inflow_mg_L
boxplot(ratio ~ Catchment_Type, data = x, ylim = c(0,1.1), 
        xlab = "", ylab = "SRP:TP ratio")

# Compare means of multiple groups w ANOVA test
library(rstatix)
res.aov <- x %>% anova_test(ratio ~ Catchment_Type)
res.aov

## pairwise T-test for multiple groups
pwc <- x %>% pairwise_t_test(ratio ~ Catchment_Type, p.adjust.method = "bonferroni")
pwc


boxplot(ratio ~ Wetland_Type, data = x, ylim = c(0,1.1), 
        xlab = "", ylab = "SRP:TP ratio")

# Compare means of multiple groups w ANOVA test
library(rstatix)
res.aov <- x %>% anova_test(ratio ~ Wetland_Type)
res.aov

## pairwise T-test for multiple groups
pwc <- x %>% pairwise_t_test(ratio ~ Wetland_Type, p.adjust.method = "bonferroni")
pwc


text(c(1,2,3,4), 1.075, c("A", "B", "A", "A"), font = 2)



plot(x$ratio, (x$TP_retention))
plot(x$ratio, x$TP_Retention_percent)

plot(x$ratio, (x$SRP_retention))
plot(x$ratio, x$SRP_Retention_percent)




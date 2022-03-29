
#' ---
#' title: "Wetland-P-Meta-analysis scratch figures"
#' author: "Emily Ury"
#' date: "March 14, 2022"
#' output: github_document
#' ---
#' 
#' Scratch figures
#'   (a) 

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")


library(ggplot2)
library(tidyverse)
library(viridis)
library(gridExtra)

## Data set-up
x <- read.csv("Wetland_P_Clean.csv", header = T)
{head(x)

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



## flow regime


table(x$Water_regime)
levels(x$Water_regime)

plot(x$Area_m2, x$TP_Retention_percent, col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb")[x$Water_regime],
     log = "x", 
     ylim = c(-300, 105),
     pch = 16, 
     cex = 1.5,
     xlab = "Wetland area (m2)", 
     ylab = "TP % Retention")
abline(h=0, lty = 2)
legend("bottomleft", levels(x$Water_regime), 
       pch = 16, pt.cex = 1.5,
       ncol = 2,
       col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb"))


plot(x$Area_m2, x$SRP_Retention_percent, col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb")[x$Water_regime],
     log = "x", 
     ylim = c(-300, 105),
     pch = 16, 
     cex = 1.5,
     xlab = "Wetland area (m2)", 
     ylab = "SRP % Retention")
abline(h=0, lty = 2)
legend("bottomleft", levels(x$Water_regime), 
       pch = 16, pt.cex = 1.5,
       col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb"))






## normalize retention by looking at mass export over mass input

x$TP_export_norm <- x$TP_load_out/x$TP_load_in_g_m2_yr
hist(log(x$TP_export_norm))

x$SRP_export_norm <- x$SRP_load_out/x$SRP_load_in_g_m2_yr
hist(log(x$SRP_export_norm))

plot(x$Area_m2, x$TP_export_norm, col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb")[x$Water_regime],
     log = "x", 
     pch = 16, 
     cex = 1.5,
     xlab = "Wetland area (m2)", 
     ylab = "TP Export/Input")
abline(h=1, lty = 2)
legend("topleft", levels(x$Water_regime), 
       pch = 16, pt.cex = 1.5,
       ncol = 1,
       col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb"))


plot(x$Area_m2, x$SRP_export_norm, col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb")[x$Water_regime],
     log = "xy", 
     pch = 16, 
     cex = 1.5,
     xlab = "Wetland area (m2)", 
     ylab = "SRP Export/Input")
abline(h=1, lty = 2)
legend("topleft", levels(x$Water_regime), 
       pch = 16, pt.cex = 1.5,
       col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb"))







plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     col = "#515151bb",
     col = c("#a1a1a1bb", "#bd4ad4bb", "#345bebbb", "#e34327bb","#2b821fbb")[x$Water_regime],
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomleft", levels(x$Water_regime), pch = 16,
       pt.cex = 2, col = c("#a1a1a1bb", "#bd4ad4bb", "#345bebbb", "#e34327bb","#2b821fbb"))


## mean SRP retention
summary(x$SRP_Retention_percent)

summary(x$TP_Retention_percent)


plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     col = c( "#e34327bb","#2b821fbb", "#345bebbb")[x$Catchment_Type],
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomleft", levels(x$Catchment_Type), pch = 16,
       pt.cex = 2, col = c(  "#e34327bb","#2b821fbb","#345bebbb"))









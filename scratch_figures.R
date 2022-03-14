
#' ---
#' title: "Wetland-P-Meta-analysis scratch figures"
#' author: "Emily Ury"
#' date: "Feb 15, 2021"
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


## K~Tau (Fred's equations)
{
x$Tau <- 1.51*x$Area_m2^0.23 ## using constants from Cheng and Basu 2017
hist(x$Tau)

## calculate k (rate constant) TP
x$k2 <- -(log(1-(x$TP_Retention_percent/100))/x$Tau)
fit <- lm(log(x$k2) ~ log(x$Tau))
summary(fit)

x$k3 <- -(log(1-(x$SRP_Retention_percent/100))/x$Tau)
fit <- lm(x$k3 ~ x$k2)
summary(fit)

### plot TP K~tau

plot((x$Tau), (x$k2), log = 'xy', pch = 16, 
     xlab = expression(paste("Water Residence Time, ", tau, " (d)")), 
     ylab = expression(paste("Rate Constant, k ( ", d^-1, ")")))
x1 <- seq(0,120, by = 0.1)
y1 <- exp(-1.1235)*x1^(-0.812)
points(x1,y1, col = "black", type = 'l')
text(4,0.001, expression(bold(paste("TP, k = 0.33", tau)^-0.81)), cex = 0.8)
text(4,0.00055, expression(bold(paste(" ",R^2, " = 0.32, p < 0.001"))), cex = 0.8)
text(4,0.0003, "(57 points omited)", cex = 0.8, font = 2)

## Add srp on top

points((x$Tau), (x$k3), pch = 16, cex = 0.8, col = "#FF0000aa")
x1 <- seq(0,120, by = 0.1)
y1 <- exp(-1.54)*x1^(-0.532)
points(x1,y1, col = "red", type = 'l')
text(48,0.5, expression(bold(paste("SRP, k = 0.21", tau)^-0.53)), cex = 0.8, col = 'red')
text(48,0.3, expression(bold(paste(" ",R^2, " = 0.15, p < 0.001"))), cex = 0.8, col = 'red')
text(48,0.2, "(87 points omited)", cex = 0.8, font = 2, col = "red")

}



nrow(x[which(x$TP_Retention_percent < 0),])
nrow(x[which(x$SRP_Retention_percent < 0),])


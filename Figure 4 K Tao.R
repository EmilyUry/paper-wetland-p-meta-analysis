

#' ---
#' title: "Figure 4 K~Tau"
#' author: "Emily Ury"
#' date: "July 29, 2022"
#' output: github_document
#' ---
#' 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


library(ggplot2)
library(tidyverse)
#library(gridExtra)

## Data set-up
x <- read.csv("Wetland_P_Clean3.csv", header = T)

x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"



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
  x$Tau <- 1.51*x$Area_m2^0.23 ## using constants from Cheng and Basu 2017
  hist(x$Tau)
  
  plot(x$HRT_d, x$Tau)
  fit <- lm(x$Tau ~ x$HRT_d)
  abline(fit)
  summary(fit)   ## p = 0.14, R = 0.007
  
  
  
  plot(log(x$Area_m2), x$HRT_d) #xlim = c(0,200000))
  fit <- lm(x$HRT_d ~ x$Area_m2)
  abline(fit)
  summary(fit)
  
  
  abline(a = 1.5, b=0.23)
  
  
  ##  omit outliers
  
  y <- x[which(x$Area_m2 < 10000 & x$Area_m2 != 100),]
  plot((y$Area_m2), y$HRT_d)
  fit <- lm(y$HRT_d ~ y$Area_m2)
  abline(fit)
  summary(fit)
  
  x$Tau2 <- 0.0018376*x$Area_m2 + 6.044 ### bad
  
  
  y <- x[which(x$Area_m2 != 100),]
  plot(log(y$Area_m2), y$HRT_d)
  fit <- lm(y$HRT_d ~ log(y$Area_m2))
  summary(fit)
  exponent <- coef(fit)[1]
  constant <- exp(coef(fit)[2])
  abline(4.65, (0.6945))
  
  y$Tau3 <- 4.65*y$Area_m2^0.6945
  
  plot(y$HRT_d, y$Tau3)
  
  
  ## calculate k (rate constant) TP  
  x$k2 <- -(log(1-(x$TP_Retention_percent/100))/x$HRT_d)   ## change to measured
  fit <- lm(log(x$k2) ~ log(x$HRT_d))
  summary(fit)
  exponent <- coef(fit)[1]
  constant <- exp(coef(fit)[2])
  
  ### plot TP K~tau
  par(mfrow = c(1,1), mar = c(5,6,2,3))
  
  #tiff(filename = "figures/K_Tau_plot.tiff", height=3600, width=3600, units= "px", res=800, compression= "lzw")
  
  
  plot((x$HRT_d), (x$k2), log = 'xy', pch = 16, cex = 0.7, xlim = c(1.2,115),
       xlab = expression(paste("Water Residence Time, ", tau, " (d)")), 
       ylab = expression(paste("Rate Constant, k ( ", d^-1, ")")))
  x1 <- seq(0,150, by = 0.1)
  y1 <- constant*x1^exponent
  points(x1,y1, col = "gray20", type = 'l')
  text(4,0.001, expression(bold(paste("TP, k = 0.36", tau)^-0.86)), cex = 0.8)
  text(4,0.00055, expression(bold(paste(" ",R^2, " = 0.46, p < 0.0001"))), cex = 0.8)
  nrow(x[which(x$TP_Retention_percent < 0),])
  text(4,0.0003, "(57 points omited)", cex = 0.8, font = 2)
  
  
  ## Add srp on top
  
  x$k3 <- -(log(1-(x$SRP_Retention_percent/100))/x$HRT_d)
  fit1 <- lm(log(x$k3) ~ log(x$HRT_d))
  summary(fit1)
  exponent1 <- coef(fit1)[1]
  constant1 <- exp(coef(fit1)[2])
  
  
  points((x$Tau), (x$k3), pch = 16, cex = 0.6, col = "#FF0000aa")
  x2 <- seq(0,150, by = 0.1)
  y2 <- constant1*x1^exponent1
  points(x2,y2, col = "red", type = 'l')
  text(48,0.5, expression(bold(paste("SRP, k = 0.52", tau)^-1.4)), cex = 0.8, col = 'red')
  text(48,0.3, expression(bold(paste(" ",R^2, " = 0.24, p < 0.0001"))), cex = 0.8, col = 'red')
  nrow(x[which(x$SRP_Retention_percent < 0),])
  text(48,0.17, "(87 points omited)", cex = 0.8, font = 2, col = "red")
  
  




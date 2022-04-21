

#' ---
#' title: "Driver analysis of inflow P load/concentration"
#' author: "Emily Ury"
#' last update: "April 19, 2022"
#' ---
#' 
#' Drivers of P retention including 
#'    1) Wetland size
#'    2) inlet concentration
#'    3) flow regime
#'    
#'    


library(ggplot2)
library(tidyverse)


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


## Data set-up
x <- read.csv("Wetland_P_Clean2.csv", header = T)
{ lX<-log(x[,c(11,13, 16,17)])  ## area, inflow vol, inflow conc. TP/RP
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
  
  x$Water_regime <- as.factor(x$Water_regime)
}




#### inflow concentration

## TP
x$Pret <- as.factor(ifelse(x$TP_Retention_percent > 0, "pos", "neg"))


par(mfrow = c(2,2))

### inflow concentration
plot(x$TP_Inflow_mg_L, x$TP_retention, pch = 16, log = "x",
     xlab = "Inflow TP concentration (mg/L)", ylab = "TP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$Pret])

plot(x$TP_Inflow_mg_L, x$TP_Retention_percent, pch = 16, log = "x",
     xlab = "Inflow TP concentration (mg/L)", ylab = "TP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$Pret])

## inflow load
plot(x$TP_load_in_g_m2_yr, x$TP_retention, pch = 16, log = "x",
     xlab = "Inflow TP load (g/m2/yr)", ylab = "TP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$Pret])

plot(x$TP_load_in_g_m2_yr, x$TP_Retention_percent, pch = 16, log = "x",
     xlab = "Inflow TP load (g/m2/yr)", ylab = "TP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$Pret])


par(mfrow = c(2,2))

### inflow concentration
plot(x$TP_Inflow_mg_L, x$TP_retention, pch = 16, log = "xy",
     xlab = "Inflow TP concentration (mg/L)", ylab = "TP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$Pret])

plot(x$TP_Inflow_mg_L, x$TP_Retention_percent, pch = 16, log = "xy",
     xlab = "Inflow TP concentration (mg/L)", ylab = "TP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$Pret])

## inflow load
plot(x$TP_load_in_g_m2_yr, x$TP_retention, pch = 16, log = "xy",
     xlab = "Inflow TP load (g/m2/yr)", ylab = "TP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$Pret])

plot(x$TP_load_in_g_m2_yr, x$TP_Retention_percent, pch = 16, log = "xy",
     xlab = "Inflow TP load (g/m2/yr)", ylab = "TP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$Pret])





## SRP
x$SRPret <- as.factor(ifelse(x$SRP_Retention_percent > 0, "pos", "neg"))


par(mfrow = c(2,2))

### inflow concentration
plot(x$SRP_Inflow_mg_L, x$SRP_retention, pch = 16, log = "x",
     xlab = "Inflow SRP concentration (mg/L)", ylab = "SRP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])

plot(x$SRP_Inflow_mg_L, x$SRP_Retention_percent, pch = 16, log = "x",
     xlab = "Inflow SRP concentration (mg/L)", ylab = "SRP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])

## inflow load
plot(x$SRP_load_in_g_m2_yr, x$SRP_retention, pch = 16, log = "x",
     xlab = "Inflow SRP load (g/m2/yr)", ylab = "SRP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])

plot(x$SRP_load_in_g_m2_yr, x$SRP_Retention_percent, pch = 16, log = "x",
     xlab = "Inflow SRP load (g/m2/yr)", ylab = "SRP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])


par(mfrow = c(2,2))

### inflow concentration
plot(x$SRP_Inflow_mg_L, x$SRP_retention, pch = 16, log = "xy",
     xlab = "Inflow SRP concentration (mg/L)", ylab = "SRP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])

plot(x$SRP_Inflow_mg_L, x$SRP_Retention_percent, pch = 16, log = "xy",
     xlab = "Inflow SRP concentration (mg/L)", ylab = "SRP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])

## inflow load
plot(x$SRP_load_in_g_m2_yr, x$SRP_retention, pch = 16, log = "xy",
     xlab = "Inflow SRP load (g/m2/yr)", ylab = "SRP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])

plot(x$SRP_load_in_g_m2_yr, x$SRP_Retention_percent, pch = 16, log = "xy",
     xlab = "Inflow SRP load (g/m2/yr)", ylab = "SRP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])












## trying something weird with absolute values
par(mfrow = c(2,2))

### inflow concentration
plot(x$SRP_Inflow_mg_L, abs(x$SRP_retention), pch = 16, log = "xy",
     xlab = "Inflow SRP concentration (mg/L)", ylab = "SRP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])

plot(x$SRP_Inflow_mg_L, abs(x$SRP_Retention_percent), pch = 16, log = "xy",
     xlab = "Inflow SRP concentration (mg/L)", ylab = "SRP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])

## inflow load
plot(x$SRP_load_in_g_m2_yr, abs(x$SRP_retention), pch = 16, log = "xy",
     xlab = "Inflow SRP load (g/m2/yr)", ylab = "SRP retention (g/m2/year)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])

plot(x$SRP_load_in_g_m2_yr, abs(x$SRP_Retention_percent), pch = 16, log = "xy",
     xlab = "Inflow SRP load (g/m2/yr)", ylab = "SRP retention (%)",
     col = c("#FF000099", "#0000FF99")[x$SRPret])




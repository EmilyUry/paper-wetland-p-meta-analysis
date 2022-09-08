

##### Scatter plots and stat tests for Fig 3


#' ---
#' title: "Scatter plots and stat tests for Fig 3
#' author: "Emily Ury"
#' date: "July 25, 2022"
#' ---

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


library(ggplot2)
library(tidyverse)
library(cowplot)


## Data set-up
x <- read.csv("Wetland_P_Clean3.csv", header = T)
{
  
  x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
  x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"
  
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
  
  x$CWRatio <- x$Catchment_area_ha/x$Area_m2*10000
  
}


### quartiles w shading

x$quad <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "< 0", 
                 ifelse(x$TP_Retention_percent < 33 | x$SRP_Retention_percent < 33, "0-33",
                        ifelse(x$TP_Retention_percent < 67 | x$SRP_Retention_percent < 67, "33-67", "67-100")))

quad.sum <- table(x$quad)
n <- as.data.frame(quad.sum)

x$TP.S <- ifelse(x$TP_Retention_percent < 0, "source", "sink")
x$SRP.S <- ifelse(x$SRP_Retention_percent < 0, "source", "sink")


TP.sink <- x[which(x$TP.S == "sink"),]
TP.source <- x[which(x$TP.S == "source"),]


SRP.sink <- x[which(x$SRP.S == "sink"),]
SRP.source <- x[which(x$SRP.S == "source"),]

#### INFLOW TP CONCENTRATION

TPsink <- lm(TP_Retention_percent ~ (TP_Inflow_mg_L), TP.sink)
summary(TPsink)
TPsource <- lm(TP_Retention_percent ~ TP_Inflow_mg_L, TP.source)
summary(TPsource)

ggplot(x, aes(x = log(TP_Inflow_mg_L), y = TP_Retention_percent, color = TP.S)) +
  geom_point() +
  ylim(-150, 115) +
  geom_segment(aes(x = -4, y = 0, xend = 4, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm') +
  annotate(geom = "text", x = 2.5, y = 115, label = 'bold("R2 = 0.02, p = 0.027")',
           color = "#F8766D", parse = TRUE) +
  annotate(geom = "text", x = 2.5, y = 103, label = 'bold("R2 = 0.04, p = 0.11")', 
           color = "#00BFC4", parse = TRUE) 

#### INFLOW SRP CONCENTRATION

SRPsink <- lm(SRP_Retention_percent ~ SRP_Inflow_mg_L, SRP.sink)
summary(SRPsink)
SRPsource <- lm(SRP_Retention_percent ~ SRP_Inflow_mg_L, SRP.source)
summary(SRPsource) 

ggplot(x, aes(x = log(SRP_Inflow_mg_L), y = SRP_Retention_percent, color = SRP.S)) +
  geom_point() +
  ylim(-150, 115) +
  geom_segment(aes(x = -6, y = 0, xend = 5, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm') +
  annotate(geom = "text", x = 2.5, y = 115, label = 'bold("R2 = 0.07, p = 0.0001")',
           color = "#F8766D", parse = TRUE) +
  annotate(geom = "text", x = 2.5, y = 103, label = 'bold("R2 = 0.04, p = 0.07")', 
           color = "#00BFC4", parse = TRUE)





### WETLAND SIZE - TP


TPsink <- lm(TP_Retention_percent ~ log(Area_m2), TP.sink)
summary(TPsink)
TPsource <- lm(TP_Retention_percent ~ log(Area_m2), TP.source)
summary(TPsource)

ggplot(x, aes(x = log(Area_m2), y = TP_Retention_percent, color = TP.S)) +
  geom_point() +
  ylim(-150, 115) +
  geom_segment(aes(x = 0, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm') +
  annotate(geom = "text", x = 15, y = 115, label = 'bold("R2 = 0.006, p = 0.12")',
           color = "#F8766D", parse = TRUE) +
  annotate(geom = "text", x = 15, y = 103, label = 'bold("R2 = 0.034, p = 0.12")', 
           color = "#00BFC4", parse = TRUE) 

#### WETLAND SIZE -  SRP 

SRPsink <- lm(SRP_Retention_percent ~ log(Area_m2), SRP.sink)
summary(SRPsink)
SRPsource <- lm(SRP_Retention_percent ~ log(Area_m2), SRP.source)
summary(SRPsource) 

ggplot(x, aes(x = log(Area_m2), y = SRP_Retention_percent, color = SRP.S)) +
  geom_point() +
  ylim(-150, 115) +
  geom_segment(aes(x = 0, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm') +
  annotate(geom = "text", x = 15, y = 115, label = 'bold("R2 = 0.07, p < 0.0001")',
           color = "#F8766D", parse = TRUE) +
  annotate(geom = "text", x = 15, y = 103, label = 'bold("R2 = 0.02, p = 0.10")', 
           color = "#00BFC4", parse = TRUE)

  


### WETLAND AGE



TPsink <- lm(TP_Retention_percent ~ Age_yr, TP.sink)
summary(TPsink)
TPsource <- lm(TP_Retention_percent ~ Age_yr, TP.source)
summary(TPsource)

ggplot(x, aes(x = Age_yr, y = TP_Retention_percent, color = TP.S)) +
  geom_point() +
  ylim(-150, 115) +
  xlim(0,20) +
  geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm') +
  annotate(geom = "text", x = 15, y = 115, label = 'bold("R2 = 0.00, p = 0.5")',
           color = "#F8766D", parse = TRUE) +
  annotate(geom = "text", x = 15, y = 103, label = 'bold("R2 = 0.00, p = 0.9")', 
           color = "#00BFC4", parse = TRUE) 

#### WETLAND AGE -  SRP 

SRPsink <- lm(SRP_Retention_percent ~ Age_yr, SRP.sink)
summary(SRPsink)
SRPsource <- lm(SRP_Retention_percent ~ Age_yr, SRP.source)
summary(SRPsource) 

ggplot(x, aes(x = Age_yr, y = SRP_Retention_percent, color = SRP.S)) +
  geom_point() +
  ylim(-150, 115) +
  xlim(0,20) +
  geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm') +
  annotate(geom = "text", x = 15, y = 115, label = 'bold("R2 = 0.005, p = 0.15")',
           color = "#F8766D", parse = TRUE) +
  annotate(geom = "text", x = 15, y = 103, label = 'bold("R2 = 0.01, p = 0.16")', 
           color = "#00BFC4", parse = TRUE)






#### HLR





TPsink <- lm(TP_Retention_percent ~ HLR, TP.sink)
summary(TPsink)
TPsource <- lm(TP_Retention_percent ~ HLR, TP.source)
summary(TPsource)

ggplot(x, aes(x = HLR, y = TP_Retention_percent, color = TP.S)) +
  geom_point() +
  ylim(-150, 115) +
  xlim(0,300) +
  geom_segment(aes(x = 0, y = 0, xend = 300, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm') +
  annotate(geom = "text", x = 250, y = 115, label = 'bold("R2 = 0.02, p = 0.015")',
           color = "#F8766D", parse = TRUE) +
  annotate(geom = "text", x = 250, y = 103, label = 'bold("R2 = 0.00, p = 0.5")', 
           color = "#00BFC4", parse = TRUE) 

#### HLR -  SRP 

SRPsink <- lm(SRP_Retention_percent ~ HLR, SRP.sink)
summary(SRPsink)
SRPsource <- lm(SRP_Retention_percent ~ HLR, SRP.source)
summary(SRPsource) 

ggplot(x, aes(x = HLR, y = SRP_Retention_percent, color = SRP.S)) +
  geom_point() +
  ylim(-150, 115) +
  xlim(0,300) +
  geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm') +
  annotate(geom = "text", x = 250, y = 115, label = 'bold("R2 = 0.03, p = 0.008")',
           color = "#F8766D", parse = TRUE) +
  annotate(geom = "text", x = 250, y = 103, label = 'bold("R2 = 0.001, p = 0.3")', 
           color = "#00BFC4", parse = TRUE)





#### CATCHMENT:WETLAND AREA ratio
  

  TPsink <- lm(TP_Retention_percent ~ log(CWRatio), TP.sink)
  summary(TPsink)
  TPsource <- lm(TP_Retention_percent ~ log(CWRatio), TP.source)
  summary(TPsource)
  
  ggplot(x, aes(x = log(CWRatio), y = TP_Retention_percent, color = TP.S)) +
    geom_point() +
    ylim(-150, 115) +
    geom_segment(aes(x = 1, y = 0, xend = 7, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm') +
    annotate(geom = "text", x = 6, y = 115, label = 'bold("R2 = 0.17, p < 0.0001")',
             color = "#F8766D", parse = TRUE) +
    annotate(geom = "text", x = 6, y = 103, label = 'bold("R2 = 0.00, p = 0.8")', 
             color = "#00BFC4", parse = TRUE) 
  
  #### CATCHMENT:WETLAND AREA  -  SRP 
  
  SRPsink <- lm(SRP_Retention_percent ~ log(CWRatio), SRP.sink)
  summary(SRPsink)
  SRPsource <- lm(SRP_Retention_percent ~ log(CWRatio), SRP.source)
  summary(SRPsource) 
  
  ggplot(x, aes(x = log(CWRatio), y = SRP_Retention_percent, color = SRP.S)) +
    geom_point() +
    ylim(-150, 115) +
    geom_segment(aes(x = 1, y = 0, xend = 7, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm') +
    annotate(geom = "text", x = 6, y = 115, label = 'bold("R2 = 0.26, p < 0.0001")',
             color = "#F8766D", parse = TRUE) +
    annotate(geom = "text", x = 6, y = 103, label = 'bold("R2 = 0.00, p = 0.9")', 
             color = "#00BFC4", parse = TRUE)  
 
  
  
  
summary(x$TP_Retention_percent) 
summary(x$SRP_Retention_percent)

y <- x[which(x$Wetland_Type != "Mesocosm"),]
summary(y$TP_Retention_percent)
summary(y$SRP_Retention_percent)


summary(x$TP_retention)
summary(x$SRP_retention)

summary(y$TP_retention)
summary(y$SRP_retention)

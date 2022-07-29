

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
}


### quartiles w shading

x$quad <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "< 0", 
                 ifelse(x$TP_Retention_percent < 33 | x$SRP_Retention_percent < 33, "0-33",
                        ifelse(x$TP_Retention_percent < 67 | x$SRP_Retention_percent < 67, "33-67", "67-100")))

quad.sum <- table(x$quad)
n <- as.data.frame(quad.sum)

x$source.sink <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "source", "sink")



#### INFLOW TP CONCENTRATION
x$bins <- cut_number(x$TP_Inflow_mg_L, 4)
table(x$quad, x$bins)
summary <- table(x$quad, x$bins)
m <- as.data.frame(summary)
TP <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic() +
  scale_fill_manual(labels = c(" < 0.1", "0.1 - 0.2", "0.2 - 1.7", "1.7+"),
                    values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF") ) +
  labs(x = "Retention bins (%) ", y = " ", fill = "Inflow TP\n (mg/L)") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
TP

ggplot(x, aes(x = log(TP_Inflow_mg_L), y = TP_Retention_percent, color = source.sink)) +
  geom_point() +
  ylim(-150, 100) +
  geom_segment(aes(x = -4, y = 0, xend = 4, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm')

mod1 <- lm(TP_Retention_percent ~ TP_Inflow_mg_L + source.sink, x)
summary(mod1)  
  
  
  
ggplot(x, aes(x = log(TP_Inflow_mg_L), y = TP_Retention_percent)) +
  geom_point() +
  ylim(-150, 100) +
  geom_segment(aes(x = -4, y = 0, xend = 4, yend = 0), lty = 2, col = "black") +
  geom_smooth(method = 'lm')

mod <- lm(TP_Retention_percent ~ TP_Inflow_mg_L, x)
summary(mod)  

#### INFLOW SRP CONCENTRATION
{
  x$bins <- cut_number(x$SRP_Inflow_mg_L, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  SRP <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic() +
    scale_fill_manual(labels = c(" < 0.05", "0.05 - 0.1", "0.1 - 0.6", "0.6+"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF") ) +
    labs(x = "Retention bins (%) ", y = " ", fill = "Inflow SRP\n (mg/L)") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  SRP
  
  ggplot(x, aes(x = log(SRP_Inflow_mg_L), y = SRP_Retention_percent, color = source.sink)) +
    geom_point() +
    ylim(-150, 100) +
    geom_segment(aes(x = -4, y = 0, xend = 4, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod2 <- lm(SRP_Retention_percent ~ SRP_Inflow_mg_L + source.sink, x)
  summary(mod2)  
  
  
  ggplot(x, aes(x = log(SRP_Inflow_mg_L), y = SRP_Retention_percent)) +
    geom_point() +
    ylim(-150, 100) +
    geom_segment(aes(x = -4, y = 0, xend = 4, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod <- lm(SRP_Retention_percent ~ SRP_Inflow_mg_L, x)
  summary(mod)  
}

### WETLAND SIZE
{x$bins <- cut_number(x$Area_m2, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  size <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("smallest", " ", " ", "largest"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF"  )) +
    labs(x = "Retention bins (%) ", y = "Frequency", fill = "Wetland \nSize") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold"))
  size

  ggplot(x, aes(x = log(Area_m2), y = TP_Retention_percent, color = source.sink)) +
    geom_point() +
    ylim(-150, 100) +
    geom_segment(aes(x = -4, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod1 <- lm(TP_Retention_percent ~ Area_m2 + source.sink, x)
  summary(mod1)  
  
  ggplot(x, aes(x = log(Area_m2), y = SRP_Retention_percent, color = source.sink)) +
    geom_point() +
    ylim(-150, 100) +
    geom_segment(aes(x = -4, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod2 <- lm(SRP_Retention_percent ~ (Area_m2) + source.sink, x)
  summary(mod2) 
  
  
  ggplot(x, aes(x = log(Area_m2), y = TP_Retention_percent)) +
    geom_point() +
    ylim(-150, 100) +
    geom_segment(aes(x = -4, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod <- lm(TP_Retention_percent ~ Area_m2, x)
  summary(mod) 
  
  
  ggplot(x, aes(x = log(Area_m2), y = SRP_Retention_percent)) +
    geom_point() +
    ylim(-150, 100) +
    geom_segment(aes(x = -4, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod <- lm(SRP_Retention_percent ~ Area_m2, x)
  summary(mod) 
  
  
  }

### WETLAND AGE
{table(x$quad, x$Age_yr)
  x$bins <- cut_number(x$Age_yr, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  
  age <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("<2 year", "2 years", "3-4 years", "5+ years"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF"  )) +
    labs(x = "Retention bins (%) ", y = " ", fill = "Wetland \nAge") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  age
  
  ggplot(x, aes(x = Age_yr, y = TP_Retention_percent, color = source.sink)) +
    geom_point() +
    ylim(-150, 100) +
    xlim(0,20) +
    geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod1 <- lm(TP_Retention_percent ~ Age_yr + source.sink, x)
  summary(mod1)  
  
  ggplot(x, aes(x = (Age_yr), y = SRP_Retention_percent, color = source.sink)) +
    geom_point() +
    ylim(-150, 100) +
    xlim(0,20) +
    geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod2 <- lm(SRP_Retention_percent ~ (Age_yr) + source.sink, x)
  summary(mod2) 
  
  
  ggplot(x, aes(x = Age_yr, y = TP_Retention_percent)) +
    geom_point() +
    ylim(-150, 100) +
    xlim(0,20) +
    geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod <- lm(TP_Retention_percent ~ Age_yr, x)
  summary(mod) 
  
  
  ggplot(x, aes(x = Age_yr, y = SRP_Retention_percent)) +
    geom_point() +
    xlim(0,20) +
    ylim(-150, 100) +
    geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod <- lm(SRP_Retention_percent ~ Age_yr, x)
  summary(mod) 
}

#### HLR
{x$bins <- cut_number(x$HLR, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  hlr <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("< 7.1", "7.1 - 14.3", "14.3 - 36.6", "36.6 +"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF"  )) +
    labs(x = "Retention bins (%) ", y = " ", fill = "HLR") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  hlr

  ggplot(x, aes(x = (HLR), y = TP_Retention_percent, color = source.sink)) +
    geom_point() +
    ylim(-150, 100) +
    xlim(0,300) +
    geom_segment(aes(x = -4, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod1 <- lm(TP_Retention_percent ~ HLR + source.sink, x)
  summary(mod1) 
  
  ggplot(x, aes(x = (HLR), y = SRP_Retention_percent, color = source.sink)) +
    geom_point() +
    ylim(-150, 100) +
    xlim(0,300) +
    geom_segment(aes(x = -4, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod1 <- lm(SRP_Retention_percent ~ HLR + source.sink, x)
  summary(mod1) 
  
  }

#### WETLAND:CATCHMENT ratio
{ x$CWRatio <- x$Catchment_area_ha/x$Area_m2*10000
  x$bins <- cut_number(x$CWRatio, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  
  ratio <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("< 3.3", "19 - 36", "36 - 200", "200+"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF")) +
    labs(x = "Retention bins (%) ", y = " ", fill = "Catchment \nto wetland\narea ratio") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  ratio
  
  ggplot(x, aes(x = log(CWRatio), y = TP_Retention_percent, color = source.sink)) +
    geom_point() +
    ylim(-150, 100) +
    xlim(0,8) +
    geom_segment(aes(x = 0, y = 0, xend = 8, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod1 <- lm(TP_Retention_percent ~ CWRatio + source.sink, x)
  summary(mod1) 
  
  ggplot(x, aes(x = log(CWRatio), y = SRP_Retention_percent, color = source.sink)) +
    geom_point() +
    ylim(-150, 100) +
    xlim(0,8) +
    geom_segment(aes(x = 0, y = 0, xend = 8, yend = 0), lty = 2, col = "black") +
    geom_smooth(method = 'lm')
  
  mod1 <- lm(SRP_Retention_percent ~ CWRatio + source.sink, x)
  summary(mod1) 
}











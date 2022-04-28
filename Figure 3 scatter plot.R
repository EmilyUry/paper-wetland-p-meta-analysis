


#' ---
#' title: "Figure 3. TP SPR retention scatter plot with distribution"
#' author: "Emily Ury"
#' date: "April 27, 2022"
#' ---
#' 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


library(ggplot2)
library(tidyverse)
# library(viridis)
# library(gridExtra)
library(cowplot)
#library(ggExtra)
#library(ggpubr)

## Data set-up
x <- read.csv("Wetland_P_Clean2.csv", header = T)
{head(x)
  
  
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



p <- ggplot(x, aes(x = SRP_Retention_percent, y = TP_Retention_percent)) +
  geom_point() + 
  theme(legend.position = "none") +
  xlim(-250, 105) +
  ylim(-150, 105) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("SRP % Retention") +
  ylab("TP % Retention")
p

hist <- ggplot(x, (aes(x = SRP_retention))) +
  geom_density() +
  xlim(-11, 5) +
  theme_classic() +
  xlab(" ") +
  theme(plot.margin = margin(t = 0, r = 0, b = -0.5, l = 1.4, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 


hist2 <- ggplot(x, (aes(x = TP_retention))) +
  geom_density() +
  xlim(-18, 13) +
  theme_classic() +
  xlab(" ") +
  coord_flip()  +
  theme(plot.margin = margin(t = 0, r = 0, b = 1, l = -0.5, unit = "cm"),
        axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), panel.grid = element_blank(), 
        axis.line.y = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.x = element_line(size = 0.5, colour = "white", linetype=1))

lab <- ggplot(x, (aes(x = TP_retention))) +
  annotate("text", 0,0, label = "Retention \n (g/m2/yr)") +
  theme_void()


tiff(filename = "figures/Figure 3 TP v SRP.tiff", height=3600, width=3600, units= "px", res=800, compression= "lzw")

plot_grid(hist, lab, p, hist2, labels = " ",rel_widths = c(4, 1), rel_heights = c(1, 4), ncol = 2)

dev.off()



############# 8 histogram - mass retention for each quadrat

I <- x[which(x$TP_Retention_percent > 0 & x$SRP_Retention_percent > 0),]
nrow(I)/nrow(x)*100

II <- x[which(x$TP_Retention_percent > 0 & x$SRP_Retention_percent < 0),]
nrow(II)/nrow(x)*100

III <- x[which(x$TP_Retention_percent < 0 & x$SRP_Retention_percent < 0),]
nrow(III)/nrow(x)*100

IV <- x[which(x$TP_Retention_percent < 0 & x$SRP_Retention_percent > 0),]
nrow(IV)/nrow(x)*100


i <-  ggplot(I, (aes(x = SRP_retention))) +
  geom_density(color = "red", fill = "#FF000033") + 
  geom_density(aes(x = TP_retention), color = "blue", fill = "#0000FF33") +
  xlim(-40, 30) +
  xlab(" ") +
  ylab(" ") +
  theme_classic() 

ii <-  ggplot(II, (aes(x = SRP_retention))) +
  geom_density(color = "red", fill = "#FF000033") + 
  geom_density(aes(x = TP_retention), color = "blue", fill = "#0000FF33") +
  xlim(-40, 30) +
  xlab(" ") +
  theme_classic() +
  annotate("rect", xmin = -40, xmax = -37, ymin = 0.3, ymax = 0.35, col = "red", fill = "#FF000033") +
  annotate("rect", xmin = -40, xmax = -37, ymin = 0.36, ymax = 0.41, col = "blue", fill = "#0000FF33") +
  annotate("text", x = -32, y = 0.325, label =  "SRP") +
  annotate("text", x = -33, y = 0.3825, label =  "TP")

iii <-  ggplot(III, (aes(x = SRP_retention))) +
  geom_density(color = "red", fill = "#FF000033") + 
  geom_density(aes(x = TP_retention), color = "blue", fill = "#0000FF33") +
  xlim(-40, 30) +
  xlab("Mass rentention (g/m2/yr)") +
  theme_classic()    

iv <-  ggplot(IV, (aes(x = SRP_retention))) +
  geom_density(color = "red", fill = "#FF000033") + 
  geom_density(aes(x = TP_retention), color = "blue", fill = "#0000FF33") +
  xlim(-40, 30) +
  xlab("Mass rentention (g/m2/yr)") +
  ylab(" ") +
  theme_classic() 

plot_grid(ii, i, iii, iv, labels = c("II", "I", "III", "IV"), ncol = 2)
    
    
    
  


a <-  ggplot(II, (aes(x = SRP_retention))) +
  geom_density() +
  xlim(-13, 5) +
  theme_classic() +
  xlab(" ") +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 
a
b <-  ggplot(I, (aes(x = SRP_retention))) +
  geom_density() +
  xlim(-5, 13) +
  theme_classic() +
  xlab(" ") +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 
b

c <-  ggplot(III, (aes(x = SRP_retention))) +
  geom_density() +
  xlim(-13, 5) +
  theme_classic() +
  xlab(" ") +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 
c
d <-  ggplot(IV, (aes(x = SRP_retention))) +
  geom_density() +
  xlim(-5, 13) +
  theme_classic() +
  xlab(" ") +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 
d

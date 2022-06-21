


#' ---
#' title: "Figure 2. TP SPR retention bar plot and scatter plot with distributions"
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
x <- read.csv("Wetland_P_Clean3.csv", header = T)

x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"


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
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("SRP % Retention") +
  ylab("TP % Retention") +
  annotate(geom = "text", x = 25, y = 105, label = "Q1", size = 6) + 
  annotate(geom = "text", x = -240, y = 105, label = "Q2", size = 6) + 
  annotate(geom = "text", x = -240, y = -147, label = "Q3", size = 6) + 
  annotate(geom = "text", x = 25, y = -147, label = "Q4", size = 6) 
p

hist <- ggplot(x, (aes(x = SRP_retention))) +
  geom_density() +
  xlim(-11.7, 5) +
  theme_classic(base_size = 16) +
  xlab(" ") +
  theme(plot.margin = margin(t = 0, r = 0, b = -0.5, l = 1.4, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 


hist2 <- ggplot(x, (aes(x = TP_retention))) +
  geom_density() +
  xlim(-18.9, 13) +
  theme_classic(base_size = 16) +
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

A <- plot_grid(hist, lab, p, hist2, labels = " ",rel_widths = c(4, 1), rel_heights = c(1, 4), ncol = 2)

A



##### bar plot

x$ret <- ifelse(x$TP_Retention_percent > 0, "pos", "neg")
TP.source <- table(x$ret)[1]
TP.sink <- table(x$ret)[2]
TP.source.percent <- round(TP.source/(TP.sink+TP.source)*100,1)

x$PO4ret <- ifelse(x$SRP_Retention_percent > 0, "pos", "neg")
SRP.source <- table(x$PO4ret)[1]
SRP.sink <- table(x$PO4ret)[2]
SRP.source.percent <- round(SRP.source/(SRP.sink+SRP.source)*100,1)

behavior <- c("source", "sink", "source", "sink")
species <- c("TP", "TP", "SRP", "SRP")
num <- c(TP.sink, TP.source, SRP.sink, SRP.source)
label_ypos <- c(250, 10, 250, 10)
label_text <- c("16%", " ", "25%", " ")
data <- data.frame(behavior, species, num, label_ypos, label_text)

c <- ggplot(data, aes(x = factor(species, level = c("TP", "SRP")), y = (num), fill = behavior)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle", color = "white", size = 6, fontface = "bold")+
  theme_classic(base_size = 16) +
  theme(legend.position = "right", legend.direction = "vertical", legend.text = element_text(size = 15), legend.key.size = unit(1,"cm"))+
  labs(title = " ", x = " ", y = "n (site-years)", fill = " " ) +
  #labs(title = "(c) Wetland sink/source behavior", x = " ", y = "n" ) +
  scale_fill_manual(values = c("#2c728eFF", "#2c728e55"), labels = c("source", "sink")) + 
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")) +
  theme(plot.title = element_text(hjust = 0.6, size = 7, face = "bold"))

c




tiff(filename = "figures/Source_sink.tif", height=3600, width=7200, units= "px", res=800, compression= "lzw")

plot_grid(c, A, labels = c("A", "B"), ncol = 2)

dev.off()

unique(x$Source)
unique(x$WetlandID)
nrow(x[which(x$Age_yr < 4),])
176/273


mean(x$TP_load_in_g_m2_yr)
range(x$TP_load_in_g_m2_yr)
median(x$TP_load_in_g_m2_yr)

mean(x$SRP_load_in_g_m2_yr)
range(x$SRP_load_in_g_m2_yr)
median(x$SRP_load_in_g_m2_yr)







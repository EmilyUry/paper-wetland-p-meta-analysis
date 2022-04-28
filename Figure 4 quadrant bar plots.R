


#' ---
#' title: "Figure 4. Bar plots by behavior quadrats"
#' author: "Emily Ury"
#' date: "April 27, 2022"
#' ---
#' 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


library(ggplot2)
library(tidyverse)


## Data set-up
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

I <- x[which(x$TP_Retention_percent > 0 & x$SRP_Retention_percent > 0),]
nrow(I)/nrow(x)*100

II <- x[which(x$TP_Retention_percent > 0 & x$SRP_Retention_percent < 0),]
nrow(II)/nrow(x)*100

III <- x[which(x$TP_Retention_percent < 0 & x$SRP_Retention_percent < 0),]
nrow(III)/nrow(x)*100

IV <- x[which(x$TP_Retention_percent < 0 & x$SRP_Retention_percent > 0),]
nrow(IV)/nrow(x)*100



x$quad <- ifelse(x$TP_Retention_percent > 0 & x$SRP_Retention_percent > 0, "I", 
                 ifelse(x$TP_Retention_percent > 0 & x$SRP_Retention_percent < 0, "II",
                        ifelse(x$TP_Retention_percent < 0 & x$SRP_Retention_percent < 0, "III", "IV")))

table(x$quad)



#### FLOW REGIME
levels(x$Water_regime)

x <- x %>%                                ### reorder flow regime
  mutate(Water_regime = fct_relevel(Water_regime, "continuous, constant" , "intermittent, constant" ,
                                    "continuous, variable", "intermittent, variable", 
                                   "n.s.")) 
table(x$quad, x$Water_regime)
summary <- table(x$quad, x$Water_regime)
m <- as.data.frame(summary)


tiff(filename = "figures/Figure4s Flow regime.tiff", height=3600, width=4800, units= "px", res=800, compression= "lzw")

ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic() +
  scale_fill_manual(labels = c("Continuous, constant", "Intermittent, constant", "Continuous, variable", "Intermittent, variable", "Not specified" ),
                    values = c("#2b821fbb",   "#345bebbb", "#bd4ad4bb","#e34327bb",  "#a1a1a1bb")) +
  labs(x = " ", y = "proportion of site-years", fill = "Inflow Regime") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 10, family = "serif")) +
  scale_x_discrete(labels = c("I. TP sink\nSRP sink", "II. TP sink\n SRP source",
                              "III. TP source\nSRP source", "IV. TP source\nSRP sink"))

dev.off()


#### Wetland TYPE
x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
table(x$quad, x$Wetland_Type)
summary <- table(x$quad, x$Wetland_Type)
m <- as.data.frame(summary)

tiff(filename = "figures/Figure4b Wetland Type.tiff", height=3600, width=4800, units= "px", res=800, compression= "lzw")
ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = c("#e34327bb", "#2b821fbb", "#a1a1a1bb",  "#345bebbb"  )) +
  labs(x = " ", y = "proportion of site-years", fill = "Wetland Type") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 10, family = "serif")) +
  scale_x_discrete(labels = c("I. TP sink\nSRP sink", "II. TP sink\n SRP source",
                              "III. TP source\nSRP source", "IV. TP source\nSRP sink"))
dev.off()


### wetland size distributions
{histI <- ggplot(I, (aes(x = Area_m2))) +
  geom_density() +
  xlim(-5000, 100000) +
  ylim(0,0.00025) +
  theme_classic() +
  xlab("Wetland area (m2) ") +
  theme(plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.5, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 
histI

histII <- ggplot(II, (aes(x = Area_m2))) +
  geom_density() +
  xlim(-5000, 100000) +
  ylim(0,0.00025) +
  theme_classic() +
  xlab("Wetland area (m2) ") +
  theme(plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.5, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 
histII


histIII <- ggplot(III, (aes(x = Area_m2))) +
  geom_density() +
  xlim(-5000, 100000) +
  ylim(0,0.00025) +
  theme_classic() +
  xlab("Wetland area (m2) ") +
  theme(plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.5, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 
histIII

histIV <- ggplot(IV, (aes(x = Area_m2))) +
  geom_density() +
  xlim(-5000, 100000) +
  ylim(0,0.00025) +
  theme_classic() +
  xlab("Wetland area (m2) ") +
  theme(plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.5, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 
histIV



plot_grid(histII, histI, histIII, histIV, labels = c("II", "I", "III", "IV"), 
          label_fontfamily = "serif", rel_widths = c(1,1),  ncol = 2)}


#### INFLOW CONCENTRATION
# rbPal <- colorRampPalette(c('blue','red'))
# rbPal(4)
# mypal4 = c("#0000FF99", "#5500AA99" , "#AA005599", "#FF000099")


mypal3 = c("#0000FF99",  "#7F007F99",  "#FF000099")


x$bins <- cut_number(x$TP_Inflow_mg_L, 3)
table(x$quad, x$bins)
summary <- table(x$quad, x$bins)
m <- as.data.frame(summary)
TP <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = mypal3) +
  labs(x = " ", y = "proportion of site years", fill = "Inflow TP (mg/L)") +
  theme(legend.position= "top", legend.direction = "vertical", legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_x_discrete(labels = c("I. TP sink\nSRP sink", "II. TP sink\n SRP source",
                              "III. TP source\nSRP source", "IV. TP source\nSRP sink"))


x$TPbins <- cut_number(x$SRP_Inflow_mg_L, 3)
table(x$quad, x$TPbins)
summary <- table(x$quad, x$TPbins)
w <- as.data.frame(summary)
SRP <- ggplot(w, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = mypal3) +
  labs(x = " ", y = "proportion of site years", fill = "Inflow SRP (mg/L)") +
  theme(legend.position= "top", legend.direction = "vertical", legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))+
  scale_x_discrete(labels = c("I. TP sink\nSRP sink", "II. TP sink\n SRP source",
                              "III. TP source\nSRP source", "IV. TP source\nSRP sink"))


tiff(filename = "figures/Figure4C Inflow concentration.tiff", height=3600, width=6000, units= "px", res=800, compression= "lzw")

plot_grid(TP, SRP, labels = c("A", "B"), ncol = 2)

dev.off()





#### Inflow volume
x$bins <- cut_number(x$Inflow_m3_yr, 3)
table(x$quad, x$bins)
summary <- table(x$quad, x$bins)
m <- as.data.frame(summary)
vol <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = mypal3) +
  labs(x = " ", y = "proportion of site-years", fill = "Inflow volume (m3/yr)") +
  theme(legend.position= "top", legend.direction = "vertical", legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))+
  scale_x_discrete(labels = c("I. TP sink\nSRP sink", "II. TP sink\n SRP source",
                              "III. TP source\nSRP source", "IV. TP source\nSRP sink"))

#### HLR
x$bins <- cut_number(x$HLR, 3)
table(x$quad, x$bins)
summary <- table(x$quad, x$bins)
m <- as.data.frame(summary)
hlr <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = mypal3) +
  labs(x = " ", y = "proportion of site-years", fill = "HLR (m/yr)") +
  theme(legend.position= "top", legend.direction = "vertical", legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))+
  scale_x_discrete(labels = c("I. TP sink\nSRP sink", "II. TP sink\n SRP source",
                              "III. TP source\nSRP source", "IV. TP source\nSRP sink"))



tiff(filename = "figures/Figure4D Flow.tiff", height=3600, width=6000, units= "px", res=800, compression= "lzw")

plot_grid(vol, hlr, labels = c("A", "B"), ncol = 2)

dev.off()

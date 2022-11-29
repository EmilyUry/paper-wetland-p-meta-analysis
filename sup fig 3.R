



library(ggplot2)
library(cowplot)
library(forcats) ## need for fct_relevel
library(dplyr) # easier data wrangling 
library(viridis)
library(gridExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
library(ggpmisc)

options(scipen = 0)
options(scipen = 100)

##### sup fig 3
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop



##### annual data set up
{
  x <- read.csv("Wetland_P_Clean3.csv", header = T)
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
  ### mass removed
  x$TP_retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
  x$SRP_retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out
  x$water_atten <- x$Inflow_m3_yr - x$Outflow_m3_yr
  x$water_atten_percent <- 100*x$water_atten/x$Inflow_m3_yr
  x$CAWA <- x$Catchment_area_ha/x$Area_m2*10000
  
  
  x$binTP <- ifelse(x$TP_Retention_percent > 66, 1,
                    ifelse(x$TP_Retention_percent > 33, 2,
                           ifelse(x$TP_Retention_percent > 0.00001, 3, 4)))
  x$binTP <- ifelse(x$TP_Retention_percent > 66, 1,
                    ifelse(x$TP_Retention_percent > 33, 2,
                           ifelse(x$TP_Retention_percent > 0.00001, 3, 4)))
  
  
  x.ON <- x[which(x$Source == "DUCs 2022"),]
  x.Aud <- x[which(x$Source == "Audet 2020"),]
}


A <- x %>%
  ggplot(aes(x=  CAWA, y = water_atten_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  #scale_y_continuous(trans='log10')+
  ylim(-120, 120) +
  xlab("Catchment:wetland area") +
  ylab("Water Attenuation (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none')

B <- x %>%
  ggplot(aes(x=  CAWA, y = HLR))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  scale_y_continuous(trans='log10')+
  #ylim(-120, 120) +
  xlab("Catchment:wetland area") +
  ylab("HLR (m/yr)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


C <- x %>%
  ggplot(aes(x=  CAWA, y = TP_load_in_g_m2_yr))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  scale_y_continuous(trans='log10')+
  #ylim(-120, 120) +
  xlab("Catchment:wetland area") +
  ylab("TP Load (g/m2/yr)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

D <- x %>%
  ggplot(aes(x=  CAWA, y = SRP_load_in_g_m2_yr))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  scale_y_continuous(trans='log10')+
  #ylim(-120, 120) +
  xlab("Catchment:wetland area") +
  ylab("PO4 Load (g/m2/yr)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


tiff(filename = "figures/Sup_fig_3.tif", height=5, width=6, units= "in", res=800, compression= "lzw")


plot_grid(A, B, C, D, nrow = 2, labels = c("A", "B", "C", "D"), rel_widths = c(1,1))

dev.off()



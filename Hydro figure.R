

library(ggplot2)
library(cowplot)
library(forcats) ## need for fct_relevel
library(dplyr) # easier data wrangling 
library(viridis)
library(gridExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
library(ggpmisc)

options(scipen = 0)
options(scipen = 10)

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


#########
# monthly data set up
{
  x <- read.csv("Monthly_Wetland_P_Clean.csv", header = T)
  x <- x[which(x$data_type == "both" | x$data_type == "load"),]
  x$TP_Retention <- x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo
  x$SRP_Retention <- x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo
  x$Unique_ID <- paste(x$Short_Ref, x$Short_ID, x$Short_year, sep = "_")
  M <-x %>% select(Unique_ID, Short_Ref, Short_ID,  Short_year, Month, TP_IN_g_m2_mo, TP_Retention, TP_Retention_percent, 
                   SRP_IN_g_m2_mo, SRP_Retention, SRP_Retention_percent, TP_OUT_g_m2_mo, SRP_OUT_g_m2_mo, 
                   Monthly_Inflow_m3_month, Monthly_Outflow_m3_month, 
                   SA_m2, Catchment_area)
  M <- M[which(M$Short_Ref !=  17),]       ### drop site 17 because it is too gappy
  M <- M[which(M$Short_year != "YN"),]     ### drop partial years
  #M <- na.omit(M)                           ### drop months with NAs
  M <- M %>%                                ### reorder months in order
    mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                               "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 
  M$water_atten <- M$Monthly_Inflow_m3_month - M$Monthly_Outflow_m3_month
  M$water_atten_percent <- 100*M$water_atten/M$Monthly_Inflow_m3_month
  M$CAWA <- M$Catchment_area/(M$SA_m2/10000)
  M$HLR <- M$Monthly_Inflow_m3_month/M$SA_m2
  
  M$binTP <- ifelse(M$TP_Retention_percent > 66, 1,
                    ifelse(M$TP_Retention_percent > 33, 2,
                           ifelse(M$TP_Retention_percent > 0.00001, 3, 4)))
  M$binSRP <- ifelse(M$SRP_Retention_percent > 66, 1,
                    ifelse(M$SRP_Retention_percent > 33, 2,
                           ifelse(M$SRP_Retention_percent > 0.00001, 3, 4)))
  
  M.ON <- M[which(M$Short_Ref == 18),]
  M.Aud <- M[which(M$Short_Ref == 4),]
}

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




A <- M %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent,
             color = water_atten_percent, fill = water_atten_percent))+
  geom_point(pch =21, alpha = 0.7, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
    scale_x_continuous(trans='log10')+
  ylim(-120,130) +
  xlab("HLR (m/month)") +
  ylab("TP Retention (%)")+
  theme_classic(base_size = 8) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 2, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = "none", legend.title = element_blank()) 


B <- M %>%
  ggplot(aes(x=  HLR, y = SRP_Retention_percent, 
             color = water_atten_percent, fill = water_atten_percent))+
  geom_point(pch =21, alpha = 0.7, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_x_continuous(trans='log10')+
  ylim(-120, 130) +
  xlab("HLR (m/month)") +
  ylab("PO4 Retention(%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 2, color = "gray30") +
  theme_classic(base_size =8) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'right', legend.title = element_blank())



tiff(filename = "figures/Fig5_new_monthly.tif", height=2.5, width=5, units= "in", res=800, compression= "lzw")

plot_grid(A, B, nrow = 1, labels = c("TP", "PO4"), rel_widths = c(1,1.3), label_size = 10)

dev.off()





############# talk version

M %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent,
             color = water_atten_percent, fill = water_atten_percent))+
  geom_point(pch =21, alpha = 0.7, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_x_continuous(trans='log10')+
  ylim(-120,150) +
  xlab("HLR (m/month)") +
  ylab("TP Retention (%)")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 4, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = "right", legend.title = element_blank()) 









### Hydro figure




A <- M %>%
  ggplot(aes(x=  HLR, y = water_atten_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-120,120) +
  xlab("HLR (m)") +
  ylab("Water attenuation (%)")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


B <- x %>%
  ggplot(aes(x=  HLR, y = water_atten_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  #scale_y_continuous(trans='log10')+
  ylim(-120, 120) +
  xlab("HLR (m)") +
  ylab("Water attenuation (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


C <- M %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-120,120) +
  xlab("HLR (m)") +
  ylab("TP Retention(%)")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


D <- x %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  #scale_y_continuous(trans='log10')+
  ylim(-120, 120) +
  xlab("HLR (m)") +
  ylab("TP Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 





plot_grid(A, B, C, D, nrow = 2, labels = c("Monthly", "Annual", " ", " "))





A <- M %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-120,120) +
  xlab("HLR (m)") +
  ylab("TP Retention(%)")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


B <- x %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  #scale_y_continuous(trans='log10')+
  ylim(-120, 120) +
  xlab("HLR (m)") +
  ylab("TP Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 



C <- M %>%
  ggplot(aes(x=  HLR, y = SRP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-120,120) +
  xlab("HLR (m)") +
  ylab("PO4 Retention (%)")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


D <- x %>%
  ggplot(aes(x=  HLR, y = SRP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  #scale_y_continuous(trans='log10')+
  ylim(-120, 120) +
  xlab("HLR (m)") +
  ylab("PO4 Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 



plot_grid(A, B, C, D, nrow = 2, labels = c("Monthly", "Annual", " ", " "))





A <- M %>%
  ggplot(aes(x=  CAWA, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-120,120) +
  xlab("Catchment:wetland area") +
  ylab("TP Retention(%)")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


B <- x %>%
  ggplot(aes(x=  CAWA, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  #scale_y_continuous(trans='log10')+
  ylim(-120, 120) +
  xlab("Catchment:wetland area") +
  ylab("TP Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 



C <- M %>%
  ggplot(aes(x=  CAWA, y = SRP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-120,120) +
  xlab("Catchment:wetland area") +
  ylab("PO4 Retention (%)")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


D <- x %>%
  ggplot(aes(x=  CAWA, y = SRP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(5,1000))+
  #scale_y_continuous(trans='log10')+
  ylim(-120, 120) +
  xlab("Catchment:wetland area") +
  ylab("PO4 Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 



plot_grid(A, B, C, D, nrow = 2, labels = c("Monthly", "Annual", " ", " "))




A <- M %>%
  ggplot(aes(x=  water_atten_percent, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  #scale_x_continuous(trans='log10')+
  xlim(-60,100) +
  #scale_y_continuous(trans='log10')+
  ylim(-120,120) +
  xlab("Water Attenuation (%)") +
  ylab("TP Retention(%)")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


B <- x %>%
  ggplot(aes(x=  water_atten_percent, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  #scale_x_continuous(trans='log10', limits = c(5,1000))+
  xlim(-60,100) +
  #scale_y_continuous(trans='log10')+
  ylim(-120, 120) +
  xlab("Water Attenuation (%)") +
  ylab("TP Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 



C <- M %>%
  ggplot(aes(x=  water_atten_percent, y = SRP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  xlim(-60,100) +
  #scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-120,120) +
  xlab("Water Attenuation (%)") +
  ylab("PO4 Retention (%)")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


D <- x %>%
  ggplot(aes(x=  water_atten_percent, y = SRP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  #scale_x_continuous(trans='log10', limits = c(5,1000))+
  xlim(-60,100) +
  #scale_y_continuous(trans='log10')+
  ylim(-120, 120) +
  xlab("Water Attenuation (%)") +
  ylab("PO4 Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 



plot_grid(A, B, C, D, nrow = 2, labels = c("Monthly", "Annual", " ", " "))





M <- M[!is.na(M$binSRP),]
M <- M[!is.na(M$binTP),]

A <- M %>%
  ggplot(aes(x=  Catchment_area, y = SA_m2/10000, fill = as.factor(binTP)))+
  geom_point(pch =21, alpha = 0.5, cex = 3)+
  scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
                    label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  xlab("Catchment area (ha)") +
  ylab("Monthly inflow \nvolume (m3)")+
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


B <- M %>%
  ggplot(aes(x=  Catchment_area, y = Monthly_Inflow_m3_month, fill = as.factor(binSRP)))+
  geom_point(pch =21, alpha = 0.5, cex = 3)+
  scale_fill_manual("PO4 % \nretention", values = c("green", "yellow", "orange", "red"),
                    label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  xlab("Catchment area (ha)") +
  ylab("Monthly inflow \nvolume (m3)")+
  theme_classic() +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = 'none') 


top <- plot_grid(A, B,NULL, NULL, nrow = 2,  labels = c("   TP", "PO4 ", " ", " "), rel_widths = c(1,1), hjust = -2)

a <- x %>%
  ggplot(aes(x=  Area_m2/10000, y = Catchment_area_ha))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  ylab("Catchment area (ha)") +
  xlab("Wetland area (ha))")+
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

b <- x %>%  
  ggplot(aes(x=  CAWA, y = TP_Retention_percent))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-150,100) +
  xlab("Catchment:wetland area") +
  ylab("TP Retention (%))")+
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') +
  stat_smooth(method = "lm")

c <- x %>%  
  ggplot(aes(x=  CAWA, y = SRP_Retention_percent))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-150,100) +
  xlab("Catchment:wetland area") +
  ylab("PO4 Retention (%))")+
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') +
  stat_smooth(method = "lm")
  
  
bottom <- plot_grid(a,b, c, nrow = 1, labels = c("Annual data", "TP", "PO4 "))

plot_grid(top, bottom, nrow = 2, rel_heights = c(2,1))

top








a <- x %>%
  ggplot(aes(x=  CAWA, y = HLR))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  ylab("hydraulic loading rate") +
  xlab("Catchment:wetland area")+
  theme_classic(base_size = 12) +
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


b <- x %>%  
  ggplot(aes(x=  CAWA, y = water_atten_percent))+
  geom_point() +
  #geom_point(data = . %>% filter(TP_Retention_percent >= 0), col = "blue")+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-30,100)+
  xlab("Catchment:wetland area") +
  ylab("Water attenuation (%)")+
  theme_classic(base_size = 12) +
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

c <- x %>%  
  ggplot(aes(x=  CAWA, y = TP_load_in_g_m2_yr))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  xlab("Catchment:wetland area") +
  ylab("TP load (g/m2/yr))")+
  theme_classic(base_size = 12)+
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

d <- x %>%  
  ggplot(aes(x=  CAWA, y = SRP_load_in_g_m2_yr))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  xlab("Catchment:wetland area") +
  ylab("PO4 load (g/m2/yr))")+
  theme_classic(base_size = 12)+
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

left <- plot_grid(a,b, c,d, nrow = 4, labels = c(" ", "", " "))

a <- x %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-200,100) +
  ylab("TP Retention (%)") +
  xlab("Hydraulic loading rate (m)")+
  theme_classic(base_size = 12) +
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


b <- x %>%  
  ggplot(aes(x=  water_atten_percent, y = TP_Retention_percent))+
  geom_point()+
  #scale_x_continuous(trans='log10')+
  xlim(-30,100) +
  #scale_y_continuous(trans='log10')+
  ylim(-200,100) +
  ylab("TP Retention (%)") +
  xlab("Water attenuation (%)")+
  theme_classic(base_size = 12) +
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

c <- x %>%  
  ggplot(aes(x=  TP_load_in_g_m2_yr, y = TP_Retention_percent))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-200,100) +
  ylab("TP Retention (%)") +
  xlab("TP load (g/m2/yr))")+
  theme_classic(base_size = 12)+
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

d <- x %>%  
  ggplot(aes(x=  CAWA, y = TP_Retention_percent))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-200,100) +
  xlab("Catchment:wetland area") +
  ylab("TP Retention (%)")+
  theme_classic(base_size = 12)+
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

middle <- plot_grid(a,b, c,d, nrow = 4, labels = c(" ", "", " "))


a <- x %>%
  ggplot(aes(x=  HLR, y = SRP_Retention_percent))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-200,100) +
  ylab("PO4 Retention (%)") +
  xlab("Hydraulic loading rate (m)")+
  theme_classic(base_size = 12) +
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


b <- x %>%  
  ggplot(aes(x=  water_atten_percent, y = SRP_Retention_percent))+
  geom_point()+
  #scale_x_continuous(trans='log10')+
  xlim(-30,100) +
  #scale_y_continuous(trans='log10')+
  ylim(-200,100) +
  ylab("PO4 Retention (%)") +
  xlab("Water attenuation (%)")+
  theme_classic(base_size = 12) +
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

c <- x %>%  
  ggplot(aes(x=  SRP_load_in_g_m2_yr, y = SRP_Retention_percent))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-200,100) +
  ylab("PO4 Retention (%)") +
  xlab("PO4 load (g/m2/yr))")+
  theme_classic(base_size = 12)+
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

d <- x %>%  
  ggplot(aes(x=  CAWA, y = SRP_Retention_percent))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  ylim(-200,100) +
  xlab("Catchment:wetland area") +
  ylab("PO4 Retention (%)")+
  theme_classic(base_size = 12)+
  stat_smooth(method = "lm") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 

right <- plot_grid(a,b,c,d, nrow = 4)
plot_grid(left, middle, right, nrow = 1, labels = c("CAWA", "TP", "PO4"))


fit <- lm(TP_Retention_percent ~ log(HLR), data = x)
summary(fit)
plot(x$HLR, x$TP_Retention_percent, log = "x", ylim = c(-200,100))
abline(fit)
fit <- lm(TP_Retention_percent ~ log(CAWA), data = x[which(x$TP_retention >0),])
summary(fit)





M <- M[!is.na(M$binSRP),]
M <- M[!is.na(M$binTP),]

z <- x[!is.na(x$Catchment_area_ha),]
fit <- lm(TP_Retention_percent ~ CAWA, data = z)
summary(fit)

##  Alt


A <- M %>%
  ggplot(aes(x=  SA_m2/10000, y = Monthly_Inflow_m3_month, fill = as.factor(binTP)))+
  geom_point(pch =21, alpha = 0.5, cex = 3)+
  scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
                     label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  xlab("Wetland surface area (ha)") +
  ylab("Monthly inflow \nvolume (m3)")+
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none') 


B <- M %>%
  ggplot(aes(x=  SA_m2/10000, y = Monthly_Inflow_m3_month, fill = as.factor(binSRP)))+
  geom_point(pch =21, alpha = 0.5, cex = 3)+
  scale_fill_manual("PO4 % \nretention", values = c("green", "yellow", "orange", "red"),
                     label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  xlab("Wetland surface area (ha)") +
  ylab("Monthly inflow \nvolume (m3)")+
  theme_classic() +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = 'none') 


C <-M %>%
  ggplot(aes(x=  HLR, y = water_atten_percent, fill = as.factor(binTP)))+
  geom_point(pch =21, alpha = 0.5, cex = 3)+
  scale_fill_manual("PO4 % \nretention", values = c("green", "yellow", "orange", "red"),
                    label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  ylim(-70,100)+
  xlab("Hydraulic loading rate (m)") +
  ylab("\nWater retention (%)")+
  theme_classic() +
  theme(plot.margin = margin(20, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "none") 

D <-M %>%
  ggplot(aes(x=  HLR, y = water_atten_percent, fill = as.factor(binSRP)))+
  geom_point(pch =21, alpha = 0.5, cex = 3)+
  scale_fill_manual("PO4 % \nretention", values = c("green", "yellow", "orange", "red"),
                    label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10')+
  ylim(-70,100)+
  xlab("Hydraulic loading rate (m)") +
  ylab("\nWater retention (%)")+
  theme_classic() +
  theme(plot.margin = margin(20, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "none") 

plot_grid(A, B, C, D, nrow = 3,  labels = c("   TP", "PO4 ", " ", " "), rel_widths = c(1,1), hjust = -2)










######## Supplemental (same, but annual scale)

A <- x %>%
  ggplot(aes(x= Area_m2, y = Inflow_m3_yr)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 67), col = "green", alpha = 0.6) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 33 & TP_Retention_percent <= 67), col = "yellow", alpha = 0.6) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0 & TP_Retention_percent <= 33), col = "orange", alpha = 0.6) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), col = "red", alpha = 0.6) +
  #scale_color_gradient2(midpoint = -300, low = "blue",  high = "red", mid = "blue") +
  #xlim(0,400) +
  #ylim(-75,100) +
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  xlab("Surface Area") +
  ylab("Yearly inflow volume (m3)")+
  #geom_abline(slope = 1, intercept = 0) +
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  theme_classic() +
  theme(plot.margin = margin(20, 5, 0, 5))
B <- x %>%
  ggplot(aes(x=  HLR, y = water_atten_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 67), col = "green", alpha = 0.6) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 33 & TP_Retention_percent <= 67), col = "yellow", alpha = 0.6) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0 & TP_Retention_percent <= 33), col = "orange", alpha = 0.6) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), col = "red", alpha = 0.6) +
  #scale_color_gradient2(midpoint = -300, low = "blue",  high = "red", mid = "blue") +
  #xlim(-50,100) +
  ylim(-100,100) +
  scale_x_continuous(trans='log10')+
  #scale_y_continuous(trans='log10')+
  xlab("Hydraulic loading rate") +
  ylab("Water % Retention")+
  #geom_abline(slope = 1, intercept = 0) +
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  theme_classic() +
  #geom_vline(xintercept = 0.05, lty = 2) +
  theme(plot.margin = margin(20, 5, 0, 5)) 


plot_grid(A, B, NULL, NULL, nrow = 2,  labels = c(" ", " ", "TP", "PO4 "), rel_widths = c(1,1), hjust = -1)

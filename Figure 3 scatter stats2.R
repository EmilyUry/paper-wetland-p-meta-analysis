

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
library(ggpmisc)


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

# x$quad <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "< 0", 
#                  ifelse(x$TP_Retention_percent < 33 | x$SRP_Retention_percent < 33, "0-33",
#                         ifelse(x$TP_Retention_percent < 67 | x$SRP_Retention_percent < 67, "33-67", "67-100")))
# 
# quad.sum <- table(x$quad)
# n <- as.data.frame(quad.sum)
# 
# x$TP.S <- ifelse(x$TP_Retention_percent < 0, "source", "sink")
# x$SRP.S <- ifelse(x$SRP_Retention_percent < 0, "source", "sink")
# 

# TP.sink <- x[which(x$TP.S == "sink"),]
# TP.source <- x[which(x$TP.S == "source"),]
# 
# 
# SRP.sink <- x[which(x$SRP.S == "sink"),]
# SRP.source <- x[which(x$SRP.S == "source"),]
# 
# #### INFLOW TP CONCENTRATION
# 
# TPsink <- lm(TP_Retention_percent ~ log(TP_load_in_g_m2_yr), TP.sink)
# summary(TPsink)
# TPsource <- lm(TP_Retention_percent ~ TP_Inflow_mg_L, TP.source)
# summary(TPsource)
# 
# ggplot(x, aes(x = (TP_load_in_g_m2_yr), y = TP_Retention_percent, color = TP.S)) +
#   geom_point() +
#   scale_x_continuous(trans='log10')+
#   ylim(-150, 115) +
#   geom_segment(aes(x = -4, y = 0, xend = 4, yend = 0), lty = 2, col = "black") +
#   geom_smooth(method = 'lm') +
#   annotate(geom = "text", x = 2.5, y = 115, label = 'bold("R2 = 0.02, p = 0.027")',
#            color = "#F8766D", parse = TRUE) +
#   annotate(geom = "text", x = 2.5, y = 103, label = 'bold("R2 = 0.04, p = 0.11")', 
#            color = "#00BFC4", parse = TRUE) 
# library(ggpmisc)
# 
# 
# a <- x %>%
#   ggplot(aes(x = (TP_load_in_g_m2_yr), y = TP_Retention_percent)) +
#   geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue") +
#   geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red") +
#   ylim(-150, 150) +
#   scale_x_continuous(trans='log10')+
#   xlab("TP load (g/m2/year)") +
#   ylab("TP Retention (%)") +
#   geom_segment(aes(x = 0.01, y = 0, xend = 100, yend = 0), lty = 2, col = "black") +
#   #geom_point(alpha = 0.1, cex = 2) +
#   #geom_smooth(method = 'lm', color = "gray50") +
#   #annotate(geom = "text", x = 2.5, y = 115, label = 'bold("R2 = 0.02, p = 0.027")',
#   #         color = "blue", parse = TRUE) +
#   theme_classic(base_size = 12) +
#   stat_poly_line(data = .%>% filter(TP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
#   stat_poly_eq(data = .%>% filter(TP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                color = "blue") +
#   theme(plot.margin = margin(25, 25, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# 
# #### INFLOW SRP CONCENTRATION
# 
# SRPsink <- lm(SRP_Retention_percent ~ SRP_Inflow_mg_L, SRP.sink)
# summary(SRPsink)
# SRPsource <- lm(SRP_Retention_percent ~ SRP_Inflow_mg_L, SRP.source)
# summary(SRPsource) 
# 
# ggplot(x, aes(x = log(SRP_Inflow_mg_L), y = SRP_Retention_percent, color = SRP.S)) +
#   geom_point() +
#   ylim(-150, 115) +
#   geom_segment(aes(x = -6, y = 0, xend = 5, yend = 0), lty = 2, col = "black") +
#   geom_smooth(method = 'lm') +
#   annotate(geom = "text", x = 2.5, y = 115, label = 'bold("R2 = 0.07, p = 0.0001")',
#            color = "#F8766D", parse = TRUE) +
#   annotate(geom = "text", x = 2.5, y = 103, label = 'bold("R2 = 0.04, p = 0.07")', 
#            color = "#00BFC4", parse = TRUE)
# 
# 
# 
# 
# 
# ### WETLAND SIZE - TP
# 
# 
# TPsink <- lm(TP_Retention_percent ~ log(Area_m2), TP.sink)
# summary(TPsink)
# TPsource <- lm(TP_Retention_percent ~ log(Area_m2), TP.source)
# summary(TPsource)
# 
# ggplot(x, aes(x = log(Area_m2), y = TP_Retention_percent, color = TP.S)) +
#   geom_point() +
#   ylim(-150, 115) +
#   geom_segment(aes(x = 0, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
#   geom_smooth(method = 'lm') +
#   annotate(geom = "text", x = 15, y = 115, label = 'bold("R2 = 0.006, p = 0.12")',
#            color = "#F8766D", parse = TRUE) +
#   annotate(geom = "text", x = 15, y = 103, label = 'bold("R2 = 0.034, p = 0.12")', 
#            color = "#00BFC4", parse = TRUE) 
# 
# #### WETLAND SIZE -  SRP 
# 
# SRPsink <- lm(SRP_Retention_percent ~ log(Area_m2), SRP.sink)
# summary(SRPsink)
# SRPsource <- lm(SRP_Retention_percent ~ log(Area_m2), SRP.source)
# summary(SRPsource) 
# 
# ggplot(x, aes(x = log(Area_m2), y = SRP_Retention_percent, color = SRP.S)) +
#   geom_point() +
#   ylim(-150, 115) +
#   geom_segment(aes(x = 0, y = 0, xend = 18, yend = 0), lty = 2, col = "black") +
#   geom_smooth(method = 'lm') +
#   annotate(geom = "text", x = 15, y = 115, label = 'bold("R2 = 0.07, p < 0.0001")',
#            color = "#F8766D", parse = TRUE) +
#   annotate(geom = "text", x = 15, y = 103, label = 'bold("R2 = 0.02, p = 0.10")', 
#            color = "#00BFC4", parse = TRUE)
# 
#   
# 
# 
# ### WETLAND AGE
# 
# 
# 
# TPsink <- lm(TP_Retention_percent ~ Age_yr, TP.sink)
# summary(TPsink)
# TPsource <- lm(TP_Retention_percent ~ Age_yr, TP.source)
# summary(TPsource)
# 
# ggplot(x, aes(x = Age_yr, y = TP_Retention_percent, color = TP.S)) +
#   geom_point() +
#   ylim(-150, 115) +
#   xlim(0,20) +
#   geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
#   geom_smooth(method = 'lm') +
#   annotate(geom = "text", x = 15, y = 115, label = 'bold("R2 = 0.00, p = 0.5")',
#            color = "#F8766D", parse = TRUE) +
#   annotate(geom = "text", x = 15, y = 103, label = 'bold("R2 = 0.00, p = 0.9")', 
#            color = "#00BFC4", parse = TRUE) 
# 
# #### WETLAND AGE -  SRP 
# 
# SRPsink <- lm(SRP_Retention_percent ~ Age_yr, SRP.sink)
# summary(SRPsink)
# SRPsource <- lm(SRP_Retention_percent ~ Age_yr, SRP.source)
# summary(SRPsource) 
# 
# ggplot(x, aes(x = Age_yr, y = SRP_Retention_percent, color = SRP.S)) +
#   geom_point() +
#   ylim(-150, 115) +
#   xlim(0,20) +
#   geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
#   geom_smooth(method = 'lm') +
#   annotate(geom = "text", x = 15, y = 115, label = 'bold("R2 = 0.005, p = 0.15")',
#            color = "#F8766D", parse = TRUE) +
#   annotate(geom = "text", x = 15, y = 103, label = 'bold("R2 = 0.01, p = 0.16")', 
#            color = "#00BFC4", parse = TRUE)
# 
# 
# 
# 
# 
# 
# #### HLR
# 
# 
# 
# 
# 
# TPsink <- lm(TP_Retention_percent ~ log(HLR), TP.sink)
# summary(TPsink)
# TPsource <- lm(TP_Retention_percent ~ log(HLR), TP.source)
# summary(TPsource)
# 
# ggplot(x, aes(x = HLR, y = TP_Retention_percent, color = TP.S)) +
#   geom_point() +
#   ylim(-150, 115) +
#   scale_x_continuous(trans='log10')+
#   geom_segment(aes(x = 0, y = 0, xend = 300, yend = 0), lty = 2, col = "black") +
#   geom_smooth(method = 'lm') +
#   annotate(geom = "text", x = 250, y = 115, label = 'bold("R2 = 0.02, p = 0.015")',
#            color = "#F8766D", parse = TRUE) +
#   annotate(geom = "text", x = 250, y = 103, label = 'bold("R2 = 0.00, p = 0.5")', 
#            color = "#00BFC4", parse = TRUE) 
# 
# #### HLR -  SRP 
# 
# SRPsink <- lm(SRP_Retention_percent ~ log(HLR), SRP.sink)
# summary(SRPsink)
# SRPsource <- lm(SRP_Retention_percent ~ log(HLR), SRP.source)
# summary(SRPsource) 
# 
# ggplot(x, aes(x = HLR, y = SRP_Retention_percent, color = SRP.S)) +
#   geom_point() +
#   ylim(-150, 115) +
#   scale_x_continuous(trans='log10')+
#   geom_segment(aes(x = 0, y = 0, xend = 20, yend = 0), lty = 2, col = "black") +
#   geom_smooth(method = 'lm') +
#   annotate(geom = "text", x = 250, y = 115, label = 'bold("R2 = 0.03, p = 0.008")',
#            color = "#F8766D", parse = TRUE) +
#   annotate(geom = "text", x = 250, y = 103, label = 'bold("R2 = 0.001, p = 0.3")', 
#            color = "#00BFC4", parse = TRUE)
# 
# 
# 
# 
# 
# #### CATCHMENT:WETLAND AREA ratio
#   
# 
#   TPsink <- lm(TP_Retention_percent ~ log(CWRatio), TP.sink)
#   summary(TPsink)
#   TPsource <- lm(TP_Retention_percent ~ log(CWRatio), TP.source)
#   summary(TPsource)
#   
#   ggplot(x, aes(x = log(CWRatio), y = TP_Retention_percent, color = TP.S)) +
#     geom_point() +
#     ylim(-150, 115) +
#     geom_segment(aes(x = 1, y = 0, xend = 7, yend = 0), lty = 2, col = "black") +
#     geom_smooth(method = 'lm') +
#     annotate(geom = "text", x = 6, y = 115, label = 'bold("R2 = 0.17, p < 0.0001")',
#              color = "#F8766D", parse = TRUE) +
#     annotate(geom = "text", x = 6, y = 103, label = 'bold("R2 = 0.00, p = 0.8")', 
#              color = "#00BFC4", parse = TRUE) 
#   
#   #### CATCHMENT:WETLAND AREA  -  SRP 
#   
#   SRPsink <- lm(SRP_Retention_percent ~ log(CWRatio), SRP.sink)
#   summary(SRPsink)
#   SRPsource <- lm(SRP_Retention_percent ~ log(CWRatio), SRP.source)
#   summary(SRPsource) 
#   
#   ggplot(x, aes(x = log(CWRatio), y = SRP_Retention_percent, color = SRP.S)) +
#     geom_point() +
#     ylim(-150, 115) +
#     geom_segment(aes(x = 1, y = 0, xend = 7, yend = 0), lty = 2, col = "black") +
#     geom_smooth(method = 'lm') +
#     annotate(geom = "text", x = 6, y = 115, label = 'bold("R2 = 0.26, p < 0.0001")',
#              color = "#F8766D", parse = TRUE) +
#     annotate(geom = "text", x = 6, y = 103, label = 'bold("R2 = 0.00, p = 0.9")', 
#              color = "#00BFC4", parse = TRUE)  
#  
#   
#   
#   
# summary(x$TP_Retention_percent) 
# summary(x$SRP_Retention_percent)
# 
# y <- x[which(x$Wetland_Type != "Mesocosm"),]
# summary(y$TP_Retention_percent)
# summary(y$SRP_Retention_percent)
# 
# 
# summary(x$TP_retention)
# summary(x$SRP_retention)
# 
# summary(y$TP_retention)
# summary(y$SRP_retention)




a <- x %>%
  ggplot(aes(x = (TP_load_in_g_m2_yr), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue") +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red") +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10')+
  xlab("TP load (g/m2/year)") +
  ylab("TP Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(TP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(TP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               color = "blue", size = 3) +
  geom_segment(aes(x = 0.01, y = 0, xend = 1800, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))


b <- x %>%
  ggplot(aes(x = (SRP_load_in_g_m2_yr), y = SRP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue") +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red") +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10')+
  xlab("PO4 load (g/m2/year)") +
  ylab("PO4 Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue") +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent <= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "red") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent <= 0),method = "lm", na.rm = TRUE,
               label.y.npc = "bottom" ,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               color = "red", size = 3) +
  geom_segment(aes(x = 0.01, y = 0, xend = 1800, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

c <- x %>%
  ggplot(aes(x = (Area_m2), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue") +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red") +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10')+
  xlab("Wetland area (m2)") +
  ylab("TP Retention (%)") +
  geom_segment(aes(x = 0.01, y = 0, xend = 8000000, yend = 0), lty = 2, col = "black") +
  theme_classic(base_size = 9) +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))


d <- x %>%
  ggplot(aes(x = (Area_m2), y = SRP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue") +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red") +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10')+
  xlab("Wetland area (m2)") +
  ylab("PO4 Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue") +
  # stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray50") +
  # stat_poly_eq(method = "lm", na.rm = TRUE,
  #              aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
  #              label.y.npc = "bottom" , size = 3, color = "gray50") +
  geom_segment(aes(x = 0.01, y = 0, xend = 8000000, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

e <- x %>%
  ggplot(aes(x = (HLR), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue") +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red") +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10', limits = c(1,5000))+
  xlab("HLR (m)") +
  ylab("TP Retention (%)") +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue") +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = "bottom" , size = 3, color = "gray30") +
  geom_segment(aes(x = 1, y = 0, xend = 5000, yend = 0), lty = 2, col = "black") +
  theme_classic(base_size = 9) +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))


f <- x %>%
  ggplot(aes(x = (HLR), y = SRP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue") +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red") +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10', limits = c(1,5000))+
  xlab("HLR (m)") +
  ylab("PO4 Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue") +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = "bottom" , size = 3, color = "gray30") +
  geom_segment(aes(x = 1, y = 0, xend = 5000, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))


g <- x %>%
  ggplot(aes(x = (CWRatio), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue") +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red") +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10', limits = c(1,5000))+
  xlab("Catchment:wetland area") +
  ylab("TP Retention (%)") +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue") +
  geom_segment(aes(x = 1, y = 0, xend = 5000, yend = 0), lty = 2, col = "black") +
  theme_classic(base_size = 9) +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))


h <- x %>%
  ggplot(aes(x = (CWRatio), y = SRP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue") +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red") +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10', limits = c(1,5000))+
  xlab("Catchment:wetland area") +
  ylab("PO4 Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue") +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = "bottom" , size = 3, color = "gray30") +
  geom_segment(aes(x = 1, y = 0, xend = 5000, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(25, 25, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))



plot_grid(a,b, c, d, e, f, g, h, nrow = 4, labels = c("a", "b", "c", "d",
                                                      "e", "f", "g", "h"))










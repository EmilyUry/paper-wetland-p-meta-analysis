

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
  x$ratio <- (x$SRP_outflow_mg_L/x$TP_outflow_mg_L)/(x$SRP_Inflow_mg_L/x$TP_Inflow_mg_L)
  
  
  ## mass at outflow
  x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
  x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)
  
  ### Hydraulic loading rate
  x$HLR <- x$Inflow_m3_yr/x$Area_m2
  
  x$CWRatio <- x$Catchment_area_ha/x$Area_m2*10000
  
  x$water_atten <- x$Inflow_m3_yr - x$Outflow_m3_yr
  x$water_atten_percent <- 100*x$water_atten/x$Inflow_m3_yr
  x$CAWA <- x$Catchment_area_ha/x$Area_m2*10000
  
}


a <- x %>%
  ggplot(aes(x = (TP_Inflow_mg_L), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10')+
  xlab(expression(paste("Influent [TP] (mg" %.% "L"^-1,")"))) +
  ylab("Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(TP_Retention_percent < 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "red") +
  stat_poly_eq(data = .%>% filter(TP_Retention_percent < 0),method = "lm", na.rm = TRUE,
               label.y.npc = "bottom" ,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               color = "red", size = 3) +
  geom_segment(aes(x = 0.01, y = 0, xend = 1800, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(10, 10, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))


b <- x %>%
  ggplot(aes(x = (SRP_Inflow_mg_L), y = SRP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10')+
  xlab(expression(paste("Influent [PO"[4]^"3-", "] (mg" %.% "L"^-1,")"))) +
  ylab("Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue", label.y.npc = 1) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent <= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "red") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent <= 0),method = "lm", na.rm = TRUE,
               label.y.npc = "bottom" ,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               color = "red", size = 3) +
  geom_segment(aes(x = 0.01, y = 0, xend = 1800, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(10, 10, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))


e <- x %>%
  ggplot(aes(x = (HLR), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10', limits = c(1,5000))+
  xlab(expression(paste("HLR (m" %.% "yr"^-1, ")"))) +
  #xlab("HLR (m/yr)") +
  ylab("Retention (%)") +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue", label.y.npc = 1) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = "bottom" , size = 3, color = "gray30") +
  geom_segment(aes(x = 1, y = 0, xend = 5000, yend = 0), lty = 2, col = "black") +
  theme_classic(base_size = 9) +
  theme(plot.margin = margin(10, 10, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))


f <- x %>%
  ggplot(aes(x = (HLR), y = SRP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10', limits = c(1,5000))+
  xlab(expression(paste("HLR (m" %.% "yr"^-1, ")"))) +
  #xlab("HLR (m/yr)") +
  ylab("Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue", label.y.npc = 1) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = "bottom" , size = 3, color = "gray30") +
  geom_segment(aes(x = 1, y = 0, xend = 5000, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(10, 10, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))


g <- x %>%
  ggplot(aes(x = (CWRatio), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10', limits = c(1,5000))+
  xlab("Catchment : wetland area") +
  ylab("Retention (%)") +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue", label.y.npc = 1) +
  geom_segment(aes(x = 1, y = 0, xend = 5000, yend = 0), lty = 2, col = "black") +
  theme_classic(base_size = 9) +
  annotate("rect", xmin = 200, xmax = 3500, ymin = -180, ymax = -70, color = "black", alpha = 0)+
  annotate("text", x = 800, y = -100, label = "sink", color = "blue")+
  annotate("text", x = 1100, y = -150, label = "source", color = "red") +
  annotate("point", x = 350, y = -100, pch = 21, color = "blue", fill = "blue", alpha = 0.6) +
  annotate("point", x = 350, y = -150, pch = 21, color = "red", fill = "red", alpha = 0.6) +
  theme(plot.margin = margin(10, 10, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        plot.title = element_text( hjust = 0.5, size = 14))+
  ggtitle("TP")



h <- x %>%
  ggplot(aes(x = (CWRatio), y = SRP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10', limits = c(1,5000))+
  xlab("Catchment : wetland area") +
  ylab("Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue", label.y.npc = 1) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = "bottom" , size = 3, color = "gray30") +
  geom_segment(aes(x = 1, y = 0, xend = 5000, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(10, 10, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        plot.title = element_text( hjust = 0.5, size = 14)) +
  ggtitle(expression("PO"[4]^"3-"))




#### figure for pub (Fig 7)

tiff(filename = "figures/scatter_stats3.tif", height=6.5, width=6, units= "in", res=800, compression= "lzw")

plot_grid(g, h, a,b, e, f,  nrow = 3, label_size = 10,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"), rel_heights = c(1,0.9, 0.9))

dev.off()


###### figure for supplement (Sup Fig 3)
options(scipen = 9)

c <- x %>%
  ggplot(aes(x = (Area_m2), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10')+
  xlab(expression(paste("Wetland area (m"^2, ")"))) +
  ylab("Retention (%)") +
  geom_segment(aes(x = 0.01, y = 0, xend = 8000000, yend = 0), lty = 2, col = "black") +
  theme_classic(base_size = 9) +
  annotate("rect", xmin = 0.02, xmax = 22, ymin = -180, ymax = -70, color = "black", alpha = 0)+
  annotate("text", x = 0.9, y = -100, label = "sink", color = "blue")+
  annotate("text", x = 1.1, y = -150, label = "source", color = "red") +
  annotate("point", x = 0.06, y = -100, pch = 21, color = "blue", fill = "blue", alpha = 0.6) +
  annotate("point", x = 0.06, y = -150, pch = 21, color = "red", fill = "red", alpha = 0.6) +
  theme(plot.margin = margin(10, 10, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))



d <- x %>%
  ggplot(aes(x = (Area_m2), y = SRP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10')+
  xlab(expression(paste("Wetland area (m"^2, ")"))) +
  ylab("Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue", label.y.npc = 1) +
  # stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray50") +
  # stat_poly_eq(method = "lm", na.rm = TRUE,
  #              aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
  #              label.y.npc = "bottom" , size = 3, color = "gray50") +
  geom_segment(aes(x = 0.01, y = 0, xend = 8000000, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(10, 10, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

x["Age_yr"][x["Age_yr"] == 0] <- 0.5

i <- x %>%
  ggplot(aes(x = (Age_yr), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  #xlim(-2, 70) +
  scale_x_continuous(trans='log10')+
  xlab("Wetland age (years)") +
  ylab("Retention (%)") +
  geom_segment(aes(x = 0.25, y = 0, xend = 60, yend = 0), lty = 2, col = "black") +
  theme_classic(base_size = 9) +
  theme(plot.margin = margin(10, 10, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        plot.title = element_text( hjust = 0.5)) +
  ggtitle("TP")



j <- x %>%
  ggplot(aes(x = (Age_yr), y = SRP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  #xlim(-2, 70) +
  scale_x_continuous(trans='log10')+
  xlab("Wetland age (years)") +
  ylab("Retention (%)") +
  theme_classic(base_size = 9) +
  stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
  stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               size = 3, color = "blue", label.y.npc = 1) +
  geom_segment(aes(x = 0.25, y = 0, xend = 60, yend = 0), lty = 2, col = "black") +
  theme(plot.margin = margin(10, 10, 0, 5),
      axis.text.y = element_text(angle = 90, hjust = 0.5),
      plot.title = element_text( hjust = 0.5)) +
  ggtitle(expression("PO"[4]^"3-"))

tiff(filename = "figures/Supp_scatter_stats.tif", height=4.5, width=6, units= "in", res=800, compression= "lzw")

plot_grid(i, j, c, d,  nrow = 2, label_size = 10,
          labels = c("A", "B", "C", "D"), rel_heights = c(1,0.9))

dev.off()





############### supplemental figure 5
a <- x %>%
  ggplot(aes(x=  HLR, y = water_atten_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  #scale_fill_manual("Retention (%)", values = c("green", "yellow", "orange", "red"),
  #                  label = c("67-100", "33-67", "0-33", "<0"))+
  scale_x_continuous(trans='log10', limits = c(0.9, 1000))+
  #scale_y_continuous(trans='log10')+
  ylim(-50, 120) +
  xlab(expression(paste("HLR (m"%.% "year" ^"-1", ")")))+
  ylab("Water attenuation (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = 'none')


b <- x %>%
  ggplot(aes(x=  HLR, y = CAWA))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 3)+
  scale_x_continuous(trans='log10', limits = c(0.9, 1000))+
  scale_y_continuous(trans='log10')+
  xlab(expression(paste("HLR (m"%.% "year" ^"-1", ")")))+
  ylab("Catchment:wetland area")+
  theme_classic(base_size = 12) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 3, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = 'none')


plot_grid(a,b, nrow = 1, labels = c("A", "B"))


tiff(filename = "figures/Supp_fig5_hlr.tif", height=2.5, width=6, units= "in", res=800, compression= "lzw")

plot_grid(a,b, nrow = 1, labels = c("A", "B"))

dev.off()



#### old versions

### quartiles w shading
{
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
  
}





# ############### add ratio plots
{ 
# 
# x$SS <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "source", "sink")
# x$SS <- as.factor(x$SS)
# 
# ab <- x %>%
#   ggplot(aes(x = (TP_Inflow_mg_L), y = ratio)) +
#   geom_point(data = . %>% filter(SS == "sink"), color = "blue", alpha = 0.5) +
#   geom_point(data = . %>% filter(SS == "source"), color = "red", alpha = 0.5) +
#   #ylim(-190, 150) +
#   scale_x_continuous(trans='log10')+
#   scale_y_continuous(trans='log10')+
#     xlab("TP in (mg/L)") +
#   ylab("Magnification Ratio") +
#   theme_classic(base_size = 9) +
#   stat_poly_line(data = .%>% filter(SS == "source"), color = "red", method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   stat_poly_eq(data = .%>% filter(SS == "source"), color = "red",method = "lm", na.rm = TRUE, size = 2,
#                label.y.npc = 1 , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")), ) +
#   stat_poly_line(data = .%>% filter(SS == "sink"), color = "blue",method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   stat_poly_eq(data = .%>% filter(SS == "sink"), color = "blue", size = 2, method = "lm", na.rm = TRUE,
#                label.y.npc = "bottom" , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*"))) +
#   # stat_poly_line(color = "gray30",method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   # stat_poly_eq( color = "gray30", size = 2, method = "lm", na.rm = TRUE,
#   #              label.y.npc = "top" , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*"))) +
#   geom_segment(aes(x = 0.01, y = 1, xend = 1800, yend = 1), lty = 2, col = "black") +
#   theme(plot.margin = margin(10, 10, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# ab
# 
# cd <- x %>%
#   ggplot(aes(x = (Area_m2), y = ratio)) +
#   geom_point(data = . %>% filter(SS == "sink"), color = "blue", alpha = 0.5) +
#   geom_point(data = . %>% filter(SS == "source"), color = "red", alpha = 0.5) +
#   #ylim(-190, 150) +
#   scale_x_continuous(trans='log10')+
#   scale_y_continuous(trans='log10')+
#   xlab("Wetland area (m2)") +
#   ylab("Magnification Ratio") +
#   theme_classic(base_size = 9) +
#   #stat_poly_line(data = .%>% filter(SS == "source"), color = "red", method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   #stat_poly_eq(data = .%>% filter(SS == "source"), color = "red",method = "lm", na.rm = TRUE, size = 2,
#   #             label.y.npc = 1 , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")), ) +
#   stat_poly_line(data = .%>% filter(SS == "sink"), color = "blue",method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   stat_poly_eq(data = .%>% filter(SS == "sink"), color = "blue", size = 2, method = "lm", na.rm = TRUE,
#                label.y.npc = "bottom" , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*"))) +
#   # stat_poly_line(color = "gray30",method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   # stat_poly_eq( color = "gray30", size = 2, method = "lm", na.rm = TRUE,
#   #              label.y.npc = "top" , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*"))) +
#   geom_segment(aes(x = 0.01, y = 1, xend = 18000000, yend = 1), lty = 2, col = "black") +
#   theme(plot.margin = margin(10, 10, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# cd
# 
# ef <- x %>%
#   ggplot(aes(x = (HLR), y = ratio)) +
#   geom_point(data = . %>% filter(SS == "sink"), color = "blue", alpha = 0.5) +
#   geom_point(data = . %>% filter(SS == "source"), color = "red", alpha = 0.5) +
#   #ylim(-190, 150) +
#   scale_x_continuous(trans='log10')+
#   scale_y_continuous(trans='log10')+
#   xlab("HLR (m/yr)") +
#   ylab("Magnification Ratio") +
#   theme_classic(base_size = 9) +
#   #stat_poly_line(data = .%>% filter(SS == "source"), color = "red", method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   #stat_poly_eq(data = .%>% filter(SS == "source"), color = "red",method = "lm", na.rm = TRUE, size = 2,
#   #             label.y.npc = 1 , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")), ) +
#   stat_poly_line(data = .%>% filter(SS == "sink"), color = "blue",method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   stat_poly_eq(data = .%>% filter(SS == "sink"), color = "blue", size = 2, method = "lm", na.rm = TRUE,
#                label.y.npc = "bottom" , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*"))) +
#    stat_poly_line(color = "gray30",method = "lm", fullrange = TRUE, na.rm = TRUE) +
#    stat_poly_eq( color = "gray30", size = 2, method = "lm", na.rm = TRUE,
#                 label.y.npc = "top" , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*"))) +
#   geom_segment(aes(x = 0.01, y = 1, xend = 18000000, yend = 1), lty = 2, col = "black") +
#   theme(plot.margin = margin(10, 10, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
#   
# ef  
#   
# 
# gh <- x %>%
#   ggplot(aes(x = (CWRatio), y = ratio)) +
#   geom_point(data = . %>% filter(SS == "sink"), color = "blue", alpha = 0.5) +
#   geom_point(data = . %>% filter(SS == "source"), color = "red", alpha = 0.5) +
#   #ylim(-190, 150) +
#   scale_x_continuous(trans='log10')+
#   scale_y_continuous(trans='log10')+
#   xlab("Catchment:wetland area") +
#   ylab("Magnification Ratio") +
#   theme_classic(base_size = 9) +
#   stat_poly_line(data = .%>% filter(SS == "source"), color = "red", method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   stat_poly_eq(data = .%>% filter(SS == "source"), color = "red",method = "lm", na.rm = TRUE, size = 2,
#                label.y.npc = 1 , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")), ) +
#   # stat_poly_line(data = .%>% filter(SS == "sink"), color = "blue",method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   # stat_poly_eq(data = .%>% filter(SS == "sink"), color = "blue", size = 2, method = "lm", na.rm = TRUE,
#   #              label.y.npc = "bottom" , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*"))) +
#   stat_poly_line(color = "gray30",method = "lm", fullrange = TRUE, na.rm = TRUE) +
#   stat_poly_eq( color = "gray30", size = 2, method = "lm", na.rm = TRUE,
#                 label.y.npc = "top" , aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*"))) +
#   geom_segment(aes(x = 0.01, y = 1, xend = 18000000, yend = 1), lty = 2, col = "black") +
#   theme(plot.margin = margin(10, 10, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# gh  
# 
#   
# tiff(filename = "figures/Supp_scatter_stats_Ratio.tif", height=6.5, width=6, units= "in", res=800, compression= "lzw")
# 
# plot_grid(a,b, ab, c, d, cd, e, f, ef, g, h, gh, nrow = 4, label_size = 10,
#           labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))
# 
# dev.off()
# 

# 
# 
# ########## load (not used)
# a <- x %>%
#   ggplot(aes(x = (TP_load_in_g_m2_yr), y = TP_Retention_percent)) +
#   geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
#   geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red", alpha = 0.5) +
#   ylim(-190, 150) +
#   scale_x_continuous(trans='log10')+
#   xlab("TP load (g/m2/year)") +
#   ylab("TP Retention (%)") +
#   theme_classic(base_size = 9) +
#   stat_poly_line(data = .%>% filter(TP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
#   stat_poly_eq(data = .%>% filter(TP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                color = "blue", size = 2, label.y.npc = 1) +
#   geom_segment(aes(x = 0.01, y = 0, xend = 1800, yend = 0), lty = 2, col = "black") +
#   theme(plot.margin = margin(10, 10, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# 
# b <- x %>%
#   ggplot(aes(x = (SRP_load_in_g_m2_yr), y = SRP_Retention_percent)) +
#   geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
#   geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red", alpha = 0.5) +
#   ylim(-190, 150) +
#   scale_x_continuous(trans='log10')+
#   xlab("PO4 load (g/m2/year)") +
#   ylab("PO4 Retention (%)") +
#   theme_classic(base_size = 9) +
#   stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
#   stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                size = 2, color = "blue", label.y.npc = 1) +
#   stat_poly_line(data = .%>% filter(SRP_Retention_percent <= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "red") +
#   stat_poly_eq(data = .%>% filter(SRP_Retention_percent <= 0),method = "lm", na.rm = TRUE,
#                label.y.npc = "bottom" ,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                color = "red", size = 2) +
#   geom_segment(aes(x = 0.01, y = 0, xend = 1800, yend = 0), lty = 2, col = "black") +
#   theme(plot.margin = margin(10, 10, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# table(x$Water_regime)
# x$source <- ifelse(x$TP_Retention_percent < 0, 1,0)
# table(x$Water_regime, x$source)
# x$source <- ifelse(x$SRP_Retention_percent < 0, 1,0)
# table(x$Water_regime, x$source)
# 
# 
# plot(x$Area_m2, x$HLR, log= "xy")
# 
# ggplot(x, aes(x = (Area_m2), y = HLR)) +
#   geom_point() +
#   scale_y_continuous(trans='log10')+
#   scale_x_continuous(trans='log10')+
#   stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
#   stat_poly_eq(method = "lm", na.rm = TRUE,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                size = 2, color = "blue", label.y.npc = 1) +
#   theme_classic(base_size = 9) +
#   theme(plot.margin = margin(10, 10, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
#   
# test <- x[which(x$TP_Retention_percent >0),]
# (unique(test$WetlandID))
# 
# summary(test$SRP_Retention_percent)
# 
# 
# 
# 
# 
# 
# 





# 
# 
# 
# #### checking some correlations
# 
# source <- x[which(x$TP_Retention_percent < 0 | x$SRP_Retention_percent <0),]
# 
# plot(source$TP_Inflow_mg_L , source$TP_Retention_percent, 
#      log = "x", ylim = c(-200,100))
# 
# plot(source$SRP_Inflow_mg_L , source$SRP_Retention_percent, 
#      log = "x", ylim = c(-200,100))
# ggplot(source, aes(x= SRP_Inflow_mg_L, y= SRP_Retention_percent))+
#   geom_point()+
#   scale_x_continuous(trans='log10')+
#   geom_smooth(method = "lm") +
#   stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
#   stat_poly_eq(method = "lm", na.rm = TRUE,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                size = 3, color = "blue", label.y.npc = 1) 
# 
# x$SS <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "source", "sink")
# x$SS <- as.factor(x$SS)
# 
# table(x$SS)  
# 
# boxplot(x$TP_Inflow_mg_L, x$SS )
# boxplot(x$SRP_Inflow_mg_L, x$SS )
# 
# ggplot(x, aes(x = SS, y = SRP_Inflow_mg_L)) +
#   geom_boxplot()
# 
# ### t.test
# library(rstatix)
# # library(ggpubr)
# # library(stringr)
# 
# x %>%
#   group_by(SS) %>%
#   get_summary_stats(SRP_Inflow_mg_L, type = "median_mad")
# pwc <- x %>%
#   pairwise_t_test(SRP_Inflow_mg_L ~  SS, p.adjust.method = 'bonferroni')
# pwc
# 
# x %>%
#   group_by(SS) %>%
#   get_summary_stats(TP_Inflow_mg_L, type = "median_mad")
# pwc <- x %>%
#   pairwise_t_test(TP_Inflow_mg_L ~  SS, p.adjust.method = 'bonferroni')
# pwc
# 
# 
# #### size and retention (per mass)
# x$TP_retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
# 
# x %>%
#   ggplot(aes(x = (Area_m2), y = TP_retention)) +
#   geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
#   geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red", alpha = 0.5) +
#   ylim(-110, 130) +
#   scale_x_continuous(trans='log10')+
#   xlab("Wetland area (m2)") +
#   ylab("TP Retention (g/m2/yr)") +
#   stat_poly_line(data = .%>% filter(TP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
#   stat_poly_eq(data = .%>% filter(TP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                size = 2, color = "blue", label.y.npc = 1) +
#   #stat_poly_line(data = .%>% filter(TP_Retention_percent <= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "red") +
#   # stat_poly_eq(data = .%>% filter(TP_Retention_percent <= 0),method = "lm", na.rm = TRUE,
#   #              label.y.npc = "bottom" ,
#   #              aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#   #              color = "red", size = 2) +
#   stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray50") +
#   stat_poly_eq(method = "lm", na.rm = TRUE,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                label.y.npc = "bottom" , size = 2, color = "gray50") +
#   geom_segment(aes(x = 0.01, y = 0, xend = 8000000, yend = 0), lty = 2, col = "black") +
#   theme_classic(base_size = 9) +
#   theme(plot.margin = margin(10, 10, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# 
# x$SRP_retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out
# 
# 
# x %>%
#   ggplot(aes(x = (Area_m2), y = SRP_retention)) +
#   geom_point(data = . %>% filter(SRP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
#   geom_point(data = . %>% filter(SRP_Retention_percent <= 0), color = "red", alpha = 0.5) +
#   ylim(-60, 120) +
#   scale_x_continuous(trans='log10')+
#   xlab("Wetland area (m2)") +
#   ylab("PO4 Retention (g/m2/year)") +
#   theme_classic(base_size = 9) +
#   stat_poly_line(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "blue") +
#   stat_poly_eq(data = .%>% filter(SRP_Retention_percent >= 0),method = "lm", na.rm = TRUE,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                size = 2, color = "blue", label.y.npc = 1) +
#   stat_poly_line(data = .%>% filter(SRP_Retention_percent <= 0),method = "lm", fullrange = TRUE, na.rm = TRUE, color = "red") +
#   stat_poly_eq(data = .%>% filter(SRP_Retention_percent <= 0),method = "lm", na.rm = TRUE,
#                label.y.npc = "bottom" ,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                color = "red", size = 2) +
#   stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray50") +
#   stat_poly_eq(method = "lm", na.rm = TRUE,
#                aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
#                label.y.npc = 0.9 , size = 2, color = "gray50") +
#   geom_segment(aes(x = 0.01, y = 0, xend = 8000000, yend = 0), lty = 2, col = "black") +
#   theme(plot.margin = margin(10, 10, 0, 5),
#         axis.text.y = element_text(angle = 90, hjust = 0.5))
# 
# 
# 
# 
# 
# 
# 
# 
# 

}





#' ---
#' title: "Scatter plots and stat tests for Fig 4
#' author: "Emily Ury"
#' date: "March 31, 2023"
#' ---

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpmisc)


## Data set-up
x <- read.csv("Wetland_P_Clean3.csv", header = T)
{
  #x$Water_regime <- as.factor(x$Water_regime)
  # lX<-log(x[,c(11,13, 16,17)])
  # colnames(lX)<-paste("log",colnames(lX),sep="")
  # x<-cbind(x,lX); rm(lX)

  
  ## mass at outflow

  ### Hydraulic loading rate
  x$HLR <- x$Inflow_m3_yr/x$Area_m2
  x$CWRatio <- x$Catchment_area_ha/x$Area_m2*10000

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
  xlab("Catchment to Wetland Area Ratio") +
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
  xlab("Catchment to Wetland Area Ratio") +
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




#### figure for pub (Fig 6)

tiff(filename = "figures/scatter_stats3.tif", height=6.5, width=6, units= "in", res=800, compression= "lzw")

plot_grid(g, h, a,b, e, f,  nrow = 3, label_size = 10,
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), rel_heights = c(1,0.9, 0.9), label_fontface = 1)

dev.off()


###### figure for supplement (Sup Fig 3)
options(scipen = 9)

c <- x %>%
  ggplot(aes(x = (Area_m2), y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(TP_Retention_percent >= 0), color = "blue", alpha = 0.5) +
  geom_point(data = . %>% filter(TP_Retention_percent <= 0), color = "red", alpha = 0.5) +
  ylim(-190, 150) +
  scale_x_continuous(trans='log10')+
  xlab(expression(paste("Wetland Area (m"^2, ")"))) +
  ylab("Retention (%)") +
  geom_segment(aes(x = 0.01, y = 0, xend = 8000000, yend = 0), lty = 2, col = "black") +
  theme_classic(base_size = 9) +
  annotate("rect", xmin = 0.02, xmax = 22, ymin = -180, ymax = -70, color = "black", alpha = 0)+
  annotate("text", x = 0.5, y = -100, label = "sink", color = "blue")+
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
  xlab(expression(paste("Wetland Area (m"^2, ")"))) +
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
  xlab("Wetland Age (years)") +
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
  xlab("Wetland Age (years)") +
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
          labels = c("(a)", "(b)", "(c)", "(d)"), rel_heights = c(1,0.9), label_fontface = 1)

dev.off()




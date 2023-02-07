

### new monthly scatter plots ###


setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop



library(ggplot2)
library(tidyverse)
library(cowplot)
#library(forcats) ## need for fct_relevel
library(ggpmisc)


options(scipen = 9)

## data setup
{
x <- read.csv("Monthly_Wetland_P_Clean.csv", header = T)
x <- x[which(x$data_type == "both" | x$data_type == "load"),]
table(x$Source)
unique(x$Source)
x$TP_Retention <- x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo
x$SRP_Retention <- x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo
x$HRL_m_month <- x$Monthly_Inflow_m3_month/x$SA_m2
x$HLR <- x$Monthly_Inflow_m3_month/x$SA_m2
x$water_atten <- x$Monthly_Inflow_m3_month - x$Monthly_Outflow_m3_month
x$water_atten_percent <- 100*x$water_atten/x$Monthly_Inflow_m3_month
x$ratio <- (x$SRP_OUT_mg_L/x$TP_OUT_mg_L)/(x$SRP_IN_mg_L/x$TP_IN_mg_L)
x$ratio2 <- (x$SRP_OUT_g_m2_mo/x$TP_OUT_g_m2_mo)/(x$SRP_IN_g_m2_mo/x$TP_IN_g_m2_mo)
x$RATIO <- ifelse(is.finite(x$ratio2), x$ratio2, x$ratio)

## subset data
df <-x %>% select(Short_Ref, Short_ID,  Short_year, Month, TP_IN_g_m2_mo, TP_Retention, TP_Retention_percent, 
                  SRP_IN_g_m2_mo, SRP_Retention, SRP_Retention_percent, TP_OUT_g_m2_mo, SRP_OUT_g_m2_mo,
                  HRL_m_month, HLR, water_atten, water_atten_percent, ratio, ratio2, RATIO)
df$Unique_ID <- paste(x$Short_Ref, x$Short_ID, x$Short_year, sep = "_")
site_year <-unique(df$Unique_ID)
df <- df[which(df$Short_Ref !=  17),]       ### drop site 17 because it is too gappy
df <- df[which(df$Short_Ref !=  5),] 
df <- df[which(df$Short_Ref !=  15),] 
df <- df[which(df$Short_year != "YN"),]     ### drop partial years
#df <- na.omit(df)                           ### drop months with NAs
df <- df %>%                                ### reorder months in order
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 
df$Study <- as.factor(df$Short_Ref)
levels(df$Study) <- c("Audet et al. 2020", "Hoffmann et al. 2012",
                    "Choate et al. 1990", "Page et al. 2020")

rm(x)
}

#### black and white

A <- df %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 2)+
  scale_x_continuous(trans='log10')+
  ylim(-120,130) +
  xlab(expression(paste("HLR (m"%.% "month" ^"-1", ")")))+
  ylab("Retention (%)")+
  theme_classic(base_size = 8) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 2, color = "gray30") +
  geom_hline(yintercept=0, linetype = "dashed") + 
  theme(plot.margin = margin(10, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = "none", legend.title = element_blank(), 
        plot.title = element_text(hjust = -0.15)) +
  ggtitle("TP")


B <- df %>%
  ggplot(aes(x=  HLR, y = SRP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 2)+
  scale_x_continuous(trans='log10')+
  ylim(-120, 130) +
  xlab(expression(paste("HLR (m"%.% "month" ^"-1", ")")))+
  ylab("Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 2, color = "gray30") +
  geom_hline(yintercept=0, linetype = "dashed") + 
  theme_classic(base_size =8) +
  theme(plot.margin = margin(5, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'right',  
        plot.title = element_text( hjust = -0.15),
        legend.background = element_rect(color = "black")) +
  ggtitle(expression("PO"[4]^"3-"))

plot_grid(A, B, nrow = 1, rel_widths = c(1,1.3), label_size = 10)


tiff(filename = "figures/Fig5_new_monthly_bw.tif", height=2.5, width=5, units= "in", res=800, compression= "lzw")

plot_grid(A, B, nrow = 1, rel_widths = c(1,1), label_size = 10)

dev.off()


### color by flow attenuation

A <- df %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent,
             color = water_atten_percent, fill = water_atten_percent))+
  geom_point(pch =21, alpha = 0.7, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_x_continuous(trans='log10')+
  ylim(-120,130) +
  xlab(expression(paste("HLR (m"%.% "month" ^"-1", ")")))+
  ylab("Retention (%)")+
  theme_classic(base_size = 8) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 2, color = "gray30") +
  geom_hline(yintercept=0, linetype = "dashed") + 
    theme(plot.margin = margin(10, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = "none", legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("TP")


B <- df %>%
  ggplot(aes(x=  HLR, y = SRP_Retention_percent, 
             color = water_atten_percent, fill = water_atten_percent))+
  geom_point(pch =21, alpha = 0.7, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue", name = "Water \nretention \n(%)") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue", name = "Water \nretention \n(%)") +
  scale_x_continuous(trans='log10')+
  ylim(-120, 130) +
  xlab(expression(paste("HLR (m"%.% "month" ^"-1", ")")))+
  ylab("Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 2, color = "gray30") +
  geom_hline(yintercept=0, linetype = "dashed") + 
  theme_classic(base_size =8) +
  theme(plot.margin = margin(5, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'right',  
        plot.title = element_text( hjust = 0.5),
        legend.background = element_rect(color = "black")) +
  ggtitle(expression("PO"[4]^"3-"))

plot_grid(A, B, nrow = 1, rel_widths = c(1,1.3), label_size = 10)


tiff(filename = "figures/Fig5_new_monthly.tif", height=2.5, width=5, units= "in", res=800, compression= "lzw")

plot_grid(A, B, nrow = 1, rel_widths = c(1,1.3), label_size = 10)

dev.off()





C <- df %>%
  ggplot(aes(x=  HLR, y = RATIO, 
             color = water_atten_percent, fill = water_atten_percent))+
  geom_point(pch =21, alpha = 0.7, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_x_continuous(trans='log10', limits = c(0.001,14))+
  ylim(0.1, 2.2) +
  xlab(expression(paste("HLR (m"%.% "month" ^"-1", ")")))+
  ylab("PO4 Magnification Ratio")+
  # stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  # stat_poly_eq(method = "lm", na.rm = TRUE,
  #              aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
  #              label.y.npc = 1, size = 2, color = "gray30") +
  #geom_segment(aes(x = 0.001, y = 1, xend = 12, yend = 1), lty = 2, col = "black") +
  geom_hline(yintercept=1, linetype = "dashed") + 
  theme_classic(base_size =8) +
  theme(plot.margin = margin(10, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none', legend.title = element_blank(), 
        plot.title = element_text( hjust = 0.5)) +
  ggtitle("Magnification ratio")
C





##### THIS IS FIGURE 5

tiff(filename = "figures/Fig5_new_monthly_b.tif", height=2.5, width=7, units= "in", res=800, compression= "lzw")

plot_grid(A, B, C, nrow = 1, rel_widths = c(1, 1.3, 1), label_size = 10)

dev.off()










##### supplemental figure


TP <- ggplot(df, aes(x = HRL_m_month, y = TP_Retention_percent, 
               color = water_atten_percent, fill = water_atten_percent))+
  geom_point(pch =21, alpha = 0.7, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue", name = "Water \nretention \n(%)") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue",  name = "Water \nretention \n(%)") +
  facet_wrap(~ Month) +
  scale_x_continuous(trans ='log10') +
  coord_cartesian(ylim = c(-200,100)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(color = "black")) +
  xlab(expression(paste("HLR (m"%.% "month" ^"-1", ")")))+
  ylab("Retention (%)") + 
  ggtitle("TP")



PO4 <- ggplot(df, aes(x = HRL_m_month, y = SRP_Retention_percent, 
               color = water_atten_percent, fill = water_atten_percent))+
  geom_point(pch =21, alpha = 0.7, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue", name = "Water \nretention \n(%)") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue",  name = "Water \nretention \n(%)") +
  facet_wrap(~ Month) +
  scale_x_continuous(trans ='log10') +
  coord_cartesian(ylim = c(-200,100)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(color = "black")) +
  xlab(expression(paste("HLR (m"%.% "month" ^"-1", ")")))+
  ylab("Retention (%)") + 
  ggtitle(expression("PO"[4]^"3-"))




##### This is Supplemental figure 

tiff(filename = "figures/Supp_by_month.tif", height=9, width=8, units= "in", res=800, compression= "lzw")

plot_grid(TP, PO4, nrow = 2)

dev.off()



###### old version, colored by study

ggplot(df, aes(x = HRL_m_month, y = TP_Retention_percent, color = Study)) +
  geom_point() +
  #geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ Month) +
  scale_x_continuous(trans ='log10') +
  coord_cartesian(ylim = c(-200,100)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw(base_size = 14) +
  xlab(expression(paste("HLR (m"%.% "month" ^"-1", ")")))+
  ylab("Retention (%)") + 
  ggtitle("TP")



ggplot(df, aes(x = HRL_m_month, y = SRP_Retention_percent, color = Study)) +
  geom_point() +
  #geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ Month) +
  scale_x_continuous(trans ='log10') +
  coord_cartesian(ylim = c(-200,100)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw(base_size = 14) +
  xlab(expression(paste("HLR (m"%.% "month" ^"-1", ")")))+
  ylab("Retention (%)") + 
  ggtitle(expression("PO"[4]^"3-"))














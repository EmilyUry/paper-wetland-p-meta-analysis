



#### supp figure 4
#### monthly HLR vs Retention



library(ggplot2)
library(cowplot)
library(forcats) ## need for fct_relevel
library(dplyr) # easier data wrangling 
#library(viridis)
#library(gridExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
library(ggpmisc)

options(scipen = 10)


setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop



#### data set-up
x <- read.csv("Lit_data_monthly.csv")
x$HLR <- x$Monthly_Inflow_m3_month/x$SA_m2



A <- x %>%
  ggplot(aes(x=  HLR, y = TP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_x_continuous(trans='log10')+
  ylim(-120,130) +
  xlab(expression(paste("HLR (m" %.% "month"^-1, ")"))) +
  ylab("Retention (%)")+
  theme_classic(base_size = 8) +
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 2, color = "gray30") +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = "none", legend.title = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed")


B <- x %>%
  ggplot(aes(x=  HLR, y = SRP_Retention_percent))+
  geom_point(pch =21, fill = "black", alpha = 0.5, cex = 2)+
  scale_fill_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_color_gradient2(midpoint = 80, low = "red",  high = "blue", mid = "blue") +
  scale_x_continuous(trans='log10')+
  ylim(-120, 130) +
  xlab(expression(paste("HLR (m" %.% "month"^-1, ")"))) +
  ylab("Retention (%)")+
  stat_poly_line(method = "lm", fullrange = TRUE, na.rm = TRUE, color = "gray30") +
  stat_poly_eq(method = "lm", na.rm = TRUE,
               aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
               label.y.npc = 1, size = 2, color = "gray30") +
  theme_classic(base_size =8) +
  theme(plot.margin = margin(25, 5, 0, 5),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'right', legend.title = element_blank())+
  geom_hline(yintercept = 0, linetype = "dashed")



tiff(filename = "figures/Supp_figure 4.tif", height=2.5, width=5, units= "in", res=800, compression= "lzw")

plot_grid(A, B, nrow = 1, labels = c("TP", "PO4"), rel_widths = c(1,1), label_size = 10)

dev.off()











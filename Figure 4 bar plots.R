

#' ---
#' title: "Figure 4. Bar plots by retention quartile"
#' author: "Emily Ury"
#' date: "March 30, 2023"
#' ---

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop

library(ggplot2)
library(tidyverse)
library(cowplot)



## Data set-up
x <- read.csv("Wetland_P_Clean3.csv", header = T)
{
  x$Water_regime <- as.factor(x$Water_regime)
  lX<-log(x[,c(11,13, 16,17)])
  colnames(lX)<-paste("log",colnames(lX),sep="")
  x<-cbind(x,lX); rm(lX)
  
  ## mass at outflow
  x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
  x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)
  
  ### Hydraulic loading rate
  x$HLR <- x$Inflow_m3_yr/x$Area_m2
  
  x$TP_retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
  x$SRP_retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out
}


### quartiles w shading

scatter <- ggplot(x, aes(x = SRP_Retention_percent, y = TP_Retention_percent)) +
  geom_rect(aes(xmin = -100, xmax = 0, ymin = -100, ymax = 100), fill = "#ffc8c2") +
  geom_rect(aes(xmin = -100, xmax = 100, ymin = -100, ymax = 0), fill = "#ffc8c2") + 
  geom_rect(aes(xmin = 0, xmax = 33, ymin = 0, ymax = 100), fill = "#ffe2c2") +
  geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 33), fill = "#ffe2c2") +
  geom_rect(aes(xmin = 33, xmax = 67, ymin = 33, ymax = 100), fill = "#fffdc2") +
  geom_rect(aes(xmin = 33, xmax = 100, ymin = 33, ymax = 67), fill = "#fffdc2") +
  geom_rect(aes(xmin = 67, xmax = 100, ymin = 67, ymax = 100), fill = "#c5ffc2") +
  geom_point(size = 0.3) + 
  theme(legend.position = "none") +
  xlim(-100, 160) +
  ylim(-100, 105) + 
  theme_classic(base_size = 9) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = unit(c(t = 0.1, r = 0.0, b = 0, l = 0.3), "cm")) +
  geom_abline(slope = 1, intercept = 0) +
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  xlab(expression(paste("PO"[4]^"3-", " Retention (%)"))) +
  ylab("TP Retention (%)") +
  annotate(geom = "text", x = 130, y = -16, label = "< 0", size = 2.5) + 
  annotate(geom = "text", x = 135, y = 16, label = "0-33", size = 2.5) + 
  annotate(geom = "text", x = 135, y = 50, label = "33-67", size = 2.5) + 
  annotate(geom = "text", x = 140, y = 84, label = "67-100", size = 2.5) +
  geom_segment(aes(x = -100, y = 0, xend = 110, yend = 0), lty = 2) +
  geom_segment(aes(x = 0, y = -100, xend = 0, yend = 105), lty = 2) +
  geom_segment(aes(x = 33, y = 33, xend = 33, yend = 105), lty = 2) +
  geom_segment(aes(x = 33, y = 33, xend = 110, yend = 33) , lty = 2) +
  geom_segment(aes(x = 67, y = 67, xend = 67, yend = 105), lty = 2) +
  geom_segment(aes(x = 67, y = 67, xend = 110, yend = 67) , lty = 2) 


x$quad <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "< 0", 
                 ifelse(x$TP_Retention_percent < 33 | x$SRP_Retention_percent < 33, "0-33",
                        ifelse(x$TP_Retention_percent < 67 | x$SRP_Retention_percent < 67, "33-67", "67-100")))
quad.sum <- table(x$quad)
n <- as.data.frame(quad.sum)



##### Wetland Type *** NEW ***

{x$Catchment_Type <- as.factor(x$Catchment_Type)
x$group <- ifelse(x$Catchment_Type == "WWTP", "WWT", 
                  ifelse(x$Wetland_Type == "Mesocosm", "Mesocosm", "Restored/\nConstruct."))

table(x$quad, x$group)
summary <- table(x$quad, x$group)
m <- as.data.frame(summary)
  WT <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(values = c("#440154FF", "#44015477","#2c728eFF"  )) +
    labs(x = "  ", y = "  ", fill = "Wetland \nType") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),
          plot.margin = unit(c(t = 0.1, r = 0.0, b = 0, l = 0.3), "cm"))
  WT
}


#### FLOW REGIME
{levels(x$Water_regime)
  x <- x %>%                                ### reorder flow regime
    mutate(Water_regime = fct_relevel(Water_regime, "continuous, constant" , "intermittent, constant" ,
                                      "continuous, variable", "intermittent, variable", "n.s.")) 
  table(x$quad, x$Water_regime)
  summary <- table(x$quad, x$Water_regime)
  m <- as.data.frame(summary)
  
  FlowR <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("C. reg.", "I. reg.", "C.  unreg.", "I. unreg.", "N.S." ),
                      values = c("#440154FF",   "#44015477", "#2c728eFF","#2c728e55",  "#31313122")) +
    labs(x = " ", y = " ", fill = "Hydrologic\n Regime") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),
          plot.margin = unit(c(t = 0.1, r = 0.0, b = 0, l = 0.3), "cm")) +
    guides(fill=guide_legend(ncol=1))
  FlowR
}

#### INFLOW TP CONCENTRATION
{x$bins <- cut_number(x$TP_Inflow_mg_L, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  TP <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c(" < 0.1", "0.1 - 0.2", "0.2 - 1.7  ", "1.7+"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF") ) +
    labs(x = "Retention bins (%)",y = "Frequency ", parse = TRUE, fill = expression(atop("Influent [TP]",
                                                                          atop(textstyle(paste("(mg" %.% "L"^-1, ")")))))) +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),  
          plot.margin = unit(c(t = 0.1, r = 0.0, b = 0, l = 0.3), "cm")) 
  TP
}

#### INFLOW SRP CONCENTRATION
{
  x$bins <- cut_number(x$SRP_Inflow_mg_L, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  SRP <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c(" < 0.05", "0.05 - 0.1", "0.1 - 0.6", "0.6+"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF") ) +
    labs(x = "Retention bins (%) ",y = "  ", parse = TRUE, fill = expression(atop(paste("Influent [PO"[4]^"3-","]"), 
                                                                         atop(textstyle(paste("(mg" %.% "L"^-1, ")")))))) +    
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),
          plot.margin = unit(c(t = 0.1, r = 0.0, b = 0, l = 0.3), "cm")) 
  SRP
}

### WETLAND SIZE
{x$bins <- cut_number(x$Area_m2, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  size <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("smallest    ", " ", " ", "largest    "),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF"  )) +
    labs(x = "  ", y = " ", fill = "Wetland \nSize") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),
          plot.margin = unit(c(t = 0.1, r = 0.0, b = 0, l = 0.3), "cm")) 
  size
}

### WETLAND AGE
{table(x$quad, x$Age_yr)
  x$bins <- cut_number(x$Age_yr, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  
  age <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("<2 year", "2 years", "3-4 years", "5+ years"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF"  )) +
    labs(x = " ",y = "Frequency ", fill = "Wetland \nAge") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),
          plot.margin = unit(c(t = 0.1, r = 0.0, b = 0, l = 0.3), "cm"))
  
  age
}

#### HLR
{x$bins <- cut_number(x$HLR, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  hlr <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("< 7", "7 - 14", "14 - 36    ", "36 +"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF"  )) +
    #labs(x = "Retention bins (%) ", y = " ", fill = "HLR\n (m/yr)") +
    labs(x = "Retention bins (%) ", y = " ", fill = expression(atop("HLR",
                                                                atop(textstyle(paste("(m" %.% "yr"^-1,")")))))) +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),
          plot.margin = unit(c(t = 0.1, r = 0.0, b = 0, l = 0.2), "cm"))
  
  hlr
}

#### WETLAND:CATCHMENT ratio
{ x$CWRatio <- x$Catchment_area_ha/x$Area_m2*10000
  x$bins <- cut_number(x$CWRatio, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  
  ratio <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("< 3.3", "19 - 36", "36 - 200  ", "200+"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF")) +
    labs(x = " ", y = " ", fill = "Catchment \nto Wetland\nArea Ratio") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),
          plot.margin = unit(c(t = 0.1, r = 0.0, b = 0, l = 0.2), "cm"))
  
  ratio
}
sum(is.na(x$CWRatio))
174/273
273 - 174

#### full plot

#tiff(filename = "figures/Figure4_NEW.tif", height=4, width=7, units= "in", res=800, compression= "lzw")

tiff(filename = "figures/Figure4_NEW_size.tif", height=5, width=9, units= "in", res=800, compression= "lzw")

plot_grid(scatter, WT, FlowR, age, size, ratio, TP, SRP, hlr, 
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)"), label_fontface = 1,
          ncol = 3, label_size = 8)

dev.off()








#################### fig 3 not fixed to 100%


#### WETLAND TYPE
{x$Catchment_Type <- as.factor(x$Catchment_Type)
  x$group <- ifelse(x$Catchment_Type == "WWTP", "WWT", 
                    ifelse(x$Wetland_Type == "Mesocosm", "Mesocosm", "Restored/\nConstructed"))
  
  table(x$quad, x$group)
  summary <- table(x$quad, x$group)
  m <- as.data.frame(summary)
  WT <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(values = c("#440154FF", "#44015477","#2c728eFF"  )) +
    labs(x = "  ", y = "  ", fill = "Wetland \nType") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),
          plot.margin = unit(c(t = 0.15, r = 0.0, b = 0, l = 0.3), "cm"))
  WT
  
}

#### FLOW REGIME
{levels(x$Water_regime)
  x <- x %>%                                ### reorder flow regime
    mutate(Water_regime = fct_relevel(Water_regime, "continuous, constant" , "intermittent, constant" ,
                                      "continuous, variable", "intermittent, variable", "n.s.")) 
  table(x$quad, x$Water_regime)
  summary <- table(x$quad, x$Water_regime)
  m <- as.data.frame(summary)
  
  FlowR <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("C. regulated", "I. regulated", "C.  unregulated", "I. unregulated", "Not specified" ),
                      values = c("#440154FF",   "#44015477", "#2c728eFF","#2c728e55",  "#31313122")) +
    labs(x = " ", y = " ", fill = "Hydrologic\n Regime") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8))
  FlowR
}

#### INFLOW TP CONCENTRATION
{x$bins <- cut_number(x$TP_Inflow_mg_L, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  TP <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar( stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c(" < 0.1", "0.1 - 0.2", "0.2 - 1.7", "1.7+"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF") ) +
    labs(x = "Retention bins (%)",y = "Frequency ", parse = TRUE, fill = expression(atop("Influent [TP]",
                                                                         atop(textstyle(paste("(mg" %.% "L"^-1, ")")))))) +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8))
  
  TP
}

#### INFLOW SRP CONCENTRATION
{
  x$bins <- cut_number(x$SRP_Inflow_mg_L, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  SRP <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar( stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c(" < 0.05", "0.05 - 0.1", "0.1 - 0.6", "0.6+"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF") ) +
    labs(x = "Retention bins (%)",y = "  ", parse = TRUE, fill = expression(atop(paste("Influent [PO"[4]^"3-","]"), 
                                                                         atop(textstyle(paste("(mg" %.% "L"^-1, ")")))))) + 
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8)) 
  SRP
}

### WETLAND SIZE
{x$bins <- cut_number(x$Area_m2, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  size <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar( stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("smallest", " ", " ", "largest"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF"  )) +
    labs(x = "  ", y = " ", fill = "Wetland \nSize") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8))
  size
}

### WETLAND AGE
{table(x$quad, x$Age_yr)
  x$bins <- cut_number(x$Age_yr, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  
  age <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar( stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("<2 year", "2 years", "3-4 years", "5+ years"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF"  )) +
    labs(x = " ", y = "Frequency", fill = "Wetland \nAge") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8)) 
  age
}

#### HLR
{x$bins <- cut_number(x$HLR, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  hlr <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar( stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("< 7.1", "7.1 - 14.3", "14.3 - 36.6", "36.6 +"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF"  )) +
    labs(x = "Retention bins (%) ", y = " ", fill = expression(atop("HLR",
                                                                    atop(textstyle(paste("(m" %.% "yr"^-1,")")))))) +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8)) 
  hlr
}

#### WETLAND:CATCHMENT ratio
{ x$CWRatio <- x$Catchment_area_ha/x$Area_m2*10000
  x$bins <- cut_number(x$CWRatio, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  
  ratio <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar( stat = "identity") +
    theme_classic(base_size = 9) +
    scale_fill_manual(labels = c("< 3.3", "19 - 36", "36 - 200", "200+"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF")) +
    labs(x = " ", y = " ", fill = "Catchment \nto Wetland\nArea Ratio") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8),
          plot.margin = unit(c(t = 0.0, r = 0.0, b = 0, l = 0.2), "cm"))
          
  ratio
}


#### full plot

tiff(filename = "figures/Figure4_NEW_supp.tif", height=5, width=9, units= "in", res=800, compression= "lzw")

plot_grid(scatter, WT, FlowR, age, size, ratio, TP, SRP, hlr,  
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)"), label_fontface = 1,
          ncol = 3, label_size = 8)

dev.off()








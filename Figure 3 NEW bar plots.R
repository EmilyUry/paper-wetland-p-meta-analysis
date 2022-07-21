

#' ---
#' title: "Figure 3. Bar plots by retention quartile"
#' author: "Emily Ury"
#' date: "July 21, 2022"
#' ---

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


library(ggplot2)
library(tidyverse)
library(cowplot)


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
}


### quartiles w shading

scatter <- ggplot(x, aes(x = SRP_Retention_percent, y = TP_Retention_percent)) +
  geom_rect(aes(xmin = -160, xmax = 0, ymin = -150, ymax = 100), fill = "#ffc8c2") +
  geom_rect(aes(xmin = -160, xmax = 100, ymin = -150, ymax = 0), fill = "#ffc8c2") + 
  geom_rect(aes(xmin = 0, xmax = 33, ymin = 0, ymax = 100), fill = "#ffe2c2") +
  geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 33), fill = "#ffe2c2") +
  geom_rect(aes(xmin = 33, xmax = 67, ymin = 33, ymax = 100), fill = "#fffdc2") +
  geom_rect(aes(xmin = 33, xmax = 100, ymin = 33, ymax = 67), fill = "#fffdc2") +
  geom_rect(aes(xmin = 67, xmax = 100, ymin = 67, ymax = 100), fill = "#c5ffc2") +
  geom_point() + 
  theme(legend.position = "none") +
  xlim(-170, 120) +
  ylim(-150, 105) + 
  theme_classic(base_size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_abline(slope = 1, intercept = 0) +
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  xlab("SRP % Retention") +
  ylab("TP % Retention") +
  annotate(geom = "text", x = 115, y = -16, label = "< 0", size = 5, fontface = "bold") + 
  annotate(geom = "text", x = 115, y = 16, label = "0-33", size = 5, fontface = "bold") + 
  annotate(geom = "text", x = 115, y = 50, label = "33-67", size = 5, fontface = "bold") + 
  annotate(geom = "text", x = 117, y = 84, label = "67-100", size = 5, fontface = "bold") +
  geom_segment(aes(x = -170, y = 0, xend = 120, yend = 0), lty = 2) +
  geom_segment(aes(x = 0, y = -150, xend = 0, yend = 105), lty = 2) +
  geom_segment(aes(x = 33, y = 33, xend = 33, yend = 105), lty = 2) +
  geom_segment(aes(x = 33, y = 33, xend = 120, yend = 33) , lty = 2) +
  geom_segment(aes(x = 67, y = 67, xend = 67, yend = 105), lty = 2) +
  geom_segment(aes(x = 67, y = 67, xend = 120, yend = 67) , lty = 2) 


x$quad <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "< 0", 
                 ifelse(x$TP_Retention_percent < 33 | x$SRP_Retention_percent < 33, "0-33",
                        ifelse(x$TP_Retention_percent < 67 | x$SRP_Retention_percent < 67, "33-67", "67-100")))
quad.sum <- table(x$quad)
n <- as.data.frame(quad.sum)

bar <- ggplot(n, aes(x = Var1, y = Freq, fill = as.factor(Var1))) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#ffc8c2", "#ffe2c2", "#fffdc2", "#c5ffc2")) +
  coord_flip() +
  ylab("n (site-years)") +
  xlab("Retention bins (%) ")+
  ylim(0,90) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")+
  annotate(geom = "text", y = n$Freq - 11, x = 1:4, label = paste("n = ", n$Freq, sep=""), size = 6, fontface = "bold")  
  

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
label_ypos <- c(245, 10, 245, 10)
label_text <- c("16%", " ", "25%", " ")
data <- data.frame(behavior, species, num, label_ypos, label_text)

c <- ggplot(data, aes(x = factor(species, level = c("TP", "SRP")), y = (num), fill = behavior)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle",
            color = "white", size = 5, fontface = "bold")+
  theme_classic(base_size = 16) +
  theme(legend.position = "right", legend.direction = "vertical", legend.text = element_text(size = 13), 
        legend.key.size = unit(0.5,"cm")) +
  labs(x = " ", y = "n (site-years)", fill = " " ) +
  scale_fill_manual(values = c("#414547", "#9ba4a8"), labels = c("source", "sink"))  


top <- plot_grid(c, bar, labels = c("A", "B"), ncol = 2)
plot_grid(top, scatter, labels = c(" ", " ", "C "), ncol = 1, rel_heights = c(1,2))


tiff(filename = "figures/Fig2_new.tif", height=7200, width=6000, units= "px", res=800, compression= "lzw")
plot_grid(top, scatter, labels = c(" ", " ", "C "), ncol = 1, rel_heights = c(1,2))
dev.off()









#### WETLAND TYPE
{ table(x$quad, x$Wetland_Type)
  summary <- table(x$quad, x$Wetland_Type)
  m <- as.data.frame(summary)
  
  WT <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("Constructed", "Mesocosm", "Natural", "Restored"), 
                      values = c("#440154FF", "#44015477", "#2c728e55",  "#2c728eFF"  )) +
    labs(x = "Retention bins (%) ", y = "Frequency ", fill = "Wetland \nType") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
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
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("Continuous,\n  regulated", "Intermittent,\n  regulated", "Continuous,\n  variable", "Intermittent,\n  variable", "Not \n  specified" ),
                      values = c("#440154FF",   "#44015477", "#2c728eFF","#2c728e55",  "#31313122")) +
    labs(x = "Retention bins (%)", y = " ", fill = "Hydrologic\n Regime") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
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
    theme_classic() +
    scale_fill_manual(labels = c(" < 0.1", "0.1 - 0.2", "0.2 - 1.7", "1.7+"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF") ) +
    labs(x = "Retention bins (%) ", y = " ", fill = "Inflow TP\n (mg/L)") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
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
    theme_classic() +
    scale_fill_manual(labels = c(" < 0.05", "0.05 - 0.1", "0.1 - 0.6", "0.6+"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF") ) +
    labs(x = "Retention bins (%) ", y = " ", fill = "Inflow SRP\n (mg/L)") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  SRP
}

### WETLAND SIZE
{x$bins <- cut_number(x$Area_m2, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  size <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("smallest", " ", " ", "largest"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF"  )) +
    labs(x = "Retention bins (%) ", y = "Frequency", fill = "Wetland \nSize") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold"))
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
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("<2 year", "2 years", "3-4 years", "5+ years"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF"  )) +
    labs(x = "Retention bins (%) ", y = " ", fill = "Wetland \nAge") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  age
}

#### HLR
{x$bins <- cut_number(x$HLR, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  hlr <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("< 7.1", "7.1 - 14.3", "14.3 - 36.6", "36.6 +"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF"  )) +
    labs(x = "Retention bins (%) ", y = " ", fill = "HLR") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
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
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("< 3.3", "19 - 36", "36 - 200", "200+"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF")) +
    labs(x = "Retention bins (%) ", y = " ", fill = "Catchment \nto wetland\narea ratio") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  ratio
}


#### full plot

tiff(filename = "figures/Figure3_NEW.tif", height=3600, width=9600, units= "px", res=800, compression= "lzw")

plot_grid(WT, FlowR, TP, SRP, size, age, hlr, ratio, labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 4)

dev.off()


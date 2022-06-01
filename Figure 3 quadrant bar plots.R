


#' ---
#' title: "Figure 3. Bar plots by behavior quadrats"
#' author: "Emily Ury"
#' date: "June 1, 2022"
#' ---
#' 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


library(ggplot2)
library(tidyverse)
library(cowplot)


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
}




#### WETLAND TYPE
{x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
table(x$quad, x$Wetland_Type)
x$Wetland_Type <- droplevels(x$Wetland_Type)
summary <- table(x$quad, x$Wetland_Type)
m <- as.data.frame(summary)


WT <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic(base_size = 10) +
  scale_fill_manual(labels = c("Constructed", "Mesocosm", "Natural", "Restored"), 
                    values = c("#440154FF", "#44015477", "#2c728e55",  "#2c728eFF"  )) +
  labs(x = " ", y = "Frequency ", fill = "Wetland \nType") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
  scale_x_discrete(labels = c("Q1", "Q2",
                              "Q3", "Q4"))
WT

# tiff(filename = "figures/Fig3a.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")
# WT
# dev.off()
}



#### FLOW REGIME
{levels(x$Water_regime)
  
  x <- x %>%                                ### reorder flow regime
    mutate(Water_regime = fct_relevel(Water_regime, "continuous, constant" , "intermittent, constant" ,
                                      "continuous, variable", "intermittent, variable", 
                                      "n.s.")) 
  table(x$quad, x$Water_regime)
  summary <- table(x$quad, x$Water_regime)
  m <- as.data.frame(summary)
  
  FlowR <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("Continuous,\n  constant", "Intermittent,\n  constant", "Continuous,\n  variable", "Intermittent,\n  variable", "Not \n  specified" ),
                      values = c("#440154FF",   "#44015477", "#2c728eFF","#2c728e55",  "#31313122")) +
    labs(x = " ", y = " ", fill = "Hydrologic\n Regime") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
    scale_x_discrete(labels = c("Q1", "Q2",
                                "Q3", "Q4"))+
    # scale_x_discrete(labels = c("Q1.\nTP sink\nSRP sink", "Q2.\nTP sink\n SRP source",
    #                             "Q3.\nTP source\nSRP source", "Q4.\nTP source\nSRP sink"))+
    guides(fill=guide_legend(ncol=1))
  
  #FlowR
  
  # tiff(filename = "figures/Fig3b.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")
  # FlowR
  # dev.off()
}



#### INFLOW TP CONCENTRATION


{x$bins <- cut_number(x$TP_Inflow_mg_L, 4)
table(x$quad, x$bins)
summary <- table(x$quad, x$bins)
m <- as.data.frame(summary)
TP <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic() +
  scale_fill_manual(labels = c(" < 0.11", "0.11 - 0.23", "0.23 - 1.6", "1.6+"),
                    values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF") ) +
  labs(x = " ", y = " ", fill = "Inflow TP\n (mg/L)") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
  scale_x_discrete(labels = c("Q1", "Q2",
                              "Q3", "Q4"))
#TP
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
  scale_fill_manual(labels = c(" < 0.44", "0.44 - 0.10", "0.10 - 0.53", "0.53+"),
                    values = c("#44015433", "#44015477", "#440154bb",  "#440154FF") ) +
  labs(x = " ", y = " ", fill = "Inflow TP\n (mg/L)") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
  scale_x_discrete(labels = c("Q1", "Q2",
                              "Q3", "Q4"))
#SRP
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
  labs(x = " ", y = "Frequency", fill = "Wetland \nSize") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
  scale_x_discrete(labels = c("Q1", "Q2",
                              "Q3", "Q4"))
# size
# 
# tiff(filename = "figures/Fig3d.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")
# size
# dev.off()
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
  labs(x = " ", y = " ", fill = "Wetland \nAge") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
  scale_x_discrete(labels = c("Q1", "Q2",
                              "Q3", "Q4"))
# age
# 
# tiff(filename = "figures/Fig3e.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")
# age
# dev.off()
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
  labs(x = " ", y = " ", fill = "HLR") +
  theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
  scale_x_discrete(labels = c("Q1", "Q2",
                              "Q3", "Q4"))

#hlr
}



### WETLAND:CATCHMENT ratio
{
  x$AreaRatio <- x$Area_m2/x$Catchment_area_ha/10000
  x$bins <- cut_number(x$AreaRatio, 4)
  table(x$quad, x$bins)
  summary <- table(x$quad, x$bins)
  m <- as.data.frame(summary)
  
  
  ratio <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic(base_size = 10) +
    scale_fill_manual(labels = c("< 0.004", "0.004 - 0.025", "0.025 - 0.05", "0.05+"),
                      values = c("#44015433", "#44015477", "#440154bb",  "#440154FF")) +
    labs(x = " ", y = " ", fill = "Wetland to \ncatchment\nratio") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
    scale_x_discrete(labels = c("Q1", "Q2",
                                "Q3", "Q4"))
   #ratio
   #23+18+21+14+6+10+7
  ## Note: n=99 
   
   
  # tiff(filename = "figures/Fig3d.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")
  # size
  # dev.off()
}



#### full plot


tiff(filename = "figures/Figure3_All.tif", height=3600, width=9600, units= "px", res=800, compression= "lzw")

plot_grid(WT, FlowR, TP, SRP, size, age, hlr, ratio, labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 4)

dev.off()








# ### export editable version
# library(officer)
# library(rvg)
# 
# doc <- read_pptx()
# doc <- add_slide(doc, 'Title and Content', 'Office Theme')
# dml <- dml(ggobj = FlowR)
# doc <- ph_with(doc, dml, location = ph_location_fullsize())
# print(doc, target = 'plot.pptx')
#







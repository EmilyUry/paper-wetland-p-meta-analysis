


#' ---
#' title: "Figure 3b. Bar plots by behavior quadrats for SINK (Q1) wetlands only"
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





plot(density(x$SRP_Retention_percent), xlim = c(-170,100), ylim = c(0, 0.014))
lines(density(x$TP_Retention_percent), xlim = c(-170,100))


model <- lm(TP_Retention_percent~SRP_Retention_percent, data=x)
res <- resid(model)
plot(fitted(model), res, xlim = c(-170, 100), ylim = c(-150, 105))
abline(0,0)
abline(v=1)
qqnorm(res)
qqline(res) 
plot(density(res), xlim = c(-100,100))

df <- data.frame(x$SRP_Retention_percent, res)
df.neg <- df[which(df$res < 0),]
df.pos <- df[which(df$res > 0),]
plot(density(df.pos$res), xlim = c(-150,100))
lines(density(df.neg$res))


plot(df$x.SRP_Retention_percent, df$res, xlim = c(-150,100), ylim = c(-160, 100))
abline(h=0)
abline(v=0)



### Q1 quadrats
ggplot(x, aes(x = SRP_Retention_percent, y = TP_Retention_percent)) +
  geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100), size = 2, color = "#f57971") +
  geom_segment(aes(x = 0, y = 50, xend = 100, yend = 50) , size = 2, color = "#f57971") +
  geom_point() + 
  theme(legend.position = "none") +
  xlim(-250, 105) +
  ylim(-150, 105) + 
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("SRP % Retention") +
  ylab("TP % Retention") +
  annotate(geom = "text", x = 100, y = 105, label = "Q1", size = 6, color = "#fc4f44") + 
  annotate(geom = "text", x = 0, y = 105, label = "Q2", size = 6, color = "#fc4f44") + 
  annotate(geom = "text", x = 0, y = 0, label = "Q3", size = 6, color = "#fc4f44") + 
  annotate(geom = "text", x = 100, y = 0, label = "Q4", size = 6, color = "#fc4f44") 


x <- I
x$quad <- ifelse(x$TP_Retention_percent > 50 & x$SRP_Retention_percent > 50, "I", 
                  ifelse(x$TP_Retention_percent > 50 & x$SRP_Retention_percent < 50, "II",
                         ifelse(x$TP_Retention_percent < 50 & x$SRP_Retention_percent < 50, "III", "IV")))

#### WETLAND TYPE
{ table(x$quad, x$Wetland_Type)
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
    scale_fill_manual(labels = c(" < 0.1", "0.1 - 0.3", "0.3 - 1.9", "1.9+"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF") ) +
    labs(x = " ", y = " ", fill = "Inflow TP\n (mg/L)") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
    scale_x_discrete(labels = c("Q1", "Q2",
                                "Q3", "Q4"))
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
    labs(x = " ", y = " ", fill = "Inflow SRP\n (mg/L)") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
    scale_x_discrete(labels = c("Q1", "Q2",
                                "Q3", "Q4"))
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
    labs(x = " ", y = "Frequency", fill = "Wetland \nSize") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
    scale_x_discrete(labels = c("Q1", "Q2",
                                "Q3", "Q4"))
   size
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
   age
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
  
  hlr
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
  ratio
  #23+18+21+14+6+10+7
  ## Note: n=99 
  
  
  # tiff(filename = "figures/Fig3d.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")
  # size
  # dev.off()
}



#### full plot


tiff(filename = "figures/Figure3b_sink_bars.tif", height=3600, width=9600, units= "px", res=800, compression= "lzw")

plot_grid(WT, FlowR, TP, SRP, size, age, hlr, ratio, labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 4)

dev.off()




#################################################################################################


#################################################################################################




### Q1 arcs
ggplot(x, aes(x = SRP_Retention_percent, y = TP_Retention_percent)) +
  geom_segment(aes(x = 25, y = 0, xend = 25, yend = 25), size = 2, color = "#f57971") +
  geom_segment(aes(x = 0, y = 25, xend = 25, yend = 25) , size = 2, color = "#f57971") +
  geom_segment(aes(x = 50, y = 0, xend = 50, yend = 50), size = 2, color = "#f57971") +
  geom_segment(aes(x = 0, y = 50, xend = 50, yend = 50) , size = 2, color = "#f57971") +
  geom_segment(aes(x = 75, y = 0, xend = 75, yend = 75), size = 2, color = "#f57971") +
  geom_segment(aes(x = 0, y = 75, xend = 75, yend = 75) , size = 2, color = "#f57971") +
  geom_segment(aes(x = 100, y = 0, xend = 100, yend = 100), size = 2, color = "#f57971") +
  geom_segment(aes(x = 0, y = 100, xend = 100, yend = 100) , size = 2, color = "#f57971") +
  geom_point() + 
  theme(legend.position = "none") +
  xlim(-250, 105) +
  ylim(-150, 105) + 
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("SRP % Retention") +
  ylab("TP % Retention") +
  annotate(geom = "text", x = 25, y = -5, label = "25", size = 5, color = "#fc4f44") + 
  annotate(geom = "text", x = 50, y = -5, label = "50", size = 5, color = "#fc4f44") + 
  annotate(geom = "text", x = 75, y = -5, label = "75", size = 5, color = "#fc4f44") + 
  annotate(geom = "text", x = 100, y = -5, label = "100", size = 5, color = "#fc4f44") 




x$quad <- ifelse(x$TP_Retention_percent < 25 & x$SRP_Retention_percent < 25, "0-25", 
                 ifelse(x$TP_Retention_percent < 50 & x$SRP_Retention_percent < 50, "25-50",
                        ifelse(x$TP_Retention_percent < 75 & x$SRP_Retention_percent < 75, "50-75", "75-100")))


#### WETLAND TYPE
{ table(x$quad, x$Wetland_Type)
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
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) #+
    #scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4"))
  
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
    # scale_x_discrete(labels = c("Q1", "Q2",
    #                             "Q3", "Q4"))+
    # scale_x_discrete(labels = c("Q1.\nTP sink\nSRP sink", "Q2.\nTP sink\n SRP source",
    #                             "Q3.\nTP source\nSRP source", "Q4.\nTP source\nSRP sink"))+
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
    scale_fill_manual(labels = c(" < 0.1", "0.1 - 0.3", "0.3 - 1.9", "1.9+"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF") ) +
    labs(x = " ", y = " ", fill = "Inflow TP\n (mg/L)") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
    # scale_x_discrete(labels = c("Q1", "Q2",
    #                             "Q3", "Q4"))
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
    labs(x = " ", y = " ", fill = "Inflow SRP\n (mg/L)") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
   # scale_x_discrete(labels = c("Q1", "Q2","Q3", "Q4"))
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
    labs(x = " ", y = "Frequency", fill = "Wetland \nSize") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
           axis.text.x = element_text(size = 8, family = "serif", face = "bold"))
    # scale_x_discrete(labels = c("Q1", "Q2",
    #                             "Q3", "Q4"))
  size
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
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
    #scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4"))
  age
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
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
    #scale_x_discrete(labels = c("Q1", "Q2","Q3", "Q4"))
  
  hlr
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
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
   # scale_x_discrete(labels = c("Q1", "Q2",   "Q3", "Q4"))
  ratio
  #23+18+21+14+6+10+7
  ## Note: n=99 
  
  
  # tiff(filename = "figures/Fig3d.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")
  # size
  # dev.off()
}



#### full plot


tiff(filename = "figures/Figure3c_sink_bars.tif", height=3600, width=9600, units= "px", res=800, compression= "lzw")

plot_grid(WT, FlowR, TP, SRP, size, age, hlr, ratio, labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 4)

dev.off()















#################################################################################################


#################################################################################################



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


### Q1 arcs
ggplot(x, aes(x = SRP_Retention_percent, y = TP_Retention_percent)) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = -145), size = 2, color = "#f57971") +
  geom_segment(aes(x = 0, y = 0, xend = -250, yend = 0) , size = 2, color = "#f57971") +
  geom_segment(aes(x = 33, y = 33, xend = 33, yend = -145), size = 2, color = "#f57971") +
  geom_segment(aes(x = 33, y = 33, xend = -250, yend = 33) , size = 2, color = "#f57971") +
  geom_segment(aes(x = 67, y = 67, xend = 67, yend = -145), size = 2, color = "#f57971") +
  geom_segment(aes(x = 67, y = 67, xend = -250, yend = 67) , size = 2, color = "#f57971") +
  geom_segment(aes(x = 100, y = 100, xend = 100, yend = -145), size = 2, color = "#f57971") +
  geom_segment(aes(x = 100, y = 100, xend = -250, yend = 100) , size = 2, color = "#f57971") +
  geom_point() + 
  theme(legend.position = "none") +
  xlim(-250, 105) +
  ylim(-150, 105) + 
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("SRP % Retention") +
  ylab("TP % Retention") +
  annotate(geom = "text", x = 0, y = -150, label = "0", size = 5, color = "#fc4f44") + 
  annotate(geom = "text", x = 33, y = -150, label = "33", size = 5, color = "#fc4f44") + 
  annotate(geom = "text", x = 67, y = -150, label = "67", size = 5, color = "#fc4f44") + 
  annotate(geom = "text", x = 100, y = -150, label = "100", size = 5, color = "#fc4f44") 




x$quad <- ifelse(x$TP_Retention_percent < 0 & x$SRP_Retention_percent < 0, "< 0", 
                 ifelse(x$TP_Retention_percent < 33 & x$SRP_Retention_percent < 33, "0-33",
                        ifelse(x$TP_Retention_percent < 67 & x$SRP_Retention_percent < 67, "33-67", "67-100")))


#### WETLAND TYPE
{ table(x$quad, x$Wetland_Type)
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
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) #+
  #scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4"))
  
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
    # scale_x_discrete(labels = c("Q1", "Q2",
    #                             "Q3", "Q4"))+
    # scale_x_discrete(labels = c("Q1.\nTP sink\nSRP sink", "Q2.\nTP sink\n SRP source",
    #                             "Q3.\nTP source\nSRP source", "Q4.\nTP source\nSRP sink"))+
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
    scale_fill_manual(labels = c(" < 0.1", "0.1 - 0.3", "0.3 - 1.9", "1.9+"),
                      values = c("#2c728e33", "#2c728e77", "#2c728ebb",  "#2c728eFF") ) +
    labs(x = " ", y = " ", fill = "Inflow TP\n (mg/L)") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  # scale_x_discrete(labels = c("Q1", "Q2",
  #                             "Q3", "Q4"))
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
    labs(x = " ", y = " ", fill = "Inflow SRP\n (mg/L)") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  # scale_x_discrete(labels = c("Q1", "Q2","Q3", "Q4"))
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
    labs(x = " ", y = "Frequency", fill = "Wetland \nSize") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold"))
  # scale_x_discrete(labels = c("Q1", "Q2",
  #                             "Q3", "Q4"))
  size
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
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  #scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4"))
  age
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
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  #scale_x_discrete(labels = c("Q1", "Q2","Q3", "Q4"))
  
  hlr
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
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  # scale_x_discrete(labels = c("Q1", "Q2",   "Q3", "Q4"))
  ratio
  #23+18+21+14+6+10+7
  ## Note: n=99 
  
  
  # tiff(filename = "figures/Fig3d.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")
  # size
  # dev.off()
}



#### full plot


tiff(filename = "figures/Figure3d_sink_bars.tif", height=3600, width=9600, units= "px", res=800, compression= "lzw")

plot_grid(WT, FlowR, TP, SRP, size, age, hlr, ratio, labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 4)

dev.off()


#################################################################################



### Q1 arcs
ggplot(x, aes(x = SRP_Retention_percent, y = TP_Retention_percent)) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 100), size = 2, color = "#f57971") +
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 0) , size = 2, color = "#f57971") +
  geom_segment(aes(x = 33, y = 33, xend = 33, yend = 100), size = 2, color = "#f57971") +
  geom_segment(aes(x = 33, y = 33, xend = 100, yend = 33) , size = 2, color = "#f57971") +
  geom_segment(aes(x = 67, y = 67, xend = 67, yend = 100), size = 2, color = "#f57971") +
  geom_segment(aes(x = 67, y = 67, xend = 100, yend = 67) , size = 2, color = "#f57971") +
  geom_segment(aes(x = 100, y = 100, xend = 100, yend = 100), size = 2, color = "#f57971") +
  geom_segment(aes(x = 100, y = 100, xend = 100, yend = 100) , size = 2, color = "#f57971") +
  geom_point() + 
  theme(legend.position = "none") +
  xlim(-250, 117) +
  ylim(-150, 105) + 
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("SRP % Retention") +
  ylab("TP % Retention") +
  annotate(geom = "text", x = 115, y = 5, label = "0", size = 5, color = "#fc4f44") + 
  annotate(geom = "text", x = 115, y = 38, label = "33", size = 5, color = "#fc4f44") + 
  annotate(geom = "text", x = 115, y = 72, label = "67", size = 5, color = "#fc4f44") + 
  annotate(geom = "text", x = 117, y = 105, label = "100", size = 5, color = "#fc4f44") 




x$quad <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "< 0", 
                 ifelse(x$TP_Retention_percent < 33 | x$SRP_Retention_percent < 33, "0-33",
                        ifelse(x$TP_Retention_percent < 67 | x$SRP_Retention_percent < 67, "33-67", "67-100")))


#### WETLAND TYPE
{ table(x$quad, x$Wetland_Type)
  #x$Wetland_Type <- droplevels(x$Wetland_Type)
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
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) #+
  #scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4"))
  
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
    scale_fill_manual(labels = c("Continuous,\n  regulated", "Intermittent,\n  regulated", "Continuous,\n  variable", "Intermittent,\n  variable", "Not \n  specified" ),
                      values = c("#440154FF",   "#44015477", "#2c728eFF","#2c728e55",  "#31313122")) +
    labs(x = "Retention bins (%)", y = " ", fill = "Hydrologic\n Regime") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) +
    # scale_x_discrete(labels = c("Q1", "Q2",
    #                             "Q3", "Q4"))+
    # scale_x_discrete(labels = c("Q1.\nTP sink\nSRP sink", "Q2.\nTP sink\n SRP source",
    #                             "Q3.\nTP source\nSRP source", "Q4.\nTP source\nSRP sink"))+
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
  # scale_x_discrete(labels = c("Q1", "Q2",
  #                             "Q3", "Q4"))
  size
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
    labs(x = "Retention bins (%) ", y = " ", fill = "Wetland \nAge") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  #scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4"))
  age
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
    labs(x = "Retention bins (%) ", y = " ", fill = "HLR") +
    theme(legend.position= "right", legend.direction = "vertical", legend.text = element_text(size = 8),
          legend.title = element_text(size = 10), legend.key.size = unit(0.4, 'cm'),
          axis.text.x = element_text(size = 8, family = "serif", face = "bold")) 
  #scale_x_discrete(labels = c("Q1", "Q2","Q3", "Q4"))
  
  hlr
}



### WETLAND:CATCHMENT ratio
{
  x$CWRatio <- x$Catchment_area_ha/x$Area_m2*10000
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
  # scale_x_discrete(labels = c("Q1", "Q2",   "Q3", "Q4"))
  ratio
  #23+18+21+14+6+10+7
  ## Note: n=99 
  
  
  # tiff(filename = "figures/Fig3d.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")
  # size
  # dev.off()
}



#### full plot


tiff(filename = "figures/Figure3e_sink_bars.tif", height=3600, width=9600, units= "px", res=800, compression= "lzw")

plot_grid(WT, FlowR, TP, SRP, size, age, hlr, ratio, labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 4)

dev.off()





# 
# ggplot(x, aes(x = SRP_Retention_percent, y = TP_Retention_percent)) +
#   geom_segment(aes(x = 0, y = -150, xend = 0, yend = 100), size = 2, color = "#f57971") +
#   geom_segment(aes(x = -250, y = 0, xend = 100, yend = 0) , size = 2, color = "#f57971") +
#   geom_point() + 
#   theme(legend.position = "none") +
#   xlim(-250, 105) +
#   ylim(-150, 105) + 
#   theme_bw(base_size = 16) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#   geom_abline(slope = 1, intercept = 0) +
#   geom_hline(yintercept = 0, lty = 2) +
#   geom_vline(xintercept = 0, lty = 2) +
#   xlab("SRP % Retention") +
#   ylab("TP % Retention") +    
#   annotate(geom = "text", x = 25, y = 105, label = "Q1", size = 6) + 
#   annotate(geom = "text", x = -240, y = 105, label = "Q2", size = 6) + 
#   annotate(geom = "text", x = -240, y = -147, label = "Q3", size = 6) + 
#   annotate(geom = "text", x = 25, y = -147, label = "Q4", size = 6) 
#   


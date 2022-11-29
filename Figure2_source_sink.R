


#' ---
#' title: "Figure 2. TP SPR retention bar plot and scatter plot with distributions"
#' author: "Emily Ury"
#' date: "April 27, 2022"
#' ---
#' 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


library(ggplot2)
library(tidyverse)
# library(viridis)
# library(gridExtra)
library(cowplot)
#library(ggExtra)
#library(ggpubr)

## Data set-up
x <- read.csv("Wetland_P_Clean3.csv", header = T)

x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"


{head(x)
  
  
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
  
  
  ### mass removed
  x$TP_retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
  x$SRP_retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out
}



p <- x %>%
  ggplot(aes(x = SRP_Retention_percent, y = TP_Retention_percent)) +
  geom_point(data = . %>% filter(SRP_Retention_percent > 0 & TP_Retention_percent > 0),pch = 21, fill = "#bababa55", size = 1) + 
  geom_point(data = . %>% filter(SRP_Retention_percent < 0 | TP_Retention_percent < 0),pch = 21,color = "red", fill = "#ff000025", size = 1) + 
  theme(legend.position = "none") +
  xlim(-170, 105) +
  ylim(-150, 105) + 
  theme_bw(base_size = 10) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0.1, l = 0.3, unit = "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("PO4 Retention (%)") +
  ylab("TP Retention (%)") 
p

hist <- ggplot(x, (aes(x = SRP_retention))) +
  geom_density() +
  xlim(-11.7, 5) +
  theme_classic(base_size = 10) +
  xlab(" ") +
  theme(plot.margin = margin(t = 0, r = 0.55, b = -0.4, l = 1.4, unit = "cm"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.grid = element_blank(), 
        axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, colour = "white", linetype=1)) 


hist2 <- ggplot(x, (aes(x = TP_retention))) +
  geom_density() +
  xlim(-18.9, 13) +
  theme_classic(base_size = 10) +
  xlab(" ") +
  coord_flip()  +
  theme(plot.margin = margin(t = 0, r = 0, b = 0.9, l = -0.5, unit = "cm"),
        axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), panel.grid = element_blank(), 
        axis.line.y = element_line(size = 0.5, colour = "black", linetype=1),
        axis.line.x = element_line(size = 0.5, colour = "white", linetype=1))

lab <- ggplot(x, (aes(x = TP_retention))) +
  annotate("text", 0,0, label = "Retention \n (g/m2/yr)", size = 3) +
  theme_void()

C <- plot_grid(hist, lab, p, hist2, labels = " ",rel_widths = c(4, 1), rel_heights = c(1, 4), ncol = 2)

C



##### bar plot

x$ret <- ifelse(x$TP_Retention_percent > 0, "pos", "neg")
TP.source <- table(x$ret)[1]
TP.sink <- table(x$ret)[2]
TP.source.percent <- round(TP.source/(TP.sink+TP.source)*100,1)

x$PO4ret <- ifelse(x$SRP_Retention_percent > 0, "pos", "neg")
SRP.source <- table(x$PO4ret)[1]
SRP.sink <- table(x$PO4ret)[2]
SRP.source.percent <- round(SRP.source/(SRP.sink+SRP.source)*100,1)

behavior <- c("source", "sink", "source", "sink")
species <- c("TP", "TP", "PO4", "PO4")
num <- c(TP.sink, TP.source, SRP.sink, SRP.source)
label_ypos <- c(240, 10, 230, 10)
label_text <- c("16%", " ", "25%", " ")
data <- data.frame(behavior, species, num, label_ypos, label_text)

a <- ggplot(data, aes(x = factor(species, level = c("TP", "PO4")), y = (num), fill = behavior, color = behavior)) +
  #geom_bar(stat = "identity") +
  geom_bar(stat = "identity", color = c( "black", "red", "black", "red")) +
  geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle", color = "black", size = 3)+
  theme_classic(base_size = 10) +
  theme(plot.margin = margin(t = -0.250, r = 0, b = -0.25, l = 0.7, unit = "cm"),
               legend.position = "right", legend.direction = "vertical", 
        legend.text = element_text(size = 10), legend.key.size = unit(0.3,"cm"),
        legend.key=element_rect(color = "white"))+
  labs(title = " ", x = " ", y = "n ", fill = " " ) +
  scale_fill_manual(values = c("#ff000025", "#bababa55"), labels = c("source", "sink")) +
  guides(fill = guide_legend(override.aes = list(colour = c("red", "black"))))
a

## bar plot
nrow(x[which(x$TP_retention > 0 & x$SRP_retention >0),])
nrow(x[which(x$TP_retention < 0 & x$SRP_retention <0),])
nrow(x[which(x$TP_retention < 0 & x$SRP_retention >0),])
nrow(x[which(x$TP_retention > 0 & x$SRP_retention <0),])

quad <- c("Q1","Q2","Q3","Q4")
count <- c(12, 31, 37, 192 )
df <- data.frame(quad, count)

labs <- c("TP Source, PO4 Sink", "TP Source, PO4 Source", "TP Sink, PO4 Source", "TP Sink, PO4 Sink")
b <- ggplot(df, aes(x = quad, y = count, fill = quad))+
  geom_bar(stat = "identity", color = c("red", "red", "red", "black")) +
  theme_classic(base_size = 10) +
  coord_flip() +
  geom_text(aes(label=count), hjust=c(-0.2,-0.2, -0.2, 1.2), size=3, color = "black")+
  xlab(" ") +
  ylab("n (site-years)") +
  scale_x_discrete(labels = labs) +
  scale_fill_manual(values=c("#ff000025", "#ff000025", "#ff000025", "#bababa55"))+
  theme(plot.margin = margin(t = 0.2, r = 0.5, b = 0.1, l = 0, unit = "cm"),
        legend.position = "none")
b  
12+31+37
80/273
192/273


# 
# left <- plot_grid(a, b, labels = c("A", "B"), ncol = 1, rel_heights = c(3,4), label_size = 11)
# 
# 
# tiff(filename = "figures/Source_sink.tif", height=3, width=6, units= "in", res=800, compression= "lzw")
# 
# plot_grid(left, C, labels = c(" ", "C"), ncol = 2, rel_widths = c(3,4), label_size = 11)
# 
# dev.off()






x$quad <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "< 0", 
                 ifelse(x$TP_Retention_percent < 33 | x$SRP_Retention_percent < 33, "0-33",
                        ifelse(x$TP_Retention_percent < 67 | x$SRP_Retention_percent < 67, "33-67", "67-100")))

x$source.sink <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "source", "sink")

mu <- x %>%
  group_by(source.sink) %>%
  summarise(mean = mean(ratio, na.rm = TRUE),
            median = median(ratio, na.rm = TRUE))

dist <-  ggplot(x, aes(x = ratio, color = source.sink, fill = source.sink)) +
  geom_density() +
  xlim(-1,4) +
  theme_classic(base_size = 10) +
  geom_vline(data = mu, aes(xintercept = median, color = source.sink),
              linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1),
             color="black", linetype="solid", size=0.5) +
  # geom_vline(aes(xintercept=-0.42),
  #            color="red", linetype="dashed", size=1) +
  # geom_vline(aes(xintercept=1.08),
  #            color="black", linetype="dashed", size=1) +
  scale_fill_manual(values = c("#bababa55", "#ff000025" )) +
  scale_color_manual(values = c("black", "red" )) +
  theme(plot.margin = margin(t = 0.6, r = 0.2, b = 0.1, l = 0.5, unit = "cm"),
    legend.title = element_blank(),
        legend.position = c(0.7,0.75),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm')) +
  xlab("PO4 magnification ratio")
dist  



#### updated version

top <- plot_grid(a, b, nrow = 1,  rel_widths = c(1.5,2),
                 labels = c("A", "B"), label_size = 10)
bottom <- plot_grid(C, dist, nrow = 1,  rel_widths = c(2, 1.7),
                    labels = c( "C", "D"), label_size = 10)


tiff(filename = "figures/Source_sink_v3.tif", height=4, width=5.5, units= "in", res=800, compression= "lzw")

plot_grid(top, bottom, nrow = 2, rel_heights = c(1,2), 
          labels = c( " ", " "), label_size = 10)

dev.off()







unique(x$Source)
unique(x$WetlandID)
nrow(x[which(x$Age_yr < 4),])
176/273


mean(x$TP_load_in_g_m2_yr)
range(x$TP_load_in_g_m2_yr)
median(x$TP_load_in_g_m2_yr)

mean(x$SRP_load_in_g_m2_yr)
range(x$SRP_load_in_g_m2_yr)
median(x$SRP_load_in_g_m2_yr)





##### retention by wetland type

n <- x %>%
  group_by(Wetland_Type) %>%
  summarise(n = n())
labs <- n$n

ggplot(x, aes(y = TP_Retention_percent, x = Wetland_Type, group = Wetland_Type)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-150,100)) +
  annotate("text", x = c(1,2,3,4), y = -150, label = labs, color = "gray40") +
  theme_classic()


ggplot(x, aes(y = SRP_Retention_percent, x = Wetland_Type, group = Wetland_Type)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-150,100)) +
  annotate("text", x = c(1,2,3,4), y = -150, label = labs, color = "gray40") +
  theme_classic()

median(x$TP_Retention_percent)
median(x$SRP_Retention_percent)

mean(x$TP_Retention_percent)
mean(x$SRP_Retention_percent)

xx <- x[which(x$Wetland_Type != "Mesocosm"),]

median(xx$TP_Retention_percent)
median(xx$SRP_Retention_percent)

mean(xx$TP_Retention_percent)
mean(xx$SRP_Retention_percent)

xc <- x[which(x$Catchment_Type != "WWTP"),]


median(xc$TP_Retention_percent)
median(xc$SRP_Retention_percent)

mean(xc$TP_Retention_percent)
mean(xc$SRP_Retention_percent)


n <- x %>%
  group_by(Catchment_Type) %>%
  summarise(n = n())
labs <- n$n

ggplot(x, aes(y = TP_Retention_percent, x = Catchment_Type, group = Catchment_Type)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-150,100)) +
  annotate("text", x = c(1,2,3), y = -150, label = labs, color = "gray40") +
  theme_classic()


ggplot(x, aes(y = SRP_Retention_percent, x = Catchment_Type, group = Catchment_Type)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-150,100)) +
  annotate("text", x = c(1,2,3), y = -150, label = labs, color = "gray40") +
  theme_classic()




nrow(x[which(x$TP_Retention_percent > 0 & x$SRP_Retention_percent > 0),])/273*100



#' ---
#' title: "Figure 2: Source v Sink"
#' author: "Emily Ury"
#' last update: "April 27, 2022"
#' ---
#' 
#' Source-sink behavior at the annual and monthly scales


library(ggplot2)
library(tidyverse)
library(cowplot)

## data setup:
setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop
x <- read.csv("Wetland_P_Clean2.csv", header = T)

## mass at outflow
x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)
### retention
x$TP_Retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
x$SRP_Retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out


x$ret <- ifelse(x$TP_Retention_percent > 0, "pos", "neg")
table(x$ret)
45/(45+233)
TP.source <- table(x$ret)[1]
TP.sink <- table(x$ret)[2]
TP.source.percent <- round(TP.source/(TP.sink+TP.source)*100,1)

x$PO4ret <- ifelse(x$SRP_Retention_percent > 0, "pos", "neg")
table(x$PO4ret)
72/(72+205)
SRP.source <- table(x$PO4ret)[1]
SRP.sink <- table(x$PO4ret)[2]
SRP.source.percent <- round(SRP.source/(SRP.sink+SRP.source)*100,1)

behavior <- c("source", "sink", "source", "sink")
species <- c("TP", "TP", "SRP", "SRP")
num <- c(TP.sink, TP.source, SRP.sink, SRP.source)
label_ypos <- c(250, 10, 250, 10)
label_text <- c("16%", " ", "26%", " ")
data <- data.frame(behavior, species, num, label_ypos, label_text)

c <- ggplot(data, aes(x = factor(species, level = c("TP", "SRP")), y = (num), fill = behavior)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle", color = "white", size = 4, fontface = "bold")+
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "vertical", legend.text = element_text(size = 15), legend.key.size = unit(1,"cm"))+
  labs(title = " ", x = " ", y = "n (site-years)", fill = " " ) +
  #labs(title = "(c) Wetland sink/source behavior", x = " ", y = "n" ) +
  scale_fill_manual(values = c("#414487bb", "#414487ff")) + 
  theme(plot.margin = margin(t = 1, r = 0.5, b = 2, l = 0.4, unit = "cm")) +
  theme(plot.title = element_text(hjust = 0.6, size = 7, face = "bold"))

c


# tiff(filename = "figures/Figure2.tiff", height=1800, width=1800, units= "px", res=800, compression= "lzw")
# c
# dev.off()




## MONTHLY TP
y <- read.csv("Monthly_Wetland_P_Clean.csv", header = T)

y <- y[which(y$data_type == "both" | y$data_type == "load"),]
y$Source <- droplevels(y$Source)
y$Wetland_ID <- droplevels(y$Wetland_ID)
y$TP_Retention <- y$TP_IN_g_m2_mo - y$TP_OUT_g_m2_mo
y$SRP_Retention <- y$SRP_IN_g_m2_mo - y$SRP_OUT_g_m2_mo


y <- y[which(y$Short_Ref !=  17),]       ### drop site 17 because it is too gappy
y <- y[which(y$Short_year != "YN"),]     ### drop partial years
y <- y %>%                                ### reorder months in order
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 


### TP

y$ret <- ifelse(y$TP_Retention_percent > 0, "pos", "neg")
table(y$ret)
table(y$ret, y$Month)

summary <- table(y$Month, y$ret)
m <- as.data.frame(summary)

labels <- c(round(summary[,1]/(summary[,1] + summary[,2])*100,0), rep(NA, 12))
labels <- paste(labels, "%", sep = "")
ypos <- c((summary[,1] + summary[,2] - 3), rep(NA, 12))

TP <- ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(y = ypos, label = labels), vjust = 0, hjust = "middle", color = "white", size = 4, fontface = "bold")+
  theme_classic() +
  scale_fill_manual(labels = c("Source", "Sink"), values = c("#414487bb", "#414487ff")) +
  labs(x = " ", y = "n (site-months)", fill = "TP behavior") +
  theme(plot.margin = margin(t = 1, r = 0.4, b = 0, l = 0.0, unit = "cm")) +
  theme(legend.position="none")
TP

### SRP

y$ret2 <- ifelse(y$SRP_Retention_percent > 0, "pos", "neg")
table(y$ret2)
table(y$ret2, y$Month)
summary2 <- table(y$Month, y$ret2)
w <- as.data.frame(summary2)

labels2 <- c(round(summary2[,1]/(summary2[,1] + summary2[,2])*100,0), rep(NA, 12))
labels2 <- paste(labels2, "%", sep = "")
ypos2 <- c((summary2[,1] + summary2[,2] - 3), rep(NA, 12))

SRP <- ggplot(w, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(y = ypos2, label = labels2), vjust = 0, hjust = "middle", color = "white", size = 4, fontface = "bold")+
  theme_classic() +
  scale_fill_manual(labels = c("Source", "Sink"), values = c("#414487bb", "#414487ff")) +
  labs(x = " ", y = "n (site-months)", fill = "SRP behavior") +
  theme(plot.margin = margin(t = 1, r = 0.4, b = 0, l = 0.0, unit = "cm")) +
    theme(legend.position="none")
SRP

tiff(filename = "figures/Figure2.tiff", height=4200, width=6000, units= "px", res=800, compression= "lzw")

right <- plot_grid(TP, SRP, labels = c("B (TP)", "C (SRP)"), ncol = 1)
plot_grid(c, right, labels = c("A", " "), rel_widths = c(1, 2), ncol = 2)


dev.off()






#' ---
#' title: "Wetland-P-Meta-analysis Summary Statistics"
#' author: "Emily Ury"
#' date: "April 14, 2022"
#' ---
#' 
#' Reported in Table 1 of the manuscript
#'   (a) mean, median, range: 
#'           (i) loading rate
#'           (ii) retention rate
#'           (iii) retention efficiency (%)

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop

library(tidyverse)

## Data set-up
x <- read.csv("Wetland_P_Clean3.csv", header = T)
x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"




## mass at outflow
x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)
### retention
x$TP_Retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
x$SRP_Retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out

### TP
TP_IN <- x %>%
  summarise(mean = mean(TP_load_in_g_m2_yr, na.rm = TRUE),
            sd = sd(TP_load_in_g_m2_yr, na.rm = TRUE),
            median = median(TP_load_in_g_m2_yr, na.rm = TRUE),
            max = max(TP_load_in_g_m2_yr, na.rm = TRUE),
            min = min(TP_load_in_g_m2_yr, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(TP_IN) <- "TP Loading"

TP_Retention <- x %>%
  summarise(mean = mean(TP_Retention, na.rm = TRUE),
            sd = sd(TP_Retention, na.rm = TRUE),
            median = median(TP_Retention, na.rm = TRUE),
            max = max(TP_Retention, na.rm = TRUE),
            min = min(TP_Retention, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(TP_Retention) <- "TP Retention"

TP_Retention_percent <- x %>%
  summarise(mean = mean(TP_Retention_percent, na.rm = TRUE),
            sd = sd(TP_Retention_percent, na.rm = TRUE),
            median = median(TP_Retention_percent, na.rm = TRUE),
            max = max(TP_Retention_percent, na.rm = TRUE),
            min = min(TP_Retention_percent, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(TP_Retention_percent) <- "TP Efficiency"



### SRP
SRP_IN <- x %>%
  summarise(mean = mean(SRP_load_in_g_m2_yr, na.rm = TRUE),
            sd = sd(SRP_load_in_g_m2_yr, na.rm = TRUE),
            median = median(SRP_load_in_g_m2_yr, na.rm = TRUE),
            max = max(SRP_load_in_g_m2_yr, na.rm = TRUE),
            min = min(SRP_load_in_g_m2_yr, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(SRP_IN) <- "SRP Loading"

SRP_Retention <- x %>%
  summarise(mean = mean(SRP_Retention, na.rm = TRUE),
            sd = sd(SRP_Retention, na.rm = TRUE),
            median = median(SRP_Retention, na.rm = TRUE),
            max = max(SRP_Retention, na.rm = TRUE),
            min = min(SRP_Retention, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(SRP_Retention) <- "SRP Retention"

SRP_Retention_percent <- x %>%
  summarise(mean = mean(SRP_Retention_percent, na.rm = TRUE),
            sd = sd(SRP_Retention_percent, na.rm = TRUE),
            median = median(SRP_Retention_percent, na.rm = TRUE),
            max = max(SRP_Retention_percent, na.rm = TRUE),
            min = min(SRP_Retention_percent, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(SRP_Retention_percent) <- "SRP Efficiency"




summary <- rbind(TP_IN, TP_Retention, TP_Retention_percent, SRP_IN, SRP_Retention, SRP_Retention_percent)



plot(density(x$TP_load_in_g_m2_yr))


summary





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

A <- ggplot(data, aes(x = factor(species, level = c("TP", "SRP")), y = (num), fill = behavior)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle",
            color = "white", size = 4.5, fontface = "bold")+
  theme_classic(base_size = 16) +
  theme(legend.position = "right", legend.direction = "vertical", legend.text = element_text(size = 13), 
        legend.key.size = unit(0.5,"cm")) +
  labs(x = " ", y = "n (site-years)", fill = " " ) +
  scale_fill_manual(values = c("#414547", "#9ba4a8"), labels = c("source", "sink"))  +
  scale_x_discrete(labels = c("TP", "PO4"))
A

x <- x[which(x$Wetland_Type != "Mesocosm"),]


TP.source <- table(x$ret)[1]
TP.sink <- table(x$ret)[2]
TP.source.percent <- round(TP.source/(TP.sink+TP.source)*100,1)

SRP.source <- table(x$PO4ret)[1]
SRP.sink <- table(x$PO4ret)[2]
SRP.source.percent <- round(SRP.source/(SRP.sink+SRP.source)*100,1)

behavior <- c("source", "sink", "source", "sink")
species <- c("TP", "TP", "SRP", "SRP")
num <- c(TP.sink, TP.source, SRP.sink, SRP.source)
label_ypos <- c(180, 10, 180, 10)
label_text <- c("20.2%", " ", "32.7%", " ")
data <- data.frame(behavior, species, num, label_ypos, label_text)

A <- ggplot(data, aes(x = factor(species, level = c("TP", "SRP")), y = (num), fill = behavior)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle",
            color = "white", size = 4.5, fontface = "bold")+
  theme_classic(base_size = 16) +
  theme(legend.position = "right", legend.direction = "vertical", legend.text = element_text(size = 13), 
        legend.key.size = unit(0.5,"cm")) +
  labs(x = " ", y = "n (site-years)", fill = " " ) +
  scale_fill_manual(values = c("#414547", "#9ba4a8"), labels = c("source", "sink"))  +
  scale_x_discrete(labels = c("TP", "PO4"))
A




## TP median release/retention
Source <- x[which(x$TP_Retention < 0),]
summary(Source$TP_Retention)
Sink <- x[which(x$TP_Retention > 0),]
summary(Sink$TP_Retention)

## SRP median release/retention
Source <- x[which(x$SRP_Retention < 0),]
summary(Source$SRP_Retention)
Sink <- x[which(x$SRP_Retention > 0),]
summary(Sink$SRP_Retention)


## percent of construncted wetlands that are source

constructed <- x[which(x$Wetland_Type == "Constructed"),]
nrow(constructed[which(constructed$TP_Retention < 0),])
21/136 # 15% TP sink
nrow(constructed[which(constructed$SRP_Retention < 0),])
47/136 ## 35% SRP source



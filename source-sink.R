

#' ---
#' title: "Source/sink"
#' author: "Emily Ury"
#' last update: "April 19, 2022"
#' ---
#' 
#' Source-sink behavior at the annual scale


library(ggplot2)

## data setup:
setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop
x <- read.csv("Wetland_P_Clean2.csv", header = T)
head(x)

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
label_ypos <- c(240, 10, 240, 10)
label_text <- c("source \n(16%)", "sink", "source \n(26%)", "sink")
data <- data.frame(behavior, species, num, label_ypos, label_text)

c <- ggplot(data, aes(x = factor(species, level = c("TP", "SRP")), y = (num), fill = behavior)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle", color = "white", size = 2.5, fontface = "bold")+
  theme_classic() +
  labs(title = " ", x = " ", y = "n" ) +
  #labs(title = "(c) Wetland sink/source behavior", x = " ", y = "n" ) +
  scale_fill_manual(values = c("#414487bb", "#414487ff")) + 
  theme(legend.position = "none") +
  theme(plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.0, unit = "cm")) +
  theme(plot.title = element_text(hjust = 0.6, size = 7, face = "bold"))

c


tiff(filename = "figures/Figure2.tiff", height=1800, width=1800, units= "px", res=800, compression= "lzw")
c
dev.off()








################## bin data by an input variable
x$ret <- ifelse(x$TP_Retention_percent > 0, "pos", "neg")
x$SRPret <- ifelse(x$SRP_Retention_percent > 0, "pos", "neg")

#### Inflow bins
x$cutby <- x$TP_Inflow_mg_L
xlab <- "Inflow TP concentration (mg/L)"

x$cutby <- x$SRP_Inflow_mg_L
xlab <- "Inflow SRP concentration (mg/L)"

x$cutby <- x$TP_load_in_g_m2_yr
xlab <- "Inflow TP load (g/m2/year)"


#### size
x$cutby <- x$Area_m2
xlab <- "Wetland surface area m2"

#### Age
x$cutby <- x$Age_yr
xlab <- "Wetland Age (years)"



### build the figure
### TP
n <- 4 ## number of breaks
x$bins <- (cut_number(x$cutby, n))
summary <- table(x$bins, x$ret)
m <- as.data.frame(summary)

labels <- c(round(summary[,1]/(summary[,1] + summary[,2])*100,0), rep(NA,n))
labels <- paste(labels, "%", sep = "")
ypos <- c((summary[,1] + summary[,2] - 4), rep(NA,n))

ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(y = ypos, label = labels), vjust = 0, hjust = "middle", color = "white", size = 4, fontface = "bold")+
  theme_classic() +
  scale_fill_manual(labels = c("Source", "Sink"), values = c("#414487bb", "#414487ff")) +
  labs(x = xlab, y = "n", fill = "TP Behavior")

 ### SRP 
x$bins <- (cut_number(x$cutby, n))
summary <- table(x$bins, x$SRPret)
m <- as.data.frame(summary)

labels <- c(round(summary[,1]/(summary[,1] + summary[,2])*100,0), rep(NA,n))
labels <- paste(labels, "%", sep = "")
ypos <- c((summary[,1] + summary[,2] - 4), rep(NA,n))

ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(y = ypos, label = labels), vjust = 0, hjust = "middle", color = "white", size = 4, fontface = "bold")+
  theme_classic() +
  scale_fill_manual(labels = c("Source", "Sink"), values = c("#414487bb", "#414487ff")) +
  labs(x = xlab, y = "n", fill = "SRP Behavior")







## MONTHLY TP

x <- read.csv("Monthly_Wetland_P_Clean.csv", header = T)
head(x)

x <- x[which(x$data_type == "both" | x$data_type == "load"),]
x$Source <- droplevels(x$Source)
x$Wetland_ID <- droplevels(x$Wetland_ID)

x$TP_Retention <- x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo
x$SRP_Retention <- x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo


x <- x[which(x$Short_Ref !=  17),]       ### drop site 17 because it is too gappy

x <- x[which(x$Short_year != "YN"),]     ### drop partial years
x <- x %>%                                ### reorder months in order
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 


### TP

x$ret <- ifelse(x$TP_Retention_percent > 0, "pos", "neg")
table(x$ret)
table(x$ret, x$Month)

summary <- table(x$Month, x$ret)
m <- as.data.frame(summary)

labels <- c(round(summary[,1]/(summary[,1] + summary[,2])*100,0), rep(NA, 12))
labels <- paste(labels, "%", sep = "")
ypos <- c((summary[,1] + summary[,2] - 3), rep(NA, 12))

ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(y = ypos, label = labels), vjust = 0, hjust = "middle", color = "white", size = 4, fontface = "bold")+
  theme_classic() +
  scale_fill_manual(labels = c("Source", "Sink"), values = c("#414487bb", "#414487ff")) +
  labs(x = " ", y = "n (site-months)", fill = "TP behavior")


### SRP

x$ret <- ifelse(x$SRP_Retention_percent > 0, "pos", "neg")
table(x$ret)
table(x$ret, x$Month)

summary <- table(x$Month, x$ret)
m <- as.data.frame(summary)

labels <- c(round(summary[,1]/(summary[,1] + summary[,2])*100,0), rep(NA, 12))
labels <- paste(labels, "%", sep = "")
ypos <- c((summary[,1] + summary[,2] - 3), rep(NA, 12))

ggplot(m, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(y = ypos, label = labels), vjust = 0, hjust = "middle", color = "white", size = 4, fontface = "bold")+
  theme_classic() +
  scale_fill_manual(labels = c("Source", "Sink"), values = c("#414487bb", "#414487ff")) +
  labs(x = " ", y = "n (site-months)", fill = "SRP behavior")








### extra stuff



# ### release and retention by mass
# 
# ### absolute mass of P retention (adjusted for wetlands size)
# x$TP_Yield <- x$TP_Retention*x$Area_m2
# x$SRP_Yield <- x$SRP_Retention*x$Area_m2
# 
# TPsummary <- x %>%
#   group_by(ret) %>%
#   summarise(sum = sum(TP_Yield))
# 
# SRPsummary <- x %>%
#   group_by(PO4ret) %>%
#   summarise(sum = sum(SRP_Yield))
# 
# 
# num <- c(TPsummary[[2,2]], TPsummary[[1,2]], SRPsummary[[2,2]], SRPsummary[[1,2]])
# num <- num/1000
# behavior <- c("source", "sink", "source", "sink")
# species <- c("TP", "TP", "SRP", "SRP")
# label_ypos <- c(-1500, 230000, -10000, 150000)
# label_text <- c("released", "retained", "released", "retained")
# data <- data.frame(behavior, species, num, label_ypos, label_text)
# 
# 
# d <- ggplot(data, aes(x = factor(species, level = c("TP", "SRP")), y = (num), fill = behavior)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle", color = "white", size = 2.5, fontface = "bold")+
#   theme_classic() +
#   labs(title = " ", x = " ", y = "P (kg/year)" ) +
#   scale_fill_manual(values = c("#414487bb", "#414487ff")) + 
#   theme(legend.position = "none") +
#   theme(plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.0, unit = "cm")) +
#   theme(plot.title = element_text(hjust = 0.6, size = 7, face = "bold")) 
# 
# 
# d
# 
# 
# tiff(filename = "figures/Figure2b.tiff", height=1800, width=1800, units= "px", res=800, compression= "lzw")
# d
# dev.off()








#' ---
#' title: "Wetland-P-Meta-analysis Figure 1"
#' author: "Emily Ury"
#' last update: "April 19, 2022"
#' output: github_document
#' ---
#' 
#' Figure 1. Description of the data included in the meta-analysis
#'   (a) map
#'   (b) histogram of size distribution
#'   (c) source/sink behavior
#'   (d) flow regime by wetland type
#'   (e) study publication year and catchment type


library(ggplot2)
library(tidyverse)
library(viridis)
#library(patchwork)
#library(gridExtra)
library(ggpubr)


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
#setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")

#x <- read.csv("Wetland_P_Toy_Data2.csv", header = T)
x <- read.csv("Wetland_P_Clean2.csv", header = T)
head(x)


unique(x$Source)
unique(x$WetlandID)

options(scipen = 100) ## gets out of scientific notation
pal <- viridis(6)
pal

## (a) - world map + study sites


world <- borders("world", colour="#8a8cb5", fill="#414487bb") # create a layer of borders
map <- ggplot() + world + ylim(-55,100) + theme_void() +
  geom_point(aes(x=x$Long, y = x$Lat), color = "black", pch = 21, fill = "white", size = 0.8) +
 # labs(title="(a) Location of studied wetlands ")+
  theme(plot.margin = margin(t = 0.5, r = 0.0, b = 1, l = 0.5, unit = "cm")) +
  theme(plot.title = element_text(hjust = 1, size = 7, face = "bold"))

map

## (b) - histogram of wetland size distribition

n <- x %>%
  group_by(WetlandID) %>%
  summarize(area = mean(Area_m2))

b <- ggplot(n, aes(area)) +
  geom_histogram(bins = 10, color = "#414487bb", fill = "#414487bb" ) +
  scale_x_log10(breaks=c(1, 100, 1000000)) + 
  annotation_logticks(base = 10, sides = "b", outside = TRUE, short = unit(0, "cm"), 
                      mid = unit(0.0, "cm"), long = unit(0.1, "cm"))  +
  coord_cartesian(clip = "off") +
  theme_classic() +
  labs(title = " ", x = expression(paste("Area (", m^2, ")")))+
  #labs(title = "(b) Wetland size distribution", x = expression(paste("Area (", m^2, ")")))+
  theme(plot.margin = margin(t = 0.2, r = 0.3, b = 0.3, l = 0.2, unit = "cm")) +
  theme(plot.title = element_text(hjust = 0.1, size = 7, face = "bold"))

b 



## (c) Source/Sink

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



### (d) - Inflow

table(x$Catchment_Type)
table(x$Water_regime)
table(x$Wetland_Type)

xx <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
xx$Wetland_Type <- droplevels(xx$Wetland_Type)

xx <- xx %>%
  group_by(WetlandID) %>%
  filter(Age_yr == min(Age_yr))

dd <- data.frame(table(xx$Wetland_Type, xx$Catchment_Type))
names(dd) <- c("Type", "Catchment", "count")

d <- ggplot(dd, aes(factor(Type), Catchment, fill = count)) +
  geom_tile() +
  geom_text(aes(label=count), size = 2.5, color = "white") +
  scale_fill_gradient(low="#41448777", high ="#414487ff") +
  theme_classic() +
  ylab("Catchment Type          ") +
  xlab("Wetland Type")+
  scale_y_discrete(labels = c("Ag.", "Urban", "WTP")) +
  #labs(title = "(d) Flow regime by wetland type", y = " ", x = " ") +
  theme(plot.margin = margin(t = 0, r = 0.2, b = 0.1, l = 0.2, unit = "cm"), legend.position = "none") +
  theme(plot.title = element_text(hjust = 1, size = 7, face = "bold")) +
  theme(axis.text.x = element_text(hjust = 1, size = 9, angle = 45))


d

### (e) - number of studies in each year


n <- x %>%
  filter(Catchment_Type == "ag") %>%
  group_by(Source) %>%
  summarize(num = n())

n <- n %>%
  separate(Source, into = c("Author", "Year"), sep = " ")
n[is.na(n)] <- '1994'
n$Year <- as.numeric(n$Year)

ag <- n %>%
  group_by(Year) %>%
  summarize(num = n()) %>%
  mutate(type = "Ag")

n <- x %>%
  filter(Catchment_Type == "urban") %>%
  group_by(Source) %>%
  summarize(num = n())

n <- n %>%
  separate(Source, into = c("Author", "Year"), sep = " ")
n[is.na(n)] <- '1994'
n$Year <- as.numeric(n$Year)

urb <- n %>%
  group_by(Year) %>%
  summarize(num = n()) %>%
  mutate(type = "Urban")

n <- x %>%
  filter(Catchment_Type == "WWTP") %>%
  group_by(Source) %>%
  summarize(num = n())

n <- n %>%
  separate(Source, into = c("Author", "Year"), sep = " ")
n[is.na(n)] <- '1994'
n$Year <- as.numeric(n$Year)

WTP <- n %>%
  group_by(Year) %>%
  summarize(num = n()) %>%
  mutate(type = "WTP")

e <- rbind(ag, urb, WTP) %>%
  mutate(type = fct_relevel(type, "Urban", "WTP", "Ag"))



p <- ggplot(data = e, aes(fill = type, x=Year, y = num)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = c("#8a8cb588", "#41448799", "#414487")) +
  labs(title = " ", y = "n", x = " ") +
  #labs(title = "(e) Study publication year", y = "n", x = " ") +
  theme(plot.margin = margin(t = 0, r = 0.1, b = 0, l = 0.5, unit = "cm")) +
  theme(plot.title = element_text(hjust = -0.10 , size = 7, face = "bold")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, 'cm'))
p



## plot z, number of sites vs number of years per study
z <- x %>%
  group_by(Source)  %>%
  summarize(num = n_distinct(WetlandID), yr = n_distinct(Data_Year))

zz <- ggplot(data = z, (aes(x = num, y = yr ))) +
  geom_jitter(width = 0.15, height = 0.15, color = "#414487ff", size = 2)+
  xlab("Wetlands per study") +
  ylab("Years per wetland") + 
  theme(panel.background = element_rect(fill = "#41448733", colour = "white")) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = c(0,2,4,6,8, 10,  12, 14, 16))

zz




       
       ## panel E alternative
{
# 
# n <- x %>%
#   filter(Wetland_Type == "Constructed") %>%
#   group_by(Source) %>%
#   summarize(num = n())
# n <- n %>%
#   separate(Source, into = c("Author", "Year"), sep = " ")
# n[is.na(n)] <- '1994'
# n$Year <- as.numeric(n$Year)
# Con <- n %>%
#   group_by(Year) %>%
#   summarize(num = n()) %>%
#   mutate(type = "Constructed")
# 
# n <- x %>%
#   filter(Wetland_Type == "Restored") %>%
#   group_by(Source) %>%
#   summarize(num = n())
# n <- n %>%
#   separate(Source, into = c("Author", "Year"), sep = " ")
# n[is.na(n)] <- '1994'
# n$Year <- as.numeric(n$Year)
# Res <- n %>%
#   group_by(Year) %>%
#   summarize(num = n()) %>%
#   mutate(type = "Restored")
# 
# n <- x %>%
#   filter(Wetland_Type == "Mesocosm") %>%
#   group_by(Source) %>%
#   summarize(num = n())
# n <- n %>%
#   separate(Source, into = c("Author", "Year"), sep = " ")
# n[is.na(n)] <- '1994'
# n$Year <- as.numeric(n$Year)
# Mes <- n %>%
#   group_by(Year) %>%
#   summarize(num = n()) %>%
#   mutate(type = "Mesocosm")
# 
# n <- x %>%
#   filter(Wetland_Type == "Natural") %>%
#   group_by(Source) %>%
#   summarize(num = n())
# n <- n %>%
#   separate(Source, into = c("Author", "Year"), sep = " ")
# n[is.na(n)] <- '1994'
# n$Year <- as.numeric(n$Year)
# Nat <- n %>%
#   group_by(Year) %>%
#   summarize(num = n()) %>%
#   mutate(type = "Natural")
# 
# 
# e <- rbind(Con, Res, Mes, Nat) %>%
#   mutate(type = fct_relevel(type, "Natural", "Mesocosm", "Restored", "Constructed"))
# 
# 
# p <- ggplot(data = e, aes(fill = type, x=Year, y = num)) + 
#   geom_bar(position = "stack", stat = "identity") +
#   theme_classic() +
#   scale_fill_manual(values = c("#8a8cb588", "#41448799", "#414487", "#0a0c40")) +
#   labs(title = "(e) Study publication year", y = "n", x = " ") +
#   theme(plot.margin = margin(t = 0, r = 1, b = 0, l = 1, unit = "cm")) +
#   theme(plot.title = element_text(hjust = -0.10 , size = 7, face = "bold")) +
#   theme(legend.title = element_blank(), legend.text = element_text(size = 7),
#         legend.key.size = unit(0.3, 'cm'))
# p
}



hist(x$Age_yr, xlim = c(0,20), breaks = 100)
nrow(x[which(x$Age_yr <= 5),])
nrow(x[which(x$Age_yr <= 3),])
nrow(x)

22+26+42+58+62

179/277

210/277

tiff(filename = "figures/Figure1.tiff", height=3600, width=3600, units= "px", res=800, compression= "lzw")

#ggarrange(map, b, d, ncol = 3, nrow = 1, labels = c("A", "B", "C")) 
ggarrange(map, zz, d, b, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))                        

dev.off()






ggarrange(ggarrange(map, b, d, ncol = 3, labels = c("A", "B", "C")),   # First row with 3 plots
          p, nrow = 2, labels = c(" ", "D")) 

# grid.arrange(map, b, c, p, nrow = 2, layout_matrix = rbind(c(1,2,3), c(4,4,4)), 
#              name = c("A", "B", "C", "D"),)
# 
# library(ggpubr)
ggarrange(map, b, c, p, ncol = 2, nrow = 2, #layout_matrix = rbind(c(1,2,3), c(4,4,4)), 
        labels = c("A", "B", "C", "D"))
#grid.arrange(map, b, c, d, p, nrow = 3, layout_matrix = rbind(c(1,2), c(3,4), c(5,5)))

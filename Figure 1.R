

#' ---
#' title: "Wetland-P-Meta-analysis"
#' author: "Emily Ury"
#' date: "Feb 15, 2021"
#' output: github_document
#' ---
#' 
#' Figure 1. Description of the data included in the meta-analysis
#'   (a) map
#'   (b) histogram of size distribution
#'   (c) inflow source/flow regime
#'   (d) source sink behavior


library(ggplot2)
library(tidyverse)
library(viridis)
library(gridExtra)


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
x <- read.csv("Wetland_P_Toy_Data2.csv", header = T)
head(x)


options(scipen = 100) ## gets out of scientific notation
pal <- viridis(6)
pal

## (a) - world map + study sites


world <- borders("world", colour="#414487bb", fill="#414487bb") # create a layer of borders
map <- ggplot() + world + ylim(-55,85) + theme_void() +
  geom_point(aes(x=x$Long, y = x$Lat), color = "black", size = 1) +
  labs(title="(a) Location of studied wetlands ")+
  theme(plot.margin = margin(t = 0, r = 1, b = 0.5, l = 1, unit = "cm")) +
  theme(plot.title = element_text(hjust = 1, size = 10, face = "bold"))

map

## (b) - histogram of wetland size distribition

b <- ggplot(x, aes(Area_m2)) +
  geom_histogram(bins = 10, color = "#414487bb", fill = "#414487bb" ) +
  scale_x_log10() +
  theme_classic() +
  labs(title = "(b) Wetland size distribution", x = expression(paste("Area (", m^2, ")")))+
  theme(plot.margin = margin(t = 0.2, r = 1, b = 0.3, l = 1.5, unit = "cm")) +
  theme(plot.title = element_text(hjust = 1.2, size = 10, face = "bold"))

b 



## (c) Source/Sink

x$ret <- ifelse(x$TP_Retention_percent > 0, "pos", "neg")
table(x$ret)
32/(32+144)
TP.source <- table(x$ret)[1]
TP.sink <- table(x$ret)[2]
TP.source.percent <- round(TP.source/(TP.sink+TP.source)*100,1)

x$PO4ret <- ifelse(x$SRP_Retention_percent > 0, "pos", "neg")
table(x$PO4ret)
49/(49+126)
SRP.source <- table(x$PO4ret)[1]
SRP.sink <- table(x$PO4ret)[2]
SRP.source.percent <- round(SRP.source/(SRP.sink+SRP.source)*100,1)

behavior <- c("source", "sink", "source", "sink")
species <- c("TP", "TP", "SRP", "SRP")
num <- c(32, 144, 49, 126)
label_ypos <- c(125, 10, 125, 10)
label_text <- c("source \n(18%)", "sink", "source \n(28%)", "sink")
data <- data.frame(behavior, species, num, label_ypos, label_text)

c <- ggplot(data, aes(x = factor(species, level = c("TP", "SRP")), y = (percent), fill = behavior)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle", color = "white", size = 3.5)+
  theme_classic() +
  labs(title = "(c) Wetland sink/source behavior", x = " ", y = "n" ) +
  scale_fill_manual(values = c("#414487bb", "#414487ff")) + 
  theme(legend.position = "none") +
  theme(plot.margin = margin(t = 0, r = 2, b = 0, l = 1, unit = "cm")) +
  theme(plot.title = element_text(hjust = 0.6, size = 10, face = "bold"))
  
c



### (d) - Inflow

table(x$Catchment_Type)
table(x$Water_regime)

dd <- data.frame(table(x$Catchment_Type, x$Water_regime))
names(dd) <- c("Source", "Flow", "count")

d <- ggplot(dd, aes(Source, factor(Flow, level = c("n.s.", "continuous, constant", "continuous, variable", 
                                              "intermittent, constant", "intermittent, variable")), fill = count)) +
  geom_tile() +
  geom_text(aes(label=count), size = 3.5, color = "white") +
  scale_fill_gradient(low="#41448777", high ="#414487ff") +
  theme_classic()+
  labs(title = "(d) Wetland inflow charactersistics", y = " ", x = " ") +
  theme(plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.5, unit = "cm"), legend.position = "none") +
  theme(plot.title = element_text(hjust = 1, size = 10, face = "bold"))




### (e) - number of studies in each year
n <- x %>%
  group_by(Source) %>%
  summarize(num = n())

n <- n %>%
  separate(Source, into = c("Author", "Year"), sep = " ")
n[is.na(n)] <- '1994'
n$Year <- as.numeric(n$Year)

n <- n %>%
  group_by(Year) %>%
  summarize(num = n())

p <- ggplot(data =n, aes(x=Year, y = num)) + 
  geom_bar(stat = "identity", fill = "#414487bb") +
  theme_classic() +
  labs(title = "(e) Study publication year", y = "n", x = " ") +
  theme(plot.margin = margin(t = 0, r = 1, b = 0, l = 1, unit = "cm")) +
  theme(plot.title = element_text(hjust = -0.10 , size = 10, face = "bold"))
p


grid.arrange(map, b, c, d, p, nrow = 3, layout_matrix = rbind(c(1,2), c(3,4), c(5,5)))





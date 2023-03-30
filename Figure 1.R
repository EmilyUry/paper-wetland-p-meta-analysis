

#' ---
#' title: "Wetland-P-Meta-analysis - Figure 1"
#' author: "Emily Ury"
#' last update: "March 30, 2023"
#' output: github_document
#' ---
#' 
#' Figure 1. Description of the data included in the meta-analysis
#'   (a) Map
#'   (b) Study n and duration
#'   (c) Wetland type by catchment type
#'   (d) Wetland size distribution


library(ggplot2)
library(tidyverse)
library(ggpubr)
library(cowplot)


setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")

x <- read.csv("Wetland_P_Clean3.csv", header = T)

options(scipen = 100) ## gets out of scientific notation


## (a) - world map + study sites


world <- borders("world", colour="#B9D0D9", fill="#2c728eff") # create a layer of borders
map <- ggplot() + world + ylim(-55,100) + theme_void(base_size = 14) +
  geom_point(aes(x=x$Long, y = x$Lat), color = "black", pch = 21, fill = "white", size = 1) +
 # labs(title="(a) Location of studied wetlands ")+
  theme(plot.margin = margin(t = 0.5, r = 0.1, b = 1.25, l = 0, unit = "cm")) +
  theme(plot.title = element_text(hjust = 1, size = 7, face = "bold"))

map




## (b) number of sites vs number of years per study
z <- x %>%
  group_by(Source)  %>%
  summarize(num = n_distinct(WetlandID), yr = n_distinct(Data_Year))

B <- ggplot(data = z, (aes(x = num, y = yr ))) +
  geom_jitter(width = 0.15, height = 0.15, shape = 21, fill = "#2c728ebb", size = 2)+
  xlab("Wetlands per Study") +
  ylab("Years Studied") + 
  theme_gray(base_size = 14)+
  theme(panel.background = element_rect(fill = "#2c728e22", colour = "white")) +
  theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0.0, l = 0.5, unit = "cm"), legend.position = "none") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = c(0,2,4,6,8, 10,  12, 14, 16))

B



### (c) - Wetland type


xx <- x %>%
  group_by(WetlandID) %>%
  filter(Age_yr == min(Age_yr))


dd <- data.frame(table(xx$Wetland_Type, xx$Catchment_Type))
names(dd) <- c("Type", "Catchment", "count")
dd


c <- ggplot(dd, aes(factor(Type), Catchment, fill = count)) +
  geom_tile() +
  geom_text(aes(label=count), size = 4, color = "black") +
  scale_fill_gradient(low="white", high ="#2c728e") +
  theme_classic(base_size = 14) +
  ylab("Catchment Type          ") +
  xlab("Wetland Type")+
  scale_y_discrete(labels = c("Ag.", "Urban", "WWT")) +
  #labs(title = "(d) Flow regime by wetland type", y = " ", x = " ") +
  theme(plot.margin = margin(t = 0.2, r = 0.4, b = 0.2, l = 0.5, unit = "cm"), legend.position = "none") +
  theme(plot.title = element_text(hjust = 1, size = 8, face = "bold")) +
  theme(axis.text.x = element_text(hjust = 1, size = 9, angle = 45))

c

### (d) - histogram of wetland size distribution

n <- x %>%
  group_by(WetlandID) %>%
  summarize(area = mean(Area_m2))

D <- ggplot(n, aes(area)) +
  geom_histogram(bins = 10, color = "#bee0ec", fill = "#2c728eff" ) +
  scale_x_log10(breaks=c(1, 100, 1000000)) + 
  annotation_logticks(base = 10, sides = "b", outside = TRUE, short = unit(0, "cm"), 
                      mid = unit(0.0, "cm"), long = unit(0.1, "cm"))  +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 14) +
  labs(title = " ", x = expression(paste("Wetland Area (", m^2, ")")))+
  #labs(title = "(b) Wetland size distribution", x = expression(paste("Wetland area (", m^2, ")")))+
  theme(plot.margin = margin(t = 0.2, r = 0.2, b = 0.1, l = 0.4, unit = "cm")) +
  theme(plot.title = element_text(hjust = 0.1, size = 7, face = "bold"))

D 


### #Paper version Figure 1

tiff(filename = "figures/Figure1.tif", height=5, width=6.5, units= "in", res=800, compression= "lzw")

plot_grid(map, B, c, D, labels = c("A", "B", "C", "D"), label_size = 12, rel_widths = c(1, 1), ncol = 2)

dev.off()



#### Metrics used in text


## Results - First paragraph

# how many wetlands are in ag catchments
nrow(xx[which(xx$Catchment_Type == "ag"),])/139*100   
# 76%

# how many wetlands are treating wastewater 
nrow(xx[which(xx$Catchment_Type == "WWTP"),])/139*100
# 18 %

# how many wetlands are categorized as constructed
nrow(xx[which(xx$Wetland_Type == "Constructed"),])
# n = 64

# how many wetlands are categorized as constructed
nrow(xx[which(xx$Wetland_Type == "Mesocosm"),])
# n = 39

# how many wetlands are categorized as constructed
nrow(xx[which(xx$Wetland_Type == "Restored"),])
# n = 33 

# how many wetlands are categorized as constructed
nrow(xx[which(xx$Wetland_Type == "Natural"),])
# n = 3

#### how many site-years were monitored when the wetland was 3 years old or younger
nrow(x[which(x$Age_yr <= 3),])
# 178
nrow(x[which(x$Age_yr <= 3),])/273*100
# 65%

# how much data comes from sites 5 years or older
nrow(x[which(x$Age_yr > 5),])/273*100
# 17%





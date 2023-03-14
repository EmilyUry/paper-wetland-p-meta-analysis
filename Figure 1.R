

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
library(cowplot)


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")

#x <- read.csv("Wetland_P_Toy_Data2.csv", header = T)
x <- read.csv("Wetland_P_Clean3.csv", header = T)
x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"

head(x)


unique(x$Source)
unique(x$WetlandID)

options(scipen = 100) ## gets out of scientific notation
pal <- viridis(6)
pal

## (a) - world map + study sites


world <- borders("world", colour="#B9D0D9", fill="#2c728eff") # create a layer of borders
map <- ggplot() + world + ylim(-55,100) + theme_void(base_size = 14) +
  geom_point(aes(x=x$Long, y = x$Lat), color = "black", pch = 21, fill = "white", size = 1) +
 # labs(title="(a) Location of studied wetlands ")+
  theme(plot.margin = margin(t = 0.5, r = 0.1, b = 1.25, l = 0, unit = "cm")) +
  theme(plot.title = element_text(hjust = 1, size = 7, face = "bold"))

map

## (b) - histogram of wetland size distribition

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





### (c) - Inflow

table(x$Catchment_Type)
table(x$Water_regime)
table(x$Wetland_Type)


xx <- x %>%
  group_by(WetlandID) %>%
  filter(Age_yr == min(Age_yr))

## missing 1 constructed Ag
## missing 1 constructed WWTP

dd <- data.frame(table(xx$Wetland_Type, xx$Catchment_Type))
names(dd) <- c("Type", "Catchment", "count")
dd

dd[1,3] <- 37
dd[9,3] <- 25
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
25/139
9/139
37+36+1+31
105/139

## plot B, number of sites vs number of years per study
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





hist(x$Age_yr, xlim = c(0,20), breaks = 100)
nrow(x[which(x$Age_yr <= 5),])
nrow(x[which(x$Age_yr <= 3),])
nrow(x)

22+26+42+58+62

176/273
224/273

210/277



### #Paper version Figure 1

tiff(filename = "figures/Figure1.tif", height=5, width=6.5, units= "in", res=800, compression= "lzw")

plot_grid(map, B, c, D, labels = c("A", "B", "C", "D"), label_size = 12, rel_widths = c(1, 1), ncol = 2)

dev.off()









#### old stuff
{


#ggarrange(map, b, d, ncol = 3, nrow = 1, labels = c("A", "B", "C")) 
#ggarrange(map, B, c, D, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D")) 

plot_grid(map, B, c, D, labels = c("A", "B", "C", "D"), label_size = 16, rel_widths = c(1.5, 1), ncol = 2)



ggarrange(ggarrange(map, b, d, ncol = 3, labels = c("A", "B", "C")),   # First row with 3 plots
          p, nrow = 2, labels = c(" ", "D")) 

# grid.arrange(map, b, c, p, nrow = 2, layout_matrix = rbind(c(1,2,3), c(4,4,4)), 
#              name = c("A", "B", "C", "D"),)
# 
# library(ggpubr)
ggarrange(map, b, c, p, ncol = 2, nrow = 2, #layout_matrix = rbind(c(1,2,3), c(4,4,4)), 
        labels = c("A", "B", "C", "D"))
#grid.arrange(map, b, c, d, p, nrow = 3, layout_matrix = rbind(c(1,2), c(3,4), c(5,5)))


}
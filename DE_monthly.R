

#' ---
#' title: "Data Exploration - Montly Wetland P Removal"
#' author: "Emily Ury"
#' last update: "April 5, 2022"
#' output: R_script
#' ---
#' 
#' Look at seasonal patters of P retention in wetlands
#' from literature meta-analysis
#' 



library(ggplot2)
library(tidyverse)
library(viridis)
library(gridExtra)



setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
x <- read.csv("Monthly_Wetland_P_Clean.csv", header = T)
head(x)


table(x$Source)
unique(x$Source)


table(x$Source, x$data_type)

table(x$data_type)

x <- x %>%
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                           "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 


x$TP.rem <- (x$TP_IN_mg_L - x$TP_OUT_mg_L)
x$TP.rem.conc <- (x$TP_IN_mg_L - x$TP_OUT_mg_L)/x$TP_IN_mg_L*100
x$SRP.rem <- (x$SRP_IN_mg_L - x$SRP_OUT_mg_L)
x$SRP.rem.conc <- (x$SRP_IN_mg_L - x$SRP_OUT_mg_L)/x$SRP_IN_mg_L*100



ggplot(x, aes(x=Month, y = TP.rem.conc )) +
  geom_boxplot() +
  ylim(-100,100)

ggplot(x, aes(x=Month, y = TP.rem )) +
  geom_boxplot() +
  ylim(-1, 15)

ggplot(x, aes(x=Month, y = SRP.rem.conc )) +
  geom_boxplot() +
  ylim(-100,100)

ggplot(x, aes(x=Month, y = SRP.rem )) +
  geom_boxplot() +
  ylim(-1, 15)



source <- x[which(x$TP.rem < 0),]
ggplot(source, aes(x=Month, y = TP.rem.conc )) +
  geom_boxplot() 

ggplot(source, aes(x=Month, y = TP.rem )) +
  geom_boxplot() 

tiff(filename = "figures/monthly_conc_red.tiff", height=3600, width=3600, units= "px", res=800, compression= "lzw")

p

dev.off()














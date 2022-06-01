

#' ---
#' title: "Heatmap of monthly wetland P retention"
#' author: "Emily Ury"
#' last update: "April 13, 2022"
#' ---
#' 
#' Figure 1. Description of the data included in the meta-analysis

library(ggplot2)
library(forcats) ## need for fct_relevel
library(dplyr) # easier data wrangling 
library(viridis)
library(gridExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 


## data setup

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


x <- read.csv("Monthly_Wetland_P_Clean.csv", header = T)
head(x)

x <- x[which(x$data_type == "both" | x$data_type == "load"),]
x$Source <- droplevels(x$Source)
x$Wetland_ID <- droplevels(x$Wetland_ID)

table(x$Source)
unique(x$Source)

x$TP_Retention <- x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo
x$SRP_Retention <- x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo



## subset data

df <-x %>% select(Short_Ref, Short_ID,  Short_year, Month, TP_IN_g_m2_mo, TP_Retention, TP_Retention_percent, 
                  SRP_IN_g_m2_mo, SRP_Retention, SRP_Retention_percent, TP_IN_mg_L, SRP_IN_mg_L)
df$Short_ID <- droplevels(df$Short_ID)
df$Unique_ID <- paste(x$Short_Ref, x$Short_ID, x$Short_year, sep = "_")
site_year <-unique(df$Unique_ID)




df <- df[which(df$Short_Ref !=  17),]       ### drop site 17 because it is too gappy
df <- df[which(df$Short_year != "YN"),]     ### drop partial years
df <- na.omit(df)                           ### drop months with NAs
df <- df %>%                                ### reorder months in order
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 

rbPal <- colorRampPalette(c('red', 'blue'))
rbPal(12)
plot(log(df$TP_IN_mg_L), df$TP_Retention_percent, pch = 16, col = rbPal(12)[df$Month] )
















#' ---
#' title: "Heatmap of monthly wetland P retention"
#' author: "Emily Ury"
#' last update: "April 13, 2022"
#' ---
#' 
#' Figure 1. Description of the data included in the meta-analysis

library(ggplot2)
library(cowplot)
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
                  SRP_IN_g_m2_mo, SRP_Retention, SRP_Retention_percent)
df$Short_ID <- droplevels(df$Short_ID)
df$Unique_ID <- paste(x$Short_Ref, x$Short_ID, x$Short_year, sep = "_")
site_year <-unique(df$Unique_ID)




df <- df[which(df$Short_Ref !=  17),]       ### drop site 17 because it is too gappy
df <- df[which(df$Short_year != "YN"),]     ### drop partial years
df <- na.omit(df)                           ### drop months with NAs
df <- df %>%                                ### reorder months in order
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 

df.sum <- df %>%
  group_by(Unique_ID) %>%
  summarise(
    an_TP_rem = sum(TP_Retention, na.rm = TRUE),
    an_TP_IN = sum(TP_IN_g_m2_mo, na.rm = TRUE),
    an_SRP_rem = sum(SRP_Retention, na.rm = TRUE),
    an_SRP_IN = sum(SRP_IN_g_m2_mo, na.rm = TRUE))

df.sum$an_TP_rem_percent <- df.sum$an_TP_rem/df.sum$an_TP_IN*100
df.sum$an_SRP_rem_percent <- df.sum$an_SRP_rem/df.sum$an_SRP_IN*100


########### Order the plots by total annual retention

# df.sum$Unique_ID <- fct_reorder(df.sum$Unique_ID , df.sum$an_TP_rem)  ### reorder factor by total TP retention
# Unique <- df.sum$Unique_ID
# order <- levels(Unique)
# df <- df %>%
#   mutate(Unique_ID = fct_relevel(Unique_ID, order))




######## Basic Version  #####################

## set class breaks by TP removal
breaks <- c("<-0.1", "-0.1 - -0.05", "-0.05 - 0", "0", "0 - 0.05", "0.05 - 0.1", "0.1 - 1", "1+")
df$col <- breaks[as.numeric(cut(df$TP_Retention, breaks = c(-Inf, -0.1, -0.05, -0.0000001, 0.000001, 0.05, 0.1, 1,  Inf)))]
df <- df %>%
  mutate(col = fct_relevel(col,"1+","0.1 - 1", "0.05 - 0.1", "0 - 0.05", "0", "-0.05 - 0", "-0.1 - -0.05", "<-0.1"   )) 



TP <- ggplot(df,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "TP Retention \n (g/m2/month)",
                    values = c("#053061", "#2166ac", "#67a9cf" , "#d1e5f0",
                               "#bababa" , "#f2b9b1","#f76752", "#b2182b" )) +
  theme_minimal(base_size = 18) +
  ylab("Site ID") +
  xlab(" ") +
  theme(legend.position = "none") +
  ggtitle("TP")
TP




df$col <- breaks[as.numeric(cut(df$SRP_Retention, breaks = c(-Inf, -0.1, -0.05, -0.0000001, 0.000001, 0.05, 0.1, 1,  Inf)))]
df <- df %>%
  mutate(col = fct_relevel(col,"1+","0.1 - 1", "0.05 - 0.1", "0 - 0.05", "0", "-0.05 - 0", "-0.1 - -0.05", "<-0.1"   )) 


SRP <- ggplot(df,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "Retention \n (g/m2/month)",
                    values = c("#053061", "#2166ac", "#67a9cf" , "#d1e5f0",
                               "#bababa" , "#f2b9b1","#f76752", "#b2182b" )) +
  theme_minimal(base_size = 18) +
  ylab(" ") +
  xlab(" ") + 
  ggtitle("SRP") +
  theme(axis.text.y = element_blank())

tiff(filename = "figures/Heatmaps.tif", height=6000, width=11000, units= "px", res=800, compression= "lzw")
plot_grid(TP, SRP, labels = c('A', 'B'), rel_widths = c(7,8), ncol = 2)

dev.off()










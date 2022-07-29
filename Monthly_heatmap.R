

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

table(x$Source)
unique(x$Source)

x$TP_Retention <- x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo
x$SRP_Retention <- x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo



## subset data

df <-x %>% select(Short_Ref, Short_ID,  Short_year, Month, TP_IN_g_m2_mo, TP_Retention, TP_Retention_percent, 
                  SRP_IN_g_m2_mo, SRP_Retention, SRP_Retention_percent, TP_OUT_g_m2_mo, SRP_OUT_g_m2_mo)
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



######## Basic Version  #####################

## set class breaks by TP removal
breaks <- c("<-0.1", "-0.1 - -0.05", "-0.05 - 0", "0", "0 - 0.05", "0.05 - 0.1", "0.1 - 1", "1+")
df$col <- breaks[as.numeric(cut(df$TP_Retention, breaks = c(-Inf, -0.1, -0.05, -0.0000001, 0.000001, 0.05, 0.1, 1,  Inf)))]
df <- df %>%
  mutate(col = fct_relevel(col,"1+","0.1 - 1", "0.05 - 0.1", "0 - 0.05", "0", "-0.05 - 0", "-0.1 - -0.05", "<-0.1"   )) 

labs <- (c("Z2","Z1","Y2", "Y1", "X", "W", 
           "V", "U",
           "T2","T1","S2", "S1","R2", "R1","Q2", "Q1", 
           "P2", "P1", "O2", "O1", "N2", "N1", "M2", "M1", 
          "L", "K", "J","I", "H","G", "F", "E", 
          "D", "C", "B", "A"))
locs <- c("Denmark", "Denmark", "ON, Canada", "KY, USA", "Slovenia", "Denmark")
los.pos <-c("10", "8", "5", "3","2","1")

TP <- ggplot(df,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "TP Retention \n (g/m2/month)",
                    values = c("#053061", "#2166ac", "#67a9cf" , "#d1e5f0",
                               "#bababa" , "#f2b9b1","#f76752", "#b2182b" )) +
  theme_minimal(base_size = 18) +
  theme(plot.margin = margin(1, .3, 1, 3, "cm"),
        axis.text.y = element_text(hjust = 0)) +
  ylab(" ") +
  xlab(" ") +
  scale_y_discrete(labels = labs) +
  scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme(legend.position = "none")
TP


### set breaks for SRP

df$col2 <- breaks[as.numeric(cut(df$SRP_Retention, breaks = c(-Inf, -0.1, -0.05, -0.0000001, 0.000001, 0.05, 0.1, 1,  Inf)))]
df <- df %>%
  mutate(col2 = fct_relevel(col2,"1+","0.1 - 1", "0.05 - 0.1", "0 - 0.05", "0", "-0.05 - 0", "-0.1 - -0.05", "<-0.1"   )) 

SRP <- ggplot(df,aes(x = Month, y = Unique_ID, fill=col2))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "Retention \n (g/m2/month)",
                    values = c("#053061", "#2166ac", "#67a9cf" , "#d1e5f0",
                               "#bababa" , "#f2b9b1","#f76752", "#b2182b" )) +
  theme_minimal(base_size = 18) +
  ylab(" ") +
  xlab(" ") + 
  scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  theme(axis.text.y = element_blank(),
        plot.margin = margin(1, 1, 1, 0.3, "cm"))
SRP


tiff(filename = "figures/Heatmaps.tif", height=7000, width=7000, units= "px", res=800, compression= "lzw")
plot_grid(TP, SRP, labels = c('TP', 'SRP'), label_size = 16, rel_widths = c(6.5,8), ncol = 2)
dev.off()







df$SRP.TP.ratio <- (df$SRP_OUT_g_m2_mo/df$TP_OUT_g_m2_mo)/(df$SRP_IN_g_m2_mo/df$TP_IN_g_m2_mo)


##df$SRP.TP.ratio <- df$SRP_IN_g_m2_mo/df$TP_IN_g_m2_mo - (df$SRP_IN_g_m2_mo - df$SRP_Retention)/(df$TP_IN_g_m2_mo-df$TP_Retention)
breaks <- c("0-0.5", "0.5-1", "1-1.5", "1.5+" )

df$col3 <- breaks[as.numeric(cut(df$SRP.TP.ratio, breaks = c(0, 0.5, 1, 1.5, Inf)))]
df <- df %>%
  mutate(col3 = fct_relevel(col3, "0-0.5", "0.5-1", "1-1.5", "1.5+"  ))

ratio <- ggplot(df,aes(x = Month, y = Unique_ID, fill=col3))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "SRP:TP ratio",
                    values = c("#2166ac", "#67a9cf" , "#f2b9b1" ,"#f76752")) +
  theme_minimal(base_size = 18) +
  ylab(" ") +
  xlab(" ") + 
    theme(axis.text.y = element_blank(),
          plot.margin = margin(1, 1, 1, 0.3, "cm"))+
  scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 
  #scale_y_discrete(labels = labs)

ratio 

tiff(filename = "figures/Heatmaps2.tif", height=7000, width=10000, units= "px", res=800, compression= "lzw")
plot_grid(TP, SRP, ratio, labels = c('TP', 'SRP', 'SRP:TP'), label_size = 16, rel_widths = c(6.5,8.1,8), ncol = 3)
dev.off()


summary(df$SRP.TP.ratio)

hist(df$SRP.TP.ratio)



########### Order the plots by total annual retention

# df.sum$Unique_ID <- fct_reorder(df.sum$Unique_ID , df.sum$an_TP_rem)  ### reorder factor by total TP retention
# Unique <- df.sum$Unique_ID
# order <- levels(Unique)
# df <- df %>%
#   mutate(Unique_ID = fct_relevel(Unique_ID, order))










####### Plot Version 2
########### add column for annual total

df.sum$col <- breaks[as.numeric(cut(df.sum$an_TP_rem, breaks = c(-Inf, -0.1, -0.05, -0.0000001, 0.000001, 0.05, 0.1, 1,  Inf)))]
df.sum2 <- df.sum %>%
  mutate(col = fct_relevel(col,"1+","0.1 - 1", "0.05 - 0.1", "0 - 0.05", "0", "-0.05 - 0", "-0.1 - -0.05", "<-0.1"   )) %>%
  separate(col = "Unique_ID", into = c("Short_Ref", "Short_ID", "Short_year"), sep = "_", remove = "FALSE") %>%
  rename(TP_IN_g_m2_mo = an_TP_IN )%>%
  rename(TP_Retention = an_TP_rem )%>%
  rename(TP_Retention_percent = an_TP_rem_percent )%>%
  rename(SRP_IN_g_m2_mo = an_SRP_IN )%>%
  rename(SRP_Retention = an_SRP_rem )%>%
  rename(SRP_Retention_percent = an_SRP_rem_percent )

df.sum2$Month <- "Total"
#names(df)
#names(df.sum2)

df2 <- rbind(df,  df.sum2)


df2 <- df2 %>%
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total")) %>%
  mutate(Unique_ID = fct_relevel(Unique_ID, order)) 

tiff(filename = "figures/HeatmapTP.tiff", height=6000, width=6000, units= "px", res=800, compression= "lzw")

ggplot(df2,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "TP Retention \n (g/m2/month)", 
                    values = c("#053061", "#2166ac", "#67a9cf" , "#d1e5f0",
                               "#bababa" , "#f2b9b1","#f76752", "#b2182b" )) + 
  theme_minimal(base_size = 8) +
  ylab("Site ID") +
  xlab(" ") +
  geom_text(data=df.sum2, aes(x = 14, y=Unique_ID,label=round(TP_Retention,2)), size = 3, fontface = "bold") +
  coord_cartesian(clip = "off") +
  geom_rect(mapping=aes(xmin=12.5, xmax=14.6, ymin=0.5, ymax=36.5), color = "black", fill = NA, size = 1)


dev.off()





###### TP Percent




df.sum$Unique_ID <- fct_reorder(df.sum$Unique_ID , df.sum$an_TP_rem_percent)  ### reorder factor by total TP retention
Unique <- df.sum$Unique_ID
order <- levels(Unique)
df <- df %>%
  mutate(Unique_ID = fct_relevel(Unique_ID, order)) 

######## Basic Version  #####################

## set class breaks by TP removal
breaks <- c("<-100", "-100 - -67", "-67 - -33", "-33 - 0", "0 - 33", "33 - 67", "67 - 100")
df$col <- breaks[as.numeric(cut(df$TP_Retention_percent, breaks = c(-Inf, -100, -67, -33, 0, 33, 67,  Inf)))]
df <- df %>%
  mutate(col = fct_relevel(col,"67 - 100","33 - 67", "0 - 33", "0", "-33 - 0", "-67 - -33", "-100 - -67", "<-100"   )) 

# ggplot(df,aes(x = Month, y = Unique_ID, fill=col))+
#   geom_tile(color= "white",size=0.1) +
#   scale_fill_manual(name = "TP Retention \n (%)", 
#                     values = c("#053061", "#67a9cf" , "#d1e5f0",
#                                "#f2b9b1","#f76752", "#b2182b", "#800000" )) + 
#   theme_minimal(base_size = 8) +
#   ylab("Site ID") +
#   xlab(" ")



####### Plot Version 2
########### add column for annual total

df.sum$col <- breaks[as.numeric(cut(df.sum$an_TP_rem_percent, breaks = c(-Inf, -100, -67, -33, 0, 33, 67,  Inf)))]
df.sum2 <- df.sum %>%
  mutate(col = fct_relevel(col,"67 - 100","33 - 67", "0 - 33", "-33 - 0", "-67 - -33", "-100 - -67", "<-100")) %>%
  separate(col = "Unique_ID", into = c("Short_Ref", "Short_ID", "Short_year"), sep = "_", remove = "FALSE") %>%
  rename(TP_IN_g_m2_mo = an_TP_IN )%>%
  rename(TP_Retention = an_TP_rem )%>%
  rename(TP_Retention_percent = an_TP_rem_percent )%>%
  rename(SRP_IN_g_m2_mo = an_SRP_IN )%>%
  rename(SRP_Retention = an_SRP_rem )%>%
  rename(SRP_Retention_percent = an_SRP_rem_percent )

df.sum2$Month <- "Total"
names(df)
names(df.sum2)

df2 <- rbind(df,  df.sum2)


df2 <- df2 %>%
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total")) %>%
  mutate(Unique_ID = fct_relevel(Unique_ID, order)) 

ggplot(df2,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "TP Retention \n (%)", 
                    values = c("#053061", "#67a9cf" , "#d1e5f0",
                               "#f2b9b1","#f76752", "#b2182b", "#800000" )) + 
  theme_minimal(base_size = 8) +
  ylab("Site ID") +
  xlab(" ") +
  geom_text(data=df.sum2, aes(x = 14.1, y=Unique_ID,label=round(TP_Retention_percent,1)), size = 3, fontface = "bold") +
  coord_cartesian(clip = "off") +
  geom_rect(mapping=aes(xmin=12.5, xmax=14.7, ymin=0.5, ymax=36.5), color = "black", fill = NA, size = 1)









##### For SRP
df.sum$Unique_ID <- fct_reorder(df.sum$Unique_ID , df.sum$an_SRP_rem)  ### reorder factor by total SRP retention
Unique <- df.sum$Unique_ID
order <- levels(Unique)
df <- df %>%
  mutate(Unique_ID = fct_relevel(Unique_ID, order)) 

######## Basic Version  #####################

## set class breaks by SRP removal
breaks <- c("<-0.1", "-0.1 - -0.05", "-0.05 - 0", "0", "0 - 0.05", "0.05 - 0.1", "0.1 - 1", "1+")
df$col <- breaks[as.numeric(cut(df$SRP_Retention, breaks = c(-Inf, -0.1, -0.05, -0.0000001, 0.000001, 0.05, 0.1, 1,  Inf)))]
df <- df %>%
  mutate(col = fct_relevel(col,"1+","0.1 - 1", "0.05 - 0.1", "0 - 0.05", "0", "-0.05 - 0", "-0.1 - -0.05", "<-0.1"   )) 


# ggplot(df,aes(x = Month, y = Unique_ID, fill=col))+
#   geom_tile(color= "white",size=0.1) +
#   scale_fill_manual(name = "SRP Retention \n (g/m2/month)", 
#                     values = c("#053061", "#2166ac", "#67a9cf" , "#d1e5f0",
#                                "#bababa" , "#f2b9b1","#f76752", "#b2182b" )) + 
#   theme_minimal(base_size = 8) +
#   ylab("Site ID") +
#   xlab(" ")



####### Plot Version 2
########### add column for annual total

df.sum$col <- breaks[as.numeric(cut(df.sum$an_SRP_rem, breaks = c(-Inf, -0.1, -0.05, -0.0000001, 0.000001, 0.05, 0.1, 1,  Inf)))]
df.sum2 <- df.sum %>%
  mutate(col = fct_relevel(col,"1+","0.1 - 1", "0.05 - 0.1", "0 - 0.05", "0", "-0.05 - 0", "-0.1 - -0.05", "<-0.1"   )) %>%
  separate(col = "Unique_ID", into = c("Short_Ref", "Short_ID", "Short_year"), sep = "_", remove = "FALSE") %>%
  rename(TP_IN_g_m2_mo = an_TP_IN )%>%
  rename(TP_Retention = an_TP_rem )%>%
  rename(TP_Retention_percent = an_TP_rem_percent )%>%
  rename(SRP_IN_g_m2_mo = an_SRP_IN )%>%
  rename(SRP_Retention = an_SRP_rem )%>%
  rename(SRP_Retention_percent = an_SRP_rem_percent )

df.sum2$Month <- "Total"
names(df)
names(df.sum2)

df2 <- rbind(df,  df.sum2)


df2 <- df2 %>%
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total")) %>%
  mutate(Unique_ID = fct_relevel(Unique_ID, order)) 

ggplot(df2,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "SRP Retention \n (g/m2/month)", 
                    values = c("#053061", "#2166ac", "#67a9cf" , "#d1e5f0",
                               "#bababa" , "#f2b9b1","#f76752", "#b2182b" )) + 
  theme_minimal(base_size = 8) +
  ylab("Site ID") +
  xlab(" ") +
  geom_text(data=df.sum2, aes(x = 14, y=Unique_ID,label=round(SRP_Retention,2)), size = 3, fontface = "bold") +
  coord_cartesian(clip = "off") +
  geom_rect(mapping=aes(xmin=12.5, xmax=14.6, ymin=0.5, ymax=36.5), color = "black", fill = NA, size = 1)






###SRP monthly (%)

df.sum$Unique_ID <- fct_reorder(df.sum$Unique_ID , df.sum$an_SRP_rem_percent)  ### reorder factor by total TP retention
Unique <- df.sum$Unique_ID
order <- levels(Unique)
df <- df %>%
  mutate(Unique_ID = fct_relevel(Unique_ID, order)) 

######## Basic Version  #####################

## set class breaks by TP removal
breaks <- c("<-100", "-100 - -67", "-67 - -33", "-33 - 0", "0 - 33", "33 - 67", "67 - 100")
df$col <- breaks[as.numeric(cut(df$SRP_Retention_percent, breaks = c(-Inf, -100, -67, -33, 0, 33, 67,  Inf)))]
df <- df %>%
  mutate(col = fct_relevel(col,"67 - 100","33 - 67", "0 - 33", "0", "-33 - 0", "-67 - -33", "-100 - -67", "<-100"   )) 

# ggplot(df,aes(x = Month, y = Unique_ID, fill=col))+
#   geom_tile(color= "white",size=0.1) +
#   scale_fill_manual(name = "SRP Retention \n (%)", 
#                     values = c("#053061", "#67a9cf" , "#d1e5f0",
#                                "#f2b9b1","#f76752", "#b2182b", "#800000" )) + 
#   theme_minimal(base_size = 8) +
#   ylab("Site ID") +
#   xlab(" ")



####### Plot Version 2
########### add column for annual total

df.sum$col <- breaks[as.numeric(cut(df.sum$an_SRP_rem_percent, breaks = c(-Inf, -100, -67, -33, 0, 33, 67,  Inf)))]
df.sum2 <- df.sum %>%
  mutate(col = fct_relevel(col,"67 - 100","33 - 67", "0 - 33", "-33 - 0", "-67 - -33", "-100 - -67", "<-100")) %>%
  separate(col = "Unique_ID", into = c("Short_Ref", "Short_ID", "Short_year"), sep = "_", remove = "FALSE") %>%
  rename(TP_IN_g_m2_mo = an_TP_IN )%>%
  rename(TP_Retention = an_TP_rem )%>%
  rename(TP_Retention_percent = an_TP_rem_percent )%>%
  rename(SRP_IN_g_m2_mo = an_SRP_IN )%>%
  rename(SRP_Retention = an_SRP_rem )%>%
  rename(SRP_Retention_percent = an_SRP_rem_percent )

df.sum2$Month <- "Total"
names(df)
names(df.sum2)

df2 <- rbind(df,  df.sum2)


df2 <- df2 %>%
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total")) %>%
  mutate(Unique_ID = fct_relevel(Unique_ID, order)) 

ggplot(df2,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "SRP Retention \n (%)", 
                    values = c("#053061", "#67a9cf" , "#d1e5f0",
                               "#f2b9b1","#f76752", "#b2182b", "#800000" )) + 
  theme_minimal(base_size = 8) +
  ylab("Site ID") +
  xlab(" ") +
  geom_text(data=df.sum2, aes(x = 14.1, y=Unique_ID,label=round(SRP_Retention_percent,1)), size = 3, fontface = "bold") +
  coord_cartesian(clip = "off") +
  geom_rect(mapping=aes(xmin=12.5, xmax=14.7, ymin=0.5, ymax=36.5), color = "black", fill = NA, size = 1)










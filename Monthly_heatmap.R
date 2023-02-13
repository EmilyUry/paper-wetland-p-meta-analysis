

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


x$TP_retention <- (x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo)/x$TP_IN_g_m2_mo*100
x$SRP_retention <- (x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo)/x$SRP_IN_g_m2_mo*100


## subset data

df <-x %>% select(Short_Ref, Short_ID,  Short_year, Month, TP_IN_g_m2_mo, TP_Retention, TP_Retention_percent, 
                  SRP_IN_g_m2_mo, SRP_Retention, SRP_Retention_percent, TP_OUT_g_m2_mo, SRP_OUT_g_m2_mo,
                  TP_retention, SRP_retention)
df$Unique_ID <- paste(x$Short_Ref, x$Short_ID, x$Short_year, sep = "_")
site_year <-unique(df$Unique_ID)




df <- df[which(df$Short_Ref !=  17),]       ### drop site 17 because it is too gappy
df <- df[which(df$Short_year != "YN"),]     ### drop partial years
df <- na.omit(df)                           ### drop months with NAs
df <- df %>%                                ### reorder months in order
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 


######## Supplement: Retention g/m2  #####################

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
  theme(plot.margin = margin(1, .3, 1, 0, "cm"),
        axis.text.y = element_text(hjust = 0)) +
  ylab(" ") +
  xlab(" ") +
  scale_y_discrete(labels = labs) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
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
  theme(axis.text.y = element_blank(),
        plot.margin = margin(1, 1, 1, 0.3, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
SRP


tiff(filename = "figures/Heatmaps_retention.tif", height=7000, width=9000, units= "px", res=800, compression= "lzw")
plot_grid(NULL, TP, SRP, labels = c(' ', 'TP', 'SRP'), label_size = 16, rel_widths = c(3,5.7,9), ncol = 3)
dev.off()





#### ratio plot



df$SRP.TP.ratio <- (df$SRP_OUT_g_m2_mo/df$TP_OUT_g_m2_mo)/(df$SRP_IN_g_m2_mo/df$TP_IN_g_m2_mo)
summary(df$SRP.TP.ratio)

##df$SRP.TP.ratio <- df$SRP_IN_g_m2_mo/df$TP_IN_g_m2_mo - (df$SRP_IN_g_m2_mo - df$SRP_Retention)/(df$TP_IN_g_m2_mo-df$TP_Retention)
breaks <- c("0-0.5", "0.5-1", "1-1.5", "1.5+" )

df$col5 <- breaks[as.numeric(cut(df$SRP.TP.ratio, breaks = c(0, 0.5, 1, 1.5, Inf)))]
df <- df %>%
  mutate(col5 = fct_relevel(col5, "0-0.5", "0.5-1", "1-1.5", "1.5+"  ))

ratio <- ggplot(df,aes(x = Month, y = Unique_ID, fill=col5))+
  geom_tile(color= "black",size=0.1) +
  scale_fill_manual(name = "Ratio",
                    values = c("#018571", "#80cdc1" , "#c2a5cf" ,"#7b3294")) +
  theme_classic(base_size = 18) +
  ylab(" ") +
  xlab(" ") + 
  ggtitle("Ratio")+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "none",
          plot.margin = margin(0.3, 0, 0.5, 0, "cm"),)

ratio 




####### Percent Retention
########### add column for annual total

df.sum <- df %>%
  group_by(Unique_ID) %>%
  summarise(
    an_TP_rem = sum(TP_Retention, na.rm = TRUE),
    an_TP_IN = sum(TP_IN_g_m2_mo, na.rm = TRUE),
    an_SRP_rem = sum(SRP_Retention, na.rm = TRUE),
    an_SRP_IN = sum(SRP_IN_g_m2_mo, na.rm = TRUE))

df.sum$an_TP_rem_percent <- df.sum$an_TP_rem/df.sum$an_TP_IN*100
df.sum$an_SRP_rem_percent <- df.sum$an_SRP_rem/df.sum$an_SRP_IN*100

df.sum2 <- df.sum %>%
  #mutate(col3 = fct_relevel(col3,"67 - 100","33 - 67", "0 - 33", "-33 - 0", "-67 - -33", "-100 - -67", "<-100")) %>%
  rename(TP_Retention = an_TP_rem )%>%
  rename(TP_Retention_percent = an_TP_rem_percent )%>%
  rename(SRP_Retention = an_SRP_rem )%>%
  rename(SRP_Retention_percent = an_SRP_rem_percent ) %>%
  select(-c("an_TP_IN", "an_SRP_IN"))

df.sum2$Month <- "Total"

df.select <- df %>%
  select("Unique_ID", "TP_Retention", "SRP_Retention", "TP_Retention_percent", "SRP_Retention_percent", "Month")

df2 <- rbind(df.select,  df.sum2)


breaks <- c("<-100", "-100 - -67", "-67 - -33", "-33 - 0", "0 - 33", "33 - 67", "67 - 100")
df2$col <- breaks[as.numeric(cut(df2$TP_Retention_percent, breaks = c(-Inf, -100, -67, -33, 0, 33, 67,  Inf)))]
df2 <- df2 %>%
  mutate(col = fct_relevel(col,"67 - 100","33 - 67", "0 - 33", "0", "-33 - 0", "-67 - -33", "-100 - -67", "<-100"   )) 

labs2 <- as.character(round(df.sum2$TP_Retention_percent,1))

TPp <- ggplot(df2,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "TP Retention \n (%)", 
                    values = c("#053061", "#67a9cf" , "#d1e5f0",
                               "#f2b9b1","#f76752", "#b2182b", "#800000" )) +
  coord_cartesian(clip = "off") +
  ylab(" ") +
  xlab(" ") +
  ggtitle("TP")+
    scale_y_discrete(labels = labs) +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  geom_rect(mapping=aes(xmin=12.5, xmax=16.5, ymin=0.5, ymax=36.5), color = "black", fill = NA, size = 1) +
  annotate("text", x = 15, y = 1:36, label = labs2, size = 4, fontface = "bold")





##### For SRP
df2$col2 <- breaks[as.numeric(cut(df2$SRP_Retention_percent, breaks = c(-Inf, -100, -67, -33, 0, 33, 67,  Inf)))]
df2 <- df2 %>%
  mutate(col2 = fct_relevel(col2,"67 - 100","33 - 67", "0 - 33", "0", "-33 - 0", "-67 - -33", "-100 - -67", "<-100"   )) 

labs3 <- as.character(round(df.sum2$SRP_Retention_percent,1))

SRPp <- ggplot(df2,aes(x = Month, y = Unique_ID, fill=col2))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "PO4 Retention \n (%)", 
                    values = c("#053061", "#67a9cf" , "#d1e5f0",
                               "#f2b9b1","#f76752", "#b2182b", "#800000" )) +
  coord_cartesian(clip = "off") +
  ylab(" ") +
  xlab(" ") +  
  ggtitle("SRP")+
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  geom_rect(mapping=aes(xmin=12.5, xmax=16.5, ymin=0.5, ymax=36.5), color = "black", fill = NA, size = 1) +
  annotate("text", x = 15, y = 1:36, label = labs3, size = 4, fontface = "bold")



tiff(filename = "figures/Heatmaps_percent.tif", height=7000, width=10000, units= "px", res=800, compression= "lzw")
plot_grid(NULL, TPp, SRPp, NULL, ratio, labels = c(' ', 'A', 'B', ' ', 'C'), label_size = 16,
          rel_widths = c(3, 8.3,8,4,6.5), ncol = 5)
dev.off()


##




####################
#########################
###############
##########
##
####
################
###################
###############





### same plot, but if SRP out = zero, set monthly to NA


####### Percent Retention
########### add column for annual total

df$TP_Retention_percent_new <- ifelse(df$TP_IN_g_m2_mo == 0 & df$TP_OUT_g_m2_mo == 0, NA, df$TP_Retention_percent)
df$SRP_Retention_percent_new <- ifelse(df$SRP_IN_g_m2_mo == 0 & df$SRP_OUT_g_m2_mo == 0, NA, df$SRP_Retention_percent)


df.sum <- df %>%
  group_by(Unique_ID) %>%
  summarise(
    an_TP_rem = sum(TP_Retention, na.rm = TRUE),
    an_TP_IN = sum(TP_IN_g_m2_mo, na.rm = TRUE),
    an_SRP_rem = sum(SRP_Retention, na.rm = TRUE),
    an_SRP_IN = sum(SRP_IN_g_m2_mo, na.rm = TRUE))

df.sum$an_TP_rem_percent <- df.sum$an_TP_rem/df.sum$an_TP_IN*100
df.sum$an_SRP_rem_percent <- df.sum$an_SRP_rem/df.sum$an_SRP_IN*100

df.sum2 <- df.sum %>%
  #mutate(col3 = fct_relevel(col3,"67 - 100","33 - 67", "0 - 33", "-33 - 0", "-67 - -33", "-100 - -67", "<-100")) %>%
  rename(TP_Retention = an_TP_rem )%>%
  rename(TP_Retention_percent_new = an_TP_rem_percent )%>%
  rename(SRP_Retention = an_SRP_rem )%>%
  rename(SRP_Retention_percent_new = an_SRP_rem_percent ) %>%
  select(-c("an_TP_IN", "an_SRP_IN"))

df.sum2$Month <- "Total"

df.select <- df %>%
  select("Unique_ID", "TP_Retention", "SRP_Retention", "TP_Retention_percent_new", "SRP_Retention_percent_new", "Month")

df2 <- rbind(df.select,  df.sum2)


breaks <- c("<-100", "-100 - -67", "-67 - -33", "-33 - 0", "0 - 33", "33 - 67", "67 - 100")
df2$col <- breaks[as.numeric(cut(df2$TP_Retention_percent_new, breaks = c(-Inf, -100, -67, -33, 0, 33, 67,  Inf)))]
df2 <- df2 %>%
  mutate(col = fct_relevel(col,"67 - 100","33 - 67", "0 - 33", "0", "-33 - 0", "-67 - -33", "-100 - -67", "<-100"   )) 

labs2 <- as.character(round(df.sum2$TP_Retention_percent_new,0))

TPp <- ggplot(df2,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "black",size=0.1) +
  scale_fill_manual(name = "TP Retention \n (%)", 
                    values = c("#053061", "#67a9cf" , "#d1e5f0",
                               "#f2b9b1","#f76752", "#b2182b", "#800000" )) +
  coord_cartesian(clip = "off") +
  ylab(" ") +
  xlab(" ") +
  ggtitle("TP")+
  scale_y_discrete(labels = labs) +
  theme_classic(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 13),
        legend.position = "none") +
  geom_rect(mapping=aes(xmin=12.5, xmax=17.2, ymin=0.5, ymax=36.5), color = "black", fill = NA, size = 1) +
  annotate("text", x = 15.3, y = 1:36, label = labs2, size = 5)





##### For SRP
df2$col2 <- breaks[as.numeric(cut(df2$SRP_Retention_percent_new, breaks = c(-Inf, -100, -67, -33, 0, 33, 67,  Inf)))]
df2 <- df2 %>%
  mutate(col2 = fct_relevel(col2,"67 - 100","33 - 67", "0 - 33", "0", "-33 - 0", "-67 - -33", "-100 - -67", "<-100"   )) 

labs3 <- as.character(round(df.sum2$SRP_Retention_percent_new,0))

SRPp <- ggplot(df2,aes(x = Month, y = Unique_ID, fill=col2))+
  geom_tile(color= "black",size=0.1) +
  scale_fill_manual(name = "PO4 Retention \n (%)", 
                    values = c("#053061", "#67a9cf" , "#d1e5f0",
                               "#f2b9b1","#f76752", "#b2182b", "#800000" )) +
  coord_cartesian(clip = "off") +
  ylab(" ") +
  xlab(" ") +  
  ggtitle("SRP")+
  theme_classic(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  geom_rect(mapping=aes(xmin=12.5, xmax=17.2, ymin=0.5, ymax=36.5), color = "black", fill = NA, size = 1) +
  annotate("text", x = 15.3, y = 1:36, label = labs3, size = 5)




###################
#######      This is the paper version

###


tiff(filename = "figures/Heatmaps_percent_new.tif", height=7000, width=10000, units= "px", res=800, compression= "lzw")
plot_grid(NULL, TPp, SRPp, NULL, ratio, labels = c(' ', ' ', ' ', ' ', ' '), label_size = 16,
          rel_widths = c(3, 8.3,8,4,5.7), ncol = 5)
dev.off()






####### summary stats

###

x <- df

summary(x$TP_IN_g_m2_mo)
std.error(x$TP_IN_g_m2_mo)
summary(x$TP_Retention)
std.error(x$TP_Retention)
summary(x$TP_retention) ## this is retention percent
std.error(x$TP_retention) ## ## this is retention percent

summary(x$SRP_IN_g_m2_mo)
std.error(x$SRP_IN_g_m2_mo)
summary(x$SRP_Retention)
std.error(x$SRP_Retention)
summary(x$SRP_Retention_percent)
std.error(x$SRP_Retention_percent)


TP.source <- x[which(x$TP_Retention < 0),]
TP.sink <- x[which(x$TP_Retention >= 0),]

SRP.source <- x[which(x$SRP_Retention < 0),]
SRP.sink <- x[which(x$SRP_Retention >= 0),]

summary(TP.sink$TP_IN_g_m2_mo)
std.error(TP.sink$TP_IN_g_m2_mo)
summary(TP.sink$TP_Retention)
std.error(TP.sink$TP_Retention)
summary(TP.sink$TP_retention) ## this is retention percent
std.error(TP.sink$TP_retention) ## this is retention percent

summary(TP.source$TP_IN_g_m2_mo)
std.error(TP.source$TP_IN_g_m2_mo)
summary(TP.source$TP_Retention)
std.error(TP.source$TP_Retention)
summary(TP.source$TP_retention) ## this is retention percent
std.error(TP.source$TP_retention) ## this is retention percent


summary(SRP.sink$SRP_IN_g_m2_mo)
std.error(SRP.sink$SRP_IN_g_m2_mo)
summary(SRP.sink$SRP_Retention)
std.error(SRP.sink$SRP_Retention)
summary(SRP.sink$SRP_retention) ## this is retention percent
std.error(SRP.sink$SRP_retention) ## this is retention percent


summary(SRP.source$SRP_IN_g_m2_mo)
std.error(SRP.source$SRP_IN_g_m2_mo)
summary(SRP.source$SRP_Retention)
std.error(SRP.source$SRP_Retention)

SRP.source1 <- SRP.source[!is.infinite(SRP.source$SRP_retention),]
summary(SRP.source1$SRP_retention) ## this is retention percent
std.error(SRP.source1$SRP_retention) ## this is retention percent







##### extra stuff




# ### quick check
# 
# df <-x %>% select(Short_Ref, Short_ID,  Short_year, Month, TP_IN_g_m2_mo, TP_Retention, TP_Retention_percent, 
#                   SRP_IN_g_m2_mo, SRP_Retention, SRP_Retention_percent, TP_OUT_g_m2_mo, SRP_OUT_g_m2_mo, Monthly_Inflow_m3_month, Monthly_Outflow_m3_month)
# df$Unique_ID <- paste(x$Short_Ref, x$Short_ID, x$Short_year, sep = "_")
# site_year <-unique(df$Unique_ID)
# 
# 
# 
# 
# df <- df[which(df$Short_Ref !=  17),]       ### drop site 17 because it is too gappy
# df <- df[which(df$Short_year != "YN"),]     ### drop partial years
# df <- na.omit(df)                           ### drop months with NAs
# df <- df %>%                                ### reorder months in order
#   mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
#                              "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 
# 
# 
# 
# plot(log(df$Monthly_Inflow_m3_month), df$TP_Retention)
# 
# plot(log(df$Monthly_Inflow_m3_month), df$SRP_Retention)
# 
# plot(log(df$Monthly_Inflow_m3_month), df$TP_Retention_percent, 
#      ylim = c(-400, 100))
# abline(h=0)
# 
# plot(log(df$Monthly_Inflow_m3_month), df$SRP_Retention_percent, 
#      ylim = c(-400, 100))
# abline(h=0)




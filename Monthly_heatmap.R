

## subannual - load data

library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis)
library(gridExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
x <- read.csv("Monthly_Wetland_P_Clean.csv", header = T)
head(x)

x <- x[which(x$data_type == "both" | x$data_type == "load"),]
x$Source <- droplevels(x$Source)
x$Wetland_ID <- droplevels(x$Wetland_ID)

table(x$Source)
unique(x$Source)



x$TP_Retention <- x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo

x$SRP_Retention <- x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo



### heat map

df <-x %>% select(Short_Ref, Short_ID,  Short_year, Month, TP_IN_g_m2_mo, TP_Retention, TP_Retention_percent)
df$Short_ID <- droplevels(df$Short_ID)

df$Unique_ID <- paste(x$Short_Ref, x$Short_ID, x$Short_year, sep = "_")

site_year <-unique(df$Unique_ID)
site_year
table(df$Unique_ID, df$Month)


######## Plotting starts here#####################

df <- df[which(df$Short_Ref !=  17),]
df <- na.omit(df) 
df <- df %>%
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 

df.sum <- df %>%
  group_by(Unique_ID) %>%
  summarise(
    an_TP_rem = sum(TP_Retention, na.rm = TRUE),
    an_TP_IN = sum(TP_IN_g_m2_mo, na.rm = TRUE))

df.sum$an_TP_rem_percent <- df.sum$an_TP_rem/df.sum$an_TP_IN*100

df.sum$Unique_ID <- fct_reorder(df.sum$Unique_ID , df.sum$an_TP_rem)
Unique <- df.sum$Unique_ID
order <- levels(Unique)

df <- df %>%
  mutate(Unique_ID = fct_relevel(Unique_ID, order)) 


rbPal <- colorRampPalette(c('red','blue'))
pal <- rbPal(10)
pal
breaks <- c("<-0.5", "-0.5 - 0", "0-0.5", "0.5-1", "1+")
df$col <- breaks[as.numeric(cut(df$TP_Retention, breaks = c(-Inf, -0.5, 0, 0.5, 1,  Inf)))]
df <- df %>%
  mutate(col = fct_relevel(col,"1+","0.5-1", "0-0.5","-0.5 - 0", "<-0.5"   )) 

pal6 <- c("#FF0000","#AA0055" , "#5500AA", "#3800C6", "#1C00E2", "#0000FF", "#0000FF")



ggplot(df,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(values = c("#0000FF","#1C00E2", "#3800C6", "#C60038" ,"#FF0000")) + 
  theme_minimal(base_size = 8)

ggplot(df,aes(x = Month, y = Unique_ID, fill=TP_Retention))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_stepsn(n.breaks = 7, colours = pal6) + 
  theme_minimal(base_size = 8)


  scale_fill_viridis(name="Hrly Temps C",option ="C")
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Hourly Temps - Station",statno), x="Day", y="Hour Commencing")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness






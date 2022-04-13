

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
df <- df[which(df$Short_year != "YN"),]
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
rbPal(10)

breaks <- c("<-0.1", "-0.1 - -0.05", "-0.05 - 0", "0", "0 - 0.05", "0.05 - 0.1", "0.1 - 1", "1+")
df$col <- breaks[as.numeric(cut(df$TP_Retention, breaks = c(-Inf, -0.1, -0.05, -0.0000001, 0.000001, 0.05, 0.1, 1,  Inf)))]
df <- df %>%
  mutate(col = fct_relevel(col,"1+","0.1 - 1", "0.05 - 0.1", "0 - 0.05", "0", "-0.05 - 0", "-0.1 - -0.05", "<-0.1"   )) 

#pal6 <- c("#FF0000","#AA0055" , "#5500AA", "#3800C6", "#1C00E2", "#0000FF", "#0000FF")



ggplot(df,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "TP Retention \n (mg/m2/month)", values = c("#053061", "#2166ac", "#67a9cf" , "#d1e5f0","#bababa" , "#f2b9b1","#f76752", "#b2182b" )) + 
  theme_minimal(base_size = 8) +
  ylab("Site ID") +
  xlab(" ")




### add annual total

head(df)
head(df.sum)

df.sum$col <- breaks[as.numeric(cut(df.sum$an_TP_rem, breaks = c(-Inf, -0.1, -0.05, -0.0000001, 0.000001, 0.05, 0.1, 1,  Inf)))]
df.sum <- df.sum %>%
  mutate(col = fct_relevel(col,"1+","0.1 - 1", "0.05 - 0.1", "0 - 0.05", "0", "-0.05 - 0", "-0.1 - -0.05", "<-0.1"   )) 

df.sum$Month <- "Total"

df.sum <- df.sum %>%
  separate(col = "Unique_ID", into = c("Short_Ref", "Short_ID", "Short_year"), sep = "_", remove = "FALSE")

df.sum <- df.sum %>%
  rename(TP_IN_g_m2_mo = an_TP_IN )%>%
  rename(TP_Retention = an_TP_rem )%>%
  rename(TP_Retention_percent = an_TP_rem_percent )

names(df)
names(df.sum)

df2 <- rbind(df,  df.sum)


df2 <- df2 %>%
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total")) %>%
  mutate(Unique_ID = fct_relevel(Unique_ID, order)) 

ggplot(df2,aes(x = Month, y = Unique_ID, fill=col))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_manual(name = "TP Retention \n (g/m2/month)", 
                    values = c("#053061", "#2166ac", "#67a9cf" , "#d1e5f0",
                               "#bababa" , "#f2b9b1","#f76752", "#b2182b" )) + 
  theme_minimal(base_size = 8) +
  ylab("Site ID") +
  xlab(" ") +
  geom_text(data=df.sum, aes(x = 14, y=Unique_ID,label=round(TP_Retention,2)), size = 3, fontface = "bold") +
  coord_cartesian(clip = "off") +
  geom_rect(mapping=aes(xmin=12.5, xmax=14.6, ymin=0.5, ymax=36.5), color = "black", fill = NA, size = 1)













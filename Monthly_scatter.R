

### new monthly scatter plots ###


setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop



library(ggplot2)
library(tidyverse)
library(forcats) ## need for fct_relevel

options(scipen = 9)

## data setup
{
x <- read.csv("Monthly_Wetland_P_Clean.csv", header = T)
x <- x[which(x$data_type == "both" | x$data_type == "load"),]
table(x$Source)
unique(x$Source)
x$TP_Retention <- x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo
x$SRP_Retention <- x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo
x$HRL_m_month <- x$Monthly_Inflow_m3_month/x$SA_m2

## subset data
df <-x %>% select(Short_Ref, Short_ID,  Short_year, Month, TP_IN_g_m2_mo, TP_Retention, TP_Retention_percent, 
                  SRP_IN_g_m2_mo, SRP_Retention, SRP_Retention_percent, TP_OUT_g_m2_mo, SRP_OUT_g_m2_mo, HRL_m_month)
df$Unique_ID <- paste(x$Short_Ref, x$Short_ID, x$Short_year, sep = "_")
site_year <-unique(df$Unique_ID)
df <- df[which(df$Short_Ref !=  17),]       ### drop site 17 because it is too gappy
df <- df[which(df$Short_year != "YN"),]     ### drop partial years
df <- na.omit(df)                           ### drop months with NAs
df <- df %>%                                ### reorder months in order
  mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 
df$Study <- as.factor(df$Short_Ref)
levels(df$Study) <- c("Audet et al. 2020", "Hoffmann et al. 2012",
                    "Choate et al. 1990", "Page et al. 2020")

rm(x)
}


ggplot(df, aes(x = HRL_m_month, y = TP_Retention_percent, color = Study)) +
  geom_point() +
  #geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ Month) +
  scale_x_continuous(trans ='log10') +
  coord_cartesian(ylim = c(-200,100)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw(base_size = 14) 



ggplot(df, aes(x = HRL_m_month, y = SRP_Retention_percent, color = Study)) +
  geom_point() +
  #geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ Month) +
  scale_x_continuous(trans ='log10') +
  coord_cartesian(ylim = c(-200,100)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw(base_size = 14) 














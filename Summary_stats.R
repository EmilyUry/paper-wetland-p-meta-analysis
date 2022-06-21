


#' ---
#' title: "Wetland-P-Meta-analysis Summary Statistics"
#' author: "Emily Ury"
#' date: "April 14, 2022"
#' ---
#' 
#' Reported in Table 1 of the manuscript
#'   (a) mean, median, range: 
#'           (i) loading rate
#'           (ii) retention rate
#'           (iii) retention efficiency (%)

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")


## Data set-up
x <- read.csv("Wetland_P_Clean3.csv", header = T)
x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"




## mass at outflow
x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)
### retention
x$TP_Retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
x$SRP_Retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out

### TP
TP_IN <- x %>%
  summarise(mean = mean(TP_load_in_g_m2_yr, na.rm = TRUE),
            sd = sd(TP_load_in_g_m2_yr, na.rm = TRUE),
            median = median(TP_load_in_g_m2_yr, na.rm = TRUE),
            max = max(TP_load_in_g_m2_yr, na.rm = TRUE),
            min = min(TP_load_in_g_m2_yr, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(TP_IN) <- "TP Loading"

TP_Retention <- x %>%
  summarise(mean = mean(TP_Retention, na.rm = TRUE),
            sd = sd(TP_Retention, na.rm = TRUE),
            median = median(TP_Retention, na.rm = TRUE),
            max = max(TP_Retention, na.rm = TRUE),
            min = min(TP_Retention, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(TP_Retention) <- "TP Retention"

TP_Retention_percent <- x %>%
  summarise(mean = mean(TP_Retention_percent, na.rm = TRUE),
            sd = sd(TP_Retention_percent, na.rm = TRUE),
            median = median(TP_Retention_percent, na.rm = TRUE),
            max = max(TP_Retention_percent, na.rm = TRUE),
            min = min(TP_Retention_percent, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(TP_Retention_percent) <- "TP Efficiency"



### SRP
SRP_IN <- x %>%
  summarise(mean = mean(SRP_load_in_g_m2_yr, na.rm = TRUE),
            sd = sd(SRP_load_in_g_m2_yr, na.rm = TRUE),
            median = median(SRP_load_in_g_m2_yr, na.rm = TRUE),
            max = max(SRP_load_in_g_m2_yr, na.rm = TRUE),
            min = min(SRP_load_in_g_m2_yr, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(SRP_IN) <- "SRP Loading"

SRP_Retention <- x %>%
  summarise(mean = mean(SRP_Retention, na.rm = TRUE),
            sd = sd(SRP_Retention, na.rm = TRUE),
            median = median(SRP_Retention, na.rm = TRUE),
            max = max(SRP_Retention, na.rm = TRUE),
            min = min(SRP_Retention, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(SRP_Retention) <- "SRP Retention"

SRP_Retention_percent <- x %>%
  summarise(mean = mean(SRP_Retention_percent, na.rm = TRUE),
            sd = sd(SRP_Retention_percent, na.rm = TRUE),
            median = median(SRP_Retention_percent, na.rm = TRUE),
            max = max(SRP_Retention_percent, na.rm = TRUE),
            min = min(SRP_Retention_percent, na.rm = TRUE)) %>%
  mutate(n = nrow(x)) %>%
  mutate(se = sd/sqrt(n))
rownames(SRP_Retention_percent) <- "SRP Efficiency"




summary <- rbind(TP_IN, TP_Retention, TP_Retention_percent, SRP_IN, SRP_Retention, SRP_Retention_percent)






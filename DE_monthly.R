

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


x$TP.rem.conc <- (x$TP_IN_mg_L - x$TP_OUT_mg_L)
x$TP.rem.concP <- (x$TP_IN_mg_L - x$TP_OUT_mg_L)/x$TP_IN_mg_L*100
x$SRP.rem.conc <- (x$SRP_IN_mg_L - x$SRP_OUT_mg_L)
x$SRP.rem.concP <- (x$SRP_IN_mg_L - x$SRP_OUT_mg_L)/x$SRP_IN_mg_L*100



ggplot(x, aes(x=Month, y = TP.rem.concP )) +
  geom_boxplot() +
  ylim(-100,100)

ggplot(x, aes(x=Month, y = TP.rem.conc )) +
  geom_boxplot() +
  ylim(-1, 15)

ggplot(x, aes(x=Month, y = SRP.rem.concP )) +
  geom_boxplot() +
  ylim(-100,100)

ggplot(x, aes(x=Month, y = SRP.rem.conc )) +
  geom_boxplot() +
  ylim(-1, 15)



source <- x[which(x$TP.rem.conc < 0),]
ggplot(source, aes(x=Month, y = TP.rem.conc )) +
  geom_boxplot() 

ggplot(source, aes(x=Month, y = TP.rem.concP )) +
  geom_boxplot() 




### TP

df.summary <- x %>%
  group_by(Month) %>%
  summarise(
    sd = sd(TP.rem.conc, na.rm = TRUE),
    TP.rem.conc = median(TP.rem.conc, na.rm = TRUE)
  )
df.summary

df.summary2 <- x %>%
  group_by(Month) %>%
  summarise(
    sdP = sd(TP.rem.concP, na.rm = TRUE),
    TP.rem.concP = median(TP.rem.concP, na.rm = TRUE)
  )
df.summary2
df.summary2[1] <- names("month")

newd <- cbind(df.summary, df.summary2)
ggplot(newd, aes(x=Month, y = TP.rem.conc, group = 1 )) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=TP.rem.conc-sd, ymax=TP.rem.conc+sd), width=.2,
                position=position_dodge(.9)) #+
  geom_point(aes(x=Month, y = TP.rem.concP)) +
  geom_line(aes(x=Month, y = TP.rem.concP)) 




##### SRP

x<-replace(x, "-Inf",NaN)

df.summary <- x %>%
  group_by(Month) %>%
  summarise(
    sd = sd(SRP.rem.conc, na.rm = TRUE),
    SRP.rem.conc = mean(SRP.rem.conc, na.rm = TRUE)
  )
df.summary

df.summary2 <- x %>%
  group_by(Month) %>%
  summarise(
    sdP = sd(SRP.rem.concP, na.rm = TRUE),
    SRP.rem.concP = mean(SRP.rem.concP, na.rm = TRUE)
  )
df.summary2
df.summary2[1] <- names("month")

newd <- cbind(df.summary, df.summary2)
ggplot(newd, aes(x=Month, y = TP.rem.conc, group = 1 )) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=TP.rem.conc-sd, ymax=TP.rem.conc+sd), width=.2,
                position=position_dodge(.9)) +
  geom_point(aes(x=Month, y = TP.rem.concP)) +
  geom_line(aes(x=Month, y = TP.rem.concP)) 



tiff(filename = "figures/monthly_conc_red.tiff", height=3600, width=3600, units= "px", res=800, compression= "lzw")

p

dev.off()






ggplot(x, aes(x=Month, y = TP.rem.conc )) +
  geom_boxplot() +
  ylim(-1, 15)

ggplot(x, aes(x=TP_IN_mg_L, y = TP.rem.conc )) +
  geom_point() +
  ylim(-1, 15)



load <- x[which(x$data_type == "both" | x$data_type == "load"),]

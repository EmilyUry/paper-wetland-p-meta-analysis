
#' ---
#' title: "Wetland-P-Meta-analysis scratch figures"
#' author: "Emily Ury"
#' date: "March 14, 2022"
#' output: github_document
#' ---
#' 
#' Scratch figures
#'   (a) 

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")


library(ggplot2)
library(tidyverse)
library(viridis)
library(gridExtra)

## Data set-up
x <- read.csv("Wetland_P_Clean2.csv", header = T)
{head(x)

lX<-log(x[,c(11,13, 16,17)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)

x$ratio <- x$SRP_Retention_percent/x$TP_Retention_percent

## mass at outflow
x$TP_load_out <- x$TP_load_in_g_m2_yr*(1 - x$TP_Retention_percent/100)
x$SRP_load_out <- x$SRP_load_in_g_m2_yr*(1- x$SRP_Retention_percent/100)

### Hydraulic loading rate
x$HLR <- x$Inflow_m3_yr/x$Area_m2


### mass removed
x$TP_retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
x$SRP_retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out
}

median(x$TP_retention)
median(x$SRP_retention)


## flow regime


table(x$Water_regime)
levels(x$Water_regime)

plot(x$Area_m2, x$TP_Retention_percent, col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb")[x$Water_regime],
     log = "x", 
     ylim = c(-300, 105),
     pch = 16, 
     cex = 1.5,
     xlab = "Wetland area (m2)", 
     ylab = "TP % Retention")
abline(h=0, lty = 2)
legend("bottomleft", levels(x$Water_regime), 
       pch = 16, pt.cex = 1.5,
       ncol = 2,
       col = c("#a1a1a1bb", "#bd4ad4bb","#e34327bb", "#345bebbb", "#2b821fbb"))


plot(x$Area_m2, x$SRP_Retention_percent, col = c("#2b821fbb", "#bd4ad4bb","#e34327bb", "#345bebbb","#a1a1a1bb" )[x$Water_regime],
     log = "x", 
     ylim = c(-300, 105),
     pch = 16, 
     cex = 1.5,
     xlab = "Wetland area (m2)", 
     ylab = "SRP % Retention")
abline(h=0, lty = 2)
legend("bottomleft", levels(x$Water_regime), 
       pch = 16, pt.cex = 1.5,
       col = c("#2b821fbb", "#bd4ad4bb","#e34327bb", "#345bebbb","#a1a1a1bb" ))






## normalize retention by looking at mass export over mass input

x$TP_export_norm <- x$TP_load_out/x$TP_load_in_g_m2_yr
hist(log(x$TP_export_norm))

x$SRP_export_norm <- x$SRP_load_out/x$SRP_load_in_g_m2_yr
hist(log(x$SRP_export_norm))

plot(x$Area_m2, x$TP_export_norm, col = c("#2b821fbb", "#bd4ad4bb","#e34327bb", "#345bebbb","#a1a1a1bb" )[x$Water_regime],
     log = "x", 
     pch = 16, 
     cex = 1.5,
     xlab = "Wetland area (m2)", 
     ylab = "TP Export/Input")
abline(h=1, lty = 2)
legend("topleft", levels(x$Water_regime), 
       pch = 16, pt.cex = 1.5,
       ncol = 1,
       col =c("#2b821fbb", "#bd4ad4bb","#e34327bb", "#345bebbb","#a1a1a1bb" ))


plot(x$Area_m2, x$SRP_export_norm, col =c("#2b821fbb", "#bd4ad4bb","#e34327bb", "#345bebbb","#a1a1a1bb" )[x$Water_regime],
     log = "xy", 
     pch = 16, 
     cex = 1.5,
     xlab = "Wetland area (m2)", 
     ylab = "SRP Export/Input")
abline(h=1, lty = 2)
legend("topleft", levels(x$Water_regime), 
       pch = 16, pt.cex = 1.5,
       col = c("#2b821fbb", "#bd4ad4bb","#e34327bb", "#345bebbb","#a1a1a1bb" ))





####### Flow regime

plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     #col = "#515151bb",
     col = c("#2b821fbb", "#bd4ad4bb","#345bebbb", "#e34327bb", "#a1a1a1bb" )[x$Water_regime],
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomleft", levels(x$Water_regime), pch = 16,
       pt.cex = 2, col = c("#2b821fbb", "#bd4ad4bb","#345bebbb", "#e34327bb", "#a1a1a1bb" ))


### wetland type
table(x$Wetland_Type)
x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
x$Wetland_Type <- droplevels(x$Wetland_Type)


plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     #col = "#515151bb",
     col = c(  "#e34327bb","#2b821fbb",  "#345bebbb","#a1a1a1bb" )[x$Wetland_Type],
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomleft", levels(x$Wetland_Type), pch = 16,
       pt.cex = 2, col = c("#e34327bb", "#2b821fbb",   "#345bebbb","#a1a1a1bb" ))



##
rbPal <- colorRampPalette(c('pink' , 'red','orange', 'yellow', 'green', 'cyan', 'blue', 'purple', 'black', 'gray'))
x$Source <- as.factor(x$Source)

### STUDY
plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     col = rbPal(51)[x$Source],
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("top", levels(x$Source), pch = 16,
       pt.cex = 1, col = rbPal(51)[x$Source], ncol = 3, cex = 0.5)


## mean SRP retention
summary(x$SRP_Retention_percent)

summary(x$TP_Retention_percent)
table(x$Catchment_Type)


plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     col = c( "#e34327bb","#2b821fbb", "#345bebbb")[x$Catchment_Type],
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomleft", levels(x$Catchment_Type), pch = 16,
       pt.cex = 2, col = c(  "#e34327bb","#2b821fbb","#345bebbb"))




### wetland size (sm, med, large)
hist(log10(x$Area_m2))


rbPal <- colorRampPalette(c('red','blue'))
x$col <- rbPal(3)[as.numeric(cut(x$Area_m2, breaks = c(0, 100, 10000,  Inf)))]
x$alpha <- rep(99, 276)
x$col2 <- paste(x$col, x$alpha, sep = "")

plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     col = x$col2,
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomleft", c(" < 100 m2", "100 - 1000 m2", " > 1000 m2"), pch = 16,
       pt.cex = 2, col = c("#FF000099",  "#7F007F99","#0000FF99"))


### flow/area
hist(log10(x$Inflow_m3_yr/x$Area_m2))
x$flow.norm <- x$Inflow_m3_yr/x$Area_m2


rbPal <- colorRampPalette(c('red','blue'))
x$col <- rbPal(3)[as.numeric(cut(x$flow.norm, breaks = c(0, 8, 30,  Inf)))]
x$alpha <- rep(99, 276)
x$col2 <- paste(x$col, x$alpha, sep = "")
x$col2[which(x$col2 == "NA99" )] <- "#99999999"

plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     col = x$col2,
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomleft", c(" low", "med", " high"), pch = 16,
       pt.cex = 2, col = c("#FF000099",  "#7F007F99","#0000FF99"))




### flow volume in
hist(log10(x$Inflow_m3_yr))
x$flow.norm <- x$Inflow_m3_yr/x$Area_m2


rbPal <- colorRampPalette(c('red','blue'))
x$col <- rbPal(3)[as.numeric(cut(x$Inflow_m3_yr, breaks = c(0, 1000, 10000,  Inf)))]
x$alpha <- rep(99, 276)
x$col2 <- paste(x$col, x$alpha, sep = "")
x$col2[which(x$col2 == "NA99" )] <- "#99999999"

plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     col = x$col2,
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomleft", c(" low", "med", " high"), pch = 16,
       pt.cex = 2, col = c("#FF000099",  "#7F007F99","#0000FF99"))





### flow anomally
library(tidyverse)
Flow <- aggregate(Inflow_m3_yr ~ Source + WetlandID, data = x, FUN = mean) %>%
        mutate(Unique_ID = paste(Source, WetlandID))
names(Flow)[3] <- "mean_flow"


wetlands <- x %>%
        mutate(Unique_ID = paste(Source, WetlandID)) %>%
        group_by(Unique_ID) %>%
        summarize(num = n()) %>%
        filter(num > 1) %>%
        left_join(Flow, by = "Unique_ID") %>%
        select(c('Source', 'WetlandID', 'mean_flow'))


data <- x %>%
        left_join(wetlands, by = c("Source", "WetlandID")) %>%
        mutate(flow_anom = (Inflow_m3_yr - mean_flow)/mean_flow*100)
data <- data[-c(249:259),] 
data <- data[-c(98:107),]                              ###### a bunch of rows report the average flow for all yrs. need to remove these
data <- data[-c(54:82),]


## Red/Blue binary
rbPal <- colorRampPalette(c('red','blue'))
data$col <- paste(rbPal(10)[as.numeric(cut(data$flow_anom, breaks = 10))], "85", sep = "")
data <- data %>%
        mutate(col = replace(col, col == "NA85", "#8a8a8a")) %>%
        mutate(col = replace(col, flow_anom == 0, "#8a8a8a")) %>%
        mutate(col = replace(col, flow_anom < 0, "#FF000085")) %>%
        mutate(col = replace(col, flow_anom > 0, "#0000FF85")) 
data$flow_anom[is.na(data$flow_anom)] <- 0
plot(data$SRP_Retention_percent, data$TP_Retention_percent, 
     pch = 16,
     cex = abs(data$flow_anom)/30+1,
     col = data$col,
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomright", c("High flow year", "Low flow year"), pch = 16,
       pt.cex = 2, col = c("#0000FF85","#FF000085"))









##
#### influent concentration
## 

hist(x$TP_Inflow_mg_L)
rbPal <- colorRampPalette(c('red','blue'))
x$col <- rbPal(3)[as.numeric(cut(x$TP_Inflow_mg_L, breaks = c(0, 0.2, 2,  Inf)))]
x$alpha <- rep(99, nrow(x))
x$col2 <- paste(x$col, x$alpha, sep = "")
x$col2[which(x$col2 == "NA99" )] <- "#99999999"

plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.5,
     col = x$col2,
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
legend("bottomleft", c(" < 0.2 mg/L", "0.2 - 2", " > 2 mg/L"), pch = 16,
       pt.cex = 2, col = c("#FF000099",  "#7F007F99","#0000FF99"))


x$ret <- ifelse(x$TP_Retention_percent > 0, "pos", "neg")

plot(x$logInflow_m3_yr, x$logTP_Inflow_mg_L, pch = 16, col = as.factor(x$ret) )

x$SRPret <- ifelse(x$SRP_Retention_percent > 0, "pos", "neg")

plot(x$logInflow_m3_yr, x$logSRP_Inflow_mg_L, pch = 16, col = as.factor(x$SRPret) )


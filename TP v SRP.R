
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

####### Flow regime

par(mfrow = c(1,1), xpd = TRUE)


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










par(xpd = FALSE)
plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.3,
     col = "black",
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)





##
#### influent loads
## 

## blue, purple, red
mypal3 = c("#0000FF99",  "#7F007F99",  "#FF000099")
mypal5 = c("#FF000099", "#BF003F99", "#7F007F99", "#3F00BF99", "#0000FF99")


############# % retention

########  TP

rbPal <- colorRampPalette(c('blue','red'))
x$bins <- cut_number(x$TP_load_in_g_m2_yr, 3)
x$col <- rbPal(3)[as.numeric(cut_number(x$TP_load_in_g_m2_yr, 3))]
x$alpha <- rep(99, nrow(x))
x$col2 <- paste(x$col, x$alpha, sep = "")
x$col2[which(x$col2 == "NA99" )] <- "#99999999"
par(xpd = FALSE)
plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.3,
     col = x$col2,
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
par(xpd = TRUE)
legend("top", inset=c(0,-0.2), levels(x$bins), pch = 16,
       pt.cex = 2, col = mypal3, title = "TP load (g/m2/yr)", 
       horiz=TRUE, box.lty=0 , cex = 0.8)



######  SRP



x$bins <- cut_number(x$SRP_load_in_g_m2_yr, 3)
x$col <- rbPal(3)[as.numeric(cut_number(x$SRP_load_in_g_m2_yr, 3))]
x$alpha <- rep(99, nrow(x))
x$col2 <- paste(x$col, x$alpha, sep = "")
x$col2[which(x$col2 == "NA99" )] <- "#99999999"

par(xpd = FALSE)
plot(x$SRP_Retention_percent, x$TP_Retention_percent, 
     pch = 16,
     cex = 1.3,
     col = x$col2,
     xlim = c(-250, 105), 
     ylim = c(-150, 105), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
par(xpd = TRUE)
legend("top", inset=c(0,-0.2), levels(x$bins), pch = 16,
       pt.cex = 2, col = mypal3, title = "SRP load (g/m2/yr)", 
       horiz=TRUE, box.lty=0 , cex = 0.8)




################## absolute retention



########  TP


x$bins <- cut_number(x$TP_load_in_g_m2_yr, 3)
x$col <- rbPal(3)[as.numeric(cut_number(x$TP_load_in_g_m2_yr, 3))]
x$alpha <- rep(99, nrow(x))
x$col2 <- paste(x$col, x$alpha, sep = "")
x$col2[which(x$col2 == "NA99" )] <- "#99999999"
par(xpd = FALSE)
plot(x$SRP_retention, x$TP_retention, 
     pch = 16,
     cex = 1.3,
     col = x$col2,
     xlim = c(-10, 40), 
     ylim = c(-10, 40), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
par(xpd = TRUE)
legend("top", inset=c(0,-0.2), levels(x$bins), pch = 16,
       pt.cex = 2, col = mypal3, title = "TP load (g/m2/yr)", 
       horiz=TRUE, box.lty=0 , cex = 0.8)



######  SRP


rbPal <- colorRampPalette(c('red','blue'))
x$bins <- cut_number(x$SRP_load_in_g_m2_yr, 3)
x$col <- rbPal(3)[as.numeric(cut_number(x$SRP_load_in_g_m2_yr, 3))]
x$alpha <- rep(99, nrow(x))
x$col2 <- paste(x$col, x$alpha, sep = "")
x$col2[which(x$col2 == "NA99" )] <- "#99999999"

par(xpd = FALSE)
plot(x$SRP_retention, x$TP_retention, 
     pch = 16,
     cex = 1.3,
     col = x$col2,
     xlim = c(-10, 40), 
     ylim = c(-10, 40), 
     xlab = "SRP % Retention",
     ylab = "TP % Retention")
abline(1,1)
abline(h=0, col = 'gray50', lwd =1, lty = 2)
abline(v=0, col = 'gray30', lwd = 1, lty = 2)
par(xpd = TRUE)
legend("top", inset=c(0,-0.2), levels(x$bins), pch = 16,
       pt.cex = 2, col = mypal3, title = "SRP load (g/m2/yr)", 
       horiz=TRUE, box.lty=0 , cex = 0.8)


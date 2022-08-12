

#' ---
#' title: "Figure 4 K~Tau"
#' author: "Emily Ury"
#' date: "July 29, 2022"
#' output: github_document
#' ---
#' 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop


library(ggplot2)
library(tidyverse)
#library(gridExtra)

## Data set-up
x <- read.csv("Wetland_P_Clean3.csv", header = T)

x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"



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



## calculate k (rate constant) TP  
x$kTP <- -(log(1-(x$TP_Retention_percent/100))/x$HRT_d)   ## measured HRT
x$kSRP <- -(log(1-(x$SRP_Retention_percent/100))/x$HRT_d)   ## measured HRT

#y <- x[complete.cases(x[36]),]
#z <- x[complete.cases(x[37]),]

#### K tao
#plot((x$HRT_d), (x$kTP), log = 'xy', cex = 1.5)
#plot(log(x$HRT_d), log(x$kTP), pch = 16)



tiff(filename = "figures/Fig3_Ktao_new.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")


par(mar= c(3,3,1,1), cex.axis= 0.6, cex.lab= 0.6)
plot(log(x$HRT_d), log(x$kTP), pch = 16, col = "#2c728ecc", cex = 0.6,
     ylab = " ", #xlab = "log retention time (d)"
     xlab = " ")
title(ylab = "log k", line = 2)
title(xlab = expression(paste("log ", tau)), line = 2)
fitTP <- lm(log(x$kTP) ~ log(x$HRT_d))
k <- coef(fitTP)
abline(fitTP, col = "#2c728e")
summary(fitTP)
coef(fitTP)[1]
exp(coef(fitTP)[2])
text(0,-6.1, expression(bold(paste("k"[TP], " = 0.43", tau)^-1.26)), cex = 0.6, col = "#2c728e")
text(0,0.-6.7, expression(bold(paste(" ",R^2, " = 0.43, p < 0.0001"))), cex = 0.6, col ="#2c728e")


### add SRP
points(log(x$HRT_d), log(x$kSRP), col = "#440154bb", pch = 16, cex = 0.6)
fitSRP <- lm(log(x$kSRP) ~ log(x$HRT_d))
k <- coef(fitSRP)
abline(fitSRP, col = "#440154")
summary(fitSRP)
coef(fitSRP)[1]
exp(coef(fitSRP)[2])
text(3,0.3, expression(bold(paste("k"[PO4]," = 0.43", tau)^-1.15)), cex = 0.6, col = "#440154")
text(2.9,-0.3, expression(bold(paste(" ",R^2, " = 0.33, p < 0.0001"))), cex = 0.6, col = "#440154")


dev.off()











###quads

x$quad <- ifelse(x$TP_Retention_percent < 0 | x$SRP_Retention_percent < 0, "< 0", 
                 ifelse(x$TP_Retention_percent < 33 | x$SRP_Retention_percent < 33, "0-33",
                        ifelse(x$TP_Retention_percent < 67 | x$SRP_Retention_percent < 67, "33-67", "67-100")))



Q33 <- x[which(x$quad == "0-33"),]
Q67 <- x[which(x$quad == "33-67"),]
Q100 <- x[which(x$quad == "67-100"),]

#
tiff(filename = "figures/Fig3_Ktao_supp_TP.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")

par(mar= c(3,3,1,1), cex.axis= 0.6, cex.lab= 0.6)

plot(log(Q33$HRT_d), log(Q33$kTP), pch = 21, bg = "orange", cex = 0.6,
     ylab = " ", #xlab = "log retention time (d)"
     xlab = " ",
     xlim = c(-2,5), ylim = c(-7,1))
title("(A) TP", adj = 0, cex.main = 0.9)
title(ylab = "log k", line = 2)
title(xlab = expression(paste("log ", tau)), line = 2)
fitTP <- lm(log(Q33$kTP) ~ log(Q33$HRT_d))
k <- coef(fitTP)
abline(fitTP, col = "#d17300")
summary(fitTP)
coef(fitTP)[1]
exp(coef(fitTP)[2])
text(0,-6.3, expression(bold(paste("k"[0-33], " = 0.43", tau)^-1.86)), cex = 0.4, col = "#d17300")
text(0,0.-6.7, expression(bold(paste(" ",R^2, " = 0.48, p < 0.0001"))), cex = 0.4, col = "#d17300")



points(log(Q67$HRT_d), log(Q67$kTP), pch = 21, bg = "yellow", cex = 0.6,
     ylab = " ", #xlab = "log retention time (d)"
     xlab = " ")
fitTP <- lm(log(Q67$kTP) ~ log(Q67$HRT_d))
k <- coef(fitTP)
abline(fitTP, col = "black")
summary(fitTP)
coef(fitTP)[1]
exp(coef(fitTP)[2])
text(-1,-5.1, expression(bold(paste("k"[33-67], " = 0.37", tau)^-0.43)), cex = 0.4)
text(-1,0.-5.5, expression(bold(paste(" ",R^2, " = 0.87, p < 0.0001"))), cex = 0.4)



points(log(Q100$HRT_d), log(Q100$kTP), pch = 21, bg = "green", cex = 0.6,
     ylab = " ", #xlab = "log retention time (d)"
     xlab = " ")
fitTP <- lm(log(Q100$kTP) ~ log(Q100$HRT_d))
k <- coef(fitTP)
abline(fitTP, col = "#007d15")
summary(fitTP)
coef(fitTP)[1]
exp(coef(fitTP)[2])
text(3.2,0.3, expression(bold(paste("k"[67-100], " = 0.38", tau)^0.28)), cex = 0.4, col = "#007d15")
text(3.1,-0.1, expression(bold(paste(" ",R^2, " = 0.94, p < 0.0001"))), cex = 0.4, col = "#007d15")


dev.off()




tiff(filename = "figures/Fig3_Ktao_supp_PO4.tif", height=2400, width=2400, units= "px", res=800, compression= "lzw")

par(mar= c(3,3,1,1), cex.axis= 0.6, cex.lab= 0.6)

plot(log(Q33$HRT_d), log(Q33$kSRP), pch = 21, bg = "orange", cex = 0.6,
     ylab = " ", #xlab = "log retention time (d)"
     xlab = " ",
     xlim = c(-2,5), ylim = c(-7,1))
title("(B) PO4", adj = 0, cex.main = 0.9)
title(ylab = "log k", line = 2)
title(xlab = expression(paste("log ", tau)), line = 2)
fitTP <- lm(log(Q33$kSRP) ~ log(Q33$HRT_d))
abline(fitTP, col = "#d17300")
summary(fitTP)
coef(fitTP)[1]
exp(coef(fitTP)[2])
text(0,-6.3, expression(bold(paste("k"[0-33], " = 0.31", tau)^-1.30)), cex = 0.4, col = "#d17300")
text(0,0.-6.7, expression(bold(paste(" ",R^2, " = 0.60, p < 0.0001"))), cex = 0.4, col = "#d17300")



points(log(Q67$HRT_d), log(Q67$kSRP), pch = 21, bg = "yellow", cex = 0.6,
       ylab = " ", #xlab = "log retention time (d)"
       xlab = " ")
fitTP <- lm(log(Q67$kSRP) ~ log(Q67$HRT_d))
abline(fitTP, col = "black")
summary(fitTP)
coef(fitTP)[1]
exp(coef(fitTP)[2])
text(-1,-5.1, expression(bold(paste("k"[33-67], " = 0.47", tau)^-0.68)), cex = 0.4)
text(-1,0.-5.5, expression(bold(paste(" ",R^2, " = 0.69, p < 0.0001"))), cex = 0.4)



points(log(Q100$HRT_d), log(Q100$kSRP), pch = 21, bg = "green", cex = 0.6,
       ylab = " ", #xlab = "log retention time (d)"
       xlab = " ")
fitTP <- lm(log(Q100$kSRP) ~ log(Q100$HRT_d))
abline(fitTP, col = "#007d15")
summary(fitTP)
coef(fitTP)[1]
exp(coef(fitTP)[2])
text(3.2,0.3, expression(bold(paste("k"[67-100], " = 0.38", tau)^0.55)), cex = 0.4, col = "#007d15")
text(3.1,-0.1, expression(bold(paste(" ",R^2, " = 0.95, p < 0.0001"))), cex = 0.4, col = "#007d15")


dev.off()



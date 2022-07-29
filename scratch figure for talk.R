

##scratch - talk figure
setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")

x <- read.csv("Wetland_P_Clean2.csv", header = T)
{
  x$Water_regime <- as.factor(x$Water_regime)
  
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

options(scipen = 99)

par(mfrow = c(1,1))

plot(x$Area_m2, x$TP_retention, log = "x", ylim = c(-100,150), pch = 16, col = "#00000077",
     xlab = "Wetland Area (m2)", ylab = "Retention (g/m2/yr)")
points(x$Area_m2, x$SRP_retention, log = "x", ylim = c(-100,150), pch = 16, col = "#0000FF55")
legend("topright", c("TP", "SRP"), pch = 16, col = c( "#000000aa" , "#0000FFdd"  ))

plot(x$Area_m2, x$TP_retention, log = "x", ylim = c(-100,150), pch = 16,
     xlab = "Wetland Area (m2)", ylab = "Retention (g/m2/yr)")
points(x$Area_m2, x$SRP_retention, log = "x", ylim = c(-100,150), pch = 16)
abline(b = (-56.26/4.53), a = (56.29))
#text(1000000,130, "R2=0.04")
text(1000000, 140, expression(paste(" ",R^2, "= 0.040")), font =  2)
text(1000000, 110, "P = 0.0004")

fit <- lm(x$TP_retention ~ log(x$Area_m2))
abline(fit)

fit


summary(fit)


plot(x$Area_m2, x$SRP_retention, log = "x", ylim = c(-100,150), pch = 16,
     xlab = "Wetland Area (m2)", ylab = "Retention (g/m2/yr)", 
     main = "SRP")



plot(x$SRP_Retention_percent, x$TP_Retention_percent, pch = 16, cex =0.5,
     xlim = c(-300,100),
     ylim = c(-500, 100))
abline(h=0)
abline(v=0)
xx <- x[which(x$Source == "Hoffmann 2012"),]
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "red")
xx <- x[which(x$Source == "Tanner 2011"),]
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "red")
xx <- x[which(x$Source == "Toet 2005"),]
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "red")
xx <- x[which(x$Source == "Audet 2020"),]
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "green")
xx <- x[which(x$Source == "Mendes 2018"),] 
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "cyan")
xx <- x[which(x$Source == "Barco 2017"),] 
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "purple")
xx <- x[which(x$Source == "Dunne 2015"),] 
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "blue")
xx <- x[which(x$Source == "Beutel 2014"),] 
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "orange")
xx <- x[which(x$Source == "Jiang 2020"),] 
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "violet")
xx <- x[which(x$Source == "Carstensen 2019"),] 
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "darkgreen")
xx <- x[which(x$Source == "DUCs 2022"),] 
points(xx$SRP_Retention_percent, xx$TP_Retention_percent, pch = 16, cex =1, col = "lightblue")


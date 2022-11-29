


# library
library(rgl)

# This is to output a rgl plot in a rmarkdown document.
# setupKnitr()

# Data: the iris data is provided by R
data <- iris

# Add a new column with color

mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
data$color <- mycolors[ as.numeric(data$Species) ]

# Plot
plot3d( 
  x=data$Sepal.Length, y=data$Sepal.Width, z=data$Petal.Length, 
  col = data$color, 
  type = 's', 
  radius = .1,
  xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")

# To display in an R Markdown document:
# rglwidget()

# To save to a file:
htmlwidgets::saveWidget(rglwidget(width = 520, height = 520), 
                        file = "HtmlWidget/3dscatter.html",
                        libdir = "libs",
                        selfcontained = FALSE
)





# monthly data set up
{
  x <- read.csv("Monthly_Wetland_P_Clean.csv", header = T)
  x <- x[which(x$data_type == "both" | x$data_type == "load"),]
  x$TP_Retention <- x$TP_IN_g_m2_mo - x$TP_OUT_g_m2_mo
  x$SRP_Retention <- x$SRP_IN_g_m2_mo - x$SRP_OUT_g_m2_mo
  x$Unique_ID <- paste(x$Short_Ref, x$Short_ID, x$Short_year, sep = "_")
  M <-x %>% select(Unique_ID, Short_Ref, Short_ID,  Short_year, Month, TP_IN_g_m2_mo, TP_Retention, TP_Retention_percent, 
                   SRP_IN_g_m2_mo, SRP_Retention, SRP_Retention_percent, TP_OUT_g_m2_mo, SRP_OUT_g_m2_mo, 
                   Monthly_Inflow_m3_month, Monthly_Outflow_m3_month, 
                   SA_m2, Catchment_area)
  M <- M[which(M$Short_Ref !=  17),]       ### drop site 17 because it is too gappy
  M <- M[which(M$Short_year != "YN"),]     ### drop partial years
  #M <- na.omit(M)                           ### drop months with NAs
  M <- M %>%                                ### reorder months in order
    mutate(Month = fct_relevel(Month, "Jan", "Feb", "Mar", "Apr",
                               "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 
  M$water_atten <- M$Monthly_Inflow_m3_month - M$Monthly_Outflow_m3_month
  M$water_atten_percent <- 100*M$water_atten/M$Monthly_Inflow_m3_month
  M$CAWA <- M$Catchment_area/(M$SA_m2/10000)
  M$HLR <- M$Monthly_Inflow_m3_month/M$SA_m2
  
  M$binTP <- ifelse(M$TP_Retention_percent > 66, 1,
                    ifelse(M$TP_Retention_percent > 33, 2,
                           ifelse(M$TP_Retention_percent > 0.00001, 3, 4)))
  M$binSRP <- ifelse(M$SRP_Retention_percent > 66, 1,
                     ifelse(M$SRP_Retention_percent > 33, 2,
                            ifelse(M$SRP_Retention_percent > 0.00001, 3, 4)))
  
  M.ON <- M[which(M$Short_Ref == 18),]
  M.Aud <- M[which(M$Short_Ref == 4),]
}

##### annual data set up
{
  x <- read.csv("Wetland_P_Clean3.csv", header = T)
  x <- x[which(x$Source != "Kennedy 2020"),] ## remove the one whose type is "cranberry farm"
  x <- x[which(x$Source != "Dunne 2012"),] ## remove the one whose type is "cranberry farm"
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
  x$water_atten <- x$Inflow_m3_yr - x$Outflow_m3_yr
  x$water_atten_percent <- 100*x$water_atten/x$Inflow_m3_yr
  x$CAWA <- x$Catchment_area_ha/x$Area_m2*10000
  
  
  x$binTP <- ifelse(x$TP_Retention_percent > 66, 1,
                    ifelse(x$TP_Retention_percent > 33, 2,
                           ifelse(x$TP_Retention_percent > 0.00001, 3, 4)))
  x$binTP <- ifelse(x$TP_Retention_percent > 66, 1,
                    ifelse(x$TP_Retention_percent > 33, 2,
                           ifelse(x$TP_Retention_percent > 0.00001, 3, 4)))
  
  
  x.ON <- x[which(x$Source == "DUCs 2022"),]
  x.Aud <- x[which(x$Source == "Audet 2020"),]
}

M$color <- ifelse(M$TP_Retention_percent > 66, "green", 
                  ifelse(M$TP_Retention_percent > 33, "yellow", 
                         ifelse(M$TP_Retention_percent > 0, "orange", "red")))
M$color <- "gray50"

M <- M[!is.na(M$HLR),]
  
# Plot
plot3d(x= log(M$HLR),
       y= M$TP_Retention_percent, 
       z= M$water_atten_percent, 
  xlim = c(-8,4),
  ylim = c(-200,100),
  zlim = c(-100,110),
  col = M$color,
  type = 's', 
  radius = 3,
  xlab="HLR", ylab="TP Reten (%)", zlab="Water Atten (%)")


open3d()
x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x, y)
plot3d(x, y, z, col = rainbow(1000))






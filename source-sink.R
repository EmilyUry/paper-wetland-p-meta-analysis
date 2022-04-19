

#' ---
#' title: "Source/sink"
#' author: "Emily Ury"
#' last update: "April 19, 2022"
#' ---
#' 
#' Source-sink behavior at the annual scale


## data setup:
setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop
x <- read.csv("Wetland_P_Clean2.csv", header = T)
head(x)





x$ret <- ifelse(x$TP_Retention_percent > 0, "pos", "neg")
table(x$ret)
45/(45+233)
TP.source <- table(x$ret)[1]
TP.sink <- table(x$ret)[2]
TP.source.percent <- round(TP.source/(TP.sink+TP.source)*100,1)

x$PO4ret <- ifelse(x$SRP_Retention_percent > 0, "pos", "neg")
table(x$PO4ret)
72/(72+205)
SRP.source <- table(x$PO4ret)[1]
SRP.sink <- table(x$PO4ret)[2]
SRP.source.percent <- round(SRP.source/(SRP.sink+SRP.source)*100,1)

behavior <- c("source", "sink", "source", "sink")
species <- c("TP", "TP", "SRP", "SRP")
num <- c(TP.sink, TP.source, SRP.sink, SRP.source)
label_ypos <- c(240, 10, 240, 10)
label_text <- c("source \n(16%)", "sink", "source \n(26%)", "sink")
data <- data.frame(behavior, species, num, label_ypos, label_text)

c <- ggplot(data, aes(x = factor(species, level = c("TP", "SRP")), y = (num), fill = behavior)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_ypos, label = label_text), vjust = 0, hjust = "middle", color = "white", size = 2.5, fontface = "bold")+
  theme_classic() +
  labs(title = " ", x = " ", y = "n" ) +
  #labs(title = "(c) Wetland sink/source behavior", x = " ", y = "n" ) +
  scale_fill_manual(values = c("#414487bb", "#414487ff")) + 
  theme(legend.position = "none") +
  theme(plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.0, unit = "cm")) +
  theme(plot.title = element_text(hjust = 0.6, size = 7, face = "bold"))

c


tiff(filename = "figures/Figure2.tiff", height=1800, width=1800, units= "px", res=800, compression= "lzw")
c
dev.off()



























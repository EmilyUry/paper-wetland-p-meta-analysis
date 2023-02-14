


#### New categorical plots

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis")  #laptop

library(ggplot2)
library(tidyverse)
library(cowplot)

x <- read.csv("Wetland_P_Clean3.csv", header = T)

## data set-up
{
  
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
  
  x$TP_retention <- x$TP_load_in_g_m2_yr - x$TP_load_out
  x$SRP_retention <- x$SRP_load_in_g_m2_yr - x$SRP_load_out
}

x$Catchment_Type <- as.factor(x$Catchment_Type)
x$group <- ifelse(x$Catchment_Type == "WWTP", "WWT", 
                  ifelse(x$Wetland_Type == "Mesocosm", "Mesocosm", "Restored/\nConstructed"))


x$Water_regime <- as.factor(x$Water_regime)
table(x$group, x$Water_regime)

x$WR <- ifelse(x$Water_regime == "continuous, constant", "Continuous\nregulated", 
               ifelse(x$Water_regime == "continuous, variable", "Continuous\nunregulated",
                      ifelse(x$Water_regime == "intermittent, constant", "Intermittent\nregulated",
                             ifelse(x$Water_regime == "intermittent, variable", "Intermittent\nunregulated", "other"))))

table(x$WR)
table(x$Water_regime)

x$flow <- ifelse(x$Water_regime == "continuous, constant", "Regulated", 
               ifelse(x$Water_regime == "continuous, variable", "Unregulated",
                      ifelse(x$Water_regime == "intermittent, constant", "Regulated",
                             ifelse(x$Water_regime == "intermittent, variable", "Unregulated", "other"))))
table(x$flow)

x$flow2 <- ifelse(x$Water_regime == "continuous, constant", "Continuous", 
                  ifelse(x$Water_regime == "continuous, variable", "Continuous",
                         ifelse(x$Water_regime == "intermittent, constant", "Intermittent",
                                ifelse(x$Water_regime == "intermittent, variable", "Intermittent", "other"))))

y <- x[which(x$WR != "other"),]
table(y$group, y$flow)

summary <- y %>%
  group_by(flow, group) %>%
  summarise(count = n()) %>%
  mutate(TP_Retention_percent = -178,
         SRP_Retention_percent = -178)

sig <- y %>%
  group_by(flow, group) %>%
  summarise(count = n()) %>%
  mutate(TP_Retention_percent = 115,
         SRP_Retention_percent = 115)
sig$TP <- c("A", "A", "B" , "A", "A" , "A")
sig$SRP <- c("A", "A", "B", "A", "A", "A")



pal <- c('#e5a77f', '#3e783f', '#6fabd9')

a <- ggplot(y, aes(x = group, y = TP_Retention_percent, fill = group )) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(labels = c("M", "R/C", "W")) +
  coord_cartesian(ylim = c(-180,120)) +
  facet_grid(.~flow, space = "free") +
  theme_bw(base_size = 10) +
  geom_text(data = summary, aes(label = count),
            position = position_dodge(width = 1.0), size = 2.9, color = "gray50")+
  geom_text(data = sig, aes(label = TP), 
            position = position_dodge(width = 1.0), size = 2.5) +
  ylab("Retention (%)") +
  xlab(" ") +
  theme(plot.margin = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text( hjust = -0.2, vjust = -10, size = 14)) +
  ggtitle("TP")



b <- ggplot(y, aes(x = group, y = SRP_Retention_percent, fill = group )) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(labels = c("M", "R/C", "W")) +
  coord_cartesian(ylim = c(-180,120)) +
  facet_grid(.~flow, space = "free") +
  theme_bw(base_size = 10) +
  geom_text(data = summary, aes(label = count),
            position = position_dodge(width = 1.0), size = 2.9, color = "gray50")+
  geom_text(data = sig, aes(label = SRP), 
            position = position_dodge(width = 1.0), size = 2.5) +
  ylab("Retention (%)") +
  xlab(" ") +
  theme(plot.margin = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text( hjust = -0.2, size = 14)) +
  ggtitle(expression("PO"[4]^"3-"))


plot_grid(a,b, nrow = 2, labels = c(" ", " "), rel_heights = c(1,0.9))


##### paper version figure 4

tiff(filename = "figures/Type_box_facet.tif", height=5, width=4, units= "in", res=800, compression= "lzw")

plot_grid(a,b, nrow = 2, labels = c(" ", " "), rel_heights = c(1,0.85))

dev.off()







### stats

y$newgroup <- paste(y$group, y$flow)
y %>%
  group_by(newgroup) %>%
  get_summary_stats(SRP_Retention_percent, type = "median_mad")

pwc <- y %>%
  pairwise_t_test(TP_Retention_percent ~  newgroup, p.adjust.method = 'bonferroni')
pwc

pwc <- y %>%
  pairwise_t_test(SRP_Retention_percent ~  newgroup, p.adjust.method = 'bonferroni')
pwc


reg <- y[which(y$flow == "Regulated"),]
ureg <- y[which(y$flow == "Unregulated"),]


pwc <- reg %>%
  pairwise_t_test(TP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
pwc

pwc <- reg %>%
  pairwise_t_test(SRP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
pwc


pwc <- ureg %>%
  pairwise_t_test(TP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
pwc

pwc <- ureg %>%
  pairwise_t_test(SRP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
pwc





##### same plot but with points jitter

pal <- c('#e5a77faa', '#3e783f33', '#6fabd9ee')

a <- ggplot(y, aes(x = group, y = TP_Retention_percent, fill = group )) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = pal) +
  geom_point(position = position_jitterdodge(), alpha = 0.5, size = 2, aes(pch = flow2)) +
  #scale_color_manual(values = c("black", "gray70"))+
  scale_x_discrete(labels = c("M", "R/C", "W")) +
  coord_cartesian(ylim = c(-180,120)) +
  facet_grid(.~flow, space = "free") +
  theme_bw(base_size = 14) +
  geom_text(data = summary, aes(label = count),
            position = position_dodge(width = 1.0), size = 5, color = "gray50")+
  geom_text(data = sig, aes(label = TP), 
            position = position_dodge(width = 1.0), size = 5) +
  ylab("Retention (%)") +
  xlab(" ") +
  theme(plot.margin = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text( hjust = -0.1, vjust = -10, size = 18)) +
  ggtitle("TP")



b <- ggplot(y, aes(x = group, y = SRP_Retention_percent, fill = group )) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  geom_point(position = position_jitterdodge(), size = 2, alpha = 0.5, aes(pch = flow2)) +
    scale_fill_manual(values = pal) +
  scale_x_discrete(labels = c("M", "R/C", "W")) +
  coord_cartesian(ylim = c(-180,120)) +
  facet_grid(.~flow, space = "free") +
  theme_bw(base_size = 14) +
  geom_text(data = summary, aes(label = count),
            position = position_dodge(width = 1.0), size = 5, color = "gray50")+
  geom_text(data = sig, aes(label = SRP), 
            position = position_dodge(width = 1.0), size = 5) +
  ylab("Retention (%)") +
  xlab(" ") +
  theme(plot.margin = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text( hjust = -0.1, size = 18)) +
  ggtitle(expression("PO"[4]^"3-"))


#plot_grid(a,b, nrow = 2, labels = c(" ", " "), rel_heights = c(1,0.9))





tiff(filename = "figures/Type_box_facet_jitter.tif", height=7.5, width=7, units= "in", res=800, compression= "lzw")

plot_grid(a,b, nrow = 2, labels = c(" ", " "), rel_heights = c(1,0.85))

dev.off()








# ##### old versions
{
# 
# 
# x$Catchment_Type <- as.factor(x$Catchment_Type)
# x$group <- ifelse(x$Catchment_Type == "WWTP", "WWT", 
#                   ifelse(x$Wetland_Type == "Mesocosm", "Mesocosm", "Restored/\nConstructed"))
# 
# 
# table(x$group)
# a <- x %>% ggplot(aes(x = group, y = TP_Retention_percent))+
#   geom_boxplot()  +
#   coord_cartesian(ylim = c(-200,100)) +
#   theme_classic()
# 
# b <- x %>% ggplot(aes(x = group, y = SRP_Retention_percent))+
#   geom_boxplot()  +
#   coord_cartesian(ylim = c(-200,100)) +
#   theme_classic()
# 
# plot_grid(a,b, nrow = 1)
# 
# 
# summary <- x %>%
#   group_by(group) %>%
#   summarise(count = n()) %>%
#   mutate(TP_Retention_percent = -180,
#          SRP_Retention_percent = -180)
# 
# sig <- x %>%
#   group_by(group) %>%
#   summarise(count = n()) %>%
#   mutate(TP_Retention_percent = 110,
#          SRP_Retention_percent = 110)
# sig$TP <- c("A", "A", "A" )
# sig$SRP <- c("A", "B", "B")
# 
# 
# pal <- c('#e5a77f', '#3e783f', '#6fabd9')
# 
# a <- x %>% ggplot(aes(x = group, y = TP_Retention_percent, fill = group))+
#   geom_boxplot()  +
#   scale_fill_manual(values = pal) +
#   coord_cartesian(ylim = c(-180,110)) +
#   theme_classic(base_size = 7) +
#   geom_text(data = summary, aes(label = count), 
#             position = position_dodge(width = 1.0), size = 2.5, color = "gray50")+
#   geom_text(data = sig, aes(label = TP), 
#             position = position_dodge(width = 1.0), size = 2.5) +
#   ylab("Retention (%)") +
#   xlab(" ") +
#   theme(plot.margin = unit(c(t = 0.1, r = 0, b = 0, l = 0.5), "cm"),
#         plot.title = element_text( hjust = -0.35, size = 10),
#         legend.position = "none") +
#   ggtitle("TP")
# 
# 
# b <- x %>% ggplot(aes(x = group, y = SRP_Retention_percent, fill = group))+
#   geom_boxplot()  +
#   scale_fill_manual(values = pal) +
#   coord_cartesian(ylim = c(-180,110)) +
#   theme_classic(base_size = 7) +
#   geom_text(data = summary, aes(label = count), 
#             position = position_dodge(width = 1.0), size = 2.5, color = "gray50") +
#   geom_text(data = sig, aes(label = SRP), 
#             position = position_dodge(width = 1.0), size = 2.5) +
#   ylab(" ") +
#   xlab(" ") +
#   theme(plot.margin = unit(c(t = 0.1, r = 0.5, b = 0, l = 0), "cm"),
#         plot.title = element_text( hjust = -0.2, size = 10),
#         legend.position = "none") +
#   ggtitle(expression("PO"[4]^"3-"))
# 
# plot_grid(a,b, nrow = 1, labels = c(" ", " "))
# 
# 
# 
# #### paper version Figure 4
# 
# 
# tiff(filename = "figures/Type_box_new2.tif", height=1.7, width=4, units= "in", res=800, compression= "lzw")
# plot_grid(a,b, nrow = 1, labels = c(" ", " "), label_size = 9)
# dev.off()
# 
# 
# 
# 
# #### by flow type
# x$Water_regime <- as.factor(x$Water_regime)
# table(x$group, x$Water_regime)
# 
# x$WR <- ifelse(x$Water_regime == "continuous, constant", "Continuous\nregulated", 
#                   ifelse(x$Water_regime == "continuous, variable", "Continuous\nunregulated",
#                          ifelse(x$Water_regime == "intermittent, constant", "Intermittent\nregulated",
#                                 ifelse(x$Water_regime == "intermittent, variable", "Intermittent\nunregulated", "other"))))
# 
# table(x$WR)
# table(x$Water_regime)
# 
# y <- x[which(x$WR != "other"),]
# table(y$group, y$Water_regime)
# 
# summary <- y %>%
#   group_by(WR, group) %>%
#   summarise(count = n()) %>%
#   mutate(TP_Retention_percent = -178,
#          SRP_Retention_percent = -178)
# 
# sig <- y %>%
#   group_by(WR, group) %>%
#   summarise(count = n()) %>%
#   mutate(TP_Retention_percent = 115,
#          SRP_Retention_percent = 115)
# sig$TP <- c("A", "A", "B" , "A", "A" , "A", "A" , " ", "B" , " ")
# sig$SRP <- c("A", "A", "B", "A", "AB", "B", "A", " ", "B", " ")
# 
# pal <- c('#e5a77f', '#3e783f', '#6fabd9')
# 
# a <- ggplot(y, aes(x = group, y = TP_Retention_percent, fill = group )) +
#   geom_boxplot(position = position_dodge2(preserve = "single")) +
#   scale_fill_manual(values = pal) +
#   scale_x_discrete(labels = c("M", "R/C", "W")) +
#   coord_cartesian(ylim = c(-180,120)) +
#   facet_grid(.~WR, space = "free") +
#   theme_bw(base_size = 10) +
#   geom_text(data = summary, aes(label = count),
#            position = position_dodge(width = 1.0), size = 2.9, color = "gray50")+
#   geom_text(data = sig, aes(label = TP), 
#             position = position_dodge(width = 1.0), size = 2.5) +
#   ylab("Retention (%)") +
#   xlab(" ") +
#   theme(plot.margin = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), "cm"),
#         legend.position = "top",
#         legend.title = element_blank(),
#         plot.title = element_text( hjust = -0.11, vjust = -10, size = 14)) +
#   ggtitle("TP")
# 
# 
# 
# b <- ggplot(y, aes(x = group, y = SRP_Retention_percent, fill = group )) +
#   geom_boxplot(position = position_dodge2(preserve = "single")) +
#   scale_fill_manual(values = pal) +
#   scale_x_discrete(labels = c("M", "R/C", "W")) +
#   coord_cartesian(ylim = c(-180,120)) +
#   facet_grid(.~WR, space = "free") +
#   theme_bw(base_size = 10) +
#   geom_text(data = summary, aes(label = count),
#             position = position_dodge(width = 1.0), size = 2.9, color = "gray50")+
#   geom_text(data = sig, aes(label = SRP), 
#             position = position_dodge(width = 1.0), size = 2.5) +
#   ylab("Retention (%)") +
#   xlab(" ") +
#   theme(plot.margin = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), "cm"),
#         legend.position = "none",
#         legend.title = element_blank(),
#         plot.title = element_text( hjust = -0.11, size = 14)) +
#   ggtitle(expression("PO"[4]^"3-"))
# 
# 
# plot_grid(a,b, nrow = 2, labels = c(" ", " "), rel_heights = c(1,0.9))
# 
# 
# ##### paper version figure 5
# 
# tiff(filename = "figures/Type_box_facet.tif", height=5, width=6, units= "in", res=800, compression= "lzw")
# 
# plot_grid(a,b, nrow = 2, labels = c(" ", " "), rel_heights = c(1,0.85))
# 
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## older versions
# ggplot(y, aes(x = group, y = TP_Retention_percent, fill = group )) +
#   geom_boxplot(position = position_dodge2(preserve = "single")) +
#   scale_fill_manual(values = pal) +
#   coord_cartesian(ylim = c(-180,110)) +
#   facet_grid(.~WR, space = "free") +
#   theme_bw(base_size = 7) +
#   geom_text(data = summary, aes(label = count),
#             position = position_dodge(width = 1.0), size = 2.5, color = "gray50")+
#   #geom_text(data = sig, aes(label = TP), 
#   #          position = position_dodge(width = 1.0), size = 2.5) +
#   ylab("Retention (%)") +
#   xlab(" ") +
#   theme(plot.margin = unit(c(t = 0.5, r = 0, b = 0, l = 0.5), "cm"),
#         legend.position = "top",
#         legend.title = element_blank()) +
#   ggtitle("TP")
# 
# 
# x$group <- ifelse(x$Catchment_Type == "WWTP", "WWT", 
#                   ifelse(x$Wetland_Type == "Mesocosm", "Meso",
#                          ifelse(x$Wetland_Type == "Constructed", "Const.", "Rest.")))
# 
# summary <- x %>%
#   group_by(group) %>%
#   summarise(count = n()) %>%
#   mutate(TP_Retention_percent = -180,
#          SRP_Retention_percent = -180)
# 
# sig <- x %>%
#   group_by(group) %>%
#   summarise(count = n()) %>%
#   mutate(TP_Retention_percent = 110,
#          SRP_Retention_percent = 110)
# sig$TP <- c("A", "A", "A", "A")
# sig$SRP <- c("A", "B", "AB", "A")
# 
# a <- x %>% ggplot(aes(x = group, y = TP_Retention_percent))+
#   geom_boxplot()  +
#   coord_cartesian(ylim = c(-180,110)) +
#   theme_classic(base_size = 7) +
#   geom_text(data = summary, aes(label = count), 
#             position = position_dodge(width = 1.0), size = 2.5, color = "gray50")+
#   geom_text(data = sig, aes(label = TP), 
#           position = position_dodge(width = 1.0), size = 2.5) +
#   ylab("Retention (%)") +
#   xlab(" ") +
#   theme(plot.margin = unit(c(t = 0.5, r = 0, b = 0, l = 0.5), "cm"))
# 
# 
# b <- x %>% ggplot(aes(x = group, y = SRP_Retention_percent))+
#   geom_boxplot()  +
#   coord_cartesian(ylim = c(-180,110)) +
#   theme_classic(base_size = 7) +
#   geom_text(data = summary, aes(label = count), 
#             position = position_dodge(width = 1.0), size = 2.5, color = "gray50") +
#   geom_text(data = sig, aes(label = SRP), 
#             position = position_dodge(width = 1.0), size = 2.5) +
#   ylab(" ") +
#   xlab(" ") +
#   theme(plot.margin = unit(c(t = 0.5, r = 0.5, b = 0, l = 0), "cm"))
# 
# plot_grid(a,b, nrow = 1, labels = c("TP", "PO4"))
# 
# 
# tiff(filename = "figures/Type_box_new.tif", height=1.7, width=3, units= "in", res=800, compression= "lzw")
# plot_grid(a,b, nrow = 1, labels = c("TP", "PO4"), label_size = 9)
# dev.off()
# 
# 
# 
# 
# ### t.test
# library(rstatix)
# # library(ggpubr)
# # library(stringr)
# 
# x %>%
#   group_by(group) %>%
#   get_summary_stats(SRP_Retention_percent, type = "median_mad")
# pwc <- x %>%
#   pairwise_t_test(TP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# 
# 
# pwc <- x %>%
#   pairwise_t_test(SRP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# 
# 
# 
# y$newgroup <- paste(y$group, y$WR)
# y %>%
#   group_by(newgroup) %>%
#   get_summary_stats(TP_Retention_percent, type = "median_mad")
# pwc <- y %>%
#   pairwise_t_test(SRP_Retention_percent ~  newgroup, p.adjust.method = 'bonferroni')
# pwc
# 
# table(y$newgroup)
# 
# 
# y$newgroup <- paste(y$group, y$WR)
# z <- y[which(y$newgroup != "Restored/\nConstructed Intermittent\nregulated"),]
# z %>%
#   group_by(newgroup) %>%
#   get_summary_stats(TP_Retention_percent, type = "median_mad")
# pwc <- z %>%
#   pairwise_t_test(TP_Retention_percent ~  newgroup, p.adjust.method = 'bonferroni')
# pwc %>% print(n = nrow(.))
# pwc <- z %>%
#   pairwise_t_test(SRP_Retention_percent ~  newgroup, p.adjust.method = 'bonferroni')
# pwc %>% print(n = nrow(.))
# 
# 
# CR <- y[which(y$WR == "Continuous\nregulated"  ),]
# pwc <- CR %>%
#   pairwise_t_test(TP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# pwc <- CR %>%
#   pairwise_t_test(SRP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# 
# 
# CU <- y[which(y$WR == "Continuous\nunregulated"  ),]
# pwc <- CU %>%
#   pairwise_t_test(TP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# pwc <- CU %>%
#   pairwise_t_test(SRP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# 
# 
# IR <- z[which(z$WR == "Intermittent\nregulated"  ),]
# pwc <- IR %>%
#   pairwise_t_test(TP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# pwc <- IR %>%
#   pairwise_t_test(SRP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# 
# IU <- y[which(y$WR == "Intermittent\nunregulated"  ),]
# pwc <- IU %>%
#   pairwise_t_test(TP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# pwc <- IU %>%
#   pairwise_t_test(SRP_Retention_percent ~  group, p.adjust.method = 'bonferroni')
# pwc
# 
# # 
# # a <- x %>% ggplot(aes(x = Wetland_Type, y = TP_Retention_percent))+
# #   geom_boxplot() +
# #   coord_cartesian(ylim = c(-200,100)) +
# #   theme_classic()
# # b <- x %>% ggplot(aes(x = Wetland_Type, y = SRP_Retention_percent))+
# #   geom_boxplot() +
# #   coord_cartesian(ylim = c(-200,100)) +
# #   theme_classic()
# # 
# # c <- x %>% ggplot(aes(x = Catchment_Type, y = TP_Retention_percent))+
# #   geom_boxplot() +
# #   coord_cartesian(ylim = c(-200,100)) +
# #   theme_classic()
# # d <- x %>% ggplot(aes(x = Catchment_Type, y = SRP_Retention_percent))+
# #   geom_boxplot() +
# #   coord_cartesian(ylim = c(-200,100)) +
# #   theme_classic()
# # 
# # x1 <- x[which(x$Water_regime != "n.s."),]
# # e <- x1 %>% ggplot(aes(x = Water_regime, y = TP_Retention_percent))+
# #   geom_boxplot() +
# #   coord_cartesian(ylim = c(-200,100)) +
# #   theme_classic()
# # f <- x1 %>% ggplot(aes(x = Water_regime, y = SRP_Retention_percent))+
# #   geom_boxplot() +
# #   coord_cartesian(ylim = c(-200,100)) +
# #   theme_classic()
# # plot_grid(a,b, c, d, e, f, nrow = 3)
# # 
# # 
# # levels(x$Water_regime)
# 
# 
# 
# 
# 
# 
# 
# 
# 

}






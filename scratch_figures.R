
#' ---
#' title: "Wetland-P-Meta-analysis scratch figures"
#' author: "Emily Ury"
#' date: "Feb 15, 2021"
#' output: github_document
#' ---
#' 
#' Scratch figures
#'   (a) 



library(ggplot2)
library(tidyverse)
library(viridis)
library(gridExtra)


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Data_files/Wetland_P_Analysis/")
x <- read.csv("Wetland_P_Clean.csv", header = T)
head(x)
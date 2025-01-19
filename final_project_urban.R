#Adds Census Data

#Part 1
rm(list=ls())
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
setwd("~/Poli 281")

urb_rur <- read.csv("nhgis.csv")

#We only want urban and rural data, so we remove everything unrelated, and then rename the columns to match
urb_rur <- urb_rur[-c(1, 4:6, 8:9)] 

urb_rur <- setNames(urb_rur, c("year1","state","Urban","Rural"))
##SInce we want to have the proportion of people living in cities compared to rural areas, we just divide to find that proportion
urb_rur$total <- urb_rur$Urban + urb_rur$Rural
urb_rur$propurb <- urb_rur$Urban / urb_rur$total
urb_rur$state<- state.abb[match(urb_rur$state, state.name)]

#Load other datasets

load("~/Poli 281/final_data2.Rdata")
load("~/Poli 281/final_data.Rdata")

#In order to merge the data we have to divide, truncate, and then multiply by 10. (thanks for the help)

data1$year1 <- data1$year / 10
data1$year1 <- trunc(data1$year1)
data1$year1 <- data1$year1 * 10

data1$year1[data1$year1==2020] <- 2010

data2 <- full_join(data1, urb_rur)

data2 <- data2[-c( 16:18)] 
data2 <- data2[-(836:841),]

#Save the data with new name (:
save(data2, urb_rur, file= "~/Poli 281/final_data_urb.Rdata")



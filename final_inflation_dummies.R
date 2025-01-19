rm(list=ls())
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

load("~/Poli 281/final_data.Rdata")

inflation <- read_excel("~/Poli 281//Inflation.xlsx", sheet="hcpi_a")

us_inflation <- subset(inflation, inflation$`Country Code`=="USA")

##This section turns code to long format, then makes it numeric
us_inflation_long <- us_inflation %>%
  pivot_longer(cols=c("1970":"2021"), names_to = "year", values_to = "inflation") 

us_inflation_long <- subset(us_inflation_long, select= c("year", "inflation"))
us_inflation_long$year <- as.numeric(us_inflation_long$year)

data1 <- merge(data, us_inflation_long)

data1 <- data1[-(836:864),]

#Creates and adds dummy variables

data1$pres_r <- ifelse(data1$party_pres== "R", 1, 0)
data1$inc_d <- ifelse(data1$inc_party=="D", 1, 0)
data1$inc_d[is.na(data1$inc_d)] <- 0.5

#Step 4: Save data

save(data1, us_inflation_long, file= "~/Poli 281/final_data2.Rdata")
#Final Regression
rm(list=ls())
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

load("~/Poli 281/final_data2.Rdata")
load("~/Poli 281/final_data.Rdata")
load("~/Poli 281/final_data_urb.Rdata")

##This section creates the fit for our model

fit1 <- lm(dem_senate_vote_share ~ dem_pres_vote_share + propurb + unemp_change * inc_d + unemp_mean  + inc_d + inflation *pres_r + pres_r * approval + pres_r, data=data2)
summary(fit1)

##This section is to subset for just this election season (our predictions)
pred_data <- subset(data2, data2$year==2022)
pred_data$inflation <- 7.7

##Creates new df for our predicted values

pred_2022 <- predict(fit1, newdata= pred_data)

pred_2022 <- data.frame(cbind(pred_2022, pred_data$state))
pred_2022 <- setNames(pred_2022, c("Pred_dem_vote","State"))
pred_2022$Pred_dem_vote <- as.numeric(pred_2022$Pred_dem_vote)

##Graphs of predictions and residuals
resids <- fit1$residuals
p_resid <- hist(resids, breaks = 5)

pred_2022$winner[pred_2022$Pred_dem_vote>.55] <- "Democrat"
pred_2022$winner[pred_2022$Pred_dem_vote<.45] <- "Republican"
pred_2022$winner[pred_2022$Pred_dem_vote>.45 & pred_2022$Pred_dem_vote <.55] <- "Toss Up"

plot <- ggplot(pred_2022, aes(x=State, y= Pred_dem_vote, color=winner)) +
  geom_point()
plot + geom_hline(yintercept = .5)
plot

#Using maps now!!!
library(maps)

statesMap <- map_data("state")
colnames(statesMap)[5] = "State"

pred_2022$State <- state.name[match(pred_2022$State, state.abb)]
pred_2022$State <- tolower(pred_2022$State)

predictionMap <- full_join(statesMap, pred_2022)

plot2 <- ggplot(predictionMap, aes(x = long, y= lat, group= group, fill= Pred_dem_vote)) +
  geom_polygon(color= "black")
plot2

##Cleaning up/reformatting the map 
predictionMap2 <- predictionMap
predictionMap2$winner[predictionMap2$winner=="Republican"] = 0
predictionMap2$winner[predictionMap2$winner=="Democrat"] = 1
predictionMap2$winner[predictionMap2$winner=="Toss Up"] = 0.5

plot3 <- ggplot(predictionMap2, aes(x = long, y= lat, group= group, fill= winner)) +
  geom_polygon(color= "black")
plot3

#Creating confidence intervals
Fitted_CIs <- predict(fit1, interval = "confidence", newdata = pred_data)
cis_2022 <- data.frame(cbind(Fitted_CIs, pred_data$state))
cis_2022 <- setNames(cis_2022, c("Pred_Dem","State"))
cis_2022$Pred_Dem <- as.numeric(cis_2022$Pred_Dem)
cis_2022$lwr <- as.numeric(cis_2022$lwr)
cis_2022$upr <- as.numeric(cis_2022$upr)

plot4 <- ggplot(cis_2022, aes(x= State, y= Pred_Dem)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr, ymax=upr)) +
  geom_hline(yintercept = .5)
 
plot4
## sampling distributions for tossup states
NC_predicted_share <-0.4633865
PA_predicted_share <-0.4786449
VT_predicted_share <-0.5312227

n <- 10000
NC_sample <- c()
PA_sample <- c()
VT_sample <- c()
for(i in 1:n){
  NC_sample[i] = mean(rnorm(300, mean=NC_predicted_share))
}
plot6 <- hist(NC_sample, main = "", xlab = "NC Vote Share", col = "Purple")
plot6
for(i in 1:n){
  PA_sample[i] = mean(rnorm(300, mean=PA_predicted_share))
}
plot7 <-hist(PA_sample, main = "", xlab = "PA Vote Share", col = "Purple")
plot7
for(i in 1:n){
  VT_sample[i] = mean(rnorm(300, mean=VT_predicted_share))
}
plot8 <- hist(VT_sample, main = "", xlab = "VT Vote Share", col = "Purple")
plot8
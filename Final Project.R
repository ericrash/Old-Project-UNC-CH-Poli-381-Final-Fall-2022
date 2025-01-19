##Eric Rash
rm(list = ls())
setwd("~/POLI 281")
cces18 <- read.csv("cces18.csv")
library(ggplot2)
library(dplyr)

##problem 2
##The first line of code creates a table of proportions for the amount of time people waited to vote. The summary line gives the mean and median of the wait times. The last two lines of code create a boolean variable in the data frame where anyone who waits longer than 10 minutes is defined as having a long wait. And anyone who did not respond is defined as NA. 
prop.table(table(cces18$wait))
summary(cces18$wait)
cces18$longwait <- cces18$wait == 3 | cces18$wait == 4 | cces18$wait == 5
cces18$longwait[cces18$wait ==6] <- NA
##problem 3
##The first section takes the data we are looking at and then groups each state together. Then it finds the proportion of people who reported a wait of greater than 10 minutes for each of those states. Then it arranges them in descending order. 
cces18 <- cces18 %>%
  group_by(state) %>%
  mutate(longwait_by_states = mean(longwait, na.rm=TRUE)/length(longwait)) %>% arrange(desc(longwait_by_states))
##This section of code creates a bar graph displaying the proportion of people with long waits by states. 
longwait_bar <- ggplot(cces18, aes(x=state, y=longwait_by_states)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90)) + ylab("Proportion of People that Waited More Than 10 Mins")
longwait_bar
##Problem 4
##The first section takes the data we are looking at and then groups each state together and groups each region together. Then it finds the proportion of people who reported a wait of greater than 10 minutes for each of those states. Then it arranges them in descending order.
cces18 <- cces18 %>%
  group_by(state, region) %>%
  mutate(longwait_by_states = mean(longwait, na.rm=TRUE)/lenth(longwait)) %>%
  arrange(desc(longwait_by_states))
##This section of code creates a bar graph displaying the proportion of people with long waits by states. And finally color codes each region. 
longwait_bar2 <- ggplot(cces18, aes(x=state, y=longwait_by_states, fill = region)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90)) + ylab("Proportion of People that Waited More Than 10 Mins")
longwait_bar2
##Problem 5
##The first four lines of code create a new variable in the data frame, setting every instance "NA" then takes from the previous vote in 2016 and assigns voters either "rep" "dem" or "ind" based on whether they voted from Trump, Clinton or other in the previous election respectively. 
cces18$conserv_vote <- NA
cces18$conserv_vote[cces18$vote2016 == 1] <- "rep" 
cces18$conserv_vote[cces18$vote2016 == 2] <- "dem" 
cces18$conserv_vote[cces18$vote2016 == 3] <- "ind" 
##These lines group the data by party affiliation, and then makes a bar graph using that variable, with the proportion of voters who waited more than 10 minutes as the y axis. 
cces18 <- cces18 %>%
  group_by(conserv_vote) %>%
  mutate(longwait_by_states = mean(longwait, na.rm=TRUE)/length(longwait))
longwait_bar3 <- ggplot(cces18, aes(x=conserv_vote, y=longwait_by_states)) + geom_col() + ylab("Proportion of People that Waited More than 10 mins") + xlab("Party Affilliation")
##Problem 6
##These lines of code create a new variable in the cces18 dataframe called "race_5" which takes from the "race" variable and defines voters based on their response to the survey question on race. 1=white non hispanic, 2=black, 3=hispanic, 4=asian, 5, 6, 7, and 8 are all reclassified as other. 
cces18$race_5 <- NA
cces18$race_5[cces18$race == 1] <- "White Non-Hispanic"
cces18$race_5[cces18$race == 2] <- "Black"
cces18$race_5[cces18$race == 3] <- "Hispanic"
cces18$race_5[cces18$race == 4] <- "Asian"
cces18$race_5[cces18$race == 5] <- "Other"
cces18$race_5[cces18$race == 6] <- "Other"
cces18$race_5[cces18$race == 7] <- "Other"
cces18$race_5[cces18$race == 8] <- "Other"
##These lines of code group the race_5 instances together, and then create a bar graph using the proportion of voters from each race who waited longer than 10 minutes to vote. 
cces18 <- cces18 %>%
  group_by(race_5) %>%
  mutate(longwait_by_states = mean(longwait, na.rm=TRUE)/length(longwait))
longwait_bar4 <- ggplot(cces18, aes(x=race_5, y=longwait_by_states)) +geom_col() + ylab("Proportion of People that Waited more than 10 mins") + xlab("Race")
##problem 7
##These lines of code create a new variable "faminc_4" which puts the data from the "faminc" survey question into 4 categories. Either "low income" for those between 0-29,999 reported income, "lower middle income" for those between 30,000-69,999, "upper middle income" for those between 70,000-149,999, and "upper income" for those greater than 150,000. Then it puts these four categories into a table that displays the proportion of respondants that falls into each category.  
cces18$faminc_4 <- NA
cces18$faminc_4[cces18$faminc == 1 | cces18$faminc==2 | cces18$faminc == 3] <- "low income"
cces18$faminc_4[cces18$faminc == 4 | cces18$faminc==5 | cces18$faminc == 6 | cces18$faminc == 7] <- "lower middle income"
cces18$faminc_4[cces18$faminc == 8 | cces18$faminc==9 | cces18$faminc == 10 | cces18$faminc == 11] <- "upper middle income"
cces18$faminc_4[cces18$faminc == 12 | cces18$faminc==13 | cces18$faminc == 14 | cces18$faminc == 15 | cces18$faminc == 16] <- "high income"
prop.table(table(cces18$faminc_4))
##Problem 8
##These lines of code create a new bar graph using faminc_4 categories and the proportion of people who waited more than 10 minutes to vote.
cces18 <- cces18 %>%
  group_by(faminc_4) %>%
  mutate(longwait_by_states = mean(longwait, na.rm=TRUE)/length(longwait))
longwait_bar5 <- ggplot(cces18, aes(x=faminc_4, y=longwait_by_states)) + geom_col() + xlab("Family Income") + ylab("Proportion of People that Waited more than 10 Mins")
##This line of code creates a proportion table of the number of people by race than reported having each level of income.
prop.table(table(cces18$race_5, cces18$faminc_4),1)
##Problem 9 
##These lines of code create new dataframes for each of the four income categories.
cces18_low <- subset(cces18, cces18$faminc_4=="low income")
cces18_lm <- subset(cces18, cces18$faminc_4=="lower middle income")
cces18_um <- subset(cces18, cces18$faminc_4=="upper middle income")
cces18_high <- subset(cces18, cces18$faminc_4=="high income")
##Creates a bar graph of the proportion of people with long wait times by race using the low income dataframe
cces18_low <- cces18_low %>%
  group_by(race_5) %>%
  mutate(longwait_by_states = mean(longwait, na.rm=TRUE)/length(longwait))
longwait_bar_low <- ggplot(cces18_low, aes(x=race_5, y=longwait_by_states)) +geom_col() + ylab("Proportion of People that Waited more than 10 mins") + xlab("Race")+ ggtitle("Low Income")
##Creates a bar graph of the proportion of people with long wait times by race using the lower middle income dataframe
cces18_lm <- cces18_lm %>%
  group_by(race_5) %>%
  mutate(longwait_by_states = mean(longwait, na.rm=TRUE)/length(longwait))
longwait_bar_lm <- ggplot(cces18_lm, aes(x=race_5, y=longwait_by_states)) +geom_col() + ylab("Proportion of People that Waited more than 10 mins") + xlab("Race")+ ggtitle("Lower Middle Income")
##Creates a bar graph of the proportion of people with long wait times by race using the upper middle income dataframe
cces18_um <- cces18_um %>%
  group_by(race_5) %>%
  mutate(longwait_by_states = mean(longwait, na.rm=TRUE)/length(longwait))
longwait_bar_um <- ggplot(cces18_um, aes(x=race_5, y=longwait_by_states)) +geom_col() + ylab("Proportion of People that Waited more than 10 mins") + xlab("Race")+ ggtitle("Upper Middle Income")
##Creates a bar graph of the proportion of people with long wait times by race using the high income dataframe
cces18_high <- cces18_high %>%
  group_by(race_5) %>%
  mutate(longwait_by_states = mean(longwait, na.rm=TRUE)/length(longwait))
longwait_bar_high <- ggplot(cces18_high, aes(x=race_5, y=longwait_by_states)) +geom_col() + ylab("Proportion of People that Waited more than 10 mins") + xlab("Race") + ggtitle("High Income")
##Problem 10
##The first line of code creates a histogram of the income in each county. The second creates the "density" variable in our data frame which is every counties population divided by the size of the county. The final line takes the density variable and creates a histogram showing how dense every county is. 
ggplot(cces18, aes(x=income_county)) + geom_histogram()
cces18$density <- cces18$county_pop/cces18$land_area
ggplot(cces18, aes(x=density)) + geom_histogram()
##These lines of code create new variables in the cces18 dataframe. Four boolean ones for black, hispanic, asian, and other voters. And two that remove nonresponses from the "wait" and "faminc" variables. 
cces18$black <- cces18$race_5 == "Black"
cces18$hispanic <- cces18$race_5 == "Hispanic"
cces18$asian <- cces18$race_5 == "Asian"
cces18$other <- cces18$race_5 == "Other"
cces18$wait_reg <- cces18$wait
cces18$wait_reg[cces18$wait_reg == 6] <-NA
cces18$faminc_reg <-cces18$faminc
cces18$faminc_reg[cces18$faminc == 97] <-NA
##Problem 11
##These two sections of code show the regression between wait times and different variables from the dataframe. It runs multiple regression on wait times accounting for potential confounders, and race. 
fit1 <- lm(cces18$wait_reg ~ cces18$black + cces18$hispanic + cces18$other + cces18$asian)
summary(fit1)
nobs(fit1)
fit2 <- lm(cces18$wait_reg ~ cces18$black + cces18$hispanic + cces18$other + cces18$asian + cces18$faminc_reg + cces18$density + cces18$income_county)
summary(fit2)
nobs(fit2)
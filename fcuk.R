library(ggplot2)
library(GGally)
library(plyr)
library(dplyr)
library(tidyr)


events <- read.csv("events.csv")
patients <- read.csv("patients.csv")
wb_full <- read.csv("wellbeing_full.csv")
wb_sample <- read.csv("wellbeing_sample.csv")

events <- events %>%
  separate(col = Date, into = c("Day", "Date"), sep = " ")
events$Day <- as.factor(events$Day)
events$Time <- as.numeric(gsub("[[:punct:]]", "", events$Time))

#events$Delay <- as.numeric(gsub(" minutes", "", events$Delay))
#events$Delay <- as.numeric(gsub(" minute", "", events$Delay))
#events$Delay[events$Delay == '< 1 minute'] <- 0

wb_sample$Time <- as.numeric(gsub("[[:punct:]]", "", wb_sample$Time))/10000


events %>% 
  group_by(Customer) %>%
  count(Customer) %>%
  mutate(freq = n/sum(n))

wb_sample %>% 
  group_by(Sleep) %>%
  count(Sleep) %>% 
  mutate(freq = n/sum(n))
  
prop.table(table(wb_sample$Customer, wb_sample$Red),1)


ggplot(data=wb_sample, aes(x=Time)) + geom_density(alpha = 0.3)

ggplot(data=patients, aes(x=Age)) + geom_histogram(bins = 30)

ggpairs(wb_sample[,c("Time","Yellow","Sleep")], 
        lower=list(continuous="smooth", colour="blue"),
        diag=list(continuous="bar", colour="blue"), 
        upper=list(axisLabels='show'))


# Buchanan, Christopher

rm(list = ls()) # Cleans working environment
library(tidyverse)
library(infer)
library(statsr)

# Q1A
acsData <- read.csv("datasets/acs12.csv", stringsAsFactors = TRUE)
summary(acsData)

#Q1B
employedData <- subset(acsData, employment == "employed")
summary(employedData)

#1C
ggplot(employedData, aes(x=hrs_work)) +
  geom_histogram()+
  xlab("hours worked per week") + ylab("Frequency")

#1D
employedData %>%
  summarize(mean_hpw = mean(hrs_work),
            median_hpw = median(hrs_work),
            sd_hpw = sd(hrs_work),
            iqr_hpw = IQR(hrs_work),
  )

#1E
employedData %>%
  count(gender) %>%
  mutate(p = n /sum(n))

#1F
n <- 470
p_hat_male <- 0.5575
crit_value <- 1.96

se <- sqrt((p_hat_male*(1-p_hat_male))/n)

c.i.lower_bound <- p_hat_male - (crit_value*se)
c.i.upper_bound <- p_hat_male + (crit_value*se)

c.i.lower_bound
c.i.upper_bound

#Extra Credit

n*p_hat_male
n*(1-p_hat_male)

#Yes


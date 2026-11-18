# Buchanan, Christopher

rm(list = ls())
library(tidyverse)
library(infer)
library(statsr)

# Part A/B
genderdiscriminationData <- read.csv("datasets/gender_discrimination.csv", stringsAsFactors = TRUE)
summary(genderdiscriminationData)
# The decision determines whether a candidate is promoted or not.

# Part C
femaleData <- subset(genderdiscriminationData, gender == "female")

# Part D
femaleData %>%
  count(decision) %>%
  mutate(p = n /sum(n))

# Part E
inference(y = decision,
          data = femaleData,
          statistic = "proportion",
          type = "ci",
          method = "theoretical",
          success = "promoted",
          conf_level = 0.95)

# Part F
femaleData %>%
  summarize(
    n = n(),
    p_hat = round(mean(decision == "promoted"), 4),
    successes = n * p_hat,
    failures = n * (1 - p_hat)
  )

# Part G
#Yes, because both the number of successes and the number of failures are
#greater than or equal to 10. Therefore, we can use the Central Limit Theorem to
#calculate a 95% confidence interval.

# Part H
#We cannot reject our friend's null hypothesis because 0.50 falls within our
#95% confidence interval. This means that a true promotion rate of 50% is still
#a reasonable possibility based on our sample.



# Buchanan, Christopher

# Clean up the working environment
rm(list = ls())

# Load the package(s).
library(tidyverse)
library(infer)
library(statsr)

# Q1
MandMData <- read.csv("datasets/MandMlist.csv", stringsAsFactors = TRUE)
summary(MandMData)


# Exploratory analysis chi-sq goodness-of-fit test

# Count the number of M&Ms of each color in the sample
mm_summ <- MandMData %>%
  group_by(color) %>%
  summarize(color_n = n())
mm_summ

# Total sample size
n <- sum(mm_summ$color_n)
n

# Plot the data
ggplot(MandMData, aes(x = color)) +
  geom_bar(stat = "count", fill = "firebrick") +
  labs(x = "", y = "Frequency") +
  theme_classic()


# Are conditions met - chi-sq goodness-of-fit

# Condition 1: Independence
# The 55 M&Ms are a random sample and are far less than 10% of all M&Ms
# produced, so we can assume the observations are independent of each other.

# Condition 2: Every category has at least 5 EXPECTED cases
mm_summ <- add_column(mm_summ,
                      expected_p = c(0.24, 0.13, 0.16, 0.20, 0.13, 0.14)) %>%
  mutate(expected = n * expected_p)
mm_summ

# Check that every expected count is at least 5; prints TRUE if so
all(mm_summ$expected >= 5)

# Testing independence and that the expected count is greater than 5 are the two
# required conditions and are both met.


# Statistical test of null hypothesis - chi-sq goodness-of-fit

# Ho: The distribution of M&M colors matches the hypothesized proportions
# Ha: The distribution of M&M colors does NOT match the hypothesized proportions

chisq_test(MandMData,
           response = color,
           p = c("Blue" = 0.24,
                 "Brown" = 0.13,
                 "Green" = 0.16,
                 "Orange" = 0.20,
                 "Red" = 0.13,
                 "Yellow" = 0.14))

# The chi-squared goodness of fit test gives a test statistic of about 4.19
# with 5 degrees of freedom and a p-value of about 0.52. Since the p-value is
# much greater than alpha = 0.05, we fail to reject the null hypothesis.



# Q2

titanicData <- read.csv("datasets/titanic.csv", stringsAsFactors = TRUE)
summary(titanicData)

# The variable "survive" identifies whether or not a passenger dies when it sank.

titanicTable <- table(titanicData$sex, titanicData$survive)
titanicTable

# Mosaic plot of the table
mosaicplot(titanicTable, color = c("darkred", "gold"),
           xlab = "Sex", ylab = "Survived")

# Are conditions met - chi-sq test of independence

# Condition 1: Independence
# Each row is a different passenger, and we assume one passenger's survival
# does not determine another's, so the observations are independent.

# Condition 2: Every cell has at least 5 EXPECTED cases
chisq.test(titanicTable)$expected

# Both conditions are met.


# Statistical test of null hypothesis - chi-sq test of independence

# Ho: Sex and survival are independent.
# Ha: Sex and survival are NOT independent.

chisq_test(titanicData,
           formula = survive ~ sex)

# The p-value is essentially 0, far smaller than alpha = 0.05, so we reject
# the null hypothesis.

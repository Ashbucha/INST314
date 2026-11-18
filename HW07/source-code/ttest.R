# Buchanan, Christopher

# Clean up the working environment
rm(list = ls())
library(tidyverse)
library(infer)
library(statsr)
library(ggpubr)
library(skimr)

# Question 1
spinData <- read.csv("datasets/spin.csv", stringsAsFactors = TRUE)

# Null Hypothesis: The mean angle of the Earth's spin from the historical
# measurements (1460-1570) is equal to the 1738 Paris measurement of 23.4722 degrees.
# Alternate Hypthesis: The mean angle of the Earth's spin from the historical
# measurements (1460-1570) is different from 23.4722 degrees.

t.test(spinData, mu = 23.4722, alternative = "two.sided")

# Results: After running a two-sided t-test, the sample mean was 23.49878
# degrees, t = 3.0301, p = 0.03878. Because p is less than 0.05, we reject the
# null hypothesis. Therefore, we have evidence that the mean of the earlier
# historical measurements (1460-1570) is different from the "modern" 1738
# measurement.
# (95% CI: 23.47443 to 23.52313 degrees.)

# Question 2
yrbssData <- read.csv("datasets/yrbss.csv", stringsAsFactors = TRUE)
skim(yrbssData)

# Num of Observations: 13,583
# Num of Variables: 13
# Categorical: 8
# Num of NA's (weight): 1,004

yrbssData <- yrbssData %>% drop_na(weight)

female <- yrbssData %>%
  filter(gender == "female")
male <- yrbssData %>%
  filter(gender == "male")

female_QQ <- ggplot(data = female, aes(sample = weight)) +
  geom_qq() +
  ggtitle("Female weight")
male_QQ <- ggplot(data = male, aes(sample = weight)) +
  geom_qq() +
  ggtitle("Male weight")

ggarrange(female_QQ, male_QQ)

summ_weight <- yrbssData %>%
  group_by(gender) %>%
  summarize(mean   = mean(weight),
            median = median(weight),
            sd = sd(weight),
            iqr = IQR(weight),
            n = n(),
            min = min(weight),
            max = max(weight))
summ_weight

# Both Q-Q plots mostly follow a straight line through the middle, but bend
# sharply upward which means there are some extreme high weights that skew the
# distributions right, therefore, the data are not perfectly normal. But since
# the sample size is very large, it is fine to use the t-distribution.

# Null hypothesis: There is no difference in the average weight of male and
# female high schoolers.

t_test(x = yrbssData,
       formula = weight ~ gender,
       order = c("female", "male"),
       alternative = "two-sided")

# Results: After running a two-sided t-test, the mean weight was 61.9kg for
# females and 73.6 kg for males, t = -41.5, p < 0.001. Because p is less than
# 0.05, we reject the null hypothesis. Therefore, we have evidence that the
# average weight of male and female high schoolers is different.



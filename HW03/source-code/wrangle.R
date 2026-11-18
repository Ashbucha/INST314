# Buchanan, Christopher
rm(list = ls())
library(tidyverse)

# Question 1A
fastfoodData <- read.csv("datasets/fastfood.csv", stringsAsFactors = TRUE)
summary(fastfoodData)

# Question 1B
dqMcdData <- fastfoodData %>%
  filter(restaurant == "Mcdonalds" | restaurant == "Dairy Queen")

# Question 1C
dqMcdData %>%
  group_by(restaurant) %>%
  summarize(mean_protein = mean(protein),
    median_protein = median(protein),
    sd_protein = sd(protein),
    iqr_protein = IQR(protein),
    min_protein = min(protein),
    max_protein = max(protein)
  )

# Question 1D
ggplot(data = dqMcdData, aes(x = restaurant, y = protein)) +
  geom_boxplot()

# Question 1E
# I belive Mcdonalds has a more skewed distribution for protein because:
# 1. Mcdonalds has several high outliers, including some far above the upper
#    whisker. While Dairy Queen appears to have no outliers.
# 2. The upper whisker for McDonalds extends much farther above the box than the
#    lower whisker extends below it, suggesting a longer right tail.
# 3. The median protein value for McDonalds is not centered within the overall
#    distribution and the extreme high values pull the mean above the median,
#    which is consistent with a right-skewed distribution.

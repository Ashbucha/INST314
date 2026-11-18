#Buchanan, Christopher

#Cleans Workspace and Loads tidyverse
rm(list = ls())
library(tidyverse)

#Question 1A
countriesData <- read.csv("datasets/countries.csv", stringsAsFactors = TRUE)

#Question 1B
summary(countriesData)
#How many countries are missing data for the variable "ecological_footprint_2000"?
#58

#Question 1C
#How many countries in this data set are from Asia, based on the variable "continent"?
#44

#Question 1D
#What kinds of variables are "measles_immunization_oneyearolds" and "fines_for_tobocco_advertising_2014"?
#measles_immunization_oneyearolds is numerical because it shows a numeric summary (min, max, quartiles, median, mean)
#measles_immunization_oneyearolds is continuous because it's a percentage that can take any value between 0 and 100.
#fines_for_tobocco_advertising_2014 is categorical because it's organized with two labels Yes and No, with two NAs.
#fines_for_tobocco_advertising_2014 is not ordinal becayse its two categories don't have a natural order.

#Question 1E
ggplot(countriesData, aes(x=continent, y=measles_immunization_oneyearolds)) +
  geom_boxplot() +
  xlab("Continent") + ylab("Measles immunization of one-year-olds") +
  theme_minimal()

#Based on this plot which continent has the most symmetrical distribution?
#I think it's South America

#What specific elements of that continent's box plot indicate symmetry?
#It has no outliers, the median is almost center, and the whiskers are about the same length.

#Question 1F
ggplot(countriesData, aes(x=ecological_footprint_2000)) +
  geom_histogram() + xlab("Ecological footprint (2000)") + ylab("Number of countries") +
  theme_minimal()

#The districution is right-skewed.

#Question 1G
#Looking at the summary() from above, we can see that the mean (3.147) is greater than the median (2.140).
#This shows that the distribution is right-skewed because the high values pull the mean up.




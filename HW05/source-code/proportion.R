# Buchanan, Christopher

# Clean up the working environment
rm(list = ls())

# Load the package(s).
library(tidyverse)
library(infer)
library(statsr)

# Question 1A
install.packages("openintro")
stentData <- openintro::stent365

# Question 1B
summary(stentData)
# The treatment group is the group that recieved the brain stent.

# Question 1C
# The alternate hypothesis, Ha, is that stroke proportion is different for control and treatment
# The null hypothesis, Ho, is that stroke proportion is the same for control and treatment

# Or in symbols
# Ha: p_treatment notEqual to p_control
# Ho: p_treatment = p_control

# Question 1D
# Calculate the proportion of strokes in each group
stentData %>%
  group_by(group) %>%
  count(outcome) %>%
  mutate(p = n /sum(n))

# For our question, we are counting "stroke" as our success.
p_hat_pooled <- (45+28)/(45+179+28+199)
p_hat_pooled

# First test the success-failure condition for the treatment group
n_treatment <- 45+179
n_treatment
n_treatment*p_hat_pooled
n_treatment*(1-p_hat_pooled)

# Second test the success-failure condition for the control group
n_control <- 28+199
n_control
n_control*p_hat_pooled
n_control*(1-p_hat_pooled)

# Since all values are above 10, we have met the success-failure condition.

# Question 1E
prop_test(stentData,
          formula = outcome ~ group,
          order = c("treatment","control"),
          success = "stroke",
          z = TRUE)

# Question 1F
# 95% CI for the difference in stroke proportion (treatment - control): (0.00985, 0.145)
# p-value = 0.0254
# Since 0.0254 < 0.05, we reject the null hypothesis
# In conclusion the stroke proportion is different between the treatment and control
# groups. The CI doesn't include 0, and the treatment group had more strokes.

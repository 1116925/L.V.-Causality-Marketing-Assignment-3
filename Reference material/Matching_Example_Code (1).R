# Import data set (may need to adjust to your folder)

df <- read.csv("Matching_example_data.csv", sep=";")


###############################################
##########   Regression models   ##############

# Linear regression without covariates
lm.model1 <- lm(Sales ~ Discount, data = df)
summary(lm.model1)

# Linear regression with covariates
lm.model2 <- lm(Sales ~ Discount + Age + Prev_Purch + Loyalty_card, data = df)
summary(lm.model2)


###############################################
##########    Matching models    ##############

# Use the MatchIt package
library(MatchIt)

# Use different methods for matching:
# Examples here Euclidean, Mahalanobis, Exact, PSM with and without replacement

###############################################
# Euclidean
psm.out <- matchit(Discount ~ Age + Prev_Purch + Loyalty_card, data = df, 
                   method = "nearest", 
                   distance = "euclidean", 
                   ratio = 1, 
                   replace = TRUE, 
                   normalize = FALSE) # typically we should normalize
psm.out

summary(psm.out)
plot(psm.out, type = "density", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)
plot(psm.out, type = "ecdf", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)

# Extract the matched data for estimation
m.data <- match.data(psm.out)

# Estimate linear model on matched data
lm.match <- lm(Sales ~ Discount, data = m.data, weights = weights)
summary(lm.match)

# Normalization:
# Imagine we have 100 treated units and you use 1:1 matching with replacement.
# Because of replacement, maybe the algorithm only needed to use 50 unique control units to match all 100 treated units.
# This means the average control unit was used 2 times (100 matches / 50 unique controls = 2).
# To normalize the weights, MatchIt divides every raw count by 2.
# If Control unit 1 was a popular match and got picked 4 times, its final matched weight is 4 / 2 = 2.0
# If Control unit 2 was only picked 1 time, its final matched weight is 1 / 2 = 0.5
# It ensures that the sum of the control weights equals the total number of uniquely matched control units.
# In essence it affects the sample size of the matched control group

###############################################
# Mahalanobis
psm.out <- matchit(Discount ~ Age + Prev_Purch + Loyalty_card, data = df, 
                   method = "nearest", 
                   distance = "mahalanobis", 
                   replace = TRUE)
psm.out

summary(psm.out)
plot(psm.out, type = "density", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)
plot(psm.out, type = "ecdf", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)

# Extract the matched data for estimation
m.data <- match.data(psm.out)

# Estimate linear model on matched data
lm.match <- lm(Sales ~ Discount, data = m.data, weights = weights)
summary(lm.match)

###############################################
# Exact matching
psm.out <- matchit(Discount ~ Age + Prev_Purch + Loyalty_card, data = df, 
                   method = "exact")
psm.out

summary(psm.out)
plot(psm.out, type = "density", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)
plot(psm.out, type = "ecdf", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)

# Extract the matched data for estimation
m.data <- match.data(psm.out)

# Estimate linear model on matched data
lm.match <- lm(Sales ~ Discount, data = m.data, weights = weights)
summary(lm.match)

###############################################
# PSM without replacement
psm.out <- matchit(Discount ~ Age + Prev_Purch + Loyalty_card, data = df, 
                   method = "nearest", 
                   distance = "glm", 
                   replace = FALSE)
psm.out

summary(psm.out)
plot(psm.out, type = "density", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)
plot(psm.out, type = "ecdf", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)
plot(psm.out, type = "histogram", interactive = FALSE) # Only available for propensity score matching
plot(psm.out, type = "jitter", interactive = FALSE) # Only available for propensity score matching

# Extract the matched data for estimation
m.data <- match.data(psm.out)

# Estimate linear model on matched data
lm.match <- lm(Sales ~ Discount, data = m.data, weights = weights)
summary(lm.match)

###############################################
# PSM without replacement - with pruning
psm.out <- matchit(Discount ~ Age + Prev_Purch + Loyalty_card, data = df, 
                   method = "nearest", 
                   distance = "glm", 
                   replace = FALSE,
                   caliper = 0.2)
psm.out

summary(psm.out)
plot(psm.out, type = "density", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)
plot(psm.out, type = "ecdf", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)
plot(psm.out, type = "histogram", interactive = FALSE) # Only available for propensity score matching
plot(psm.out, type = "jitter", interactive = FALSE) # Only available for propensity score matching

# Extract the matched data for estimation
m.data <- match.data(psm.out)

# Estimate linear model on matched data
lm.match <- lm(Sales ~ Discount, data = m.data, weights = weights)
summary(lm.match)

###############################################
# PSM with replacement
psm.out <- matchit(Discount ~ Age + Prev_Purch + Loyalty_card, data = df, 
                   method = "nearest", 
                   distance = "glm", 
                   replace = TRUE)
psm.out

summary(psm.out)
plot(psm.out, interactive = FALSE)
plot(psm.out, type = "density", interactive = FALSE, which.xs = ~ Age + Prev_Purch + Loyalty_card)
plot(psm.out, type = "histogram", interactive = FALSE)  # Only available for propensity score matching
plot(psm.out, type = "jitter", interactive = FALSE) # Only available for propensity score matching

# Extract the matched data for estimation
m.data <- match.data(psm.out)

# Estimate linear model on matched data
lm.match <- lm(Sales ~ Discount, data = m.data, weights = weights)
summary(lm.match)

#### Investigate the different matching results! 
#### what do you observe? Any preference for these results??


# What would you think is the true ATT???
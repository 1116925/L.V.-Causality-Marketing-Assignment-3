set.seed(42) # For reproducibility

####################################################################
# GENERATE THE POPULATION (N = 10,000 potential impressions)
n <- 10000

# Generate independent variables
# Buying intent: Normal distribution (0 = avg interest)
intent <- rnorm(n, mean = 0, sd = 1) 

# Ad creativity: high vs. low
creative <- sample(c(0, 1), size=n, replace=TRUE)
####################################################################

# Ad creativity is not related to buying intent
summary(lm(intent~creative))

####################################################################
# THE COLLIDER MECHANISM
# Users click the higher the intent and the higher the creativity
# We use a logistic function to simulate probability
prob_click <- plogis(2 * intent + 2 * creative - 1)
click <- rbinom(n, 1, prob_click)

# Create a dataframe with all customers
df_all <- data.frame(intent, creative, click)

# Ad creativity is related to buying intent when controlling for clicks
summary(lm(intent~creative+click, data=df_all))

# What we do here is creating (reverse) selection bias
# We select all people that click
df_click <- df_all[df_all$click == 1, ]

# For people who click, we observe a negative relation between intent and creative quality
summary(lm(intent~creative, data=df_click))

####################################################################
####################################################################

# ADDING CONVERSION RATE
# Buying depends entirely on intent
# (A catchy ad gets you to click, but you only buy if you actually want the product)
prob_buy <- plogis(2 * intent - 3) # -3 makes buying harder than clicking (i.e., only few buy)
conversion <- rbinom(n, 1, prob_buy)

# However, you can't buy if you don't click:
df_all$conversion <- ifelse(click == 1, conversion, 0)
df_click <- df_all[df_all$click == 1, ]


# This simulates real life where you can't measure 'Intent'
# We regress conversion against just the ad creative
model_full <- glm(conversion ~ creative, data = df_all, family = "binomial")
print(summary(model_full)$coefficients)

# What happens if we control for clicks?
model_full <- glm(conversion ~ creative+click, data = df_all, family = "binomial")
print(summary(model_full)$coefficients)

# What if we would know the buying intent?
model_full <- glm(conversion ~ intent+creative+click, data = df_all, family = "binomial")
print(summary(model_full)$coefficients)
model_full <- glm(conversion ~ intent+creative, data = df_all, family = "binomial")
print(summary(model_full)$coefficients)


# Again, controlling for clicks is like selecting only those who click
# We again select all people that click
df_click <- df_all[df_all$click == 1, ]

# This provides the same results as above when controlling for clicks
model_full <- glm(conversion ~ creative, data = df_click, family = "binomial")
print(summary(model_full)$coefficients)

# Again, we would need to know intent to get the right results
model_full <- glm(conversion ~ intent+creative, data = df_click, family = "binomial")
print(summary(model_full)$coefficients)

# Marketeers often analyze only those on the website and where they come from
# The find that coming from high creative ads lowers purchases (but this does not imply a causal effect)
# It is because we have a lot of low interest people that click on the creative ad, but not want to buy
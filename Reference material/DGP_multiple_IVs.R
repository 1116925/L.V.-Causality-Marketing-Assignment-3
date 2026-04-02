library(AER)

set.seed(1427)
n <- 1000

# ---------------------------------------------------------
# SETUP: Core variables
# ---------------------------------------------------------
# Unobserved factors
u <- rnorm(n, 0, 5) # Pure random error
v1 <- rnorm(n) # Unobserved factor causing endogeneity
v2 <- rnorm(n) # Unobserved factor causing endogeneity

# Instruments
z1 <- rnorm(n)
z2 <- rnorm(n)

# Exogenous covariates
w1 <- rnorm(n)
w2 <- rnorm(n)

# -----------------------------------------------------------
# SCENARIO 1: One endogenous + two instruments both valid
# -----------------------------------------------------------

df1 <- data.frame(cbind(u,v1,z1,z2))

# Endogenous variables (influenced by v)
df1$x <- 1 + 0.9*z1 + 0.5*z2 + 3*v1 + rnorm(n, 0, 3)

# The outcome
# Depends on x1 and unobserved v1
df1$y <- 2 - 2*df1$x + 3*v1 + u

# OLS - Short regression (endogeneity bias)
lm1.short <- lm(y~x, data=df1)
summary(lm1.short)

# Manual approach - First stage
lm1.firststage <- lm(x~z1+z2, data=df1)
summary(lm1.firststage)

df1$xhat <- predict(lm1.firststage)

# Manual approach - Second stage
lm1.secondstage <- lm(y~xhat, data=df1)
summary(lm1.secondstage)

# Run IV regression and check diagnostics
model1 <- ivreg(y ~ x | z1 + z2, data = df1)
summary(model1, diagnostics = TRUE)


# ------------------------------------------------------------
# SCENARIO 2: One endogenous + two instruments one invalid
# ------------------------------------------------------------

rm(df1)

df2 <- data.frame(cbind(u,v1,z1,z2))

# Endogenous variables (influenced by v)
df2$x <- 1 + 0.9*z1 + 0.5*z2 + 3*v1 + rnorm(n, 0, 3)

# The outcome
# Depends on x1 and unobserved v1
# Also depends on z2 (invalid instrument)
df2$y <- 2 - 2*df2$x + 3*v1 + u + 2*z2

# OLS - Short regression (endogeneity bias)
lm2.short <- lm(y~x, data=df2)
summary(lm2.short)

# Manual approach - First stage
lm2.firststage <- lm(x~z1+z2, data=df2)
summary(lm2.firststage)

df2$xhat <- predict(lm2.firststage)

# Manual approach - Second stage
lm2.secondstage <- lm(y~xhat, data=df2)
summary(lm2.secondstage)

# Run IV regression and check diagnostics
model2 <- ivreg(y ~ x | z1 + z2, data = df2)
summary(model2, diagnostics = TRUE)


# -----------------------------------------------------------------------------
# SCENARIO 3: Two endogenous + two (valid) instruments + exogenous covariates
# -----------------------------------------------------------------------------

df3 <- data.frame(cbind(u,v1,z1,z2,v2,w1,w2))

# Endogenous variables (influenced by v)
df3$x1 <- 1 + 1.8*z1 + 0.5*z2 + 3*v1 + rnorm(n, 0, 3)
df3$x2 <- 1 + 0.8*z1 + 1.4*z2 + 1.5*v2 + rnorm(n, 0, 3)

# The true relationship: Y = 2 + 3*X1 - 2*X2 + 2*v + error
df3$y <- 2 + 2*df3$x1 - 1.3*df3$x2 + 3*v1 + w1 - w2 + 4*v2 + 2*u

# OLS - Short regression (endogeneity bias)
lm3.short <- lm(y~x1+x2+w1+w2, data=df3)
summary(lm3.short)

# Manual approach - First stage
lm3.firststage.x1 <- lm(x1~w1+w2+z1+z2, data=df3)
lm3.firststage.x2 <- lm(x2~w1+w2+z1+z2, data=df3)

summary(lm3.firststage.x1)
summary(lm3.firststage.x2)

df3$x1hat <- predict(lm3.firststage.x1)
df3$x2hat <- predict(lm3.firststage.x2)

# Manual approach - Second stage
lm3.secondstage <- lm(y~x1hat+x2hat+w1+w2, data=df3)
summary(lm3.secondstage)

# Run IV regression and check diagnostics
model3 <- ivreg(y ~ x1 + x2 + w1 + w2 | w1 + w2 + z1 + z2, data = df3)
summary(model3, diagnostics = TRUE)

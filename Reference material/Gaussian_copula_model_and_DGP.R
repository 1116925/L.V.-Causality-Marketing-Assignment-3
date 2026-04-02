set.seed(5682)

rho=0.5 # Error correlation of 0.5

# Generate data from a multivariate normal distribution with pre-specified correlation matrix sigma
Sigma <- matrix(c(1,rho,rho,1),2,2)
df <- data.frame(MASS::mvrnorm(600,rep(0,2),Sigma))
colnames(df) <- c("Epsilon_star","X_star")

# Epsilon is supposed to be normally distributed so epsilon=epsilon_star
df$Epsilon <- df$Epsilon_star  

# X must be non-normally distributed, here we use Chi-Square distributed with df=3
# pnorm gives the univariate marginal distribution for X_star and then we transform using the inverse chi-square
df$X <- qchisq(pnorm(df$X_star), df = 3)

# Show the distribution of X
plot(density(df$X))

# Equation with specified parameters
df$Y <- 2 - 1*df$X + df$Epsilon

# Uncorrected OLS model
lm.endo <- lm(Y~X, data = df)
summary(lm.endo)

#############################
# Gaussian copula model
#############################

# Copula transformation function using the empirical cumulative distribution function (ECDF)
get_copula_transform <- function(x){  
  n <- length(x)  
  U <- 1/(2*n) + (n-1)/(n)*ecdf(x)(x)
  qnorm(U)
}
df$Copula_x <- get_copula_transform(df$X)

# Add copula term to the regression model
lm.cop <- lm(Y ~ X + Copula_x, data=df)
summary(lm.cop)

# Additional plots to show the distribution of the marginal (uniform) and the copula-term (normal)
plot(density(ecdf(df$X)(df$X))) # uniform
plot(density(df$Copula_x)) # normal
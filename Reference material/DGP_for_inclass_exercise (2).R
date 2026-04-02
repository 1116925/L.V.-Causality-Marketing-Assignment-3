
# Set the seed for reproducibility
set.seed(123)

# Generate z and v as two independent random variables
z <- rnorm(400, 0, 5) # our instrument
v <- rnorm(400, 0, 5) # a confounder for the X-Y relationship

# Generate x that is related to both z and v plus some independent (random) error
x <- 0.5*z + v + rnorm(400, 0, 5)

# Generate y that is related to both x and v plus some independent (random) error component
y <- 50 + x + v + rnorm(400, 0, 5)

# We delete v, because it is an unobserved confounder
rm(v)

df <- data.frame(cbind(y,x,z))

write.table(df, "IV_example.csv", sep = ",", row.names = FALSE)

#########################################################
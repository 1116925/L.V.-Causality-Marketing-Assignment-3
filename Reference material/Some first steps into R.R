# =============================================================================
# Introduction to Linear Regression in R
# =============================================================================

# First, install sjPlot if you haven't already (run once)
# install.packages("sjPlot")

# Load required library
library(sjPlot)

# =============================================================================
# Step 1: Load Data from CSV
# =============================================================================

# Option A: Load from a file path
# data <- read.csv("path/to/your/data.csv")

# Option B: Let user select file interactively
# data <- read.csv(file.choose())

# For this demo, we will first create sample data and save/reload it afterwards
# This simulates what you would do with real data

sample_data <- data.frame(
  hours_studied = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  sleep_hours   = c(4, 5, 6, 7, 8, 5, 6, 7, 8, 9),
  exam_score    = c(52, 58, 65, 70, 75, 68, 78, 82, 88, 95)
)

# Save to CSV (so you can see the format)
write.csv(sample_data, "student_data.csv", row.names = FALSE)

# Now load it back (this is what you'd normally do)
data <- read.csv("student_data.csv")

# =============================================================================
# Step 2: Explore Your Data
# =============================================================================

# View first few rows
head(data)

# Summary statistics
summary(data)

# Check structure
str(data)

# =============================================================================
# Step 3: Run Linear Regression and store results in an object
# =============================================================================

# Simple regression: exam_score predicted by hours_studied
model_simple <- lm(exam_score ~ hours_studied, data = sample_data)

# Multiple regression: exam_score predicted by hours_studied AND sleep_hours
model_multiple <- lm(exam_score ~ hours_studied + sleep_hours, data = sample_data)

# =============================================================================
# Step 4: View Results with summary()
# This is the standard summary function for regression objects that prints well
# on the console
# =============================================================================

cat("\n========== SIMPLE REGRESSION RESULTS ==========\n")
summary(model_simple)

cat("\n========== MULTIPLE REGRESSION RESULTS ==========\n")
summary(model_multiple)

# =============================================================================
# Step 5: View Results with sjPlot's tab_model()
# I will use tab_model a lot for printing regression results, because it gives
# a much nicer to read and more structured output than summary, especially when
# printing in a markdown format
# =============================================================================

# Single model - creates a nicely formatted table
tab_model(model_simple)

# Compare multiple models side by side
tab_model(model_simple, model_multiple,
          show.ci = FALSE,
          p.style = "stars",
          dv.labels = c("Simple Model", "Multiple Model"),
          title = "Regression Results: Predicting Exam Scores")

# Save the table to an HTML file
tab_model(model_multiple,
          file = "regression_results.html",
          title = "Exam Score Regression Results")

cat("\nTable saved to 'regression_results.html'\n")
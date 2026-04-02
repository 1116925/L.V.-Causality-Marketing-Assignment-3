###################################################################################################################
# Please install the following packages to be able to execute the provided R qmd files and to do you own analysis
#
# Additional packages may be necessary as we progress, but this is a solid first start with most of the 
# packages required for the course
###################################################################################################################

# Some basic packages for nice plotting and data handling - needed throughout the course (required for session 3)
install.packages(c("sjPlot", "kableExtra", "tidyverse", "broom"))

# Package(s) for plotting and analyzing DAGs (required for session 3)
install.packages("dagitty")

# Package(s) for parallel processing of the simulations (required for session 3)
install.packages(c("foreach", "doParallel"))

# Package(s) for analysis of experiments
install.packages(c("car", "modelsummary", "emmeans", "lme4", "lmerTest"))

# Package(s) for instrumental variable regression (session 7)
install.packages("AER")

# Package(s) for matching (session 9)
install.packages("MatchIt")

# Package(s) for the Double ML method (session 10)
install.packages(c("DoubleML","mlr3","mlr3learners"))

# Package(s) for difference and difference and synthetic control methods (sessions 11-13)
install.packages(c("plm", "Synth", "synthdid"))



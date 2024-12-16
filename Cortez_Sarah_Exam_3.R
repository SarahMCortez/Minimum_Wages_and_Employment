##################################################
# ECON 418-518 Exam 3
# Sarah Cortez
# The University of Arizona
# scorte7056@arizona.edu 
# 15 December 2024
###################################################


#####################
# Preliminaries
#####################

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# Load packages
pacman::p_load(data.table)

# Set sead
set.seed(418518)

#Load data set
setwd("Users/Sarah Cortez/Downloads")
dt<- fread("Exam3Data.csv")


#################
# Question (ii)
#################

#Code

# Create the indicator columns
dt[, is_nov := ifelse(time_period == "Nov", 1, 0)]
dt[, is_nj := ifelse(state == 1, 1, 0)]

# Calculate mean total employment grouped by state and time period
mean_employment <- dt[, .(Mean_Total_Employment = mean(total_emp, na.rm = TRUE)), 
                        by = .(state, time_period)]

# Print the results
print(mean_employment)


##############
# Part (iii)
##############

# Code

#  Compute sample means for each group and time period
group_means <- dt[, .(Mean_Total_Employment = mean(total_emp, na.rm = TRUE)), 
                    by = .(state, time_period)]

# Calculate the DiD estimator
# Extract means
mean_pa_pre <- group_means[state == 0 & time_period == "Feb", Mean_Total_Employment]
mean_pa_post <- group_means[state == 0 & time_period == "Nov", Mean_Total_Employment]
mean_nj_pre <- group_means[state == 1 & time_period == "Feb", Mean_Total_Employment]
mean_nj_post <- group_means[state == 1 & time_period == "Nov", Mean_Total_Employment]

# Compute differences
diff_pa <- mean_pa_post - mean_pa_pre  # Change in Pennsylvania (control group)
diff_nj <- mean_nj_post - mean_nj_pre  # Change in New Jersey (treatment group)

# DiD estimator
did_estimator <- diff_nj - diff_pa

# Print results
cat("Difference in Employment (PA):", diff_pa, "\n")
cat("Difference in Employment (NJ):", diff_nj, "\n")
cat("DiD Estimator:", did_estimator, "\n")


##############
# Part (iv)
##############

# Code

# Estimate the DiD model
model <- lm(total_emp ~ state * is_nov, data = dt)


#  Summarize the model
summary(model)

#  Extract the ATT (coefficient of interaction term state:is_nov)
att <- coef(model)["state:is_nov"]

#  Calculate the 95% confidence interval 
std_error <- summary(model)$coefficients["state:is_nov", "Std. Error"]
lower_bound <- att - 1.96 * std_error
upper_bound <- att + 1.96 * std_error

cat("ATT:", att, "\n")
cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")


#################
# Question (vii)
#################

# Code

#add restaurant fixed effects
model_fe <- lm(total_emp ~ state * is_nov + factor(restaurant_id), data = dt)

# SUmmarize the model
summary(model_fe)


#  Extract the ATT (coefficient of interaction term state:is_nov)
att <- coef(model_fe)["state:is_nov"]


cat("ATT:", att, "\n")








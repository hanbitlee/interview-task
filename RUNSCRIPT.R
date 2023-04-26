install.packages("here")
library(testthat)
library(here) # for easy and clear relative paths
library(MASS)
library(ggplot2)
library(nortest)
library(Matching)
library(fitdistrplus)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set work directory to this file
here::i_am("TaskReadme.txt")

source("utils.R") # common utility functions for this repository
source("calculator_utils.R") # specific model utility functions

# FIRST TASK
IAM.OUTPUT.SHORT.FILENAME <-"SHAPE-final"
source("calculator_output_lognormal-calculation.R")
write.csv(scenario.assessment.calculated, "scenario_assessment_calculated.csv", row.names = FALSE)
saveRDS(scenario.assessment.calculated, "scenario_assessment_calculated.RData")

# CODE TESTS
testthat::test_dir("tests/testthat")

# Extract energy per capita data from scenario.assessment.data
data <- scenario.assessment.data$energy.per.capita

# Fit log-normal and Weibull distributions to data
fit_ln <- fitdist(data, "lnorm")
fit_wb <- fitdist(data, "weibull")

# Plot histogram of data with log-normal and Weibull density curves overlaid
ggplot(data.frame(x = data), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dlnorm, args = list(meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]), color = "red") +
  labs(x = "Energy per capita", y = "Density") +
  ggtitle("Histogram and density curves for Lognormal distribution")

ggplot(data.frame(x = data), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dweibull, args = list(shape = fit_wb$estimate[1], scale = fit_wb$estimate[2]), color = "blue") +
  labs(x = "Energy per capita", y = "Density") +
  ggtitle("Histogram and density curves for Weibull distribution")

# Create log-normal and Weibull QQ plots
sorted_data <- sort(data)
n <- length(data)
quantiles <- (1:n - 0.5) / n
lnorm_quantiles <- qlnorm(quantiles, meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2])
weibull_quantiles <- qweibull(quantiles, shape = fit_wb$estimate[1], scale = fit_wb$estimate[2])

plot(lnorm_quantiles, sorted_data, main = "Lognormal QQ-Plot", xlab = "Theoretical Quantiles", ylab = "Observed Data")
abline(0, 1, col = "red")  # Add a 45-degree reference line
plot(weibull_quantiles, sorted_data, main = "Weibull QQ-Plot", xlab = "Theoretical Quantiles", ylab = "Observed Data")
abline(0, 1, col = "blue")  # Add a 45-degree reference line

# Perform KS test for both distributions
lognorm_cdf <- function(x, meanlog, sdlog) {
  plnorm(x, meanlog = meanlog, sdlog = sdlog)
}
weibull_cdf <- function(x, shape, scale) {
  pweibull(x, shape = shape, scale = scale)
}
data_unique <- unique(data)
ks_test_ln <- ks.test(data_unique, "lognorm_cdf", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2])
print(ks_test_ln)
ks_test_wb <- ks.test(data_unique, "weibull_cdf", shape = fit_wb$estimate[1], scale = fit_wb$estimate[2])
print(ks_test_wb)

# Check rename_remind scenarios
# Check unique scenarios
df = readRDS(
  here(
    "data",
    paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")
  )
)

unique_scenarios <- unique(df$scenario)
print(unique_scenarios)

# Count the number of unique scenarios
num_unique_scenarios <- length(unique_scenarios)
cat("Number of unique scenarios:", num_unique_scenarios)

# Check for NA values
na_count <- sum(is.na(df$scenario))
cat("Number of NA values:", na_count)

rename_remind_scenarios(df)
# Count the number of unique scenarios
num_unique_scenarios <- length(unique_scenarios)
cat("Number of unique scenarios:", num_unique_scenarios)
print(unique_scenarios)


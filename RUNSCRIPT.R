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

# example data
data <- scenario.assessment.data$energy.per.capita

# plot histogram and lognormal PDF
ggplot(data.frame(x = data), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dlnorm, args = list(meanlog = mean(log(data)), sdlog = sd(log(data))), color = "red") +
  labs(x = "Data", y = "Density") +
  ggtitle("Histogram and lognormal PDF")

#Shapiro-Wilk Test
sampled_data <-sample(log(data), size=4999)
shapiro.test(sampled_data)
alpha <- 0.05
if (shapiro.test(sampled_data)$p.value > alpha) {
  print('Logarithmically transformed data follows a normal distribution (fail to reject H0)')
} else {
  print('Logarithmically transformed data does not follow a normal distribution (reject H0)')
}

# create Q-Q plot
qqnorm(log(data))
qqline(log(data), col = "red")

# estimate lognormal parameters using fitdistr
fit <- fitdistr(data, "lognormal")
fit$estimate

# compare estimated parameters to input parameters
c(meanlog = mean(log(data)), sdlog = sd(log(data)))

# Estimate the Weibull distribution parameters and plot graph
ggplot(data.frame(x = data), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dweibull, args = list(shape = fit$estimate[1], scale = exp(fit$estimate[2])), color = "red") +
  labs(x = "Data", y = "Density") +
  ggtitle("Histogram and Weibull PDF")

fit <- fitdist(data, "weibull")
# Define a function to generate the CDF of a Weibull distribution
weibull_cdf <- function(x, shape, scale) {
  pweibull(x, shape, scale)
}

# Perform bootstrap version of the Kolmogorov-Smirnov test
n_replicates <- 1000
ks_stats <- numeric(n_replicates)
for (i in 1:n_replicates) {
  sampled_data <- sample(data, replace = TRUE)
  ks_stats[i] <- ks.test(sampled_data, weibull_cdf, shape = fit$estimate[1], scale = fit$estimate[2])$statistic
}
p_value <- mean(ks_stats)
conf_int <- quantile(ks_stats, c(0.025, 0.975))
# Print results
print("Kolmogorov-Smirnov test results:\n")
print(paste0("p-value: ", round(p_value, 4), "\n"))
print(paste0("95% confidence interval: [", round(conf_int[1], 4), ", ", round(conf_int[2], 4), "]\n"))

# Check rename_remind scenarios
# Check unique scenarios
unique_scenarios <- unique(df_input$scenario)
print(unique_scenarios)

# Count the number of unique scenarios
num_unique_scenarios <- length(unique_scenarios)
cat("Number of unique scenarios:", num_unique_scenarios, "\n")

# Check for NA values
na_count <- sum(is.na(df_result$scenario))
cat("Number of NA values:", na_count, "\n")
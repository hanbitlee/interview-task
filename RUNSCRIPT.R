install.packages("here")
library(here) # for easy and clear relative paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set work directory to this file
here::i_am("TaskReadme.txt")

source("utils.R") # common utility functions for this repository
source("calculator_utils.R") # specific model utility functions

#' WRITE YOUR CODE BELOW
IAM.OUTPUT.SHORT.FILENAME <-"SHAPE-final"
source("calculator_output_lognormal-calculation.R")
write.csv(scenario.assessment.calculated, "scenario_assessment_calculated.csv", row.names = FALSE)
saveRDS(scenario.assessment.calculated, "scenario_assessment_calculated.RData")

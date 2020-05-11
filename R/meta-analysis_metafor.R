runMetaAnalysis <- function(jsonData) {

library(tidyverse)
library(metafor)
library(jsonlite)
  
df <- fromJSON(jsonData)

# We'll need to port in the data from javascript somehow, but it will need to be formatted something like this.
# These are just some made up numbers with the required structure.
data <- data.frame(
  # required fields:
  study_id = c(df$studyId),  #c("a", "b", "c", "d"),                   
  standardized_effect_size = c(df$effectSize), #c(1.9, 2.5, 1.5, -0.1),
  std_err = c(df$stdError), #c(.8, 1.5, 0.6, 1.9),                 
  # optional fields:    
  outcome = c("test_scores", "test_scores", "test_scores", "admissions"), # different measurements
  grouping1 = c("private_school", "private_school", "public_school", "private_school") # user-defined groupings 
  # may need to allow for multiple group columns...
  # grouping2
  # grouping3
)

# Build up model specification based on data provided and constraints:
# Base model specification as a string.
spec = "rma.mv(yi, vi" # start with simple fixed effects model
# Include moderators if provided based on data format.
if ("outcome" %in% colnames(data) & nlevels(data$outcome) <= 2) {
  spec <- paste(spec, ",", "mods = ~ outcome") # add fixed effects for different measurements
} else {
  spec <- paste(spec, ",", "mods = ~") # start moderator formula expression
}
for (col in colnames(data)) {
  if (str_detect(col, "group") & substr(spec, nchar(spec), nchar(spec)) == "~") {
    spec <- paste(spec, "", col) # first grouping factor
  } else if (str_detect(col, "group")) {
    spec <- paste(spec, "+", col) # add grouping factors to model common sources of bias
  }
}
# We need more than two levels of factor to learn random effects variance parameters.
if (("study_id" %in% colnames(data) & nlevels(data$study_id) > 2) & ("outcome" %in% colnames(data) & nlevels(data$outcome) > 2)) {
    spec <- paste(spec, ",", "random = list(~ 1 | study_id, ~ 1 | outcome)") # add random effects per study and outcome measure
} else if ("study_id" %in% colnames(data) & nlevels(data$study_id) > 2 & ("outcome" %in% colnames(data) & nlevels(data$outcome) <= 2)) {
    spec <- paste(spec, ",", "random = ~ 1 | study_id") # add random effects per study
} else if ("study_id" %in% colnames(data) & nlevels(data$study_id) <= 2 & ("outcome" %in% colnames(data) & nlevels(data$outcome) > 2)) {
  spec <- paste(spec, ",", "random = ~ 1 | outcome") # add random effects per outcome, and not per study
}
spec <- paste(spec, ",", "data = data)") # add data and close parenthesis

# Prep data for model.
data <- data %>%
  mutate(
    study_id = as.factor(study_id),
    outcome = as.factor(outcome),
    grouping1 = as.factor(grouping1) # TODO: make this flexible to handle multiple grouping factors
  )
data <- escalc(measure = "SMD", yi = standardized_effect_size, sei = std_err, data = data) # TODO: adapt this for different inputs

# Run the model.
model <- eval(parse(text = spec))
# model <- rma(yi, vi, data = data)

# Postprocessing model output.
# TODO: Figure out what this looks like.
summary(model)

# For now return a simple string message
return(list(message = "finished analysis"))

}
library(tidyverse)
library(brms)
library(tidybayes)
library(jsonlite)

runMetaAnalysis <- function(jsonData) {
print("start...")
print(jsonData)
df <- jsonlite::fromJSON(jsonData)

# We'll need to port in the data from javascript somehow, but it will need to be formatted something like this.
# These are just some made up numbers with the required structure.
data <- tibble(
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

# Prep data for model.
data <- data %>%
    mutate(
        study_id = as.factor(study_id),
        outcome = as.factor(outcome),
        grouping1 = as.factor(grouping1) # TODO: make this flexible to handle multiple grouping factors
    )

# Base model specification as a string.
spec = "standardized_effect_size | se(std_err) ~ 1" # simple fixed effects model

# Build up model specification based on data provided and constraints.
if ("study_id" %in% colnames(data) & nlevels(data$study_id) > 2) { # need more than two levels of factor to learn study-level variance parameter
    spec <- paste(spec, "+ (1 | study_id)") # make it a random effects model
}
if ("outcome" %in% colnames(data) & nlevels(data$outcome) > 2) {
    spec <- paste(spec, "+ (1 | outcome)") # add random effects for different measurements
} else if ("outcome" %in% colnames(data)) {
    spec <- paste(spec, "+ outcome") # add fixed effects for different measurements
}
for (col in colnames(data)) {
    if (str_detect(col, "group")) {
        spec <- paste(spec, "+", col) # add grouping factors to model common sources of bias
    }
}

# Run the model (may take a minute or two).
model <- brm(data = data, family = gaussian,
        eval(parse(text = spec)),
        prior = c(prior(normal(0, 1), class = b),
                  prior(cauchy(0, 1), class = sd)),
        iter = 2000, warmup = 1000, cores = 4, chains = 4, seed = 14,
        control = list("adapt_delta" = 0.95, "max_treedepth" = 12))

# Postprocessing model output.
# TODO: Figure out what this looks like.

# For now return a simple string message
return(list(message = "finished analysis"))

}
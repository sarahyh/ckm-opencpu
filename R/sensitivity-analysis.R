runMetaAnalysis <- function(jsonData) {

library(tidyverse)
library(metafor)
library(jsonlite)
# library(multiverse)

# Read in data from json.
data <- fromJSON(jsonData)

# Pre-processing
data <- data %>%
  mutate(
    # effectSize = if_else(outcomeType == "dichotomous",
    #                      logOddsRatio,
    #                      SMD),
    # stdErrEffectSize = if_else(outcomeType == "dichotomous",
    #                            stdErrLogOddsRatio,
    #                            stdErrSMD),
    # standardizedMetric = if_else(outcomeType == "dichotomous",
    #                              "logOddsRatio",
    #                              "SMD"),
    study_id = paste(author, " ", year),
    yi = effectSize,                  
    vi = stdErrEffectSize^2,
    outcome = as.factor(standardizedMetric)
  )

# List out every possible combination of studies:
# Define permutation function
perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}

# Get unique studies.
studies <- unique(data$study_id)

# Declare results data structure.
results <- list()

# Permutation to get subsets of each possible size.
for (i in 1:length(studies)) { 
  
  subsets_of_size_i <- cbind(unique(perm(studies)[,1:i]))
  for (j in 1:length(subsets_of_size_i[,1])) {
    # Filter data to current subset (run each analysis)
    curr_data <- data %>% 
      filter(study_id %in% subsets_of_size_i[j,]) %>%
      mutate(study_id = as.factor(study_id))
    
    if (i > 1) {
      # Build up model specification based on data provided and constraints:
      # Base model specification as a string.
      spec = "rma.mv(yi, vi" # start with simple fixed effects model
      # Include moderators if provided based on data format. 
      # if ("outcome" %in% colnames(curr_data) && nlevels(curr_data$outcome) >= 2) {
      #   spec <- paste(spec, ",", "mods = ~ outcome") # add fixed effects for different measurements
      # } else {
      #   spec <- paste(spec, ",", "mods = ~") # start moderator formula expression
      # }
      # for (col in colnames(curr_data)) {
      #   if (str_detect(col, "group") & substr(spec, nchar(spec), nchar(spec)) == "~") {
      #     spec <- paste(spec, "", col) # first grouping factor
      #   } else if (str_detect(col, "group")) {
      #     spec <- paste(spec, "+", col) # add grouping factors to model common sources of bias
      #   }
      # }
      # We need more than two levels of factor (i.e., study_id) to learn random effects variance parameters.
      if ("study_id" %in% colnames(curr_data) & nlevels(curr_data$study_id) > 2) {
        spec <- paste(spec, ",", "random = ~ 1 | study_id") # add random effects per study
      }
      spec <- paste(spec, ",", "data = curr_data)") # add data and close parenthesis
      
      # Run the model.
      model <- eval(parse(text = spec))
      
      # Postprocessing model output:
      # Get overall effect size estimate and heterogeniety stats
      # AMK: Adding moderators to model spec changes output of predict so that there is one prediction per study. This will require a change in postprocessing.
      summary <- data_frame(
        "author" = "Overall",
        "summary" = TRUE,
        "study_set" = list(subsets_of_size_i[j,]),
        "effectSize" = predict.rma(model)$pred,
        "stdErrEffectSize" = predict.rma(model)$se,
        "tau2" = model$tau2,
        "stdErrTau2" = if_else(is.null(model[["se.tau2"]]),
                               NA,
                               model$se.tau2),
        "Q" = model$QE,
        "dfQ" = model$k - model$p
      ) %>% mutate(
        "pQ" = pchisq(Q, dfQ, lower.tail = FALSE),
        "I2" = if_else(is.null(model[["I2"]]),
                       (model$QE - (model$k - model$p)) / model$QE, # overall I^2 if no moderators
                       model$I2), # residual I^2 after grouping by moderators
        "standardizedMetric" = if_else(all(curr_data$standardizedMetric == "SMD" | curr_data$standardizedMetric == "arcsineRiskDiff"),
                                       "SMD",
                                       "logOddsRatio") # logOddsRatio or logRiskRatio
      )
      # Get study weights.
      curr_data <- curr_data %>%
        mutate(weight = weights.rma.mv(model))
    } else { # only one study
      # Run no model.
      
      # Carry forward effect size estimate from single study as result.
      summary <- data_frame(
        "author" = "Overall",
        "summary" = TRUE,
        "study_set" = list(subsets_of_size_i[j,]),
        "effectSize" = curr_data$effectSize,
        "stdErrEffectSize" = curr_data$stdErrEffectSize,
        "tau2" = NULL,
        "stdErrTau2" = NULL,
        "Q" = NULL,
        "dfQ" = NULL,
        "pQ" = NULL,
        "I2" = NULL,
        "standardizedMetric" = if_else(all(curr_data$standardizedMetric == "SMD" | curr_data$standardizedMetric == "arcsineRiskDiff"),
                                       "SMD",
                                       "logOddsRatio") # logOddsRatio or logRiskRatio
      )
      # Get study weights.
      curr_data <- curr_data %>%
        mutate(weight = 100)
    }
    
    
    # Join summary with study data.
    curr_data <- curr_data %>% full_join(summary, by = c("author", "effectSize", "stdErrEffectSize", "standardizedMetric"))
    # Drop columns used only for modeling.
    curr_data <- curr_data[ , !(names(curr_data) %in% c("study_id", "yi", "vi", "outcome"))]
    
    # Collate results
    results <- append(results, curr_data)
  }
}

return(list(message = "finished", data = toJSON(results)))
}
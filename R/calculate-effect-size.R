calculateEffectSize <- function(jsonData) {

library(tidyverse)
library(jsonlite)

# Read in data that users extract for each study. 
df <- fromJSON(jsonData)

# Fill in dataframe with the summary statistics we need for meta-analysis.
# Handle cases differently depending on outcomeType and how information was reported.
df <- df %>%
  mutate(
    # Do we have separate sample sizes for our two groups, or do we need to assume a balanced design?
    N = case_when(
      !is.na(nExp) & !is.na(nCtrl) ~ nExp + nCtrl, # calculate overall sample size
      TRUE                         ~ N
    ),
    nExp = case_when(
      is.na(nExp) & !is.na(N) ~ floor(N / 2), # assume a balanced design
      TRUE                    ~ nExp
    ),
    nCtrl = case_when(
      is.na(nCtrl) & !is.na(N) ~ floor(N / 2), # assume a balanced design
      TRUE                     ~ nCtrl
    ),

    # Calculate unstandardized effect size if we don't already have it.
    meanDiff = case_when(
      outcomeType == "continuous" & !is.na(meanExp) & !is.na(meanCtrl) ~ meanExp - meanCtrl,
      outcomeType == "continuous"                                      ~ meanDiff,
      TRUE                                                                  ~ meanDiff
    ),
    # TODO: get both counts and props from odds ratios, and risk ratios
    # countExp =
    # countCtrl =
    # countDiff =
    # propExp =
    # propCtrl =
    # propDiff =
    # oddsRatio =
    # riskRatio =

    # Calculate the standard error and pooled standard deviation.
    # TODO: fill in cases for non-continuous outcomes
    tValue = case_when(
      outcomeType == "continuous" & !is.na(pValue) & !is.na(nTails) ~ qt(p = pValue / nTails, df = N - 2),
      outcomeType == "continuous" & !is.na(fValue)                  ~ sqrt(fValue),
      TRUE                                                               ~ tValue 
    ),
    stdErr = case_when( # cases where we calculate stdErr from test statistics, etc.
      outcomeType == "continuous" & !is.na(lowerBoundCI) & !is.na(upperBoundCI) & !is.na(confLevelCI) ~ 0.5 * (upperBoundCI - lowerBoundCI) / abs(qt(p = (confLevelCI + (100 - confLevelCI) / 2) / 100, df = N - 2)),
      outcomeType == "continuous" & !is.na(meanDiff) & !is.na(tValue)                                 ~ abs(meanDiff / tValue),
      TRUE                                                                                                 ~ stdErr
    ),
    sdDiff = case_when(
      outcomeType == "continuous" & !is.na(sdExp) & !is.na(sdCtrl) ~ sqrt(((nExp - 1) * sdExp^2 + (nCtrl - 1) * sdCtrl^2) / (N - 2)),
      outcomeType == "continuous" & !is.na(stdErr)                 ~ stdErr / sqrt(1 / nExp + 1 / nCtrl),
      TRUE                                                              ~ sdDiff
    ),
    stdErr = case_when( # cases where we calculate stdErr from sdDiff and not the other way around
      outcomeType == "continuous" & !is.na(sdDiff) & is.na(stdErr) ~ sdDiff * sqrt(1 / nExp + 1 / nCtrl), 
      TRUE                                                              ~ stdErr
    ),

    # Now, calculate standardized effect size if we can.
    SMD = case_when(
      outcomeType == "continuous" & !is.na(meanDiff) & !is.na(sdDiff) ~ meanDiff / sdDiff * (1 - 3 / (4 * (nExp + nCtrl) - 9)), # apply Hedges' correction
      TRUE                                                                 ~ SMD
    ),
    stdErrSMD = case_when(
      outcomeType == "continuous" & !is.na(SMD) ~ sqrt((nExp + nCtrl) / (nExp * nCtrl) + SMD^2 / (2 * (nExp + nCtrl))),
      TRUE                                           ~ stdErrSMD
    )
    # TODO: fill in log odds ratio 
    # TODO: fill in log risk ratio 
    # TODO: fill in arcsine risk difference
    # TODO: fill in point-biserial correlations 
    # TODO: fill in Fisher Z transform for Peason correlations
  )

  return(list(message = "success", data = toJSON(df)))

# metafor::escalc approach from group statistics
# df <- escalc(measure = "SMD", m1i = meanExp, m2i = meanCtrl, sd1i = sdExp, sd2i = sdCtrl, n1i = nExp, n2i = nCtrl, data = df) %>%
#   rename(
#     effectSize = yi,
#     stdErrEffectSize = sqrt(vi)
#   )

# # Do we have separate sample sizes for our two groups, or do we need to assume a balanced design?
# if ("nExp" %in% colnames(df) & "nCtrl" %in% colnames(df) & !"N" %in% colnames(df)) {
#   # calculate overall sample size
#   df <- df %>%
#     mutate(N = nExp + nCtrl)
# } else if ("N" %in% colnames(df) & (!"nExp" %in% colnames(df) | !"nCtrl" %in% colnames(df))) {
#   # assume a balanced design
#   df <- df %>%
#     mutate(
#       nExp = floor(N / 2),
#       nCtrl = floor(N / 2)
#     )
# }

# # Handle cases differently depending on outcomeType and how information was reported:
# # Continuous outcomes => SMD
# if (df$outcomeType == "continuous") { 
#   # Calculate unstandardized mean difference if we don't already have it.
#   if (!"meanDiff" %in% colnames(df) & "meanExp" %in% colnames(df) & "meanCtrl" %in% colnames(df)) {
#     df <- df %>%
#       mutate(meanDiff = meanExp - meanCtrl)
#   }

#   # Calculate the standard error and pooled standard deviation.
#   # Assume t-dist w/ df = N - 2 if we need to.
#   if ("sdExp" %in% colnames(df) & "sdCtrl" %in% colnames(df)) {
#     # user extracted group standard deviations
#     df <- df %>%
#       mutate(
#         sdDiff = sqrt(((nExp - 1) * sdExp^2 + (nCtrl - 1) * sdCtrl^2) / (N - 2)),
#         stdErr = sdDiff * sqrt(1 / nExp + 1 / nCtrl),
#       ) 
#   } else if ("sdDiff" %in% colnames(df)) {
#     # user extracted pooled standard deviation
#     df <- df %>%
#       mutate(stdErr = sdDiff * sqrt(1 / nExp + 1 / nCtrl))
#   } else if ("stdErr" %in% colnames(df)) {
#     # user extracted standard error
#     df <- df %>%
#       mutate(sdDiff = stdErr / sqrt(1 / nExp + 1 / nCtrl))
#   } else if ("lowerBoundCI" %in% colnames(df) & "upperBoundCI" %in% colnames(df) & "confLevelCI" %in% colnames(df)) {
#     # user extracted confidence interval 
#     df <- df %>%
#       mutate(
#         stdErr = 0.5 * (upperBoundCI - lowerBoundCI) / abs(qt(p = (confLevelCI + (100 - confLevelCI) / 2) / 100, df = N - 2)),
#         sdDiff = stdErr / sqrt(1 / nExp + 1 / nCtrl)
#       )
#   } else if ("tValue" %in% colnames(df)) {
#     # user extracted t test statistic
#     df <- df %>%
#         mutate(
#           stdErr = meanDiff / tValue,
#           sdDiff = stdErr / sqrt(1 / nExp + 1 / nCtrl)
#         )
#   } else if ("fValue" %in% colnames(df)) {
#     # user extracted f test statistic
#     df <- df %>%
#         mutate(
#           tValue = sqrt(fValue), # result will always be positive, so need to use abs on next line
#           stdErr = abs(meanDiff / tValue),
#           sdDiff = stdErr / sqrt(1 / nExp + 1 / nCtrl)
#         )
#   } else if ("pValue" %in% colnames(df) & "nTails" %in% colnames(df)) {
#     # user extracted p value
#     df <- df %>%
#         mutate(
#           tValue = qt(p = pValue / nTails, df = N - 2) # result will always be negative, so need to use abs on next line
#           stdErr = abs(meanDiff / tValue),
#           sdDiff = stdErr / sqrt(1 / nExp + 1 / nCtrl)
#         )
#   } 

#   # Now, calculate standardized effect size if we can.
#   if ("meanDiff" %in% colnames(df) & "sdDiff" %in% colnames(df) & "nExp" %in% colnames(df) & "nCtrl" %in% colnames(df)) {
#     df <- df %>%
#         mutate(
#           SMD = meanDiff / sdDiff * (1 - 3 / (4 * (nExp + nCtrl) - 9)), # apply Hedges' correction
#           stdErrSMD = sqrt((nExp + nCtrl) / (nExp * nCtrl) + effectSize^2 / (2 * (nExp + nCtrl)))
#         )
#     return(list(message = "success", data = toJSON(df)))
#   } else {
#     return(list(message = "failed to convert to standardized units", data = toJSON(df)))
#   }


# # Dichotomous outcomes => RR, OR, AS, 
# } else if (df$outcomeType == "ratio") {
#   # Calculate proportions and counts per group.
#   if ("propExp" %in% colnames(df) & "propCtrl" %in% colnames(df) & (!"countExp" %in% colnames(df) | !"countCtrl" %in% colnames(df))) {
#     # calculate proportions from counts and sample size
#     df <- df %>%
#         mutate(
#           propExp = countExp / nExp,
#           propCtrl = countCtrl / nCtrl
#         )
#   } else if ("countExp" %in% colnames(df) & "countCtrl" %in% colnames(df) & (!"propExp" %in% colnames(df) | !"propCtrl" %in% colnames(df))) {
#     # calculate counts from proportions and sample size
#     df <- df %>%
#         mutate(
#           countExp = propExp * nExp,
#           countCtrl = propCtrl * nCtrl
#         )
#   } # TODO: get both counts and props from odds ratios, and risk ratios
  
#   # Now, calculate standardized effect size if we can.
#   # TODO: mimic syntax above
# } else if (df$outcomeType == "correlation") {
#   # TODO: fill in fisher Z transform for correlations
# }

}
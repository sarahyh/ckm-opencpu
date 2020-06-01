calculateEffectSize <- function(jsonData) {

library(tidyverse)
library(jsonlite)

# Read in data that users extract for each study. 
df <- fromJSON(jsonData)

# Fill in dataframe with the summary statistics we need for meta-analysis.
# Handle cases differently depending on outcomeType and how information was reported.
# df <- df %>%
#   mutate(
#     # Do we have separate sample sizes for our two groups, or do we need to assume a balanced design?
#     N = case_when(
#       is.na(N) & !is.na(nExp) & !is.na(nCtrl) ~ nExp + nCtrl, # calculate overall sample size
#       TRUE                                    ~ N
#     ),
#     nExp = case_when(
#       is.na(nExp) & !is.na(N) ~ floor(N / 2), # assume a balanced design
#       TRUE                    ~ nExp
#     ),
#     nCtrl = case_when(
#       is.na(nCtrl) & !is.na(N) ~ floor(N / 2), # assume a balanced design
#       TRUE                     ~ nCtrl
#     ),

#     # For continuous outcomes...
#     # Calculate untransformed mean difference if we don't already have it.
#     meanDiff = case_when(
#       outcomeType == "continuous" & is.na(meanDiff) & !is.na(meanExp) & !is.na(meanCtrl) ~ meanExp - meanCtrl,
#       TRUE                                                                               ~ meanDiff
#     ),
#     # Calculate the standard error and pooled standard deviation with whatever information was extracted.
#     tValue = case_when(
#       outcomeType == "continuous" & is.na(tValue) & !is.na(pValue) & !is.na(nTails) ~ qt(p = pValue / nTails, df = N - 2),
#       outcomeType == "continuous" & is.na(tValue) & !is.na(fValue)                  ~ sqrt(fValue),
#       TRUE                                                                          ~ tValue 
#     ),
#     stdErrMeanDiff = case_when( # cases where we calculate stdErrMeanDiff from test statistics, etc.
#       outcomeType == "continuous" & is.na(stdErrMeanDiff) & !is.na(lowerBoundCI) & !is.na(upperBoundCI) & !is.na(confLevelCI) ~ 0.5 * (upperBoundCI - lowerBoundCI) / abs(qt(p = (confLevelCI + (100 - confLevelCI) / 2) / 100, df = N - 2)),
#       outcomeType == "continuous" & is.na(stdErrMeanDiff) & !is.na(meanDiff) & !is.na(tValue)                                 ~ abs(meanDiff / tValue),
#       TRUE                                                                                                                    ~ stdErrMeanDiff
#     ),
#     sdDiff = case_when(
#       outcomeType == "continuous" & is.na(sdDiff) & !is.na(sdExp) & !is.na(sdCtrl) ~ sqrt(((nExp - 1) * sdExp^2 + (nCtrl - 1) * sdCtrl^2) / (N - 2)),
#       outcomeType == "continuous" & is.na(sdDiff) & !is.na(stdErrMeanDiff)         ~ stdErrMeanDiff / sqrt(1 / nExp + 1 / nCtrl),
#       TRUE                                                                         ~ sdDiff
#     ),
#     stdErrMeanDiff = case_when( # cases where we calculate stdErrMeanDiff from sdDiff and not the other way around
#       outcomeType == "continuous" & is.na(stdErrMeanDiff) & !is.na(sdDiff) ~ sdDiff * sqrt(1 / nExp + 1 / nCtrl), 
#       TRUE                                                                 ~ stdErrMeanDiff
#     ),
#     # Now, calculate standardized mean difference if we can.
#     SMD = case_when(
#       outcomeType == "continuous" & !is.na(meanDiff) & !is.na(sdDiff) ~ meanDiff / sdDiff * (1 - 3 / (4 * (nExp + nCtrl) - 9)), # apply Hedges' correction
#       TRUE                                                                 ~ SMD
#     ),
#     stdErrSMD = case_when(
#       outcomeType == "continuous" & !is.na(SMD) ~ sqrt((nExp + nCtrl) / (nExp * nCtrl) + SMD^2 / (2 * (nExp + nCtrl))),
#       TRUE                                           ~ stdErrSMD
#     )

#     # For dichotomous outcomes...
#     # Calculate proportions and counts.
#     pExp = case_when(
#       outcomeType == "dichotomous" & is.na(pExp) & !is.na(countExp) ~ countExp / nExp,
#       TRUE                                                          ~ pExp
#     ),
#     pCtrl = case_when(
#       outcomeType == "dichotomous" & is.na(pCtrl) & !is.na(countCtrl) ~ countCtrl / nCtrl,
#       TRUE                                                            ~ pCtrl
#     ),
#     countExp = case_when(
#       outcomeType == "dichotomous" & is.na(countExp) & !is.na(pExp) ~ floor(pExp * nExp),
#       TRUE                                                          ~ countExp
#     ),
#     countCtrl = case_when(
#       outcomeType == "dichotomous" & is.na(countCtrl) & !is.na(pCtrl) ~ floor(pCtrl * nCtrl),
#       TRUE                                                            ~ countCtrl
#     ),
#     # Calculate untransformed risk difference, risk ratio, and odds ratio
#     # riskDiff =
#     # riskRatio =
#     # oddsRatio =

    
#     # TODO: fill in log odds ratio 
#     # TODO: fill in log risk ratio 
#     # TODO: fill in arcsine risk difference


#     # TODO: fill in point-biserial correlations 
#     # TODO: fill in Fisher Z transform for Peason correlations
#   )

  return(list(message = "success", data = toJSON(df)))

}
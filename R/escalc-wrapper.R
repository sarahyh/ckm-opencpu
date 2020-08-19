# escalcWrapper <- function(jsonData) {

library(tidyverse)
library(jsonlite)
library(metafor)

# Read in data that users extract for each study. 
# data <- fromJSON(jsonData)
# data <- read_json("test.json", simplifyVector = TRUE)
data <- read_json("testFromExtraction.json", simplifyVector = TRUE)

# Force R to handle all numerical data as doubles to prevent type errors.
data <- data %>% mutate_if(is.integer, as.numeric) %>% mutate_if(~all(is.na(.)), as.numeric)
  
# helper function to add empty columns to df if they don't already exists
add_col_if_not_exist <- function(df, ...) {
  args <- ensyms(...)
  for (arg in args) {
    if (is.null(df[[toString(arg)]])) {
      df <- df %>% mutate(!!arg := NA)
    }
  }
  return(df)
}

## Fill in dataframe with the summary statistics we need for meta-analysis, handling cases differently depending on outcomeType and how information was reported.

# Sample size is relevant to all types of outcomes.
# Add columns if we need to.
data <- data %>% add_col_if_not_exist(N, nExp, nCtrl)
# Do we have separate sample sizes for our two groups, or do we need to assume a balanced design?
data <- data %>% 
  mutate(
    N = case_when(
      is.na(N) & !is.na(nExp) & !is.na(nCtrl) ~ as.numeric(nExp + nCtrl), # calculate overall sample size
      TRUE                                    ~ as.numeric(N)),
    nExp = case_when(
      is.na(nExp) & !is.na(nCtrl) & !is.na(N) ~ as.numeric(N - nCtrl), # calculate treatment group sample size
      is.na(nExp) & is.na(nCtrl) & !is.na(N)  ~ as.numeric(floor(N / 2)), # assume a balanced design
      TRUE                                    ~ as.numeric(nExp)),
    nCtrl = case_when(
      is.na(nCtrl) & !is.na(nExp) & !is.na(N) ~ as.numeric(N - nExp), # calculate control group sample size
      is.na(nCtrl) & is.na(nExp) & !is.na(N)  ~ as.numeric(floor(N / 2)), # assume a balanced design
      TRUE                                    ~ as.numeric(nCtrl))
  )

# Split our dataframe into separate dataframes for continuous and dichotomous outcomes, creating an index column to maintain the original order when we re-join them.
data <- data %>% rowid_to_column("index")
continuous_studies <- data %>% filter(outcomeType == "continuous")
dichotomous_studies <- data %>% filter(outcomeType == "dichotomous")

## For studies with continuous outcomes...
# Add columns if we need to.
continuous_studies <- continuous_studies %>% add_col_if_not_exist(meanDiff, meanExp, meanCtrl, tValue, pValue, nTails, fValue, stdErrMeanDiff, lowerBoundCI, upperBoundCI, confLevelCI, sdDiff, sdExp, sdCtrl, r, SMD, stdErrSMD, pbCor, stdErrPbCor)

# With whatever information was extracted, calculate the either group standard deviations or the sampling variance of the estimate.

# Use escalc to get standardized mean differences (SMD with vtype = "UB") or approximate log odds ratios (D2ORN) for combining with dichotomous outcomes.


#############################
if (any(data[["outcomeType"]] == "continuous")) {
  # For continuous outcomes...
  # Add columns if we need to.
  data <- data %>% add_col_if_not_exist(meanDiff, meanExp, meanCtrl, tValue, pValue, nTails, fValue, stdErrMeanDiff, lowerBoundCI, upperBoundCI, confLevelCI, sdDiff, sdExp, sdCtrl, r, SMD, stdErrSMD, pbCor, stdErrPbCor)
  
  # Calculate untransformed mean difference if we don't already have it.
  data <- data %>% 
    mutate(
      meanDiff = case_when(
        is.na(meanDiff) & !is.na(meanExp) & !is.na(meanCtrl) ~ meanExp - meanCtrl,
        TRUE                                                 ~ as.numeric(meanDiff))
    )
  
  # Calculate the standard error and pooled standard deviation with whatever information was extracted.
  data <- data %>%
    mutate(
      tValue = case_when(
        is.na(tValue) & !is.na(pValue) & !is.na(nTails) ~ qt(p = pValue / nTails, df = N - 2),
        is.na(tValue) & !is.na(fValue)                  ~ sqrt(fValue),
        TRUE                                            ~ as.numeric(tValue)),
      stdErrMeanDiff = case_when( # cases where we calculate stdErrMeanDiff from test statistics, etc.
        is.na(stdErrMeanDiff) & !is.na(lowerBoundCI) & !is.na(upperBoundCI) & !is.na(confLevelCI) ~ 0.5 * (upperBoundCI - lowerBoundCI) / abs(qt(p = (confLevelCI + (100 - confLevelCI) / 2) / 100, df = N - 2)),
        is.na(stdErrMeanDiff) & !is.na(meanDiff) & !is.na(tValue)                                 ~ abs(meanDiff / tValue),
        TRUE                                                                                      ~ as.numeric(stdErrMeanDiff)),
      sdDiff = case_when(
        is.na(sdDiff) & !is.na(sdExp) & !is.na(sdCtrl) ~ sqrt(((nExp - 1) * sdExp^2 + (nCtrl - 1) * sdCtrl^2) / (N - 2)),
        is.na(sdDiff) & !is.na(stdErrMeanDiff)         ~ stdErrMeanDiff / sqrt(1 / nExp + 1 / nCtrl),
        TRUE                                           ~ as.numeric(sdDiff)),
      stdErrMeanDiff = case_when( # cases where we calculate stdErrMeanDiff from sdDiff and not the other way around
        is.na(stdErrMeanDiff) & !is.na(r) & !is.na(sdDiff) ~ sqrt(2 * sdDiff * (1 - r) / N), # accounting for correlation (r) across timepoints for within-subjects contrasts
        is.na(stdErrMeanDiff) & !is.na(sdDiff)             ~ sdDiff * sqrt(1 / nExp + 1 / nCtrl), 
        TRUE                                               ~ as.numeric(stdErrMeanDiff))
    )
  
  # Now, calculate standardized mean difference and point biserial correlation.
  data <- data %>%
    mutate(
      SMD = case_when(
        is.na(SMD) & !is.na(meanDiff) & !is.na(sdDiff) & !is.na(r) ~ meanDiff / sdDiff, # no correction for within-subjects measures (indicated by presence of r)
        is.na(SMD) & !is.na(meanDiff) & !is.na(sdDiff)             ~ meanDiff / sdDiff * (1 - 3 / (4 * (nExp + nCtrl) - 9)), # apply Hedges' correction
        TRUE                                                       ~ as.numeric(SMD)),
      stdErrSMD = case_when(
        is.na(stdErrSMD) & !is.na(SMD) & !is.na(r) ~ sqrt(2 * (1 - r) / N + SMD^2 / (2 * N)), # accounting for correlation (r) across timepoints for within-subjects contrasts
        is.na(stdErrSMD) & !is.na(SMD)             ~ sqrt((nExp + nCtrl) / (nExp * nCtrl) + SMD^2 / (2 * (nExp + nCtrl))),
        TRUE                                       ~ as.numeric(stdErrSMD)),
      pbCor = case_when(
        is.na(pbCor) & !is.na(SMD) ~ SMD / sqrt(1 / ((nExp / N) * (nCtrl / N)) + SMD^2),
        TRUE                       ~ as.numeric(pbCor)),
      stdErrPbCor = case_when(
        is.na(stdErrPbCor) & !is.na(pbCor) ~ sqrt((1 - pbCor^2)^2 * (N * pbCor^2 / (4 * nExp * nCtrl) + (2 - 3 * pbCor^2) / (2 * N))),
        TRUE                               ~ as.numeric(stdErrPbCor))
    )
} 
#############################


## For studies with dichotomous outcomes...
# Add columns if we need to.
dichotomous_studies <- dichotomous_studies %>% add_col_if_not_exist(pExp, countExp, pCtrl, countCtrl)

# With whatever information was extracted, calculate proportions and counts per group.
dichotomous_studies <- dichotomous_studies %>%
  mutate(
    pExp = case_when(
      is.na(pExp) & !is.na(countExp) ~ countExp / nExp,
      TRUE                           ~ as.numeric(pExp)),
    pCtrl = case_when(
      is.na(pCtrl) & !is.na(countCtrl) ~ countCtrl / nCtrl,
      TRUE                             ~ as.numeric(pCtrl)),
    countExp = case_when(
      is.na(countExp) & !is.na(pExp) ~ floor(pExp * nExp),
      TRUE                           ~ as.numeric(countExp)),
    countCtrl = case_when(
      is.na(countCtrl) & !is.na(pCtrl) ~ floor(pCtrl * nCtrl),
      TRUE                             ~ as.numeric(countCtrl))
  )

# Use escalc to get risk difference (RD) and its standard error (default original units).
exp_event <- dichotomous_studies$countExp
exp_no_event <- dichotomous_studies$nExp - dichotomous_studies$countExp
ctrl_event <- dichotomous_studies$countCtrl
ctrl_no_event <- dichotomous_studies$nCtrl - dichotomous_studies$countCtrl
dichotomous_studies <- escalc(measure = "RD", ai = exp_event, bi = exp_no_event, ci = ctrl_event, di = ctrl_no_event, to = "if0all", data = dichotomous_studies) %>%
  mutate(
    riskDiff = yi,
    stdErrRiskDiff = sqrt(vi)
  ) %>%
  select(-c("yi", "vi"))

# Use escalc to get log risk ratio (RR) and log odds ratio (OR), or arcsine square-root transformed risk difference (AS) for combining with continuous outcomes.
if (nrow(continuous_studies) == 0) {
  dichotomous_studies <- escalc(measure = "RR", ai = exp_event, bi = exp_no_event, ci = ctrl_event, di = ctrl_no_event, to = "if0all", data = dichotomous_studies) %>%
    mutate(
      logRiskRatio = yi,
      stdErrLogRiskRatio = sqrt(vi)
    ) %>%
    select(-c("yi", "vi"))
  dichotomous_studies <- escalc(measure = "OR", ai = exp_event, bi = exp_no_event, ci = ctrl_event, di = ctrl_no_event, to = "if0all", data = dichotomous_studies) %>%
    mutate(
      logOddsRatio = yi,
      stdErrLogOddsRatio = sqrt(vi)
    ) %>%
    select(-c("yi", "vi"))
} else {
  dichotomous_studies <- escalc(measure = "AS", ai = exp_event, bi = exp_no_event, ci = ctrl_event, di = ctrl_no_event, to = "if0all", data = dichotomous_studies) %>%
    mutate(
      arcsineRiskDiff = yi,
      stdErrArcsineRiskDiff = sqrt(vi)
    ) %>%
    select(-c("yi", "vi"))
}

## Prepare output dataframe.
# Join dataframes for continuous and dichotomous outcomes.
output <- rbind(continuous_studies, dichotomous_studies) # this may not work due to column mismatch

# Drop columns with nothing in them.
output <- output[,colSums(is.na(output)) < nrow(output)]

# return(list(message = "success", data = toJSON(output)))
# }

# escalcWrapper <- function(jsonData) {
## Fills in dataframe with the summary statistics we need for meta-analysis.
## Handles studies differently depending on outcomeType, study design, and how information was reported.

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


## Sample size is relevant to all types of outcomes.
# Add columns if we need to.
data <- data %>% add_col_if_not_exist(N, nExp, nCtrl)

# We handle sample size differently for within- vs between-subjects designs.
# Within-subjects: The overall sample size is the sample size for both groups.
# Between subjects: Do we have separate sample sizes for our two groups, or do we need to assume a balanced design?
data <- data %>% 
  mutate(
    N = case_when(
      (studyDesign == "withinSubjects") & is.na(N) & !is.na(nExp)                  ~ as.numeric(nExp),         # calculate overall sample size, within-subjects   
      (studyDesign == "withinSubjects") & is.na(N) & !is.na(nCtrl)                 ~ as.numeric(nCtrl),
      (studyDesign == "betweenSubjects") & is.na(N) & !is.na(nExp) & !is.na(nCtrl) ~ as.numeric(nExp + nCtrl), # between-subjects
      TRUE                                                                         ~ as.numeric(N)),
    nExp = case_when(
      (studyDesign == "withinSubjects") & is.na(nExp) & !is.na(N)                  ~ as.numeric(N),            # calculate treatment group sample size, within-subjects
      (studyDesign == "withinSubjects") & is.na(nExp) & !is.na(nCtrl)              ~ as.numeric(nCtrl),
      (studyDesign == "betweenSubjects") & is.na(nExp) & !is.na(nCtrl) & !is.na(N) ~ as.numeric(N - nCtrl),    # between-subjects
      (studyDesign == "betweenSubjects") & is.na(nExp) & is.na(nCtrl) & !is.na(N)  ~ as.numeric(floor(N / 2)), # assuming a balanced design
      TRUE                                                                         ~ as.numeric(nExp)),
    nCtrl = case_when(
      (studyDesign == "withinSubjects") & is.na(nCtrl) & !is.na(N)                 ~ as.numeric(N),            # calculate treatment group sample size, within-subjects
      (studyDesign == "withinSubjects") & is.na(nCtrl) & !is.na(nExp)              ~ as.numeric(nExp),
      (studyDesign == "betweenSubjects") & is.na(nCtrl) & !is.na(nExp) & !is.na(N) ~ as.numeric(N - nExp),     # between-subjects
      (studyDesign == "betweenSubjects") & is.na(nCtrl) & is.na(nExp) & !is.na(N)  ~ as.numeric(floor(N / 2)), # assuming a balanced design
      TRUE                                                                         ~ as.numeric(nCtrl))
  )

# Split our dataframe into separate dataframes for continuous and dichotomous outcomes, creating an index column to maintain the original order when we re-join them.
data <- data %>% rowid_to_column("index")
continuous_studies <- data %>% filter(outcomeType == "continuous")
dichotomous_studies <- data %>% filter(outcomeType == "dichotomous")


## For studies with continuous outcomes...
# Add columns if we need to.
continuous_studies <- continuous_studies %>% add_col_if_not_exist(meanDiff, meanExp, meanCtrl, tValue, pValue, nTails, fValue, stdErrMeanDiff, lowerBoundCI, upperBoundCI, confLevelCI, stdErrExp, stdErrCtrl, sdDiff, sdExp, sdCtrl, r, rSquared, sdPooled, SMD, stdErrSMD)

# Calculate untransformed mean difference if we don't already have it.
continuous_studies <- continuous_studies %>% 
  mutate(
    meanDiff = case_when(
      is.na(meanDiff) & !is.na(meanExp) & !is.na(meanCtrl) ~ meanExp - meanCtrl,
      TRUE                                                 ~ as.numeric(meanDiff))
  )

# Alternatively if we don't have the mean difference, we can calculate effect size from a t-test statistic (which can be derived from F or p values).
continuous_studies <- continuous_studies %>%
  mutate(
    tValue = case_when(
      is.na(tValue) & !is.na(pValue) & !is.na(nTails) ~ qt(p = pValue / nTails, df = N - 2), # this derivation is always positive (no info on direction of effect)
      is.na(tValue) & !is.na(fValue)                  ~ sqrt(fValue), # this derivation is always positive (no info on direction of effect)
      TRUE                                            ~ as.numeric(tValue))
  )

# In cases where we calculate effect size from mean difference (not t value), we also need the within-groups standard deviation, pooled across groups.
continuous_studies <- continuous_studies %>%
  mutate(
    # In some cases, users extract data about the standard error of the difference between groups.
    stdErrMeanDiff = case_when(  
      is.na(stdErrMeanDiff) & !is.na(lowerBoundCI) & !is.na(upperBoundCI) & !is.na(confLevelCI) ~ 0.5 * (upperBoundCI - lowerBoundCI) / abs(qt(p = (confLevelCI + (100 - confLevelCI) / 2) / 100, df = N - 2)), # extracted CI
      is.na(stdErrMeanDiff) & !is.na(meanDiff) & !is.na(tValue)                                 ~ abs(meanDiff / tValue), # extracted t value as reliability metric
      TRUE                                                                                      ~ as.numeric(stdErrMeanDiff)),
    # In some cases, users extract data about the sd or stdErr per group in a between-subjects design.
    sdDiff = case_when(
      (studyDesign == "betweenSubjects") & is.na(sdDiff) & !is.na(sdExp) & !is.na(sdCtrl)         ~ sqrt(((nExp - 1) * sdExp^2 + (nCtrl - 1) * sdCtrl^2) / (N - 2)), # extracted sd per group in between-subjects design
      (studyDesign == "betweenSubjects") & is.na(sdDiff) & !is.na(stdErrExp) & !is.na(stdErrCtrl) ~ sqrt(((nExp - 1) * (stdErrExp^2 * nExp) + (nCtrl - 1) * (stdErrCtrl^2 * nCtrl)) / (N - 2)), # extracted stdErr per group in between-subjects design
      is.na(sdDiff) & !is.na(stdErrMeanDiff)                                                      ~ stdErrMeanDiff / sqrt(1 / nExp + 1 / nCtrl), # carrying forward data about the stdErr of the difference
      TRUE                                                                                        ~ as.numeric(sdDiff)),
    # With all these possible extraction strategies, sdPooled is what we actually need to calculate effect size.
    sdPooled = case_when(
      (studyDesign == "withinSubjects") & is.na(sdPooled) & !is.na(sdExp) & !is.na(sdCtrl)                ~ sqrt((sdExp^2 + sdCtrl^2) / 2), # extracted sd per group in within-subjects design (not sure about this)
      (studyDesign == "withinSubjects") & is.na(sdPooled) & !is.na(stdErrExp) & !is.na(stdErrCtrl)        ~ sqrt(((stdErrExp^2 * nExp) + (stdErrCtrl^2 * nCtrl)) / 2), # extracted stdErr per group in within-subjects design (not sure about this)
      (studyDesign == "withinSubjects") & is.na(sdPooled) & !is.na(r) & is.na(rSquared) & !is.na(sdDiff)  ~ sdDiff / sqrt(2 * (1 - r)), # extracted sd is unadjusted sd of a within subjects difference, apply correction for repeated measures correlation
      (studyDesign == "withinSubjects") & is.na(sdPooled) & !is.na(r) & !is.na(rSquared) & !is.na(sdDiff) ~ sdDiff / sqrt(2 * (1 - r)) / sqrt(1 - rSquared), # extracted sd is adjusted for covariates and is the sd of a within subjects difference, apply correction for repeated measures correlation and for coefficient of variation (not sure about this)
      (studyDesign == "betweenSubjects") & is.na(sdPooled) & is.na(rSquared) & !is.na(sdDiff)             ~ as.numeric(sdDiff), # direct extraction of unadjusted pooled sd for between subjects comparison
      (studyDesign == "betweenSubjects") & is.na(sdPooled) & !is.na(rSquared) & !is.na(sdDiff)            ~ sdDiff / sqrt(1 - rSquared), # extracted sd is adjusted for covariates, apply correction for coefficient of variation
      TRUE                                                                                                ~ as.numeric(sdPooled))
  )

# Calculate effect size using either the meanDiff and sdPooled OR tValue, making adjustments/corrections as appropriate.
continuous_studies <- continuous_studies %>%
  mutate(
    SMD = case_when(
      is.na(SMD) & !is.na(meanDiff) & !is.na(sdPooled)                                               ~ meanDiff / sdPooled, # sdPooled is already appropriately adjusted
      (studyDesign == "withinSubjects") & is.na(SMD) & !is.na(tValue) & !is.na(r) & is.na(rSquared)  ~ tValue * sqrt(2 * (1 - r) / N), # adjust t value for repeated measures correlation
      (studyDesign == "withinSubjects") & is.na(SMD) & !is.na(tValue) & !is.na(r) & !is.na(rSquared) ~ tValue * sqrt(2 * (1 - r) / N) * sqrt(1 - rSquared), # adjust t value for repeated measures correlation and coefficient of variation (not sure about this)
      (studyDesign == "betweenSubjects") & is.na(SMD) & !is.na(tValue) & is.na(rSquared)             ~ tValue * sqrt((nExp + nCtrl) / (nExp * nCtrl)), # t value does not need to be adjusted for between-subjects design with no model covariates
      (studyDesign == "betweenSubjects") & is.na(SMD) & !is.na(tValue) & !is.na(rSquared)            ~ tValue * sqrt((nExp + nCtrl) / (nExp * nCtrl)) * sqrt(1 - rSquared), # adjust t value for coefficient of variation
      TRUE                                                                                           ~ as.numeric(SMD)),
    stdErrSMD = case_when(
      (studyDesign == "withinSubjects") & is.na(stdErrSMD) & !is.na(SMD) & !is.na(r) & is.na(rSquared)  ~ sqrt(((1 / N) + (SMD^2 / (2 * N))) * (2 * (1 - r))), # adjust stdErr of SMD for repeated measures correlation
      (studyDesign == "withinSubjects") & is.na(stdErrSMD) & !is.na(SMD) & !is.na(r) & !is.na(rSquared) ~ sqrt(((1 / N) + (SMD^2 / (2 * N))) * (2 * (1 - r)) * (1 - rSquared)), # adjust stdErr of SMD for repeated measures correlation and coefficient of variation (not sure about this)
      (studyDesign == "betweenSubjects") & is.na(stdErrSMD) & !is.na(SMD) & is.na(rSquared)             ~ sqrt(((nExp + nCtrl) / (nExp * nCtrl)) + (SMD^2 / (2 * (nExp + nCtrl)))), # stdErr of SMD does not need to be adjusted for between-subjects design with no model covariates
      (studyDesign == "betweenSubjects") & is.na(stdErrSMD) & !is.na(SMD) & !is.na(rSquared)            ~ sqrt(((nExp + nCtrl) * (1 - rSquared) / (nExp * nCtrl)) + (SMD^2 / (2 * (nExp + nCtrl)))), # adjust stdErr of SMD for coefficient of variation
      TRUE                                                                                              ~ as.numeric(stdErrSMD))
  )

# Apply Hedges' correction.
# Degrees of freedom vary depending on study design (look at book).
#### pick up here



## For studies with dichotomous outcomes...
# Add columns if we need to.
dichotomous_studies <- dichotomous_studies %>% add_col_if_not_exist(pExp, countExp, pCtrl, countCtrl)

# With whatever information was extracted, calculate proportions and counts per group.
dichotomous_studies <- dichotomous_studies %>%
  mutate(
    pExp = case_when(
      is.na(pExp) & !is.na(countExp)   ~ countExp / nExp,
      TRUE                             ~ as.numeric(pExp)),
    pCtrl = case_when(
      is.na(pCtrl) & !is.na(countCtrl) ~ countCtrl / nCtrl,
      TRUE                             ~ as.numeric(pCtrl)),
    countExp = case_when(
      is.na(countExp) & !is.na(pExp)   ~ floor(pExp * nExp),
      TRUE                             ~ as.numeric(countExp)),
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

# Standardized units used for meta-analysis need to be comparable across all studies in sample.
if (nrow(continuous_studies) == 0) {
  # No continuous outcomes in sample: Use escalc to get log risk ratio (RR) and log odds ratio (OR).
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
  # Continuous outcomes in sample: Use escalc to get arcsine square-root transformed risk difference (AS) for combining with continuous outcomes.
  dichotomous_studies <- escalc(measure = "AS", ai = exp_event, bi = exp_no_event, ci = ctrl_event, di = ctrl_no_event, to = "if0all", data = dichotomous_studies) %>%
    mutate(
      arcsineRiskDiff = yi,
      stdErrArcsineRiskDiff = sqrt(vi)
    ) %>%
    select(-c("yi", "vi"))
}


## Prepare output dataframe.
# Make sure dataframes have matching column names.    TODO

# Join dataframes for continuous and dichotomous outcomes.
output <- rbind(continuous_studies, dichotomous_studies) # this may not work due to column mismatch

# Drop columns with nothing in them.
output <- output[,colSums(is.na(output)) < nrow(output)]

# Sort by index to return rows to original order, and drop index column.
output <- output %>%
  arrange(index) %>%
  select(-index)

# return(list(message = "success", data = toJSON(output)))
# }

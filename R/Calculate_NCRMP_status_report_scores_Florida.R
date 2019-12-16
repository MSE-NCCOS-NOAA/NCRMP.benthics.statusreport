## Function to calculate domain level z scores values and categorical scores for percent cover of hard corals, macroalgae
## CCA, adult coral density and old mortality for the benthic component of the 2020 Status Report.
## Florida regions only

# Purpose:
# Calculate and compiles cores for the 3 Florida NCRMP regions


## Tag: data analysis


# outputs created in this file --------------
# SEFL_scores
# FLK_scores
# Tort_scores


# CallS:
# NCRMP_SR_calculate_reference_values.R
# NCRMP_SR_calculate_current_values.R
# NCRMP_calculate_Z_scores_density.R
# NCRMP_calculate_Z_scores_mortality.R
# NCRMP_calculate_Z_scores_cover.R

# output gets called by:
# Compile_NCRMP_status_report_scores.R


# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Dec 2019


##############################################################################################################################

#' Creates a dataframe of NCRMP status report benthic scores by region
#'
#'
#'
#'
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


Calculate_NCRMP_status_report_scores_Florida <- function(){

##### SEFCRI #####

### Density

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "SEFCRI",
                                           min_year = 2005,
                                           max_year = 2007,
                                           datatype = "density",
                                           reef_type = "NULL") # Can also leave blank

SEFCRI_ref_val <- tmp$SEFCRI_density_ref_values


#### Calculate current value

tmp <- NCRMP_SR_calculate_current_values(region = "SEFCRI",
                                         indicator = "NULL", # Can also leave blank
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "density",
                                         reef_type = "NULL")

SEFCRI_current_val <- tmp$SEFCRI_density_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_density(region = "SEFCRI",
                                      min_year_ref = 2005,
                                      max_year_ref = 2007,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = SEFCRI_ref_val$avDen,
                                      std = SEFCRI_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Den <- SEFCRI_ref_val$avDen
d$Current_Den <- SEFCRI_current_val$avDen

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)

Density_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99  ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha =", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Good",
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ "Fair",
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ "Impaired",
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ "Critical",
                                          current_Z < LCI_99 ~ "Critical"),
                indicator = "Density",
                Reference_val = round(Reference_Den, 1),
                Current_val = round(Current_Den, 1),
                Baseline_score = "Impaired") %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### Mortality

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "SEFCRI",
                                           min_year = 2005,
                                           max_year = 2007,
                                           datatype = "mortality")

SEFCRI_ref_val <- tmp$SEFCRI_old_mort_ref_values


#### Calculate current value

tmp <- NCRMP_SR_calculate_current_values(region = "SEFCRI",
                                         indicator = "NULL", # Can also leave blank
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "mortality",
                                         reef_type = "NULL")

SEFCRI_current_val <- tmp$SEFCRI_old_mort_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_mortality(region = "SEFCRI",
                                      min_year_ref = 2005,
                                      max_year_ref = 2007,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = SEFCRI_ref_val$avMort,
                                      std = SEFCRI_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Mort <- SEFCRI_ref_val$avMort
d$Current_Mort <- SEFCRI_current_val$avMort

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)

Mortality_scores <- d %>%
 dplyr::mutate(pvalue = NA_character_,

                score = "Not scored",
                indicator = "Old mortality",
                Reference_val = round(Reference_Mort, 1),
                Current_val = round(Current_Mort, 1),
                Baseline_score = NA_character_) %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### Coral Cover

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "SEFCRI",
                                           indicator = "HARD CORALS",
                                           min_year = 1979,
                                           max_year = 1992,
                                           datatype = "cover")

SEFCRI_ref_val <- tmp$SEFCRI_cover_ref_values


#### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "SEFCRI",
                                         indicator = "HARD CORALS",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "NULL")

SEFCRI_current_val <- tmp$SEFCRI_cover_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "SEFCRI",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 1979,
                                      max_year_ref = 1992,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = SEFCRI_ref_val$avCvr,
                                      std = SEFCRI_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- SEFCRI_ref_val$avCvr
d$Current_Cvr <- SEFCRI_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)


CoralCvr_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "Coral cover",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### Macroalgae Cover

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "SEFCRI",
                                           indicator = "MACROALGAE",
                                           min_year = 2003,
                                           max_year = 2005,
                                           datatype = "cover")

SEFCRI_ref_val <- tmp$SEFCRI_cover_ref_values

#### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "SEFCRI",
                                         indicator = "MACROALGAE",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "NULL")

SEFCRI_current_val <- tmp$SEFCRI_cover_current_values




#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "SEFCRI",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 2003,
                                      max_year_ref = 2015,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = SEFCRI_ref_val$avCvr,
                                      std = SEFCRI_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- SEFCRI_ref_val$avCvr
d$Current_Cvr <- SEFCRI_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)

Maca_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99.9  ~ paste("Significantly different p <", alpha_001),
                                          current_Z > UCI_99 & current_Z < UCI_99.9 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99  ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha =", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99.9 ~ "Critical",
                                          current_Z > UCI_99 & current_Z < UCI_99.9 ~ "Impaired",
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ "Fair",
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ "Very good",
                                          current_Z < LCI_99 ~ "Very good"),

                indicator = "Macroalgae cover",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### CCA Cover

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "SEFCRI",
                                           indicator = "CCA",
                                           min_year = 2014,
                                           max_year = 2014,
                                           datatype = "cover")

SEFCRI_ref_val <- tmp$SEFCRI_cover_ref_values


#### Calculate current value - this is what gets converted to a z score and compared to the reference value


tmp <- NCRMP_SR_calculate_current_values(region = "SEFCRI",
                                         indicator = "CCA",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "NULL")

SEFCRI_current_val <- tmp$SEFCRI_cover_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "SEFCRI",
                                      indicator = "CCA",
                                      min_year_ref = 2014,
                                      max_year_ref = 2014, # Must include at least one NCRMP year due to sample size of LTM data (2)
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = SEFCRI_ref_val$avCvr,
                                      std = SEFCRI_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- SEFCRI_ref_val$avCvr
d$Current_Cvr <- SEFCRI_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)



CCA_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99  ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha =", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Good",
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ "Fair",
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ "Impaired",
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ "Critical",
                                          current_Z < LCI_99 ~ "Critical"),
                indicator = "CCA cover",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Impaired") %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


# Compile scores

SEFCRI_scores <- dplyr::bind_rows(Density_scores, Mortality_scores, CoralCvr_scores, Maca_scores, CCA_scores) %>%

  # Rename the regions
  dplyr::mutate(REGION = forcats::fct_recode(REGION,
                                    "Southeast Florida" = "SEFCRI"))

names(SEFCRI_scores) <- c("Region", "Indicator", "Reference value",
                            "Current value", "Baseline score", "P value", "Score")


SEFCRI_scores <- SEFCRI_scores %>% dplyr::select("Region", "Indicator", "Baseline score", "P value", "Score")



##### FLK #####

### Density

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "FLK",
                                           min_year = 2005,
                                           max_year = 2007,
                                           datatype = "density",
                                           reef_type = "NULL") # Can also leave blank

FLK_ref_val <- tmp$FLK_density_ref_values


#### Calculate current value

tmp <- NCRMP_SR_calculate_current_values(region = "FLK",
                                         indicator = "NULL", # Can also leave blank
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "density",
                                         reef_type = "NULL")

FLK_current_val <- tmp$FLK_density_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_density(region = "FLK",
                                        min_year_ref = 2005,
                                        max_year_ref = 2007,
                                        min_year_current = 2016,
                                        max_year_current = 2018,
                                        reference_value = FLK_ref_val$avDen,
                                        std = FLK_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual values

d$Reference_Den <- FLK_ref_val$avDen
d$Current_Den <- FLK_current_val$avDen

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)

Density_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99  ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha =", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Fair",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Impaired",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Critical",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "Density",
                region = "FLK",
                Reference_val = round(Reference_Den, 1),
                Current_val = round(Current_Den, 1),
                Baseline_score = "Impaired") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### Mortality

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "FLK",
                                           min_year = 2005,
                                           max_year = 2007,
                                           datatype = "mortality")

FLK_ref_val <- tmp$FLK_old_mort_ref_values


#### Calculate current value

tmp <- NCRMP_SR_calculate_current_values(region = "FLK",
                                         indicator = "NULL", # Can also leave blank
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "mortality",
                                         reef_type = "NULL")

FLK_current_val <- tmp$FLK_old_mort_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_mortality(region = "FLK",
                                          min_year_ref = 2005,
                                          max_year_ref = 2007,
                                          min_year_current = 2016,
                                          max_year_current = 2018,
                                          reference_value = FLK_ref_val$avMort,
                                          std = FLK_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Mort <- FLK_ref_val$avMort
d$Current_Mort <- FLK_current_val$avMort

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)

Mortality_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99  ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha =", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99  ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Old mortality",
                region = "FLK",
                Reference_val = round(Reference_Mort, 1),
                Current_val = round(Current_Mort, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


## Coral Cover

#### Bank reefs

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "FLK",
                                           indicator = "HARD CORALS",
                                           min_year = 1974,
                                           max_year = 1999,
                                           datatype = "cover",
                                           reef_type = "bank")

FLK_ref_val <- tmp$FLK_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "FLK",
                                         indicator = "HARD CORALS",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "bank")

FLK_current_val <- tmp$FLK_cover_current_values

##### Calculate scores - Bank reefs

tmp <- NCRMP_calculate_Z_scores_cover(region = "FLK",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 1974,
                                      max_year_ref = 1999,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = FLK_ref_val$avCvr,
                                      std = FLK_ref_val$std,
                                      reef_type = "bank")


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites
ref_strat <- reference$n_strat

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- FLK_ref_val$avCvr
d$Current_Cvr <- FLK_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)


CoralCvr_scores_BA <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_95 ~ paste("Significantly different p <", alpha_05), # Very good,
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05), # Good
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05), # Fair
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_01), # Poor/Impaired
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_001)), # Critical

                score = dplyr::case_when(current_Z > UCI_95 ~ "Very good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Fair",
                                         current_Z < LCI_99 & current_Z > LCI_99.9 ~ "Impaired",
                                         current_Z < LCI_99.9 ~ "Critical"),
                indicator = "Coral cover",
                reef_type = "Bank reefs",
                region = "FLK",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Patch reefs

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "FLK",
                                           indicator = "HARD CORALS",
                                           min_year = 1974,
                                           max_year = 1999,
                                           datatype = "cover",
                                           reef_type = "patch")

FLK_ref_val <- tmp$FLK_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "FLK",
                                         indicator = "HARD CORALS",
                                         min_year = 2018,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "patch")

FLK_current_val <- tmp$FLK_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "FLK",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 1974,
                                      max_year_ref = 1999,
                                      min_year_current = 2018,
                                      max_year_current = 2018,
                                      reference_value = FLK_ref_val$avCvr,
                                      std = FLK_ref_val$std,
                                      reef_type = "patch")


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites
ref_strat <- reference$n_strat

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- FLK_ref_val$avCvr
d$Current_Cvr <- FLK_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - ref_strat

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)


CoralCvr_scores_PT <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Very good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Fair",
                                         current_Z < LCI_99 ~ "Impaired"),
                indicator = "Coral cover",
                region = "FLK",
                reef_type = "Patch reef",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


## Macroalgae

#### Bank reefs

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "FLK",
                                           indicator = "MACROALGAE",
                                           min_year = 1996,
                                           max_year = 1999,
                                           datatype = "cover",
                                           reef_type = "bank")

FLK_ref_val <- tmp$FLK_cover_ref_values

##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "FLK",
                                         indicator = "MACROALGAE",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "bank")

FLK_current_val <- tmp$FLK_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "FLK",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 1996,
                                      max_year_ref = 1999,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = FLK_ref_val$avCvr,
                                      std = FLK_ref_val$std,
                                      reef_type = "bank")


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites
ref_strat <- reference$n_strat

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- FLK_ref_val$avCvr
d$Current_Cvr <- FLK_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)


MacaCvr_scores_BA <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Impaired",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Fair",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Very good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Macroalgae cover",
                region = "FLK",
                reef_type = "Bank reefs",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Patch reefs

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "FLK",
                                           indicator = "MACROALGAE",
                                           min_year = 1999,
                                           max_year = 1999,
                                           datatype = "cover",
                                           reef_type = "patch")

FLK_ref_val <- tmp$FLK_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "FLK",
                                         indicator = "MACROALGAE",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "patch")

FLK_current_val <- tmp$FLK_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "FLK",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 1999,
                                      max_year_ref = 1999,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = FLK_ref_val$avCvr,
                                      std = FLK_ref_val$std,
                                      reef_type = "patch")


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites
ref_strat <- reference$n_strat

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- FLK_ref_val$avCvr
d$Current_Cvr <- FLK_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)


MacaCvr_scores_PT <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Impaired",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Fair",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Very good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Macroalgae cover",
                region = "FLK",
                reef_type = "Patch reefs",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


### CCA Cover

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "FLK",
                                           indicator = "CCA",
                                           min_year = 2014,
                                           max_year = 2014,
                                           datatype = "cover")

FLK_ref_val <- tmp$FLK_cover_ref_values


#### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "FLK",
                                         indicator = "CCA",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "NULL")

FLK_current_val <- tmp$FLK_cover_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "FLK",
                                      indicator = "CCA",
                                      min_year_ref = 2014,
                                      max_year_ref = 2014, # Must include at least one NCRMP year due to sample size of LTM data (2)
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = FLK_ref_val$avCvr,
                                      std = FLK_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- FLK_ref_val$avCvr
d$Current_Cvr <- FLK_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)



CCA_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99  ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha =", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Very good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Fair",
                                         current_Z < LCI_99 ~ "Impaired"),
                indicator = "CCA cover",
                region = "FLK",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


# Compile scores

FLK_scores <- dplyr::bind_rows(Density_scores,
                               Mortality_scores,
                               MacaCvr_scores_PT,
                               MacaCvr_scores_BA,
                               CoralCvr_scores_BA,
                               CoralCvr_scores_PT,
                               CCA_scores) %>%
  # Rename the regions
  dplyr::mutate(region = forcats::fct_recode(region,
                                             "Florida Keys" = "FLK"))

names(FLK_scores) <- c("Region", "Indicator", "Reference value",
                            "Current value", "Baseline score", "P value", "Score", "Reef type")


FLK_scores <- FLK_scores %>% dplyr::select("Region", "Indicator", "Reef type", "Baseline score", "P value", "Score")

##### Tortugas #####

### Density

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "Tortugas",
                                           min_year = 2007,
                                           max_year = 2009,
                                           datatype = "density",
                                           reef_type = "NULL") # Can also leave blank

Tortugas_ref_val <- tmp$Tort_density_ref_values


#### Calculate current value

tmp <- NCRMP_SR_calculate_current_values(region = "Tortugas",
                                         indicator = "NULL", # Can also leave blank
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "density",
                                         reef_type = "NULL")

Tortugas_current_val <- tmp$Tort_density_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_density(region = "Tortugas",
                                        min_year_ref = 2007,
                                        max_year_ref = 2009,
                                        min_year_current = 2016,
                                        max_year_current = 2018,
                                        reference_value = Tortugas_ref_val$avDen,
                                        std = Tortugas_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Den <- Tortugas_ref_val$avDen
d$Current_Den <- Tortugas_current_val$avDen

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)

Density_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99  ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha =", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "Density",
                region = "Tortugas",
                Reference_val = round(Reference_Den, 1),
                Current_val = round(Current_Den, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### Mortality

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "Tortugas",
                                           min_year = 2007,
                                           max_year = 2009,
                                           datatype = "mortality")

Tortugas_ref_val <- tmp$Tort_old_mort_ref_values


#### Calculate current value

tmp <- NCRMP_SR_calculate_current_values(region = "Tortugas",
                                         indicator = "NULL", # Can also leave blank
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "mortality",
                                         reef_type = "NULL")

Tortugas_current_val <- tmp$Tort_old_mort_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_mortality(region = "Tortugas",
                                          min_year_ref = 2007,
                                          max_year_ref = 2009,
                                          min_year_current = 2016,
                                          max_year_current = 2018,
                                          reference_value = Tortugas_ref_val$avMort,
                                          std = Tortugas_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Mort <- Tortugas_ref_val$avMort
d$Current_Mort <- Tortugas_current_val$avMort

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)

Mortality_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99  ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha =", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "Old mortality",
                region = "Tortugas",
                Reference_val = round(Reference_Mort, 1),
                Current_val = round(Current_Mort, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


## Coral Cover

#### Mid/high relief reefs

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "Tortugas",
                                           indicator = "HARD CORALS",
                                           min_year = 1975,
                                           max_year = 1999,
                                           datatype = "cover",
                                           reef_type = "mid/high")

Tortugas_ref_val <- tmp$Tort_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "Tortugas",
                                         indicator = "HARD CORALS",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "mid/high")

Tortugas_current_val <- tmp$Tort_cover_current_values


##### Calculate scores - Mid/high relief reefs

tmp <- NCRMP_calculate_Z_scores_cover(region = "Tortugas",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 1975,
                                      max_year_ref = 1999,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = Tortugas_ref_val$avCvr,
                                      std = Tortugas_ref_val$std,
                                      reef_type = "mid/high")


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites
ref_strat <- reference$n_strat

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- Tortugas_ref_val$avCvr
d$Current_Cvr <- Tortugas_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)


CoralCvr_scores_H <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_95 ~ paste("Significantly different p <", alpha_05), # Very good,
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05), # Good
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05), # Fair
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_01), # Poor/Impaired
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_001)), # Critical

                score = dplyr::case_when(current_Z > UCI_95 ~ "Very good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Very good",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 & current_Z > LCI_99.9 ~ "Fair",
                                         current_Z < LCI_99.9 ~ "Impaired"),
                indicator = "Coral cover",
                reef_type = "Mid to high relief reefs",
                region = "Tortugas",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Very good") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Low relief reefs

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "Tortugas",
                                           indicator = "HARD CORALS",
                                           min_year = 1975,
                                           max_year = 1999,
                                           datatype = "cover",
                                           reef_type = "low")

Tortugas_ref_val <- tmp$Tort_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "Tortugas",
                                         indicator = "HARD CORALS",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "low")

Tortugas_current_val <- tmp$Tort_cover_current_values


##### Calculate scores - Low relief reefs

tmp <- NCRMP_calculate_Z_scores_cover(region = "Tortugas",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 1975,
                                      max_year_ref = 1999,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = Tortugas_ref_val$avCvr,
                                      std = Tortugas_ref_val$std,
                                      reef_type = "low")


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites
ref_strat <- reference$n_strat

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- Tortugas_ref_val$avCvr
d$Current_Cvr <- Tortugas_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - ref_strat

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)


CoralCvr_scores_L <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Very good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Fair",
                                         current_Z < LCI_99 ~ "Impaired"),
                indicator = "Coral cover",
                region = "Tortugas",
                reef_type = "Low relief reefs",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


## Macroalgae

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "Tortugas",
                                           indicator = "MACROALGAE",
                                           min_year = 2014,
                                           max_year = 2014,
                                           datatype = "cover",
                                           reef_type = "NULL")

Tortugas_ref_val <- tmp$Tort_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "Tortugas",
                                         indicator = "MACROALGAE",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "NULL")

Tortugas_current_val <- tmp$Tort_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "Tortugas",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 2014,
                                      max_year_ref = 2014,
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = Tortugas_ref_val$avCvr,
                                      std = Tortugas_ref_val$std,
                                      reef_type = "NULL")


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites
ref_strat <- reference$n_strat

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- Tortugas_ref_val$avCvr
d$Current_Cvr <- Tortugas_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)


MacaCvr_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_95 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Critical",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Impaired",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Fair",
                                         current_Z < LCI_99 ~ "Good"),
                indicator = "Macroalgae cover",
                region = "Tortugas",
                #reef_type = "Bank reefs",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Impaired") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### CCA Cover

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "Tortugas",
                                           indicator = "CCA",
                                           min_year = 2014,
                                           max_year = 2014,
                                           datatype = "cover")

Tortugas_ref_val <- tmp$Tort_cover_ref_values


#### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "Tortugas",
                                         indicator = "CCA",
                                         min_year = 2016,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "NULL")

Tortugas_current_val <- tmp$Tort_cover_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "Tortugas",
                                      indicator = "CCA",
                                      min_year_ref = 2014,
                                      max_year_ref = 2014, # Must include at least one NCRMP year due to sample size of LTM data (2)
                                      min_year_current = 2016,
                                      max_year_current = 2018,
                                      reference_value = Tortugas_ref_val$avCvr,
                                      std = Tortugas_ref_val$std)


# Set your Z reference value
reference <- tmp$Domain_est_Z_ref
ref_val <- reference$ref_Z
ref_var <- reference$Var
ref_n <- reference$n_sites

# Set your Z current value - this becomes your current value which you are comparing to the distribution of your reference value
d <- tmp$Domain_est_Z_current
d$ref_Z <- ref_val
d$ref_Z_Var <- ref_var
d$ref_n <- ref_n

# Set the reference and current actual cover values

d$Reference_Cvr <- Tortugas_ref_val$avCvr
d$Current_Cvr <- Tortugas_current_val$avCvr

# Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
d$df <- d$ref_n - 1

# Calculate 95% CI
d$alpha_05 <- 0.05
d$t_value_95  <- abs(qt(d$alpha_05/2, d$df))
d$LCI_95      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_95)
d$UCI_95      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_95)

# Calculate 99% CI
d$alpha_01 <- 0.01
d$t_value_99  <- abs(qt(d$alpha_01/2, d$df))
d$LCI_99      <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99)
d$UCI_99      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99)

# Calculate 99.9% CI
d$alpha_001 <- 0.001
d$t_value_99.9  <- abs(qt(d$alpha_001/2, d$df))
d$LCI_99.9     <- d$ref_Z - (sqrt(d$ref_Z_Var) * d$t_value_99.9)
d$UCI_99.9      <- d$ref_Z + (sqrt(d$ref_Z_Var) * d$t_value_99.9)



CCA_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99  ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha =", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Very good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Fair",
                                         current_Z < LCI_99 ~ "Impaired"),
                indicator = "CCA cover",
                region = "Tortugas",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


# Compile scores

Tortugas_scores <- dplyr::bind_rows(Density_scores,
                                    Mortality_scores,
                                    MacaCvr_scores,
                                    CoralCvr_scores_H,
                                    CoralCvr_scores_L,
                                    CCA_scores) %>%
  # Rename the regions
  dplyr::mutate(region = forcats::fct_recode(region,
                                    "Dry Tortugas" = "Tortugas"))

names(Tortugas_scores) <- c("Region", "Indicator", "Reference value",
                  "Current value", "Baseline score", "P value", "Score", "Reef type")


Tortugas_scores <- Tortugas_scores %>% dplyr::select("Region", "Indicator", "Reef type", "Baseline score", "P value", "Score")


  ################
  # Export
  ################

  # Create list to export
output <- list(

  "SEFL_scores" = SEFCRI_scores,
  "FLK_scores" = FLK_scores,
  "Tortugas_scores" = Tortugas_scores
)
return(output)
}

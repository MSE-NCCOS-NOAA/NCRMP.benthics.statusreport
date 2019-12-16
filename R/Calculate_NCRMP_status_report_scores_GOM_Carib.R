## Function to calculate domain level z scores values and categorical scores for percent cover of hard corals, macroalgae
## CCA, adult coral density and old mortality for the benthic component of the 2020 Status Report.
## Caribbean and GOM regions only

# Purpose:
# Calculate and compiles cores for the 4 Caribbean/Gulf of Mexico NCRMP regions


## Tag: data analysis


# outputs created in this file --------------
# STTSTJ_scores
# STX_scores
# PRICO_scores
# GOM_scores


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


Calculate_NCRMP_status_report_scores_GOM_Carib <- function(){

##### St. Thomas & St. John #####

##Density

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           min_year = 2013,
                                           max_year = 2013,
                                           datatype = "density",
                                           reef_type = "High coral")

STTSTJ_ref_val <- tmp$STTSTJ_density_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "NULL",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "density",
                                         reef_type = "High coral")

STTSTJ_current_val <- tmp$STTSTJ_density_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_density(region = "STTSTJ",
                                        min_year_ref = 2013,
                                        max_year_ref = 2013,
                                        min_year_current = 2015,
                                        max_year_current = 2017,
                                        reference_value = STTSTJ_ref_val$avDen,
                                        std = STTSTJ_ref_val$std,
                                        reef_type = "High coral")


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
d$ref_strat <- ref_strat

# Set the reference and current actual cover values

d$Reference_Den <- STTSTJ_ref_val$avDen
d$Current_Den <- STTSTJ_current_val$avDen

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

Density_scores_HC <- d %>%
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
                region = "STTSTJ",
                indicator = "Density",
                reef_type = "High coral",
                Reference_val = round(Reference_Den, 1),
                Current_val = round(Current_Den, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


##Density

#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           min_year = 2013,
                                           max_year = 2013,
                                           datatype = "density",
                                           reef_type = "Low coral")

STTSTJ_ref_val <- tmp$STTSTJ_density_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "NULL",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "density",
                                         reef_type = "Low coral")

STTSTJ_current_val <- tmp$STTSTJ_density_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_density(region = "STTSTJ",
                                        min_year_ref = 2013,
                                        max_year_ref = 2013,
                                        min_year_current = 2015,
                                        max_year_current = 2017,
                                        reference_value = STTSTJ_ref_val$avDen,
                                        std = STTSTJ_ref_val$std,
                                        reef_type = "Low coral")


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
d$ref_strat <- ref_strat

# Set the reference and current actual cover values

d$Reference_Den <- STTSTJ_ref_val$avDen
d$Current_Den <- STTSTJ_current_val$avDen

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

Density_scores_LC <- d %>%
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
                indicator = "Density",
                region = "STTSTJ",
                reef_type = "Low coral",
                Reference_val = round(Reference_Den, 1),
                Current_val = round(Current_Den, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


##Mortality

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           min_year = 2013,
                                           max_year = 2013,
                                           datatype = "mortality",
                                           reef_type = "High coral")

STTSTJ_ref_val <- tmp$STTSTJ_old_mort_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "NULL",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "mortality",
                                         reef_type = "High coral")

STTSTJ_current_val <- tmp$STTSTJ_old_mort_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_mortality(region = "STTSTJ",
                                          min_year_ref = 2013,
                                          max_year_ref = 2013,
                                          min_year_current = 2015,
                                          max_year_current = 2017,
                                          reference_value = STTSTJ_ref_val$avMort,
                                          std = STTSTJ_ref_val$std,
                                          reef_type = "High coral")


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

d$Reference_Mort <- STTSTJ_ref_val$avMort
d$Current_Mort <- STTSTJ_current_val$avMort

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

Mortality_scores_HC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Old mortality",
                region = "STTSTJ",
                reef_type = "High coral",
                Reference_val = round(Reference_Mort, 1),
                Current_val = round(Current_Mort, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


##Mortality

#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           min_year = 2013,
                                           max_year = 2013,
                                           datatype = "mortality",
                                           reef_type = "Low coral")

STTSTJ_ref_val <- tmp$STTSTJ_old_mort_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "NULL",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "mortality",
                                         reef_type = "Low coral")

STTSTJ_current_val <- tmp$STTSTJ_old_mort_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_mortality(region = "STTSTJ",
                                          min_year_ref = 2013,
                                          max_year_ref = 2013,
                                          min_year_current = 2015,
                                          max_year_current = 2017,
                                          reference_value = STTSTJ_ref_val$avMort,
                                          std = STTSTJ_ref_val$std,
                                          reef_type = "Low coral")


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

d$Reference_Mort <- STTSTJ_ref_val$avMort
d$Current_Mort <- STTSTJ_current_val$avMort

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

Mortality_scores_LC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Old mortality",
                region = "STTSTJ",
                reef_type = "Low coral",
                Reference_val = round(Reference_Mort, 1),
                Current_val = round(Current_Mort, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)

## Coral Cover

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           indicator = "HARD CORALS",
                                           min_year = 1999,
                                           max_year = 2005,
                                           datatype = "cover",
                                           reef_type = "High coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "HARD CORALS",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "cover",
                                         reef_type = "High coral")

STTSTJ_current_val <- tmp$STTSTJ_cover_current_values


##### Calculate scores - High coral

tmp <- NCRMP_calculate_Z_scores_cover(region = "STTSTJ",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 1999,
                                      max_year_ref = 2005,
                                      min_year_current = 2015,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "High coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STTSTJ_current_val$avCvr

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


CoralCvr_scores_HC <- d %>%
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
                reef_type = "High coral",
                region = "STTSTJ",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           indicator = "HARD CORALS",
                                           min_year = 2013,
                                           max_year = 2013,
                                           datatype = "cover",
                                           reef_type = "Low coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "HARD CORALS",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "cover",
                                         reef_type = "Low coral")

STTSTJ_current_val <- tmp$STTSTJ_cover_current_values


##### Calculate scores - High coral

tmp <- NCRMP_calculate_Z_scores_cover(region = "STTSTJ",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 2013,
                                      max_year_ref = 2013,
                                      min_year_current = 2015,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "Low coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STTSTJ_current_val$avCvr

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


CoralCvr_scores_LC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01), # Very good
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05), # Good
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05), # Fair
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05), # Poor/Impaired
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)), # Critical

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "Coral cover",
                region = "STTSTJ",
                reef_type = "Low coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


## Macroalage

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           indicator = "MACROALGAE",
                                           min_year = 1999,
                                           max_year = 2005,
                                           datatype = "cover",
                                           reef_type = "High coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "MACROALGAE",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "cover",
                                         reef_type = "High coral")

STTSTJ_current_val <- tmp$STTSTJ_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "STTSTJ",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 1999,
                                      max_year_ref = 2005,
                                      min_year_current = 2015,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "High coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STTSTJ_current_val$avCvr

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


MacaCvr_scores_HC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Macroalgae cover",
                region = "STTSTJ",
                reef_type = "High coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           indicator = "MACROALGAE",
                                           min_year = 2013,
                                           max_year = 2013,
                                           datatype = "cover",
                                           reef_type = "Low coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "MACROALGAE",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "cover",
                                         reef_type = "Low coral")

STTSTJ_current_val <- tmp$STTSTJ_cover_current_values


##### Calculate scores

# Calculate domain level Z for current values using the two most recent NCRMP sampling years

tmp <- NCRMP_calculate_Z_scores_cover(region = "STTSTJ",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 2013,
                                      max_year_ref = 2013,
                                      min_year_current = 2015,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "Low coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STTSTJ_current_val$avCvr

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


MacaCvr_scores_LC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Macroalgae cover",
                region = "STTSTJ",
                reef_type = "Low coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


## CCA

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           indicator = "CCA",
                                           min_year = 1999,
                                           max_year = 2005,
                                           datatype = "cover",
                                           reef_type = "High coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "CCA",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "cover",
                                           reef_type = "High coral")

STTSTJ_current_val <- tmp$STTSTJ_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "STTSTJ",
                                      indicator = "CCA",
                                      min_year_ref = 1999,
                                      max_year_ref = 2005,
                                      min_year_current = 2015,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "High coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STTSTJ_current_val$avCvr

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


CCACvr_scores_HC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "CCA cover",
                region = "STTSTJ",
                reef_type = "High coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STTSTJ",
                                           indicator = "CCA",
                                           min_year = 2013,
                                           max_year = 2013,
                                           datatype = "cover",
                                           reef_type = "Low coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STTSTJ",
                                         indicator = "CCA",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "cover",
                                           reef_type = "Low coral")

STTSTJ_current_val <- tmp$STTSTJ_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "STTSTJ",
                                      indicator = "CCA",
                                      min_year_ref = 2013,
                                      max_year_ref = 2013,
                                      min_year_current = 2015,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "Low coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STTSTJ_current_val$avCvr

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


CCACvr_scores_LC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "CCA cover",
                reef_type = "Low coral",
                region = "STTSTJ",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


# Compile scores

STTSTJ_scores <- dplyr::bind_rows(
                               Density_scores_HC,
                               Density_scores_LC,
                               Mortality_scores_HC,
                               Mortality_scores_LC,
                               CoralCvr_scores_HC,
                               MacaCvr_scores_HC,
                               CCACvr_scores_HC,
                               CoralCvr_scores_LC,
                               MacaCvr_scores_LC,
                               CCACvr_scores_LC) %>%
    dplyr::arrange(., -dplyr::desc(reef_type)) %>%

  # Rename the regions
  dplyr::mutate(region = forcats::fct_recode(region,
                                    "St. Thomas & St. John" = "STTSTJ"))

names(STTSTJ_scores) <- c("Region", "Indicator", "Reef type", "Reference value",
                  "Current value", "Baseline score", "P value", "Score")


STTSTJ_scores <- STTSTJ_scores %>% dplyr::select("Region", "Indicator", "Reef type", "Baseline score", "P value", "Score")


##### St. Croix #####

##Density

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           min_year = 2015,
                                           max_year = 2015,
                                           datatype = "density",
                                           reef_type = "High coral")

STX_ref_val <- tmp$STX_density_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "NULL",
                                         min_year = 2017,
                                         max_year = 2017,
                                         datatype = "density",
                                         reef_type = "High coral")

STX_current_val <- tmp$STX_density_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_density(region = "STX",
                                        min_year_ref = 2015,
                                        max_year_ref = 2015,
                                        min_year_current = 2017,
                                        max_year_current = 2017,
                                        reference_value = STX_ref_val$avDen,
                                        std = STX_ref_val$std,
                                        reef_type = "High coral")


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
d$ref_strat <- ref_strat

# Set the reference and current actual cover values

d$Reference_Den <- STX_ref_val$avDen
d$Current_Den <- STX_current_val$avDen

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

Density_scores_HC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Fair",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Impaired",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Critical",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "Density",
                reef_type = "High coral",
                Reference_val = round(Reference_Den, 1),
                Current_val = round(Current_Den, 1),
                Baseline_score = "Impaired") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           min_year = 2015,
                                           max_year = 2015,
                                           datatype = "density",
                                           reef_type = "Low coral")

STX_ref_val <- tmp$STX_density_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "NULL",
                                         min_year = 2017,
                                         max_year = 2017,
                                         datatype = "density",
                                         reef_type = "Low coral")

STX_current_val <- tmp$STX_density_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_density(region = "STX",
                                        min_year_ref = 2015,
                                        max_year_ref = 2015,
                                        min_year_current = 2017,
                                        max_year_current = 2017,
                                        reference_value = STX_ref_val$avDen,
                                        std = STX_ref_val$std,
                                        reef_type = "Low coral")


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
d$ref_strat <- ref_strat

# Set the reference and current actual cover values

d$Reference_Den <- STX_ref_val$avDen
d$Current_Den <- STX_current_val$avDen

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

Density_scores_LC <- d %>%
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
                indicator = "Density",
                reef_type = "Low coral",
                Reference_val = round(Reference_Den, 1),
                Current_val = round(Current_Den, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


##Mortality

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           min_year = 2015,
                                           max_year = 2015,
                                           datatype = "mortality",
                                           reef_type = "High coral")

STX_ref_val <- tmp$STX_old_mort_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "NULL",
                                         min_year = 2017,
                                         max_year = 2017,
                                         datatype = "mortality",
                                         reef_type = "High coral")

STX_current_val <- tmp$STX_old_mort_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_mortality(region = "STX",
                                          min_year_ref = 2015,
                                          max_year_ref = 2015,
                                          min_year_current = 2017,
                                          max_year_current = 2017,
                                          reference_value = STX_ref_val$avMort,
                                          std = STX_ref_val$std,
                                          reef_type = "High coral")


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

d$Reference_Mort <- STX_ref_val$avMort
d$Current_Mort <- STX_current_val$avMort

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

Mortality_scores_HC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Old mortality",
                reef_type = "High coral",
                Reference_val = round(Reference_Mort, 1),
                Current_val = round(Current_Mort, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           min_year = 2015,
                                           max_year = 2015,
                                           datatype = "mortality",
                                           reef_type = "Low coral")

STX_ref_val <- tmp$STX_old_mort_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "NULL",
                                         min_year = 2017,
                                         max_year = 2017,
                                         datatype = "mortality",
                                         reef_type = "Low coral")

STX_current_val <- tmp$STX_old_mort_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_mortality(region = "STX",
                                          min_year_ref = 2015,
                                          max_year_ref = 2015,
                                          min_year_current = 2017,
                                          max_year_current = 2017,
                                          reference_value = STX_ref_val$avMort,
                                          std = STX_ref_val$std,
                                          reef_type = "Low coral")


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

d$Reference_Mort <-STX_ref_val$avMort
d$Current_Mort <- STX_current_val$avMort

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

Mortality_scores_LC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Old mortality",
                reef_type = "Low coral",
                Reference_val = round(Reference_Mort, 1),
                Current_val = round(Current_Mort, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


## Coral Cover

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           indicator = "HARD CORALS",
                                           min_year = 1999,
                                           max_year = 2005,
                                           datatype = "cover",
                                           reef_type = "High coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "HARD CORALS",
                                         min_year = 2017,
                                         max_year = 2017,
                                         datatype = "cover",
                                         reef_type = "High coral")

STX_current_val <- tmp$STX_cover_current_values


##### Calculate scores - High coral

# Calculate domain level Z for current values using the two most recent NCRMP sampling years

tmp <- NCRMP_calculate_Z_scores_cover(region = "STX",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 1999,
                                      max_year_ref = 2005,
                                      min_year_current = 2017,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "High coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STX_current_val$avCvr

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


CoralCvr_scores_HC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_95 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_01),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_001)),

                score = dplyr::case_when(current_Z > UCI_95 ~ "Very good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Fair",
                                         current_Z < LCI_99 & current_Z > LCI_99.9 ~ "Impaired",
                                         current_Z < LCI_99.9 ~ "Critical"),
                indicator = "Coral cover",
                reef_type = "High coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           indicator = "HARD CORALS",
                                           min_year = 2015,
                                           max_year = 2015,
                                           datatype = "cover",
                                           reef_type = "Low coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "HARD CORALS",
                                         min_year = 2017,
                                         max_year = 2017,
                                         datatype = "cover",
                                         reef_type = "Low coral")

STX_current_val <- tmp$STX_cover_current_values


##### Calculate scores - High coral

tmp <- NCRMP_calculate_Z_scores_cover(region = "STX",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 2015,
                                      max_year_ref = 2015,
                                      min_year_current = 2017,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "Low coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STX_current_val$avCvr

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


CoralCvr_scores_LC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "Coral cover",
                reef_type = "Low coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


## Macroalage

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           indicator = "MACROALGAE",
                                           min_year = 1999,
                                           max_year = 2005,
                                           datatype = "cover",
                                           reef_type = "High coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "MACROALGAE",
                                         min_year = 2015,
                                         max_year = 2017,
                                         datatype = "cover",
                                         reef_type = "High coral")

STX_current_val <- tmp$STX_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "STX",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 1999,
                                      max_year_ref = 2005,
                                      min_year_current = 2017,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "High coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STX_current_val$avCvr

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


MacaCvr_scores_HC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Macroalgae cover",
                reef_type = "High coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           indicator = "MACROALGAE",
                                           min_year = 2015,
                                           max_year = 2015,
                                           datatype = "cover",
                                           reef_type = "Low coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "MACROALGAE",
                                         min_year = 2017,
                                         max_year = 2017,
                                         datatype = "cover",
                                         reef_type = "Low coral")

STX_current_val <- tmp$STX_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "STX",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 2015,
                                      max_year_ref = 2015,
                                      min_year_current = 2017,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "Low coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STX_current_val$avCvr

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


MacaCvr_scores_LC <- d %>%
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
                reef_type = "Low coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


## CCA

#### High coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           indicator = "CCA",
                                           min_year = 1999,
                                           max_year = 2005,
                                           datatype = "cover",
                                           reef_type = "High coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "CCA",
                                         min_year = 2017,
                                         max_year = 2017,
                                         datatype = "cover",
                                           reef_type = "High coral")

STX_current_val <- tmp$STX_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "STX",
                                      indicator = "CCA",
                                      min_year_ref = 1999,
                                      max_year_ref = 2005,
                                      min_year_current = 2017,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "High coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STX_current_val$avCvr

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


CCACvr_scores_HC <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01), # Very good, sig. greater than ref value - baseline is Very good
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05), # Good, not sig. diff from ref value
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05), # Fair
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05), # Poor/Impaired
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)), # Critical

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "CCA cover",
                reef_type = "High coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


#### Low coral habitats

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "STX",
                                           indicator = "CCA",
                                           min_year = 2015,
                                           max_year = 2015,
                                           datatype = "cover",
                                           reef_type = "Low coral")

USVI_ref_val <- tmp$USVI_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "STX",
                                         indicator = "CCA",
                                         min_year = 2017,
                                         max_year = 2017,
                                         datatype = "cover",
                                         reef_type = "Low coral")

STX_current_val <- tmp$STX_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "STX",
                                      indicator = "CCA",
                                      min_year_ref = 2015,
                                      max_year_ref = 2015,
                                      min_year_current = 2017,
                                      max_year_current = 2017,
                                      reference_value = USVI_ref_val$avCvr,
                                      std = USVI_ref_val$std,
                                      reef_type = "Low coral")


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

d$Reference_Cvr <- USVI_ref_val$avCvr
d$Current_Cvr <- STX_current_val$avCvr

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


CCACvr_scores_LC <- d %>%
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
                indicator = "CCA cover",
                reef_type = "Low coral",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(indicator, reef_type, Reference_val, Current_val, Baseline_score, pvalue, score)


# Compile scores

STX_scores <- dplyr::bind_rows(
                               Density_scores_HC,
                               Density_scores_LC,
                               Mortality_scores_HC,
                               Mortality_scores_LC,
                               CoralCvr_scores_HC,
                               MacaCvr_scores_HC,
                               CCACvr_scores_HC,
                               CoralCvr_scores_LC,
                               MacaCvr_scores_LC,
                               CCACvr_scores_LC) %>%
    dplyr::arrange(., -dplyr::desc(reef_type)) %>%

  # Rename the regions
  dplyr::mutate(region = "St. Croix")

names(STX_scores) <- c("Indicator", "Reef type", "Reference value",
                  "Current value", "Baseline score", "P value", "Score","Region")


STX_scores <- STX_scores %>% dplyr::select("Region", "Indicator", "Reef type", "Baseline score", "P value", "Score")


##### Puerto Rico #####

##Density

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "PRICO",
                                           min_year = 2014,
                                           max_year = 2014,
                                           datatype = "density")

PRICO_ref_val <- tmp$PRICO_density_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "PRICO",
                                         indicator = "NULL",
                                         min_year = 2016,
                                         max_year = 2016,
                                         datatype = "density")

PRICO_current_val <- tmp$PRICO_density_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_density(region = "PRICO",
                                        min_year_ref = 2014,
                                        max_year_ref = 2014,
                                        min_year_current = 2016,
                                        max_year_current = 2016,
                                        reference_value = PRICO_ref_val$avDen,
                                        std = PRICO_ref_val$std)


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
d$ref_strat <- ref_strat

# Set the reference and current actual cover values

d$Reference_Den <- PRICO_ref_val$avDen
d$Current_Den <- PRICO_current_val$avDen

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
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                region = "PRICO",
                indicator = "Density",
                Reference_val = round(Reference_Den, 1),
                Current_val = round(Current_Den, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


##Mortality

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "PRICO",
                                           min_year = 2014,
                                           max_year = 2014,
                                           datatype = "mortality")

PRICO_ref_val <- tmp$PRICO_old_mort_ref_values


##### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "PRICO",
                                         indicator = "NULL",
                                         min_year = 2016,
                                         max_year = 2016,
                                         datatype = "mortality")

PRICO_current_val <- tmp$PRICO_old_mort_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_mortality(region = "PRICO",
                                          min_year_ref = 2014,
                                          max_year_ref = 2014,
                                          min_year_current = 2016,
                                          max_year_current = 2016,
                                          reference_value = PRICO_ref_val$avMort,
                                          std = PRICO_ref_val$std)


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

d$Reference_Mort <- PRICO_ref_val$avMort
d$Current_Mort <- PRICO_current_val$avMort

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
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Old mortality",
                region = "PRICO",
                Reference_val = round(Reference_Mort, 1),
                Current_val = round(Current_Mort, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator,  Reference_val, Current_val, Baseline_score, pvalue, score)

## Coral Cover

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "PRICO",
                                           indicator = "HARD CORALS",
                                           min_year = 2014,
                                           max_year = 2014,
                                           datatype = "cover")

PRICO_ref_val <- tmp$PRICO_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "PRICO",
                                         indicator = "HARD CORALS",
                                         min_year = 2016,
                                         max_year = 2016,
                                         datatype = "cover")

PRICO_current_val <- tmp$PRICO_cover_current_values


##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "PRICO",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 2014,
                                      max_year_ref = 2014,
                                      min_year_current = 2016,
                                      max_year_current = 2016,
                                      reference_value = PRICO_ref_val$avCvr,
                                      std = PRICO_ref_val$std)


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

d$Reference_Cvr <- PRICO_ref_val$avCvr
d$Current_Cvr <- PRICO_current_val$avCvr

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
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_95 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_01),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_001)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "Coral cover",
                region = "PRICO",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


## Macroalage

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "PRICO",
                                           indicator = "MACROALGAE",
                                           min_year = 2014,
                                           max_year = 2014,
                                           datatype = "cover")

PRICO_ref_val <- tmp$PRICO_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "PRICO",
                                         indicator = "MACROALGAE",
                                         min_year = 2016,
                                         max_year = 2016,
                                         datatype = "cover")

PRICO_current_val <- tmp$PRICO_cover_current_values


##### Calculate scores

# Calculate domain level Z for current values using the two most recent NCRMP sampling years

tmp <- NCRMP_calculate_Z_scores_cover(region = "PRICO",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 2014,
                                      max_year_ref = 2014,
                                      min_year_current = 2016,
                                      max_year_current = 2016,
                                      reference_value = PRICO_ref_val$avCvr,
                                      std = PRICO_ref_val$std)


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

d$Reference_Cvr <- PRICO_ref_val$avCvr
d$Current_Cvr <- PRICO_current_val$avCvr

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
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Critical",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Impaired",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                         current_Z < LCI_99 ~ "Very good"),
                indicator = "Macroalgae cover",
                region = "PRICO",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


## CCA

##### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "PRICO",
                                           indicator = "CCA",
                                           min_year = 2014,
                                           max_year = 2014,
                                           datatype = "cover",
                                           reef_type = "High coral")

PRICO_ref_val <- tmp$PRICO_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "PRICO",
                                         indicator = "CCA",
                                         min_year = 2016,
                                         max_year = 2016,
                                         datatype = "cover")

PRICO_current_val <- tmp$PRICO_cover_current_values

##### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "PRICO",
                                      indicator = "CCA",
                                      min_year_ref = 2014,
                                      max_year_ref = 2014,
                                      min_year_current = 2016,
                                      max_year_current = 2016,
                                      reference_value = PRICO_ref_val$avCvr,
                                      std = PRICO_ref_val$std)


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

d$Reference_Cvr <- PRICO_ref_val$avCvr
d$Current_Cvr <- PRICO_current_val$avCvr

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


CCACvr_scores <- d %>%
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_99 ~ paste("Significantly different p <", alpha_01),
                                          current_Z > UCI_95 & current_Z < UCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_01)),

                score = dplyr::case_when(current_Z > UCI_99 ~ "Very good",
                                         current_Z > UCI_95 & current_Z < UCI_99 ~ "Good",
                                         current_Z > LCI_95 & current_Z < UCI_95 ~ "Fair",
                                         current_Z < LCI_95 & current_Z > LCI_99 ~ "Impaired",
                                         current_Z < LCI_99 ~ "Critical"),
                indicator = "CCA cover",
                region = "PRICO",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Fair") %>%
  dplyr::select(region, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


# Compile scores

PRICO_scores <- dplyr::bind_rows(
                               Density_scores,
                               Mortality_scores,
                               CoralCvr_scores,
                               MacaCvr_scores,
                               CCACvr_scores
                               ) %>%
    #dplyr::arrange(., -dplyr::desc(indicator)) %>%

  # Rename the regions
  dplyr::mutate(region = forcats::fct_recode(region,
                                    "Puerto Rico" = "PRICO"))

names(PRICO_scores) <- c("Region", "Indicator", "Reference value",
                  "Current value", "Baseline score", "P value", "Score")


PRICO_scores <- PRICO_scores %>% dplyr::select("Region", "Indicator", "Baseline score", "P value", "Score")


##### Flower Garden Banks National Marine Sanctuary, Gulf of Mexico #####

### Density

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "GOM",
                                           min_year = 2013,
                                           max_year = 2013,
                                           datatype = "density",
                                           reef_type = "NULL") # Can also leave blank

FGBNMS_ref_val <- tmp$FGBNMS_density_ref_values

#### Calculate current value

tmp <- NCRMP_SR_calculate_current_values(region = "GOM",
                                         indicator = "NULL", # Can also leave blank
                                         min_year = 2015,
                                         max_year = 2018,
                                         datatype = "density",
                                         reef_type = "NULL")

FGBNMS_current_val <- tmp$FGBNMS_density_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_density(region = "GOM",
                                      min_year_ref = 2013,
                                      max_year_ref = 2013,
                                      min_year_current = 2015,
                                      max_year_current = 2018,
                                      reference_value = FGBNMS_ref_val$avDen,
                                      std = FGBNMS_ref_val$std)


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

d$Reference_Den <- FGBNMS_ref_val$avDen
d$Current_Den <- FGBNMS_current_val$avDen

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
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_95 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_01),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_001)),

                score = dplyr::case_when(current_Z > UCI_95 ~ "Very good",
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ "Very good",
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ "Fair",
                                          current_Z < LCI_99.9 ~ "Impaired"),
                indicator = "Density",
                Reference_val = round(Reference_Den, 1),
                Current_val = round(Current_Den, 1),
                Baseline_score = "Very good") %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### Mortality

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "GOM",
                                           min_year = 2013,
                                           max_year = 2013,
                                           datatype = "mortality")

FGBNMS_ref_val <- tmp$FGBNMS_old_mort_ref_values


#### Calculate current value

tmp <- NCRMP_SR_calculate_current_values(region = "GOM",
                                         indicator = "NULL", # Can also leave blank
                                         min_year = 2015,
                                         max_year = 2018,
                                         datatype = "mortality",
                                         reef_type = "NULL")

FGBNMS_current_val <- tmp$FGBNMS_old_mort_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_mortality(region = "GOM",
                                      min_year_ref = 2013,
                                      max_year_ref = 2013,
                                      min_year_current = 2015,
                                      max_year_current = 2018,
                                      reference_value = FGBNMS_ref_val$avMort,
                                      std = FGBNMS_ref_val$std)


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

d$Reference_Mort <- FGBNMS_ref_val$avMort
d$Current_Mort <- FGBNMS_current_val$avMort

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
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_95 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_01),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_001)),

                score = dplyr::case_when(current_Z > UCI_95 ~ "Very good",
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ "Very good",
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ "Fair",
                                          current_Z < LCI_99.9 ~ "Impaired"),
                indicator = "Old mortality",
                Reference_val = round(Reference_Mort, 1),
                Current_val = round(Current_Mort, 1),
                Baseline_score = "Very good") %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### Coral Cover

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "GOM",
                                           indicator = "HARD CORALS",
                                           min_year = 2009,
                                           max_year = 2013,
                                           datatype = "cover")

FGBNMS_ref_val <- tmp$FGBNMS_cover_ref_values


##### Calculate current value - this data is what gets converted to z scores and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "GOM",
                                         indicator = "HARD CORALS",
                                         min_year = 2015,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "NULL")

FGBNMS_current_val <- tmp$FGBNMS_cover_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "GOM",
                                      indicator = "HARD CORALS",
                                      min_year_ref = 2009,
                                      max_year_ref = 2013, # Must include at least one NCRMP year due to sample size of LTM data (n=2)
                                      min_year_current = 2015,
                                      max_year_current = 2018,
                                      reference_value = 56.75049,
                                      std = 16.48123)


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

d$Reference_Cvr <- FGBNMS_ref_val$avCvr
d$Current_Cvr <- FGBNMS_current_val$avCvr

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
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_95 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_01),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_001)),

                score = dplyr::case_when(current_Z > UCI_95 ~ "Very good",
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ "Very good",
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ "Good",
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ "Fair",
                                          current_Z < LCI_99.9 ~ "Impaired"),
                indicator = "Coral cover",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Very good") %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### Macroalgae Cover

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "GOM",
                                           indicator = "MACROALGAE",
                                           min_year = 1992,
                                           max_year = 2013,
                                           datatype = "cover")

FGBNMS_ref_val <- tmp$FGBNMS_cover_ref_values


#### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "GOM",
                                         indicator = "MACROALGAE",
                                         min_year = 2015,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "NULL")

FGBNMS_current_val <- tmp$FGBNMS_cover_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "GOM",
                                      indicator = "MACROALGAE",
                                      min_year_ref = 1992,
                                      max_year_ref = 2013,
                                      min_year_current = 2015,
                                      max_year_current = 2018,
                                      reference_value = FGBNMS_ref_val$avCvr,
                                      std = FGBNMS_ref_val$std)


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

d$Reference_Cvr <- FGBNMS_ref_val$avCvr
d$Current_Cvr <- FGBNMS_current_val$avCvr

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
  dplyr::mutate(pvalue = NA_character_,

                score = "Not scored",

                indicator = "Macroalgae cover",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = NA_character_) %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


### CCA Cover

#### Calculate reference value

tmp <- NCRMP_SR_calculate_reference_values(region = "GOM",
                                           indicator = "CCA",
                                           min_year = 2009,
                                           max_year = 2013,
                                           datatype = "cover")

FGBNMS_ref_val <- tmp$FGBNMS_cover_ref_values


#### Calculate current value - this is what gets converted to a z score and compared to the reference value

tmp <- NCRMP_SR_calculate_current_values(region = "GOM",
                                         indicator = "CCA",
                                         min_year = 2015,
                                         max_year = 2018,
                                         datatype = "cover",
                                         reef_type = "NULL")

FGBNMS_current_val <- tmp$FGBNMS_cover_current_values


#### Calculate scores

tmp <- NCRMP_calculate_Z_scores_cover(region = "GOM",
                                      indicator = "CCA",
                                      min_year_ref = 2009,
                                      max_year_ref = 2013, # Must include at least one NCRMP year due to sample size of LTM data (2)
                                      min_year_current = 2015,
                                      max_year_current = 2018,
                                      reference_value = 3.29263,
                                      std = 2.986127)


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

d$Reference_Cvr <- FGBNMS_ref_val$avCvr
d$Current_Cvr <- FGBNMS_current_val$avCvr

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
  dplyr::mutate(pvalue = dplyr::case_when(current_Z > UCI_95 ~ paste("Significantly different p <", alpha_05),
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ paste("NOT significant at alpha = ", alpha_05),
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ paste("Significantly different p <", alpha_05),
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ paste("Significantly different p <", alpha_01),
                                          current_Z < LCI_99.9 ~ paste("Significantly different p <", alpha_001)),

                score = dplyr::case_when(current_Z > UCI_95 ~ "Excellent",
                                          current_Z > LCI_95 & current_Z < UCI_95 ~ "Good",
                                          current_Z < LCI_95 & current_Z > LCI_99 ~ "Fair",
                                          current_Z < LCI_99 & current_Z > LCI_99.9 ~ "Poor/Impaired",
                                          current_Z < LCI_99.9 ~ "Critical"),
                indicator = "CCA cover",
                Reference_val = round(Reference_Cvr, 1),
                Current_val = round(Current_Cvr, 1),
                Baseline_score = "Good") %>%
  dplyr::select(REGION, indicator, Reference_val, Current_val, Baseline_score, pvalue, score)


## Compile scores

GOM_scores <- dplyr::bind_rows(Density_scores,
                               Mortality_scores,
                               CoralCvr_scores,
                               Maca_scores,
                               CCA_scores) %>%

  # Rename the regions
  dplyr::mutate(REGION = forcats::fct_recode(REGION,
                                    "Flower Garden Banks NMS" = "GOM"))

names(GOM_scores) <- c("Region", "Indicator", "Reference value",
                  "Current value", "Baseline score", "P value", "Score")


GOM_scores <- GOM_scores %>% dplyr::select("Region", "Indicator", "Baseline score", "P value", "Score")



  ################
  # Export
  ################

  # Create list to export
output <- list(

  "STTSTJ_scores" = STTSTJ_scores,
  "STX_scores" = STX_scores,
  "PRICO_scores" = PRICO_scores,
  "GOM_scores" = GOM_scores
)
return(output)
}

## Function to compile the 2020 NCRMP status report scores for all Atlantic regions

# Purpose:
# create csv files all benthic indicator scores


## Tag: data analysis


# outputs created in this file --------------
# NCRMP_StatusReport_2020_benthic_scores


# CallS:
# Calculate_NCRMP_status_report_scores.R
# Calculate_NCRMP_status_report_scores_Florida.R

# output gets called by:
# You, this is the end of the line!


# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Dec 2019


##############################################################################################################################

#' Creates a dataframe of NCRMP status report benthic scores for all regions
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


Compile_NCRMP_status_report_scores <- function(region){



tmp <- Calculate_NCRMP_status_report_scores_GOM_Carib()

 # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])

CaribGOM_scores <- dplyr::bind_rows(GOM_scores, PRICO_scores, STTSTJ_scores, STX_scores)

tmp <- Calculate_NCRMP_status_report_scores_Florida()

 # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


FL_scores <- dplyr::bind_rows(SEFL_scores, FLK_scores, Tortugas_scores)

All_scores <- dplyr::bind_rows(CaribGOM_scores, FL_scores)

All_scores <- All_scores %>% dplyr::select("Region", "Indicator", "Reef type", "Baseline score", "P value", "Score") %>%
dplyr::arrange(., -dplyr::desc(Region))



  ################
  # Export
  ################

  # Create list to export
  output <- list(

    "NCRMP_StatusReport_2020_benthic_scores" = All_scores

)
  return(output)
}

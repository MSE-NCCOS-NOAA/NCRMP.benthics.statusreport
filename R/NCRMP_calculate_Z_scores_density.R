## Function to calculate domain level z scores for density

# Purpose:
# creates csv files with colony density.


## Tag: data analysis


# outputs created in this file --------------
# Domain_est_Z_ref
# Domain_est_Z_current


# CallS:
# NCRMP_DRM_calculate_colony_density.R
# DRM_SCREAM_calculate_colony_density.R
# Site level data

# output gets called by:
# Calculate_NCRMP_status_report_scores.R


# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Dec 2019


##############################################################################################################################

#' Creates colony density summary dataframes
#'
#'
#'
#'
#' @param region A string indicating the region
#' @param min_year_ref  A value indicating the first year of sampling to be included in the reference data
#' @param max_year_ref A value indicating the last year of sampling to be included in the reference data
#' @param min_year_current  A value indicating the first year of sampling to be included in the current data
#' @param max_year_current A value indicating the last year of sampling to be included in the current data
#' @param reference_value A value indcating the reference mean
#' @param std A value indcating the reference std
#' @param reef_type  A string indicating reef classification
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_calculate_Z_scores_density <- function(region, min_year_ref, max_year_ref, min_year_current, max_year_current, reference_value, std, reef_type){


  if(region == "SEFCRI"){

    # Create filtered site level NCRMP data which is not stored in package

    tmp <- DRM_SCREAM_calculate_colony_density(project = "DRM",
                                               species_filter = TRUE)

    DRM_FL_2005_13_density_site <- tmp$density_site


    # Combine reference year for FGB LTM data, calculate site mean
    dat_ref <- DRM_FL_2005_13_density_site %>%

      dplyr::filter(YEAR <= max_year_ref,
                    YEAR >= min_year_ref,
                    REGION == "SEFCRI") %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, PROT, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


    tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP_DRM",
                                              region = "SEFCRI",
                                              species_filter = TRUE)

    NCRMP_SEFCRI_2014_18_density_site <- tmp$density_site


    dat_current <- NCRMP_SEFCRI_2014_18_density_site %>%

      dplyr::filter(YEAR <= max_year_current,
                    YEAR >= min_year_current) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT,  PROT, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


  }

  if(region == "FLK"){

    # Create filtered site level NCRMP data which is not stored in package

    tmp <- DRM_SCREAM_calculate_colony_density(project = "DRM",
                                               species_filter = TRUE)

    DRM_FL_2005_13_density_site <- tmp$density_site


    # Combine reference year for FGB LTM data, calculate site mean
    dat_ref <- DRM_FL_2005_13_density_site %>%

      dplyr::filter(YEAR <= max_year_ref,
                    YEAR >= min_year_ref,
                    REGION == "FLK") %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT,  PROT, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


    tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP_DRM",
                                              region = "FLK",
                                              species_filter = TRUE)

    NCRMP_FLK_2014_18_density_site <- tmp$density_site


    dat_current <- NCRMP_FLK_2014_18_density_site %>%

      dplyr::filter(YEAR <= max_year_current,
                    YEAR >= min_year_current) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, PROT, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


  }

  if(region == "Tortugas"){

    # Create filtered site level NCRMP data which is not stored in package

    tmp <- DRM_SCREAM_calculate_colony_density(project = "DRM",
                                               species_filter = TRUE)

    DRM_FL_2005_13_density_site <- tmp$density_site


    # Combine reference year for FGB LTM data, calculate site mean
    dat_ref <- DRM_FL_2005_13_density_site %>%

      dplyr::filter(YEAR <= max_year_ref,
                    YEAR >= min_year_ref,
                    REGION == "Tortugas") %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, PROT, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


    tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP_DRM",
                                              region = "Tortugas",
                                              species_filter = TRUE)

    NCRMP_Tort_2014_18_density_site <- tmp$density_site


    dat_current <- NCRMP_Tort_2014_18_density_site %>%

      dplyr::filter(YEAR <= max_year_current,
                    YEAR >= min_year_current) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, PROT, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


  }

  # Carib / GOM

  if(region == "STTSTJ"){

    # Create filtered site level NCRMP data which is not stored in package

    tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                              region = "STTSTJ",
                                              species_filter = TRUE)

    NCRMP_STTSTJ_2013_17_density_site <- tmp$density_site

    # Combine reference year for FGB LTM data, calculate site mean
    dat_ref <- NCRMP_STTSTJ_2013_17_density_site %>%
      dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                       if_else(HABITAT_CD == "PTRF", "High coral",
                                               if_else(HABITAT_CD == "BDRK", "High coral",
                                                       if_else(STRAT == "HARD_DEEP", "High coral",
                                                               if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                       if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                               if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
      dplyr::filter(YEAR <= max_year_ref,
                    YEAR >= min_year_ref
                    ,
                    reef_cat == reef_type
      ) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, reef_cat, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


    dat_current <- NCRMP_STTSTJ_2013_17_density_site %>%
      dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                       if_else(HABITAT_CD == "PTRF", "High coral",
                                               if_else(HABITAT_CD == "BDRK", "High coral",
                                                       if_else(STRAT == "HARD_DEEP", "High coral",
                                                               if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                       if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                               if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
      dplyr::filter(YEAR <= max_year_current,
                    YEAR >= min_year_current
                    ,
                    reef_cat == reef_type
      ) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, reef_cat, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


  }

  if(region == "STX"){

    # Create filtered site level NCRMP data which is not stored in package

    tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                              region = "STX",
                                              species_filter = TRUE)

    NCRMP_STX_2015_17_density_site <- tmp$density_site

    # Combine reference year for FGB LTM data, calculate site mean
    dat_ref <- NCRMP_STX_2015_17_density_site %>%
      dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                       if_else(HABITAT_CD == "PTRF", "High coral",
                                               if_else(HABITAT_CD == "BDRK", "High coral",
                                                       if_else(STRAT == "HARD_DEEP", "High coral",
                                                               if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                       if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                               if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
      dplyr::filter(YEAR <= max_year_ref,
                    YEAR >= min_year_ref
                    ,
                    reef_cat == reef_type
      ) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, reef_cat, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


    dat_current <- NCRMP_STX_2015_17_density_site %>%
      dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                       if_else(HABITAT_CD == "PTRF", "High coral",
                                               if_else(HABITAT_CD == "BDRK", "High coral",
                                                       if_else(STRAT == "HARD_DEEP", "High coral",
                                                               if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                       if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                               if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
      dplyr::filter(YEAR <= max_year_current,
                    YEAR >= min_year_current
                    ,
                    reef_cat == reef_type
      ) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, reef_cat, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))



  }


  if(region == "PRICO"){

      # Create filtered site level NCRMP data which is not stored in package

    tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                              region = "PRICO",
                                              species_filter = TRUE)

    NCRMP_PRICO_2014_16_density_site <- tmp$density_site

    dat_ref <- NCRMP_PRICO_2014_16_density_site %>%
           dplyr::filter(YEAR <= max_year_ref,
                    YEAR >= min_year_ref) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


    dat_current <- NCRMP_PRICO_2014_16_density_site %>%
         dplyr::filter(YEAR <= max_year_current,
                    YEAR >= min_year_current) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, STRAT, DENSITY) %>%

      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std))


  }

  if(region == "GOM"){

    # Create filtered site level NCRMP data which is not stored in package

    tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                              region = "GOM",
                                              species_filter = TRUE)

    NCRMP_FGBNMS_2013_18_density_site <- tmp$density_site

    # Combine reference year for FGB LTM data, calculate site mean
    dat_ref <- FGBNMS_LTM_2017_density_site %>%
      dplyr::filter(YEAR <= max_year_ref,
                    YEAR >= min_year_ref) %>%
      dplyr::group_by(REGION, SURVEY, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT) %>%
      dplyr::summarise(DENSITY = mean(DENSITY)) %>%

      # combine FGB LTM and NCRMP site level data
      dplyr::bind_rows(NCRMP_FGBNMS_2013_18_density_site %>%
                         dplyr::filter(YEAR <= max_year_ref,
                                       YEAR >= min_year_ref) %>%
                         dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%
      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std)) %>%
      dplyr::ungroup()


    dat_current <- FGBNMS_LTM_2017_density_site %>%
      dplyr::filter(YEAR <= max_year_current,
                    YEAR >= min_year_current) %>%
      dplyr::group_by(REGION, SURVEY, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT) %>%
      dplyr::summarise(DENSITY = mean(DENSITY)) %>%

      # combine FGB LTM and NCRMP site level data
      dplyr::bind_rows(NCRMP_FGBNMS_2013_18_density_site %>%
                         dplyr::filter(YEAR <= max_year_current,
                                       YEAR >= min_year_current) %>%
                         dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%
      dplyr::mutate(DENSITY = ((DENSITY - reference_value)/std)) %>%
      dplyr::ungroup()

  }


  # Add NTOT, # Cells sampled and calculate sampling weights in weighting function
  if(region == "SEFCRI" ||
     region == "FLK" ||
     region == "Tortugas") {
    tmp  <- NCRMP_make_weighted_demo_Zscores_FL(input_ref = dat_ref,
                                                input_current = dat_current,
                                                region,
                                                datatype = "density",
                                                reef_type)
  } else {

    tmp <- NCRMP_make_weighted_demo_Zscores_GOM_Carib(input_ref = dat_ref,
                                                      input_current = dat_current,
                                                      region,
                                                      datatype = "density",
                                                      reef_type)
  }


  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  ################
  # Export
  ################


  # Create list to export
  output <- list(
    "Domain_est_Z_ref" = Domain_est_Z_ref,
    'Domain_est_Z_current' = Domain_est_Z_current)



  return(output)

}




## Function to calculate domain level z scores for percent cover of hard corals, macroalgae and CCA

# Purpose:
# create csv files with percent cover by site at the species and group levels


## Tag: data analysis


# outputs created in this file --------------
# Z score Domain estimates


# CallS:
# site level NCRMP data
# NCRMP formatted literature sourced and long-term monitoring data

# output gets called by:
# Calculate_NCRMP_status_report_scores.R


# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Jan 2023


##############################################################################################################################

#' Creates percent cover dataframe
#'
#'
#'
#'
#' @param region A string indicating the region
#' @param indicator  A string indicating benthic metric
#' @param min_year_ref  A value indicating the first year of sampling to be included in the reference data
#' @param max_year_ref A value indicating the last year of sampling to be included in the reference data
#' @param min_year_current  A value indicating the first year of sampling to be included in the current data
#' @param max_year_current A value indicating the last year of sampling to be included in the current data
#' @param reference_value A value indcating the reference mean
#' @param std A value indcating the reference std
#' @param reef_cat  A string indicating reef classification
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "if_else"
#' @importFrom dplyr "group_by"
#' @export
#'
#'


NCRMP_calculate_Z_scores_cover <- function(region, indicator, min_year_ref, max_year_ref, min_year_current, max_year_current, reference_value, std, reef_type = "NULL"){


  # Load analysis ready (AR) data
  # Florida

  if(region == "SEFCRI"){

    if(indicator == "HARD CORALS"){

      dat_ref <- SEFCRI_1979_92_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

      dat_current <- NCRMP_SEFCRI_2014_18_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator
                      ) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

    }
    if(indicator == "MACROALGAE"){

      dat_ref <- SECREMP_2003_2018_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

      dat_current <- NCRMP_SEFCRI_2014_18_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))
    }

    if(indicator == "CCA"){

      dat_ref <- NCRMP_SEFCRI_2014_18_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

      dat_current <- NCRMP_SEFCRI_2014_18_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

    }

  }

  if(region == "FLK"){

    if(indicator == "HARD CORALS" || indicator == "MACROALGAE" ){

      dat_ref <- FLK_1974_00_percent_cover_site_CREMP_cond %>%

        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator,
                      STRAT == reef_type) %>%
        dplyr::mutate(reef_cat = STRAT) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT, reef_cat, HABITAT_CD, PROT, cover_group) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


      dat_current <- NCRMP_FLK_2014_18_percent_cover_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(STRAT == "FDLR", "bank",
                                         dplyr::if_else(STRAT == "FMLR", "bank",
                                                 dplyr::if_else(STRAT == "FSLR", "bank",
                                                         dplyr::if_else(STRAT == "HRRF", "bank",
                                                                 dplyr::if_else(STRAT == "INPR", "patch",
                                                                         dplyr::if_else(STRAT == "MCPR", "patch",
                                                                                 dplyr::if_else(STRAT == "OFPR", "patch", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator,
                      reef_cat == reef_type) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT, reef_cat, HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

    } else {

      dat_ref <- NCRMP_FLK_2014_18_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


      dat_current <- NCRMP_FLK_2014_18_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))
    }

  }


  if(region == "Tortugas"){

    if(indicator == "HARD CORALS"){

      dat_ref <- Tort_1975_99_percent_cover_site_CREMP_cond %>%

        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator,
                      ANALYSIS_STRATUM == reef_type) %>%
        dplyr::mutate(reef_cat = ANALYSIS_STRATUM) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT, reef_cat, HABITAT_CD, PROT, cover_group) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


      dat_current <- NCRMP_Tort_2014_18_percent_cover_site %>%

        mutate(reef_cat = dplyr::if_else(STRAT == "CONT_MR", "mid/high",
                                  dplyr::if_else(STRAT == "CONT_LR", "low",
                                          dplyr::if_else(STRAT == "CONT_HR", "mid/high",
                                                  dplyr::if_else(STRAT == "SPGR_LR", "low",
                                                          dplyr::if_else(STRAT == "SPGR_HR", "mid/high",
                                                                  dplyr::if_else(STRAT == "ISOL_HR", "mid/high",
                                                                          dplyr::if_else(STRAT == "ISOL_LR", "low",
                                                                                  dplyr::if_else(STRAT == "ISOL_MR", "mid/high", "other"))))))))) %>%
        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator,
                      reef_cat == reef_type) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT, reef_cat, HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

    } else {

      dat_ref <- NCRMP_Tort_2014_18_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


      dat_current <-  NCRMP_Tort_2014_18_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


    }
  }

  # GOM / Carib

  if(region == "STTSTJ"){

    if(reef_type == "High coral"){


      dat_ref <- USVI_1999_18_percent_cover_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other"))))))),
                      REGION = "STTSTJ") %>%
        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator
                      ,
                      reef_cat == reef_type
        ) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


      dat_current <- NCRMP_STTSTJ_2013_17_percent_cover_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator
                      ,
                      reef_cat == reef_type
        ) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

    }

    if(reef_type == "Low coral"){

      # Combine reference year for FGB LTM data, calculate site mean
      dat_ref <- NCRMP_STTSTJ_2013_17_percent_cover_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator,
                      reef_cat == reef_type) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


      dat_current <- NCRMP_STTSTJ_2013_17_percent_cover_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator,
                      reef_cat == reef_type) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%

        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


    }

  }

  if(region == "STX"){

    if(reef_type == "High coral"){

      # Combine reference year for FGB LTM data, calculate site mean
      dat_ref <- USVI_1999_18_percent_cover_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other"))))))),
                      REGION = "STX") %>%
        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator
                      ,
                      reef_cat == reef_type
        ) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


      dat_current <- NCRMP_STX_2015_17_percent_cover_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator
                      ,
                      reef_cat == reef_type
        ) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

    }

    if(reef_type == "Low coral"){

      # Combine reference year for FGB LTM data, calculate site mean
      dat_ref <- NCRMP_STX_2015_17_percent_cover_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year_ref,
                      YEAR >= min_year_ref,
                      cover_group == indicator,
                      reef_cat == reef_type) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, Percent_Cvr) %>%
        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


      dat_current <- NCRMP_STX_2015_17_percent_cover_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year_current,
                      YEAR >= min_year_current,
                      cover_group == indicator,
                      reef_cat == reef_type) %>%
        dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%

        #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
        dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


    }



  }

  if(region == "PRICO" && reef_type == "NULL"){

    # Combine reference year for FGB LTM data, calculate site mean
    dat_ref <- NCRMP_PRICO_2014_16_percent_cover_site %>%

      dplyr::filter(YEAR <= max_year_ref,
                    YEAR >= min_year_ref,
                    cover_group == indicator) %>%
      dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%
      dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%
      dplyr::ungroup() %>%
      #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
      dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


    dat_current <- NCRMP_PRICO_2014_16_percent_cover_site %>%

      dplyr::filter(YEAR <= max_year_current,
                    YEAR >= min_year_current,
                    cover_group == indicator) %>%
      dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n, Percent_Cvr) %>%
      #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
      dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))

  }


  if(region == "GOM" && reef_type == "NULL"){

    # Combine reference year for FGB LTM data, calculate site mean
    dat_ref <- FGBNMS_1992_18_percent_cover_site %>%
      dplyr::filter(YEAR <= max_year_ref,
                    YEAR >= min_year_ref,
                    cover_group == indicator) %>%
      dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, MIN_DEPTH, MAX_DEPTH, LAT_DEGREES, LON_DEGREES, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n) %>%
      dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%

      # combine FGB LTM and NCRMP site level data
      dplyr::bind_rows(NCRMP_FGBNMS_2013_18_percent_cover_site %>%
                         dplyr::filter(YEAR <= max_year_ref,
                                       YEAR >= min_year_ref,
                                       cover_group == indicator) %>%
                         dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%
      #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
      dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


    dat_current <- FGBNMS_1992_18_percent_cover_site %>%
      dplyr::filter(YEAR <= max_year_current,
                    YEAR >= min_year_current,
                    cover_group == indicator) %>%
      dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, MIN_DEPTH, MAX_DEPTH, LAT_DEGREES, LON_DEGREES, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n) %>%
      dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%

      # combine FGB LTM and NCRMP site level data
      dplyr::bind_rows(NCRMP_FGBNMS_2013_18_percent_cover_site %>%
                         dplyr::filter(YEAR <= max_year_current,
                                       YEAR >= min_year_current,
                                       cover_group == indicator) %>%
                         dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%
      #dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)/std))
      dplyr::mutate(Percent_Cvr = ((Percent_Cvr - reference_value)))


  }



  # Add NTOT, # Cells sampled and calculate sampling weights in weighting function
  if(region == "SEFCRI" ||
     region == "FLK" ||
     region == "Tortugas") {
    tmp  <- NCRMP_make_weighted_LPI_Zscores_FL(input_ref = dat_ref,
                                               input_current = dat_current,
                                               region,
                                               reef_type,
                                               indicator)
  } else {

    tmp <- NCRMP_make_weighted_LPI_Zscores_GOM_Carib(input_ref = dat_ref,
                                                     input_current = dat_current,
                                                     region,
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

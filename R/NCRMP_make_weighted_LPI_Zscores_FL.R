## Function to calculate weighted percent cover by strata and region

# Purpose:
# support function to calculate weighted percent cover data


## Tag: data analysis


# outputs created in this file --------------
# Domain_est_Z_ref
# Domain_est_Z_current

# Weighting scheme:
# Region specific


# CallS:
# analysis ready data

# output gets called by:
# NCRMP_calculate_Z_scores_cover.R
#

# NCRMP Caribbean Benthic analytics team: Groves and Viehman
# Last update: Aug 2019


##############################################################################################################################

#' Creates weighted LPI data
#'
#'
#'
#'
#' @param input_ref A dataframe
#' @param input_current A dataframe
#' @param region A string indicating the region
#' @param reef_type  A string indicating reef classification
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_make_weighted_LPI_Zscores_FL <- function(input_ref, input_current, region, reef_type, indicator = "NULL"){


  #### Read in ntot ####

  # Note that any strata not sampled in a given year MUST be removed from the NTOT df before ngrot is calculated. Not an issue when two sampling years are combined EXCEPT
  # St. Croix 2015 + 2017 (HARD SHLW not sampled either year; See NCRMP_make_weighted_LPI_Zscores_GOM_Carib.R) and Tortugas 2016 + 2018 (ISOL_LR / PROT = 0 not sampled).


  ## Florida
  # SE FL
  if(region == "SEFCRI"){

    ntot <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "SEFCRI"
                    # ,
                    # STRAT != "DPRC"
                    ) %>%
      dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = ""),
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ) %>%
      dplyr::select(-YEAR)

  }

  # FL Keys
  if(region == "FLK"){

    if(indicator == "HARD CORALS" || indicator == "MACROALGAE" ){

      ntot <- FL_2018_NTOT %>%
        dplyr::mutate(reef_cat = if_else(STRAT == "FDLR", "bank",
                                         if_else(STRAT == "FMLR", "bank",
                                                 if_else(STRAT == "FSLR", "bank",
                                                         if_else(STRAT == "HRRF", "bank",
                                                                 if_else(STRAT == "INPR", "patch",
                                                                         if_else(STRAT == "MCPR", "patch",
                                                                                 if_else(STRAT == "OFPR", "patch", "other")))))))) %>%
        dplyr::filter(REGION == "FLK",
                      reef_cat == reef_type) %>%

        dplyr::group_by(REGION, YEAR, reef_cat, STRAT, PROT, GRID_SIZE, RUG_CD) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::select(-YEAR)

    } else {

      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::select(-YEAR)


    }


  }

  # Tortugas
  if(region == "Tortugas"){

    if(indicator == "HARD CORALS"){

      ntot <- FL_2018_NTOT %>%

        mutate(reef_cat = if_else(STRAT == "CONT_MR", "mid/high",
                                  if_else(STRAT == "CONT_LR", "low",
                                          if_else(STRAT == "CONT_HR", "mid/high",
                                                  if_else(STRAT == "SPGR_LR", "low",
                                                          if_else(STRAT == "SPGR_HR", "mid/high",
                                                                  if_else(STRAT == "ISOL_HR", "mid/high",
                                                                          if_else(STRAT == "ISOL_LR", "low",
                                                                                  if_else(STRAT == "ISOL_MR", "mid/high", "other"))))))))) %>%
        dplyr::filter(REGION == "Tortugas",
                      reef_cat == reef_type) %>%

        dplyr::group_by(REGION, YEAR, reef_cat, STRAT, PROT, GRID_SIZE, RUG_CD) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0") %>%
        dplyr::select(-YEAR)

    } else {

      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0") %>%
        dplyr::select(-YEAR)


    }
  }

  ntot <- ntot %>%
    dplyr::mutate(ngrtot = sum(NTOT))  %>%
    dplyr::mutate(wh = NTOT/ngrtot)

  if(region == "FLK" && indicator == "HARD CORALS" ||
     region == "FLK" && indicator == "MACROALGAE" ||
     region == "Tortugas" && indicator == "HARD CORALS") {

    cover_est_ref <- input_ref %>%
      # group by analysis level strata
      dplyr::group_by(
        reef_cat
        #REGION
      ) %>% # Modify this line to changes analysis substrate
      dplyr::summarise(# compute average cover
        ref_Z = mean(Percent_Cvr),
        # compute stratum variance
        svar = var(Percent_Cvr),
        # calculate N
        n_sites = length(Percent_Cvr),
        n_strat = length(unique(ANALYSIS_STRATUM))) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n_sites) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        #REGION,
        reef_cat, ref_Z, svar, n_sites, n_strat, Var)



    # Calculate Z score avdns, svar, n and std for the current data
    cover_est_current <- input_current %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      # group by analysis level strata
      dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis substrate
      dplyr::summarise(# compute average Percent_Cvr
        avcvr = mean(Percent_Cvr),
        # compute stratum variance
        svar = var(Percent_Cvr),
        # calculate N
        n_sites = length(Percent_Cvr),
        n_strat = length(unique(ANALYSIS_STRATUM))) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n_sites, #variance of mean Percent_Cvr in stratum
                    std = sqrt(svar), # std dev of Percent_Cvr in stratum
                    SE=sqrt(Var)) #SE of the mean Percent_Cvr in stratum

    cover_est_current <- cover_est_current %>%
      # Merge ntot
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavcvr = wh * avcvr,
                    whvar = wh^2 * Var)  %>%
      dplyr::ungroup()

    ## Domain Estimates
    # region/population means
    Domain_est_current <- cover_est_current %>%
      dplyr::group_by(
        #REGION
        reef_cat
      ) %>%
      dplyr::summarise(current_Z = sum(whavcvr, na.rm = T),
                       Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                       n_sites = sum(n_sites),
                       strat_num = length(unique(ANALYSIS_STRATUM)))  %>%
      dplyr::ungroup()


  } else {

    cover_est_ref <- input_ref %>%
      # group by analysis level strata
      dplyr::group_by(REGION) %>% # Modify this line to changes analysis stratum
      dplyr::summarise(# compute average cover
        ref_Z = mean(Percent_Cvr),
        # compute stratum variance
        svar = var(Percent_Cvr),
        # calculate N
        n_sites = length(Percent_Cvr),
        n_strat = 1) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n_sites,
                    SE=sqrt(Var)) %>%
      dplyr::ungroup() %>%
      dplyr::select(REGION, ref_Z, svar, n_sites, n_strat, Var, SE)



    # Calculate Z score avdns, svar, n and std for the current data
    cover_est_current <- input_current %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      # group by analysis level strata
      dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
      dplyr::summarise(# compute average Percent_Cvr
        avcvr = mean(Percent_Cvr),
        # compute stratum variance
        svar = var(Percent_Cvr),
        # calculate N
        n_sites = length(Percent_Cvr),
        n_strat = length(unique(ANALYSIS_STRATUM))) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n_sites, #variance of mean Percent_Cvr in stratum
                    std = sqrt(svar), # std dev of Percent_Cvr in stratum
                    SE=sqrt(Var)) #SE of the mean Percent_Cvr in stratum

    cover_est_current <- cover_est_current %>%
      # Merge ntot
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavcvr = wh * avcvr,
                    whvar = wh^2 * Var)  %>%
      dplyr::ungroup()

    ## Domain Estimates
    # region/population means
    Domain_est_current <- cover_est_current %>%
      dplyr::group_by(REGION) %>%
      dplyr::summarise(current_Z = sum(whavcvr, na.rm = T),
                       Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                       n_sites = sum(n_sites),
                       strat_num = length(unique(ANALYSIS_STRATUM)))  %>%
      dplyr::ungroup()

  }

  ################
  # Export
  ################


  # Create list to export
  output <- list(
    "Domain_est_Z_ref" = cover_est_ref,
    'Domain_est_Z_current' = Domain_est_current
  )

  return(output)





}

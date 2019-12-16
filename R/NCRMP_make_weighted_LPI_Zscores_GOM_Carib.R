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
# Last update: July 2019


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

NCRMP_make_weighted_LPI_Zscores_GOM_Carib <- function(input_ref, input_current, region, reef_type){

  #### Read in ntots ####

  ## USVI
  # St Thomas - St John

  if(region == "STTSTJ"){

    # Create NTOT
    ntot <- USVI_2017_NTOT %>%
      dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                       if_else(HABITAT_CD == "PTRF", "High coral",
                                               if_else(HABITAT_CD == "BDRK", "High coral",
                                                       if_else(STRAT == "HARD_DEEP", "High coral",
                                                               if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                       if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                               if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
      dplyr::filter(REGION == "STTSTJ"
                    ,
                    reef_cat == reef_type
      ) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD, reef_cat) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(YEAR) %>%
      dplyr::mutate(ngrtot = sum(NTOT),
                    wh = NTOT/ngrtot) %>%
      dplyr::ungroup()
  }

  # St Croix
  if(region == "STX"){

    # Create NTOT
    ntot <- USVI_2017_NTOT %>%
      dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                       if_else(HABITAT_CD == "PTRF", "High coral",
                                               if_else(HABITAT_CD == "BDRK", "High coral",
                                                       if_else(STRAT == "HARD_DEEP", "High coral",
                                                               if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                       if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                               if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
      dplyr::filter(REGION == "STX",
                    reef_cat == reef_type,
                    STRAT != "HARD_SHLW") %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD, reef_cat) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(YEAR) %>%
      dplyr::mutate(ngrtot = sum(NTOT),
                    wh = NTOT/ngrtot) %>%
      dplyr::ungroup()
  }

  ## Puerto Rico
  if(region == "PRICO"){

    # Create NTOT
    ntot <- PRICO_2016_NTOT %>%

      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT),
                    wh = NTOT/ngrtot)


  }

  ## Flower Garden Banks National Marine Sanctuary (GOM)
  if(region == "GOM"){
    # NTOT not currently needed for FGB as there is only 1 strata
  }

  # Add high and low reef categories to Caribbean data
  if(region == "STTSTJ" || region == "STX"){

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
      dplyr::mutate(Var=svar/n_sites,
                    REGION = "USVI") %>%
      dplyr::ungroup() %>%
      dplyr::select(REGION,
                    #reef_cat,
                    ref_Z, svar, n_sites, n_strat, Var)



    # Calculate Z score avdns, svar, n and std for the current data
    cover_est_current <- input_current %>%
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

  if(region == "PRICO"){

    cover_est_ref <- input_ref %>%
      # group by analysis level strata
      dplyr::group_by(REGION) %>% # Modify this line to changes analysis substrate
      dplyr::summarise(# compute average cover
        ref_Z = mean(Percent_Cvr, na.rm = T),
        # compute stratum variance
        svar = var(Percent_Cvr, na.rm = T),
        # calculate N
        n_sites = length(Percent_Cvr),
        n_strat = length(unique(ANALYSIS_STRATUM))) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n_sites,
                    REGION = "PRICO") %>%
      dplyr::ungroup() %>%
      dplyr::select(REGION, ref_Z, svar, n_sites, n_strat, Var)



    # Calculate Z score avdns, svar, n and std for the current data
    cover_est_current <- input_current %>%
      # group by analysis level strata
      dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
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

  if(region == "GOM") {

    # Calculate Z score mean and variance for reference data

    cover_est_ref <- input_ref %>%
      # group by analysis level strata
      dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
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
      dplyr::ungroup()

    # Calculate Z score avdns, svar, n and std for the most recent 2 sampling years
    cover_est_current <- input_current %>%
      # group by analysis level strata
      dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
      dplyr::summarise(# compute average Percent_Cvr
        current_Z = mean(Percent_Cvr),
        # compute stratum variance
        svar = var(Percent_Cvr),
        # calculate N
        current_sites = length(Percent_Cvr),
        n_strat = length(unique(ANALYSIS_STRATUM))) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/current_sites) %>%
      dplyr::ungroup()

    ################
    # Export
    ################


    # Create list to export
    output <- list(
      "Domain_est_Z_ref" = cover_est_ref,
      'Domain_est_Z_current' = cover_est_current
    )

    return(output)


  }


}

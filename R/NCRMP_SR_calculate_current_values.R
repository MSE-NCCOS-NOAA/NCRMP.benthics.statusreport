## Function to calculate reference value for the NCRMP benthic status report

# Purpose:
# create csv files current metric based on status report classification


## Tag: data analysis


# outputs created in this file --------------
# REGION_metric_current_values


# CallS:
# NCRMP_status_report_make_weighted_LPI_data_GOM_Carib
# Region specific non-NCRMP and NCRMP data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: August 2019


##############################################################################################################################

#' Creates a dataframe
#'
#'
#'
#'
#' @param region A string indicating the region
#' @param indicator  A string indicating benthic metric for cover: "HARD CORALS", "CCA", or "MACROALGAE"
#' @param datatype  A string indicating datatype: cover, density or mortality
#' @param min_year  A value indicating the first year of sampling to be included in the reference
#' @param max_year A values indicating the last year of sampling to be included in the reference
#' @param reef_type  A string indicating reef classification
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "if_else"
#' @importFrom dplyr "group_by"
#' @importFrom dplyr "summarize"
#' @importFrom dplyr "mutate"
#' @export
#'
#'


NCRMP_SR_calculate_current_values <- function(region, datatype , indicator = "NULL", min_year, max_year, reef_type = "NULL"){

  # Load data
  # Florida

  if(region == "SEFCRI"){

    ntot <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "SEFCRI") %>%
      dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::mutate(ngrtot = sum(NTOT),
                    wh = NTOT/ngrtot)

    ##### Cover #####

    if(datatype == "cover"){

      dat <- NCRMP_SEFCRI_2014_18_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year,
                      cover_group == indicator
                      # ,
                      # SUB_REGION_NAME == "Broward-Miami"
                      )

      #### Calculate avcvr, svar, n and std  ####
      cover_est <- dat %>%
        # make avcvr
        dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avcvr = mean(Percent_Cvr),
          # compute stratum variance
          svar = var(Percent_Cvr),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      cover_est <- cover_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavcvr = wh * avcvr,
                      whsvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      SEFCRI_cover_current_values <- cover_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avCvr = sum(whavcvr, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("SEFCRI_cover_current_values" = SEFCRI_cover_current_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density"){

      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP_DRM",
                                                region = "SEFCRI",
                                                species_filter = TRUE)

      NCRMP_SEFCRI_2014_18_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- NCRMP_SEFCRI_2014_18_density_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      density_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      density_est <- density_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      SEFCRI_density_current_values <- density_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()



      ################
      # Export
      ################

      # Create list to export
      output <- list("SEFCRI_density_current_values" = SEFCRI_density_current_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){

      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP_DRM",
                                           region = "SEFCRI",
                                           species_filter = TRUE)

      NCRMP_SEFCRI_2014_18_old_mort_site <- tmp$old_mortality_site


      # Filter to years of interest
      dat <- NCRMP_SEFCRI_2014_18_old_mort_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avmort, svar, n and std  ####
      mortality_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean mortality in stratum
                      std = sqrt(svar), # std dev of mortality in stratum
                      SE=sqrt(Var)) #SE of the mean mortality in stratum

      mortality_est <- mortality_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      SEFCRI_old_mort_current_values <- mortality_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avMort = sum(whavmort, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()



      ################
      # Export
      ################

      # Create list to export
      output <- list("SEFCRI_old_mort_current_values" = SEFCRI_old_mort_current_values)

      return(output)

    }



  }

  if(region == "FLK"){


    ##### Cover #####

    if(datatype == "cover"){

      if(indicator == "HARD CORALS" || indicator == "MACROALGAE" ){

        ntot <- FL_2018_NTOT %>%
          dplyr::mutate(reef_cat = dplyr::if_else(STRAT == "FDLR", "bank",
                                           dplyr::if_else(STRAT == "FMLR", "bank",
                                                   dplyr::if_else(STRAT == "FSLR", "bank",
                                                           dplyr::if_else(STRAT == "HRRF", "bank",
                                                                   dplyr::if_else(STRAT == "INPR", "patch",
                                                                           dplyr::if_else(STRAT == "MCPR", "patch",
                                                                                   dplyr::if_else(STRAT == "OFPR", "patch", "other")))))))) %>%
          dplyr::filter(REGION == "FLK",
                        reef_cat == reef_type) %>%
          dplyr::group_by(REGION, reef_cat, STRAT, PROT, GRID_SIZE, RUG_CD) %>%
          dplyr::summarise(NTOT = sum(NTOT)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          dplyr::mutate(ngrtot = sum(NTOT),
                        wh = NTOT/ngrtot)

        # Combine reference year for FGB LTM data, calculate site mean
        dat <- NCRMP_FLK_2014_18_percent_cover_site %>%

          dplyr::mutate(reef_cat = dplyr::if_else(STRAT == "FDLR", "bank",
                                           dplyr::if_else(STRAT == "FMLR", "bank",
                                                   dplyr::if_else(STRAT == "FSLR", "bank",
                                                           dplyr::if_else(STRAT == "HRRF", "bank",
                                                                   dplyr::if_else(STRAT == "INPR", "patch",
                                                                           dplyr::if_else(STRAT == "MCPR", "patch",
                                                                                   dplyr::if_else(STRAT == "OFPR", "patch", "other")))))))) %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator,
                        reef_cat == reef_type)

        #### Calculate avcvr, svar, n and std at the strata + PROT level ####
        cover_est <- dat %>%
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          # make avcvr
          dplyr::group_by(ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis stratum
          dplyr::summarise(
            # compute average cover
            avcvr = mean(Percent_Cvr),
            # compute stratum variance
            svar = var(Percent_Cvr),
            # calculate N
            n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var)) #SE of the mean density in stratum

        cover_est <- cover_est %>%
          # Merge ntot with cover_est
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavcvr = wh * avcvr,
                        whsvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0))  %>%
          dplyr::ungroup()


        # Reformat output

        ## Domain Estimates
        FLK_cover_current_values <- cover_est %>%
          # replace inf values so we can add the strata means
          dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
          dplyr::group_by(
            #REGION
            reef_cat
          ) %>%
          dplyr::summarise(avCvr = sum(whavcvr, na.rm = T),
                           Var = sum(whsvar, na.rm = T),
                           SE=sqrt(Var),
                           # calculate N
                           n_sites = sum(n),
                           n_strat = length(ANALYSIS_STRATUM)) %>%
          dplyr::ungroup()

      } else {

        ntot <- FL_2018_NTOT %>%
          dplyr::filter(REGION == "FLK") %>%
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          dplyr::mutate(ngrtot = sum(NTOT),
                        wh = NTOT/ngrtot)

        dat <- NCRMP_FLK_2014_18_percent_cover_site %>%

          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator)

        #### Calculate avcvr, svar, n and std  ####
        cover_est <- dat %>%
          # make avcvr
          dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
          dplyr::summarise(
            # compute average cover
            avcvr = mean(Percent_Cvr),
            # compute stratum variance
            svar = var(Percent_Cvr),
            # calculate N
            n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var)) #SE of the mean density in stratum

        cover_est <- cover_est %>%
          # Merge ntot with cover_est
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavcvr = wh * avcvr,
                        whsvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0))  %>%
          dplyr::ungroup()


        # Reformat output

        ## Domain Estimates
        FLK_cover_current_values <- cover_est %>%
          # replace inf values so we can add the strata means
          dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
          dplyr::group_by(REGION) %>%
          dplyr::summarise(avCvr = sum(whavcvr, na.rm = T),
                           Var = sum(whsvar, na.rm = T),
                           SE=sqrt(Var),
                           # calculate N
                           n_sites = sum(n),
                           n_strat = length(ANALYSIS_STRATUM)) %>%
          dplyr::ungroup()

      }


      ################
      # Export
      ################

      # Create list to export
      output <- list("FLK_cover_current_values" = FLK_cover_current_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density"){

      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::mutate(ngrtot = sum(NTOT),
                      wh = NTOT/ngrtot)%>%
        dplyr::select(-YEAR)

      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP_DRM",
                                                region = "FLK",
                                                species_filter = TRUE)

      NCRMP_FLK_2014_18_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- NCRMP_FLK_2014_18_density_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      density_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      density_est <- density_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      FLK_density_current_values <- density_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("FLK_density_current_values" = FLK_density_current_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){

      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::mutate(ngrtot = sum(NTOT),
                      wh = NTOT/ngrtot)%>%
        dplyr::select(-YEAR)

      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP_DRM",
                                           region = "FLK",
                                           species_filter = TRUE)

      NCRMP_FLK_2014_18_old_mort_site <- tmp$old_mortality_site


      # Filter to years of interest
      dat <- NCRMP_FLK_2014_18_old_mort_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avmort, svar, n and std  ####
      mortality_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean mortality in stratum
                      std = sqrt(svar), # std dev of mortality in stratum
                      SE=sqrt(Var)) #SE of the mean mortality in stratum

      mortality_est <- mortality_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      FLK_old_mort_current_values <- mortality_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avMort = sum(whavmort, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("FLK_old_mort_current_values" = FLK_old_mort_current_values)

      return(output)

    }


  }

  if(region == "Tortugas"){


    ##### Cover #####

    if(datatype == "cover"){

      if(indicator == "HARD CORALS"){

        ntot <- FL_2018_NTOT %>%
          mutate(reef_cat = dplyr::if_else(STRAT == "CONT_MR", "mid/high",
                                    dplyr::if_else(STRAT == "CONT_LR", "low",
                                            dplyr::if_else(STRAT == "CONT_HR", "mid/high",
                                                    dplyr::if_else(STRAT == "SPGR_LR", "low",
                                                            dplyr::if_else(STRAT == "SPGR_HR", "mid/high",
                                                                    dplyr::if_else(STRAT == "ISOL_HR", "mid/high",
                                                                            dplyr::if_else(STRAT == "ISOL_LR", "low",
                                                                                    dplyr::if_else(STRAT == "ISOL_MR", "mid/high", "other"))))))))) %>%
          dplyr::filter(REGION == "Tortugas",
                        reef_cat == reef_type) %>%
          dplyr::group_by(REGION, reef_cat, STRAT, PROT, GRID_SIZE, RUG_CD) %>%
          dplyr::summarise(NTOT = sum(NTOT)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0") %>%
          dplyr::mutate(ngrtot = sum(NTOT),
                        wh = NTOT/ngrtot)

        # Combine reference year for FGB LTM data, calculate site mean
        dat <- NCRMP_Tort_2014_18_percent_cover_site %>%

          mutate(reef_cat = dplyr::if_else(STRAT == "CONT_MR", "mid/high",
                                    dplyr::if_else(STRAT == "CONT_LR", "low",
                                            dplyr::if_else(STRAT == "CONT_HR", "mid/high",
                                                    dplyr::if_else(STRAT == "SPGR_LR", "low",
                                                            dplyr::if_else(STRAT == "SPGR_HR", "mid/high",
                                                                    dplyr::if_else(STRAT == "ISOL_HR", "mid/high",
                                                                            dplyr::if_else(STRAT == "ISOL_LR", "low",
                                                                                    dplyr::if_else(STRAT == "ISOL_MR", "mid/high", "other"))))))))) %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator,
                        reef_cat == reef_type)

        #### Calculate avcvr, svar, n and std at the strata + PROT level ####
        cover_est <- dat %>%
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          # make avcvr
          dplyr::group_by(ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis stratum
          dplyr::summarise(
            # compute average cover
            avcvr = mean(Percent_Cvr),
            # compute stratum variance
            svar = var(Percent_Cvr),
            # calculate N
            n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var)) #SE of the mean density in stratum

        cover_est <- cover_est %>%
          # Merge ntot with cover_est
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavcvr = wh * avcvr,
                        whsvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0))  %>%
          dplyr::ungroup()


        # Reformat output

        ## Domain Estimates
        Tort_cover_current_values <- cover_est %>%
          # replace inf values so we can add the strata means
          dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
          dplyr::group_by(
            #REGION
            reef_cat
          ) %>%
          dplyr::summarise(avCvr = sum(whavcvr, na.rm = T),
                           Var = sum(whsvar, na.rm = T),
                           SE=sqrt(Var),
                           # calculate N
                           n_sites = sum(n),
                           n_strat = length(ANALYSIS_STRATUM)) %>%
          dplyr::ungroup()

      } else {

        ntot <- FL_2018_NTOT %>%
          dplyr::filter(REGION == "Tortugas") %>%
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          dplyr::mutate(ngrtot = sum(NTOT),
                        wh = NTOT/ngrtot)

        dat <- NCRMP_Tort_2014_18_percent_cover_site %>%

          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator)

        #### Calculate avcvr, svar, n and std  ####
        cover_est <- dat %>%
          # make avcvr
          dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
          dplyr::summarise(
            # compute average cover
            avcvr = mean(Percent_Cvr),
            # compute stratum variance
            svar = var(Percent_Cvr),
            # calculate N
            n = sum(n)) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var)) #SE of the mean density in stratum

        cover_est <- cover_est %>%
          # Merge ntot with cover_est
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavcvr = wh * avcvr,
                        whsvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0))  %>%
          dplyr::ungroup()


        # Reformat output

        ## Domain Estimates
        Tort_cover_current_values <- cover_est %>%
          # replace inf values so we can add the strata means
          dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
          dplyr::group_by(REGION) %>%
          dplyr::summarise(avCvr = sum(whavcvr, na.rm = T),
                           Var = sum(whsvar, na.rm = T),
                           SE=sqrt(Var),
                           # calculate N
                           n_sites = sum(n),
                           n_strat = length(ANALYSIS_STRATUM)) %>%
          dplyr::ungroup()

      }


      ################
      # Export
      ################

      # Create list to export
      output <- list("Tort_cover_current_values" = Tort_cover_current_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density"){

      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0") %>%
        dplyr::mutate(ngrtot = sum(NTOT),
                      wh = NTOT/ngrtot)

      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP_DRM",
                                                region = "Tortugas",
                                                species_filter = TRUE)

      NCRMP_Tort_2014_18_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- NCRMP_Tort_2014_18_density_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      density_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      density_est <- density_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      Tort_density_current_values <- density_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("Tort_density_current_values" = Tort_density_current_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){

      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0") %>%
        dplyr::mutate(ngrtot = sum(NTOT),
                      wh = NTOT/ngrtot)

      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP_DRM",
                                           region = "Tortugas",
                                           species_filter = TRUE)

      NCRMP_Tort_2014_18_old_mort_site <- tmp$old_mortality_site


      # Filter to years of interest
      dat <- NCRMP_Tort_2014_18_old_mort_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avmort, svar, n and std  ####
      mortality_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean mortality in stratum
                      std = sqrt(svar), # std dev of mortality in stratum
                      SE=sqrt(Var)) #SE of the mean mortality in stratum

      mortality_est <- mortality_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      Tort_old_mort_current_values <- mortality_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avMort = sum(whavmort, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("Tort_old_mort_current_values" = Tort_old_mort_current_values)

      return(output)

    }

  }

  # GOM / Carib

  if(region == "STTSTJ"){

    # Create NTOT
    ntot <- USVI_2017_NTOT %>%
      dplyr::ungroup() %>%
      dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                              dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                             dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                                            dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                                           dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                                                          dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                                                         dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
      dplyr::filter(REGION == "STTSTJ"
                    ,
                    reef_cat == reef_type
      ) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD, reef_cat) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT),
                    wh = NTOT/ngrtot)


    ##### Cover #####

    if(datatype == "cover"){


      # Combine reference year for FGB LTM data, calculate site mean
      dat <- NCRMP_STTSTJ_2013_17_percent_cover_site %>%

        dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                         if_else(HABITAT_CD == "PTRF", "High coral",
                                                 if_else(HABITAT_CD == "BDRK", "High coral",
                                                         if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year,
                      cover_group == indicator
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avcvr, svar, n and std at the strata + PROT level ####
      cover_est <- dat %>%
        # make avcvr
        dplyr::group_by(ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avcvr = mean(Percent_Cvr),
          # compute stratum variance
          svar = var(Percent_Cvr),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      cover_est <- cover_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavcvr = wh * avcvr,
                      whsvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      STTSTJ_cover_current_values <- cover_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avCvr = sum(whavcvr, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("STTSTJ_cover_current_values" = STTSTJ_cover_current_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density"){



      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                                region = "STTSTJ",
                                                species_filter = TRUE)

      NCRMP_STTSTJ_2013_17_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- NCRMP_STTSTJ_2013_17_density_site %>%
        dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                         if_else(HABITAT_CD == "PTRF", "High coral",
                                                 if_else(HABITAT_CD == "BDRK", "High coral",
                                                         if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avden, svar, n and std  ####
      density_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      density_est <- density_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      STTSTJ_density_current_values <- density_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("STTSTJ_density_current_values" = STTSTJ_density_current_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){

      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP",
                                           region = "STTSTJ",
                                           species_filter = TRUE)

      NCRMP_STTSTJ_2013_17_old_mort_site <- tmp$old_mortality_site


      # Filter to years of interest
      dat <- NCRMP_STTSTJ_2013_17_old_mort_site %>%

        dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                         if_else(HABITAT_CD == "PTRF", "High coral",
                                                 if_else(HABITAT_CD == "BDRK", "High coral",
                                                         if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avmort, svar, n and std  ####
      mortality_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean mortality in stratum
                      std = sqrt(svar), # std dev of mortality in stratum
                      SE=sqrt(Var)) #SE of the mean mortality in stratum

      mortality_est <- mortality_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      STTSTJ_old_mort_current_values <- mortality_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avMort = sum(whavmort, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("STTSTJ_old_mort_current_values" = STTSTJ_old_mort_current_values)

      return(output)

    }


  }

  if(region == "STX"){

    # Create NTOT
    ntot <- USVI_2017_NTOT %>%
      dplyr::ungroup() %>%
      dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                              dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                             dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                                            dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                                           dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                                                          dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                                                         dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
      dplyr::filter(REGION == "STX"
                    ,
                    reef_cat == reef_type
      ) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD, reef_cat) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT),
                    wh = NTOT/ngrtot)


    ##### Cover #####

    if(datatype == "cover"){



      dat <- NCRMP_STX_2015_17_percent_cover_site %>%

        dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                         if_else(HABITAT_CD == "PTRF", "High coral",
                                                 if_else(HABITAT_CD == "BDRK", "High coral",
                                                         if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year,
                      cover_group == indicator
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avcvr, svar, n and std  ####
      cover_est <- dat %>%
        # make avcvr
        dplyr::group_by(ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avcvr = mean(Percent_Cvr),
          # compute stratum variance
          svar = var(Percent_Cvr),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean cover in stratum
                      std = sqrt(svar), # std dev of cover in stratum
                      SE=sqrt(Var)) #SE of the mean cover in stratum

      cover_est <- cover_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavcvr = wh * avcvr,
                      whsvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      STX_cover_current_values <- cover_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avCvr = sum(whavcvr, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("STX_cover_current_values" = STX_cover_current_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density"){

      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                                region = "STX",
                                                species_filter = TRUE)

      NCRMP_STX_2015_17_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- NCRMP_STX_2015_17_density_site %>%
        dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                         if_else(HABITAT_CD == "PTRF", "High coral",
                                                 if_else(HABITAT_CD == "BDRK", "High coral",
                                                         if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avden, svar, n and std  ####
      density_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      density_est <- density_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      STX_density_current_values <- density_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("STX_density_current_values" = STX_density_current_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){


      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP",
                                           region = "STX",
                                           species_filter = TRUE)

      NCRMP_STX_2015_17_old_mort_site <- tmp$old_mortality_site


      # Filter to years of interest
      dat <- NCRMP_STX_2015_17_old_mort_site %>%

        dplyr::mutate(reef_cat = if_else(HABITAT_CD == "AGRF", "High coral",
                                         if_else(HABITAT_CD == "PTRF", "High coral",
                                                 if_else(HABITAT_CD == "BDRK", "High coral",
                                                         if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avden, svar, n and std  ####
      mortality_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean mortality in stratum
                      std = sqrt(svar), # std dev of mortality in stratum
                      SE=sqrt(Var)) #SE of the mean mortality in stratum

      mortality_est <- mortality_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      STX_old_mort_current_values <- mortality_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avMort = sum(whavmort, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("STX_old_mort_current_values" = STX_old_mort_current_values)

      return(output)

    }


  }

  if(region == "PRICO"){

    ##### Cover #####

    if(datatype == "cover"){

         # Create NTOT
      ntot <- PRICO_2016_NTOT %>%

        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT),
                      wh = NTOT/ngrtot)



      # Combine reference year for FGB LTM data, calculate site mean
      dat <- NCRMP_PRICO_2014_16_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year,
                      cover_group == indicator)

      #### Calculate avcvr, svar, n and std at the strata + PROT level ####
      cover_est <- dat %>%
        # make avcvr
        dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avcvr = mean(Percent_Cvr),
          # compute stratum variance
          svar = var(Percent_Cvr),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      cover_est <- cover_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavcvr = wh * avcvr,
                      whsvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      PRICO_cover_current_values <- cover_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avCvr = sum(whavcvr, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("PRICO_cover_current_values" = PRICO_cover_current_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density"){

      # Create NTOT
      ntot <- PRICO_2016_NTOT %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT),
                      wh = NTOT/ngrtot)

      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                                region = "PRICO",
                                                species_filter = TRUE)

      NCRMP_PRICO_2014_16_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- NCRMP_PRICO_2014_16_density_site %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      density_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var)) #SE of the mean density in stratum

      density_est <- density_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      PRICO_density_current_values <- density_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("PRICO_density_current_values" = PRICO_density_current_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){

      # Create NTOT
      ntot <- PRICO_2016_NTOT %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT),
                      wh = NTOT/ngrtot)


      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP",
                                           region = "PRICO",
                                           species_filter = TRUE)

      NCRMP_PRICO_2014_16_old_mort_site <- tmp$old_mortality_site


      # Filter to years of interest
      dat <- NCRMP_PRICO_2014_16_old_mort_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avmort, svar, n and std  ####
      mortality_est <- dat %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis stratum
        dplyr::summarise(
          # compute average cover
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean mortality in stratum
                      std = sqrt(svar), # std dev of mortality in stratum
                      SE=sqrt(Var)) #SE of the mean mortality in stratum

      mortality_est <- mortality_est %>%
        # Merge ntot with cover_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whsvar = wh^2 * Var,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()


      # Reformat output

      ## Domain Estimates
      PRICO_old_mort_current_values <- mortality_est %>%
        # replace inf values so we can add the strata means
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avMort = sum(whavmort, na.rm = T),
                         std = sum(whstd, na.rm = T),
                         Var = sum(whsvar, na.rm = T),
                         SE=sqrt(Var),
                         # calculate N
                         n_sites = sum(n),
                         n_strat = length(ANALYSIS_STRATUM)) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("PRICO_old_mort_current_values" = PRICO_old_mort_current_values)

      return(output)

    }

  }

  if(region == "GOM"  && reef_type == "NULL"){

    if(datatype == "cover"){

      ##### Cover #####

      FGBNMS_cover_current_values <- FGBNMS_1992_18_percent_cover_site %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year,
                      cover_group == indicator) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, MIN_DEPTH, MAX_DEPTH, LAT_DEGREES, LON_DEGREES, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr)) %>%

        # combine FGB LTM and NCRMP site level data
        dplyr::bind_rows(NCRMP_FGBNMS_2013_18_percent_cover_site %>%
                           dplyr::filter(YEAR <= max_year,
                                         YEAR >= min_year,
                                         cover_group == indicator) %>%
                           dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%

        dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average Percent_Cvr
          avCvr = mean(Percent_Cvr),
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



      ################
      # Export
      ################

      # Create list to export
      output <- list("FGBNMS_cover_current_values" = FGBNMS_cover_current_values)

      return(output)

    }

    if(datatype == "density" && indicator == "NULL"){

      ##### Density #####

      dat <- FGBNMS_LTM_2017_density_site %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year) %>%
        dplyr::group_by(REGION, SURVEY, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT) %>%
        dplyr::summarise(DENSITY = mean(DENSITY))

      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                                region = "GOM",
                                                species_filter = TRUE)

      NCRMP_FGBNMS_2013_18_density_site <- tmp$density_site


      # combine FGB LTM and NCRMP site level data
      FGBNMS_density_current_values <- dat %>% dplyr::bind_rows(NCRMP_FGBNMS_2013_18_density_site %>%
                                                                  dplyr::filter(YEAR <= max_year,
                                                                                YEAR >= min_year) %>%
                                                                  dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%

        dplyr::group_by(REGION, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average Percent_Cvr
          avDen = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n_sites = length(DENSITY),
          n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites) %>%
        dplyr::ungroup()



      ################
      # Export
      ################

      # Create list to export
      output <- list("FGBNMS_density_current_values" = FGBNMS_density_current_values)

      return(output)

    }

    if(datatype == "mortality" && indicator == "NULL"){


      dat <- FGBNMS_LTM_2017_old_mort_site %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year) %>%
        dplyr::group_by(REGION, SURVEY, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT) %>%
        dplyr::summarise(avsitemort = mean(avsitemort))


      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP",
                                           region = "GOM",
                                           species_filter = TRUE)

      NCRMP_FGBNMS_2013_18_old_mort_site <- tmp$old_mortality_site


      # combine FGB LTM and NCRMP site level data
      FGBNMS_old_mort_current_values <-  dplyr::bind_rows(dat, NCRMP_FGBNMS_2013_18_old_mort_site %>%
                                                                   dplyr::filter(YEAR <= max_year,
                                                                                 YEAR >= min_year) %>%
                                                                   dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%

        dplyr::group_by(REGION, STRAT) %>%
        dplyr::summarise(
          avMort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n_sites = length(avsitemort),
          n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites) %>%
        dplyr::ungroup()


      ##### Mortality #####

      ################
      # Export
      ################

      # Create list to export
      output <- list("FGBNMS_old_mort_current_values" = FGBNMS_old_mort_current_values)

      return(output)

    }

  }



}

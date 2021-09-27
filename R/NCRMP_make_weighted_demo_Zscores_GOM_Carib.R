## Function to calculate weighted coral density and mortality

# Purpose:
# support function to calculate weighted coral density and mortality


## Tag: data analysis


# outputs created in this file --------------
# Domain_est_Z_ref
# Domain_est_Z_current

# Weighting scheme:
# Region specific

# CallS:
# analysis ready data

# output gets called by:
# NCRMP_calculate_Z_scores_density.R
# NCRMP_calculate_Z_scores_mortality.R

# NCRMP Caribbean Benthic analytics team: Groves and Viehman
# Last update: July 2019


##############################################################################################################################

#' Creates weighted demo data
#'
#'
#'
#'
#' @param input_ref A dataframe
#' @param input_current A dataframe
#' @param region A string indicating the region
#' @param datatype A string indicating the datatype
#' @param reef_type  A string indicating reef classification
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_make_weighted_demo_Zscores_GOM_Carib <- function(input_ref, input_current, region, datatype, reef_type){

  #### Read in ntot ####


  if(region == "STTSTJ"){

    # Create NTOT
    ntot_current <- USVI_2017_NTOT %>%
      dplyr::ungroup() %>%
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

  if(region == "STX"){

    ntot_current <- USVI_2017_NTOT %>%
      dplyr::ungroup() %>%
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

  if(region == "PRICO"){

    ntot_current <- PRICO_2016_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(YEAR) %>%
      dplyr::mutate(ngrtot = sum(NTOT),
                    wh = NTOT/ngrtot) %>%
      dplyr::ungroup()

  }

  if(region == "GOM"){

    # NTOT not currently needed for FGB as there is only 1 strata

  }



  #### Calculate weighted density ####

  if(datatype == "density"){

    if(region == "STTSTJ" || region == "STX"){

      # Calculate Z score avdns, svar, n and std for the reference data
      density_est_ref <- input_ref %>%
        # group by analysis level strata
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average DENSITY
          ref_Z = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n_sites = length(DENSITY),
          n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites, #variance of mean DENSITY in stratum
                      std = sqrt(svar), # std dev of DENSITY in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum)
        dplyr::ungroup()


      # Calculate Z score avdns, svar, n and std for the current data
      density_est_current <- input_current %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        # group by analysis level strata
        dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average DENSITY
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n_sites = length(DENSITY),
          n_strat = length(unique(ANALYSIS_STRATUM))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites, #variance of mean DENSITY in stratum
                      std = sqrt(svar), # std dev of DENSITY in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum
        dplyr::ungroup()

      density_est_current <- density_est_current %>%
        # Merge ntot
        dplyr::full_join(., ntot_current) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whvar = wh^2 * Var)  %>%
        dplyr::ungroup()

      ## Domain Estimates
      # region/population means
      Domain_est_current <- density_est_current %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(current_Z = sum(whavden, na.rm = T),
                         Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                         n_sites = sum(n_sites),
                         strat_num = sum(n_strat, na.rm = T))  %>%
        dplyr::ungroup()

      ################
      # Export
      ################


      # Create list to export
      output <- list(
        "Domain_est_Z_ref" = density_est_ref,
        'Domain_est_Z_current' = Domain_est_current
      )

      return(output)

    }

    if(region == "PRICO") {

      # Calculate Z score avdns, svar, n and std for the reference data
      density_est_ref <- input_ref %>%
        # group by analysis level strata
        dplyr::group_by(REGION) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average DENSITY
          ref_Z = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n_sites = length(DENSITY),
          n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites, #variance of mean DENSITY in stratum
                      std = sqrt(svar), # std dev of DENSITY in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum)
        dplyr::ungroup()


      # Calculate Z score avdns, svar, n and std for the current data
      density_est_current <- input_current %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        # group by analysis level strata
        dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average DENSITY
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n_sites = length(DENSITY),
          n_strat = length(unique(ANALYSIS_STRATUM))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites, #variance of mean DENSITY in stratum
                      std = sqrt(svar), # std dev of DENSITY in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum
        dplyr::ungroup()

      density_est_current <- density_est_current %>%
        # Merge ntot
        dplyr::full_join(., ntot_current) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whvar = wh^2 * Var)  %>%
        dplyr::ungroup()

      ## Domain Estimates
      # region/population means
      Domain_est_current <- density_est_current %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(current_Z = sum(whavden, na.rm = T),
                         Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                         n_sites = sum(n_sites),
                         strat_num = sum(n_strat, na.rm = T))  %>%
        dplyr::ungroup()

      ################
      # Export
      ################


      # Create list to export
      output <- list(
        "Domain_est_Z_ref" = density_est_ref,
        'Domain_est_Z_current' = Domain_est_current
      )

      return(output)

    }

    if(region == "GOM") {

      # Calculate Z score mean and variance for reference data

      density_est_ref <- input_ref %>%
        # group by analysis level strata
        dplyr::group_by(REGION, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average DENSITY
          ref_Z = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          n_sites = length(DENSITY),
          n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites, #variance of mean DENSITY in stratum
                      std = sqrt(svar), # std dev of DENSITY in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum)
        dplyr::ungroup()

      # Calculate Z score avdns, svar, n and std for the most recent 2 sampling years
      density_est_current <- input_current %>%
        # group by analysis level strata
        dplyr::group_by(REGION, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average DENSITY
          current_Z = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
          current_sites = length(DENSITY),
          n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/current_sites, #variance of mean DENSITY in stratum
                      std = sqrt(svar), # std dev of DENSITY in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum)
        dplyr::ungroup()

      ################
      # Export
      ################


      # Create list to export
      output <- list(
        "Domain_est_Z_ref" = density_est_ref,
        'Domain_est_Z_current' = density_est_current
      )

      return(output)


    }

  }




  #### Calculate weighted mortality ####

  if(datatype == "mortality"){

    if(region == "STTSTJ" || region == "STX"){


      # Calculate Z score avmort, svar, n and std for the reference data
      mortality_est_ref <- input_ref %>%
        # group by analysis level strata
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average mortality
          ref_Z = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n_sites = length(avsitemort),
          n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites, #variance of mean DENSITY in stratum
                      std = sqrt(svar), # std dev of DENSITY in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum)
        dplyr::ungroup()

      # Calculate Z score avmort, svar, n and std for the current data
      mortality_est_current <- input_current %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        # group by analysis level strata
        dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT, reef_cat) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average mortality
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n_sites = length(avsitemort),
          n_strat = length(unique(ANALYSIS_STRATUM))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites, #variance of mean mortality in stratum
                      std = sqrt(svar), # std dev of mortality in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum)
        dplyr::ungroup()

      mortality_est_current <- mortality_est_current %>%
        # Merge ntot
        dplyr::full_join(., ntot_current) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whvar = wh^2 * Var)  %>%
        dplyr::ungroup()

      ## Domain Estimates
      # region/population means
      Domain_est_current <- mortality_est_current %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(current_Z = sum(whavmort, na.rm = T),
                         Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                         n_sites = sum(n_sites),
                         strat_num = sum(n_strat, na.rm = T))  %>%
        dplyr::ungroup()

      ################
      # Export
      ################


      # Create list to export
      output <- list(
        "Domain_est_Z_ref" = mortality_est_ref,
        'Domain_est_Z_current' = Domain_est_current
      )

      return(output)


    }

    if(region == "PRICO") {

      # Calculate Z score avmort, svar, n and std for the reference data
      mortality_est_ref <- input_ref %>%
        # group by analysis level strata
        dplyr::group_by(REGION) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average mortality
          ref_Z = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n_sites = length(avsitemort),
          n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites, #variance of mean DENSITY in stratum
                      std = sqrt(svar), # std dev of DENSITY in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum)
        dplyr::ungroup()

      # Calculate Z score avmort, svar, n and std for the current data
      mortality_est_current <- input_current %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        # group by analysis level strata
        dplyr::group_by(REGION, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average mortality
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          n_sites = length(avsitemort),
          n_strat = length(unique(ANALYSIS_STRATUM))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites, #variance of mean mortality in stratum
                      std = sqrt(svar), # std dev of mortality in stratum
                      SE=sqrt(Var)) %>% #SE of the mean DENSITY in stratum)
        dplyr::ungroup()

      mortality_est_current <- mortality_est_current %>%
        # Merge ntot
        dplyr::full_join(., ntot_current) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whvar = wh^2 * Var)  %>%
        dplyr::ungroup()

      ## Domain Estimates
      # region/population means
      Domain_est_current <- mortality_est_current %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(current_Z = sum(whavmort, na.rm = T),
                         Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                         n_sites = sum(n_sites),
                         strat_num = sum(n_strat, na.rm = T))  %>%
        dplyr::ungroup()

      ################
      # Export
      ################


      # Create list to export
      output <- list(
        "Domain_est_Z_ref" = mortality_est_ref,
        'Domain_est_Z_current' = Domain_est_current
      )

      return(output)

    }

    if(region == "GOM") {

      # Calculate Z score mean and variance for reference data & current data

      mortality_est_ref <- input_ref %>%
        # group by analysis level strata
        dplyr::group_by(REGION, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average mortality
          ref_Z = mean(avsitemort),
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

      mortality_est_current <- input_current %>%
        # group by analysis level strata
        dplyr::group_by(REGION, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average mortality
          current_Z = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          # calculate N
          current_sites = length(avsitemort),
          n_strat = length(unique(STRAT))) %>%
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
        "Domain_est_Z_ref" = mortality_est_ref,
        'Domain_est_Z_current' = mortality_est_current
      )

      return(output)




    }

  }

}




















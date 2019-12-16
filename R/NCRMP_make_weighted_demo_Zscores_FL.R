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

NCRMP_make_weighted_demo_Zscores_FL <- function(input_ref, input_current, region, datatype, reef_type){


  #### Read in ntot ####

  ## Florida
  # SE FL
  if(region == "SEFCRI"){

    ntot <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "SEFCRI") %>%
      dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = ""),
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::select(-YEAR)

  }

  # FL Keys
  if(region == "FLK"){

    ntot <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "FLK") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::select(-YEAR)

  }

  # Tortugas
  if(region == "Tortugas"){

    ntot <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "Tortugas") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0") %>%
      dplyr::select(-YEAR)
  }

  ntot <- ntot %>%
    dplyr::mutate(ngrtot = sum(NTOT))  %>%
    dplyr::mutate(wh = NTOT/ngrtot)




  #### Calculate weighted density ####

  if(datatype == "density"){

    # Calculate Z score avdns, svar, n and std for the reference data
    density_est_ref <- input_ref %>%
      # group by analysis level strata
      dplyr::group_by(
        REGION
        #reef_cat
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
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
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
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavden = wh * avden,
                    whvar = wh^2 * Var)  %>%
      dplyr::ungroup()

    ## Domain Estimates
    # region/population means
    Domain_est_current <- density_est_current %>%
      dplyr::group_by(
        REGION
        #reef_cat
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





  #### Calculate weighted mortality ####

  if(datatype == "mortality"){



    # Calculate Z score avmort, svar, n and std for the reference data
    mortality_est_ref <- input_ref %>%
      # group by analysis level strata
      dplyr::group_by(
        REGION
        #reef_cat
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
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
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
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavmort = wh * avmort,
                    whvar = wh^2 * Var)  %>%
      dplyr::ungroup()

    ## Domain Estimates
    # region/population means
    Domain_est_current <- mortality_est_current %>%
      dplyr::group_by(
        REGION
        #reef_cat
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

}




















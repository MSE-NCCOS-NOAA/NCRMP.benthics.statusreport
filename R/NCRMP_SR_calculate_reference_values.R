## Function to calculate reference value for the NCRMP benthic status report

# Purpose:
# create csv files current metric based on status report classification


## Tag: data analysis


# outputs created in this file --------------
# REGION_metric_ref_values


# CallS:
# NCRMP_status_report_make_weighted_LPI_data_GOM_Carib
# Region specific non-NCRMP and NCRMP data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: August 2019


##############################################################################################################################

#' Creates percent cover dataframe
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


NCRMP_SR_calculate_reference_values <- function(region, datatype , indicator = "NULL", min_year, max_year, reef_type = "NULL"){

  # Load data
  # Florida

  if(region == "SEFCRI"){

    ##### Cover #####

    if(datatype == "cover"){

      if(indicator == "HARD CORALS"){

        SEFCRI_cover_ref_values <- SEFCRI_1979_92_percent_cover_site %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator) %>%
          dplyr::group_by(REGION) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        datasets = paste("Historic lit. -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(REGION, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      }

       if(indicator == "MACROALGAE"){

         SEFCRI_cover_ref_values <- SECREMP_2003_2018_percent_cover_site %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator) %>%
          dplyr::group_by(REGION) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        datasets = paste("Historic lit. -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(REGION, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

       }

       if(indicator == "CCA"){

        SEFCRI_cover_ref_values <- NCRMP_SEFCRI_2014_18_percent_cover_site %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator) %>%
          dplyr::group_by(REGION) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(REGION, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      }

      ################
      # Export
      ################

      # Create list to export
      output <- list("SEFCRI_cover_ref_values" = SEFCRI_cover_ref_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density"){

      # Create filtered site level data
      tmp <- DRM_SCREAM_calculate_colony_density(project = "DRM",
                                                 species_filter = TRUE)

      DRM_FL_2005_13_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- DRM_FL_2005_13_density_site %>%

        dplyr::filter(REGION == "SEFCRI",
                      YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      SEFCRI_density_ref_values <- dat %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avDen = mean(DENSITY),
                         # compute variance
                         svar = var(DENSITY),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(REGION, avDen, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("SEFCRI_density_ref_values" = SEFCRI_density_ref_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){

      # Create filtered site level data
      tmp <- DRM_SCREAM_calculate_mortality(project = "DRM",
                                            species_filter = TRUE)

      DRM_FL_2005_13_old_mortality_site <- tmp$old_mortality_site

      # Filter to years of interest
      dat <- DRM_FL_2005_13_old_mortality_site %>%

        dplyr::filter(REGION == "SEFCRI",
                      YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      SEFCRI_old_mort_ref_values <- dat %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avMort = mean(avsitemort),
                         # compute variance
                         svar = var(avsitemort),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(REGION, avMort, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("SEFCRI_old_mort_ref_values" = SEFCRI_old_mort_ref_values)

      return(output)

    }


  }

  if(region == "FLK"){

    ##### Cover #####

    if(datatype == "cover"){

      if(indicator == "HARD CORALS" || indicator == "MACROALGAE" ){

        FLK_cover_ref_values <- FLK_1974_00_percent_cover_site_CREMP_cond %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator,
                        STRAT == reef_type) %>%
          dplyr::group_by(STRAT) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        reef_cat = STRAT,
                        datasets = paste("Historic lit. -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(reef_cat, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      } else {

        FLK_cover_ref_values <- NCRMP_FLK_2014_18_percent_cover_site %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator) %>%
          dplyr::group_by(REGION) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(REGION, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      }

      ################
      # Export
      ################

      # Create list to export
      output <- list("FLK_cover_ref_values" = FLK_cover_ref_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density"){

      # Create filtered site level data
      tmp <- DRM_SCREAM_calculate_colony_density(project = "DRM",
                                                 species_filter = TRUE)

      DRM_FL_2005_13_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- DRM_FL_2005_13_density_site %>%

        dplyr::filter(REGION == "FLK",
                      YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      FLK_density_ref_values <- dat %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avDen = mean(DENSITY),
                         # compute variance
                         svar = var(DENSITY),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(REGION, avDen, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("FLK_density_ref_values" = FLK_density_ref_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){

      # Create filtered site level data
      tmp <- DRM_SCREAM_calculate_mortality(project = "DRM",
                                            species_filter = TRUE)

      DRM_FL_2005_13_old_mortality_site <- tmp$old_mortality_site

      # Filter to years of interest
      dat <- DRM_FL_2005_13_old_mortality_site %>%

        dplyr::filter(REGION == "FLK",
                      YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      FLK_old_mort_ref_values <- dat %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avMort = mean(avsitemort),
                         # compute variance
                         svar = var(avsitemort),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(REGION, avMort, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("FLK_old_mort_ref_values" = FLK_old_mort_ref_values)

      return(output)

    }


  }

  if(region == "Tortugas"){

    ##### Cover #####

    if(datatype == "cover"){

      if(indicator == "HARD CORALS"){

        Tort_cover_ref_values <- Tort_1975_99_percent_cover_site_CREMP_cond %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator,
                        ANALYSIS_STRATUM == reef_type) %>%
          dplyr::group_by(ANALYSIS_STRATUM) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(ANALYSIS_STRATUM))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        reef_cat = ANALYSIS_STRATUM,
                        datasets = paste("Historic lit. -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(reef_cat, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      } else {

        Tort_cover_ref_values <- NCRMP_Tort_2014_18_percent_cover_site %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator) %>%
          dplyr::group_by(REGION) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(REGION, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      }

      ################
      # Export
      ################

      # Create list to export
      output <- list("Tort_cover_ref_values" = Tort_cover_ref_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density"){

      # Create filtered site level data
      tmp <- DRM_SCREAM_calculate_colony_density(project = "DRM",
                                                 species_filter = TRUE)

      DRM_FL_2005_13_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- DRM_FL_2005_13_density_site %>%

        dplyr::filter(REGION == "Tortugas",
                      YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      Tort_density_ref_values <- dat %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avDen = mean(DENSITY),
                         # compute variance
                         svar = var(DENSITY),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(REGION, avDen, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()



      ################
      # Export
      ################

      # Create list to export
      output <- list("Tort_density_ref_values" = Tort_density_ref_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){

      # Create filtered site level data
      tmp <- DRM_SCREAM_calculate_mortality(project = "DRM",
                                            species_filter = TRUE)

      DRM_FL_2005_13_old_mortality_site <- tmp$old_mortality_site

      # Filter to years of interest
      dat <- DRM_FL_2005_13_old_mortality_site %>%

        dplyr::filter(REGION == "Tortugas",
                      YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      Tort_old_mort_ref_values <- dat %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avMort = mean(avsitemort),
                         # compute variance
                         svar = var(avsitemort),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(REGION, avMort, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()



      ################
      # Export
      ################

      # Create list to export
      output <- list("Tort_old_mort_ref_values" = Tort_old_mort_ref_values)

      return(output)

    }


  }

  # GOM / Carib

  if(region == "STTSTJ"){

    ##### Cover #####

    if(datatype == "cover"){

      if(reef_type == "High coral"){

        dat <- USVI_1999_18_percent_cover_site %>%

          dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                           dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                   dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                           dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                   dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                           dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                   dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other"))))))),
                        REGION = "STTSTJ") %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator
                        ,
                        reef_cat == reef_type
          ) %>%

          dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%

          dplyr::summarise(Percent_Cvr = mean(Percent_Cvr),
                           n_years = sum(n))%>%
          dplyr::ungroup()


        USVI_cover_ref_values <- dat %>%
          dplyr::group_by(
            reef_cat
            #REGION
          ) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        datasets = paste("TCRMP/NPS -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(reef_cat, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      }

      if(reef_type == "Low coral"){

        dat <- NCRMP_STTSTJ_2013_17_percent_cover_site %>%

          dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                           dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                   dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                           dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                   dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                           dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                   dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%

          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator,
                        reef_cat == reef_type)


        USVI_cover_ref_values <- dat %>%
          dplyr::group_by(reef_cat) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(reef_cat, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      }


      ################
      # Export
      ################

      # Create list to export
      output <- list("USVI_cover_ref_values" = USVI_cover_ref_values)

      return(output)



    }

    ##### Density #####

    if(datatype == "density"){

      # Create filtered site level data
      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                                region = "STTSTJ",
                                                species_filter = TRUE)

      NCRMP_STTSTJ_2013_17_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- NCRMP_STTSTJ_2013_17_density_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avden, svar, n and std  ####
      STTSTJ_density_ref_values <- dat %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avDen = mean(DENSITY),
                         # compute variance
                         svar = var(DENSITY),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(reef_cat, avDen, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()




      ################
      # Export
      ################

      # Create list to export
      output <- list("STTSTJ_density_ref_values" = STTSTJ_density_ref_values)

      return(output)

    }

    ##### Mortality #####

    if(datatype == "mortality"){

      # Create filtered site level data
      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP",
                                           region = "STTSTJ",
                                           species_filter = TRUE)

      NCRMP_STTSTJ_2013_17_old_mort_site <- tmp$old_mortality_site


      # Filter to years of interest
      dat <- NCRMP_STTSTJ_2013_17_old_mort_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avden, svar, n and std  ####
      STTSTJ_old_mort_ref_values <- dat %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avMort = mean(avsitemort),
                         # compute variance
                         svar = var(avsitemort),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(reef_cat, avMort, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()




      ################
      # Export
      ################

      # Create list to export
      output <- list("STTSTJ_old_mort_ref_values" = STTSTJ_old_mort_ref_values)

      return(output)



    }



  }

  if(region == "STX"){


    if(datatype == "cover"){

      if(reef_type == "High coral"){

        dat <- USVI_1999_18_percent_cover_site %>%

          dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                           dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                   dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                           dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                   dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                           dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                   dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other"))))))),
                        REGION = "STX") %>%
          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator
                        ,
                        reef_cat == reef_type
          ) %>%

          dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, reef_cat, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%

          dplyr::summarise(Percent_Cvr = mean(Percent_Cvr),
                           n_years = sum(n))%>%
          dplyr::ungroup()


        USVI_cover_ref_values <- dat %>%
          dplyr::group_by(
            #REGION
            reef_cat
          ) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        datasets = paste("TCRMP/NPS -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(reef_cat, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      }

      if(reef_type == "Low coral"){

        dat <- NCRMP_STX_2015_17_percent_cover_site %>%

          dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                           dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                   dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                           dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                   dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                           dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                   dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%

          dplyr::filter(YEAR <= max_year,
                        YEAR >= min_year,
                        cover_group == indicator,
                        reef_cat == reef_type)


        USVI_cover_ref_values <- dat %>%
          dplyr::group_by(reef_cat) %>%
          dplyr::summarise(avCvr = mean(Percent_Cvr),
                           # compute variance
                           svar = var(Percent_Cvr),
                           # calculate N
                           n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                           n_strat = length(unique(STRAT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites,
                        std = sqrt(svar), # std dev of cvr
                        first_year = min_year,
                        last_year = max_year,
                        datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
          dplyr::select(reef_cat, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
          dplyr::ungroup()

      }


      ################
      # Export
      ################

      # Create list to export
      output <- list("USVI_cover_ref_values" = USVI_cover_ref_values)

      return(output)



    }

    ##### Density #####

    if(datatype == "density"){


      # Create filtered site level data
      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                                region = "STX",
                                                species_filter = TRUE)

      NCRMP_STX_2015_17_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- NCRMP_STX_2015_17_density_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avden, svar, n and std  ####
      STX_density_ref_values <- dat %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avDen = mean(DENSITY),
                         # compute variance
                         svar = var(DENSITY),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(reef_cat, avDen, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("STX_density_ref_values" = STX_density_ref_values)

      return(output)



    }

    ##### Mortality #####

    if(datatype == "mortality"){


      # Create filtered site level data
      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP",
                                           region = "STX",
                                           species_filter = TRUE)

      NCRMP_STX_2015_17_old_mort_site <- tmp$old_mortality_site


      # Filter to years of interest
      dat <- NCRMP_STX_2015_17_old_mort_site %>%
        dplyr::mutate(reef_cat = dplyr::if_else(HABITAT_CD == "AGRF", "High coral",
                                         dplyr::if_else(HABITAT_CD == "PTRF", "High coral",
                                                 dplyr::if_else(HABITAT_CD == "BDRK", "High coral",
                                                         dplyr::if_else(STRAT == "HARD_DEEP", "High coral",
                                                                 dplyr::if_else(STRAT == "HARD_SHLW", "Low coral",
                                                                         dplyr::if_else(HABITAT_CD == "PVMT", "Low coral",
                                                                                 dplyr::if_else(HABITAT_CD == "SCR", "Low coral", "other")))))))) %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year
                      ,
                      reef_cat == reef_type
        )

      #### Calculate avden, svar, n and std  ####
      STX_old_mort_ref_values <- dat %>%
        dplyr::group_by(
          #REGION
          reef_cat
        ) %>%
        dplyr::summarise(avMort = mean(avsitemort),
                         # compute variance
                         svar = var(avsitemort),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(reef_cat, avMort, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()



      ################
      # Export
      ################

      # Create list to export
      output <- list("STX_old_mort_ref_values" = STX_old_mort_ref_values)

      return(output)



    }

  }

  if(region == "PRICO"){

    ##### Cover #####

    if(datatype == "cover"){

      dat <- NCRMP_PRICO_2014_16_percent_cover_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year,
                      cover_group == indicator) %>%

        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group) %>%

        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr),
                         n_years = sum(n))%>%
        dplyr::ungroup()


      PRICO_cover_ref_values <- dat %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avCvr = mean(Percent_Cvr, na.rm = T),
                         # compute variance
                         svar = var(Percent_Cvr, na.rm = T),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("PRCREMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(REGION, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("PRICO_cover_ref_values" = PRICO_cover_ref_values)

      return(output)


    }


    ##### Density #####

    if(datatype == "density"){


      # Create filtered site level data
      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                                region = "PRICO",
                                                species_filter = TRUE)

      NCRMP_PRICO_2014_16_density_site <- tmp$density_site


      # Filter to years of interest
      dat <- NCRMP_PRICO_2014_16_density_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      PRICO_density_ref_values <- dat %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avDen = mean(DENSITY),
                         # compute variance
                         svar = var(DENSITY),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(REGION, avDen, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("PRICO_density_ref_values" = PRICO_density_ref_values)

      return(output)



    }

    ##### Mortality #####

    if(datatype == "mortality"){


      # Create filtered site level data
      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP",
                                           region = "PRICO",
                                           species_filter = TRUE)

      NCRMP_PRICO_2014_16_old_mort_site <- tmp$old_mortality_site


      # Filter to years of interest
      dat <- NCRMP_PRICO_2014_16_old_mort_site %>%

        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year)

      #### Calculate avden, svar, n and std  ####
      PRICO_old_mort_ref_values <- dat %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avMort = mean(avsitemort),
                         # compute variance
                         svar = var(avsitemort),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                         n_strat = length(unique(STRAT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, sep = " ")) %>%
        dplyr::select(REGION, avMort, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets) %>%
        dplyr::ungroup()


      ################
      # Export
      ################

      # Create list to export
      output <- list("PRICO_old_mort_ref_values" = PRICO_old_mort_ref_values)

      return(output)



    }
  }

  if(region == "GOM" && reef_type == "NULL"){

    ##### Cover #####

    if(datatype == "cover"){

      # Combine reference year for FGB LTM data, calculate site mean
      dat <- FGBNMS_1992_18_percent_cover_site %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year,
                      cover_group == indicator) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, MIN_DEPTH, MAX_DEPTH, LAT_DEGREES, LON_DEGREES, ANALYSIS_STRATUM, STRAT,  HABITAT_CD, PROT, cover_group, n) %>%
        dplyr::summarise(Percent_Cvr = mean(Percent_Cvr))

      # combine FGB LTM and NCRMP site level data
      FGBNMS_cover_ref_values <- dplyr::bind_rows(dat, NCRMP_FGBNMS_2013_18_percent_cover_site %>%
                                                    dplyr::filter(YEAR <= max_year,
                                                                  YEAR >= min_year,
                                                                  cover_group == indicator) %>%
                                                    dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(avCvr = mean(Percent_Cvr),
                         # compute variance
                         svar = var(Percent_Cvr),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("FGB LTM -", min_year, "to", max_year, ";", "NCRMP - up to", max_year, sep = " "),
                      n_strat = 1) %>%
        dplyr::select(REGION, avCvr, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets)

      ################
      # Export
      ################

      # Create list to export
      output <- list("FGBNMS_cover_ref_values" = FGBNMS_cover_ref_values)

      return(output)

    }

    ##### Density #####

    if(datatype == "density" && indicator == "NULL"){

      # Combine reference year for FGB LTM data, calculate site mean
      dat <- FGBNMS_LTM_2017_density_site %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, LAT_DEGREES, LON_DEGREES, STRAT) %>%
        dplyr::summarise(DENSITY = mean(DENSITY))

      # Create filtered site level NCRMP data which is not stored in package

      tmp <- NCRMP_DRM_calculate_colony_density(project = "NCRMP",
                                                region = "GOM",
                                                species_filter = TRUE)

      NCRMP_FGBNMS_2013_18_density_site <- tmp$density_site


      # combine FGB LTM and NCRMP site level data
      FGBNMS_density_ref_values <- dplyr::bind_rows(dat, NCRMP_FGBNMS_2013_18_density_site %>%
                                                      dplyr::filter(YEAR <= max_year,
                                                                    YEAR >= min_year) %>%
                                                      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%

        dplyr::group_by(REGION) %>%
        dplyr::summarise(avDen = mean(DENSITY),
                         # compute variance
                         svar = var(DENSITY),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, ";", "FGB LTM -", min_year, sep = " "),
                      n_strat = 1) %>%
        dplyr::select(REGION, avDen, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets)

      ################
      # Export
      ################

      # Create list to export
      output <- list("FGBNMS_density_ref_values" = FGBNMS_density_ref_values)

      return(output)

    }

    if(datatype == "mortality" && indicator == "NULL"){

      ##### Mortality #####

      # Combine reference year for FGB LTM data, calculate site mean
      dat <- FGBNMS_LTM_2017_old_mort_site %>%
        dplyr::filter(YEAR <= max_year,
                      YEAR >= min_year) %>%
        dplyr::group_by(REGION, PRIMARY_SAMPLE_UNIT, SUB_REGION_NAME, LAT_DEGREES, LON_DEGREES, STRAT) %>%
        dplyr::summarise(avsitemort = mean(avsitemort))

      # Create filtered site level NCRMP data which is not stored in package

      tmp <- NCRMP_DRM_calculate_mortality(project = "NCRMP",
                                           region = "GOM",
                                           species_filter = TRUE)

      NCRMP_FGBNMS_2013_18_old_mort_site <- tmp$old_mortality_site


      # combine FGB LTM and NCRMP site level data
      FGBNMS_old_mort_ref_values <- dplyr::bind_rows(dat, NCRMP_FGBNMS_2013_18_old_mort_site %>%
                                                       dplyr::filter(YEAR <= max_year,
                                                                     YEAR >= min_year) %>%
                                                       dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(as.character(PRIMARY_SAMPLE_UNIT)))) %>%

        dplyr::group_by(REGION) %>%
        dplyr::summarise(avMort = mean(avsitemort),
                         # compute variance
                         svar = var(avsitemort),
                         # calculate N
                         n_sites = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n_sites,
                      std = sqrt(svar), # std dev of cvr
                      first_year = min_year,
                      last_year = max_year,
                      datasets = paste("NCRMP -", min_year, "to", max_year, ";", "FGB LTM -", min_year, sep = " "),
                      n_strat = 1) %>%
        dplyr::select(REGION, avMort, svar, Var, std, n_sites, n_strat,  first_year, last_year, datasets)



      ################
      # Export
      ################

      # Create list to export
      output <- list("FGBNMS_old_mort_ref_values" = FGBNMS_old_mort_ref_values)

      return(output)

    }

  }




}

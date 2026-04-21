#' run_PEPRMT
#'
#' Runs all PEPRMT functions and returns an output dataframe with modeled GPP, Reco, and CH4
#'
#' @param data Data frame containing 15 required columns used as model inputs.
#'   See **Details** for expected column structure.
#' @param wetland_type Integer indicating wetland class:
#'   1 = Freshwater peatland, 2 = Tidal wetland.
#' @param a0 Empirical intercept parameter for the fPAR scaling function 
#'   (unitless). Used in GPP module.
#' @param a1 Empirical slope parameter for the fPAR scaling function
#'   (unitless). Used in GPP module.
#' @param Ha Activation energy governing the temperature response of
#'   photosynthesis for general crop-type vegetation (kJ mol^-1).
#'   Controls the rate of increase in GPP with temperature below the
#'   thermal optimum. Used in GPP module.
#' @param Hd Deactivation energy controlling the decline in photosynthesis
#'   above the thermal optimum (kJ mol^-1). Determines the rate of decrease in 
#'   GPP at high temperatures. Used in GPP module.
#' @param T_opt_GPP Temperature optimum for GPP. Used in GPP module.
#' @param Ea_SOM – Activation energy controlling the temperature sensitivity
#'   of decomposition from the soil organic matter (SOM) pool
#'   (kJ mol^-1). Used in Reco module.
#' @param kM_SOM – Half-saturation constant for microbial decomposition of
#'   the SOM pool (g C m^-3 soil). Determines substrate limitation strength for 
#'   SOM respiration. Used in Reco module.
#' @param Ea_labile Activation energy controlling the temperature sensitivity
#'   of decomposition from the labile carbon pool (kJ mol^-1). Used in Reco 
#'   module.
#' @param kM_labile Half-saturation constant for microbial decomposition of
#'   the labile carbon pool (g C m^-3 soil). Determines substrate limitation 
#'   strength for labile respiration. Used in Reco module.
#' @param Ea_SOM_CH4 Activation energy for methane production from soil
#'   organic matter (kJ mol^-1). Used in CH4 module.
#' @param kM_SOM_CH4 Half-saturation constant for SOM methane production
#'   (g C m^-3 soil). Used in CH4 module.
#' @param Ea_labile_CH4 Activation energy for methane production from
#'   labile carbon (kJ mol^-1). Used in CH4 module.
#' @param kM_labile_CH4 Half-saturation constant for labile methane production
#'   (g C m^-3 soil). Used in CH4 module.
#' @param Ea_oxi_CH4 Activation energy for methane oxidation (kJ mol^-1). Used 
#'   in CH4 module.
#' @param kM_oxi_CH4 Half-saturation constant for methane oxidation
#'   (g C m^-3 soil). Used in CH4 module.
#' @param kI_SO4 Sulfate inhibition constant (mg L^-1). Used in CH4 module.
#' @param kI_NO3 Nitrate inhibition constant (mg L^-1). Used in CH4 module.
#' @param k_plant_oxi Fraction of CH4 oxidized during transport. Used in CH4 
#'   module.
#'
#' @description
#' Wrapper function to run all steps of the PEPRMT model (v1.0).
#'
#' @details
#' The PEPRMT model was originally parameterized for restored freshwater
#' wetlands in the Sacramento–San Joaquin River Delta, California, USA
#' (Oikawa et al. 2017) and later updated for tidal wetlands with inhibition
#' of methane production in response to salinity and nitrate
#' (Oikawa et al. 2024).
#'
#' Modules are run sequentially:
#' PEPRMT_GPP, then PEPRMT_Reco, then PEPRMT_CH4.
#'
#' All variables are expected at a daily time step.
#'
#' All PEPRMT modules use the same input structure, although not all variables
#' are used in every module.
#'
#' **Required data columns (order does not matter):**
#' 1. DOY: Continuous day of year
#' 2. DOY_disc: Discontinuous day of year
#' 3. Year
#' 4. TA_C: Air temperature (°C)
#' 5. WTD_cm: Water table depth (cm)
#' 6. PAR_umol_m2_day: PAR (µmol m^-2 d^-1)
#' 7. LAI: Leaf Area Index
#' 8. EVI: Greenness Index
#' 9. FPAR: FPAR flag
#' 10. LUE: Light Use Efficiency
#' 11. Wetland_age_years: Wetland age (years)
#' 12. Salinity_daily_ave_ppt: Salinity (ppt)
#' 13. NO3_mg_L: NO3 (mg L^-1)
#' 14. SOM_MEM_gC_m3: Soil organic matter (g C m^-3)
#' 15. site: Site identifier
#'
#' @returns Updated dataframe containing:
#' \describe{
#'   \item{GPP}{gross primary productivity
#'     (g C CO2 m^-2 day^-1)}
#'   \item{APAR}{absorbed photosynthetically active radiation
#'     (umol m-2 d-1)}
#'   \item{Reco_full}{Total ecosystem respiration
#'     (g C CO2 m^-2 day^-1)}
#'   \item{NEE_mod}{Net ecosystem exchange of CO2
#'     (g C CO2 m^-2 day^-1)}
#'   \item{S1}{Labile soil carbon pool
#'     (g C m^-3, top meter of soil)}
#'   \item{S2}{Soil organic carbon pool
#'     (g C m^-3, top meter of soil)}
#'   \item{pulse_emission_total}{total methane emitted
#'     (g C CH4 m^-2 day^-1)}
#'   \item{Plant_flux_net}{net methane flux via plant-mediated transport
#'     (g C CH4 m^-2 day^-1)}
#'   \item{Hydro_flux}{net diffusive methane flux from water to atmosphere
#'     (g C CH4 m^-2 day^-1)}
#'   \item{M1}{methane pool produced from labile soil carbon
#'     (g C CH4 m^-3, top meter of soil and water)}
#'   \item{M2}{methane pool produced from soil organic carbon
#'     (g C CH4 m^-3, top meter of soil and water)}
#'   \item{trans2}{fraction of methane released via plant-mediated transport
#'     (unitless)}
#' }
#' @export
#'
run_PEPRMT <- function(data,
                       wetland_type,
                       a0 = 0.7479271,
                       a1 = 1.0497113,
                       Ha = 149.4681710 + 30,
                       Hd = 94.4532674 + 100,
                       T_opt_GPP = 25 + 274.15, # (K); our Temp opt for Ps is 25C
                       Ea_SOM = 18.41329,
                       kM_SOM = 1487.65701,
                       Ea_labile = 11.65972,
                       kM_labile = 61.29611,
                       Ea_SOM_CH4 = 14.9025078 + 67.1, # Activation Energy for SOM pool (kJ mol-1)
                       kM_SOM_CH4 = 0.4644174 + 17, # Half-saturation constant for SOM pool (gC m-3 soil)
                       Ea_labile_CH4 = 16.7845002 + 71.1, # Activation Energy for labile pool (kJ mol-1)
                       kM_labile_CH4 = 0.4359649 + 23, # Half-saturation constant for labile pool (gC m-3 soil)
                       Ea_oxi_CH4 = 15.8857612 + 75.4, # Activation Energy for CH4 oxidation (kJ mol-1)
                       kM_oxi_CH4 = 0.5120464 + 23, # Half-saturation constant for CH4 oxidation (gC m-3 soil)
                       kI_SO4 = 486.4106939, # Sulfate inhibition factor (mg L-1)
                       kI_NO3 = 0.1020278, # Nitrate inhibition factor  (mg L-1)
                       k_plant_oxi = 0.35 # percent oxidized during transport
                       ) {
  # -------------------------
  # Check data structure
  # -------------------------

  data <- data.frame(data)
  expected_colnames <- c(
    "DOY", "DOY_disc", "Year", "TA_C", "WTD_cm",
    "PAR_umol_m2_day", "LAI", "EVI", "FPAR", "LUE", "Wetland_age_years",
    "Salinity_daily_ave_ppt", "NO3_mg_L",
    "SOM_MEM_gC_m3", "site"
  )

  if (!all(expected_colnames %in% colnames(data))) {
    stop(paste0(
      "Missing required inputs.\nThe following columns were not found in data:\n",
      paste(expected_colnames[!expected_colnames %in% colnames(data)],
        collapse = ", "
      )
    ))
  }

  # -------------------------
  # Check parameters
  # -------------------------

  # TO DO: add checks here

  # -------------------------
  # Check wetland_type
  # -------------------------

  if (!is.numeric(wetland_type) || length(wetland_type) != 1 ||
    !wetland_type %in% c(1, 2)) {
    stop("wetland_type must be a single numeric value: 1 (Freshwater peatland) or 2 (Tidal wetland).",
      call. = FALSE
    )
  }

  # -------------------------
  # Run PEPRMT functions
  # -------------------------

  # GPP

  GPP_mod_data <- PEPRMT_GPP(data = data, 
                             a0 = a0,
                             a1 = a1,
                             Ha = Ha,
                             Hd = Hd,
                             T_opt_GPP = T_opt_GPP)

  # Create a new dataset that includes model results
  results <- data |>
    dplyr::left_join(
      GPP_mod_data,
      by = c("DOY", "site")
    )

  # Reco

  # Add modeled GPP into data before running Reco module (16th column)
  data$GPP_mod <- results$GPP_mod

  Reco_mod_data <- PEPRMT_Reco(
    data = data,
    Ea_SOM = Ea_SOM,
    kM_SOM = kM_SOM,
    Ea_labile = Ea_labile,
    kM_labile = kM_labile,
    wetland_type = wetland_type
  )

  # Add model results
  results <- results |>
    dplyr::left_join(
      Reco_mod_data,
      by = c("DOY", "site")
    )

  # CH4

  # Add modeled S1, S2 into data before running CH4 module (17th & 18th columns)
  data$SOM_total <- results$S1
  data$SOM_labile <- results$S2

  CH4_mod_data <- PEPRMT_CH4(
    data = data,
    Ea_SOM_CH4 = Ea_SOM_CH4,
    kM_SOM_CH4 = kM_SOM_CH4,
    Ea_labile_CH4 = Ea_labile_CH4,
    kM_labile_CH4 = kM_labile_CH4,
    Ea_oxi_CH4 = Ea_oxi_CH4,
    kM_oxi_CH4 = kM_oxi_CH4,
    kI_SO4 = kI_SO4,
    kI_NO3 = kI_NO3,
    k_plant_oxi = k_plant_oxi,
    wetland_type = wetland_type
  )

  # Add model results
  results <- results |>
    dplyr::left_join(
      CH4_mod_data,
      by = c("DOY", "site")
    )

  return(results)
}
